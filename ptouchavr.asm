; Title::     P-Touch AVR
; Author::    Anton Maurovic <anton.maurovic@gmail.com>
; URL::       http://anton.maurovic.com/
;             http://github.com/algofoogle/ptouch-avr
;
; Description::
;
;   This is prototype AVR firmware for driving the Kyocera KSH64FA TPH (Thermal Printhead)
;   as used in the Brother PT-1000 and PT-1010 hand-held label printers. It is
;   intended to accompany an article I'm writing on "Brother PT-1010 Hacking"
;   at my blog, http://anton.maurovic.com.
;
;   The behaviour spec for how this is to be used is:
;
;   1.  The printer is powered on.
;   2.  The MCU powers on, initialises, and goes idle.
;   3.  Start the printer on a run of (say) 30 random characters JUST to get
;       the tape rolling.
;   4.  Press a button attached to the MCU to signal that it should start.
;   5.  MCU renders a 64-line test pattern (which allows us to measure the resolution,
;       coverage, and aspect ratio of the dots).
;   6.  MCU then waits for another button press, after which it will repeat step 4 onwards.
;
;   The detailed technical spec for this firmware is as follows:
;
;   1.  The MCU is powered by the regulated 3.3V supply (VCC) provided by the printer.
;   2.  When the printer is powered on (and hence the MCU), the MCU should go thru an
;       init routine that:
;       a.  Sets PB0, PB2, PB3, and PB4 to be outputs, and asserts the correct signals
;           for each (corresponding to control lines of the TPH) such that CLK is low,
;           and DATA, /LATCH, and /STROBE are all high.
;       b.  Calibrates the MCU's internal oscillator to approx. 9.6MHz (which will use
;           a pre-determined and tested calibration value for 3.3V operation).
;       c.  Configures a timer and interrupt that can be used to time stages of the
;           14.4ms line rendering cycle. Timer is initially *stopped*, though.
;       d.  Sets PB1 to be an input (internally pulled high).
;       e.  Configures an interrupt on PB1.
;   3.  The MCU then waits for PB1 to be pulled low. When that happens, PB1's interrupt
;       then gets turned off, and the timer is started.
;   4.  The timer interrupt is responsible for reading data from Program Memory and
;       sending it to the TPH at the correct rate, hence rendering a 64x64 test pattern.
;   5.  At the end of 64 lines, the timer is disabled, and PB1's interrupt is re-enabled.
;
; Burning info::
;
; ***    LOW fuse byte:  0x6A
; ***    HIGH fuse byte: 0xFF
; ***    EEPROM:         Not used
;
; Usage info::
;
;   This is AVR Assembly code, written using the GNU "as" assembler syntax, and
;   specifically "avr-as" as is installed with avr-gcc. It targets the ATtiny13A
;   initially, but should work with most other larger ATtiny variants.
;
;   It is wise to ensure there is a 100nF bypass capacitor strapped between
;   VCC and GND of the MCU.
;
;   To avoid accidentally asserting any lines of the TPH before the MCU reaches
;   a stable state, pull-up resistors should be used on all digital pins that
;   interface with the TPH. I'm not sure yet what value: 2.2k? 4.7k?
;
;   The MCU should be powered directly from the 3.3V supply used by the rest of
;   the P-Touch controller PCB, to ensure there are no unexpected voltage
;   differences from another PSU or separate regulator.
;
;   The pins of the ATtiny13A have been used as follows:
;
;   | MCU Pin | Port   | TPH Pin | TPH Name | Purpose                              |
;   | -       | -      | 1       | Vheater  | +9V, supplied by PT-1010             |
;   | 5       | PB0    | 2       | DATA     | SPI data (Input, at TPH)             |
;   | 7       | PB2    | 3       | CLK      | SPI clock (Input, at TPH)            |
;   | 2       | PB3    | 4       | /LATCH   | Data buffer latch (active low)       |
;   | 8??     | VCC    | 5       | VCC?     | +3.3V, supplied by PT-1010           |
;   | 4       | GND    | 6       | GND      | Common ground                        |
;   | 4       | GND    | 7       | GND      | Physically connected to TPH pin 6    |
;   | 3       | PB4    | 8       | /STROBE  | Heater on (active low); 5.7ms max!   |
;   | 8??     | VCC    | 9       | VCC?     | +3.3V, supplied by PT-1010           |
;   | -       | -      | 10      | Vheater  | +9V, supplied by PT-1010             |
;   | 1       | /RESET | -       | -        | MCU External RESET                   |
;   | 6       | PB1    | -       | -        | Reserved for INT0 external interrupt |
;
;   NOTE: One of the TPH's VCC pins (5 and 9) might not be a supply, but rather
;   an input hard-wired to VCC, OR a line actually controlled by an output from
;   the P-Touch's internal MCU. If the latter is true, it might fail if we try
;   to use it as the VCC supply for the ATtiny! We could probably verify this
;   by measuring the resistance between TPH pins 5 and 9 ON THE MAIN P-TOUCH PCB
;   when the TPH and batteries are both disconnected. If the resistance is less
;   than 2 ohms, it's probably safe to assume it's hard-wired and both pins
;   are truly VCC.
;
;   Pin 1 of the MCU (/RESET or PB5) is configured to work as an external RESET,
;   but this can be tied high (?) and the MCU will reset itself internally at power-on.
;   Later it could be another control or input line, but once we do that the MCU can't
;   be re-programmed without a "high voltage programmer".
;
;   Pin 6 (PB1) is reserved for the moment. Later it is the ideal candidate for
;   triggering INT0 (External Interrupt Request 0), which we could use to sense when
;   the P-Touch's motor engages, and hence know the right time to start controlling
;   the TPH. It could otherwise be used to sense a manual button-press.
;
; Quick visual test:
;
;   If you have an LED and 330-ohm resistor in series between the /STROBE (PB4, pin 3)
;   line and VCC, then the LED should be off when the MCU is initially powered on.
;   When you press the button, the line print routine should kick in, and the LED
;   should be lit (at mid brightness) for about 1 second: 64 x 14.4ms ~= 0.9 seconds.
;   If it's lit for nearly 2 seconds (or longer) it means the INT0 interrupt
;   flagged twice, back to back. This ideally shouldn't happen since:
;
;   1.  The very start of the button ISR should disable INT0,
;       so that it no longer gets flagged until we're ready;
;   2.  The end of the line printing test should clear the button interrupt sense flag,
;       so that pressing the button a 2nd time during the print run will have no effect
;       anyway even if point 1 fails;
;   3.  Even if the button is held down, INT0 is only ever set to trigger on a falling
;       edge, rather than when the pin is simply at a low level.
;

.include "t13.asm"
.include "macros.asm"

; Define which pins are used for which inputs of the TPH:
.equ TPH_STROBE,    PB4
.equ TPH_LATCH,     PB3
.equ TPH_DATA,      PB0
.equ TPH_CLK,       PB2
.equ SENSE,         PB1
; MASK bit versions of the above:
.equ M_TPH_STROBE,  (1<<TPH_STROBE)
.equ M_TPH_LATCH,   (1<<TPH_LATCH)
.equ M_TPH_DATA,    (1<<TPH_DATA)
.equ M_TPH_CLK,     (1<<TPH_CLK)
.equ M_SENSE,       (1<<SENSE)
; Reserved registers:
; We've reserved these registers for specific global uses:
.equ BYTE_COUNT,    18
.equ DATA_BUFFER,   19
.equ OUT_BUFFER,    20
.equ BIT_COUNT,     21
; Line counter WORD:
.equ LCR,           XR      ; Name for use with SBIW.
.equ LCL,           XL
.equ LCH,           XH
; Graphics data pointer:
.equ GR,            YR      ; Name for use with MOVW.
.equ GL,            YL
.equ GH,            YH

; General parameters:
.equ ACTIVE_TIMEOUT,55      ; The "Line ACTIVE window" lasts for 55 timer ticks (55*1024 CPU cycles) => 5.87ms
.equ IDLE_TIMEOUT,  80      ; The "Line IDLE window" lasts 80 ticks => 8.53ms
.equ OSCCAL_TARGET, 0x6F    ; OSCCAL value of 0x6F seems to be closest to 9.6MHz on my ATtiny13A at 3.3V.


; ------------------------------- Interrupt table -------------------------------;

; Starts at 0x0000. ATtiny13A has 10 interrupt vectors (inc. RESET):
.org 0x0000
firmware_top:
; Reset vector comes first:
    rjmp init
; Interrupt no. 7 (Timer Compare Match A) does special handling depending on which step
; of the repeating 14.4ms cycle it has reached. The other 8 interrupts all just return
; without doing anything.
    rjmp int0_isr               ; Interrupt Vector 2   = EXT_INT0   (External Interrupt Request 0)
    reti                        ; Interrupt Vector 3   = PCINT0     (Pin Change)
    reti                        ; Interrupt Vector 4   = TIM0_OVF   (Timer Overflow)
    reti                        ; Interrupt Vector 5   = EE_RDY     (EEPROM Ready)
    reti                        ; Interrupt Vector 6   = ANA_COMP   (Analog Comparator)
    ijmp  ; Jump to Z address   ; Interrupt Vector 7   = TIM0_COMPA (Timer Compare Match A)
    reti                        ; Interrupt Vector 8   = TIM0_COMPB (Timer Compare Match B)
    reti                        ; Interrupt Vector 9   = WDT        (Watchdog Timeout)
    reti                        ; Interrupt Vector 10  = ADC        (ADC Conversion Complete)


; ------------------------------- Main Initialisation -------------------------------;

init:
    ; Disable interrupts:
    cli

    ; At power-on, the MCU's pins should all be inputs. Before we change them to
    ; outputs, we want to make sure they'll assert the correct states.
    ; /STROBE, /LATCH, and DATA should all be high. The rest should be low, but
    ; we also set M_SENSE (which will be an input) so that its internal pull-up
    ; is enabled:
    ldi r17, M_TPH_STROBE | M_TPH_LATCH | M_TPH_DATA | M_SENSE
    out PORTB, r17

    ; Set all pins (except SENSE) to be outputs:
    ldi r16, ~M_SENSE
    out DDRB, r16
    nop ; Synchronise.

    ; Output prior pin state again, just in case it was lost when changing DDRB:
    out PORTB, r17

    ; Gradually adjust OSCCAL until it reaches our target calibration value:
    slide_osccal OSCCAL_TARGET

    ; Make the CPU clock run at full speed:
    disable_clock_prescaler

    init_stack

    ; Configure INT0 to fire on the falling edge of the INT0 pin (PB1):
    enable_int0 int0_falling

    ; Enable interrupts:
    sei

sleep_loop:
    ; Enable "Idle" sleep mode:
    enable_sleep
    ; Sleep -- basically, halt the CPU and let interrupts fire when required:
    sleep
    ; I think we get here when a RETI occurs from within an ISR.
    rjmp sleep_loop


; ------------------------------- INT0 (Button Pressed) ISR -------------------------------;

int0_isr:
    ; Button was pressed...
    ; Disable the interrupt so a button press won't do anything until it's re-enabled
    ; after printing all lines that we need.
    disable_int0

    ; Set up our 'G' pointer to point to the start of our pixel data block, as
    ; stored in Program Memory, beginning at the 'pixel_data' label.
    ; ldi GH, hi8(pixel_data)
    ; ldi GL, lo8(pixel_data)
    ldiw_data GR, pixel_data

    ; Set up the number of lines that are defined in pixel_data
    ; (i.e. no. of bytes in the data table, divided by 8):
    ; ldi LCH, (end_pixel_data-pixel_data)>>11
    ; ldi LCL, ((end_pixel_data-pixel_data)>>3) & 0xFF
    ldiw LCR, (end_pixel_data-pixel_data)>>3

    ; Set up the Z (16-bit) pointer to be an address pointer to the
    ; "Line ACTIVE Window" ISR. Basically, the first time-out will call the ISR
    ; which loads line data and fires the heater of the TPH.
    ; ldi ZH, pm_hi8(line_active_isr)
    ; ldi ZL, pm_lo8(line_active_isr)
    ldiw_code ZR, line_active_isr

    ; Configure timer to generate an interrupt after 80 counts (or 8.53ms).
    ; That is, after 80/(CLK/1024) seconds =>
    ;   80*1024/9600000 = 0.0085333... ~= 8.53ms
    ;
    init_simple_timer clk_1024, IDLE_TIMEOUT
    ; NOTE: The timer should now be started.
    ; NOTE: Interrupts are globally enabled, automatically, after RETI:
    reti



; ------------------------------- Line ACTIVE Window ISR -------------------------------;

; This ISR (Interrupt Service Routine) handles TIM0_COMPA after the 8.53ms delay of the
; "Line IDLE Window ISR". It writes data for the next line to print, with the following
; routine:
;
;   1.  Change timeout to 55 counts (i.e. 5.87ms), setting the total duration of our
;       Line ACTIVE Window (i.e. 5.87ms before jumping to the "Line IDLE Window" ISR).
;   2.  Raises TPH_CLK, with an initial lead-in delay.
;   3.  Identifies where (in Program Memory) to read pixel data bytes from.
;   4.  In a loop, loads 8 bytes and clocks out 8 bits for each of those 8 bytes,
;       effectively pushing 64 pixels via SPI.
;   5.  Does a post-delay, before asserting /LATCH with a low pulse.
;   6.  Delays briefly again, before asserting /STROBE (to fire up the TPH heater elements).
;   7.  Letting /STROBE remain asserted now, it:
;       a.  Stashes the updated pixel data pointer (updated after steps 3 and 4);
;       b.  Sets up Z to point the next TIM0_COMPA interrupt to the "Line IDLE Window" ISR.
;       c.  Exits the ISR with "RETI".
;
;   Since the timer is running the whole time, it will time out exactly 55 counts since
;   the VERY START of this ISR. /STROBE will hence remain asserted -- being reset only
;   by the "Line IDLE Window" ISR -- after approx. 5.87ms MINUS the time it takes to
;   execute all of steps 1--7 (which has been shown to be about 220uS). Hence, /STROBE
;   will be asserted for about a total of 5.65ms.
;

; Timing formulae:
;   
;   | Var | Meaning                                     | Formula               | K[^1] | ~Time (us) |
;   | K   | 1 MCU cycle at 9.6MHz                       |                       |    1  |    0.1042  |
;   | A   | SPI front porch (initial CLK high)          |                       |   86  |    8.958   |
;   | c   | SPI CLK pulse width (half-cycle)            |                       |   10  |    1.042   |
;   | G   | [^2]                                        |                       |   84  |    8.750   |
;   | g   | Gap between each regular SPI byte sequence  | g = G-2c              |   65  |    6.771   |
;   | a   | Pre-delay before starting SPI byte sequence | a = A-g               |   21  |    2.188   |
;   | b   | SPI bit push                                | b = 2c                |   20  |    2.083   |
;   | B   | 1 full (8-bit) SPI byte sequence            | B = g+c+8b = g+17c    |  235  |   24.480   |
;
;
;   [^1]:   1 "K" is one cycle of the MCU clock at 9.6MHz, which is the amount of time it
;           takes (exactly) to execute a typical, single ATtiny13A instruction. Hence,
;           all timing is derrived from this.
;
;   [^2]:   Time measured between last CLK rising edge of prior byte, and first CLK falling
;           edge of next byte... i.e. total duration that CLK is high between bytes.
;   

line_active_isr:
    ; Raise CLK for (a):
    sbi PORTB, TPH_CLK      ; 2K

; 0K

    ; OK, so CLK has JUST gone high at this point...
    ; If we now count the cycles between here and the moment it next goes
    ; low (just after the OUT spi_begin_bit), we should count 86...

    ; (a) Early delay, with CLK high:
    ; Point Z to start of this line's data:
    movw ZR, GR             ; 1K
    ; Set byte count to 8:
    ldi BYTE_COUNT, 8       ; 1K
    ; Wait out remainder of (a), i.e. a-2K = 19K
    micro_delay 7           ; 21K
    nop                     ; 1K

next_byte:

; 24K, 259K, 494K, 729K, 964K, 1199K, 1434K, 1669K

    ; (8B) Byte load/write loop: 8B = 8(g+8b+c) = 8(g+17c) = 1880K = 195.833us
    ; Wait out MOST of (g), except for a little bit of data prep to pad it out afterwards:
    micro_delay 17          ; 51K
    ; Set bit count to 8:
    ldi BIT_COUNT, 8        ; 1K
    ; Load a byte that we need to push out, incrementing Z at the same time:
    lpm DATA_BUFFER, Z+     ; 3K

next_bit:

;  79K,  99K, 119K, 139K, 159K, 179K, 199K, 219K
; 314K                                  ... 454K
; 549K                                  ... 689K
;                                       ... 924K
;                                       ... 1159K
;                                       ... 1394K
;                                       ... 1629K
;                                       ... 1864K

    ; Prep the data we'll write directly to PORTB, in a register.
    ; We do this so we'll be able to change the state of CLK and DATA simultaneously.
    ; /LATCH=1; /STROBE=1; CLK=0; DATA=C; SENSE=1 (for pull-up).
    ldi OUT_BUFFER, M_TPH_LATCH | M_TPH_STROBE | M_SENSE; 1K
    ; Load the bit we need to push out, into C, starting with MSB:
    rol DATA_BUFFER         ; 1K
    brcc spi_begin_bit      ; 2K if C clear (i.e. if DATA pin stays low)
    ; Need to make sure DATA pin will go high:
    ori OUT_BUFFER, M_TPH_DATA     ; 1K
spi_begin_bit:
    adiw ZR, 0              ; 2K - pad ONLY
    ; Write register-buffered pin states to PORTB, hence setting CLK and DATA states simultaneously:
    out PORTB, OUT_BUFFER   ; 1K

;  86K, 106K, 126K, 146K, 166K, 186K, 206K, 226K
; 321K                                  ... 461K
; 556K                                  ... 696K
;                                       ... 931K
;                                       ... 1166K
;                                       ... 1401K
;                                       ... 1636K
;                                       ... 1871K

    ; Now wait 10K and make CLK go high:
    lpm                     ; 3K - pad ONLY
    lpm                     ; 3K - pad ONLY
    adiw ZR, 0              ; 2K - pad ONLY
    sbi PORTB, TPH_CLK      ; 2K

;  96K, 116K, 136K, 156K, 176K, 196K, 216K, 236K
; 331K, 351K, 371K, 391K, 411K, 431K, 451K, 471K
; 566K                                  ... 706K
;                                       ... 941K
;                                       ... 1176K
;                                       ... 1411K
;                                       ... 1646K
;                                       ... 1881K

    ; Now check if we have any more bits left...
    dec BIT_COUNT           ; 1K
    brne next_bit           ; 2K
    ; By this point, we're 2 cycles into 10 of the *FINAL* CLK high state,
    ; which is then followed by another (c), for a total of 2K spent out of 20K...
    micro_delay 5           ; 15K
    nop                     ; 1K
    ; /LATCH, /STROBE, and CLK are already high as required.
    ; Ensure DATA is high, too, in preparation for clocking out a byte.
    ; This is done here to fix loop timing, while also ensuring that
    ; the DATA line is in the correct state after the last iteration.
    sbi PORTB, TPH_DATA     ; 2K

; 256K, 491K, 726K, 961K, 1196K, 1431K, 1666K, 1901K

    dec BYTE_COUNT          ; 1K
    brne next_byte          ; 2K

; 1903K

    ; Now wait out the remainder of g+t and then pull CLK low again.
    micro_delay 28          ; 84K
    nop                     ; 1K
    cbi PORTB, TPH_CLK      ; 2K

; 1990K

    ; Wait out R (78K), and then pulse /LATCH for 18K
    micro_delay 25          ; 75K
    nop                     ; 1K
    cbi PORTB, TPH_LATCH    ; 2K
    micro_delay 5           ; 15K
    nop                     ; 1K
    sbi PORTB, TPH_LATCH    ; 2K

; 2086K

    ; Wait out W (23K) and then lower /STROBE
    micro_delay 7           ; 21K
    cbi PORTB, TPH_STROBE   ; 2K

; 2109K

    ; At this point, /STROBE is now lowered, so prepare the next stage...

    ; Save current pointer to the first byte of the next line (Z), in GR:
    movw GR, ZR
    ; Change ORC0A to 55, so that the next time the interrupt fires, it will
    ; be after our CURRENT window of 55*1024/9600000 seconds (5.87ms) has
    ; elapsed, and we're ready to bring /STROBE high again.
    ldi r16, ACTIVE_TIMEOUT
    out OCR0A, r16
    ; Point Z to the "Line IDLE Window" ISR.
    ldiw_code ZR, line_idle_isr
    ; Now we can exit this ISR:
    reti


; ------------------------------- Line IDLE Window ISR -------------------------------;

;   This handles the window of 8.53ms at the end of the time during which /STROBE is
;   required/permitted to be asserted. It raises /STROBE off (to stop the TPH's heater).
;   It also decrements the line counter, then checks whether there are any lines left:
;
;   *   If there ARE lines left, it adjusts the timer to fire after 8.53ms so that
;       the next line can then start after that. It then points Z to line_active_isr.
;   *   If there are NO lines left, it disables the timer interrupt, clears the INT0
;       flag, re-enables INT0, and returns. This ensures any prior button press is
;       ignored, and allows INT0 to fire when the button is pressed next... which will
;       then reset everything and re-enable the timer to start printing again.
;

line_idle_isr:
    ; Raise /STROBE, so that it's no longer asserted:
    sbi PORTB, TPH_STROBE
    ; Decrement line counter:
    sbiw LCR, 1
    ; Are there any lines left?
    brne more_lines_left
    ; No more lines left...
    ; Disable timer interrupt:
    disable_timer
    ; Clear INTF0 (flag of whether or not the interrupt has been asserted):
    ; Writing a ONE to bit 6 (INTF0) of GIFR CLEARS it... I think this is
    ; because it can't actually be set in software, and this ensures that
    ; by writing a ZERO we don't have to touch any of the other bits:
    in r16, GIFR
    sbr r16, (1<<6)
    out GIFR, r16
    nop
    ; Re-enable INT0 interrupt (using previous settings):
    enable_int0
    reti
more_lines_left:
    ; Change timer threshold to now fire after 80 ticks (8.53ms):
    ldi r16, IDLE_TIMEOUT
    out OCR0A, r16
    ; Point Z to line_active_isr:
    ldiw_code ZR, line_active_isr
    reti


pixel_data:

    ; Very fine pitch:
    .byte 0b11010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010011
    .byte 0b01100101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010110
    .byte 0b00110101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01001101
    .byte 0b01011001, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01011001
        ; Alternate:
    .byte 0b10101100, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10110010
    .byte 0b10100110, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b01101010
    .byte 0b10101011, 0b00101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b11001010
    .byte 0b10101001, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101001, 0b10101010

    ; Fine pitch:
    .byte 0b00110010, 0b11010011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011
    .byte 0b00110011, 0b01100011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110110, 0b00110011
    .byte 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00101101, 0b00110011
    .byte 0b00110011, 0b00011011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00011011, 0b00110011
        ; Alternate:
    .byte 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b10110100, 0b11001100
    .byte 0b11001100, 0b11000110, 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b01101100, 0b11001100
    .byte 0b11001100, 0b11001011, 0b01001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100
    .byte 0b11001100, 0b11001101, 0b10001100, 0b11001100, 0b11001100, 0b11001101, 0b10001100, 0b11001100

    ; Small checks:
    .byte 0b00001111, 0b00001111, 0b00001111, 0b00001111, 0b00001111, 0b00001111, 0b00001111, 0b00001111
    .byte 0b00001111, 0b00001111, 0b00001111, 0b00001111, 0b00001111, 0b00001111, 0b00001111, 0b00001111
    .byte 0b00001111, 0b00001111, 0b00001111, 0b00001111, 0b00001111, 0b00001111, 0b00001111, 0b00001111
    .byte 0b00001111, 0b00001111, 0b00001111, 0b00001111, 0b00001111, 0b00001111, 0b00001111, 0b00001111
        ; Alternate:
    .byte 0b11110000, 0b11110000, 0b11110000, 0b11110000, 0b11110000, 0b11110000, 0b11110000, 0b11110000
    .byte 0b11110000, 0b11110000, 0b11110000, 0b11110000, 0b11110000, 0b11110000, 0b11110000, 0b11110000
    .byte 0b11110000, 0b11110000, 0b11110000, 0b11110000, 0b11110000, 0b11110000, 0b11110000, 0b11110000
    .byte 0b11110000, 0b11110000, 0b11110000, 0b11110000, 0b11110000, 0b11110000, 0b11110000, 0b11110000

    ; Medium checks:
    .byte 0b00000000, 0b11111111, 0b00000000, 0b11111111, 0b00000000, 0b11111111, 0b00000000, 0b11111111
    .byte 0b00000000, 0b11111111, 0b00000000, 0b11111111, 0b00000000, 0b11111111, 0b00000000, 0b11111111
    .byte 0b00000000, 0b11111111, 0b00000000, 0b11111111, 0b00000000, 0b11111111, 0b00000000, 0b11111111
    .byte 0b00000000, 0b11111111, 0b00000000, 0b11111111, 0b00000000, 0b11111111, 0b00000000, 0b11111111
        ; Alternate:
    .byte 0b11111111, 0b00000000, 0b11111111, 0b00000000, 0b11111111, 0b00000000, 0b11111111, 0b00000000
    .byte 0b11111111, 0b00000000, 0b11111111, 0b00000000, 0b11111111, 0b00000000, 0b11111111, 0b00000000
    .byte 0b11111111, 0b00000000, 0b11111111, 0b00000000, 0b11111111, 0b00000000, 0b11111111, 0b00000000
    .byte 0b11111111, 0b00000000, 0b11111111, 0b00000000, 0b11111111, 0b00000000, 0b11111111, 0b00000000

; A more complex test pattern:

    ; Header:
    .byte 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010
    .byte 0b10101010, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b01010101

    ; "ANTON."
    .byte 0b10101010, 0b00000000, 0b01000100, 0b01011111, 0b00111001, 0b00010000, 0b00000000, 0b01010101
    .byte 0b10101010, 0b00000000, 0b10100110, 0b01000100, 0b01000101, 0b10010000, 0b00000000, 0b01010101
    .byte 0b10101010, 0b00000001, 0b00010101, 0b01000100, 0b01000101, 0b01010000, 0b00000000, 0b01010101
    .byte 0b10101010, 0b00000001, 0b11110100, 0b11000100, 0b01000101, 0b00110000, 0b00000000, 0b01010101
    .byte 0b10101010, 0b00000001, 0b00010100, 0b01000100, 0b01000101, 0b00010110, 0b00000000, 0b01010101
    .byte 0b10101010, 0b00000001, 0b00010100, 0b01000100, 0b00111001, 0b00010110, 0b00000000, 0b01010101

    ; Separator
    .byte 0b11100111, 0b00111001, 0b11001110, 0b01110011, 0b10011100, 0b11100111, 0b00111001, 0b11001110
    .byte 0b11100111, 0b00111001, 0b11001110, 0b01110011, 0b10011100, 0b11100111, 0b00111001, 0b11001110
    .byte 0b10101010, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b01010101

    ; "MAUROVIC."
    .byte 0b10101010, 0b01000100, 0b01000100, 0b01011110, 0b00111001, 0b00010100, 0b11100000, 0b01010101
    .byte 0b10101010, 0b01101100, 0b10100100, 0b01010001, 0b01000101, 0b00010101, 0b00010000, 0b01010101
    .byte 0b10101010, 0b01010101, 0b00010100, 0b01010001, 0b01000101, 0b00010101, 0b00000000, 0b01010101
    .byte 0b10101010, 0b01000101, 0b11110100, 0b01011110, 0b01000101, 0b00010101, 0b00000000, 0b01010101
    .byte 0b10101010, 0b01000101, 0b00010100, 0b01010010, 0b01000100, 0b10100101, 0b00010110, 0b01010101
    .byte 0b10101010, 0b01000101, 0b00010011, 0b10010001, 0b00111000, 0b01000100, 0b11100110, 0b01010101

    ; Separator
    .byte 0b11011011, 0b01101101, 0b10110110, 0b11011011, 0b01101101, 0b10110110, 0b11011011, 0b01101101
    .byte 0b11011011, 0b01101101, 0b10110110, 0b11011011, 0b01101101, 0b10110110, 0b11011011, 0b01101101
    .byte 0b11010100, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00101011

    ; "COM"
    .byte 0b11010100, 0b00000000, 0b00000011, 0b10001110, 0b01000100, 0b00000000, 0b00000000, 0b00101011
    .byte 0b11010100, 0b00000000, 0b00000100, 0b01010001, 0b01101100, 0b00000000, 0b00000000, 0b00101011
    .byte 0b11010100, 0b00000000, 0b00000100, 0b00010001, 0b01010100, 0b00000000, 0b00000000, 0b00101011
    .byte 0b11010100, 0b00000000, 0b00000100, 0b00010001, 0b01000100, 0b00000000, 0b00000000, 0b00101011
    .byte 0b11010100, 0b00000000, 0b00000100, 0b01010001, 0b01000100, 0b00000000, 0b00000000, 0b00101011
    .byte 0b11010100, 0b00000000, 0b00000011, 0b10001110, 0b01000100, 0b00000000, 0b00000000, 0b00101011

    ; Separator
    .byte 0b11101110, 0b11101110, 0b11101110, 0b11101110, 0b11101110, 0b11101110, 0b11101110, 0b11101110
    .byte 0b11101110, 0b11101110, 0b11101110, 0b11101110, 0b11101110, 0b11101110, 0b11101110, 0b11101110

    ; Footer
    .byte 0b10101010, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b01010101
    .byte 0b10101010, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b01010101
    .byte 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111
    .byte 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111

end_pixel_data:

