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
;   It is intended that an MCU programmed with this firmware will be used as follows:
;
;   1.  When the printer is powered on, the MCU powers on, initialises, and goes idle.
;   2.  When a print run is started, and the tape starts rolling, the MCU will detect
;       this and start sending data for printing its own image; the
;       MCU renders an image independently of whatever the printer is doing.
;   3.  When the MCU finishes sending the data for its complete image, it goes back
;       to checking to see if the printer is still running. If so, it will repeat
;       the rendering of its own image.
;
;   The detailed technical spec for this firmware is as follows:
;
;   1.  The MCU is powered by the regulated 3.3V supply (VCC) provided by the printer.
;   2.  When the printer is powered on (and hence the MCU), the MCU should go thru an
;       init routine that:
;       a.  Configures one pin as an input, which I've called "SENSE". The actual pin
;           used is whichever is designated to have the "INT0" External Interrupt
;           function (PB1 for ATtiny13, or PB2 for ATtiny25/45/85).
;       b.  Configures all other pins as outputs, and asserts the correct signals
;           for each (corresponding to control lines of the TPH) such that CLK is low,
;           and DATA, /LATCH, and /STROBE are all high.
;       c.  Calibrates the MCU's internal oscillator.  to approx. 9.6MHz (which will use
;           a pre-determined and tested calibration value for 3.3V operation).
;       c.  Configures a timer and interrupt that can be used to time stages of the
;           14.4ms line rendering cycle. Timer is initially *stopped*, though.
;       d.  Sets PB1 to be an input (internally pulled high).
;       e.  Configures an interrupt on PB1.
;   3.  The MCU then waits for PB1 to be pulled low. When that happens, PB1's interrupt
;       then gets turned off, and the timer is started.
;   4.  The timer interrupt is responsible for reading data from Program Memory and
;       sending it to the TPH at the correct rate, hence rendering a pattern line-by-line.
;   5.  At the end of the pattern, the timer is disabled, and PB1's interrupt is re-enabled.
;
;   NOTE that conditional compilation is used in this code to determine that correct
;   code, timing, and other parameters will be used for the AVR device being targeted.
;   See "Usage info" for an explanation of how this is to be used.
;
; Burning info::
;
; ***    LOW fuse byte:  0x6A
; ***    HIGH fuse byte: 0xFF
; ***    EEPROM:         Not used
;
; Usage info::
;
;   ASSEMBLING
;   ==========
;
;   This is AVR Assembly code, written using the GNU "as" assembler syntax, and
;   specifically "avr-as" as is installed with avr-gcc. It has been designed to
;   target AVR MCUs in the "ATtiny" range according to which ONE of the following
;   symbols is defined as non-zero:
;
;       *   target_t13  => ATtiny13 or ATtiny13A
;       *   target_t85  => ATtiny85
;
;   Assembling for, say, the ATtiny85 can be driven by the command-line as follows:
;
;       avr-as ptouchavr.asm -mmcu=attiny13a -o ptouchavr.o --defsym target_t13=1
;
;   NOTE that this specifies the target via the trailing option:  --defsym target_t13=1
;   but you can alternatively force this in the code (if you really want to) by
;   uncommenting one of these lines:
;
;       .equ target_t13, 1  ; Target ATtiny13(A)
;       .equ target_t85, 1  ; Target ATtiny85
;
;   BUILDING THE CIRCUIT
;   ====================
;
;   It is wise to ensure there is a 100nF bypass capacitor strapped between
;   VCC and GND of the MCU.
;
;   To avoid accidentally asserting any lines of the TPH before the MCU reaches
;   a stable state, pull-up resistors (say, 4.7k) should be used on the
;   DATA, /LATCH, and especially /STROBE pins that interface with the TPH.
;
;   The MCU should be powered directly from the 3.3V supply used by the rest of
;   the P-Touch controller PCB, to ensure there are no unexpected voltage
;   differences from another PSU or separate regulator.
;
;   The pins of the MCU have been used as follows, depending on whether you're
;   using an ATtiny13 or ATtiny25/45/85:
;
;   | ATtiny13(A)  | ATtiny85     | TPH           |                                      |
;   | Pin | Port   | Pin | Port   | Pin | Name    | Purpose                              |
;   |-----|--------|-----|--------|-----|---------|--------------------------------------|
;   | -   | -      | -   | -      | 1   | Vheater | +9V, supplied by PT-1010             |
;   | 5   | PB0    | 5   | PB0    | 2   | DATA    | SPI data (Input, at TPH)             |
;   | 7   | PB2    | 6   | PB1    | 3   | CLK     | SPI clock (Input, at TPH)            |
;   | 3   | PB4    | 3   | PB4    | 4   | /LATCH  | Data buffer latch (active low)       |
;   | 8   | VCC    | 8   | VCC    | 5   | VCC??   | +3.3V, supplied by PT-1010           |
;   | 4   | GND    | 4   | GND    | 6   | GND     | Common ground                        |
;   | 4   | GND    | 4   | GND    | 7   | GND     | Physically connected to TPH pin 6    |
;   | 2   | PB3    | 2   | PB3    | 8   | /STROBE | Heater on (active low); 5.7ms max!   |
;   | 8   | VCC    | 8   | VCC    | 9   | VCC??   | +3.3V, supplied by PT-1010           |
;   | -   | -      | -   | -      | 10  | Vheater | +9V, supplied by PT-1010             |
;   | 6   | PB1    | 7   | PB2    | -   | -       | SENSE -> INT0 external interrupt     |
;   | 1   | /RESET | 1   | /RESET | -   | -       | MCU External RESET                   |
;
;   NOTE: One of the TPH's VCC pins (5 and 9) might not be DEFINED as a supply,
;   but it has been shown to be hard-wired directly to VCC anyway inside the PT-1010.
;   In other devices that use this Kyocera TPH, one of the two pins may not be able
;   to source enough current to power the MCU, so be careful.

;   Pin 1 of the MCU (/RESET or PB5) is configured to work as an external RESET,
;   but this can be tied high and the MCU will reset itself internally at power-on.
;   Later it could be another control or input line, but once we do that the MCU can't
;   be re-programmed without a "high voltage programmer".
;
;   QUICK VISUAL TEST
;   =================
;
;   If you have an LED and 470-ohm resistor in series between the /STROBE (PB4, pin 3)
;   line and VCC, then the LED should be off when the MCU is initially powered on.
;   If you assert "SENSE" (PB1 for ATtiny13A, or PB2 for ATtiny85) by shorting the
;   respective MCU pin momentarily to GND, the line print routine should kick in, and the
;   LED should be lit (at mid brightness) for about 1 second per 64 lines
;   (since 64 x 14.4ms ~= 0.9 seconds). If it's lit for at least double the
;   expected timeframe it means the INT0 interrupt flagged twice, back to back.
;   This ideally shouldn't happen since:
;
;   1.  The very start of the INT0 ISR should disable INT0, so that it no longer gets
;       flagged until we're ready;
;   2.  The end of the line printing test should clear the INT0 sense flag, so that
;       asserting it a 2nd time during the print run will have no effect
;       anyway even if point 1 fails;
;   3.  Even if it stays asserted the whole time, INT0 is only ever set to trigger on
;       a falling edge, rather than when the pin is simply at a low level.
;

; Check which MCU we're targeting and turn it into a number in "ATTINY":
.ifdef target_t13
    .print "*** Targeting ATtiny13(A)"
    .print "* Desired calibrated Internal RC Clock: 9.6MHz"
    .equ ATTINY, 13
.endif
.ifdef target_t85
    .print "*** Targeting ATtiny85"
    .print "* Desired calibrated Internal RC Clock: 8.0MHz"
    .equ ATTINY, 85
.endif

.if (ATTINY==13)
    .include "lib/t13.asm"
.elseif (ATTINY==85)
    .include "lib/t85.asm"
.else
    .error "Unknown target!"
.endif
.include "lib/macros.asm"

; Define which pins are used for which inputs of the TPH:
.if (ATTINY==13)
    .equ TPH_STROBE,    PB3
    .equ TPH_LATCH,     PB4
    .equ TPH_DATA,      PB0
    .equ TPH_CLK,       PB2
    .equ SENSE,         PB1     ; ATtiny13 has INT0 on PB1.
.elseif (ATTINY==85)
    .equ TPH_STROBE,    PB3
    .equ TPH_LATCH,     PB4
    .equ TPH_DATA,      PB0
    .equ TPH_CLK,       PB1
    .equ SENSE,         PB2     ; ATtiny85 has INT0 on PB2.
.endif
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
.if (ATTINY==13)
    .equ OSCCAL_TARGET, 0x6F    ; OSCCAL value of 0x6F seems to be closest to 9.6MHz on my ATtiny13A at 3.3V.
    .equ ACTIVE_TIMEOUT,54      ; The "Line ACTIVE window", counted in multiples of 1024 CPU clocks.
                                ; NOTE: /STROBE is not asserted for roughly first 0.15ms of this time;
                                ; /STROBE is asserted for ROUGHLY (54*1024-1500)/9600000 seconds => 5.6ms.
    .equ IDLE_TIMEOUT,  79      ; The "Line IDLE window".
    ; NOTE: (ACTIVE_TIMEOUT+IDLE_TIMEOUT)*1024/9600000 should give about 14.2ms per line, +/- clock error.
.elseif (ATTINY==85)
    .equ OSCCAL_TARGET, 0x6F    ; OSCCAL value of 0x6F seems to be closest to 8.0MHz on my ATtiny85 at 3.3V.
    .equ ACTIVE_TIMEOUT,45      ; The "Line ACTIVE window", counted in multiples of 1024 CPU clocks.
                                ; NOTE: /STROBE is not asserted for roughly first 0.18ms of this time;
                                ; /STROBE is asserted for ROUGHLY (45*1024-1500)/8000000 seconds => 5.57ms.
    .equ IDLE_TIMEOUT,  66      ; The "Line IDLE window" => 8.45ms
    ; NOTE: (ACTIVE_TIMEOUT+IDLE_TIMEOUT)*1024/8000000 should give about 14.2ms per line, +/- clock error.
.endif


; ------------------------------- Interrupt table -------------------------------;

; Starts at 0x0000. ATtiny13A has 10 interrupt vectors (inc. RESET),
; while ATtiny85 has 15, and they're aligned a little differently.
.org 0x0000
firmware_top:
; Reset vector comes first:
    rjmp init                   ; RESET
    rjmp int0_isr               ; EXT_INT0                  External Interrupt Request 0
    reti                        ; PCINT0                    Pin Change
.if (ATTINY==85)
    reti                        ; TIM1_COMPA    (ATtiny85)  Timer/Counter 1 Compare Match A
    reti                        ; TIM1_OVF      (ATtiny85)  Timer/Counter 1 Overflow
.endif
    reti                        ; TIM0_OVF                  Timer Overflow
    reti                        ; EE_RDY                    EEPROM Ready
    reti                        ; ANA_COMP                  Analog Comparator
.if (ATTINY==85)
    reti                        ; ADC           (ATtiny85)  ADC Conversion Complete
    reti                        ; TIM1_COMPB    (ATtiny85)  Timer/Counter 1 Compare Match B
.endif
; "Timer Compare Match A" interrupt does special handling depending on which step
; of the repeating 14.4ms cycle it has reached. 
    ijmp  ; Jump to Z address   ; TIM0_COMPA                Timer Compare Match A
    reti                        ; TIM0_COMPB                Timer Compare Match B
    reti                        ; WDT                       Watchdog Timeout
.if (ATTINY==13)
    reti                        ; ADC           (ATtiny13)  ADC Conversion Complete
.elseif (ATTINY==85)
    reti                        ; USI_START     (ATtiny85)  USI START
    reti                        ; USI_OVF       (ATtiny85)  USI Overflow
.endif



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
    ldiw_data GR, pixel_data

    ; Set up the number of lines that are defined in pixel_data
    ; (i.e. no. of bytes in the data table, divided by 8):
    ldiw LCR, (end_pixel_data-pixel_data)>>3

    ; Set up the Z (16-bit) pointer to be an address pointer to the
    ; "Line ACTIVE Window" ISR. Basically, the first time-out will call the ISR
    ; which loads line data and fires the heater of the TPH.
    ldiw_code ZR, line_active_isr

    ; Configure timer to generate an interrupt after IDLE_TIMEOUT counts (~8.5ms).
    init_simple_timer clk_1024, IDLE_TIMEOUT
    ; NOTE: The timer should now be started.
    ; NOTE: Interrupts are globally enabled, automatically, after RETI:
    reti



; ------------------------------- Line ACTIVE Window ISR -------------------------------;

; This ISR (Interrupt Service Routine) handles TIM0_COMPA after the ~8.5ms delay of the
; "Line IDLE Window ISR". It writes data for the next line to print, with the following
; routine:
;
;   1.  Change timeout to ~5.6ms, setting the total duration of our
;       Line ACTIVE Window (i.e. 5.6ms before jumping to the "Line IDLE Window" ISR).
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
;   Since the timer is running the whole time, it will time out exactly since
;   the VERY START of this ISR. /STROBE will hence remain asserted -- being reset only
;   by the "Line IDLE Window" ISR -- after approx. 5.87ms MINUS the time it takes to
;   execute all of steps 1--7 (which has been shown to be about 170uS). Hence, /STROBE
;   will be asserted for about a total of 5.6ms.
;

line_active_isr:
    ; Raise CLK for (a):
    sbi PORTB, TPH_CLK      ; 2K

    ; OK, so CLK has JUST gone high at this point...
    ; If we now count the cycles between here and the moment it next goes
    ; low (just after the OUT spi_begin_bit), we should count 86...

    ; (a) Early delay, with CLK high:
    ; Point Z to start of this line's data:
    movw ZR, GR             ; 1K
    ; Set byte count to 8:
    ldi BYTE_COUNT, 8       ; 1K

next_byte:

    ; Set bit count to 8:
    ldi BIT_COUNT, 8        ; 1K
    ; Load a byte that we need to push out, incrementing Z at the same time:
    lpm DATA_BUFFER, Z+     ; 3K

next_bit:

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
    wait_2                  ; 2K
    ; Write register-buffered pin states to PORTB, hence setting CLK and DATA states simultaneously:
    out PORTB, OUT_BUFFER   ; 1K

    ; Now wait 10K and make CLK go high:
    wait_8                  ; NOTE: Could do calculations here instead.
    sbi PORTB, TPH_CLK      ; 2K

    ; Now check if we have any more bits left...
    dec BIT_COUNT           ; 1K
    brne next_bit           ; 2K    Branch if more bits are left.

    ; Byte is finished. More bytes to go?
    dec BYTE_COUNT          ; 1K
    brne next_byte          ; 2K    Branch if more bytes are left.

    ; Pull CLK low again.
    wait_4                  ; 4K
    cbi PORTB, TPH_CLK      ; 2K

    ; Wait out R (80K), and then pulse /LATCH for 20K
    micro_delay 26          ; 78K
    cbi PORTB, TPH_LATCH    ; 2K
    micro_delay 6           ; 18K
    sbi PORTB, TPH_LATCH    ; 2K

    ; Wait out W (23K) and then lower /STROBE
    micro_delay 7           ; 21K
    cbi PORTB, TPH_STROBE   ; 2K

    ; At this point, /STROBE is now lowered, so prepare the next stage...

    ; Save current pointer to the first byte of the next line (Z), in GR:
    movw GR, ZR
    ; Change ORC0A to ACTIVE_TIMEOUT, so that the next time the interrupt fires,
    ; it will be after our CURRENT window has elapsed, and we're ready to
    ; bring /STROBE high again.
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
    ; because it can't actually be asserted in software, and this ensures that
    ; by writing a ZERO we don't have to touch any of the other bits:
    ldi r16, M_INTF0
    out GIFR, r16
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

    ; Import the hackaday.com logo (after conversion with ptconvert.rb):
    .incbin "images/had-skull2.raw"

end_pixel_data:

