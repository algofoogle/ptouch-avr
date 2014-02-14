; Title::     P-Touch AVR
; Author::    Anton Maurovic <anton.maurovic@gmail.com>
; URL::       http://anton.maurovic.com/
;             http://github.com/algofoogle/ptouch-avr
;
; Description::
;
;   ...TBC...
;
; Burning info::
;
; ***    LOW fuse byte:  0x6A
; ***    HIGH fuse byte: 0xFF
; ***    EEPROM:         Not used
;
; Usage info::
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

.include "t13.asm"
.include "macros.asm"

; Define which pins are used for which inputs of the TPH:
.equ TPH_STROBE,    PB4
.equ TPH_LATCH,     PB3
.equ TPH_DATA,      PB0
.equ TPH_CLK,       PB2
.equ SENSE,         PB1 ; NOT USED YET.
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
.equ LCR,           22      ; Name for use with SBIW.
.equ LCL,           22
.equ LCH,           23
; Graphics data pointer:
.equ GR,            24      ; Name for use with MOVW.
.equ GL,            24
.equ GH,            25

; ------------------------------- Interrupt table -------------------------------;

; Starts at 0x0000. ATtiny13A has 10 interrupt vectors (inc. RESET):
.org 0x0000
firmware_top:
; Reset vector comes first:
    rjmp init
; Interrupt no. 7 (Timer Compare Match A) does special handling depending on which step
; of the repeating 14.4ms cycle it has reached. The other 8 interrupts all just return
; without doing anything.
    reti                        ; Interrupt Vector 2   = EXT_INT0   (External Interrupt Request 0)
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
    ; I've determined that an OSCCAL value of 0x6E gives the best approximation
    ; of 9.6MHz on my ATtiny13A at 5V.
    slide_osccal 0x6E

    ; Make the CPU clock run at full speed:
    disable_clock_prescaler

    init_stack

    ; From power-on, the MCU's pins should all be inputs. Before we change them to
    ; outputs, we want to make sure they'll assert the correct states.
    ; /STROBE, /LATCH, and DATA should all be high. The rest should be low:
    ldi r17, M_TPH_STROBE | M_TPH_LATCH | M_TPH_DATA
    out PORTB, r17

    ; Set all pins (except SENSE) to be outputs:
    ldi r16, ~SENSE
    out DDRB, r16
    nop ; Synchronise.

    ; Output prior pin state again, just in case it was lost when changing DDRB:
    out PORTB, r17

    ; Set up our 'G' pointer to point to the start of our pixel data block, as
    ; stored in Program Memory, beginning at the 'pixel_data' label.
    ldi GH, hi8(pixel_data)
    ldi GL, lo8(pixel_data)

    ; Set up the number of lines that are defined in pixel_data:
    ldi LCH, (end_pixel_data-pixel_data)>>11
    ldi LCL, ((end_pixel_data-pixel_data)>>3) & 0xFF

    ; Set up the Z (16-bit) pointer to be an address pointer to the
    ; "Line ACTIVE Window" ISR. Basically, the first time-out will call the ISR
    ; which loads line data and fires the heater of the TPH.
    ldi ZH, pm_hi8(line_active_isr)
    ldi ZL, pm_lo8(line_active_isr)

    ; Configure timer to generate an interrupt after 80 counts (or 8.53ms).
    ; That is, after 80/(CLK/1024) seconds =>
    ;   80/(9600000/1024) = 0.0085333... ~= 8.53ms
    ;
    init_simple_timer clk_1024, 80
    ; NOTE: The timer has now started.

sleep_loop:
    ; Enable "Idle" sleep mode:
    enable_sleep
    ; Sleep -- basically, halt the CPU and let interrupts fire when required:
    sleep
    ; I think we get here when a RETI occurs from within an ISR.
    rjmp sleep_loop


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
    ; /LATCH=1; /STROBE=1; CLK=0; DATA=C; SENSE=0.
    ldi OUT_BUFFER, M_TPH_LATCH | M_TPH_STROBE ; 1K
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

    ; At this point, /STROBE is now lowered, and we can update:
    ;   *   GR <- Load with the value now in Z.
    ;   *   Z <- Point to "Line IDLE Window" ISR.
    ;   *   Change OCR0A to 55.

;; ..................



    
; This is an ISR that handles 
; (i.e. interrupt generated when when Timer/Counter 0 hits a given limit
; in OCR0A). This is currently configured to fire every 2ms.
; 
; This ISR uses the Z pointer with a jump table to jump to the stage it
; currently needs to process. After that, it increments (or resets) the
; Z pointer as required. These are the events that happen at specific
; times in the cycle:
;
;   *   After 2ms (1 interrupt), while PB0 is high, we want to do some
;       proof-of-concept "work".
;   *   After 10ms (4 more interrupts), we want to pull PB0 low.
;   *   After 14ms (2 more interrupts), we want to pull PB0 high again
;       and reset the interrupt counter.
;
; For the time while we're NOT in this ISR, the main program is just in SLEEP
; mode, so the MCU is idling. We COULD potentially do other work during this
; time but I have nothing for it to do at the moment.
;
timer_isr:
    ; Jump to whichever point in the jump table is currently indicated by
    ; the Z pointer. The landing address represents a stage of the 7
    ; stages of the cycle.
    ijmp                        ; 2 cycles.

_timer_isr_jumps:
    ; This is a jump table. Depending on how many times the interrupt has
    ; fired already, the IJMP above will land on one of these 7 slots --
    ; -- the 7th will reset the Z pointer to the start of the jump table.
    ; Note that each RJMP here adds 2 cycles to the ISR lead-in time:
                                ; 2 cycles each...
    rjmp _timer_isr_act_toggle  ; 1st interrupt (2ms passed): PB1 -> high.
    rjmp _timer_isr_step        ; 2: Nothing to do;           PB1 still high.
    rjmp _timer_isr_act_toggle  ; 3rd interrupt (6ms passed): PB1 -> low.
    rjmp _timer_isr_do_work     ; 4th interrupt (8ms passed): Do some work.
    rjmp _timer_isr_10ms        ; 5th interrupt (10ms passed): pull PB0 low.
    rjmp _timer_isr_step        ; 6: Nothing to do.
    .if ((. - firmware_top)>>1 >= 0x0100)
        ; Our implementation doesn't use ZH for the jump table, so we can't
        ; have any part of the jump table beyond PC address 0x0100:
        .error "_timer_isr's jump table is not located wholly under 0x0100 address"
    .endif
_timer_isr_end_cycle:
    ; This is the landing address of the 7th interrupt of the cycle, having
    ; reached the 14ms mark...
    ; NOTE: The next 2 instructions sync I/O with all other execution
    ; paths by matching the same CPU cycle count as for one of the RJMPs above.
    nop                                 ; 1 cycle.
    ; Reset Z pointer to the start of the jump table:
    ldi ZL, pm_lo8(_timer_isr_jumps)    ; 1 cycle.
    ; 14ms has elapsed...
    ; Pull PB0 high again:
    sbi PORTB, PB0
    ; Exit the ISR:
    reti

_timer_isr_act_toggle:
    ; When PB1 is not being used as the DATA channel in _timer_isr_do_work,
    ; it is being driven high for a period of 4ms out of 14ms. This is so it
    ; can drive an indicator LED, which shows that we're alive and kicking.
    ; This "toggle" event is called twice per each 14ms cycle -- once at the
    ; 2ms mark to turn the LED on, and once again at 6ms to turn it off.
    sbi PINB, PB1
    rjmp _timer_isr_step

_timer_isr_do_work:
    ; 2ms has elapsed...

    ; This routine reads a sequence of 8 bytes from within the Flash ROM
    ; Program Memory of the MCU, and writes each bit of each
    ; byte out -- MSB first -- using a 200kHz SPI approach.
    ; That is, each bit is clocked at 5us intervals. There is a gap
    ; of 20us between the LSB of one byte, and the MSB of the next.
    ; If you add that all up:
    ;   (8 bits * 5us + 20us) * 8 bytes
    ;   => The cycle is complete in 480us.

    ; Push Z onto the stack, because we need to mess with it here...
    push ZL
    push ZH     ; NOTE: We could leave this out, and just
                ; assume that ZH will stay at 0, always, or
                ; at least verify that it *starts* at 0 before
                ; the loop, and then reset it to 0 after the
                ; loop is done (i.e. "clr ZH" instead of "pop ZH").
                ; This would shave at least 2 instructions,
                ; and save 1 byte of stack (SRAM).
                ; IN FACT, we know what the next step will be
                ; in the jump table, so we could load its address
                ; directly and even do away with the PUSH/POP
                ; instructions completely:
                ; * Delete 2xPUSH
                ; * Delete "LDI ZH"
                ; * Change final 2xPOP to 2xLDI.
                ; ...which will save 3 instructions.
    ; Now point Z to the data bytes (using BYTE addresses)
    ; that we have in Program Memory:
    ldi ZL, lo8(data)
    ldi ZH, hi8(data)
    ; We'll push out 8 bytes in total:
    ldi r23, 8
next_byte_loop:
    ; Load a byte from the data bytes we have in Program Memory,
    ; incrementing the Z pointer afterwards:
    lpm r21, Z+                     ; 3 cycles.
    ; Write out one bit at a time on PB1, starting with MSB...
    ; 8 bits to shift out...
    ldi r22, 8                      ; 1 cycle.
next_bit_loop:
    ; Rotate left, pushing MSB into Carry.
    rol r21                         ; 1 cycle.
    brcs out_hi_bit                 ; 1 cycle if bit 0, 2 if bit 1.
    ; Bit is 0, so clear it on PB1:
    nop                             ; 1 cycle (sync).
    cbi PORTB, PB1                  ; 1 cycle.
    rjmp bit_loop_check             ; 2 cycles.
out_hi_bit:
    ; Bit is 1, so set it on PB1:
    sbi PORTB, PB1                  ; 1 cycle.
    nop                             ; 1 cycle (sync).
    nop                             ; 1 cycle (sync).
bit_loop_check:
    ; By this point, 6 cycles have been spent
    ; (3 since commencing the SBI or CBI instruction).
    ; We want to hold the PB1 (last output bit) state for 5us
    ; (48 cycles, minus the overhead built in to each iteration of the loop):
    ;   RequiredDelay = 48 - LoopHead - LoopTail
    ;   = 48 - 6 - 3
    ;   = 39 cycles we need to pad out:
    precise_delay 12, 1             ; 3*(12+1)*1 = 39 cycles.
    ; Check if we have more bits to loop thru, for this current byte:
    dec r22                         ; 1 cycle.
    brne next_bit_loop              ; 2 cycles.
    ; When we reach THIS point, outside of the loop, 44 cycles have elapsed
    ; since the last SBI or CBI instruction. We need to pad it out to 48
    ; so that the final bit (of this byte) definitely lasts 5us.
    nop
    nop
    nop
    nop
    ; Now we need to pull PB1 low (if it isn't already) and keep it there
    ; for 20us (192 cycles) before the first bit of the next byte is
    ; output. From cycle counting, we can tell that from the moment
    ; PB1 drops low again (at this next instruction), there are 11 cycles
    ; of overhead before the first bit of the NEXT byte can be output,
    ; so if we want precise timing we need to pad with a delay of
    ; 192-11=181 cycles.
    cbi PORTB, PB1                  ; 1 cycle.
    precise_delay 59, 1             ; 3*(59+1)*1 = 180 cycles.
    nop                             ; 1 cycle.
    ; Check if we have more bytes to pump out...
    dec r23                         ; 1 cycle.
    brne next_byte_loop             ; 2 cycles (unless all done).
    ; All bytes are done. Bring back the original Z pointer
    ; (jump table index) value from the stack:
    pop ZH
    pop ZL
    ; Exit the ISR after incrementing the Z pointer:
    rjmp _timer_isr_step

_timer_isr_10ms:
    ; 10ms has elapsed...
    ; Make PB0 go low.
    cbi PORTB, PB0

_timer_isr_step:
    ; Increment the Z pointer (as a 16-bit operation):
    inc ZL
_timer_isr_exit:
    reti




pixel_data:

    ; Very fine pitch:
    .byte 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101
    .byte 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101
    .byte 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101
    .byte 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101, 0b01010101
        ; Alternate:
    .byte 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010
    .byte 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010
    .byte 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010
    .byte 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010, 0b10101010

    ; Fine pitch:
    .byte 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011
    .byte 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011
    .byte 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011
    .byte 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011, 0b00110011
        ; Alternate:
    .byte 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100
    .byte 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100
    .byte 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100
    .byte 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100, 0b11001100

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

end_pixel_data:
