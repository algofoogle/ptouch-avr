;;; ------- Constants for the ATtiny13 -------

; Memory map of ATtiny13:
;   *   Program memory: 0x0000 - 0x01FF (512 16-bit WORDS => 1kByte).
;   *   Data memory:
;       *   32 Registers (R0-R31):  0x0000 - 0x001F (32 bytes).
;       *   64 I/O Registers:       0x0020 - 0x005F (64 bytes).
;       *   64 SRAM bytes:          0x0060 - 0x009F (64 bytes).
;   *   Hence, RAMEND (typically used as top of stack) is 0x009F.
.equ RAMEND, 0x009F

.equ PORTB, 0x18    ; Port B output.
.equ DDRB,  0x17    ; Port B direction bits.
.equ PINB,  0x16    ; Port B input.
.equ OSCCAL, 0x31   ; Oscillator calibration.
.equ CLKPR, 0x26    ; Clock pre-scaler.
.equ SPL,   0x3d    ; Stack Pointer low byte (SPH not used on ATtiny13).
.equ MCUCR, 0x35    ; MCU Control Register.
.equ TIMSK0, 0x39   ; Timer 0 Interrupt Mask.
.equ GIMSK, 0x3b    ; General Interrupt Mask Register (INT0 & pin-change interrupts).
.equ GIFR,  0x3a    ; General Interrupt Flag Register.
.equ GTCCR, 0x28    ; General Timer/Counter Control Register.
.equ TCCR0A, 0x2f   ; Timer Control Register A.
.equ TCCR0B, 0x33   ; Timer Control Register B.
.equ OCR0A, 0x36    ; Timer/Counter Output Compare A.
.equ TCNT0, 0x32    ; Timer/Counter 0 value.
; Bit numbers of PORTB pins.
.equ PB0,   0
.equ PB1,   1
.equ PB2,   2
.equ PB3,   3
.equ PB4,   4
.equ PB5,   5
; Bit MASKS for PORTB pins:
.equ MPB0,  (1<<PB0)   ; 0b000001 => ATtiny13A pin 5
.equ MPB1,  (1<<PB1)   ; 0b000010 => ATtiny13A pin 6
.equ MPB2,  (1<<PB2)   ; 0b000100 => ATtiny13A pin 7
.equ MPB3,  (1<<PB3)   ; 0b001000 => ATtiny13A pin 2
.equ MPB4,  (1<<PB4)   ; 0b010000 => ATtiny13A pin 3
.equ MPB5,  (1<<PB5)   ; 0b100000 => ATtiny13A pin 1
; Half-words for X, Y, and Z 16-bit pointers:
.equ XR,    26  ; Name for use with MOVW
.equ XL,    26
.equ XH,    27
.equ YR,    28  ; Name for use with MOVW
.equ YL,    28
.equ YH,    29
.equ ZR,    30  ; Name for use with MOVW
.equ ZL,    30
.equ ZH,    31

; CLKPR bits (6.4.2):
.equ CLKPCE,        7               ; Clock Prescaler Change Enable
.equ M_CLKPCE,      (1<<CLKPCE)
.equ CLKPS_DIV1,    0b0000
.equ CLKPS_DIV2,    0b0001
.equ CLKPS_DIV4,    0b0010
.equ CLKPS_DIV8,    0b0011
.equ CLKPS_DIV16,   0b0100
.equ CLKPS_DIV32,   0b0101
.equ CLKPS_DIV64,   0b0110
.equ CLKPS_DIV128,  0b0111
.equ CLKPS_DIV256,  0b1000          ; NOTE: Values above this are reserved.

; GIMSK bits (9.3.2):
.equ INT0_EN,       6               ; INT0 enable.
.equ M_INT0_EN,     (1<<INT0_EN)

; MCUCR bits (9.3.1):
.equ M_ISC,         0b00000011      ; ISC (Interrupt Sense Control) bit mask.
.equ M_SM,          0b00011000      ; Sleep Mode mask.
.equ SE,            5               ; Sleep Enable.
.equ M_SE,          (1<<SE)

; GTCCR (General Timer/Counter Control Register) bits (12.4.1):
.equ TSM,           7               ; Timer/Counter Sync Mode.
.equ M_TSM,         (1<<TSM)
.equ PSR10,         0               ; Prescaler Reset Timer/Counter0
.equ M_PSR10,       (1<<PSR10)

; TCCR0A (11.9.1):
.equ COM0_NONE,     0b00000000      ; "Normal" (OC0A disconnected).
.equ COM0_TOGGLE,   0b01000000      ; Toggle OC0A on match.
.equ COM0_CLEAR,    0b10000000      ; Clear OC0A on match.
.equ COM0_SET,      0b11000000      ; Set OC0A on match.
.equ WGM_NORMAL,    0b000
.equ WGM_PWM,       0b001           ; PWM (Phase Correct) with 0xFF limit.
.equ WGM_CTC,       0b010           ; CTC with OCRA.
.equ WGM_FASTPWM,   0b011           ; Fast PWM with 0xFF limit.
.equ WGM_PWM_OCRA,  0b101           ; PWM (Phase Correct) with OCRA limit.
.equ WGM_FASTPWM_OCRA, 0b111        ; Fast PWM with OCRA limit.

; TIMSK0 (11.9.6):
.equ OCIE0A,        2               ; Timer/Counter0 Output Compare Match A Interrupt Enable
.equ M_OCIE0A,      (1<<OCIE0A)

; GIFR (9.3.3):
.equ INTF0,         6               ; INT0 asserted flag?
.equ M_INTF0,       (1<<INTF0)
