; ATtiny25/45/85 is basically a superset of ATtiny13:
.include "lib/t13.asm"

; Memory map of ATtiny85:
;   *   Program memory: 0x0000 - 0x0FFF (4096 16-bit WORDS => 8kBytes).
;   *   Data memory:
;       *   32 Registers (R0-R31):  0x0000 - 0x001F (32 bytes).
;       *   64 I/O Registers:       0x0020 - 0x005F (64 bytes).
;       *   512 SRAM bytes:         0x0060 - 0x025F (512 bytes).
;   *   Hence, RAMEND (typically used as top of stack) is 0x009F.
.equ RAMEND, 0x025f

.equ SPH, 0x3e
.equ TIMSK, 0x39   		; Timer Interrupt Mask: NOTE: DIFFERENT bit layout from TIMSK0 of ATtiny13!!
.equ TIMSK0, 0x1FFFF	; This should prevent code from accidentally using TIMSK0 (which is only valid on ATtiny13).
.equ TCCR0A, 0x2a   	; Timer Control Register A.
.equ OCR0A, 0x29    	; Timer/Counter Output Compare A.

; TIMSK bits:
.equ OCIE0A,        4               ; Timer/Counter0 Output Compare Match A Interrupt Enable
.equ M_OCIE0A,      (1<<OCIE0A)
