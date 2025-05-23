	processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include external files containing useful definitions and macros 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	include "vcs.h"
	include "macro.h"

	seg code
	org $F000      ; Define the origin of the ROM at $F000
	
START:
	CLEAN_START    ; Call macro to safely clear the memory

AfterCleanMem:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set background luminance color to yellow (NTSC color code $1E)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;About NTSC colors: https://en.wikipedia.org/wiki/Television_Interface_Adaptor

    lda #$1E       ; Load color code into A register
    sta COLUBK     ; Store A to memory address $09 background color (TIA COLUBK)

    jmp AfterCleanMem      ; Repeat from START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill ROM size to exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC      ; Defines origin to $FFFC
    .word START    ; Reset vector at $FFFC (where program starts)
    .word START    ; Interrupt vector at $FFFE (unused by the VCS)
