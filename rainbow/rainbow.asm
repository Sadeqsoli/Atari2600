    processor 6502

    INCLUDE "macro.h"
    INCLUDE "vcs.h"


    seg code
    org $f000
Start:
    CLEAN_START         ;macro to safely clear the memory and TIA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start a new frame by turning VSync and VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

NextFrame:
    lda #2              ; storing 2 (dec) in A register
    sta VSYNC           ; Turn on Vsync
    sta VBLANK          ; Turn on Vblank

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate the 3 lines of VSync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    sta WSYNC           ; First Scanline
    sta WSYNC           ; Second Scanline
    sta WSYNC           ; Third Scanline

    lda #0
    sta VSYNC           ;Turning off the VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; outputing the 37 line of scanlines for VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ldx #37
VBlankLoop:
    sta WSYNC               ; hit WSYNC and wait for the next scanline
    dex                     ;X--
    bne VBlankLoop     ;loop while X != 0

    lda #0
    sta VBLANK              ; Turning off VBlank

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw the main display for our game's scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ldx #192
MainLopp:
    stx COLUBK          ;set the background color
    sta WSYNC           ;wait for the next scanline
    dex 
    bne MainLopp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; outputing the 30 line of scanlines for Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK

    ldx #30
OverscanLoop:
    sta WSYNC
    dex 
    bne OverscanLoop


   jmp NextFrame 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete the ROM size to 4k
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

    org $fffc
    .word Start
    .word Start