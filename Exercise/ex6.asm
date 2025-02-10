    processor 6502
    seg Code        ; Define a new assembler segment named "Code"
    org $F000       ; Define the origin of the ROM code at address $F000

Start:
    lda #1         ; Load the A register with the decimal value 1
    ldx #2         ; Load the X register with the decimal value 2
    ldy #3         ; Load the Y register with the decimal value 3
    
    inx             ; Increment X
    iny             ; Increment Y
    
    clc
    adc #1          ; Add 1 to A (The 6502 has no "inc A" instruction)

    dex             ; Decrement X
    dey             ; Decrement Y
    
    sec
    sbc #1          ; Subtract 1 from A (The 6502 has no "dec A" instruction)

                    ; In the 6502, we can directly increment and decrement both
                    ; X and Y, but not A. We can only ADC #1 and SBC #1 from A.
                    ; This makes X and Y great choices to control loops.
    
    jmp Start       ; Jump to the Start label to force an infinite loop

    org $FFFC       ; End the ROM always at position $FFFC
    .word Start     ; Put 2 bytes with reset address at memory position $FFFC
    .word Start     ; Put 2 bytes with break address at memory position $FFFE