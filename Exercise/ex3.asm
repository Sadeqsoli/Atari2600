    processor 6502
    seg Code        ; Define a new assembler segment named "Code"
    org $F000       ; Define the origin of the ROM code at address $F000

Start:
    lda #15         ; Load the A register with the literal decimal value #15
    
    tax             ; Transfer the value from A to X
    tay             ; Transfer the value from A to Y
    txa             ; Transfer the value from X to A
    tya             ; Transfer the value from Y to A
    
                    ; Important!!! There is no TXY or TYX.
                    ; Therefore, we can't transfer directly X to Y or Y to X.
                    ; If we wish to do so, we must go through the A register.
    
    ldx #6          ; Load X with the decimal value #6
    txa             ; Transfer X to A
    tay             ; Transfer A to Y
    
    jmp Start       ; Jump to the Start label to force an infinite loop

    org $FFFC       ; End the ROM always at position $FFFC
    .word Start     ; Put 2 bytes with reset address at memory position $FFFC
    .word Start     ; Put 2 bytes with break address at memory position $FFFE