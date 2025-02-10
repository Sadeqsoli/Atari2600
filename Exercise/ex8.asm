    processor 6502
    seg Code        ; Define a new assembler segment named "Code"
    org $F000       ; Define the origin of the ROM code at address $F000

Start:
    ldy #10         ; Load the Y register with the decimal value 10
    
Loop:
    tya             ; Transfer Y to A
    sta $80,Y       ; Store the value inside A into memory position $80+Y
    dey             ; Decrement Y
    bpl Loop        ; Branch if plus (result of last instruction was positive)
    
    jmp Start       ; Jump to the Start label to force an infinite loop

    org $FFFC       ; End the ROM always at position $FFFC
    .word Start     ; Put 2 bytes with reset address at memory position $FFFC
    .word Start     ; Put 2 bytes with break address at memory position $FFFE