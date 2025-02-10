    processor 6502
    seg Code        ; Define a new assembler segment named "Code"
    org $F000       ; Define the origin of the ROM code at address $F000

Start:
    lda #$A         ; Load the A register with the hexadecimal value $A
    ldx #%11111111  ; Load the X register with the binary value %11111111
    
    sta $80         ; Store the value in the A register into memory address $80
    stx $81         ; Store the value in the X register into memory address $81
    
    jmp Start       ; Jump to the Start label to force an infinite loop

    org $FFFC       ; End the ROM always at position $FFFC
    .word Start     ; Put 2 bytes with reset address at memory position $FFFC
    .word Start     ; Put 2 bytes with break address at memory position $FFFE