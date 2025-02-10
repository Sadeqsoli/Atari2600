    processor 6502
    seg Code        ; Define a new assembler segment named "Code"
    org $F000       ; Define the origin of the ROM code at address $F000

Start:
    cld             ; Make sure we disable the decimal BCD mode of the CPU
    
    lda #10         ; Load the A register with the decimal value 10
    sta $80         ; Store the value from A into memory position $80
    
    inc $80         ; Increment the value inside a (zero page) memory position
    dec $80         ; Decrement the value inside a (zero page) memory position
    
    jmp Start       ; Jump to the Start label to force an infinite loop

    org $FFFC       ; End the ROM always at position $FFFC
    .word Start     ; Put 2 bytes with reset address at memory position $FFFC
    .word Start     ; Put 2 bytes with break address at memory position $FFFE