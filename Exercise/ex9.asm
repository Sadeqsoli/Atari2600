    processor 6502
    seg Code        ; Define a new assembler segment named "Code"
    org $F000       ; Define the origin of the ROM code at address $F000

Start:
    lda #1          ; Load the A register with the decimal value 1

Loop:
    clc
    adc #1          ; Increment A (using ADC #1)
    cmp #10         ; Compare the value in A with the decimal value #10
    bne Loop        ; Branch if the comparison was not equals (to zero)
    
    jmp Start       ; Jump to the Start label to force an infinite loop

    org $FFFC       ; End the ROM always at position $FFFC
    .word Start     ; Put 2 bytes with reset address at memory position $FFFC
    .word Start     ; Put 2 bytes with break address at memory position $FFFE