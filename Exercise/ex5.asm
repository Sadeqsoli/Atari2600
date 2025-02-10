    processor 6502
    seg Code        ; Define a new assembler segment named "Code"
    org $F000       ; Define the origin of the ROM code at address $F000

Start:
    cld             ; Make sure the Decimal mode is cleared (disabled)
    lda #$A         ; Load the A register with the hexadecimal value $A
    ldx #%1010      ; Load the X register with the binary value %1010
    
    sta $80         ; Store the value in the A register into memory address $80
    stx $81         ; Store the value in the X register into memory address $81
    
    lda #10         ; Load A with the decimal value 10
    
    clc             ; Clear the carry flag before ADC
    adc $80         ; Add to A the value inside RAM address $80
    adc $81         ; Add to A the value inside RAM address $81
                    ; A should contain (#10 + $A + %1010) = #30 (same as $1E)
                    
    sta $82         ; Store the value inside A into RAM position $82
    
    jmp Start       ; Jump to the Start label to force an infinite loop

    org $FFFC       ; End the ROM always at position $FFFC
    .word Start     ; Put 2 bytes with reset address at memory position $FFFC
    .word Start     ; Put 2 bytes with break address at memory position $FFFE