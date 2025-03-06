    processor 6502

    INCLUDE "vcs.h"
    INCLUDE "macro.h"

;code segment
    seg code 
    org $f000

Reset:
    CLEAN_START
    ldx #$FF
    txs

;add/push four values to the stack

    lda #$AA

    PHA
    PHA
    PHA
    PHA

;Pull four values from stack to the accumulator

    PLA
    PLA
    PLA
    PLA

;Fill the ROM to the 4kb
    org $fffc
    .word Reset
    .word Reset
    
