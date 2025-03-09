    processor 6502

    ;including the required files
    INCLUDE "vcs.h"
    INCLUDE "macro.h"

    ;declare variables starting from the variable address $80
    seg.u Variables
    org $80

;player0 position
JetXPos byte
JetYPos byte
;player1 position
EnemyXPos byte
EnemyYPos byte

;code segment
    seg code
    org $f000

Reset:
    CLEAN_START     ;fresh start in memory

    ;Initialize RAM variables and TIA registers
    lda #10
    sta JetYPos     ;jet position at pixel 10 {Y}

    lda #60
    sta JetXPos     ;jet position at pixel 60 {X}

;start scanlines

    