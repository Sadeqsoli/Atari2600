    processor 6502

    seg code
    org $f000       ; define the code origin at f000

Start:
    sei             ; disable interrupts
    cld             ; disable the BCD decimal math mode
    ldx #$ff        ; load the x register with literal numeber
    txs             ; transfer the x register to the stack pointer.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; clear the page zero of my memory region ($00 to $ff)           ;
; menaing the entire ram and also the entire TIA register        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #0
    ldx #$ff

MemLoop:

    sta $0,x        ; store the value a inside the mem address $0 + x
    dex             ; x--
    bne MemLoop     ; back to line of lable memloop until x is 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Fill the ROM size to exactly 4kb                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    org #$fffc
    .word Start     ; rest the program at #fffc where the program starts
    .word Start     ; interupt vector at #fffe (unused in the vcs)

    




    
