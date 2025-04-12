    processor 6502

    ;including the required files
    INCLUDE "vcs.h"
    INCLUDE "macro.h"

    ;declare variables starting from the variable address $80
    seg.u Variables
    org $80

;player0 position
PLXPos byte
PLYPos byte
;player1 position
ENXPos byte
ENYPos byte

;Player sprites
PLSpritePtr word      ;pointer to mem address of Player sprite also P0
PLColorPtr word    ;pointer to mem address of Player color also P0

ENSpritePtr word      ;pointer to mem address of Enemy sprite also P1
ENColorPtr word    ;pointer to mem address of Enemy color also P1
PLAnimOffset byte
Random byte


;Constants
Player_Height = 9 ; P0 height is 9 byte or rows
Enemy_Height = 9 ; P1 height is 9 byte or rows



;code segment
    seg code
    org $f000

Reset:
    CLEAN_START     ;fresh start in memory

    ;Initialize RAM variables and TIA registers
    lda #10
    sta PLYPos     ;jet position at pixel 10 {Y}

    lda #60
    sta PLXPos     ;jet position at pixel 60 {X}

    lda #83
    sta ENYPos
    lda #90
    sta ENXPos

    lda #%11010100
    sta Random      ; Random value = $D4 



    ;Start the pointers --> first low then high > little endian architecture
    ;Player sprite and color
    lda #<PlayerSP0
    sta PLSpritePtr  ; low-byte pointer for the palyer sprite
    lda #>PlayerSP0
    sta PLSpritePtr+1  ;high-byte pointer for the player sprite   

    lda #<PlayerCL0
    sta PLColorPtr  ; low-byte pointer for the palyer Color
    lda #>PlayerCL0
    sta PLColorPtr+1  ;high-byte pointer for the player Color 


    ;Enemy sprite and color 
    lda #<EnemySP
    sta ENSpritePtr  ; low-byte pointer for the Enemy sprite
    lda #>EnemySP
    sta ENSpritePtr+1  ;high-byte pointer for the Enemy sprite   

    lda #<EnemyCL
    sta ENColorPtr  ; low-byte pointer for the Enemy Color
    lda #>EnemyCL
    sta ENColorPtr+1  ;high-byte pointer for the Enemy Color 





;start scanlines 96 - 10 scanlines because of two line kernel
StartFrame:

    ;Calculations and task performed in pre-VBLANK
    lda PLXPos
    ldy #0
    jsr SetXPos     ;set p0 x pos

    lda ENXPos
    ldy #1
    jsr SetXPos     ;set p1 x pos

    sta WSYNC
    sta HMOVE       ;applying the x pos


    ;turn on VSYNC and VBlank
    lda #2
    sta VBLANK
    sta VSYNC

    ;dis 3 lines of VSYNC
    REPEAT 3
        sta WSYNC   
    REPEND
    ;turning off both
    lda #0
    sta VSYNC
    ;dis 37 lines of VBLANK
    REPEAT 37 
        sta WSYNC  
    REPEND
    
    sta VBLANK

    ;HUD
    lda #0                   ; clear TIA registers before each new frame
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta COLUPF
    REPEAT 20
        sta WSYNC            ; display 20 scanlines where the scoreboard goes
    REPEND

    ;Display 192 scanlines
GameVisibleLines:
    ;cb to olive 
    lda  #$82
    sta COLUBK
    ;color of the playfield
    lda #$c2
    sta COLUPF

    lda #%00000001
    sta CTRLPF

    lda #$F0
    sta PF0
    
    lda #$FC
    sta PF1
    
    lda #0
    sta PF2

    ldx #86
.GameLinesLoop:
.IsInsidePlayerSP:
    txa
    sec
    sbc PLYPos
    cmp Player_Height       ;are inside the player sprite?
    bcc .DrawPlayer
    lda #0
.DrawPlayer:
    clc
    adc PLAnimOffset
    tay                     ;transfer to Y to work with pointer
    lda (PLSpritePtr),Y     ;load p0 bitmap data from lookup table
    sta WSYNC
    sta GRP0                ;set graphic for player
    lda (PLColorPtr),Y     ;load p0 Color data from lookup table
    sta COLUP0              ;set Color for player


.IsInsideEnemySP:
    txa
    sec
    sbc ENYPos
    cmp Enemy_Height       ;are inside the Enemy sprite?
    bcc .DrawEnemy
    lda #0
.DrawEnemy:
    tay                     ;transfer to Y to work with pointer
    lda #%00000101          ;Streatch Pattern
    sta NUSIZ1
    lda (ENSpritePtr),Y     ;load p1(Enemy) bitmap data from lookup table
    sta WSYNC
    sta GRP1                ;set graphic for Enemy
    lda (ENColorPtr),Y     ;load p1(Enemy) Color data from lookup table
    sta COLUP1 


    
    dex ;X--
    bne .GameLinesLoop 

    lda #0
    sta PLAnimOffset

    ;handling the overscan by turning VBLANK on
    lda #2
    sta VBLANK
    REPEAT 30
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK


    ;process joystick for p0 input
CheckP0Up:
    lda #%00010000      ;for up
    bit SWCHA
    bne CheckP0Down
    inc PLYPos
    lda #0
    sta PLAnimOffset
CheckP0Down:
    lda #%00100000      ;for down
    bit SWCHA
    bne CheckP0Left
    dec PLYPos
    lda #0
    sta PLAnimOffset
CheckP0Left:
    lda #%01000000      ;for up
    bit SWCHA
    bne CheckP0Right
    dec PLXPos
    lda Player_Height
    sta PLAnimOffset
CheckP0Right:
    lda #%10000000      ;for up
    bit SWCHA
    bne EndInput
    inc PLXPos
    lda Player_Height
    sta PLAnimOffset
EndInput:


;update function
UpdateEnemyYPos:
    lda ENYPos
    clc 
    cmp #0
    bmi .ResetEnemyPosition
    dec ENYPos
    jmp EndPosUpdate
.ResetEnemyPosition:
    jsr GetRandomXENPos

EndPosUpdate:

;Obj Collision
CheckCollisionP0P1:
    lda #%10000000
    bit CXPPMM
    bne .CollisionP0P1
    jmp CheckCollisionP0PF
.CollisionP0P1:
    jsr GameOver

CheckCollisionP0PF:
    lda #%10000000
    bit CXP0FB
    bne .CollisionP0PF
    jmp EndCollisionFallback
.CollisionP0PF:
    jsr GameOver

EndCollisionFallback:
    sta CXCLR



    ;Loop back to the start a new fresh frame or NEXT Frame
    jmp StartFrame
    

;to handle horizontal(X) movement
;Y is the choice of object type (0=P0, 1=P1, 2=Missile0, 3=Missile1, 4=Ball)
SetXPos subroutine
    sta WSYNC
    sec 
.Div15Loop:
    sbc #15
    bcs .Div15Loop
    eor #7
    asl
    asl
    asl
    asl
    sta HMP0,Y
    sta RESP0,Y
    rts

; to handle x position of enemy randomly(Subroutine)
;Generate a LFSR number, Divide by four to fit in the river, add 30 move right
GetRandomXENPos subroutine
    lda Random
    asl
    eor Random
    asl 
    eor Random
    asl
    asl
    eor Random
    asl 
    rol Random

    lsr
    lsr
    sta ENXPos
    lda #30
    adc ENXPos
    sta ENXPos
    lda #96
    sta ENYPos
    rts

GameOver subroutine
    lda #$30
    sta COLUBK
    rts

    ;Lookup tables for sprites in ROM
    ;Sprite of the Player as a ship
PlayerSP0
    .byte #%00000000         ;
    .byte #%00010100         ;   # #
    .byte #%01111111         ; #######
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #


PlayerSP1
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

EnemySP
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00101010         ;  # # #
    .byte #%00111110         ;  #####
    .byte #%01111111         ; #######
    .byte #%00101010         ;  # # #
    .byte #%00001000         ;    #
    .byte #%00011100         ;   ###

;Color of the Player's sprite 
PlayerCL0
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$BA
    .byte #$0E
    .byte #$08
PlayerCL1
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$0E
    .byte #$0E
    .byte #$08

EnemyCL
    .byte #$00
    .byte #$32
    .byte #$32
    .byte #$0E
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40


    ;Complete the ROM size by moving to the last 4 bytes of the ROM
    org $fffc
    .word Reset
    .word Reset
 



