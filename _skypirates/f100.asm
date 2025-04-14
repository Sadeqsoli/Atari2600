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
;Missile X,Y Position
MissileXPos byte
MissileYPos byte
;Player sprites
PLSpritePtr word      ;pointer to mem address of Player sprite also P0
PLColorPtr  word    ;pointer to mem address of Player color also P0

ENSpritePtr word      ;pointer to mem address of Enemy sprite also P1
ENColorPtr  word    ;pointer to mem address of Enemy color also P1
PLAnimOffset byte
Random      byte
ScoreSprite byte
TimerSprite byte

TerrainColor byte
RiverColor byte

;Hud Vars 2-digit value as BCD
Score       byte
Timer       byte
Temp        byte
OnesDigitOffset   word
TensDigitOffset   word

;Constants
Player_Height = 9 ; P0 height is 9 byte or rows
Enemy_Height = 9 ; P1 height is 9 byte or rows
Digit_Height = 5 ; Numbers for score and timer height is 5 byte or rows



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

    lda #0
    sta Score
    sta Timer


    MAC DRAW_MISSILE
        lda #%00000000
        cpx MissileYPos      ; compare X (current scanline) with missile Y pos
        bne .SkipMissileDraw ; if (X != missile Y position), then skip draw
.DrawMissile:                ; else:
        lda #%00000010       ;     enable missile 0 display
        inc MissileYPos      ;     MissileYPos++
.SkipMissileDraw:
        sta ENAM0            ; store correct value in the TIA missile register
    ENDM

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
    REPEAT 33
        sta WSYNC  
    REPEND


    ;Calculations and task performed in pre-VBLANK
    lda PLXPos
    ldy #0
    jsr SetXPos     ;set p0 x pos

    lda ENXPos
    ldy #1
    jsr SetXPos     ;set p1 x pos

    lda MissileXPos
    ldy #2
    jsr SetXPos     ;set p1 x pos

    jsr CalculateDigits

    sta WSYNC
    sta HMOVE       ;applying the x pos

    lda #0
    sta VBLANK

    ;HUD
    lda #0                   ; clear TIA registers before each new frame
    sta COLUBK
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF

    lda #$1c                ;set playfield color to white
    sta COLUPF

    ldx Digit_Height

.HUD:
;;Score
    ldy TensDigitOffset
    lda Digits,Y 
    and #$f0
    sta ScoreSprite
    ldy OnesDigitOffset
    lda Digits,Y 
    and #$0f
    ora ScoreSprite
    sta ScoreSprite
    sta WSYNC
    sta PF1

;;Timer
    ldy TensDigitOffset +1
    lda Digits,Y 
    and #$f0
    sta TimerSprite
    ldy OnesDigitOffset +1
    lda Digits,Y 
    and #$0f
    ora TimerSprite
    sta TimerSprite

;;handling PF1 reflection
    jsr SleepFor12Cycles
    sta PF1
    ldy ScoreSprite
    sta WSYNC
    
    sty PF1
    inc TensDigitOffset
    inc TensDigitOffset +1
    inc OnesDigitOffset
    inc OnesDigitOffset +1

    jsr SleepFor12Cycles

    dex 
    sta PF1
    bne .HUD

    sta WSYNC

    lda #0                   ; clear TIA registers before each new frame
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC





    ;Display 192 scanlines
GameVisibleLines:

    ;color of the playfield
    lda TerrainColor
    sta COLUPF

    ;Color of background
    lda  RiverColor
    sta COLUBK
    

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
    DRAW_MISSILE    ;macro

.IsInsidePlayerSP:
    txa
    sec
    sbc PLYPos
    cmp #Player_Height       ;are inside the player sprite?
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
    cmp #Enemy_Height       ;are inside the Enemy sprite?
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

    sta WSYNC

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
    lda PLYPos
    cmp #77
    bpl CheckP0Down
    inc PLYPos
    lda #0
    sta PLAnimOffset
CheckP0Down:
    lda #%00100000      ;for down
    bit SWCHA
    bne CheckP0Left
    lda PLYPos
    cmp #2
    bmi CheckP0Left
    dec PLYPos
    lda #0
    sta PLAnimOffset
CheckP0Left:
    lda #%01000000      ;for up
    bit SWCHA
    bne CheckP0Right
    lda PLXPos
    cmp #32
    bmi CheckP0Right
    dec PLXPos
    lda #Player_Height
    sta PLAnimOffset
CheckP0Right:
    lda #%10000000      ;for up
    bit SWCHA
    bne CheckFireButton
    lda PLXPos
    cmp #102
    bpl CheckFireButton
    inc PLXPos
    lda #Player_Height
    sta PLAnimOffset
CheckFireButton:
    lda #%10000000           ; Button 
    bit INPT4
    bne EndInput
    lda PLXPos
    clc
    adc #5 
    sta MissileXPos 
    lda PLYPos
    clc
    adc #8
    sta MissileYPos 
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

.SetScore_Timer:
    sed 
    ; inc Timer
    lda Timer
    clc 
    adc #1
    sta Timer
    cld 

EndPosUpdate:

;Obj Collision
CheckCollisionP0P1:
    lda #%10000000
    bit CXPPMM
    bne .GameOverLabel
    jsr SetPF_BKColor
    jmp CheckCollisionM0P1

.GameOverLabel:
    jsr GameOver

CheckCollisionM0P1:
    lda #%10000000
    bit CXM0P
    bne .CollisionM0P1
    jmp EndCollisionFallback

.CollisionM0P1:
    ; inc Score
    sed
    lda Score
    clc 
    adc #1
    sta Score
    cld
    lda #0
    sta MissileYPos

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
;handling after collisions
GameOver subroutine
    lda #$cc
    sta TerrainColor
    lda #$8c
    sta RiverColor

    lda #0
    sta Score
    rts
;displaying score and timer
CalculateDigits subroutine
    ldx #1                  ;If X=1 goes to Timer and X=0 goes to Score
.PrepareHUD

    lda Score,X             ;load A with Timer 
    and #$0F                ;masking tens digits
    sta Temp
    asl 
    asl 
    clc
    adc Temp
    sta OnesDigitOffset,X 

    lda Score,X 
    and #$F0                ;masking Ones digits
    lsr 
    lsr 
    sta Temp
    lsr 
    lsr
    clc 
    adc Temp
    sta TensDigitOffset,X

    dex 
    bpl .PrepareHUD       ;while X is still positive

    rts

SetPF_BKColor subroutine

    lda #$84
    sta RiverColor
    lda #$c2
    sta TerrainColor

    rts



SleepFor12Cycles subroutine
    rts





    ;Lookup tables for sprites in ROM
    ;Sprite of the Player as a ship

Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #

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
 



