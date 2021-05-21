    processor 6502

; *******************************************************************
;   include required files with VCS register mapping
; *******************************************************************
    include "vcs.h"
    include "macro.h"

; *******************************************************************
; Declare variables starting from memory address $80
; *******************************************************************
    seg.u Variables
    org $80

JetXPos         byte                    ; player0 x-position
JetYPos         byte                    ; player0 y-position
BomberXPos      byte                    ; player1 x-position
BomberYPos      byte                    ; player1 y-position
Score           byte                    ; 2-digit score stored as BCD
Timer           byte                    ; 2-digit timer stored as BCD
Temp            byte                    ; auxilary variable to store temp score values
OnesDigitOffset word                    ; lookup table offset for the score 1's digit
TensDigitOffset word                    ; lookup table offset for the score 10's digit
JetSpritePtr    word                    ; pointer to player0 sprite table
JetColorPtr     word                    ; pointer to player0 color table
BomberSpritePtr word                    ; points to player1 sprite table
BomberColorPtr  word                    ; points to player1 color table
JetAnimOffset   byte                    ; player0 sprite frame offset
Random          byte                    ; randome number generated to set enemy X
ScoreSprite     byte                    ; store the sprite bit pattern for the score
TimerSprite     byte                    ; store the sprite bit pattern for the timer
TerrainColor    byte                    ; store the color of the terrain (grass)
RiverColor      byte                    ; store the color of the river

; *******************************************************************
; Define constants
; *******************************************************************
JET_HEIGHT = 9                          ; player0 sprite height
BOMBER_HEIGHT = 9                       ; player1 sprite height
DIGITS_HEIGHT = 5                       ; scoreboard digit height (rows in lookup table)

; *******************************************************************
; Start our ROM code at memory address $F000
; *******************************************************************
    seg Code
    org $F000

Reset:
    CLEAN_START                         ; call macro to reset memory & registers

; *******************************************************************
; Initialize RAM variables and TIA registers
; *******************************************************************
    lda #10
    sta JetYPos                         ; JetYPos = 10
    lda #68
    sta JetXPos                         ; JetXPos = 60
    lda #83
    sta BomberYPos                      ; Bomber Y Position
    lda #54
    sta BomberXPos                      ; Bomber X Position
    lda #%11010100
    sta Random                          ; Random = $D4
    lda #0
    sta Score
    sta Timer                           ; Score = Timer = 0

; *******************************************************************
; Initialize Pointers to correct lookup table addresses
; *******************************************************************
    lda #<JetSprite
    sta JetSpritePtr                    ; lo-byte pointer for jet table
    lda #>JetSprite
    sta JetSpritePtr+1                  ; hi-byte pointer for jet table

    lda #<JetColor
    sta JetColorPtr                    ; lo-byte pointer for color table
    lda #>JetColor
    sta JetColorPtr+1                  ; hi-byte pointer for color table

    lda #<BomberSprite
    sta BomberSpritePtr                ; lo-byte pointer for bomber table
    lda #>BomberSprite
    sta BomberSpritePtr+1              ; hi-byte pointer for bomber table

    lda #<BomberColor
    sta BomberColorPtr                 ; lo-byte pointer for color table
    lda #>BomberColor
    sta BomberColorPtr+1               ; hi-byte pointer for color table

; *******************************************************************
; Start the main game display loop and frame rendering (NTSC)
; *******************************************************************
StartFrame:

; *******************************************************************
; Calculations and tasks performed in the VBLANK
; *******************************************************************
    ; display VSYNC and VBLANK
    lda #2
    sta VBLANK                          ; turn on VBLANK
    sta VSYNC                           ; turn on VSYNC
    REPEAT 3
        sta WSYNC                       ; display 3 recommended lines of VSYNC
    REPEND
    lda #0
    sta VSYNC                           ; turn off VSYNC
    REPEAT 33                           
        sta WSYNC                       ; display the recommended lines of VBLANK
    REPEND

    lda JetXPos
    ldy #0
    jsr SetObjectXPos                   ; set player0 horizontal position

    lda BomberXPos
    ldy #1
    jsr SetObjectXPos                   ; set player1 horizontal position

    jsr CalculateDigitOffset            ; calculate the scoreboard digit lookup table offset

    sta WSYNC
    sta HMOVE                           ; apply the horizontal offsets previously set

    lda #0
    sta VBLANK                          ; turn VBLANK off

; *******************************************************************
; Display the scoreboard lines
; *******************************************************************
    lda #0                              ; reset TIA registers before displaying the score
    sta COLUBK
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF

    lda #$1E
    sta COLUPF                          ; set the scoreboard playfield color with yellow
    
    ldx #DIGITS_HEIGHT                  ; start X counter with 5 (height of digits graphic)
.ScoreDigitLoop:
    ldy TensDigitOffset                 ; get the tens digit offset for the Score
    lda Digits,Y                        ; load bitpattern from lookup table
    and #$F0                            ; mask/remove graphics for the ones digit
    sta ScoreSprite                     ; save the score tens digit pattern in a variable
    ldy OnesDigitOffset                 ; get the ones digit offset for the Score
    lda Digits,Y                        ; load bitpattern from lookup table
    and #$0F                            ; mask/remove graphics for the tens digit
    ora ScoreSprite                     ; merge it with the saved tens digit sprite
    sta ScoreSprite                     ; and save it
    sta WSYNC                           ; wait for end of the scanline
    sta PF1                             ; update the playfield to display the score

    ldy TensDigitOffset+1               ; get the left digit offset for the Timer
    lda Digits,Y                        ; load the digit pattern from lookup table
    and #$F0                            ; mask/remove the graphics for the ones digit
    sta TimerSprite                     ; save the timer tens digit pattern in a variable
    ldy OnesDigitOffset+1               ; get the ones digit offset for the Timer
    lda Digits,Y                        ; load digit pattern from lookup table
    and #$0F                            ; mask/remove the graphics from the tens digit
    ora TimerSprite                     ; merge with the saved tens digit graphics
    sta TimerSprite                     ; save

    jsr Sleep12Cycles                   ; wastes some cycles

    sta PF1                             ; update playfield for Timer display

    ldy ScoreSprite                     ; preload for the next scanline
    sta WSYNC                           ; wait for next scanline

    sty PF1                             ; update playfield for the score display
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1               ; increment all digits for the next line of data

    jsr Sleep12Cycles                   ; waste 12 cycles

    dex                                 ; X--
    sta PF1                             ; update the playfield for the timer display
    bne .ScoreDigitLoop                 ; if dex !=0 then branch back to score loop

    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC

; *******************************************************************
; Display the 96 visible scanlines of our main game (2-line kernal)
; *******************************************************************
GameVisibleLines:
    lda TerrainColor
    sta COLUPF                          ; set terrain background color
    lda RiverColor
    sta COLUBK                          ; set the river background color
    lda #%00000001                      ; set playfield to reflect
    sta CTRLPF
    lda #$F0
    sta PF0                             ; setting PF0 bit pattern
    lda #$FC                            ; setting PF1
    sta PF1
    lda #0                              ; setting PF2
    sta PF2

    ldx #85                             ; X counts the number of remaining scanlines
.GameLineLoop:
.AreWeInsideJetSprite:
    txa                                 ; transfer X to A
    sec                                 ; set carry flag before subraction
    sbc JetYPos                         ; subtract jet Y-coord
    cmp JET_HEIGHT                      ; compare jet height
    bcc .DrawSpriteP0                   ; if result < spriteheight, call draw routine
    lda #0
.DrawSpriteP0:
    clc                                 ; clear carry flag before addition 
    adc JetAnimOffset                   ; jump to the correct sprite frame
    tay                                 ; transfer A to Y to work with pointer
    lda (JetSpritePtr),Y                ; load player0 bitmap data from lookup table
    sta WSYNC                           ; wait for scanline
    sta GRP0                            ; set graphics for player 0
    lda (JetColorPtr),Y                 ; load color from lookup table
    sta COLUP0                          ; set color of player 0

.AreWeInsideBomberSprite:
    txa                                 ; transfer X to A
    sec                                 ; set carry flag before subraction
    sbc BomberYPos                      ; subtract jet Y-coord
    cmp BOMBER_HEIGHT                   ; compare jet height
    bcc .DrawSpriteP1                   ; if result < spriteheight, call draw routine
    lda #0
.DrawSpriteP1:
    tay                                 ; transfer A to Y to work with pointer
    lda #%00000101
    sta NUSIZ1                          ; stretch player 1 sprite
    lda (BomberSpritePtr),Y             ; load player0 bitmap data from lookup table
    sta WSYNC                           ; wait for scanline
    sta GRP1                            ; set graphics for player 0
    lda (BomberColorPtr),Y              ; load color from lookup table
    sta COLUP1                          ; set color of player 0

    dex                                 ; X--
    bne .GameLineLoop                   ; repeat next game scanline until finished

    lda #0
    sta JetAnimOffset                   ; reset jet animation frame to zero

    sta WSYNC                           ; wait for a scanline

; *******************************************************************
; Display overscan
; *******************************************************************
    lda #2
    sta VBLANK                          ; turn VBLANK on again
    REPEAT 30
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK                          ; turn VBLANK off

; *******************************************************************
; Process joystick input for player0
; *******************************************************************
CheckP0Up:
    lda #%00010000                      ; player0 joystick up
    bit SWCHA
    bne CheckP0Down                     ; if no match bypass up
    lda JetYPos
    cmp #70                             ; if player Y > 70
    bpl CheckP0Down                     ; then skip increment
    inc JetYPos                         ; else increment
    lda #0
    sta JetAnimOffset                   ; reset sprite frame to first

CheckP0Down:
    lda #%00100000                      ; player0 joystick down
    bit SWCHA
    bne CheckP0Left                     ; if no match bypass down
    lda JetYPos
    cmp #5                              ; if player Y < 5
    bmi CheckP0Left                     ; then skip decrement                         
    dec JetYPos                         ; else decrement
    lda #0
    sta JetAnimOffset                   ; reset sprite frame to first

CheckP0Left:
    lda #%01000000                      ; player0 joystick left
    bit SWCHA
    bne CheckP0Right
    lda JetXPos
    cmp #35                             ; if player X < 35
    bmi CheckP0Right                    ; then skip decrement
    dec JetXPos                         ; else decrement
    lda JET_HEIGHT                      ; 9
    sta JetAnimOffset                   ; set animation offset to second frame

CheckP0Right:
    lda #%10000000                      ; player0 joystick right
    bit SWCHA
    bne EndInputCheck
    lda JetXPos
    cmp #100                            ; if player X > 100
    bpl EndInputCheck                   ; then skip increment
    inc JetXPos                         ; else increment
    lda JET_HEIGHT                      ; 9
    sta JetAnimOffset                   ; set animation offset to second frame

EndInputCheck:                          ; fallback when no input performed

; *******************************************************************
; Calculations to update positions for next frame
; *******************************************************************
UpdateBomberPosition:
    lda BomberYPos
    clc
    cmp #0                              ; comparing bomber y pos with 0
    bmi .ResetBomberPosition            ; if < 0 then reset y position back up
    dec BomberYPos                      ; else decrement bomber y pos
    jmp EndPositionUpdate
.ResetBomberPosition
    jsr GetRandomBomberPos              ; call subroutine for next random enemy X

.SetScoreValues:
    sed                                 ; set decimal mode for score and timer values
    lda Score
    clc
    adc #1
    sta Score                           ; add 1 to the score (BCD does not do INC)
    lda Timer
    clc
    adc #1
    sta Timer                           ; add 1 to the timer (BCD does not do INC)
    cld                                 ; disable decimal mode

EndPositionUpdate:                      ; fallback for position update code

; *******************************************************************
; Check for object collision
; *******************************************************************
CheckCollisionP0P1:
    lda #%10000000                      ; CXPPMM bit 7 detects P0 and P1 collisions
    bit CXPPMM                          ; check bit 7 with above pattern
    bne .P0P1Collided                   ; if collision P0/P1 happened Game Over...
    jsr SetTerrainRiverColor            ; ...else set playfield color to green/blue...
    jmp EndCollisionCheck               ; ...else clear collision checks
.P0P1Collided:
    jsr GameOver                        ; call GameOver subroutine

EndCollisionCheck:                      ; fallback
    sta CXCLR                           ; clear all collision flags before next frame

; *******************************************************************
; Loop back to start a brand new frame
; *******************************************************************
    jmp StartFrame                      ; continue to display the next frame

; *******************************************************************
; Subroutine to handle object horizontal position with fine offset
; *******************************************************************
; A is the target x-coord position in pixels of our object
; Y is the object type (0:player0, 1:player1, 2:missle0, 3:missle1, 4:ball)
; *******************************************************************
SetObjectXPos subroutine
    sta WSYNC                           ; start a fresh new scanline
    sec                                 ; make sure the carry flag is set before subroutine
.Div15Loop
    sbc #15                             ; subtract 15 from accumulator
    bcs .Div15Loop                      ; loop until carry flag is clear
    eor #7                              ; handle offset range from -8 to 7
    asl
    asl
    asl
    asl                                 ; four shift lefts to get only the top 4 bits
    sta HMP0,Y                          ; store the fine offset to the correct HMxx
    sta RESP0,Y                         ; fix object position in 15 step increment
    rts

; *******************************************************************
; Game Over subroutine
; *******************************************************************
GameOver subroutine
    lda #$30
    sta TerrainColor                    ; set terrain color to red
    sta RiverColor                      ; set river color to red
    lda #0
    sta Score                           ; Score = 0
    rts

; *******************************************************************
; Subroutine to generate a LFSR random number
; *******************************************************************
; *******************************************************************
; Generate a random number
; Divide the random value by 4 to limit the size of the result to
; match the river size.
; Add 30 to compensate for the left green playfield
; *******************************************************************
GetRandomBomberPos  subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random                          ; performs a series of shifts and bit ops
    lsr
    lsr                                 ; divide by 4 by performing to right shifts
    sta BomberXPos                      ; save it to the variable BomberXPos
    lda #30
    adc BomberXPos                      ; adds 30 + BomberXPos to compensate left playfield
    sta BomberXPos                      ; and sets new value to bomber x-position

    lda #96
    sta BomberYPos                      ; sets the y-position to the top of the screen
    rts

; *******************************************************************
; Subroutine to handle scoreboard digits to be displayed on screen
; *******************************************************************
; convert the high and low nibbles of the variable Score and Timer
; into the offsets of digits lookup table so the values can be displayed
; each digit has a height of 5 bytes in the lookup table
;
; for the low nibble we need to multiply by 5 to get offset in lookup table
; can use left shifts to perform multiplication by 2
; for any number N the value of N*5 = (N*2*2)+N
;
; for the upper nibble, since it's already times 16, we need to divide it
; and then multiply by 5
; can use right shifts to perform division by 2
; for any number N the value of (N/16)*5 = (N/2/2)+(N/2/2/2/2)
CalculateDigitOffset subroutine
    ldx #1                              ; X register is the loop counter
.PrepareScoreLoop                       ; this will loop twice, first X=1 and then X=0
    lda Score,X                         ; load A with Timer(X=1) or Score(X=0)
    and #$0F                            ; remove 10s part keep right most nibble 00001111
    sta Temp                            ; save the value of A into temp variable
    asl                                 ; shift left (it is now N*2)
    asl                                 ; shift left (it is now N*4)
    adc Temp                            ; add the value saved in Temp (+N)
    sta OnesDigitOffset,X               ; save A in OnesDigitOffset+1 or OnesDigitOffset

    lda Score,X                         ; load A with Timer (X=1) or Score (X=0)
    and #$F0                            ; remove the ones digit by masking 4 bits 11110000
    lsr                                 ; shift right (is N/2)
    lsr                                 ; shift right (is N/4)
    sta Temp                            ; save the value of A into Temp
    lsr                                 ; shift right (is N/8)
    lsr                                 ; shift right (is N/16)
    adc Temp                            ; add the value saved in Temp (N/16+N/4)
    sta  TensDigitOffset,X              ; store A in TensDigitOffset+1 or TensDigitOffset

    dex                                 ; X--
    bpl .PrepareScoreLoop               ; While X is positive loop to pass a second time
    rts

; *******************************************************************
; Set the color for the terrain & river to green & blue
; *******************************************************************
SetTerrainRiverColor subroutine
    lda #$C2
    sta TerrainColor                    ; set terrain color to green
    lda #$84
    sta RiverColor                      ; set river color to blue

; *******************************************************************
; Subroutine to waste 12 cycles
; *******************************************************************
; jsr takes 6 cycles
; rts takes 6 cycles
; *******************************************************************
Sleep12Cycles subroutine
    rts

; *******************************************************************
; Declare ROM lookup tables
; *******************************************************************
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

JetSprite:
    .byte #%00000000
    .byte #%00010100
    .byte #%01111111
    .byte #%00111110
    .byte #%00011100
    .byte #%00011100
    .byte #%00001000
    .byte #%00001000
    .byte #%00001000

JetSpriteTurn:
    .byte #%00000000
    .byte #%00001000
    .byte #%00111110
    .byte #%00011100
    .byte #%00011100
    .byte #%00011100
    .byte #%00001000
    .byte #%00001000
    .byte #%00001000

BomberSprite:
    .byte #%00000000
    .byte #%00001000
    .byte #%00001000
    .byte #%00101010
    .byte #%00111110
    .byte #%01111111
    .byte #%00101010
    .byte #%00001000
    .byte #%00011100

JetColor:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$BA
    .byte #$0E
    .byte #$08

JetColorTurn:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$0E
    .byte #$0E
    .byte #$08

BomberColor:
    .byte #$00
    .byte #$32
    .byte #$32
    .byte #$0E
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40


; *******************************************************************
; Complete ROM size with exactly 4kb
; *******************************************************************
    org $FFFC                           ; move to position $FFFC
    word Reset                          ; write 2 bytes with Reset address
    word Reset                          ; write 2 bytes with interrupt vector