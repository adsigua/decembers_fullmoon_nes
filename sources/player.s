.include "nes.inc"
.include "global.inc"

.segment "ZEROPAGE"
; Game variables
player_dxlo:      .res 1  ; signed speed in pixels per 256 frames
player_dylo:      .res 1  ; signed speed in pixels per 256 frames
player_pos_xlo    .res 1
player_pos_xhi    .res 1
player_pos_ylo    .res 1
player_pos_yhi    .res 1

;32x30 cells, 256x240
.scope InitValues
  init_xpos = #120
  init_ypos = #190
.endscope
; constants used by move_player
; PAL frames are about 20% longer than NTSC frames.  So if you make
; dual NTSC and PAL versions, or you auto-adapt to the TV system,
; you'll want PAL velocity values to be 1.2 times the corresponding
; NTSC values, and PAL accelerations should be 1.44 times NTSC.
WALK_SPD = 105  ; speed limit in 1/256 px/frame

LEFT_WALL = 32
RIGHT_WALL = 224

.segment "CODE"

.proc init_player
  lda #0
  sta player_pos_xlo
  sta player_pos_ylo

  sta entities_list+Entity::frame_num
  sta entities_list+Entity::frame_timer
  sta entities_list+Entity::state

  lda #EntityType::player_type
  sta entities_list+Entity::type

  lda #InitValues::init_xpos
  sta player_pos_xhi
  sta entities_list+Entity::sprite_x

  lda #InitValues::init_ypos
  sta player_pos_yhi
  sta entities_list+Entity::sprite_y

  rts
.endproc

.proc update_player
  jsr update_player_velocity
  jsr apply_player_velocity
  jsr update_player_animation
  rts
.endproc

;;
; Moves the player character in response to controller 1.
; facing 0=left, 1=right
.proc update_player_velocity

  lda cur_keys
  and #KEY_RIGHT
  beq notRight     
    clc
    lda #WALK_SPD    
    sta player_dxlo 
    lda entities_list+Entity::state   ; set bit 7 to 1
    ora #$80                          ; %1000 0000
    jmp doneInputX

  notRight:
  ; Acceleration to left: Do it only if the player is holding left
  ; on the Control Pad and has a nonpositive velocity.
  lda cur_keys
  and #KEY_LEFT
  beq doneInputX
    clc
    lda #256-WALK_SPD   
    sta player_dxlo 
    lda entities_list+Entity::state   ; set bit 7 to 1
    and #$7f                          ; %1000 0000
  doneInputX:

  ; In a real game, you'd respond to A, B, Up, Down, etc. here.
  
  rts
.endproc

.proc apply_player_velocity
  ; Move the player by adding the velocity to the 16-bit X position.
  lda player_dxlo
  bpl player_dxlo_pos
    ; if velocity is negative, subtract 1 from high byte to sign extend
    dec player_xhi
  player_dxlo_pos:
  clc
  adc player_xlo
  sta player_xlo
  lda #0          ; add high byte
  adc player_xhi
  sta player_xhi

  ; Test for collision with side walls
  cmp #LEFT_WALL-4
  bcs notHitLeft
    lda #LEFT_WALL-4
    sta player_xhi
    lda #0
    sta player_dxlo
    beq doneWallCollision
  notHitLeft:

  cmp #RIGHT_WALL-12
  bcc notHitRight
    lda #RIGHT_WALL-13
    sta player_xhi
    lda #0
    sta player_dxlo
  notHitRight:

  ; Additional checks for collision, if needed, would go here.
  doneWallCollision:


  .rts
.endproc

.proc update_player_animation
    ; If V == 0:
    ;   Set initial timer
    ; Else:
    ;   Decrement timer
    ;   If frame timer == 0:
    ;     Reset frame timer based on V
    ;     Increment the frame
    lda player_dxlo
    bne @moving
    lda delay_by_velocity
    sta entities_list+Entity::animationTimer
    rts
  @moving:
    dec entities_list+Entity::animationTimer
    beq @next_frame
    rts
  @next_frame:
    ldx entities_list+Entity::animationFrame
    inx
    cmp #3
    bne :+
      ldx #0
  :
    lda frame_delay, x
    sta entities_list+Entity::animationTimer
    stx entities_list+Entity::animationFrame
    rts
  frame_delay:
    .byte 12, 11, 11
.endproc

;;
; Draws the player's character to the display list as six sprites.
; In the template, we don't need to handle half-offscreen actors,
; but a scrolling game will need to "clip" sprites (skip drawing the
; parts that are offscreen).
.proc draw_player_sprite
  lda entities_list+Entity::state
  and #$0f
  cmp #EntityState::moving
  beq @moving
@idle:
  


  rts
@moving:



  rts
  walk_tiles:
    .byte $00, $01, $10, $11, $20, $21 ; Frame 1
    .byte $00, $02, $12, $13, $22, $23 ; Frame 1
    .byte $00, $01, $14, $15, $24, $25 ; Frame 1
  idle_tiles:
    .byte $00, $01, $14, $15, $20, $21 
.endproc

.segment "RODATA"
; In frame 7, the player needs to be drawn 1 pixel forward
; because of how far he's leaned forward
player_frame_to_xoffset:
  .byte 0, 0, 0, 0, 0, 0, 0, 1
