.include "nes.inc"
.include "global.inc"

.segment "ZEROPAGE"
  player_index:       .res 1
  player_entity_index:.res 1

  min_bg_xpos_scroll = $30
  ; entity_addr = $300
  ; entity_type = $0300
  ; entity_state = $0310
  ; entity_id = $0320
  ; entity_anim_id = $0330
  ; entity_anim_delay_cnt = $0340
  ; entity_pos_x = $0350
  ; entity_pos_y = $0360
  ; entity_velocity_x = $0370
  ; entity_velocity_y = $0380
  ; entity_pos_x_lo = $0390
  ; entity_pos_y_lo = $03a0

;32x30 cells, 256x240
; constants used by move_player
; PAL frames are about 20% longer than NTSC frames.  So if you make
; dual NTSC and PAL versions, or you auto-adapt to the TV system,
; you'll want PAL velocity values to be 1.2 times the corresponding
; NTSC values, and PAL accelerations should be 1.44 times NTSC.
; WALK_SPD = 14  ; speed limit in 1/256 px/frame

LEFT_WALL = 32
RIGHT_WALL = 224

.segment "CODE"
.proc init_player_entity
  lda #0
  sta player_index
  sta player_entity_index
  sta entity_id
  sta entity_anim_delay_cnt
  sta entity_velocity_x
  sta entity_velocity_y
  sta entity_pos_x_lo
  sta entity_pos_y_lo

  lda #EntityType::player_type
  ora #EntityBehaviourType::moving_animated
  sta entity_type

  ldx player_index

  lda player_anim_idle, x
  sta entity_anim_id

  lda #EntityState::Idle
  ora #$80
  sta entity_state

  lda player_spawn_x, x
  sta entity_pos_x
  lda player_spawn_y, x
  sta entity_pos_y
  rts
.endproc

.proc update_player
  jsr update_player_velocity
  rts
.endproc

.proc check_player_action
  rts
.endproc

; Moves the player character in response to controller 1.
; facing 0=left, 1=right
.proc update_player_velocity
  velocityFlag = $01
  ldx player_index

  lda #0
  sta velocityFlag

  lda cur_keys
  and #KEY_RIGHT
  beq @notRight     
    lda player_move_speed, x    
    sta entity_velocity_x

    lda entity_state
    ora #%01000000
    sta entity_state

    lda #$80
    sta velocityFlag

    jmp @doneInputX
  @notRight:
  lda cur_keys
  and #KEY_LEFT
  beq @noInput_x
    lda player_move_speed, x      
    eor #$ff
    clc
    adc #$01
    sta entity_velocity_x

    lda entity_state
    and #%10111111
    sta entity_state

    lda #$80
    sta velocityFlag

    jmp @doneInputX                    
  @noInput_x:
    lda #0
    sta entity_velocity_x
  @doneInputX:

  lda cur_keys
  and #KEY_DOWN
  beq @notDown     
    lda player_move_speed, x
    sta entity_velocity_y
    lda #$80
    sta velocityFlag
    jmp @doneInputY
  @notDown:
  lda cur_keys
  and #KEY_UP
  beq @noInput_y
    lda player_move_speed, x    
    eor #$ff
    clc
    adc #$01
    sta entity_velocity_y 
    lda #$80
    sta velocityFlag
    jmp @doneInputY                    
  @noInput_y:
    lda #0
    sta entity_velocity_y 
  @doneInputY:
  
  bit velocityFlag
  bmi @check_state_change_moving

  @check_state_change_still:
    lda entity_state
    and #$0f
    cmp #EntityState::Idle
    beq :+
      lda entity_state
      ora #%10000000
      and #$F0
      ora #EntityState::Idle
      sta entity_state

      lda player_anim_idle, x
      sta entity_anim_id

      ldx #0
      jsr update_frame_delay_cnt
    :
    jmp @end_player_velocity_check

  @check_state_change_moving:
    lda entity_state
    and #$0f
    cmp #EntityState::Moving
    beq :+
      lda entity_state
      ora #%10000000
      and #$F0
      ora #EntityState::Moving
      sta entity_state
      
      lda player_anim_walk, x
      sta entity_anim_id

      ldx #0
      jsr update_frame_delay_cnt
    :
  @end_player_velocity_check:
    rts
.endproc

;x = frame num
.proc update_frame_delay_cnt
  lda player_anim_frame_delay_addr_lo, x 
  sta tempX
  lda player_anim_frame_delay_addr_hi, x
  sta tempX+1
  
  ldy entity_anim_id
  lda (tempX), y
  sta entity_anim_delay_cnt
.endproc


.proc apply_player_velocity
  lda entity_velocity_x
  beq @check_y_velocity
  bmi @move_x_pos
  
  ;check if good for scrolling if not continue move_x_pos
  bit level_flags
  bpl @move_x_pos
  ldy entity_pos_x
  cpy #min_bg_xpos_scroll
  bcc @move_x_pos

  @add_to_scroll_x:
    clc
    adc scroll_x_lo
    sta scroll_x_lo
    lda #0          
    adc scroll_x
    sta scroll_x
    lda level_flags
    bcc @no_nametable_switch
      ora #%00100000
    @no_nametable_switch:
    ora #%01000000
    sta level_flags
    jmp @check_y_velocity

  @move_x_pos:
    ; if velocity is negative, subtract 1 from high byte to sign extend
    lda entity_velocity_x
    bpl @entity_positive_x_speed
    dec entity_pos_x
  @entity_positive_x_speed:
    clc
    adc entity_pos_x_lo
    sta entity_pos_x_lo
    lda #0          
    adc entity_pos_x
    sta entity_pos_x
    

  @check_y_velocity:
  lda entity_velocity_y
  beq @end_velocity_apply

  bpl @entity_positive_y_speed
    dec entity_pos_y
  @entity_positive_y_speed:
  clc
  adc entity_pos_y_lo
  sta entity_pos_y_lo
  lda #0        
  adc entity_pos_y
  sta entity_pos_y
  ;jsr bound_position_y

  @end_velocity_apply:

  rts
.endproc


.proc update_player_animation
    ;check if there was state change, if not check if delay cnt is 0
    bit entity_state
    bpl :+
      lda entity_state
      eor #$80
      sta entity_state
      jmp @update_anim_frame
    :
    lda entity_anim_delay_cnt
    beq @update_anim_frame

    ;decrement frame
    @decrement_delay_count:
      dec entity_anim_delay_cnt
      jmp @end_anim_check

    @update_anim_frame:
      ;check if anim is only 1 frame, if yes exit anim update
      ldx entity_anim_id
      lda player_anim_frame_count, x
      cmp #$01
      beq @end_anim_check

      @check_next_frame:
        ldy entity_anim_frame_id
        iny
        tya
        cmp player_anim_frame_count, x
        bcs @is_over_frame_count
        sta entity_anim_frame_id
        jmp @store_frame_delay_count

      @is_over_frame_count:
        lda #0
        sta entity_anim_frame_id

      @store_frame_delay_count:
        tax
        jsr update_frame_delay_cnt
        
  @end_anim_check:
    
    rts
.endproc

.proc update_player_oam_buffer
  player_meta_sprite_index = $06
  sprite_count = $07
  pallete_index = $08

  ldx entity_anim_frame_id
  ldy entity_anim_id

  lda player_anim_frame_palette_addr_lo, X
  sta tempX
  lda player_anim_frame_palette_addr_hi, X
  sta tempX+1

  lda (tempX), y
  sta pallete_index

  lda player_anim_frame_metasprite_addr_lo, x
  sta tempX
  lda player_anim_frame_metasprite_addr_hi, x
  sta tempX+1

  lda (tempX), y
  sta player_meta_sprite_index

  ldx player_index
  lda player_meta_sprites_sprite_count, x
  sta sprite_count

  ldx #0

  @draw_entity_loop:
    ;y-pos
    lda player_metasprite_y_offset_addr_lo, x
    sta tempX
    lda player_metasprite_y_offset_addr_hi, x
    sta tempX+1

    ldy #0
    lda (tempX), y    
    clc                  
    adc entity_pos_y
    sta oam_buffer

    ;sprite index
    lda player_metasprite_index_addr_lo, x
    sta tempX
    lda player_metasprite_index_addr_hi, x
    sta tempX+1

    ldy player_meta_sprite_index
    lda (tempX), y        
    sta oam_buffer+1

    ;flags (palette, flipping and bg priority)
    lda pallete_index
    sta tempZ
    lda entity_state
    and #%01000000
    ora tempZ
    sta oam_buffer+2             

    ;x pos and offset
    lda player_metasprite_x_offset_addr_lo, x
    sta tempX
    lda player_metasprite_x_offset_addr_hi, x
    sta tempX+1

    ldy #0
    lda (tempX), y 
    sta tempZ     

    lda entity_pos_x
    bit entity_state
    bvs @is_flipped
    @not_flipped:
      clc
      adc tempZ
      jmp @place_x_offset_to_buffer
    @is_flipped:
      sec
      sbc tempZ   
      clc
      adc #$08
    @place_x_offset_to_buffer:
      sta oam_buffer+3

  @place_buffer_to_oam:
    jsr update_oam
    inx
    cpx sprite_count
    bne @draw_entity_loop

  @end_player_oam_buffer:
    rts
.endproc

.segment "RODATA"

player_entity_data:
  .byte $11 ;     EntityType::player_type, EntityBehaviourType::moving_animated ;entity type
player_spawn_x:  
  .byte $10
player_spawn_y:  
  .byte $b0                  ;entity init spawn pos
; player_spawn_data_addr_lo:  
;   .byte <player_00_data
; player_spawn_data_addr_hi:  
;   .byte >player_00_data

  player_00_data:
    .byte $00

  player_anim_idle:
    .byte $00

  player_anim_walk:
    .byte $01

  player_move_speed:
    .byte $60                   ;entity move speed

  player_anim_frame_count:
    .byte $01, $04

  player_anim_frame_palette_addr_lo:
    .byte <player_anim_palette_f0, <player_anim_palette_f1, <player_anim_palette_f2, <player_anim_palette_f3, <player_anim_palette_f4, <player_anim_palette_f5
  player_anim_frame_palette_addr_hi:
    .byte >player_anim_palette_f0, >player_anim_palette_f1, >player_anim_palette_f2, >player_anim_palette_f3, >player_anim_palette_f4, >player_anim_palette_f5
  
  player_anim_frame_delay_addr_lo:
    .byte <player_anim_delay_count_f0, <player_anim_delay_count_f1, <player_anim_delay_count_f2, <player_anim_delay_count_f3, <player_anim_delay_count_f4, <player_anim_delay_count_f5
  player_anim_frame_delay_addr_hi:
    .byte >player_anim_delay_count_f0, >player_anim_delay_count_f1, >player_anim_delay_count_f2, >player_anim_delay_count_f3, >player_anim_delay_count_f4, >player_anim_delay_count_f5
 
  player_anim_frame_metasprite_addr_lo:
    .byte <player_anim_meta_sprites_f0, <player_anim_meta_sprites_f1, <player_anim_meta_sprites_f2, <player_anim_meta_sprites_f3, <player_anim_meta_sprites_f4, <player_anim_meta_sprites_f5
  player_anim_frame_metasprite_addr_hi:
    .byte >player_anim_meta_sprites_f0, >player_anim_meta_sprites_f1, >player_anim_meta_sprites_f2, >player_anim_meta_sprites_f3, >player_anim_meta_sprites_f4, >player_anim_meta_sprites_f5  

  player_anim_palette_f0:
    .byte $00, $00
  player_anim_delay_count_f0:
    .byte $00, $0a
  player_anim_meta_sprites_f0:
    .byte $00, $01

  player_anim_palette_f1:
    .byte $00, $00
  player_anim_delay_count_f1:
    .byte $ff, $0a
  player_anim_meta_sprites_f1:
    .byte $00, $02

  player_anim_palette_f2:
    .byte $00, $00
  player_anim_delay_count_f2:
    .byte $ff, $0a
  player_anim_meta_sprites_f2:
    .byte $00, $03

  player_anim_palette_f3:
    .byte $00, $00
  player_anim_delay_count_f3:
    .byte $ff, $0a
  player_anim_meta_sprites_f3:
    .byte $00, $02
    
  player_anim_palette_f4:
    .byte $00, $00
  player_anim_delay_count_f4:
    .byte $ff, $ff
  player_anim_meta_sprites_f4:
    .byte $00, $00

  player_anim_palette_f5:
    .byte $00, $00
  player_anim_delay_count_f5:
    .byte $ff, $ff
  player_anim_meta_sprites_f5:
    .byte $00, $00


player_meta_sprites_sprite_count:
  .byte $06

player_metasprite_x_offset_addr_lo:
  .byte <player_sprites_x_offset_00, <player_sprites_x_offset_01, <player_sprites_x_offset_02, <player_sprites_x_offset_03, <player_sprites_x_offset_04, <player_sprites_x_offset_05, <player_sprites_x_offset_06, <player_sprites_x_offset_07
player_metasprite_x_offset_addr_hi:
  .byte >player_sprites_x_offset_00, >player_sprites_x_offset_01, >player_sprites_x_offset_02, >player_sprites_x_offset_03, >player_sprites_x_offset_04, >player_sprites_x_offset_05, >player_sprites_x_offset_06, >player_sprites_x_offset_07
player_metasprite_y_offset_addr_lo:
  .byte <player_sprites_y_offset_00, <player_sprites_y_offset_01, <player_sprites_y_offset_02, <player_sprites_y_offset_03, <player_sprites_y_offset_04, <player_sprites_y_offset_05, <player_sprites_y_offset_06, <player_sprites_y_offset_07
player_metasprite_y_offset_addr_hi:
  .byte >player_sprites_y_offset_00, >player_sprites_y_offset_01, >player_sprites_y_offset_02, >player_sprites_y_offset_03, >player_sprites_y_offset_04, >player_sprites_y_offset_05, >player_sprites_y_offset_06, >player_sprites_y_offset_07

player_metasprite_index_addr_lo:
  .byte <player_meta_sprites_00, <player_meta_sprites_01, <player_meta_sprites_02, <player_meta_sprites_03, <player_meta_sprites_04, <player_meta_sprites_05, <player_meta_sprites_06, <player_meta_sprites_07
player_metasprite_index_addr_hi:
  .byte >player_meta_sprites_00, >player_meta_sprites_01, >player_meta_sprites_02, >player_meta_sprites_03, >player_meta_sprites_04, >player_meta_sprites_05, >player_meta_sprites_06, >player_meta_sprites_07


player_meta_sprites_offset:
  player_sprites_x_offset_00:
    .byte $00
  player_sprites_x_offset_01:
    .byte $08
  player_sprites_x_offset_02:
    .byte $00
  player_sprites_x_offset_03:
    .byte $08
  player_sprites_x_offset_04:
    .byte $00
  player_sprites_x_offset_05:
    .byte $08
  player_sprites_x_offset_06:
    .byte $00
  player_sprites_x_offset_07:
    .byte $08

  player_sprites_y_offset_00:
    .byte $00
  player_sprites_y_offset_01:
    .byte $00
  player_sprites_y_offset_02:
    .byte $08
  player_sprites_y_offset_03:
    .byte $08
  player_sprites_y_offset_04:
    .byte $10
  player_sprites_y_offset_05:
    .byte $10
  player_sprites_y_offset_06:
    .byte $00
  player_sprites_y_offset_07:
    .byte $08


          ;id  w00  w01  w02
player_meta_sprites_index:
  player_meta_sprites_00:
    .byte $00, $00, $02, $00
  player_meta_sprites_01:
    .byte $01, $01, $03, $01
  player_meta_sprites_02:
    .byte $14, $14, $12, $10
  player_meta_sprites_03:
    .byte $15, $15, $13, $11
  player_meta_sprites_04:
    .byte $20, $24, $22, $20
  player_meta_sprites_05:
    .byte $21, $25, $23, $21

  player_meta_sprites_06:
    .byte $00, $00, $00, $00
  player_meta_sprites_07:
    .byte $00, $00, $00, $00

    ; .byte $00, $01, $14, $15, $20, $21  ;idle
    ; .byte $00, $01, $14, $15, $24, $25 ; Frame 4
    ; .byte $02, $03, $12, $13, $22, $23 ; Frame 1
    ; .byte $00, $01, $10, $11, $20, $21 ; Frame 2

;format = first byte anim frame count, second byte frame duration, 
    ;3rd byte palette, 4-5 byte frame address
    ; player_00_anim_idle:
    ;   .byte $01
    ;   .byte $ff, $00
    ;   .addr player_00_anim_idle_00
    ;   player_00_anim_idle_00:
    ;     .byte $00, $01, $14, $15, $20, $21
      
    ; player_00_anim_walk:    
    ;   .byte $04
    ;   .byte $0a, $00
    ;   .addr player_00_anim_walk_00
    ;   .byte $0a, $00
    ;   .addr player_00_anim_walk_01
    ;   .byte $0a, $00
    ;   .addr player_00_anim_walk_02
    ;   .byte $0a, $00
    ;   .addr player_00_anim_walk_03
    ;   player_00_anim_walk_00:
    ;   player_00_anim_walk_01:
    ;     .byte $02, $03, $12, $13, $22, $23 ; Frame 3
    ;   player_00_anim_walk_02:
    ;   player_00_anim_walk_03:

    ; player_00_anim_attack:
    ;   .byte $00, $01, $14, $15, $24, $25 ; Frame 1
    ;   .byte $04, $05, $16, $17, $26, $27 ; Frame 2
    ;   .byte $04, $05, $07, $08, $20, $21 ; Frame 3 
    ; player_00_anim_guard:
    ;   .byte $04, $05, $14, $15, $24, $25 ; Frame 4
    ;   .byte $00, $01, $18, $19, $28, $29 ; Frame 4
    ;   .byte $00, $01, $18, $19, $28, $29 ; Frame 4

  ; sprites_data:
  ; sprites_index:
  ;   .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b
  ;   .byte $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1a, $1b
  ;   .byte $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $2a, $2b
  ; sprites_x_offset:
  ;   .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b
  ;   .byte $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1a, $1b
  ;   .byte $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $2a, $2b
  ; sprites_y_offset:
  ;   .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b
  ;   .byte $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1a, $1b
  ;   .byte $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $2a, $2b

  ; ; In frame 7, the player needs to be drawn 1 pixel forward
  ; ; because of how far he's leaned forward
  ; player_frame_to_xoffset:
  ;   .byte 0, 0, 0, 0, 0, 0, 0, 1
