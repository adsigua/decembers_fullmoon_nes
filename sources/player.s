.include "nes.inc"
.include "global.inc"
.include "entity.inc"

.segment "ZEROPAGE"
  player_index:           .res 1
  player_width:           .res 1
  player_half_width:      .res 1
  player_min_height:      .res 1
  player_max_height:      .res 1

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

; left_screen_limit = $08
; right_screen_limit = $fd
; top_floor_limit = $8a
; bottom_floor_limit = $de
player_invu_time = $38

.segment "CODE"
.proc init_player_entity
  lda #0
  sta player_index
  sta entity_id
  sta entity_anim_delay_cnt
  sta entity_velocity_x
  sta entity_velocity_y
  sta entity_pos_x_lo
  sta entity_pos_y_lo

  lda #$10
  sta entity_invu_time

  lda #EntityType::player_type
  ora #EntityBehaviourType::moving_animated
  sta entity_type

  ldx player_index

  lda player_anim_idle, x
  sta entity_anim_id

  lda #EntityState::Idle | EntityState::State_Changed | EntityState::Facing_Right
  sta entity_state


  lda player_char_half_width, x
  sta player_half_width

  lda player_char_height, x
  sta player_max_height
  sec
  sbc #$09                  
  sta player_min_height     ;offset about more than one tile up

  ldy player_max_height
  jsr init_player_level_pos

  rts
.endproc

.proc update_player
  jsr check_player_hurt
  
  lda entity_state
  and #$0f
  cmp #EntityState::Attacking
  bcs @end_check_player_input

  jsr check_player_action_input

  lda entity_state
  and #$0f
  cmp #EntityState::Attacking
  bcs @end_check_player_input

  @check_movement_input:
  jsr check_player_move_input

  @end_check_player_input:
  rts
.endproc

.proc check_player_action_input
  lda cur_keys
  and #KEY_A
  beq @no_attack_input  
  @attack_input:
    lda entity_state
    and #$f0
    ora #EntityState::State_Changed | EntityState::Attacking
    sta entity_state

    ldx player_index
    lda player_anim_attack, x

    jmp @store_action_input

  @no_attack_input:
  lda cur_keys
  and #KEY_B
  beq @no_guard_input  
  @guard_input:
    lda entity_state
    and #$f0
    ora #EntityState::State_Changed | EntityState::Guarding
    sta entity_state

    ldx player_index
    lda player_anim_guard, x

  @store_action_input:
    sta entity_anim_id

    ldx #0
    jsr update_player_frame_delay_cnt

    jsr remove_player_velocity

  @no_guard_input:
  rts
.endproc

.proc remove_player_velocity
  ldx #0
  jsr remove_entity_velocity
  rts
.endproc

.proc check_player_hurt
  lda entity_invu_time
  bne @no_select_key

  lda cur_keys
  and #KEY_SELECT
  beq @no_select_key  
  @select_input:
    lda entity_state
    and #$f0
    ora #EntityState::State_Changed | EntityState::Hurt
    sta entity_state

    lda #player_invu_time
    sta entity_invu_time

    ldx player_index
    lda player_anim_hurt, x

    sta entity_anim_id

    ldx #0
    jsr update_player_frame_delay_cnt

    jsr remove_player_velocity

  @no_select_key:
  rts
.endproc

; Moves the player character in response to controller 1.
; facing 0=left, 1=right
.proc check_player_move_input
  ldx player_index

  lda cur_keys
  and #KEY_RIGHT
  beq @notRight     
    lda player_move_speed, x    
    sta entity_velocity_x

    lda entity_state
    ora #EntityState::Facing_Right
    sta entity_state

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
    and #<~EntityState::Facing_Right
    sta entity_state

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
    jmp @doneInputY                    
  @noInput_y:
    lda #0
    sta entity_velocity_y 
  @doneInputY:
  
  jsr check_move_state_change

  @end_player_velocity_check:
    rts
.endproc

.proc check_move_state_change
  lda entity_velocity_x
  bne @check_state_change_moving
  lda entity_velocity_y
  bne @check_state_change_moving

  @check_state_change_still:
    lda entity_state
    and #$0f
    cmp #EntityState::Idle
    beq @end_move_state_change_check
      lda entity_state
      and #$f0
      ora #EntityState::State_Changed | EntityState::Idle
      sta entity_state

      lda player_anim_idle, x
      sta entity_anim_id

      ldx #0
      jsr update_player_frame_delay_cnt
    jmp @end_move_state_change_check

  @check_state_change_moving:
    lda entity_state
    and #EntityState::Moving
    bne @end_move_state_change_check
      lda entity_state
      and #$f0
      ora #EntityState::State_Changed | EntityState::Moving
      sta entity_state
      
      lda player_anim_walk, x
      sta entity_anim_id

      ldx #0
      jsr update_player_frame_delay_cnt
    @end_move_state_change_check:
    rts
.endproc

;x = frame num
.proc update_player_frame_delay_cnt
  lda player_anim_frame_delay_addr_lo, x 
  sta tempX
  lda player_anim_frame_delay_addr_hi, x
  sta tempX+1
  
  stx tempZ
  ldx #0
  jsr update_entity_frame_delay_cnt

  rts
.endproc

.proc update_player_entity
  jsr update_player_velocity
  jsr update_player_invu_state
  jsr update_player_animation
  ;jsr update_player_oam_buffer
  rts
.endproc

.proc update_player_invu_state
  ldy entity_invu_time
  beq @invu_state_check_end
  dey
  sty entity_invu_time
  @invu_state_check_end:
  rts
.endproc

;buffers new pos x and y based on velocity values, then apply velocity
.proc update_player_velocity
  lda #0
  sta scroll_x_delta

  lda entity_velocity_x
  beq @check_y_velocity
  @check_x_velocity:
  lda entity_pos_x
  sta temp_pos_hi
  lda entity_velocity_x
  bpl @entity_positive_x_speed
    ; if velocity is negative, subtract 1 from high byte to sign extend
    dec temp_pos_hi
  @entity_positive_x_speed:
    clc
    adc entity_pos_x_lo
    sta temp_pos_lo
    lda #0          
    adc temp_pos_hi
    sta temp_pos_hi

    jsr apply_velocity_x

  @check_y_velocity:
  lda entity_pos_y
  sta temp_pos_hi

  lda entity_velocity_y
  beq @end_velocity_update

  bpl @entity_positive_y_speed
    dec temp_pos_hi
  @entity_positive_y_speed:
    clc
    adc entity_pos_y_lo
    sta temp_pos_lo
    lda #0        
    adc temp_pos_hi
    sta temp_pos_hi

    jsr apply_velocity_y
  @end_velocity_update:
  rts
.endproc

;applies velocity if new position doesn't have any collisions or not edge of screen
.proc apply_velocity_x
  
  lda entity_velocity_x
  bmi @bound_x_to_left
  @bound_x_to_right:         
    jsr check_collision_right
    cmp #0
    bne @end_check_velocity_x

    bit level_flags       ;check if can scroll
    bpl @apply_x_pos
    ldy temp_pos_hi
    cpy #min_bg_xpos_scroll
    bcc @apply_x_pos
  @apply_scroll:
    lda entity_velocity_x
    ;sta scroll_x_delta
    jsr apply_scroll_x
    rts
  @bound_x_to_left:
    jsr check_collision_left
    cmp #0
    bne @end_check_velocity_x 
  @apply_x_pos:
    ldx #0
    jsr apply_pos_x
  @end_check_velocity_x:
    rts
.endproc

.proc apply_velocity_y
  lda entity_velocity_y
  bmi @bound_y_to_top
  @bound_y_to_bottom:         
    jsr check_collision_bottom
    cmp #0
    bne @end_check_velocity_y
    jmp @apply_y_pos
  @bound_y_to_top:
    jsr check_collision_top
    cmp #0
    bne @end_check_velocity_y 
  @apply_y_pos:
    ldx #0
    jsr apply_pos_y
  @end_check_velocity_y:
    rts
.endproc

.proc check_collision_right
  temp_pos_x = $00
  lda temp_pos_hi
  clc
  adc player_half_width
  sta temp_pos_x

  lda player_min_height
  clc
  adc entity_pos_y
  tay
  ldx temp_pos_x              ;if there is collision return (no movement)
  jsr get_tile_type           ;check if top of tile has collision
  bne @end_collision_check_right

  lda player_max_height       ;check collision for bottom of feet
  clc
  adc entity_pos_y
  tay
  ldx temp_pos_x
  jsr get_tile_type
  bne @end_collision_check_right

  ; lda #right_screen_limit     ;check if colliding on right screen
  ; sec
  ; sbc player_half_width
  ; cmp temp_pos_hi
  lda #0
  ldx temp_pos_x
  cpx #right_screen_limit
  bcc @end_collision_check_right
    lda #$01
  @end_collision_check_right:
  rts
.endproc

.proc check_collision_left
  temp_pos_x = $00
  lda temp_pos_hi
  sec
  sbc player_half_width
  sta temp_pos_x

  lda player_min_height
  clc
  adc entity_pos_y
  tay
  ldx temp_pos_x
  jsr get_tile_type           ;check if top of tile has collision
  bne @end_collision_check_left

  lda player_max_height       ;check collision for bottom of feet
  clc
  adc entity_pos_y
  tay
  ldx temp_pos_x
  jsr get_tile_type
  bne @end_collision_check_left

  lda #0
  ldx temp_pos_x
  cpx #left_screen_limit
  bcs @end_collision_check_left
    lda #$01
  @end_collision_check_left:
  rts
.endproc

.proc check_collision_bottom
  temp_pos_y = $00
  lda player_max_height
  clc
  adc temp_pos_hi
  sta temp_pos_y

  ldy temp_pos_y              ;cheeck for collision going down from bot left
  lda entity_pos_x
  sec
  sbc player_half_width
  tax                         
  jsr get_tile_type           
  bne @end_collision_check_bottom

  ldy temp_pos_y              ;cheeck for collision going down from center
  ldx entity_pos_x
  jsr get_tile_type
  bne @end_collision_check_bottom

  ldy temp_pos_y              ;cheeck for collision going down from bot right
  lda entity_pos_x
  clc
  adc player_half_width
  tax                         
  jsr get_tile_type           
  bne @end_collision_check_bottom

  @end_collision_check_bottom:
  rts
.endproc

.proc check_collision_top
  temp_pos_y = $00
  lda player_min_height
  clc
  adc temp_pos_hi
  sta temp_pos_y

  ldy temp_pos_y              ;cheeck for collision going down from bot left
  lda entity_pos_x
  sec
  sbc player_half_width
  tax                         
  jsr get_tile_type           
  bne @end_collision_check_top

  ldy temp_pos_y              ;cheeck for collision going down from center
  ldx entity_pos_x
  jsr get_tile_type
  bne @end_collision_check_top

  ldy temp_pos_y              ;cheeck for collision going down from bot right
  lda entity_pos_x
  clc
  adc player_half_width
  tax                         
  jsr get_tile_type          

  lda #0
  @end_collision_check_top:
  rts
.endproc

.proc update_player_animation
    ;check if there was state change, if not check if delay cnt is 0
    bit entity_state
    bpl :+
      lda entity_state
      eor #EntityState::State_Changed
      sta entity_state
      jmp @state_change_anim
    :
    lda entity_anim_delay_cnt
    beq @update_anim_frame

    ;decrement frame
    @decrement_delay_count:
      dec entity_anim_delay_cnt
      jmp @end_anim_check

    @state_change_anim:
      ldx entity_anim_id
      lda player_anim_frame_count, x
      cmp #1
      beq @end_anim_check
      lda #0
      sta entity_anim_frame_id
      jmp @store_frame_delay_count

    @update_anim_frame:
      ;check if anim is only 1 frame, if yes exit anim update
      ldx entity_anim_id
      lda player_anim_frame_count, x
      cmp #1
      beq @end_anim_check

      @check_next_frame:
        inc entity_anim_frame_id
        lda entity_anim_frame_id
        cmp player_anim_frame_count, x
        bcs @is_over_frame_count
        jmp @store_frame_delay_count

      @is_over_frame_count:
        ;animation over
        jsr check_player_anim_end
        lda #0
        sta entity_anim_frame_id

      @store_frame_delay_count:
        tax
        jsr update_player_frame_delay_cnt
        
  @end_anim_check:
    rts
.endproc

.proc check_player_anim_end
  lda entity_state
  and #$0f
  cmp #EntityState::Attacking
  bcc @check_player_guard_anim_end
  ;jmp @check_player_guard_anim_end  
  @end_player_action_anim:
    lda entity_state
    and #$f0
    ora #EntityState::State_Changed | EntityState::Idle
    sta entity_state

    ldx player_index
    lda player_anim_idle, x
    sta entity_anim_id
  @check_player_guard_anim_end:
  rts
.endproc

; .proc update_player_oam_buffer
  ;   curr_entity_xpos = $00
  ;   curr_entity_ypos= $01
  ;   curr_entity_state = $02
  ;   entity_meta_sprite_index = $03
  ;   sprite_count = $04
  ;   pallete_index = $05
  ;   curr_sprite_index = $06
  ;   tempAddr_lo = tempX
  ;   tempAddr_hi = tempX+1

  ;   lda entity_pos_x
  ;   sta curr_entity_xpos
  ;   lda entity_pos_y
  ;   sta curr_entity_ypos
  ;   lda entity_state
  ;   sta curr_entity_state

  ;   ldx entity_anim_frame_id
  ;   ldy entity_anim_id


  ;   ;sprite number
  ;   ldx sprite_count
  ;   dex
  ;   ;stx curr_sprite_index

  ;   @draw_entity_loop:
      

  ;     jsr draw_entity_sprite
  ;   @place_buffer_to_oam:
  ;     jsr update_oam_buffer
  ;     dex 
  ;     ;stx curr_sprite_index
  ;     bpl @draw_entity_loop

  ;   @end_player_oam_buffer:
  ;     rts
  ; .endproc

;x = frame index of anim
.proc load_player_metasprite_data
  curr_entity_xpos = $00
  curr_entity_ypos= $01
  curr_entity_state = $02
  entity_meta_sprite_index = $03
  sprite_count = $04
  pallete_index = $05
  tempAddr_lo = tempX
  tempAddr_hi = tempX+1
  
  lda player_anim_frame_palette_addr_lo, x
  sta tempAddr_lo
  lda player_anim_frame_palette_addr_hi, x
  sta tempAddr_hi

  lda (tempAddr_lo), y
  sta pallete_index

  lda player_anim_frame_metasprite_addr_lo, x
  sta tempAddr_lo
  lda player_anim_frame_metasprite_addr_hi, x
  sta tempAddr_hi

  lda (tempAddr_lo), y
  sta entity_meta_sprite_index

  lda player_anim_frame_sprite_count_addr_lo, x
  sta tempAddr_lo
  lda player_anim_frame_sprite_count_addr_hi, x
  sta tempAddr_hi

  lda (tempAddr_lo), y
  sta sprite_count
  rts
.endproc
;x = sprite index in meta sprite
.proc load_player_metasprites_addresses
  lda player_metasprite_y_offset_addr_lo, x
  sta curr_y_offset_addr
  lda player_metasprite_y_offset_addr_hi, x
  sta curr_y_offset_addr+1
  
  lda player_metasprite_index_addr_lo, x
  sta curr_metasprite_addr
  lda player_metasprite_index_addr_hi, x
  sta curr_metasprite_addr+1

  lda player_metasprite_x_offset_addr_lo, x
  sta curr_x_offset_addr
  lda player_metasprite_x_offset_addr_hi, x
  sta curr_x_offset_addr+1
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

  player_char_height:
    .byte $18

  player_char_half_width:
    .byte $08

  player_00_data:
    .byte $00

  player_anim_idle:
    .byte $00

  player_anim_walk:
    .byte $01

  player_anim_attack:
    .byte $02

  player_anim_guard:
    .byte $03

  player_anim_hurt:
    .byte $04

  player_move_speed:
    .byte $7f                   ;entity move speed

 

  player_anim_frame_palette_addr_lo:
    .byte <player_anim_palette_f0, <player_anim_palette_f1, <player_anim_palette_f2, <player_anim_palette_f3 
    .byte <player_anim_palette_f4, <player_anim_palette_f5, <player_anim_palette_f6, <player_anim_palette_f7 
  player_anim_frame_palette_addr_hi:
    .byte >player_anim_palette_f0, >player_anim_palette_f1, >player_anim_palette_f2, >player_anim_palette_f3
    .byte >player_anim_palette_f4, >player_anim_palette_f5, >player_anim_palette_f6, >player_anim_palette_f7 
  
  player_anim_frame_delay_addr_lo:
    .byte <player_anim_delay_count_f0, <player_anim_delay_count_f1, <player_anim_delay_count_f2, <player_anim_delay_count_f3
    .byte <player_anim_delay_count_f4, <player_anim_delay_count_f5, <player_anim_delay_count_f6, <player_anim_delay_count_f7
  player_anim_frame_delay_addr_hi:
    .byte >player_anim_delay_count_f0, >player_anim_delay_count_f1, >player_anim_delay_count_f2, >player_anim_delay_count_f3
    .byte >player_anim_delay_count_f4, >player_anim_delay_count_f5, >player_anim_delay_count_f6, >player_anim_delay_count_f7
 
  player_anim_frame_metasprite_addr_lo:
    .byte <player_anim_meta_sprites_f0, <player_anim_meta_sprites_f1, <player_anim_meta_sprites_f2, <player_anim_meta_sprites_f3 
    .byte <player_anim_meta_sprites_f4, <player_anim_meta_sprites_f5, <player_anim_meta_sprites_f2, <player_anim_meta_sprites_f3 
  player_anim_frame_metasprite_addr_hi:
    .byte >player_anim_meta_sprites_f0, >player_anim_meta_sprites_f1, >player_anim_meta_sprites_f2, >player_anim_meta_sprites_f3 
    .byte >player_anim_meta_sprites_f4, >player_anim_meta_sprites_f5, >player_anim_meta_sprites_f6, >player_anim_meta_sprites_f7  

  player_anim_frame_sprite_count_addr_lo:
    .byte <player_anim_sprite_count_f0, <player_anim_sprite_count_f1, <player_anim_sprite_count_f2, <player_anim_sprite_count_f3 
    .byte <player_anim_sprite_count_f4, <player_anim_sprite_count_f5, <player_anim_sprite_count_f2, <player_anim_sprite_count_f3 
  player_anim_frame_sprite_count_addr_hi:
    .byte >player_anim_sprite_count_f0, >player_anim_sprite_count_f1, >player_anim_sprite_count_f2, >player_anim_sprite_count_f3 
    .byte >player_anim_sprite_count_f4, >player_anim_sprite_count_f5, >player_anim_sprite_count_f6, >player_anim_sprite_count_f7  

  ;anim frames data
      ;$00 - p_0 idle
      ;$01 - p_0 walk
      ;$02 - p_0 attack
      ;$03 - p_0 guard
      ;$04 - p_0 hurt

 player_anim_frame_count:
    .byte $01, $04, $03, $06,   $05
          ;0    1    2    3      4    5    6    7      8    9    a    b    c    d    e    f
  player_anim_meta_sprites_f0:
    .byte $00, $02, $04, $05,   $08
  player_anim_delay_count_f0:
    .byte $00, $0a, $07, $07,   $05
  player_anim_sprite_count_f0:
    .byte $08, $07, $08, $06,   $06
  player_anim_palette_f0:
    .byte $00, $00, $00, $00,   $00

  player_anim_meta_sprites_f1:
    .byte $00, $01, $05, $07,   $08
  player_anim_delay_count_f1:
    .byte $ff, $0a, $02, $04,   $05
  player_anim_sprite_count_f1:
    .byte $06, $06, $07, $06,   $06
  player_anim_palette_f1:
    .byte $00, $00, $00, $00,   $01

  player_anim_meta_sprites_f2:
    .byte $00, $02, $06, $07,   $08
  player_anim_delay_count_f2:
    .byte $ff, $0a, $13, $04,   $05
  player_anim_sprite_count_f2:
    .byte $06, $07, $08, $06,   $06
  player_anim_palette_f2:
    .byte $00, $00, $00, $02,   $00

  player_anim_meta_sprites_f3:
    .byte $00, $03, $00, $07,   $08
  player_anim_delay_count_f3:
    .byte $ff, $0a, $00, $04,   $05
  player_anim_sprite_count_f3:
    .byte $06, $08, $00, $06,   $06
  player_anim_palette_f3:
    .byte $00, $00, $00, $00,   $01
    
  player_anim_meta_sprites_f4:
    .byte $00, $00, $00, $07,   $08
  player_anim_delay_count_f4:
    .byte $ff, $ff, $00, $04,   $05
  player_anim_sprite_count_f4:
    .byte $06, $08, $00, $06,   $06
  player_anim_palette_f4:
    .byte $00, $00, $00, $02,   $00

  player_anim_meta_sprites_f5:
    .byte $00, $00, $00, $07,   $04
  player_anim_delay_count_f5:
    .byte $ff, $ff, $00, $13,   $04
  player_anim_sprite_count_f5:
    .byte $06, $08, $00, $06,   $04
  player_anim_palette_f5:
    .byte $00, $00, $00, $00,   $04

  player_anim_meta_sprites_f6:
    .byte $00, $00, $00, $04,   $04
  player_anim_delay_count_f6:
    .byte $ff, $ff, $00, $04,   $04
  player_anim_sprite_count_f6:
    .byte $06, $08, $00, $04,   $04
  player_anim_palette_f6:
    .byte $00, $00, $00, $04,   $04

  player_anim_meta_sprites_f7:
    .byte $00, $00, $00, $04,   $04
  player_anim_delay_count_f7:
    .byte $ff, $ff, $00, $04,   $04
  player_anim_sprite_count_f7:
    .byte $06, $08, $00, $04,   $04
  player_anim_palette_f7:
    .byte $00, $00, $00, $04,   $04

player_metasprite_index_addr_lo:
  .byte <player_meta_sprites_00, <player_meta_sprites_01, <player_meta_sprites_02, <player_meta_sprites_03
  .byte <player_meta_sprites_04, <player_meta_sprites_05, <player_meta_sprites_06, <player_meta_sprites_07
  .byte <player_meta_sprites_08, <player_meta_sprites_09
player_metasprite_index_addr_hi:
  .byte >player_meta_sprites_00, >player_meta_sprites_01, >player_meta_sprites_02, >player_meta_sprites_03
  .byte >player_meta_sprites_04, >player_meta_sprites_05, >player_meta_sprites_06, >player_meta_sprites_07
  .byte >player_meta_sprites_08, >player_meta_sprites_09

player_metasprite_x_offset_addr_lo:
  .byte <player_meta_spites_x_offset_00, <player_meta_spites_x_offset_01, <player_meta_spites_x_offset_02, <player_meta_spites_x_offset_03
  .byte <player_meta_spites_x_offset_04, <player_meta_spites_x_offset_05, <player_meta_spites_x_offset_06, <player_meta_spites_x_offset_07
  .byte <player_meta_spites_x_offset_08, <player_meta_spites_x_offset_09
player_metasprite_x_offset_addr_hi:
  .byte >player_meta_spites_x_offset_00, >player_meta_spites_x_offset_01, >player_meta_spites_x_offset_02, >player_meta_spites_x_offset_03 
  .byte >player_meta_spites_x_offset_04, >player_meta_spites_x_offset_05, >player_meta_spites_x_offset_06, >player_meta_spites_x_offset_07 
  .byte >player_meta_spites_x_offset_08, >player_meta_spites_x_offset_09

player_metasprite_y_offset_addr_lo:
  .byte <player_meta_spites_y_offset_00, <player_meta_spites_y_offset_01, <player_meta_spites_y_offset_02, <player_meta_spites_y_offset_03 
  .byte <player_meta_spites_y_offset_04, <player_meta_spites_y_offset_05, <player_meta_spites_y_offset_06, <player_meta_spites_y_offset_07 
  .byte <player_meta_spites_y_offset_08, <player_meta_spites_y_offset_09
player_metasprite_y_offset_addr_hi:
  .byte >player_meta_spites_y_offset_00, >player_meta_spites_y_offset_01, >player_meta_spites_y_offset_02, >player_meta_spites_y_offset_03 
  .byte >player_meta_spites_y_offset_04, >player_meta_spites_y_offset_05, >player_meta_spites_y_offset_06, >player_meta_spites_y_offset_07
  .byte >player_meta_spites_y_offset_08, >player_meta_spites_y_offset_09


; player_meta_sprites_data:
  ;$00 - p_0 idle f0
  ;$01 - p_0 walk f1 
  ;$02 - p_0 walk f0 
  ;$03 - p_0 walk f2 
  ;$04 - p_0 attack f0 
  ;$05 - p_0 attack f1  /  p_0 guard f0
  ;$06 - p_0 attack f2 
  ;$07 - p_0 guard f1 
  ;$08 - p_0 hurt f1 

player_meta_sprites_index:
          ;0    1    2    3      4    5    6      7      8    9    a    b    c    d    e    f
  player_meta_sprites_00:
    .byte $00, $00, $02, $00,   $04, $04, $00,   $00,   $0c  
  player_meta_spites_x_offset_00:
    .byte $88, $88, $88, $88,   $88, $88, $88,   $88,   $88
  player_meta_spites_y_offset_00:
    .byte $00, $00, $00, $00,   $00, $00, $00,   $00,   $00

  player_meta_sprites_01:
    .byte $01, $01, $03, $01,   $05, $05, $01,   $01,   $0d  
  player_meta_spites_x_offset_01:
    .byte $00, $00, $00, $00,   $00, $00, $00,   $00,   $00
  player_meta_spites_y_offset_01:
    .byte $00, $00, $00, $00,   $00, $00, $00,   $00,   $00

  player_meta_sprites_02:
    .byte $10, $14, $12, $10,   $16, $14, $07,   $18,   $1c 
  player_meta_spites_x_offset_02:
    .byte $88, $88, $88, $88,   $88, $88, $88,   $88,   $88
  player_meta_spites_y_offset_02:
    .byte $08, $08, $08, $08,   $08, $08, $08,   $08,   $08

  player_meta_sprites_03:
    .byte $11, $15, $13, $11,   $17, $15, $08,   $19,   $1d  
  player_meta_spites_x_offset_03:
    .byte $00, $00, $00, $00,   $00, $00, $00,   $00,   $00
  player_meta_spites_y_offset_03:
    .byte $08, $08, $08, $08,   $08, $08, $08,   $08,   $08



  player_meta_sprites_04:
    .byte $20, $24, $22, $20,   $26, $26, $26,   $28,   $2c  
  player_meta_spites_x_offset_04:
    .byte $88, $88, $88, $88,   $88, $88, $88,   $88,   $88
  player_meta_spites_y_offset_04:
    .byte $10, $10, $10, $10,   $10, $10, $10,   $10,   $10

  player_meta_sprites_05:
    .byte $21, $25, $23, $21,   $27, $27, $27,   $29,   $2d  
  player_meta_spites_x_offset_05:
    .byte $00, $00, $00, $00,   $00, $00, $00,   $00,   $00
  player_meta_spites_y_offset_05:
    .byte $10, $10, $10, $10,   $10, $10, $10,   $10,   $10

  ;sw 1                               
  player_meta_sprites_06:
    .byte $2a, $ff, $2b, $2a,   $1b, $2b, $0a,   $ff,   $00  
  player_meta_spites_x_offset_06:
    .byte $8d, $84, $8c, $8d,   $06, $8c, $8f,   $ff,   $00    
  player_meta_spites_y_offset_06: 
    .byte $05, $0b, $05, $05,   $00, $02, $0a,   $ff,   $00
  
  ;sw 2                              
  player_meta_sprites_07:
    .byte $1a, $ff, $ff, $1a,   $0b, $ff, $09,   $ff,   $00  
  player_meta_spites_x_offset_07:
    .byte $8d, $84, $84, $8d,   $06, $8b, $97,   $ff,   $00   
  player_meta_spites_y_offset_07:
    .byte $83, $00, $00, $82,   $88, $03, $0a,   $ff,   $00



  player_meta_sprites_08:
    .byte $00, $00, $00, $00
  player_meta_spites_x_offset_08:
    .byte $00
  player_meta_spites_y_offset_08:
    .byte $00

  player_meta_sprites_09:
    .byte $00, $00, $00, $00
  player_meta_spites_x_offset_09:
    .byte $00
  player_meta_spites_y_offset_09:
    .byte $00



    ; .byte $00, $01, $14, $15, $20, $21  ;idle
    ; .byte $00, $01, $14, $15, $24, $25 ; Frame 4
    ; .byte $02, $03, $12, $13, $22, $23 ; Frame 1
    ; .byte $00, $01, $10, $11, $20, $21 ; Frame 2

;format = first byte anim frame count, second byte frame duration,
    ; adc scroll_x_fine
    ; cmp #$08
    ; bne @no_coarse_increment
    ;   inc scroll_x_coarse

    ;   lda #0
    ; @no_coarse_increment:
    ; sta scroll_x_fine

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


