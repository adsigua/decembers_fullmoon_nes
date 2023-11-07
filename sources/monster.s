.include "nes.inc"
.include "global.inc"
.include "entity.inc"

.segment "ZEROPAGE"
  entity_id:       .res 1
  entity_width:      .res 1
  entity_min_height:      .res 1
  entity_max_height:      .res 1

.segment "CODE"
.proc init_entity_entity
  lda #0
  sta entity_id
  sta entity_id
  sta entity_anim_delay_cnt
  sta entity_velocity_x
  sta entity_velocity_y
  sta entity_pos_x_lo
  sta entity_pos_y_lo

  lda #$10
  sta entity_invu_time

  lda #EntityType::entity_type
  ora #EntityBehaviourType::moving_animated
  sta entity_type

  ldx entity_id

  lda entity_anim_idle, x
  sta entity_anim_id

  lda #EntityState::Idle | EntityState::State_Changed | EntityState::Facing_Right
  sta entity_state

  lda entity_char_width, x
  sta entity_width
  lda entity_char_height, x
  sta entity_max_height
  sec
  sbc #$09                  
  sta entity_min_height     ;offset about more than one tile up

  ldy entity_max_height
  jsr init_entity_pos

  rts
.endproc

.proc update_player
  jsr check_entity_hurt
  
  lda entity_state
  and #$0f
  cmp #EntityState::Attacking
  bcs @end_check_entity_input

  jsr check_entity_action_input

  lda entity_state
  and #$0f
  cmp #EntityState::Attacking
  bcs @end_check_entity_input

  @check_movement_input:
  jsr check_entity_move_input

  @end_check_entity_input:
  rts
.endproc

.proc check_entity_action_input
  lda cur_keys
  and #KEY_A
  beq @no_attack_input  
  @attack_input:
    lda entity_state
    and #$f0
    ora #EntityState::State_Changed | EntityState::Attacking
    sta entity_state

    ldx entity_id
    lda entity_anim_attack, x

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

    ldx entity_id
    lda entity_anim_guard, x

  @store_action_input:
    sta entity_anim_id

    ldx #0
    jsr update_frame_delay_cnt

    jsr remove_entity_velocity

  @no_guard_input:
  rts
.endproc

.proc remove_entity_velocity
  lda #0
  sta entity_velocity_x
  sta entity_velocity_y
  rts
.endproc

.proc check_entity_hurt
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

    ldx entity_id
    lda entity_anim_hurt, x

    sta entity_anim_id

    ldx #0
    jsr update_frame_delay_cnt

    jsr remove_entity_velocity

  @no_select_key:
  rts
.endproc


; Moves the player character in response to controller 1.
; facing 0=left, 1=right
.proc check_entity_move_input
  ldx entity_id

  lda cur_keys
  and #KEY_RIGHT
  beq @notRight     
    lda entity_move_speed, x    
    sta entity_velocity_x

    lda entity_state
    ora #EntityState::Facing_Right
    sta entity_state

    jmp @doneInputX
  @notRight:
  lda cur_keys
  and #KEY_LEFT
  beq @noInput_x
    lda entity_move_speed, x      
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
    lda entity_move_speed, x
    sta entity_velocity_y
    jmp @doneInputY
  @notDown:
  lda cur_keys
  and #KEY_UP
  beq @noInput_y
    lda entity_move_speed, x    
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

  @end_entity_velocity_check:
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

      lda entity_anim_idle, x
      sta entity_anim_id

      ldx #0
      jsr update_frame_delay_cnt
    jmp @end_move_state_change_check

  @check_state_change_moving:
    lda entity_state
    and #EntityState::Moving
    bne @end_move_state_change_check
      lda entity_state
      and #$f0
      ora #EntityState::State_Changed | EntityState::Moving
      sta entity_state
      
      lda entity_anim_walk, x
      sta entity_anim_id

      ldx #0
      jsr update_frame_delay_cnt
    @end_move_state_change_check:
    rts
.endproc

;x = frame num
.proc update_frame_delay_cnt
  lda entity_anim_frame_delay_addr_lo, x 
  sta tempX
  lda entity_anim_frame_delay_addr_hi, x
  sta tempX+1
  
  ldy entity_anim_id
  lda (tempX), y
  sta entity_anim_delay_cnt
  stx entity_anim_frame_id
  rts
.endproc


.proc update_entity_entity
  jsr update_entity_velocity
  jsr update_entity_invu_state
  jsr update_entity_animation
  jsr update_entity_oam_buffer
  rts
.endproc


.proc update_entity_invu_state
  ldy entity_invu_time
  beq @invu_state_check_end
  dey
  sta entity_invu_time
  @invu_state_check_end:
  rts
.endproc


;buffers new pos x and y based on velocity values, then apply velocity
.proc update_entity_velocity
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

    jsr apply_entity_velocity_x

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

    jsr apply_entity_velocity_y
  @end_velocity_update:
  rts
.endproc

;applies velocity if new position doesn't have any collisions or not edge of screen
.proc apply_entity_velocity_x
  lda entity_velocity_x
  bmi @bound_x_to_left
  @bound_x_to_right:         
    jsr check_collision_right
    bne @end_check_velocity_x

    bit level_flags       ;check if can scroll
    bpl @apply_x_pos
    ldy temp_pos_hi
    cpy #min_bg_xpos_scroll
    bcc @apply_x_pos
  @apply_scroll:
    lda entity_velocity_x
    jsr apply_scroll_x
    rts
  @bound_x_to_left:
    jsr check_collision_left
    bne @end_check_velocity_x 
  @apply_x_pos:
    jsr apply_pos_x
  @end_check_velocity_x:
    rts
.endproc

.proc apply_entity_velocity_y
  lda entity_velocity_y
  bmi @bound_y_to_top
  @bound_y_to_bottom:         
    jsr check_collision_bottom
    bne @end_check_velocity_y
    jmp @apply_y_pos
  @bound_y_to_top:
    jsr check_collision_top
    bne @end_check_velocity_y 
  @apply_y_pos:
    jsr apply_pos_y
  @end_check_velocity_y:
    rts
.endproc

.proc check_collision_right
  lda entity_min_height
  clc
  adc entity_pos_y
  tay
  lda entity_width
  clc
  adc temp_pos_hi
  tax                         ;if there is collision return (no movement)
  jsr get_tile_type           ;check if top of tile has collision
  bne @end_collision_check_right

  lda entity_max_height       ;check collision for bottom of feet
  clc
  adc entity_pos_y
  tay
  lda entity_width
  clc
  adc temp_pos_hi
  tax
  jsr get_tile_type
  bne @end_collision_check_right

  lda #right_screen_limit     ;check if colliding on right screen
  sec
  sbc entity_width
  cmp temp_pos_hi
  lda #0
  bcs @end_collision_check_right
    lda #$01
  @end_collision_check_right:
  rts
.endproc

.proc check_collision_left
  lda entity_min_height
  clc
  adc entity_pos_y
  tay
  lda temp_pos_hi
  tax                         ;if there is collision return (no movement)
  jsr get_tile_type           ;check if top of tile has collision
  bne @end_collision_check_left

  lda entity_max_height       ;check collision for bottom of feet
  clc
  adc entity_pos_y
  tay
  lda temp_pos_hi
  tax
  jsr get_tile_type
  bne @end_collision_check_left

  lda temp_pos_hi
  cmp #left_screen_limit
  lda #0
  bcs @end_collision_check_left
    lda #$01
  @end_collision_check_left:
  rts
.endproc

.proc check_collision_bottom
  lda entity_max_height
  clc
  adc temp_pos_hi
  tay
  lda entity_pos_x
  tax                         ;if there is collision return (no movement)
  jsr get_tile_type           ;check going up top left collision
  bne @end_collision_check_bottom

  lda entity_max_height       ;cheeck for collision going up, from top right
  clc
  adc temp_pos_hi
  tay
  lda entity_width
  clc
  adc entity_pos_x
  tax
  jsr get_tile_type

  @end_collision_check_bottom:
  rts
.endproc

.proc check_collision_top
  lda entity_min_height
  clc
  adc temp_pos_hi
  tay
  lda entity_pos_x
  tax                         ;if there is collision return (no movement)
  jsr get_tile_type           ;check going up top left collision
  bne @end_collision_check_top

  lda entity_min_height       ;cheeck for collision going up, from top right
  clc
  adc temp_pos_hi
  tay
  lda entity_width
  clc
  adc entity_pos_x
  tax
  jsr get_tile_type

  @end_collision_check_top:
  rts
.endproc



.proc update_entity_animation
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
      lda entity_anim_frame_count, x
      cmp #1
      beq @end_anim_check
      lda #0
      sta entity_anim_frame_id
      jmp @store_frame_delay_count

    @update_anim_frame:
      ;check if anim is only 1 frame, if yes exit anim update
      ldx entity_anim_id
      lda entity_anim_frame_count, x
      cmp #1
      beq @end_anim_check

      @check_next_frame:
        inc entity_anim_frame_id
        lda entity_anim_frame_id
        cmp entity_anim_frame_count, x
        bcs @is_over_frame_count
        jmp @store_frame_delay_count

      @is_over_frame_count:
        ;animation over
        jsr check_entity_anim_end
        lda #0
        sta entity_anim_frame_id

      @store_frame_delay_count:
        tax
        jsr update_frame_delay_cnt
        
  @end_anim_check:
    rts
.endproc

.proc check_entity_anim_end
  lda entity_state
  and #$0f
  cmp #EntityState::Attacking
  bcc @check_entity_guard_anim_end
  ;jmp @check_entity_guard_anim_end  
  @end_entity_action_anim:
    lda entity_state
    and #$f0
    ora #EntityState::State_Changed | EntityState::Idle
    sta entity_state

    ldx entity_id
    lda entity_anim_idle, x
    sta entity_anim_id
  @check_entity_guard_anim_end:
  rts
.endproc

.proc update_entity_oam_buffer
  entity_meta_sprite_index = $06
  sprite_count = $07
  pallete_index = $08
  tempAddr_lo = tempX
  tempAddr_hi = tempX+1
  oamAttrbTemp = tempZ
  xOffsetTemp = tempZ

  ldx entity_anim_frame_id
  ldy entity_anim_id

  lda entity_anim_frame_palette_addr_lo, X
  sta tempAddr_lo
  lda entity_anim_frame_palette_addr_hi, X
  sta tempAddr_hi

  lda (tempAddr_lo), y
  sta pallete_index

  lda entity_anim_frame_metasprite_addr_lo, x
  sta tempAddr_lo
  lda entity_anim_frame_metasprite_addr_hi, x
  sta tempAddr_hi

  lda (tempAddr_lo), y
  sta entity_meta_sprite_index

  lda entity_anim_frame_sprite_count_addr_lo, x
  sta tempAddr_lo
  lda entity_anim_frame_sprite_count_addr_hi, x
  sta tempAddr_hi

  lda (tempAddr_lo), y
  sta sprite_count

  ;sprite number
  ldx #0

  @draw_entity_loop:
    jsr draw_entity
  @place_buffer_to_oam:
    jsr update_oam_buffer
    inx
    cpx sprite_count
    bne @draw_entity_loop

  @end_entity_oam_buffer:
    rts
.endproc




