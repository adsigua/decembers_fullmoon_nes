.include "nes.inc"
.include "global.inc"
.include "entity.inc"

.segment "ZEROPAGE"
  entity_count:           .res 1
  curr_entity_index:      .res 1
  entity_cycle_offset:    .res 1
  entity_prime_cycle_index: .res 1
  temp_pos_hi:            .res 1
  temp_pos_lo:            .res 1
  curr_x_offset_addr:     .res 2
  curr_y_offset_addr:     .res 2
  curr_metasprite_addr:   .res 2
  curr_entity_id:         .res 1

MAX_ENTITY_COUNT = $10

.segment "CODE"

.proc clear_entities
  lda #<entity_addr
  sta tempX
  lda #>entity_addr
  sta tempX+1

  ldy #<entity_addr_end
  lda #0
  sta entity_count
  @clear_entity_loop:
    sta (tempX), y
    dey
    bne @clear_entity_loop
  rts 
.endproc

.proc init_entities
  jsr clear_entities
  jsr init_player_entity
  inc entity_count
  rts
.endproc

.proc init_entity
    jmp @end_init

  @init_static_animated:

    jmp @end_init
  @end_init:
    rts
.endproc

;tempX = posX
;tempY = posY
;y = entity_id
.proc spawn_monster_entity
  posX = tempX
  posY = tempY
  temp_id = tempZ

  ldx entity_count
  cpx #MAX_ENTITY_COUNT
  beq @no_spawn

  sty temp_id

  ;find space in entity address where entity type is blank or zero
  ldy #1
  @find_entity_index_loop:
    lda entity_type, y
    beq @store_index_to_entity_stack
    iny
    jmp @find_entity_index_loop

  @store_index_to_entity_stack:  
    ;x = entity_count
    ;y = entity index
    tya
    sta entity_stack, x
    inc entity_count
    ldy temp_id
    tya
    sta entity_id, x

  lda #0
  sta entity_anim_delay_cnt, x
  sta entity_velocity_x, x
  sta entity_velocity_y, x
  sta entity_pos_x_lo, x
  sta entity_pos_y_lo, x
  sta entity_invu_time, x

  lda entity_data_type, y
  sta entity_type, x

  lda entity_anim_idle, y
  sta entity_anim_id, x

  lda #EntityState::Idle | EntityState::State_Changed
  sta entity_state, x

  lda posX
  sta entity_pos_x, x
  lda posY
  sta entity_pos_y, x


  @no_spawn:
  rts
.endproc

.proc update_entities
  ldx #0
  @update_entity_loop:
    txa
    pha
    lda entity_stack, x
    tax
    stx curr_entity_index

    lda entity_type, x
    and #$0f
    beq @no_update

    lda entity_type, x               
    and #$f0
    cmp #EntityType::player_type
    beq @update_player_entity
    bcs @update_nonplayer_entities
    jmp @no_update

    @update_player_entity:
      jsr update_player_entity
      jmp @no_update

    @update_nonplayer_entities:
      jsr update_nonplayer_entity
      
    @no_update:
    pla
    tax
    inx
    cpx entity_count
    bne @update_entity_loop
  rts
.endproc

;x = entity_index
.proc destroy_entity
  lda #0
  sta entity_type, x
                 

  @end_destroy_entity:
    rts
.endproc

.proc update_nonplayer_entity
  lda entity_type, x 
  and #$0f          
  cmp #EntityBehaviourType::static_animated
  beq @update_static_animated
  cmp #EntityBehaviourType::moving_animated
  beq @update_animated
  @update_static:
    jsr update_static_pos
    jmp @end_update_nonplayer_entity

  @update_static_animated:
    jmp @end_update_nonplayer_entity

  @update_animated:
    jsr update_entity_animation
    jsr update_static_pos
    ;jsr update_entity_oam_buffer

  @end_update_nonplayer_entity:
    rts
.endproc

;x = entity_index
.proc update_entity_animation
  curr_entity_state = $02

  ldx curr_entity_index
  lda entity_state, x
  sta curr_entity_state

  ;check if there was state change, if not check if delay cnt is 0
  bit curr_entity_state
  bpl :+
    lda curr_entity_state
    eor #EntityState::State_Changed
    sta curr_entity_state
    sta entity_state, x
    jmp @state_change_anim
  :
  lda entity_anim_delay_cnt, x
  beq @update_anim_frame

  ;decrement frame
  @decrement_delay_count:
    ldy entity_anim_delay_cnt, x
    dey
    tya
    sta entity_anim_delay_cnt, x
    jmp @end_anim_check

  @state_change_anim:
    lda curr_entity_state
    and #$0f
    tay
    lda entity_anim_frame_count, y
    cmp #1
    beq @end_anim_check

    lda #0
    sta entity_anim_frame_id, x
    jmp @store_frame_delay_count

  @update_anim_frame:
    ;check if anim is only 1 frame, if yes exit anim update
    ldy entity_anim_id, x
    lda entity_anim_frame_count, y
    cmp #1
    beq @end_anim_check

    @check_next_frame:
      inc entity_anim_frame_id, x
      lda entity_anim_frame_id, x
      cmp entity_anim_frame_count, y
      bcs @is_over_frame_count
      jmp @store_frame_delay_count

    @is_over_frame_count:
      ;animation over
      jsr check_entity_anim_end
      ldx curr_entity_index
      lda #0
      sta entity_anim_frame_id, x

    @store_frame_delay_count:
      sta tempZ
      tay 
      lda entity_anim_frame_delay_addr_lo, y 
      sta tempX
      lda entity_anim_frame_delay_addr_hi, y
      sta tempX+1
      jsr update_entity_frame_delay_cnt
      
  @end_anim_check:
  rts
.endproc

;tempX,Y = frame delay addr
;tempZ = frame id
;x = entity index
.proc update_entity_frame_delay_cnt
  temp_frame_delay_addr_lo = tempX
  temp_frame_id = tempZ

  ldy entity_anim_id, x
  lda (temp_frame_delay_addr_lo), y
  sta entity_anim_delay_cnt, x
  lda temp_frame_id
  sta entity_anim_frame_id, x
  rts
.endproc

;x = entity index
.proc check_entity_anim_end
  curr_entity_anim_frame_count = $00
  curr_entity_ypos= $01
  curr_entity_state = $02
  entity_meta_sprite_index = $03
  sprite_count = $04
  pallete_index = $05
  tempAddr_lo = tempX
  tempAddr_hi = tempX+1
  
  lda curr_entity_state
  and #$0f
  cmp #EntityState::Attacking
  bcc @check_entity_anim_end
  ;jmp @check_player_guard_anim_end  
  @end_player_action_anim:
    lda entity_state
    and #$f0
    ora #EntityState::State_Changed | EntityState::Idle
    sta entity_state

    ldy entity_id, x
    lda entity_anim_idle, y
    sta entity_anim_id, x
  @check_entity_anim_end:
  rts
.endproc

;x = entity index
.proc apply_entity_velocity
  ldx curr_entity_index
  lda entity_velocity_x, x
  beq @check_scroll_delta

  @check_x_velocity:
  lda entity_pos_x, x
  sta temp_pos_hi
  lda entity_velocity_x, x
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

    lda scroll_x_delta
    bne @add_scroll_x_offset
    jsr apply_pos_x
    jmp @check_y_velocity

  @check_scroll_delta:
    lda scroll_x_delta
    beq @check_y_velocity

  @add_scroll_x_offset:
    dec temp_pos_hi
    clc
    adc scroll_x_delta
    sta temp_pos_lo
    lda #0          
    adc temp_pos_hi
    sta temp_pos_hi
    jsr apply_pos_x

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

    jsr apply_pos_y
  @end_velocity_update:
  rts
.endproc

;x = entity index
.proc update_static_pos
  ldx curr_entity_index
  lda scroll_x_delta
  beq @end_update_static_pos
    lda entity_pos_x, x
    sec
    sbc scroll_x_delta
    sta entity_pos_x, x
  @end_update_static_pos:
  rts
.endproc

;x = entity id
.proc apply_pos_x
  lda temp_pos_hi
  sta entity_pos_x, x
  lda temp_pos_lo
  sta entity_pos_x_lo, x
  rts
.endproc

;x = entity id
.proc apply_pos_y
  lda temp_pos_hi
  sta entity_pos_y, x
  lda temp_pos_lo
  sta entity_pos_y_lo, x
  rts
.endproc

;x = entity id
.proc remove_entity_velocity
  lda #0
  sta entity_velocity_x, x
  sta entity_velocity_y, x
  rts
.endproc

.proc entity_cycle_index_add
  prime_add_list_count = $07

  inc entity_prime_cycle_index
	lda entity_prime_cycle_index
	and #prime_add_list_count
	sta entity_prime_cycle_index
	tax
	lda entity_cycle_add_prime, x
	sta entity_cycle_offset
	rts
.endproc 

.proc draw_entities
  jsr entity_cycle_index_add
  lda #0
	:
    sta curr_entity_index
    pha
    tax
    lda entity_type, x
    beq @skip_draw      ;if none-type skip
    jsr draw_entity
    @skip_draw:
    pla
    clc
    adc entity_cycle_offset
    and #15
    bne :-
	rts
.endproc

;x = entity index
.proc draw_entity
  curr_entity_xpos = $00
  curr_entity_ypos= $01
  curr_entity_state = $02
  entity_meta_sprite_index = $03
  sprite_count = $04
  pallete_index = $05
  tempAddr_lo = tempX
  tempAddr_hi = tempX+1

  lda entity_pos_x, x
  sta curr_entity_xpos
  lda entity_pos_y, x
  sta curr_entity_ypos
  lda entity_state, x
  sta curr_entity_state

  ldy entity_anim_id, x
  lda entity_anim_frame_id, x
  tax

  lda curr_entity_index
  bne @load_nonplayer_data 

  @load_player_data:
    jsr load_player_metasprite_data
    jmp @draw_meta_sprite

  @load_nonplayer_data:
    jsr load_nonplayer_metasprite_data

  @draw_meta_sprite:
    ;sprite number
    ldx #0
    @draw_entity_loop:
      txa
      pha
      

      lda curr_entity_index
      bne @load_nonplayer_addr 
        jsr load_player_metasprites_addresses
        jmp @draw_ent_spr
      @load_nonplayer_addr:
        jsr load_nonplayer_metasprites_addresses
      @draw_ent_spr:      
        jsr draw_entity_sprite
    @place_buffer_to_oam:
      jsr update_oam_buffer
      pla
      tax
      inx
      cpx sprite_count
      bne @draw_entity_loop

  @end_entity_oam_buffer:
    rts
.endproc

;x = frame index of anim
.proc load_nonplayer_metasprite_data
  curr_entity_xpos = $00
  curr_entity_ypos= $01
  curr_entity_state = $02
  entity_meta_sprite_index = $03
  sprite_count = $04
  pallete_index = $05
  tempAddr_lo = tempX
  tempAddr_hi = tempX+1
  
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
  rts
.endproc

;x = sprite index in meta-sprite
.proc load_nonplayer_metasprites_addresses
  lda entity_metasprite_y_offset_addr_lo, x
  sta curr_y_offset_addr
  lda entity_metasprite_y_offset_addr_hi, x
  sta curr_y_offset_addr+1
  
  lda entity_metasprite_index_addr_lo, x
  sta curr_metasprite_addr
  lda entity_metasprite_index_addr_hi, x
  sta curr_metasprite_addr+1

  lda entity_metasprite_x_offset_addr_lo, x
  sta curr_x_offset_addr
  lda entity_metasprite_x_offset_addr_hi, x
  sta curr_x_offset_addr+1
  rts
.endproc

;x = sprite in
.proc draw_entity_sprite
  curr_entity_xpos = $00
  curr_entity_ypos= $01
  curr_entity_state = $02
  entity_meta_sprite_index = $03
  sprite_count = $04
  pallete_index = $05

  oamAttrbTemp = tempZ
  xOffsetTemp = tempZ

  ldy entity_meta_sprite_index

  lda (curr_x_offset_addr), y 
  bmi @negative_x_offset    
  @positive_x_offset:
    sta xOffsetTemp 
    lda curr_entity_xpos
    bit curr_entity_state
    bvs @is_plus_flipped
    @not_plus_flipped:
      clc
      adc xOffsetTemp
      bcs @skip_sprite
      jmp @place_x_offset_to_buffer
    @is_plus_flipped:
      sec
      sbc xOffsetTemp   
      bcc @skip_sprite
      sec
      sbc #$08
      jmp @place_x_offset_to_buffer
  @negative_x_offset:
    and #$7f
    sta xOffsetTemp 
    lda curr_entity_xpos
    bit curr_entity_state
    bvs @is_neg_flipped
    @not_neg_flipped:
      sec
      sbc xOffsetTemp
      bcc @skip_sprite
      jmp @place_x_offset_to_buffer
    @is_neg_flipped:
      clc
      adc xOffsetTemp   
      bcs @skip_sprite
      sec
      sbc #$08
      jmp @place_x_offset_to_buffer
  @skip_sprite:
    jmp @end_oam_buffer
  @place_x_offset_to_buffer:
    sta oam_buffer+3

  lda (curr_y_offset_addr), y    
  bmi @negative_y_offset
  @positive_y_offset:
    clc                  
    adc curr_entity_ypos
    jmp @store_y_offset_buffer
  @negative_y_offset:
    and #$7f
    sta tempZ
    lda curr_entity_ypos
    sec                  
    sbc tempZ
  @store_y_offset_buffer:
  sta oam_buffer

  lda (curr_metasprite_addr), y        
  sta oam_buffer+1

  ;flags (palette, flipping and bg priority)
  lda pallete_index
  sta oamAttrbTemp
  lda curr_entity_state
  and #SpriteAttrib::FlipX
  ora oamAttrbTemp
  sta oam_buffer+2             

  @end_oam_buffer:
  rts
.endproc


.segment "RODATA"
entity_cycle_add_prime: .byte 1, 5, 11, 15, 7, 3, 13, 9

;entity ids
  ;$00 - rock
  ;$01 - mini angel
entity_data:
  entity_data_type:
    .byte $53, $21
  
  entity_char_height:
    .byte $18, $08

  entity_char_width:
    .byte $10, $08

  entity_anim_idle:
    .byte $00, $01

  entity_anim_walk:
    .byte $00, $01

  entity_anim_attack:
    .byte $00, $01

  entity_anim_guard:
    .byte $00, $01

  entity_anim_hurt:
    .byte $00, $01

  entity_move_speed:
    .byte $7f, $7f                   

  entity_anim_frame_palette_addr_lo:
    .byte <entity_anim_palette_f0, <entity_anim_palette_f1, <entity_anim_palette_f2, <entity_anim_palette_f3 
    .byte <entity_anim_palette_f4, <entity_anim_palette_f5, <entity_anim_palette_f6, <entity_anim_palette_f7 
  entity_anim_frame_palette_addr_hi:
    .byte >entity_anim_palette_f0, >entity_anim_palette_f1, >entity_anim_palette_f2, >entity_anim_palette_f3
    .byte >entity_anim_palette_f4, >entity_anim_palette_f5, >entity_anim_palette_f6, >entity_anim_palette_f7 
  
  entity_anim_frame_delay_addr_lo:
    .byte <entity_anim_delay_count_f0, <entity_anim_delay_count_f1, <entity_anim_delay_count_f2, <entity_anim_delay_count_f3
    .byte <entity_anim_delay_count_f4, <entity_anim_delay_count_f5, <entity_anim_delay_count_f6, <entity_anim_delay_count_f7
  entity_anim_frame_delay_addr_hi:
    .byte >entity_anim_delay_count_f0, >entity_anim_delay_count_f1, >entity_anim_delay_count_f2, >entity_anim_delay_count_f3
    .byte >entity_anim_delay_count_f4, >entity_anim_delay_count_f5, >entity_anim_delay_count_f6, >entity_anim_delay_count_f7
 
  entity_anim_frame_metasprite_addr_lo:
    .byte <entity_anim_meta_sprites_f0, <entity_anim_meta_sprites_f1, <entity_anim_meta_sprites_f2, <entity_anim_meta_sprites_f3 
    .byte <entity_anim_meta_sprites_f4, <entity_anim_meta_sprites_f5, <entity_anim_meta_sprites_f2, <entity_anim_meta_sprites_f3 
  entity_anim_frame_metasprite_addr_hi:
    .byte >entity_anim_meta_sprites_f0, >entity_anim_meta_sprites_f1, >entity_anim_meta_sprites_f2, >entity_anim_meta_sprites_f3 
    .byte >entity_anim_meta_sprites_f4, >entity_anim_meta_sprites_f5, >entity_anim_meta_sprites_f6, >entity_anim_meta_sprites_f7  

  entity_anim_frame_sprite_count_addr_lo:
    .byte <entity_anim_sprite_count_f0, <entity_anim_sprite_count_f1, <entity_anim_sprite_count_f2, <entity_anim_sprite_count_f3 
    .byte <entity_anim_sprite_count_f4, <entity_anim_sprite_count_f5, <entity_anim_sprite_count_f2, <entity_anim_sprite_count_f3 
  entity_anim_frame_sprite_count_addr_hi:
    .byte >entity_anim_sprite_count_f0, >entity_anim_sprite_count_f1, >entity_anim_sprite_count_f2, >entity_anim_sprite_count_f3 
    .byte >entity_anim_sprite_count_f4, >entity_anim_sprite_count_f5, >entity_anim_sprite_count_f6, >entity_anim_sprite_count_f7  

;entity_frame_data
  ;$00 - mini_angel fly (idle, walk, attack)
  ;$01 - mini_angel hurt

  ;number of frames an animation has
  entity_anim_frame_count:
    .byte $01, $02, $04, $03,   $06,   $05
          ;0    1    2    3      4    5    6    7      8    9    a    b    c    d    e    f
  entity_anim_meta_sprites_f0:
    .byte $00, $01, $02, $04, $05,   $08
  entity_anim_delay_count_f0:
    .byte $f0, $08, $0a, $07, $0c,   $05
  entity_anim_sprite_count_f0:
    .byte $06, $04, $07, $08, $06,   $06
  entity_anim_palette_f0:
    .byte $02, $01, $00, $00, $00,   $00

  entity_anim_meta_sprites_f1:
    .byte $00, $02, $01, $05, $07,   $08
  entity_anim_delay_count_f1:
    .byte $00, $10, $0a, $02, $04,   $05
  entity_anim_sprite_count_f1:
    .byte $00, $04, $06, $07, $06,   $06
  entity_anim_palette_f1:
    .byte $00, $01, $00, $00, $00,   $01

  entity_anim_meta_sprites_f2:
    .byte $00, $00, $02, $06, $07,   $08
  entity_anim_delay_count_f2:
    .byte $00, $ff, $0a, $13, $04,   $05
  entity_anim_sprite_count_f2:
    .byte $00, $06, $07, $08, $06,   $06
  entity_anim_palette_f2:
    .byte $00, $00, $00, $00, $01,   $00

  entity_anim_meta_sprites_f3:
    .byte $00, $00, $03, $00, $07,   $08
  entity_anim_delay_count_f3:
    .byte $00, $ff, $0a, $00, $04,   $05
  entity_anim_sprite_count_f3:
    .byte $00, $00, $06, $08, $00, $06,   $06
  entity_anim_palette_f3:
    .byte $00, $00, $00, $00, $00,   $01
    
  entity_anim_meta_sprites_f4:
    .byte $00, $00, $00, $07,   $08
  entity_anim_delay_count_f4:
    .byte $ff, $ff, $00, $04,   $05
  entity_anim_sprite_count_f4:
    .byte $06, $08, $00, $06,   $06
  entity_anim_palette_f4:
    .byte $00, $00, $00, $01,   $00

  entity_anim_meta_sprites_f5:
    .byte $00, $00, $00, $07,   $04
  entity_anim_delay_count_f5:
    .byte $ff, $ff, $00, $13,   $04
  entity_anim_sprite_count_f5:
    .byte $06, $08, $00, $06,   $04
  entity_anim_palette_f5:
    .byte $00, $00, $00, $00,   $04

  entity_anim_meta_sprites_f6:
    .byte $00, $00, $00, $04,   $04
  entity_anim_delay_count_f6:
    .byte $ff, $ff, $00, $04,   $04
  entity_anim_sprite_count_f6:
    .byte $06, $08, $00, $04,   $04
  entity_anim_palette_f6:
    .byte $00, $00, $00, $04,   $04

  entity_anim_meta_sprites_f7:
    .byte $00, $00, $00, $04,   $04
  entity_anim_delay_count_f7:
    .byte $ff, $ff, $00, $04,   $04
  entity_anim_sprite_count_f7:
    .byte $06, $08, $00, $04,   $04
  entity_anim_palette_f7:
    .byte $00, $00, $00, $04,   $04

entity_metasprite_index_addr_lo:
  .byte <entity_meta_sprites_00, <entity_meta_sprites_01, <entity_meta_sprites_02, <entity_meta_sprites_03
  .byte <entity_meta_sprites_04, <entity_meta_sprites_05, <entity_meta_sprites_06, <entity_meta_sprites_07
  .byte <entity_meta_sprites_08, <entity_meta_sprites_09
entity_metasprite_index_addr_hi:
  .byte >entity_meta_sprites_00, >entity_meta_sprites_01, >entity_meta_sprites_02, >entity_meta_sprites_03
  .byte >entity_meta_sprites_04, >entity_meta_sprites_05, >entity_meta_sprites_06, >entity_meta_sprites_07
  .byte >entity_meta_sprites_08, >entity_meta_sprites_09

entity_metasprite_x_offset_addr_lo:
  .byte <entity_meta_spites_x_offset_00, <entity_meta_spites_x_offset_01, <entity_meta_spites_x_offset_02, <entity_meta_spites_x_offset_03
  .byte <entity_meta_spites_x_offset_04, <entity_meta_spites_x_offset_05, <entity_meta_spites_x_offset_06, <entity_meta_spites_x_offset_07
  .byte <entity_meta_spites_x_offset_08, <entity_meta_spites_x_offset_09
entity_metasprite_x_offset_addr_hi:
  .byte >entity_meta_spites_x_offset_00, >entity_meta_spites_x_offset_01, >entity_meta_spites_x_offset_02, >entity_meta_spites_x_offset_03 
  .byte >entity_meta_spites_x_offset_04, >entity_meta_spites_x_offset_05, >entity_meta_spites_x_offset_06, >entity_meta_spites_x_offset_07 
  .byte >entity_meta_spites_x_offset_08, >entity_meta_spites_x_offset_09

entity_metasprite_y_offset_addr_lo:
  .byte <entity_meta_spites_y_offset_00, <entity_meta_spites_y_offset_01, <entity_meta_spites_y_offset_02, <entity_meta_spites_y_offset_03 
  .byte <entity_meta_spites_y_offset_04, <entity_meta_spites_y_offset_05, <entity_meta_spites_y_offset_06, <entity_meta_spites_y_offset_07 
  .byte <entity_meta_spites_y_offset_08, <entity_meta_spites_y_offset_09
entity_metasprite_y_offset_addr_hi:
  .byte >entity_meta_spites_y_offset_00, >entity_meta_spites_y_offset_01, >entity_meta_spites_y_offset_02, >entity_meta_spites_y_offset_03 
  .byte >entity_meta_spites_y_offset_04, >entity_meta_spites_y_offset_05, >entity_meta_spites_y_offset_06, >entity_meta_spites_y_offset_07
  .byte >entity_meta_spites_y_offset_08, >entity_meta_spites_y_offset_09

;entity_frame_data
  ;$00 - rock
  ;$01 - mini_angel fly f0 (wings_up)
  ;$02 - mini_angel fly f1 (wings_down)

entity_meta_sprites_index:
  entity_meta_sprites_00:
    .byte $67, $65, $85, $02, $00,   $04, $04, $00,   $00,   $0c  
  entity_meta_spites_x_offset_00:
    .byte $88, $88, $88, $84, $84,   $84, $84, $84,   $84,   $84
  entity_meta_spites_y_offset_00:
    .byte $00, $82, $04, $00, $00,   $00, $00, $00,   $00,   $00

  entity_meta_sprites_01:
    .byte $68, $66, $86, $03, $01,   $05, $05, $01,   $01,   $0d  
  entity_meta_spites_x_offset_01:
    .byte $00, $00, $00, $04, $04,   $04, $04, $04,   $04,   $04
  entity_meta_spites_y_offset_01:
    .byte $00, $82, $04, $00, $00,   $00, $00, $00,   $00,   $00

  entity_meta_sprites_02:
    .byte $77, $75, $95, $12, $10,   $16, $14, $07,   $18,   $1c 
  entity_meta_spites_x_offset_02:
    .byte $88, $88, $88, $84, $84,   $84, $84, $84,   $84,   $84
  entity_meta_spites_y_offset_02:
    .byte $08, $06, $0c, $08, $08,   $08, $08, $08,   $08,   $08

  entity_meta_sprites_03:
    .byte $78, $76, $96, $13, $11,   $17, $15, $08,   $19,   $1d  
  entity_meta_spites_x_offset_03:
    .byte $00, $00, $00, $04, $04,   $04, $04, $04,   $04,   $04
  entity_meta_spites_y_offset_03:
    .byte $08, $06, $0c, $08, $08,   $08, $08, $08,   $08,   $08



  entity_meta_sprites_04:
    .byte $87, $20, $24, $22, $20,   $26, $26, $26,   $28,   $2c  
  entity_meta_spites_x_offset_04:
    .byte $88, $88, $84, $84, $84,   $84, $84, $84,   $84,   $84
  entity_meta_spites_y_offset_04:
    .byte $10, $10, $10, $10, $10,   $10, $10, $10,   $10,   $10

  entity_meta_sprites_05:
    .byte $88, $21, $25, $23, $21,   $27, $27, $27,   $29,   $2d  
  entity_meta_spites_x_offset_05:
    .byte $00, $00, $04, $04, $04,   $04, $04, $04,   $04,   $04
  entity_meta_spites_y_offset_05:
    .byte $10, $10, $10, $10, $10,   $10, $10, $10,   $10,   $10

  ;sw 1                               
  entity_meta_sprites_06:
    .byte $2a, $ff, $2b, $2a,   $1b, $2b, $0a,   $ff,   $00  
  entity_meta_spites_x_offset_06:
    .byte $89, $00, $83, $89,   $0a, $88, $8b,   $ff,   $00    
  entity_meta_spites_y_offset_06: 
    .byte $05, $0b, $05, $05,   $00, $02, $0a,   $ff,   $00
  
  ;sw 2                              
  entity_meta_sprites_07:
    .byte $1a, $ff, $ff, $1a,   $0b, $ff, $09,   $ff,   $00  
  entity_meta_spites_x_offset_07:
    .byte $89, $00, $00, $89,   $0a, $87, $93,   $ff,   $00   
  entity_meta_spites_y_offset_07:
    .byte $83, $00, $00, $82,   $88, $03, $0a,   $ff,   $00



  entity_meta_sprites_08:
    .byte $00, $00, $00, $00
  entity_meta_spites_x_offset_08:
    .byte $00
  entity_meta_spites_y_offset_08:
    .byte $00

  entity_meta_sprites_09:
    .byte $00, $00, $00, $00
  entity_meta_spites_x_offset_09:
    .byte $00
  entity_meta_spites_y_offset_09:
    .byte $00

