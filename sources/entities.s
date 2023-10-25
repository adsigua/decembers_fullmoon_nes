.include "nes.inc"
.include "global.inc"
.include "entity.inc"

;.segment "ZEROPAGE"
.segment "CODE"

.proc clear_entities
  lda #<entity_addr
  sta tempX
  lda #>entity_addr
  sta tempX+1

  ldy #<entity_addr_end
  lda #0
  @clear_entity_loop:
    sta (tempX), y
    dey
    bne @clear_entity_loop
  rts 
.endproc

.proc init_entities
  jsr clear_entities
  jsr init_player_entity
  rts
.endproc

;curr_entity_data = entity data from rom
.proc init_entity

    jmp @end_init

  @init_static_animated:

    jmp @end_init
  @end_init:
    rts
.endproc

.proc update_entities
  ldx #0
  stx curr_entity_index
  @update_entity_loop:
    lda entity_type, x
    and #$0f
    beq @no_update

    lda entity_type, x                ;check entity behaviour type
    and #$f0
    cmp #EntityType::player_type
    ;cmp #$01
    beq @update_player_entity
    jmp @no_update

    @update_player_entity:
      jsr update_player_entity
      jmp @no_update
      
    @no_update:
    dey
    inc curr_entity_index
    ldx curr_entity_index
    cpx #$10
    bne @update_entity_loop
  rts
.endproc

.proc apply_entity_velocity
  ldx curr_entity_index
  lda entity_velocity_x, x
  beq @check_y_velocity
  
  bpl @entity_positive_x_speed
    ; if velocity is negative, subtract 1 from high byte to sign extend
    dec entity_pos_x, x
  @entity_positive_x_speed:
    clc
    adc entity_pos_x_lo, x
    sta entity_pos_x_lo, x
    lda #0                    ; add high byte
    adc entity_pos_x, x
    sta entity_pos_x, x
    ;jsr bound_position_x

  @check_y_velocity:
  lda entity_velocity_y, x
  beq @end_velocity_apply

  bpl @entity_positive_y_speed
    dec entity_pos_y, x
  @entity_positive_y_speed:
  clc
  adc entity_pos_y_lo, x
  sta entity_pos_y_lo, x
  lda #0        
  adc entity_pos_y, x
  sta entity_pos_y, x
  ;jsr bound_position_y

  @end_velocity_apply:

  rts
.endproc

;trash =========================================================
    ;get tiles addr
  ; ldy #AnimFrameData::frame_sprites_addr
  ; lda (frame_data_addr), y
  ; sta curr_sprite_tile_data
  ; iny
  ; lda (frame_data_addr), y
  ; sta curr_sprite_tile_data+1

  ; ;get offsets addrs
  ; ldy #EntityData::spr_x_offsets
  ; lda (curr_entity_data), y
  ; sta offsetX_addr
  ; iny
  ; lda (curr_entity_data), y 
  ; sta offsetX_addr+1

  ; ldy #EntityData::spr_y_offsets
  ; lda (curr_entity_data), y
  ; sta offsetY_addr
  ; iny
  ; lda (curr_entity_data), y 
  ; sta offsetY_addr+1

  ; ;get number of tiles
  ; ldy #EntityData::tile_count
  ; lda (curr_entity_data), y
  ; tax
  ; ldy #0
  ; sty sprite_draw_index

  ; @draw_entity_loop:
  ;   ldy #Entity::screen_pos_y
  ;   lda (curr_entity_state), y    ;y-pos
  ;   sta tempX

  ;   ldy sprite_draw_index
  ;   lda (offsetY_addr), y         ;sprite y-offset
  ;   clc                  
  ;   adc tempX
  ;   sta oam_buffer

  ;   ldy sprite_draw_index
  ;   lda (curr_sprite_tile_data), y        ;sprite/cell index on chr
  ;   sta oam_buffer+1

  ;   ldy #AnimFrameData::frame_palette
  ;   lda (frame_data_addr), y
  ;   sta tempX

  ;   ;ldy #Entity::entity_state
  ;   ;lda (curr_entity_state), y
  ;   lda curr_entity_facing
  ;   and #%01000000
  ;   ora tempX
  ;   sta oam_buffer+2             

  ;   ldy sprite_draw_index
  ;   lda (offsetX_addr), y
  ;   sta tempX
  ;   ldy #Entity::screen_pos_x
  ;   lda (curr_entity_state), y    ;x-pos
    
  ;   bit curr_entity_facing
  ;   bvs @is_flipped
  ;   clc
  ;   adc tempX
  ;   sta oam_buffer+3
  ;   jmp @place_buffer_to_oam
  ; @is_flipped:
  ;   sec
  ;   sbc tempX   
  ;   clc
  ;   adc #$08
  ;   sta oam_buffer+3

  ; @place_buffer_to_oam:
  ;   jsr update_oam
  ;   inc sprite_draw_index
  ;   dex
  ;   bne @draw_entity_loop
  ;entities_list_hi = $0300
  ;entities_list_lo = $0310
  ;entities_list = $0300     ;entities data memory from 0620 to 06ff
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

  ; entity_addr_end = $03af

  ; curr_entity_index = $00

  ; curr_entity_type = $01
  ; curr_entity_state = $02       
  ; curr_entity_id = $01
  ; curr_entity_anim_num = $03
  ; curr_entity_anim_delay_cnt = $04
  ; curr_entity_pos_x = $05
  ; curr_entity_pos_y = $06
  ; curr_entity_velocity_x = $07
  ; curr_entity_velocity_y = $08
  ; curr_entity_pos_x_lo = $09
  ; curr_entity_pos_y_lo = $0a

  ; curr_entity_data:   .res 2          ;ram address for rom entity data
  ; curr_entity_anim_data:   .res 2
  ; curr_sprite_tile_data:   .res 2
  ; entity_pos:   .res 2 
  ; sprite_draw_index: .res 1

  ; init_player_xpos = $78
  ; init_player_ypos = $BE

  ; entityState = $00
  ; animIndex = $01
  ; frame_count = $02
  ; frame_num = $03
  ; delay_count = $04
  ; frame_data_addr = $05 ;06
  ; sprite_mem_addr = $07 ;08
  ; offsetX_addr = $09 ;0a
  ; offsetY_addr = $0b ;0c
  ;load type from entity_data
  ; ldy #0
  ; lda (curr_entity_data), y
  ; cmp #0           ;check if non-entity
  ; beq @end_init
  ; iny

  ; lda (curr_entity_data), y
  ; cmp #EntityBehaviourType::moving_animated
  ; beq @init_moving_animated
  ; cmp #EntityBehaviourType::static_animated
  ; beq @init_static_animated

  ; @init_static:
  ;   jmp @end_init

  ; @init_moving_animated:
    
  ;   ldy #0
  ;   lda (curr_entity_data), y
  ;   sta (curr_entity_state), y
  ;   iny
  ;   lda (curr_entity_data), y
  ;   sta (curr_entity_state), y
  ;   iny
  ;   lda (curr_entity_data), y     ;sc pos x
  ;   sta (curr_entity_state), y
  ;   sta tempX
  ;   iny
  ;   lda (curr_entity_data), y     ;sc pos y
  ;   sta (curr_entity_state), y
  ;   sta tempY
  ;   iny
  ;   lda #0
  ;   sta (curr_entity_state), y
  ;   iny

  ;   ;store anim data address
  ;   lda curr_entity_data
  ;   sta (curr_entity_state), y
  ;   iny 
  ;   lda curr_entity_data+1
  ;   sta (curr_entity_state), y
  ;   iny

  ;   ;anim id
  ;   lda #0
  ;   sta (curr_entity_state), y
  ;   iny
  ;   sta (curr_entity_state), y
  ;   iny
  ;   sta (curr_entity_state), y
  ;   iny
  ;   sta (curr_entity_state), y
  ;   iny
  ;   sta (curr_entity_state), y
  ;   iny
  ;   ;x lo
  ;   lda #0
  ;   sta (curr_entity_state), y
  ;   iny
  ;   lda tempX
  ;   sta (curr_entity_state), y
  ;   iny
  ;   ;y lo
  ;   lda #0
  ;   sta (curr_entity_state), y
  ;   iny
  ;   lda tempY
  ;   sta (curr_entity_state), y
  ;   iny