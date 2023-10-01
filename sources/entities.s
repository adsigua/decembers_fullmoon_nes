.include "nes.inc"
.include "global.inc"

.export init_entities
.import init_player

; Entity state
; [DF--ssss]
;  ||||||||
;  ||||++++--------> Bit 0-3: Entity State
;  |||+------------> Bit 4:   n/a
;  ||+-------------> Bit 5:   n/a
;  |+--------------> Bit 6:   is floating (above ground)
;  +---------------> Bit 7:   facing 0=left, 1=right

.segment "ZEROPAGE"
  max_entities = 10
  total_entities = .sizeof(Entity) * max_entities
  
  entities_list:  .res .sizeof(Entity) * max_entities
  entity_index:   .res 1
  entity_params:  .res 1


.scope Entity
  sprite_x:   .res 1
  sprite_y:   .res 1

  frame_num   .res 1
  frame_timer .res 1
  state:      .res 1
  type:       .res 1
.endscope

.enum EntityType
  none_type = 0
  player_type = 1
  enemy_type = 2
  attack_type = 3
.endenum

.enum EntityState
  idle = 0
  moving = 1
  attack = 2
  hurt = 3
  dead = 4
.endenum

.proc init_entities
  jsr init_player
  jsr clear_entities
  rts
.endproc

.proc clear_entities
  lda #$01
clear_entity_loop:
  tax
  lda #$00
  sta entities_list+Entity::frame_num, x
  sta entities_list+Entity::frame_timer, x
  sta entities_list+Entity::state, x
  sta entities_list+Entity::type, x        ;EntityType::none_type

  lda #$ff
  sta entities_list+Entity::sprite_x, x
  sta entities_list+Entity::sprite_y, x

  txa
  clc
  adc .sizeof(Entity)
  cmp #total_entities
  bne clear_entity_loop
  rts
.endproc



