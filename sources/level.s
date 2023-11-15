.include "nes.inc"
.include "global.inc"
.include "entity.inc"
.import level_player_spawn_x, level_player_spawn_y

;handles level flow and bg scrolling
.segment "ZEROPAGE"
  level_index:        .res 1

  ;x--- ---- can scroll
  ;-x-- ---- last column loaded
  ;--x- ---- there was x scroll overflow
  ;---x ---- curr nt for x scroll
  ;---- x--- encounter spawned (wait for encounter start)
  level_flags:          .res 1
  scroll_x:             .res 1
  scroll_x_lo:          .res 1
  scroll_x_delta:       .res 1
  next_xscroll_update:  .res 1

  scroll_num:         .res 1
  encounter_num:      .res 1
  encounter_pointer:  .res 2

  scroll_update_increment = $10
  ;max_scroll_num_count = $10
  max_scroll_num_count = $50

.segment "CODE"

.proc init_level_manager

  rts
.endproc

.proc init_level
  ldx #$00
  stx level_index
  stx encounter_num
  
  lda level_encounter_addr_lo, x
  sta encounter_pointer
  lda level_encounter_addr_hi, x
  sta encounter_pointer+1
  rts
.endproc

;y = player height / dist to foot pos
.proc init_player_level_pos
  ldx level_index
  lda level_player_spawn_x, x
  sta entity_pos_x

  lda level_player_spawn_y, x
  sec
  sbc identity_table, y
  sta entity_pos_y
  rts
.endproc

.proc init_scroll
  lda #0
  sta scroll_x
  sta scroll_x_lo
  sta scroll_num
  lda #$10
  sta next_xscroll_update

  lda #LevelFlags::CanScroll
  sta level_flags
.endproc

.proc check_update_bg_scroll
  bit level_flags
  bpl @end_scroll_update_check

  lda scroll_x
  cmp next_xscroll_update
  bne @end_scroll_update_check

  inc scroll_num

  ldy encounter_num
  lda (encounter_pointer), y
  tax
  lda level_encounter_scroll_spawn_pos, x
  sta tempX
  lda level_encounter_scroll_start_pos, x
  sta tempY

  lda level_flags
  and #ENCOUNTER_SPAWNED
  bne @wait_for_encounter_start
  lda scroll_num
  cmp tempX
  bne @no_encounter_start
    ;there is encounter
    jsr spawn_encounter_entities
    lda level_flags
    ora #ENCOUNTER_SPAWNED
    sta level_flags
    jmp @not_end_of_scroll
  @wait_for_encounter_start:
    lda scroll_num
    cmp tempY
    bne @no_encounter_start
      ;there is encounter
      jsr disable_scrolling
      inc encounter_num
      lda level_flags
      eor #ENCOUNTER_SPAWNED
      sta level_flags
      jmp @not_end_of_scroll
  @no_encounter_start:
  cmp #max_scroll_num_count
  bne @not_end_of_scroll
    jsr disable_scrolling
    jmp @end_scroll_update_check

  @not_end_of_scroll:
    lda next_xscroll_update
    clc
    adc #scroll_update_increment
    sta next_xscroll_update

    bit level_flags
    bvs @end_scroll_update_check
    jsr update_column

  @end_scroll_update_check:
  rts
.endproc

.proc disable_scrolling
  lda level_flags
  eor #LevelFlags::CanScroll
  sta level_flags
  rts
.endproc

;a = player velocity x
.proc apply_scroll_x
  ldy scroll_x
  sty tempY
  clc
  adc scroll_x_lo
  sta scroll_x_lo
  lda #0          
  adc scroll_x
  sta scroll_x
  bcc :+
    lda ppu_ctrl_val
    eor #$01
    sta ppu_ctrl_val
  :

  sec
  sbc tempY
  sta scroll_x_delta

  rts
.endproc

;y ppu ctrl val
;x scroll x
.proc update_scroll
  lda #VBLANK_NMI|BG_1000|OBJ_0000|VRAM_DOWN|NT_2000
  sta PPUCTRL
  bit PPUSTATUS
  lda #0
  sta PPUADDR
  sta PPUADDR    
  sta PPUSCROLL
  sta PPUSCROLL
  @Sprite0ClearWait:
    bit PPUSTATUS
    bvs @Sprite0ClearWait
  @Sprite0Wait:
    bit PPUSTATUS
    bvc @Sprite0Wait

  ldx #$10
  @WaitScanline:
    dex
    bne @WaitScanline

    lda ppu_ctrl_val
    sta PPUCTRL
    lda scroll_x
    sta PPUSCROLL
    lda #0
    sta PPUSCROLL

  rts
.endproc

;x = encounter 
.proc spawn_encounter_entities
  lda level_encounter_entity_count, x
  tax
  @spawn_loop:
    jsr spawn_encounter_entity
    dex
    bne @spawn_loop
  @end_spawn_encounter:
  rts
.endproc

;tempX
;tempY 
;y = encounter id
;x = entity index
.proc spawn_encounter_entity
  tempAddr = $00 ;$01
  encounterId = $02
  entityIndex = $03

  sty encounterId
  stx entityIndex
  
  dex
  lda level_encounter_entity_spawn_pos_x_addr_lo, x
  sta tempAddr
  lda level_encounter_entity_spawn_pos_x_addr_hi, x
  sta tempAddr+1
  lda (tempAddr), y
  sta tempX
  
  lda level_encounter_entity_spawn_pos_y_addr_lo, x
  sta tempAddr
  lda level_encounter_entity_spawn_pos_y_addr_hi, x
  sta tempAddr+1
  lda (tempAddr), y
  sta tempY
  
  lda level_encounter_entity_id_addr_lo, x
  sta tempAddr
  lda level_encounter_entity_id_addr_hi, x
  sta tempAddr+1
  lda (tempAddr), y
  tay

  jsr spawn_monster_entity

  ldx entityIndex
  ldy encounterId

  rts
.endproc


.segment "RODATA"
  level_encounter_addr_lo:
    .byte <level_cave_encounters, <level_mountains_encounters, <level_forest_encounters, <level_village_encounters, <level_beach_encounters
  level_encounter_addr_hi:
    .byte >level_cave_encounters, >level_mountains_encounters, >level_forest_encounters, >level_village_encounters, >level_beach_encounters


  level_cave_encounters:
    .byte $00
  level_mountains_encounters:
    .byte $00
  level_forest_encounters:
    .byte $00
  level_village_encounters:
    .byte $00
  level_beach_encounters:
    .byte $00


  level_encounter_entity_count:
    .byte $03
  level_encounter_scroll_spawn_pos:
    .byte $03
  level_encounter_scroll_start_pos:
    .byte $06


  level_encounter_entity_id_addr_lo:
    .byte <level_encounter_entity_id_00, <level_encounter_entity_id_01, <level_encounter_entity_id_02, <level_encounter_entity_id_03 
    .byte <level_encounter_entity_id_04, <level_encounter_entity_id_05, <level_encounter_entity_id_06, <level_encounter_entity_id_07 
    .byte <level_encounter_entity_id_08, <level_encounter_entity_id_09
  level_encounter_entity_id_addr_hi:
    .byte >level_encounter_entity_id_00, >level_encounter_entity_id_01, >level_encounter_entity_id_02, >level_encounter_entity_id_03 
    .byte >level_encounter_entity_id_04, >level_encounter_entity_id_05, >level_encounter_entity_id_06, >level_encounter_entity_id_07 
    .byte >level_encounter_entity_id_08, >level_encounter_entity_id_09
  
  level_encounter_entity_spawn_pos_x_addr_lo:
    .byte <level_encounter_entity_spawn_pos_x_00, <level_encounter_entity_spawn_pos_x_01, <level_encounter_entity_spawn_pos_x_02, <level_encounter_entity_spawn_pos_x_03 
    .byte <level_encounter_entity_spawn_pos_x_04, <level_encounter_entity_spawn_pos_x_05, <level_encounter_entity_spawn_pos_x_06, <level_encounter_entity_spawn_pos_x_07 
    .byte <level_encounter_entity_spawn_pos_x_08, <level_encounter_entity_spawn_pos_x_09
  level_encounter_entity_spawn_pos_x_addr_hi:
    .byte >level_encounter_entity_spawn_pos_x_00, >level_encounter_entity_spawn_pos_x_01, >level_encounter_entity_spawn_pos_x_02, >level_encounter_entity_spawn_pos_x_03 
    .byte >level_encounter_entity_spawn_pos_x_04, >level_encounter_entity_spawn_pos_x_05, >level_encounter_entity_spawn_pos_x_06, >level_encounter_entity_spawn_pos_x_07 
    .byte >level_encounter_entity_spawn_pos_x_08, >level_encounter_entity_spawn_pos_x_09

  level_encounter_entity_spawn_pos_y_addr_lo:
    .byte <level_encounter_entity_spawn_pos_y_00, <level_encounter_entity_spawn_pos_y_01, <level_encounter_entity_spawn_pos_y_02, <level_encounter_entity_spawn_pos_y_03 
    .byte <level_encounter_entity_spawn_pos_y_04, <level_encounter_entity_spawn_pos_y_05, <level_encounter_entity_spawn_pos_y_06, <level_encounter_entity_spawn_pos_y_07 
    .byte <level_encounter_entity_spawn_pos_y_08, <level_encounter_entity_spawn_pos_y_09
  level_encounter_entity_spawn_pos_y_addr_hi:
    .byte >level_encounter_entity_spawn_pos_y_00, >level_encounter_entity_spawn_pos_y_01, >level_encounter_entity_spawn_pos_y_02, >level_encounter_entity_spawn_pos_y_03 
    .byte >level_encounter_entity_spawn_pos_y_04, >level_encounter_entity_spawn_pos_y_05, >level_encounter_entity_spawn_pos_y_06, >level_encounter_entity_spawn_pos_y_07 
    .byte >level_encounter_entity_spawn_pos_y_08, >level_encounter_entity_spawn_pos_y_09
    

  level_encounter_entity_id_00:
    .byte $00, $00, $00, $00
  level_encounter_entity_spawn_pos_x_00:
    .byte $a0, $00, $00, $00
  level_encounter_entity_spawn_pos_y_00:
    .byte $a8, $00, $00, $00

  level_encounter_entity_id_01:
    .byte $00, $00, $00, $00
  level_encounter_entity_spawn_pos_x_01:
    .byte $d2, $00, $00, $00
  level_encounter_entity_spawn_pos_y_01:
    .byte $a8, $00, $00, $00

  level_encounter_entity_id_02:
    .byte $00, $00, $00, $00
  level_encounter_entity_spawn_pos_x_02:
    .byte $b0, $00, $00, $00
  level_encounter_entity_spawn_pos_y_02:
    .byte $a8, $00, $00, $00

  level_encounter_entity_id_03:
    .byte $00, $00, $00, $00
  level_encounter_entity_spawn_pos_x_03:
    .byte $00, $00, $00, $00
  level_encounter_entity_spawn_pos_y_03:
    .byte $c8, $00, $00, $00


  
  level_encounter_entity_id_04:
    .byte $01, $00, $00, $00
  level_encounter_entity_spawn_pos_x_04:
    .byte $00, $00, $00, $00
  level_encounter_entity_spawn_pos_y_04:
    .byte $c8, $00, $00, $00

  level_encounter_entity_id_05:
    .byte $01, $00, $00, $00
  level_encounter_entity_spawn_pos_x_05:
    .byte $00, $00, $00, $00
  level_encounter_entity_spawn_pos_y_05:
    .byte $c8, $00, $00, $00

  level_encounter_entity_id_06:
    .byte $01, $00, $00, $00
  level_encounter_entity_spawn_pos_x_06:
    .byte $00, $00, $00, $00
  level_encounter_entity_spawn_pos_y_06:
    .byte $c8, $00, $00, $00

  level_encounter_entity_id_07:
    .byte $01, $00, $00, $00
  level_encounter_entity_spawn_pos_x_07:
    .byte $00, $00, $00, $00
  level_encounter_entity_spawn_pos_y_07:
    .byte $c8, $00, $00, $00



  level_encounter_entity_id_08:
    .byte $01, $00, $00, $00
  level_encounter_entity_spawn_pos_x_08:
    .byte $00, $00, $00, $00
  level_encounter_entity_spawn_pos_y_08:
    .byte $c8, $00, $00, $00

  level_encounter_entity_id_09:
    .byte $01, $00, $00, $00
  level_encounter_entity_spawn_pos_x_09:
    .byte $00, $00, $00, $00
  level_encounter_entity_spawn_pos_y_09:
    .byte $c8, $00, $00, $00

  



