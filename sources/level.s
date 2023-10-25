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
  level_flags:    .res 1
  scroll_x:       .res 1
  scroll_x_lo:    .res 1
  next_xscroll_update: .res 1

  scroll_num:     .res 1
  encounter_num:  .res 1

  scroll_update_increment = $10
  ;max_scroll_num_count = $10
  max_scroll_num_count = $50

.segment "CODE"

.proc init_level_manager
  lda #$00
  sta level_index
  rts
.endproc

.proc init_level
  
  rts
.endproc

;y = player height / dist to foot pos
.proc init_player_pos
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
  sta encounter_num
  lda #$10
  sta next_xscroll_update

  lda #LevelFlags::CanScroll
  sta level_flags
.endproc

.proc check_update_bg_scroll
  lda level_flags
  bpl @end_scroll_update_check

  lda scroll_x
  cmp next_xscroll_update
  bne @end_scroll_update_check

  inc scroll_num
  lda scroll_num
  cmp #max_scroll_num_count
  bne :+
    lda level_flags
    eor #LevelFlags::CanScroll
    sta level_flags
    jmp @end_scroll_update_check
  :
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

;reg a => player velocity x
.proc apply_scroll_x
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

.segment "RODATA"


