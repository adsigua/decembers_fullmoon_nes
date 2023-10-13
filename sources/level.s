.include "nes.inc"
.include "global.inc"

;handles level flow and bg scrolling
.segment "ZEROPAGE"
  ;x--- ---- clear for scroling
  ;-x-- ---- there was 
  level_flags:    .res 1
  scroll_x:       .res 1
  scroll_x_lo:    .res 1
  next_xscroll_update:  .res 1
  screen_num:     .res 1
  encounter_num:  .res 1

  scroll_update_increment = $10

.segment "CODE"

.proc init_scroll
  lda #0
  sta scroll_x
  sta scroll_x_lo
  sta screen_num
  sta encounter_num
  lda #$10
  sta next_xscroll_update

  lda #$c0
  sta level_flags
.endproc

.proc check_update_bg_scroll
  ;check scroll x overflow
  lda scroll_x
  cmp next_xscroll_update
  bne @end_scroll_update_check

  lda next_xscroll_update
  clc
  adc #scroll_update_increment
  sta next_xscroll_update

  jsr update_column

  @end_scroll_update_check:
  rts
.endproc

.proc update_scroll
  bit level_flags
  bvc @end_update_scroll

  ;lda PPUCTRL
  ;sta tempX

  lda PPUSTATUS
  lda #0

  lda level_flags
  and #$20
  beq @no_nametable_switch
    lda level_flags
    eor #%00100000
    sta level_flags

    lda PPUCTRL
    and #$01
    beq @switch_to_nt_2
    @switch_to_nt_1:
      lda PPUCTRL
      eor #$01
      sta PPUCTRL
      jmp @no_nametable_switch
    @switch_to_nt_2:
      lda PPUCTRL
      eor #$01
      sta PPUCTRL
  @no_nametable_switch:
  sta PPUADDR
  sta PPUADDR
  lda scroll_x
  sta PPUSCROLL
  lda #00
  sta PPUSCROLL

  ;lda tempX
  ;sta PPUCTRL

  lda level_flags
  eor #%01000000
  sta level_flags

  @end_update_scroll:
  rts
.endproc

.segment "RODATA"


