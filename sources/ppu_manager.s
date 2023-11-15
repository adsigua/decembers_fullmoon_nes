.include "nes.inc"
.include "global.inc"

.segment "ZEROPAGE"
  oam_base:  .res 1
  oam_counter:  .res 1
  oam_buffer:   .res 4
.segment "CODE"
  sprite_0_index = $09

.proc ppu_clear_oam
  ; First round the address down to a multiple of 4 so that it won't
  ; freeze should the address get corrupted.
  txa
  and #$fc
  tax
  lda #$ff  ; Any Y value from $EF through $FF will work
loop:
  sta OAM_RAM, x
  inx
  inx
  inx
  inx
  bne loop
  rts
.endproc

; The first entry in OAM_RAM (indices 0-3) is "sprite 0".
;x = x pos   |   y = y pos
.proc place_sprite_0
  ldx #$f8
  ldy #$28
  sty OAM_RAM
  lda #sprite_0_index
  sta OAM_RAM+1
  lda #SpriteAttrib::BGPriority
  sta OAM_RAM+2
  stx OAM_RAM+3
  rts
.endproc

.proc ppu_screen_on
  lda #VBLANK_NMI|BG_1000|OBJ_0000|VRAM_DOWN|NT_2000 
  sta PPUCTRL   
  sta ppu_ctrl_val
  lda #OBJ_ON|BG_ON
  sta PPUMASK      
  sta ppu_mask_val
  rts
.endproc

.proc update_oam_buffer
  ldy oam_counter

  lda oam_buffer
  sta OAM_RAM, y          

  lda oam_buffer+1
  sta OAM_RAM+1, y          

  lda oam_buffer+2
  sta OAM_RAM+2, y      
  
  lda oam_buffer+3
  sta OAM_RAM+3, y      

  lda oam_counter
  clc
  adc #04
  bcc :+
    lda #04
  :
  sta oam_counter
  ; inc oam_counter
  ; inc oam_counter
  ; inc oam_counter
  ; inc oam_counter
  rts
.endproc

.proc send_oam_dma
  lda #0
  sta OAMADDR
  lda #>OAM_RAM
  sta OAM_DMA   
  rts
.endproc
