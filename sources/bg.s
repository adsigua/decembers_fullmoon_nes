.include "nes.inc"
.include "global.inc"

.segment "ZEROPAGE"
  rom_rle_pointer:    .res 2      ;pointer to nametable data 
  map_pointer:        .res 2
  nametable_pointer:  .res 2      
  attrtable_pointer:  .res 2       

  ;background flags - current state of bg update
  ;x--- ----    there is tile data stored in nametable buffer
  ;-x-- ----    there is palette stored in palette buffer
  background_flags:   .res 1

  ;col buffer starts at $0400
  col_ppu_buffer = $0100        ;full column should be +30, full tile col +60 $013C
  col_palette_buffer = $0140    ;pallete data for each col tile

  map_tile_data = $0400         ;meta tile data 2screen's worth (up to $05ff)

  nametable1_addr = $2000
  attrtable1_addr = $23C0
  nametable2_addr = $2400
  attrtable2_addr = $27C0

.segment "CODE"

.enum TileType
  WALKABLE = 0
  SOLID = 1
.endenum

.export draw_bg

.proc draw_bg
  lda #%10010100	; Enable NMI (specifically enable generation of an interrupt at vblank interval)
  sta PPUCTRL       ; store it on PPUCTRL

  jsr init_pointers

  .repeat 18
    jsr update_column
    jsr draw_column
    ; jsr decode_column
    ; jsr buffer_column
    ; jsr draw_column_ppu
    ; jsr draw_palette_ppu
  .endrepeat

  ;clears ppu flags
  lda PPUSTATUS

  lda PPUSTATUS
  lda #$00
  sta PPUSCROLL
  lda #$00
  sta PPUSCROLL
  rts
.endproc

.proc init_pointers
  lda #0
  sta background_flags

  lda #<beach_night
  sta rom_rle_pointer
  lda #>beach_night
  sta rom_rle_pointer+1
  
  lda #<map_tile_data
  sta map_pointer
  lda #>map_tile_data
  sta map_pointer+1

  lda #<nametable1_addr
  sta nametable_pointer
  lda #>nametable1_addr
  sta nametable_pointer+1

  lda #<attrtable1_addr
  sta attrtable_pointer
  lda #>attrtable1_addr
  sta attrtable_pointer+1
  rts
.endproc

.proc update_column
  jsr decode_column
  jsr buffer_column
  rts
.endproc

.proc draw_column
  ;check bit 7 of background flags (if there was column data buffered)
  bit background_flags
  bpl @check_palette_draw
    jsr draw_column_ppu

  ;check bit 6 of background flags (if there was palette data buffered)
  @check_palette_draw:
  bit background_flags
  bvc @end_draw_column
    jsr draw_palette_ppu

  @end_draw_column:
  lda level_flags
  ora #%01000000
  sta level_flags
  rts
.endproc

;decode rle to map_data
.proc decode_column
  rle_buffer = $00
  rle_index = $01
  tile_index = $02
  place_count = $03

  ldy #0
  sty place_count
  @decode_column_loop:
    ldx place_count
    lda (rom_rle_pointer), y
    sta rle_buffer
    bit rle_buffer              ;check rle type (copy/literal)
    bvs @place_literal          ;if bit 6 set do placing literal
    @copy_byte:
      lda (rom_rle_pointer), y        ;load len
      and #$3f                    ;remove first two bits to get actual length
      clc
      adc place_count
      sta place_count
      

      iny                         ;move pointer to tile data
      clc
      lda (rom_rle_pointer), y        ;load actual tile data
      sty rle_index
      sta tile_index
      txa
      tay
      lda tile_index
      @copy_loop:
        sta (map_pointer), y
        iny
        inx
        cpx place_count
        bne @copy_loop
        ldy rle_index
        iny
        jmp @check_end_decode

    @place_literal:
      lda (rom_rle_pointer), y        ;load len
      and #$3f                    ;remove first two bits to get actual length
      clc
      adc place_count
      sta place_count
      @literal_loop:
        iny
        clc
        lda (rom_rle_pointer), y
        sty rle_index
        sta tile_index
        txa
        tay
        lda tile_index
        sta (map_pointer), y
        ldy rle_index
        inx
        cpx place_count
        bne @literal_loop
        iny
  @check_end_decode:
    bit rle_buffer              ;check rle type (copy/literal)
    bmi @exit_rle_decode        ;if bit 7 set exit column
    jmp @decode_column_loop
  @exit_rle_decode:
    clc
    tya
    adc rom_rle_pointer
    sta rom_rle_pointer
    lda rom_rle_pointer+1
    adc #0
    sta rom_rle_pointer+1
    rts
.endproc

;convert decoded meta-tiles to sprite indeces and place them to col_ppu_buffer
.proc buffer_column
  tile_index = $00
  buffer_index = $01
  palette_buffer_index = $02
  current_palette_val = $04

  ldy #$00
  sty tile_index
  sty buffer_index
  sty palette_buffer_index
  ldx #$00
  @buffer_loop:
    ;get meta-tile index
    lda (map_pointer), y
    ;load buffer index
    ldy buffer_index
    ;get tile data by mt index
    tax
    lda meta_tiles_00, x
    sta col_ppu_buffer, y
    iny
    lda meta_tiles_01, x
    sta col_ppu_buffer, y
    iny

    ;offset tile to store right column of meta-tile
    sty buffer_index
    tya
    clc
    adc #$1c
    tay

    lda meta_tiles_10, x
    sta col_ppu_buffer, y
    iny
    lda meta_tiles_11, x
    sta col_ppu_buffer, y
    ;iny

    ;check for pallete
    lda map_pointer
    and #$10
    beq @no_palette_check
    lda tile_index
    cmp #$0e
    beq @do_pallete_buffer
    and #$01
    beq @no_palette_check
    @do_pallete_buffer:
      jsr buffer_palette

  @no_palette_check:
    inc tile_index
    ldy tile_index
    cpy #$0f
    bne @buffer_loop

  @increment_map_pointer:
    lda map_pointer
    clc
    adc #$10
    sta map_pointer
    lda map_pointer+1
    adc #$00
    cmp #$06  ;if hi byte goes over $05
    bne :+
      lda #<map_tile_data   ;if over $05 wrap around to first column of tile data
      sta map_pointer
      lda #>map_tile_data
    :
    sta map_pointer+1
  @end_column_buffer_end:

    ;set tile data buffer flag
    lda background_flags
    ora #%10000000
    sta background_flags

    rts
.endproc

;compute attribute table buffer for one column based on meta-tile data 
.proc buffer_palette
  tile_index = $00
  palette_buffer_index = $02
  current_palette_val = $04

  offset_pointer = $05 ;$06

  lda map_pointer
  clc 
  adc tile_index
  sec
  sbc #$11
  sta offset_pointer
  lda map_pointer+1
  sta offset_pointer+1

  ldy #0
  ;top left
  lda (offset_pointer), y
  tax
  lda meta_tile_pallete_data, x
  sta current_palette_val

  ;top-right
  iny
  lda (offset_pointer), y
  tax
  lda meta_tile_pallete_data, x
  clc
  asl
  asl
  ora current_palette_val
  sta current_palette_val
  
  ;bottom left
  lda map_pointer
  sta offset_pointer
  ldy tile_index
  dey
  lda (offset_pointer), y
  tax
  lda meta_tile_pallete_data, x
  clc
  asl
  asl
  asl
  asl
  ora current_palette_val
  sta current_palette_val

  ;bottom_right
  iny
  lda (offset_pointer), y
  tax
  lda meta_tile_pallete_data, x
  clc
  asl
  asl
  asl
  asl
  asl
  asl
  ora current_palette_val
  sta current_palette_val

  ;store computed attribute value to buffer, then increment buffer index
  ldx palette_buffer_index
  sta col_palette_buffer, x
  inc palette_buffer_index

  ;set palette buffered flag (bit 6)
  lda background_flags
  ora #%01000000
  sta background_flags

  rts
.endproc

.proc draw_column_ppu
  ;clear column buffer flag
  lda background_flags
  eor #%10000000
  sta background_flags

  lda PPUSTATUS
  lda nametable_pointer+1
  sta PPUADDR
  lda nametable_pointer
  sta PPUADDR

  ldy #0
  ldx #0
  @draw_ppu_loop:
    lda col_ppu_buffer, y
    sta PPUDATA
    iny
    inx
    cpx #$1e
    bne @draw_ppu_loop

    ldx #0
    lda PPUSTATUS
    inc nametable_pointer
    lda nametable_pointer+1
    sta PPUADDR
    lda nametable_pointer
    sta PPUADDR
    cpy #$3c
    bne @draw_ppu_loop

  @move_nametable_pointer:
    lda nametable_pointer
    cmp #$20
    bne @end_draw_column

  @swith_nametable:
    lda PPUSTATUS
    lda nametable_pointer+1
    cmp #$20
    beq @switch_to_nametable2
    @switch_to_nametable1:
      lda #>nametable1_addr
      sta nametable_pointer+1
      lda #<nametable1_addr
      sta nametable_pointer
      jmp @end_draw_column
    @switch_to_nametable2:
      lda #>nametable2_addr
      sta nametable_pointer+1
      lda #<nametable2_addr
      sta nametable_pointer
  @end_draw_column:
    rts
.endproc

.proc draw_palette_ppu
  attrib_offset = $00
  ;clear palette buffer flag
  lda background_flags
  eor #%01000000
  sta background_flags

  lda PPUSTATUS
  lda attrtable_pointer+1
  sta PPUADDR
  lda attrtable_pointer
  sta PPUADDR
  sta attrib_offset

  ldy #0
  ldx #0
  
  ;store palette for one column by updating ppudata per 32byte (skip 4 rows), 
  ;then just offset by 8bytes each cycle (in palette buffer row are separated by 1 byte each)
  @draw_ppu_loop:
    lda col_palette_buffer, y
    sta PPUDATA
    lda col_palette_buffer+4, y
    sta PPUDATA
    iny

    lda attrtable_pointer+1
    sta PPUADDR
    lda attrib_offset
    clc
    adc #$08
    sta PPUADDR
    sta attrib_offset
    
    cpy #$04
    bne @draw_ppu_loop

  ;increment attrb pointer, check if need to switch
  @move_attr_pointer:
    inc attrtable_pointer
    lda attrtable_pointer
    cmp #$c8
    bne @end_palette_draw

  ;attrb table switching
  @swith_attrtable:
    lda PPUSTATUS
    lda attrtable_pointer+1
    cmp #$23
    beq @switch_to_attrtable2
    @switch_to_attrtable1:
      lda #>attrtable1_addr
      sta attrtable_pointer+1
      lda #<attrtable1_addr
      sta attrtable_pointer
      jmp @end_palette_draw
    @switch_to_attrtable2:
      lda #>attrtable2_addr
      sta attrtable_pointer+1
      lda #<attrtable2_addr
      sta attrtable_pointer
  @end_palette_draw:

  rts
.endproc

;tile compression/meta-tiles
;byte 0-3 sprite/cell index, 
;00 10
;01 11
meta_tiles:
meta_tiles_00:
  .byte $fe, $00, $01, $02, $73, $b2, $65, $36, $86, $a3, $80, $a0, $82, $fe, $b1, $44
meta_tiles_10:
  .byte $fe, $00, $01, $02, $fe, $b2, $66, $36, $86, $a4, $81, $a1, $83, $fe, $fe, $45
meta_tiles_01:
  .byte $fe, $00, $01, $02, $fe, $02, $75, $65, $96, $b3, $90, $02, $92, $c2, $00, $54
meta_tiles_11:
  .byte $fe, $00, $01, $02, $fe, $02, $76, $66, $96, $b4, $91, $02, $93, $c0, $c1, $55

; byte 2 tile types
; xxyy zzww tile type 0 walkable, 1 solid
; byte 3 pallete index
; ---- --xx pallete index
beach_tile_data:
meta_tile_index:
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
meta_tile_collision_data:
  .byte $00, $00, $00, $55, $00, $00, $55, $00, $55, $00, $00, $00, $00, $00, $00, $00
meta_tile_pallete_data:
  .byte $01, $00, $01, $01, $01, $01, $01, $01, $00, $00, $00, $00, $00, $01, $01, $01

;;RLE compression of tiles 
; dest len data
; -0-- ssss copy 1 byte ssss times
; -1-- ssss literal ssss bytes
; 1--- ---- last command for column
beach_night_palletes:
  .byte $0F,$08,$19,$2A,  $11,$0c,$1c,$2c,  $11,$01,$1c,$31,   $0F,$02,$12,$22

;screen 1 ============================================================================
beach_night:
  ;.addr beach_night_col0
  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $06, $00,   $44, $04, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $05, $00,   $45, $0d, $00, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $05, $00,   $45, $0e, $00, $05, $03, $06,   $03, $01,    $c2, $08, $09

  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $05, $00,   $45, $04, $00, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09

  .byte $06, $00,   $44, $04, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $05, $00,   $45, $0d, $00, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $05, $00,   $45, $0e, $00, $05, $03, $06,   $03, $01,    $c2, $08, $09

  .byte $07, $00,   $43, $05, $03, $06,   $41, $0a,   $02, $01,   $c2, $08, $09
  .byte $05, $00,   $45, $0f, $00, $05, $03, $06,   $41, $0b,   $02, $01,   $c2, $08, $09
  .byte $06, $00,   $44, $04, $05, $03, $06,   $41, $0c,   $02, $01,   $c2, $08, $09
  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09
;screen 2 ============================================================================
  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $06, $00,   $44, $04, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $05, $00,   $45, $0d, $00, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $05, $00,   $45, $0e, $00, $05, $03, $06,   $03, $01,    $c2, $08, $09

  .byte $07, $00,   $43, $05, $03, $06,   $41, $0a,   $02, $01,   $c2, $08, $09
  .byte $05, $00,   $45, $0f, $00, $05, $03, $06,   $41, $0b,   $02, $01,   $c2, $08, $09
  .byte $06, $00,   $44, $04, $05, $03, $06,   $41, $0c,   $02, $01,   $c2, $08, $09
  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09

  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $05, $00,   $45, $04, $00, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09

  .byte $06, $00,   $44, $04, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $05, $00,   $45, $0d, $00, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $05, $00,   $45, $0e, $00, $05, $03, $06,   $03, $01,    $c2, $08, $09

;screen 3 ============================================================================
  .byte $07, $00,   $43, $05, $03, $06,   $41, $0a,   $02, $01,   $c2, $08, $09
  .byte $05, $00,   $45, $0f, $00, $05, $03, $06,   $41, $0b,   $02, $01,   $c2, $08, $09
  .byte $06, $00,   $44, $04, $05, $03, $06,   $41, $0c,   $02, $01,   $c2, $08, $09
  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09

  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $06, $00,   $44, $04, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $05, $00,   $45, $0d, $00, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $05, $00,   $45, $0e, $00, $05, $03, $06,   $03, $01,    $c2, $08, $09

  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $05, $00,   $45, $04, $00, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09

  .byte $06, $00,   $44, $04, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $07, $00,   $43, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $05, $00,   $45, $0d, $00, $05, $03, $06,   $03, $01,    $c2, $08, $09
  .byte $05, $00,   $45, $0e, $00, $05, $03, $06,   $03, $01,    $c2, $08, $09




.proc tempTrash
	; .byte $fe, $fe,   $fe, $fe		;$00 pure blank, sky
	; .byte $00, $00,   $00, $00		;$01 full 3
	; .byte $01, $01,   $01, $01		;$02 full 2
	; .byte $02, $02,   $02, $02		;$03 full 1
	; .byte $73, $fe,   $fe, $fe		;$04 full blank 1 star
	; .byte $b2, $b2,   $36, $36		;$05 sea horizon
	; .byte $65, $66,   $75, $76		;$06 sea foam pulled
	; .byte $36, $36,   $65, $66		;$07 sea foam full
	; .byte $86, $86,   $96, $96		;$08 full 3 dithered 2
	; .byte $a3, $a4,   $b3, $b4		;$09 rock
	; .byte $80, $81,   $90, $91		;$0a left hole
	; .byte $a0, $a1,   $02, $02		;$0b middle hole
	; .byte $82, $83,   $92, $93		;$0c right hole
  ; .byte $fe, $fe,   $c2, $c0    ;$0d cloud left
  ; .byte $b1, $fe,   $00, $c1    ;$0e cloud right
  ; .byte $44, $45,   $54, $55    ;$0f moon
  
  ; .byte $00, $00, $01  ;$00 pure blank, sky
	; .byte $01, $00, $00  ;$01 full 3, basic floor
	; .byte $02, $00, $01  ;$02 full 2
	; .byte $03, $55, $01  ;$03 full 1 / sea
	; .byte $04, $00, $01  ;$04 blank star
	; .byte $05, $00, $01  ;$05 sea horizon
	; .byte $06, $55, $01  ;$06 sea foam pulled
	; .byte $07, $55, $01  ;$07 sea foam full
	; .byte $08, $00, $00  ;$08 full 3 dithered 2
	; .byte $09, $55, $00  ;$09 rock
	; .byte $0a, $00, $00  ;$0a left hole
	; .byte $0b, $00, $00  ;$0b middle hole
	; .byte $0c, $00, $00  ;$0c right hole
  ; .byte $0d, $00, $01  ;$0d cloud left
  ; .byte $0e, $00, $01  ;$0e cloud right
  ; .byte $0f, $00, $01  ;$0f moon


  ; ldy #0
  ; @tile_loop_col:
  ;   lda buffer_row_index
  ;   sta PPUADDR
  ;   lda buffer_col_index
  ;   sta PPUADDR

  ;   ;get top-left
  ;   lda (map_pointer), y
  ;   and %00000011
  ;   sta pallete_buffer_index
  ;   ;top-right
  ;   clc
  ;   lda (map_pointer+$01), y
  ;   rol
  ;   rol
  ;   ora pallete_buffer_index
  ;   sta pallete_buffer_index
  ;   ;bottom left
  ;   lda (map_pointer+$10), y
  ;   clc
  ;   rol
  ;   rol  
  ;   rol
  ;   rol
  ;   ora pallete_buffer_index
  ;   sta pallete_buffer_index
  ;   ;bottom right
  ;   lda (map_pointer+$11), y
  ;   clc
  ;   ror
  ;   ror
  ;   ror
  ;   ora pallete_buffer_index

  ;   sta PPUDATA
  ;   lda buffer_col_index
  ;   clc
  ;   adc #$08
  
  ;   sta buffer_col_index
  ;   bcc :+
  ;     inc buffer_row_index
  ;   :

  ;   cmp #$e0
  ;   beq @exit_palette_placement

  ;   tya
  ;   clc
  ;   adc #$08
  ;   tay

  ;   jmp @tile_loop_col

  ; @exit_palette_placement:
  ;   ;inc buffer_row_index
  ;   lda map_pointer
  ;   clc
  ;   adc #$20
  ;   sta map_pointer
  ;   bcc :+
  ;     inc map_pointer+1
  ;   :
    
    ; .proc decode_pallete
    ;    ldy #0
    ;   @decode_column_loop:
    ;     lda (pallete_pointer), y
    ;     sta rle_buffer
    ;     bit rle_buffer              ;check rle type (copy/literal)
    ;     bvs @place_literal          ;if bit 6 set do placing literal
    ;     @copy_byte:
    ;     lda (pallete_pointer), y        ;load len
    ;     and #$3f                    ;remove first two bits to get actual length
    ;     clc
    ;     adc place_count
    ;     sta place_count

    ;     iny                         ;move pointer to tile data
    ;     clc
    ;     lda (pallete_pointer), y        ;load actual tile data
    ;     @copy_loop:
    ;       sta map_pallete_data, x
    ;       inx
    ;       cpx place_count
    ;       bne @copy_loop
    ;       iny
    ;       jmp @check_end_decode

    ;     @place_literal:
    ;     lda (pallete_pointer), y        ;load len
    ;     and #$3f                    ;remove first two bits to get actual length
    ;     clc
    ;     adc place_count
    ;     sta place_count
    ;     @literal_loop:
    ;       iny
    ;       clc
    ;       lda (pallete_pointer), y
    ;       sta map_pallete_data, x
    ;       inx
    ;       cpx place_count
    ;       bne @literal_loop
    ;       iny

    ;   @check_end_decode:
    ;     bit rle_buffer              ;check rle type (copy/literal)
    ;     bmi @exit_rle_decode        ;if bit 7 set exit column
    ;     jmp @decode_column_loop
    ;   @exit_rle_decode:
    ;     clc
    ;     tya
    ;     adc pallete_pointer
    ;     sta pallete_pointer
    ;     bcc :+
    ;       inc pallete_pointer+1
    ;     :
    ;     inx
    ;     stx place_count
    ;     rts
    ; .endproc

    ; @place_pallete:
    ;   ldy #0
    ;   lda (map_pointer), y
    ;   @pallete_loop:
    ;     bit pallete_buffer_index
    ;     bmi @load_pallete_right
    ;     bvs @load_pallete_lower_left
    ;     @load_pallete_upper_left:
            
            
            
    ;     @load_pallete_lower_left

    ;     @load_pallete_right:
    ;     bit pallete_buffer_index
    ;     bvs @load_pallete_lower_right
    ;     @load_pallete_upper_right

    ;     @load_pallete_lower_right:

  .endproc