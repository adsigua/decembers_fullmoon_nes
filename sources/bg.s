.include "nes.inc"
.include "global.inc"
.include "entity.inc"

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
  map_tile_data_col_hi_offset = $16
  map_tile_data_col_len = $0c

  nametable1_addr = $20C0
  attrtable1_addr = $23C8
  nametable2_addr = $24C0
  attrtable2_addr = $27C8

  ;palette buffer
    tile_index = $00
    palette_buffer_index = $02
    current_palette_val = $04
    offset_pointer = $05 ;$06


.segment "CODE"

.enum TileType
  WALKABLE = 0
  SOLID = 1
.endenum

.export draw_bg


.proc draw_bg
  jsr load_main_palette
  jsr init_pointers

  jsr draw_ui

  lda ppu_ctrl_val
  ora #VRAM_DOWN
  sta PPUCTRL
  sta ppu_ctrl_val

  .repeat 19
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

.proc load_main_palette
  ; seek to the start of palette memory ($3F00-$3F1F)
  ldx #$3F
  stx PPUADDR
  ldx #$00
  stx PPUADDR
  @load_bg_palette:
    lda caves_palette, x
    sta PPUDATA
    inx
    cpx #$10
    bcc @load_bg_palette
  ldx #$00
  @load_sprite_palette:
    lda sprites_palette, x
    sta PPUDATA
    inx
    cpx #$10
    bcc @load_sprite_palette
    
    rts
.endproc

.proc init_pointers
  lda #0
  sta background_flags

  lda #<level_cave_tile_rle
  sta rom_rle_pointer
  lda #>level_cave_tile_rle
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
  lda ppu_ctrl_val
  ora #VRAM_DOWN
  sta PPUCTRL
  sta ppu_ctrl_val

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
      and #$3f                        ;remove first two bits to get actual length
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
    tax
    lda meta_tile_index, x
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
    adc #$16      ;increment by 1 column cycle -2
    tay

    lda meta_tiles_10, x
    sta col_ppu_buffer, y
    iny
    lda meta_tiles_11, x
    sta col_ppu_buffer, y
    ;iny

    ;check for pallete
    lda map_pointer
    and #$04
    beq @no_palette_check
    lda tile_index
    cmp #$0b
    beq @do_pallete_buffer
    and #$01
    bne @no_palette_check
    @do_pallete_buffer:
      jsr buffer_palette

  @no_palette_check:
    inc tile_index
    ldy tile_index
    cpy #$0c
    bne @buffer_loop

  @increment_map_pointer:
    lda map_pointer
    clc
    adc #map_tile_data_col_len
    sta map_pointer
    lda map_pointer+1
    adc #00
    sta map_pointer+1
    cmp #$05
    bne @end_column_buffer_end

    lda map_pointer
    cmp #$80  ;if hi byte goes over $05
    bcc :+
      lda #<map_tile_data   ;if over $05 wrap around to first column of tile data
      sta map_pointer
      lda #>map_tile_data
      sta map_pointer+1
    :
  @end_column_buffer_end:
    lda background_flags
    ora #BGFlags::nametable_buffered
    sta background_flags

    ldy #0
    lda (rom_rle_pointer), y
    cmp #$ff
    bne :+
      lda level_flags
      ora #LevelFlags::LastColumnLoaded
      sta level_flags
    :
    rts
.endproc

;compute attribute table buffer for one column based on meta-tile data 
.proc buffer_palette
  ldy #$00
  lda #$00
  sta current_palette_val
  lda tile_index
  bne @not_first_tile
  @buffer_first_tile:
    lda map_pointer
    sec
    sbc #$0c
    sta offset_pointer
    lda map_pointer+1
    sta offset_pointer+1

    lda (offset_pointer), y
    jsr buffer_bot_left_palette
    lda (map_pointer), y
    jsr buffer_bot_right_palette
    
    jmp @store_value_to_buffer

  @not_first_tile:

  ldx tile_index
  cpx #$0b
  bne @not_last_tile
  @buffer_last_tile:
    lda map_pointer
    clc
    adc tile_index
    sec
    sbc #$0c
    sta offset_pointer
    lda map_pointer+1
    sta offset_pointer+1

    ;store top left
    lda (offset_pointer), y
    tax
    lda meta_tile_pallete_data, x
    sta current_palette_val

    ;store top right
    ldy #$0c
    lda (offset_pointer), y
    jsr buffer_top_right_palette
    
    jmp @store_value_to_buffer

  @not_last_tile:
    lda map_pointer
    clc 
    adc tile_index
    sec
    sbc #$0d
    sta offset_pointer
    lda map_pointer+1
    sta offset_pointer+1

    @top_left:
      lda (offset_pointer), y
      tax
      lda meta_tile_pallete_data, x
      sta current_palette_val

    @bottom_left:
      iny
      lda (offset_pointer), y
      jsr buffer_bot_left_palette

    @top_right:
      lda map_pointer
      sta offset_pointer
      ldy tile_index
      dey
      lda (offset_pointer), y
      jsr buffer_top_right_palette

    @bottom_right:
      ;bottom_right
      iny
      lda (offset_pointer), y
      jsr buffer_bot_right_palette

  @store_value_to_buffer:
  ;store computed attribute value to buffer, then increment buffer index
  ldx palette_buffer_index
  sta col_palette_buffer, x
  inc palette_buffer_index

  ;set palette buffered flag (bit 6)
  lda background_flags
  ora #%01000000
  sta background_flags

  @end_palette_buffer:
  rts
.endproc

.proc buffer_top_right_palette
  tax
  lda meta_tile_pallete_data, x
  clc
  asl
  asl
  ora current_palette_val
  sta current_palette_val
  rts
.endproc

.proc buffer_bot_left_palette
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
  rts
.endproc

.proc buffer_bot_right_palette
  tax
  lda meta_tile_pallete_data, x
  clc
  ror
  ror
  ror
  ora current_palette_val
  sta current_palette_val
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
    cpx #$18
    bne @draw_ppu_loop

    ldx #0
    lda PPUSTATUS
    inc nametable_pointer
    lda nametable_pointer+1
    sta PPUADDR
    lda nametable_pointer
    sta PPUADDR
    cpy #$30
    bne @draw_ppu_loop

  @move_nametable_pointer:
    lda nametable_pointer
    cmp #$e0
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
  ;clear palette buffer flag
  attrib_offset = $00
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
    bit PPUSTATUS
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
    
    cpy #$03
    bne @draw_ppu_loop

    lda col_palette_buffer, y
    sta PPUDATA

  ;increment attrb pointer, check if need to switch
  @move_attr_pointer:
    inc attrtable_pointer
    lda attrtable_pointer
    cmp #$d0
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

;in x = screen xpos
;in y = screen ypos
;out a = tile type
.proc get_tile_type
  scroll_xoffset = $01
  map_data_addr = $02 ;$03
  pos_x = $04
  pos_y = $05

  stx pos_x
  sty pos_y

  @load_col_offsets:
    lda scroll_x
    sta scroll_xoffset
    txa                   ;move pos_x to accu
    clc
    adc scroll_xoffset    ;add pos x and scroll x
    lsr
    lsr
    lsr
    lsr
    cmp #$20
    bcc :+                ;if offset does go past nt 2
      sec
      sbc #$20
    :
    tax
    lda ppu_ctrl_val
    and #$01
    beq :+
      txa
      clc
      adc #$10
      tax
    :
    lda map_tile_data_col_lo, x
    sta map_data_addr

    ldy #$04
    cpx #map_tile_data_col_hi_offset
    bcc :+
      iny
    :
    sty map_data_addr+1
    
    ldy pos_y
    tya
    lsr
    lsr
    lsr
    lsr
    tay

    dey
    dey
    dey
    lda (map_data_addr), y
    tax
    lda meta_tile_collision_data, x
    sta tempX
      
    lda pos_x
    and #$04      ;check if x is at 0 or 1 column
    lsr
    lsr
    sta tempY
    lda pos_y
    and #$04
    lsr
    ora tempY
    tax
    lda tempX
    and tile_collision_mask, x

  rts
.endproc


.proc draw_ui
  lda ppu_ctrl_val
  and #<~VRAM_DOWN
  sta PPUCTRL
  sta ppu_ctrl_val

  bit PPUSTATUS
  lda #$20
  sta PPUADDR
  lda #$00
  sta PPUADDR

  ldx #$c0
  lda #$02
  @ui_tile_draw_loop:
    sta PPUDATA
    dex
    bne @ui_tile_draw_loop

  bit PPUSTATUS
  lda #$23
  sta PPUADDR
  lda #$c0
  sta PPUADDR

  ldx #$10
  lda #$00
  @ui_palette_draw_loop:
    sta PPUDATA
    dex
    bne @ui_palette_draw_loop


  bit PPUSTATUS
  lda #$20
  sta PPUADDR
  lda #$42
  sta PPUADDR

  lda #$e1
  sta PPUDATA
  lda #$e4
  sta PPUDATA
  lda #$ef
  sta PPUDATA

  bit PPUSTATUS
  lda #$24
  sta PPUADDR
  lda #$00
  sta PPUADDR

  ldx #$c0
  lda #$02
  @ui_tile_draw_loop2:
    sta PPUDATA
    dex
    bne @ui_tile_draw_loop2

  bit PPUSTATUS
  lda #$24
  sta PPUADDR
  lda #$c0
  sta PPUADDR

  ldx #$10
  lda #$aa
  @ui_palette_draw_loop2:
    sta PPUDATA
    dex
    bne @ui_palette_draw_loop2
  
  rts
.endproc


.segment "RODATA"
  map_tile_data_col_lo:                 ;||
    .byte $00, $0c, $18, $24, $30, $3c, $48, $54,   $60, $6c, $78, $84, $90, $9c, $a8, $b4
    .byte $c0, $cc, $d8, $e4, $f0, $fc, $08, $14,   $20, $2c, $38, $44, $50, $5c, $68, $74

  tile_collision_mask:
    .byte $c0, $30, $0c, $03

  .export level_player_spawn_x, level_player_spawn_y
  level_player_spawn_x:
    .byte $20
  level_player_spawn_y:
    .byte $c8    

  level_rle_addr_lo:
    .byte <level_cave_tile_rle
  level_rle_addr_hi:
    .byte >level_cave_tile_rle

  sprites_palette:
    .byte $0F,$07,$1B,$37,  $0F,$06,$16,$26,  $0F,$0A,$1A,$2A,  $0F,$02,$12,$22

  level_palettes:
  caves_palette:
    .byte $0F,$0F,$19,$20,  $0f,$00,$10,$30,  $0f,$0c,$1c,$3c,  $0f,$01,$21,$31
  beach_night_palletes:
    .byte $0F,$08,$19,$2A,  $0F,$0c,$1c,$2c,  $0F,$01,$1c,$31,  $0F,$02,$12,$3c
  
  ui_tile_data:
  ui_meta_tiles:
    .byte 03
  ui_meta_tile_collisions:
    .byte 00
  ui_meta_tile_palette:
    .byte 02

  ;tile compression/meta-tiles
  ;byte 0-3 sprite/cell index, 
  ;00 10
  ;01 11
  ;RLE compression of tiles 
  ; dest len data
  ; -0-- ssss copy 1 byte ssss times
  ; -1-- ssss literal ssss bytes
  ; 1--- ---- last command for column

  ;tile data ============================
    ;beach tiles
      ;$01  =   full 3
      ;$02  =   full 2
      ;$03  =   full 1
      ;$04  =   full blank 1 star
      ;$05  =   sea horizon
      ;$06  =   sea foam pulled
      ;$07  =   sea foam full

      ;$08  =   full 3 dithered 2
      ;$09  =   floor edge rock
      ;$0a  =   left hole
      ;$0b  =   middle hole
      ;$0c  =   right hole
      ;$0d  =   cloud left
      ;$0e  =   cloud right
      ;$0f  =   moon
    ;cave tiles
      ;$10  =   floor right
      ;$11  =   floor left
      ;$12  =   up edge floor left
      ;$13  =   up edge floor right
      ;$14  =   right edge floor slant
      ;$15  =   left edge floor slant
      ;$16  =   upper head rock
      ;$17  =   mid head rock
      
      ;$18  =   bottom head rock
      ;$19  =   up left ground rock
      ;$1a  =   up right ground rock
      ;$1b  =   down left ground rock
      ;$1c  =   down right ground rock
      ;$1d  =   edge front floor dark
      ;$1e  =   edge front floor light
      ;$1f  =   floor left

    ;$fe = blank

  meta_tiles:
  meta_tiles_00:
    .byte $fe, $00, $01, $02, $72, $b2, $40, $36,    $86, $60, $80, $a0, $82, $fe, $b1, $42
    .byte $08, $0a, $0c, $0e, $2a, $2e, $2c, $4c,    $6c, $fe, $fe, $58, $5a, $6e, $4e, $44
  meta_tiles_10:
    .byte $fe, $00, $01, $02, $fe, $b2, $41, $36,    $86, $61, $81, $a1, $83, $fe, $fe, $43
    .byte $09, $0b, $0d, $0f, $2b, $2f, $2d, $4d,    $6d, $7d, $fe, $59, $5b, $6f, $4f, $44
  meta_tiles_01:
    .byte $fe, $00, $01, $02, $fe, $02, $50, $65,    $96, $70, $90, $fe, $92, $c2, $00, $54
    .byte $18, $1a, $1c, $1e, $3a, $3e, $3c, $5c,    $7c, $48, $4a, $68, $6a, $7e, $5e, $52
  meta_tiles_11:
    .byte $fe, $00, $01, $02, $fe, $02, $51, $66,    $96, $71, $91, $fe, $93, $c0, $c1, $55
    .byte $19, $1b, $1d, $1f, $3b, $3f, $3d, $5d,    $fe, $49, $4b, $69, $6b, $7f, $5f, $53

  ;meta tile data
    ;$00  =   floor blank palette 0 for UI / walkable
    ;$01  =   floor blank palette 1 solid
    ;$02  =   full 3 palette 0
    ;$03  =   full 2 palette 0
    ;$04  =   full 1 palette 0
    ;$05  =   full 3 palette 1
    ;$06  =   full 2 palette 1
    ;$07  =   full 1 palette 1
    
    ;$08  =   full 3 palette 2
    ;$09  =   full 2 palette 2
    ;$0a  =   full 1 palette 2
    ;$0b  =   full 3 palette 3
    ;$0c  =   full 2 palette 3
    ;$0d  =   full 1 palette 3
    ;$0e  =   edge floor slant right
    ;$0f  =   edge floor slant left

    ;$10  =   floor right
    ;$11  =   floor left
    ;$12  =   up edge floor left
    ;$13  =   up edge floor right
    ;$14  =   right edge floor slant
    ;$15  =   left edge floor slant
    ;$16  =   upper head rock
    ;$17  =   mid head rock
    
    ;$18  =   bottom head rock
    ;$19  =   up left ground rock
    ;$1a  =   up right ground rock
    ;$1b  =   down left ground rock
    ;$1c  =   down right ground rock
    ;$1d  =   edge front floor dark
    ;$1e  =   edge front floor light
    ;$1f  =   floor left

    ;$20  =   hole left level 1 cave
    ;$21  =   hole middle level 1 cave
    ;$22  =   hole right level 1 cave
    ;$23  =   floor edge rock
    ;$24  =   floor blank palette 2 solid
    ;$25  =   floor blank palette 3 solid

  meta_tile_index:
    .byte $00, $00, $01, $02, $03, $01, $02, $03,   $01, $02, $03, $01, $02, $03, $00, $00
    .byte $10, $11, $12, $13, $14, $15, $16, $17,   $18, $19, $1a, $1b, $1c, $1d, $1e, $1f
    .byte $0a, $0b, $0c, $09, $00, $00, $09
  meta_tile_collision_data:
    .byte $00, $55, $00, $00, $00, $00, $00, $00,   $00, $00, $00, $00, $00, $00, $00, $00  
    .byte $00, $00, $00, $00, $55, $55, $00, $00,   $00, $55, $55, $55, $55, $55, $55, $02  
    .byte $55, $55, $55, $55, $55, $55, $55
  meta_tile_pallete_data: 
    .byte $00, $01, $00, $00, $00, $01, $01, $01,   $02, $02, $02, $03, $03, $03, $00, $00
    .byte $02, $02, $02, $02, $02, $02, $01, $01,   $01, $03, $03, $03, $03, $02, $02, $02  
    .byte $02, $02, $02, $02, $02, $03, $01


  ; x- =>   0x = copy x count     4x = literal place x count    bit 7 is last for column
  level_cave_tile_rle:
  ;screen 1 ============================================================================
    .byte $44, $16, $16, $17, $18,   $04, $01,    $c4, $10, $10, $15, $23
    .byte $43, $16, $17, $18,   $05, $01,    $c4, $11, $11, $10, $23
    .byte $42, $16, $17,   $03, $01,    $c7, $19, $1b, $15, $10, $10, $10, $23
    .byte $cc, $16, $17,   $01, $01,    $19, $1b, $1c, $13, $11, $11, $11, $23

    .byte $cc, $16, $17,   $01, $01,    $1a, $1c, $1b, $12, $10, $10, $10, $23
    .byte $cc, $16, $17,   $01, $01, $01,    $1a, $1c, $13, $11, $11, $11, $23
    .byte $42, $17, $18,   $05, $01,    $c5, $12, $10, $10, $10, $23
    .byte $41, $18,   $05, $01,    $c6, $19, $13, $11, $11, $11, $23
    
    .byte $41, $18,   $04, $01,    $c7, $19, $1b, $12, $10, $10, $10, $23
    .byte $41, $18,   $04, $01,    $c7, $1a, $1c, $13, $11, $11, $11, $23
    .byte $41, $18,   $06, $01,    $c5, $12, $10, $10, $10, $23
    .byte $41, $18,   $06, $01,    $c5, $13, $11, $11, $11, $23

    .byte $41, $18,   $05, $01,    $c6, $19, $12, $10, $10, $10, $23
    .byte $41, $18,   $04, $01,    $c7, $19, $1b, $13, $11, $11, $11, $23
    .byte $41, $18,   $04, $01,    $c7, $1a, $1c, $12, $10, $10, $10, $23
    .byte $41, $18,   $04, $01,    $c7, $1a, $1c, $13, $11, $11, $11, $23

    ;.byte $cc, $18,   $01, $01, $01,    $19, $1b, $1c, $13, $11, $11, $14, $1d
    ;.byte $41, $18,   $04, $01,    $c7, $1a, $1c, $13, $11, $1e, $1e, $1e

  ;screen 2 ============================================================================
    .byte $41, $18,   $05, $01,    $c6, $1a, $12, $10, $10, $10, $23
    .byte $41, $18,   $06, $01,    $c5, $13, $11, $11, $11, $23
    .byte $41, $18,   $06, $01,    $c5, $12, $10, $10, $10, $23
    .byte $41, $18,   $06, $01,    $c5, $13, $11, $11, $11, $23
    
    .byte $41, $18,   $06, $01,    $c5, $12, $10, $10, $10, $23
    .byte $41, $18,   $05, $01,    $c6, $19, $13, $11, $11, $11, $23
    .byte $41, $18,   $04, $01,    $c7, $19, $1b, $12, $10, $10, $10, $23
    .byte $cc, $18,   $01, $01, $01,    $19, $1b, $1c, $13, $11, $11, $11, $23

    .byte $cc, $18,   $01, $01, $01,    $1a, $1c, $1b, $12, $10, $10, $10, $23
    .byte $41, $18,   $04, $01,    $c7, $1a, $1c, $13, $11, $11, $11, $23
    .byte $41, $18,   $06, $01,    $c5, $12, $10, $10, $10, $23
    .byte $41, $18,   $06, $01,    $c5, $13, $11, $11, $11, $23
    
    .byte $41, $18,   $06, $01,    $c5, $12, $10, $10, $10, $23
    .byte $41, $18,   $06, $01,    $c5, $13, $11, $11, $11, $23
    .byte $41, $18,   $04, $01,    $c7, $19, $1b, $12, $10, $10, $10, $23
    .byte $41, $18,   $04, $01,    $c7, $1a, $1c, $13, $11, $11, $11, $23

  ;screen 3 ============================================================================
    .byte $41, $18,   $04, $01,    $c7, $1a, $1c, $12, $10, $10, $10, $23
    .byte $41, $18,   $05, $01,    $c6, $1a, $13, $11, $11, $11, $23
    .byte $41, $18,   $06, $01,    $c5, $12, $10, $10, $10, $23
    .byte $41, $18,   $05, $01,    $c6, $19, $13, $11, $11, $11, $23

    .byte $41, $18,   $04, $01,    $c7, $19, $1b, $12, $10, $10, $10, $23
    .byte $cc, $18,   $01, $01, $01,    $19, $1b, $1c, $13, $11, $11, $11, $23
    .byte $cc, $18,   $01, $01, $01,    $1a, $1c, $1b, $12, $10, $10, $10, $23
    .byte $41, $18,   $04, $01,    $c7, $1a, $1c, $13, $11, $11, $11, $23

    .byte $41, $18,   $05, $01,    $c6, $19, $12, $10, $10, $10, $23
    .byte $41, $18,   $06, $01,    $c5, $13, $11, $11, $11, $23
    .byte $41, $18,   $06, $01,    $c5, $12, $10, $10, $10, $23
    .byte $41, $18,   $06, $01,    $c5, $13, $11, $11, $11, $23
    
    .byte $41, $18,   $05, $01,    $c6, $19, $12, $10, $10, $10, $23
    .byte $41, $18,   $04, $01,    $c7, $19, $1b, $13, $11, $11, $11, $23
    .byte $41, $18,   $04, $01,    $c7, $1a, $1c, $12, $10, $10, $10, $23
    .byte $41, $18,   $04, $01,    $c7, $1a, $1c, $13, $11, $11, $11, $23

  ;screen 4 (double holes)============================================================================
    .byte $41, $18,   $05, $01,    $c6, $1a, $12, $10, $10, $10, $23
    .byte $41, $18,   $06, $01,    $c5, $13, $11, $11, $11, $23
    .byte $41, $18,   $06, $01,    $c5, $12, $10, $10, $10, $23
    .byte $41, $18,   $06, $01,    $c5, $13, $11, $11, $11, $23

    .byte $41, $18,   $06, $01,    $c5, $12, $10, $10, $10, $23
    .byte $41, $18,   $05, $01,    $c6, $19, $13, $11, $11, $11, $23
    .byte $41, $18,   $04, $01,    $c7, $19, $1b, $12, $10, $10, $10, $23
    .byte $cc, $18,   $01, $01, $01,    $19, $1b, $1c, $13, $11, $20, $11, $23
    
    .byte $cc, $18,   $01, $01, $01,    $1a, $1c, $1b, $12, $20, $21, $10, $23
    .byte $41, $18,   $04, $01,    $c7, $1a, $1c, $13, $21, $01, $11, $23
    .byte $41, $18,   $06, $01,    $c5, $12, $21, $22, $10, $23
    .byte $41, $18,   $06, $01,    $c5, $13, $22, $11, $11, $23
    
    .byte $41, $18,   $06, $01,    $c5, $12, $10, $10, $10, $23
    .byte $41, $18,   $06, $01,    $c5, $13, $11, $11, $11, $23
    .byte $41, $18,   $04, $01,    $c7, $19, $1b, $12, $10, $10, $10, $23
    .byte $41, $18,   $04, $01,    $c7, $1a, $1c, $13, $11, $11, $11, $23

;screen 5 (bridge)============================================================================
    .byte $41, $18,   $05, $01,    $c6, $1a, $12, $10, $10, $10, $23
    .byte $41, $18,   $06, $01,    $c5, $13, $11, $11, $11, $23
    .byte $41, $18,   $06, $01,    $c5, $12, $10, $10, $10, $23
    .byte $41, $18,   $06, $01,    $c5, $13, $11, $11, $11, $23

    .byte $41, $18,   $06, $01,    $c5, $12, $10, $10, $10, $23
    .byte $41, $18,   $05, $01,    $c6, $19, $13, $11, $11, $11, $23
    .byte $41, $18,   $04, $01,    $c7, $19, $1b, $12, $10, $10, $10, $23
    .byte $cc, $18,   $01, $01, $01,    $19, $1b, $1c, $13, $11, $11, $14, $1d
    
    .byte $cc, $18,   $01, $01, $01,    $1a, $1c, $1b, $12, $10, $14, $1d, $1d
    .byte $41, $18,   $04, $01,    $c7, $1a, $1c, $13, $11, $1e, $1e, $1e
    .byte $41, $18,   $06, $01,    $c5, $14, $10, $1e, $1e, $1e
    .byte $41, $18,   $06, $01,    $c5, $01, $11, $1e, $15, $23
    
    .byte $41, $18,   $06, $01,    $c5, $01, $10, $15, $10, $23
    .byte $41, $18,   $06, $01,    $c5, $01, $11, $11, $11, $23
    .byte $41, $18,   $04, $01,    $c7, $19, $1b, $15, $10, $10, $10, $23
    .byte $41, $18,   $04, $01,    $c7, $1a, $1c, $13, $11, $11, $11, $23

  ;screen 6 boss screen ============================================================================
    .byte $41, $18,   $06, $01,    $c5, $12, $10, $10, $10, $23
    .byte $41, $18,   $05, $01,    $c6, $19, $13, $11, $11, $11, $23
    .byte $41, $18,   $04, $01,    $c7, $19, $1b, $12, $10, $10, $10, $23
    .byte $41, $18,   $03, $01,    $c8, $19, $1b, $1c, $13, $11, $11, $11, $23

    .byte $41, $18,   $03, $01,    $c8, $1a, $1c, $1b, $12, $10, $10, $10, $23
    .byte $41, $18,   $04, $01,    $c7, $1a, $1c, $13, $11, $11, $11, $23
    .byte $41, $18,   $06, $01,    $c5, $12, $10, $10, $10, $23
    .byte $41, $18,   $05, $01,    $c6, $19, $13, $11, $11, $11, $23
    
    .byte $41, $18,   $04, $01,    $c7, $19, $1b, $12, $10, $10, $10, $23
    .byte $41, $18,   $04, $01,    $c7, $1a, $1c, $13, $11, $11, $11, $23
    .byte $41, $16,   $06, $01,    $c5, $12, $10, $10, $10, $23
    .byte $42, $16, $17,   $05, $01,    $c5, $13, $11, $11, $11, $23

    .byte $42, $16, $17,   $04, $01,    $c6, $19, $12, $10, $10, $10, $23
    .byte $cc, $16, $17,   $01, $01, $01,   $19, $1b, $13, $11, $11, $14, $1d
    .byte $cc, $16, $17, $18,   $01, $01,   $1a, $1c,   $12, $10, $10, $1e, $1e
    .byte $cc, $16, $16, $17, $18,    $01,    $1a, $1c,     $14, $11, $11, $1e, $1e

    .byte $ff