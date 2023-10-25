.include "nes.inc"
.include "global.inc"

.segment "HEADER"
  .byte "NES", $1A      ; iNES header identifier    ;.byte $4E, $45, $53, $1A   'NES' string then eof ascii
  .byte 2               ; 2x 16KB PRG code    (size of prg)
  .byte 1               ; 1x  8KB CHR data    (size of chr/sprites)
  .byte $01, $00        ; mapper 0, vertical mirroring  flag 6-7
  .byte $00, $00, $00 ; mapper, vs/playchoice | prgRam size (extension) | tvSystem | tvSystem, prgRam Presence
  .byte "ADO S" ; padding, 5 bytes, filled as a signature

.segment "VECTORS"
.word nmi_handler
.word reset_handler
.word irq_handler

.segment "STARTUP"

.segment "ZEROPAGE"
varSpace:       .res 16

game_state:     .res 1
nmi_count:      .res 1
ppu_ctrl_val:   .res 1
ppu_mask_val:   .res 1

.segment "CODE"

.proc irq_handler
  rti
.endproc

.proc nmi_handler
  ; pha
  ; txa
  ; pha
  ; tya
  ; pha
  ; inc nmi_count
  ; pla
  ; tay
  ; pla
  ; tax
  ; pla
  ; rti

  inc nmi_count
  rti
.endproc

.proc main
  lda #BG_1000
  sta PPUCTRL   
  sta ppu_ctrl_val 

  ; Set up game variables
  jsr init_level
  jsr draw_bg

  jsr init_entities
  jsr init_scroll

  jsr ppu_screen_on

  @game_loop:
    jsr ppu_clear_oam
    jsr read_pads
    
    ldx #$f8
    ldy #$28
    jsr place_sprite_0


    ;do updating player updates
    jsr update_player
    jsr update_entities
    jsr check_update_bg_scroll

  lda nmi_count
  @check_nmi_loop:
    cmp nmi_count
    beq @check_nmi_loop

    jsr send_oam_dma

    jsr draw_column
    jsr update_scroll

    jmp @game_loop
    rts
.endproc

.proc level_loop
  lda #BG_1000
  sta PPUCTRL   
  sta ppu_ctrl_val 

  ; Set up game variables
  jsr init_level
  jsr draw_bg

  jsr init_entities
  jsr init_scroll

  jsr ppu_clear_oam
  jsr ppu_screen_on

  @level_loop:
    jsr read_pads
    
    ; The first entry in OAM_RAM (indices 0-3) is "sprite 0".
    lda #4
    sta oam_counter

    ;do updating player updates
    jsr update_player
    jsr update_entities
    jsr check_update_bg_scroll

  lda nmi_count
  @check_nmi_loop:
    cmp nmi_count
    beq @check_nmi_loop

    jsr send_oam_dma

    jsr draw_column
    jsr update_scroll

    jmp @level_loop

  @level_win:

    rts
  @level_lose:

    rts
.endproc


.segment "RODATA"
identity_table:
    .byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f
    .byte $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f
    .byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
    .byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f
    .byte $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f
    .byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
    .byte $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f
    .byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f
    .byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
    .byte $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f
    .byte $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af
    .byte $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
    .byte $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf
    .byte $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df
    .byte $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
    .byte $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff
    
; Include the CHR ROM data
.segment "CHARS"
  .incbin "../obj/decembersfullmoon.chr"
