.include "nes.inc"
.include "global.inc"

.segment "HEADER"
  .byte "NES", $1A      ; iNES header identifier    ;.byte $4E, $45, $53, $1A   'NES' string then eof ascii
  .byte 2               ; 2x 16KB PRG code    (size of prg)
  .byte 1               ; 1x  8KB CHR data    (size of chr/sprites)
  .byte $00, $00        ; mapper 0, vertical mirroring  flag 6-7
  .byte $00, $00, $00 ; mapper, vs/playchoice | prgRam size (extension) | tvSystem | tvSystem, prgRam Presence
  .byte "ADO S" ; padding, 5 bytes, filled as a signature

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "STARTUP"
OAM = $0200

.segment "ZEROPAGE"
nmis:          .res 1
current_oam:   .res 1  ; starts at 0

.segment "CODE"
;;
; This NMI handler is good enough for a simple "has NMI occurred?"
; vblank-detect loop.  But sometimes there are things that you always
; want to happen every frame, even if the game logic takes far longer
; than usual.  These might include music or a scroll split.  In these
; cases, you'll need to put more logic into the NMI handler.
.proc nmi_handler
  inc nmis
  rti
.endproc

; A null IRQ handler that just does RTI is useful to add breakpoints
; that survive a recompile.  Set your debugging emulator to trap on
; reads of $FFFE, and then you can BRK $00 whenever you need to add
; a breakpoint.
;
; But sometimes you'll want a non-null IRQ handler.
; On NROM, the IRQ handler is mostly used for the DMC IRQ, which was
; designed for gapless playback of sampled sounds but can also be
; (ab)used as a crude timer for a scroll split (e.g. status bar).
.proc irq_handler
  rti
.endproc

.proc main

  ; Now the PPU has stabilized, and we're still in vblank.  Copy the
  ; palette right now because if you load a palette during forced
  ; blank (not vblank), it'll be visible as a rainbow streak.
  jsr load_main_palette

  ; While in forced blank we have full access to VRAM.
  ; Load the nametable (background map).
  ;jsr draw_bg
  
  ; Set up game variables, as if it were the start of a new level.
  jsr init_player

forever:

  ; Game logic
  jsr read_pads
  jsr update_player

  ; The first entry in OAM (indices 0-3) is "sprite 0".  In games
  ; with a scrolling playfield and a still status bar, it's used to
  ; help split the screen.  This demo doesn't use scrolling, but
  ; yours might, so I'm marking the first entry used anyway.  
  ldx #4
  stx current_oam
  ; adds to oam_used
  jsr draw_player_sprite
  ldx current_oam
  jsr ppu_clear_oam


  ; Good; we have the full screen ready.  Wait for a vertical blank
  ; and set the scroll registers to display it.
  lda nmis
vw3:
  cmp nmis
  beq vw3
  
  ; Copy the display list from main RAM to the PPU
  lda #0
  sta OAMADDR
  lda #>OAM
  sta OAM_DMA
  
  ; Turn the screen on
  ldx #0
  ldy #0
  lda #VBLANK_NMI|BG_0000|OBJ_1000
  sec
  jsr ppu_screen_on
  jmp forever

; And that's all there is to it.
.endproc

.proc load_main_palette
  ; seek to the start of palette memory ($3F00-$3F1F)
  ldx #$3F
  stx PPUADDR
  ldx #$00
  stx PPUADDR
copypalloop:
  lda initial_palette,x
  sta PPUDATA
  inx
  cpx #32
  bcc copypalloop
  rts
.endproc

.segment "RODATA"
initial_palette:
  .byt $22,$18,$28,$38,$0F,$06,$16,$26,$0F,$08,$19,$2A,$0F,$02,$12,$22
  .byt $22,$08,$16,$37,$0F,$06,$16,$26,$0F,$0A,$1A,$2A,$0F,$02,$12,$22

; Include the CHR ROM data
.segment "CHARS"
  .incbin "../obj/legion.chr"
