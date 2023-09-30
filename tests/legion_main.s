;; //"input": "legion_main.asm",

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
  .word nmi   ;; When an NMI happens (once per frame if enabled) the label nmi:
  .word reset ;; When the processor first turns on or is reset, it will jump to the label reset:
  .word 0     ;; External interrupt IRQ (unused)

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"


.scope EntityType
  NoEntitty = 0
  Player = 1
.endscope

.struct Entity
  xpos  .byte
  ypos  .byte
  type  .byte
.endstruct

OAM = $0200

.segment "ZEROPAGE" ;ram 0 - FF\
  nmis:          .res 1
  oam_used:      .res 1  ; starts at 0
  cur_keys:      .res 2
  new_keys:      .res 2

  controller1:  .res 1
  buttonFlag:   .res 1
  scrollx:      .res 1
  scrolly:      .res 1

  max_entities = 10
  entities:     .res .sizeof(Entity) * max_entities
  total_entities = .sizeof(Entity) * max_entities
  
  hswap:        .res 1
  flicker:      .res 1
  spritemem:    .res 2
  drawcomplete: .res 1

  player_speed = 1

.segment "CHARS"
  .incbin "../obj/legion.chr"

; Main code segment for the program
.segment "CODE"


;=====================================================================================================================

reset:
  sei		; disable IRQs
  cld		; disable decimal mode
  ldx PPUCTRL
  ldx #$40  ; disable sound irq temporarily
  stx $4017	; disable APU frame IRQ (disable interrupts)
  ldx #$ff 	; Set up stack
  txs		;  transfer x ($ff) to stack pointer
  inx		; now X = 0 
  stx $2000	  ; setup PPUCTRL, disable non-maskable interrupt (NMI bit7=0) (VPHB SINN)
  stx $2001 	; disable rendering | PPU mask (BGRs bMmG)
  stx $4010 	; disable DMC_freq IRQs 

  jsr wait_vblank

clear_memory:
  lda #$00
  sta $0000, x
  sta $0100, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
    lda #$ff          ;used to referesh sprites 'physical data' from address 0200-02FF
    sta $0200, x
    lda #$00
  inx
  bne clear_memory    ;if A does not pass FF

  jsr wait_vblank


initialize_entities:    ;initialize entities
initialzie_player_entity:
  lda #$6C             ;initialize player pos
  sta entities+Entity::xpos
  sta entities+Entity::ypos
  lda #EntityType::Player
  sta entities+Entity::type

  ldx #$03    ;size of entitiy, so offset entities by player entitiy addr pos (3bytes)
  lda #$ff    ;max tile pos, i think, so it's offscreen
clear_other_entities:
  sta entities+Entity::ypos, x
  sta entities+Entity::xpos, x
  lda #$00    ;entity type 0, or no-entity type
  sta entities+Entity::type, x
  lda #$ff
  inx
  inx     ;incx 3 times to offset by sizeof(Entity), faster than add 3
  inx
  cpx #total_entities
  bne clear_other_entities

main:
;=====================================================================================================================



;=====================================================================================================================

prepare_palletes:
;set sprite range
  lda #$02      ;read high sprite addr ($02xx) store header of sprite used on clear memory to accumulator
  sta $4014     ;copy 256 bytes from $AA00-$AAFF address to the aomdata (AA is from previous line)
  nop           ;burn one cycle to wait for copy operation to finish properly

;tell ppu where to store pallete data / set ppu pallete address register
;note: any modifications to PPUADDR register will trigger toggle flag (for hi or low byte write) to reset execute 'bit $2006'
  lda #$3f      ;start at $3F00 (background pallete)
  sta $2006     ;store ppu address to PPUADDR register
  lda #$00      ;prepare $00 to be set as lower byte for the PPUADDR register
  sta $2006     ;ppu will understand this as a 4-byte address operation and set lower byte to $00

  ldx #$00      ;reset x register to use for loop next

;load palletes by storing 
load_palettes:
  lda palettes, x   ;load pallete data from explicit data provided in code, indexed by X
  sta $2007         ;store pallete data in PPUDATA based on PPUADDR, PPU will then increment PPUADDR with each access of PPUDATA
  inx               ;increment only X register, PPUADDR is automatically incremented by an amount stated in PPUCTRL bit 2 (0 accross, 1 downwards) by the PPU
  cpx #$20          ;compare X with $20 (count for maximum pallete colors, #16 for background, #16 for sprite)
  bne load_palettes 

  ;cli             ; enable interrupts
enable_rendering:
  lda #%10000000	; Enable NMI (specifically enable generation of an interrupt at vblank interval)
  sta $2000       ; store it on PPUCTRL
  lda #%00010110	; Enable sprite rendering, bit 5 on PPUMASK
  sta $2001       ; PPUMASK

gameLoop:         ;game logic loop
checkGameState:

  jmp gameLoop

;=====================================================================================================================
;game vblank nmi function (render frame)

nmi:
  pha
  php
  txa
  pha
  tya
  pha

read_controller1:
  lda #$01
  sta $4016
  sta controller1
  lsr A
  sta $4016
read_controller1_loop:
  lda $4016
  lsr A             ; bit0 -> Carry
  rol controller1   ; bit0 <- Carry
  bcc read_controller1_loop

;=====================================================================================================================
process_inputs:
check_left_input:
  lda controller1
  and #KEY_LEFT
  beq check_right_input       ;check if input is correct

  lda entities+Entity::xpos
  cmp #$10
  bcc check_right_input       ;check if player is already at bounds
  sec
  sbc #player_speed
  sta entities+Entity::xpos
  jmp check_up_input

check_right_input:
  lda controller1
  and #KEY_RIGHT
  beq check_up_input

  lda entities+Entity::xpos
  cmp #$ef
  beq do_input_right
  bcs check_up_input
do_input_right:
  clc
  adc #player_speed
  sta entities+Entity::xpos
  jmp check_up_input

check_up_input:
  lda controller1
  and #KEY_UP
  beq check_down_input

  lda entities+Entity::ypos
  cmp #$10
  bcc check_down_input
  sec
  sbc #player_speed
  sta entities+Entity::ypos
  jmp end_input_check

check_down_input:
  lda controller1
  and #KEY_DOWN
  beq end_input_check

  lda entities+Entity::ypos
  cmp #$ef
  beq do_input_down
  bcs end_input_check
do_input_down:
  clc
  adc #player_speed
  sta entities+Entity::ypos

end_input_check:

;=====================================================================================================================

;initialize spritemem
  ldx #$00
  lda #$00
  ldy #$00
  sta spritemem
  lda #$02
  sta spritemem+1

;Drawing Entities
draw_entities:
  lda entities+Entity::type, x
  cmp #EntityType::Player
  beq draw_player_sprite
  jmp check_end_sprite

;x is entity number
draw_player_sprite:
  lda entities+Entity::ypos, x    ; set 4 bytes for rendering data
  clc                             ; y pos
  adc character_sprite_data, y    ; add y offset 
  sta (spritemem), y              ; 
  
  iny
  lda character_sprite_data, y    ; tile number
  sta (spritemem), y              ; 

  iny
  lda character_sprite_data, y    ; attributes
  sta (spritemem), y              ; 

  iny
  lda entities+Entity::xpos, x 
  clc
  adc character_sprite_data, y    ; add x offset 
  sta (spritemem), y              ;

  iny
  cpy #$20
  bne draw_player_sprite

check_end_sprite:
  txa
  clc 
  adc #.sizeof(Entity)
  tax
  cpx #total_entities
  beq done_sprite
  jmp draw_entities

done_sprite:
  ;copy spritemem to ppu
  lda #$02    ; higher bit of spr ram $02xx
  sta $4014   ; render by copying spr ram to ppu data
  nop

  pla
  tay
  pla
  tax
  plp
  pla
  rti

;=====================================================================================================================


;screen is 32w x 30h tiles = 960 tiles in total
; first wait for vblank to make sure PPU is ready
wait_vblank:
  bit $2002        ;check PPUSTATUS (VSO- ----) bit 7 holds if within vblank
  bpl wait_vblank  ;branch if bit 7 if positive (not in vblank)
  rts

;=====================================================================================================================

; read_controller1:
;   lda #$01
;   sta $4016
;   sta controller1
;   lsr A
;   sta $4016
; read_controller1_loop:
;   lda $4016
;   lsr A             ; bit0 -> Carry
;   rol controller1   ; bit0 <- Carry
;   bcc read_controller1_loop
;   rts

;=====================================================================================================================



;custom data (yOffset, tile index, params, xOffset)
character_sprite_data:        ;drawing player sprites here using OAMDATA w/c takes 4bytes
  .byte $00, $00, $00, $00    ;byte 0 is Y pos in screen
  .byte $00, $01, $00, $08    ;byte 1 is OAM pallete index
  .byte $08, $10, $00, $00    ;byte 3 attributes
  .byte $08, $11, $00, $08    ;byte 4 X pos
  .byte $10, $20, $00, $00
  .byte $10, $21, $00, $08
  .byte $18, $30, $00, $00
  .byte $18, $31, $00, $08

palettes:
  ; Background Palette
  .byte $0f, $07, $17, $37
  .byte $0f, $05, $16, $27
  .byte $0f, $03, $12, $22
  .byte $0f, $0b, $00, $10

  ; Sprite Palette
  .byte $0f, $0f, $11, $2c
  .byte $0f, $0b, $00, $30
  .byte $0f, $0f, $18, $36
  .byte $0f, $15, $26, $30
