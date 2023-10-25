;
; NES I/O definitions
; Copyright 2010 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;

.ifndef NES_INC
.define NES_INC 1

PPUCTRL = $2000
NT_2000 = $00
NT_2400 = $01
NT_2800 = $02
NT_2C00 = $03
VRAM_DOWN = $04
OBJ_0000 = $00
OBJ_1000 = $08
OBJ_8X16 = $20
BG_0000 = $00
BG_1000 = $10
VBLANK_NMI = $80

PPUMASK = $2001
LIGHTGRAY = $01
BG_OFF = $00
BG_CLIP = $08
BG_ON = $0A
OBJ_OFF = $00
OBJ_CLIP = $10
OBJ_ON = $14
TINT_R = $20
TINT_G = $40
TINT_B = $80

OAM_RAM = $0200

PPUSTATUS = $2002
OAMADDR = $2003
; Don't worry about $2004; let OAM_DMA do the work for you.
PPUSCROLL = $2005
PPUADDR = $2006
PPUDATA = $2007

OAM_DMA = $4014
SNDCHN = $4015
JOY1 = $4016
JOY2 = $4017

KEY_A      = %10000000
KEY_B      = %01000000
KEY_SELECT = %00100000
KEY_START  = %00010000
KEY_UP     = %00001000
KEY_DOWN   = %00000100
KEY_LEFT   = %00000010
KEY_RIGHT  = %00000001

.scope GameState
    MainMenu = $00
    CharSelect = $01
    LevelPlay = $02
    LevelWin = $03
    LevelLose = $04
.endscope

.scope SpriteAttrib
    Palette0 = $00
    Palette1 = $01
    Palette2 = $02
    Palette3 = $03

    BGPriority = $20
    FlipX    = $40
    FlipY    = $80
.endscope

.scope BGFlags
    nametable_buffered = $80
    attribtable_buffered = $40
.endscope

.scope LevelFlags
    CanScroll = $80
    LastColumnLoaded = $40
.endscope


.endif