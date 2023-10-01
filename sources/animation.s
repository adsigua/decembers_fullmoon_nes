.include "nes.inc"
.include "global.inc"

;holds animation data and does animation processing

.segment "RODATA"

.scope Animation
  .
.endscope

.enum AnimationState
.endenum

.proc clear_entities
  
  rts
.endproc
