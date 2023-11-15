.include "nes.inc"
.include "global.inc"
.include "entity.inc"

.segment "ZEROPAGE"
  random_seed: .res 1

.segment "CODE"

.proc generate_random_seed
  lda random_seed
  beq @doEor
  asl
  beq @noEor ;if the input was $80, skip the EOR
  bcc @noEor
@doEor:    
  eor #$1d
@noEor:  
  sta random_seed
.endproc