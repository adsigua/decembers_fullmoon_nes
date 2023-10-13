; Object animation routine
; written in 2004 by Joey Parsell (Memblers)
; released as public domain in 2006.  do whatever you can with it, a credit is nice but not required.
; thanks to Tootai for the kickass explosion animation this code was originally developed for
;
;
;how to initialize both example animations:
;        lda #2*4
;        sta explosion_follow
;
;        lda #2*4
;        sta animspr_offset
;        lda #0
;        sta anim_number
;
;        lda #0
;        jsr explode
;
;
;        lda #16*4
;        sta explosion_follow
;        sta animspr_offset
;        lda #1
;        sta anim_number
;
;        lda #1
;        jsr explode
;
;
;--------------------------------
; now that it's initialized, just do something like this once per frame in your main loop:
;
;
;        lda #1
;        sta exploding
;        jsr run_anim
;
;
;--------------------------------
;And that's all there is to it.  Good luck.


.segment "ZEROPAGE"
anim_h_flip: .res 2
explode_count = 14
explode_spr = 2
animspr_offset: .res 2
anim_delay: .res 2
exploding: .res 2
anim_timer: .res 2
anim_number: .res 2
anim_addr: .res 2
explosion_y: .res 2
explosion_x: .res 2
explosion_follow: .res 2
temp3_lo: .res 2
anim_offset: .res 2

barfer: .res 64
.segment "SAMPLES"

anim_index:
.addr squidmove
.addr littlerunner

littlerunner:
.addr run0
.byte 68
.addr run1
.byte 68
.addr run2
.byte 68
.addr run3
.byte 68
.addr run4
.byte 68
.addr run3
.byte 68
.addr run2
.byte 68
.addr run1
.byte 68

.addr anim_loop

.segment "CODE"
run0:
.byte 4
.byte $c0,$00,$00,$23
.byte $c1,$00,$08,$23
.byte $d0,$08,$00,$23
.byte $d1,$08,$08,$23
run1:
.byte 4
.byte $c2,$00,$00,$23
.byte $c3,$00,$08,$23
.byte $d2,$08,$00,$23
.byte $d3,$08,$08,$23
run2:
.byte 4
.byte $c4,$00,$00,$23
.byte $c5,$00,$08,$23
.byte $d4,$08,$00,$23
.byte $d5,$08,$08,$23
run3:
.byte 4
.byte $c6,$00,$00,$23
.byte $c7,$00,$08,$23
.byte $d6,$08,$00,$23
.byte $d7,$08,$08,$23
run4:
.byte 4
.byte $c8,$00,$00,$23
.byte $c9,$00,$08,$23
.byte $d8,$08,$00,$23
.byte $d9,$08,$08,$23


squidmove:
.word squid1
.byte 255
.word squid1
.byte 255
.word squid1
.byte 255
.word squid2
.byte 255
.word squid2
.byte 255
.word squid3
.byte 255
.word squid3
.byte 255

.word squid1
.byte 88
.word squid2
.byte 88
.word squid3
.byte 88
.word squid0
.byte 80
.word squid1
.byte 80
.word squid2
.byte 80
.word squid3
.byte 80
.word squid0
.byte 78
.word squid1
.byte 78
.word squid2
.byte 78
.word squid3
.byte 78
.word squid0
.byte 60
.word squid1
.byte 60
.word squid2
.byte 60
.word squid3
.byte 55
.word squid0
.byte 55
.word squid1
.byte 50
.word squid2
.byte 50
.word squid3
.byte 45
.word squid0
.byte 45
.word squid1
.byte 45
.word squid2
.byte 50
.word squid3
.byte 50
.word squid0
.byte 50
.word squid1
.byte 55
.word squid2
.byte 55
.word squid3
.byte 60


.word squidloop
;.word anim_loop

squidloop: .byte $FF,23*3

squid0:
.byte 14

.byte $80,0,0,$22
.byte $81,0,$8,$22
.byte $82,0,$10,$22
.byte $83,0,$18,$22
.byte $84,0,$20,$22
.byte $85,0,$28,$22
.byte $86,0,$30,$22

.byte $90,8,0,$22
.byte $91,8,$8,$22
.byte $92,8,$10,$22
.byte $93,8,$18,$22
.byte $94,8,$20,$22
.byte $95,8,$28,$22
.byte $96,8,$30,$22

squid1:
.byte 14

.byte $87,0,2,$22
.byte $88,0,$a,$22
.byte $89,0,$12,$22
.byte $8a,0,$1a,$22
.byte $8b,0,$22,$22
.byte $8c,0,$2a,$22
.byte $8d,0,$32,$22

.byte $97,8,2,$22
.byte $98,8,$a,$22
.byte $99,8,$12,$22
.byte $9a,8,$1a,$22
.byte $9b,8,$22,$22
.byte $9c,8,$2a,$22
.byte $9d,8,$32,$22

squid2:
.byte 14

.byte $a0,0,1,$22
.byte $a1,0,$9,$22
.byte $a2,0,$11,$22
.byte $a3,0,$19,$22
.byte $a4,0,$21,$22
.byte $a5,0,$29,$22
.byte $a6,0,$31,$22

.byte $b0,8,1,$22
.byte $b1,8,$9,$22
.byte $b2,8,$11,$22
.byte $b3,8,$19,$22
.byte $b4,8,$21,$22
.byte $b5,8,$29,$22
.byte $b6,8,$31,$22


squid3:
.byte 14

.byte $a7,0,0,$22
.byte $a8,0,$8,$22
.byte $a9,0,$10,$22
.byte $aa,0,$18,$22
.byte $ab,0,$20,$22
.byte $ac,0,$28,$22
.byte $ad,0,$30,$22

.byte $b7,8,0,$22
.byte $b8,8,$8,$22
.byte $b9,8,$10,$22
.byte $ba,8,$18,$22
.byte $bb,8,$20,$22
.byte $bc,8,$28,$22
.byte $bd,8,$30,$22


anim_loop: .byte $FF,0



explode:

        ldy #0
        ldx anim_number
        sty anim_offset,x
;        jmp explode


explode_entry:
        sta temp3_lo
        asl
        tax
        lda anim_index,x
        sta temp_lo
        lda anim_index+1,x
        sta temp_hi


        ldx explosion_follow
        lda sprites,x
        sta explosion_y
        lda sprites+3,x
        sta explosion_x


        lda (temp_lo),y  ;explode_anim      ; Y = anim_offset
        sta anim_addr
        iny
        lda (temp_lo),y  ;explode_anim+1
        sta anim_addr+1
        iny
        lda (temp_lo),y  ;explode_anim+2

        ldx anim_number
        sta anim_timer,x

        lda #2
        sta exploding,x

        lda #0
        sta anim_delay,x
        tay

        lda (anim_addr),y ; get the number of tiles to do
        beq @end_anim   ; if none, end animation and erase some tiles
        cmp #$FF        ; if it's $FF
        beq @loop_anim  ; then we'll loop the animation

        sta count_lo    ; amount of tiles for this frame
        iny

        ldx animspr_offset      ;#explode_spr*4
@loadmore:
        lda (anim_addr),y
        sta sprites+1,x
        iny
        lda explosion_y
        clc
        adc (anim_addr),y
        sta sprites,x
        iny
        lda anim_h_flip
        beq @no_h_flip
        lda explosion_x
        sec
        sbc (anim_addr),y
        jmp @flipped
@no_h_flip:
        lda explosion_x
        clc
        adc (anim_addr),y
@flipped:
        sta sprites+3,x
        iny
        lda (anim_addr),y
        stx temp_lo
        ldx anim_number
        eor anim_h_flip,x
        ldx temp_lo
        sta sprites+2,x
        iny
        inx
        inx
        inx
        inx

        dec count_lo
        bne @loadmore

        rts

@loop_anim:
        iny
        lda (anim_addr),y
        sta anim_offset,x       ; X = anim_number
        tay
        lda temp3_lo
        jmp explode_entry


@end_anim:
        lda #0
        sta exploding,x ; X = anim_number
        ldx #explode_spr*4 ;#radar_spr*4
:
        sta sprites+1,x
        inx
        inx
        inx
        inx
        cpx #(explode_spr+explode_count)*4
        bne :-
        rts


run_anim:
        lda exploding
        beq exit_anim

        lda anim_delay
        clc
        adc anim_timer
        sta anim_delay
        bcc sskip_anim

        ldy anim_offset
        iny
        iny
        iny
        sty anim_offset

        lda #2*4
        sta animspr_offset
        sta explosion_follow
        lda #0
        sta anim_number

;        inc squidtime
;        lda squidtime
;        cmp #26
;        bne :+
;        lda #0
;        sta squidtime
;        jsr explode
;:


        lda #0  ; explosion
        jsr explode_entry
sskip_anim:


        lda anim_delay+1
        clc
        adc anim_timer+1
        sta anim_delay+1
        bcc exit_anim

        inc sprites+(32*4)+3


        ldy anim_offset+1
        iny
        iny
        iny
        sty anim_offset+1

        lda #16*4
        sta animspr_offset
        sta explosion_follow
        lda #1
        sta anim_number

        lda #1  ; explosion
        jsr explode_entry


exit_anim:


        rts

