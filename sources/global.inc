.ifndef GLOBAL_INC
.define GLOBAL_INC

; init.s
.global reset_handler

; main.s
.global main, nmi_handler, irq_handler
.global OAM, identity_table
.globalzp oam_counter, oam_buffer

;entities.s
.global init_entities, update_entities, update_oam

;player.s
.global init_player_entity, update_player, apply_player_velocity
.global update_player_animation, update_player_oam_buffer


; ppuclear.s
.global ppu_clear_nt, ppu_clear_oam, ppu_screen_on

; pads.s
.global read_pads
.globalzp cur_keys, new_keys

;bg .s
.global draw_bg, update_column, draw_column
.globalzp col_pointer

;level .s
.global init_scroll, check_update_bg_scroll, update_scroll
.globalzp level_flags, scroll_x, scroll_x_lo


;entity state
;low nib = EntityState
;hi nib flags
;x---   there was a state change
;-x--   facing left of right   
;--x-   there was sprite update
.struct Entity
    type                    .byte
    entity_id               .byte       ;index based on type 
    entity_state            .byte
    entity_anim_num         .byte
    entity_anim_delay_cnt   .byte
    screen_pos_x    .byte
    screen_pos_y    .byte
    velocity_x      .byte
    velocity_y      .byte
    pos_x_lo        .byte
    pos_y_lo        .byte
.endstruct

.scope EntityType
    none_type = $00
    player_type = $10
    enemy_type = $20
    player_projectile = $30
    enemy_projectile = $40
.endscope

.scope EntityBehaviourType
    static_still = $00
    moving_animated = $01
    static_animated = $02
    moving_still = $03
.endscope

.scope EntityState
    Idle = $00
    Moving = $01
    Attacking = $02
    Defending = $03
    Hurt = $04
    Dying = $06
    Dead = $07
.endscope

;entity addresses======================================================
    entity_addr = $300
    entity_type = $0300
    entity_state = $0310
    entity_id = $0320
    entity_anim_id = $0330
    entity_anim_frame_id = $0340
    entity_anim_delay_cnt = $0350
    entity_pos_x = $0360
    entity_pos_y = $0370
    entity_velocity_x = $0380
    entity_velocity_y = $0390
    entity_pos_x_lo = $03a0
    entity_pos_y_lo = $03b0
    entity_addr_end = $03bf

    curr_entity_index = $00

    curr_entity_type = $01
    curr_entity_state = $02       
    curr_entity_id = $03
    curr_entity_anim_id = $04
    curr_entity_anim_frame_id = $05

    tempX = $0c
    tempY = $0d
    tempZ = $0e
    tempW = $0f

;level addresses======================================================


;trash ==============================================
    ; .struct Entity_moving_still
    ; .struct EntityData
    ;     type            .byte
    ;     behavior_type   .byte
    ;     spawn_x         .byte
    ;     spawn_y         .byte
    ;     move_speed      .byte
    ;     tile_count      .byte
    ;     spr_x_offsets   .addr
    ;     spr_y_offsets   .addr
    ;     anim_number     .byte
    ;     anim_addrs      .addr
    ; .endstruct

    ; .struct AnimFrameData
    ;     frame_duration  .byte
    ;     frame_palette   .byte
    ;     frame_sprites_addr  .addr
    ; .endstruct

    ;     type            .byte
    ;     behavior_type   .byte
    ;     screen_pos_x    .byte
    ;     screen_pos_y    .byte
    ;     entity_state    .byte

    ;     velocity_x      .byte
    ;     velocity_y      .byte
    ;     pos_x_lo        .byte
    ;     pos_x_hi        .byte
    ;     pos_y_lo        .byte
    ;     pos_y_hi        .byte
    ; .endstruct

    ; .struct Entity_moving
    ;     type            .byte
    ;     behavior_type   .byte
    ;     screen_pos_x    .byte
    ;     screen_pos_y    .byte
    ;     entity_state    .byte

    ;     anim_data       .addr
    ;     anim_id         .byte
    ;     anim_frame_num  .byte
    ;     anim_delay_cnt  .byte

    ;     velocity_x      .byte
    ;     velocity_y      .byte
    ;     pos_x_lo        .byte
    ;     pos_x_hi        .byte
    ;     pos_y_lo        .byte
    ;     pos_y_hi        .byte
    ; .endstruct

    ; .struct Entity_static_animated
    ;     type            .byte
    ;     behavior_type   .byte
    ;     screen_pos_x    .byte
    ;     screen_pos_y    .byte
    ;     entity_state    .byte

    ;     anim_data       .addr
    ;     anim_id         .byte
    ;     anim_frame_num  .byte
    ;     anim_delay_cnt  .byte
    ; .endstruct

.endif
