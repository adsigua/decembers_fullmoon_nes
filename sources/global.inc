.ifndef GLOBAL_INC
.define GLOBAL_INC

; init.s
.global reset_handler

; main.s
.global main, nmi_handler, irq_handler
.global identity_table
.globalzp ppu_ctrl_val, ppu_mask_val

;entities.s
.global init_entities, update_entities, update_oam, get_tile_type

;player.s
.global update_player, update_player_entity, init_player_entity

; ppu
.global ppu_clear_nt, ppu_clear_oam, ppu_screen_on
.global update_oam_buffer, send_oam_dma, place_sprite_0
.globalzp oam_counter, oam_buffer

; pads.s
.global read_pads
.globalzp cur_keys, new_keys

;bg .s
.global draw_bg, update_column, draw_column
.globalzp col_pointer

;level .s
.global init_level_manager, init_level, init_player_pos
.global init_scroll, check_update_bg_scroll, update_scroll, apply_scroll_x
.globalzp level_index, level_flags, scroll_x





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
