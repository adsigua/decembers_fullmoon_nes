.ifndef ENTITY_INC
.define ENTITY_INC

;entity addresses======================================================
    entity_addr = $0300
    entity_type = $0300
    entity_state = $0310
    entity_id = $0320
    entity_anim_id = $0330
    entity_anim_frame_id = $0340
    entity_anim_delay_cnt = $0350
    entity_pos_x = $0360
    entity_pos_y = $0370
    entity_level_pos_x = $0380
    entity_velocity_x = $0390
    entity_velocity_y = $03a0
    entity_pos_x_lo = $03b0
    entity_pos_y_lo = $03c0
    entity_invu_time = $03d0
    entity_stack = $03e0
    entity_addr_end = $03ef
    
    ;curr_entity_index = $00
    tempX = $0c
    tempY = $0d
    tempZ = $0e
    tempW = $0f

    min_bg_xpos_scroll = $30
    sprite_width = $08

    left_screen_limit = $04
    right_screen_limit = $fd
    top_floor_limit = $8a
    bottom_floor_limit = $de

.struct Entity
    type                    .byte
    entity_id               .byte       
    entity_state            .byte
    entity_anim_num         .byte
    entity_anim_delay_cnt   .byte
    screen_pos_x            .byte
    screen_pos_y            .byte
    velocity_x              .byte
    velocity_y              .byte
    pos_x_lo                .byte
    pos_y_lo                .byte
.endstruct

.scope EntityType
    none_type           = $00
    player_type         = $10
    enemy_type          = $20
    player_projectile   = $30
    enemy_projectile    = $40
    environment         = $50
.endscope

.scope EntityBehaviourType
    static_still        = $00
    moving_animated     = $01
    static_animated     = $02
    moving_still        = $03
.endscope

.scope EntityID
    player         = $00
    rock           = $01
    mini_angel     = $02
    golem          = $03
    beast          = $04
    demon          = $05
    dybbuk         = $06
    witch          = $07
    snake          = $08

    leviathan      = $09
    behemoth       = $0a
    ziz            = $0b
    angel          = $0c

    boss           = $80
.endscope

;entity state
;low nib = EntityState
;hi nib flags
;x---   there was a state change
;-x--   facing left of right   
;--x-   there was sprite update
.scope EntityState
    Idle = $00
    Moving = $01
    Attacking = $02
    Guarding = $03
    Hurt = $04
    Dying = $06
    Dead = $07

    State_Changed = $80
    Facing_Right = $40
    Sprite_Update = $20
.endscope

.endif