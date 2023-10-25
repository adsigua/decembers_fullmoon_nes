.ifndef ENTITY_INC
.define ENTITY_INC
;entity state
;low nib = EntityState
;hi nib flags
;x---   there was a state change
;-x--   facing left of right   
;--x-   there was sprite update

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
.endscope

.scope EntityBehaviourType
    static_still        = $00
    moving_animated     = $01
    static_animated     = $02
    moving_still        = $03
.endscope

.scope EntityState
    Idle = $00
    Moving = $01
    Attacking = $02
    Defending = $03
    Hurt = $04
    Dying = $06
    Dead = $07

    State_Changed = $80
    Facing_Right = $40
    Sprite_Update = $20
.endscope

.endif