
#symptomatic = function(agents) {
#    (agents$immune_status == 'FS' & agents$symptomatic) |
#        (agents$immune_status == 'V1' & agents$V1_symptomatic) |
#        (agents$immune_status == 'V2' & agents$V2_symptomatic) |
#        (agents$immune_status == 'R' & agents$R_symptomatic) |
#        (agents$immune_status == 'W' & agents$W_symptomatic) |
#        (agents$immune_status == 'B' & agents$B_symptomatic)
#}

default_V2_decay = function(t) {
    0.91157392 * exp(-0.08904459 / 7 * t)
}

default_B_decay = function(t) {
    (0.471669758 * exp(-0.083161719 / 7 * t) +
     0.326600870 * exp(-0.008970573 / 7 * t))
}

default_R_decay = default_B_decay #although the protection functions are non-identical

default_V1_protection = function(t, prev) {
    #prev unused; included for consistent interface
    ifelse(t < 21,
        .36 * t / 21,
        .36
    )
}

default_V2_protection = function(t, prev) {
    ifelse(t < 14,
        (t / 14) * default_V2_decay(14) + ((14 - t) / 14) * prev,
        default_V2_decay(t)
    )
}


default_B_protection = function(t, prev) {
    ifelse(t < 7, 
        t / 7 * .62 + (7 - t) / 7 * prev,
        ifelse(t < 14,
            (14 - t) / 7 * .62 + (t - 7) / 7 * default_B_decay(14), 
            default_B_decay(t)
        )
    )
}

default_R_protection = function(t, prev) {
    #prev unused; included for consistent interface
    ifelse(t < 14,
        1,
        default_R_decay(t)
    )
}

#R_protection = B_protection
#default_B_protection = default_R_protection

make_protection_functions = function(V1_protection, V2_protection, B_protection,
                                     R_protection) {

    net_symptomatic_protection = function (agents, start_time) {
        ais = agents$immune_status
        t = (start_time - agents$time_last_immunity_event)
        t = pmax(t, 0) #TBD: find a way to not need this kludge
        prev = agents$previous_immunity
        protection = ifelse(ais == 'FS',
            0,
            ifelse(ais == 'V1',
                V1_protection(t, prev),
                ifelse(ais == 'V2',
                    V2_protection(t, prev),
                    ifelse(ais == 'B',
                        B_protection(t, prev),
                        ifelse(ais == 'R',
                            R_protection(t, prev),
                            NA
                        )
                    )
                )
            )
        )
        if(any(is.na(protection))) {
            mask = is.na(protection)
            cat('Problematic agents:\n')
            print(agents[mask,])
            stop('NAs in net_symptomatic_protection') #debugging
        }
        protection
    }

    infection_protection = function(agents, start_time) {
        1 - sqrt(1 - net_symptomatic_protection(agents, start_time))
    }

    symptom_protection = function(agents, start_time) {
        nsp = net_symptomatic_protection(agents, start_time)
        ip = infection_protection(agents, start_time)
        ifelse(nsp == 1, 1, 1 - (1 - nsp) / (1 - ip))
    }

    list(V1_protection = V1_protection,
         V2_protection = V2_protection,
         B_protection = B_protection,
         R_protection = R_protection,
         net_symptomatic_protection = net_symptomatic_protection,
         infection_protection = infection_protection,
         symptom_protection = symptom_protection
    )
}

default_protection_functions = make_protection_functions(default_V1_protection,
                                                         default_V2_protection,
                                                         default_B_protection,
                                                         default_R_protection)

default2_protection_functions = make_protection_functions(default_V1_protection,
                                                          default_V2_protection,
                                                          default_R_protection,
                                                          default_R_protection)

#old debugging check; may no longer be valid
#immune_status = c(rep('FS', 28), rep('V1', 28), rep('V2', 56), rep('B', 56),
#                  rep('R', 28))
#time_last_immunity_event = -rep((1:28),7)
#previous_immunity = c(rep(0,84), rep(.36, 28), rep(0, 28), rep(.4, 28), rep(.6,28))
#agents = list(immune_status = immune_status,
#              time_last_immunity_event = time_last_immunity_event,
#              previous_immunity = previous_immunity)
#nsp = net_symptomatic_protection(agents,0)
#ip = infection_protection(agents,0)
#sp = symptom_protection(agents,0)
#plot(nsp)
#points(ip,type='l')
#points(sp,type='l',col='red',lty=2)
#abline(h=0)

two_level_protection = function(level, duration) {
    function(t, prev) {
    #prev unused; included for consistent interface
        ifelse(t < duration,
            level,
            0
        )
    }
}
