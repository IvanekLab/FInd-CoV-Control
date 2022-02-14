
#symptomatic = function(agents) {
#    (agents$immune_status == 'FS' & agents$symptomatic) |
#        (agents$immune_status == 'V1' & agents$V1_symptomatic) |
#        (agents$immune_status == 'V2' & agents$V2_symptomatic) |
#        (agents$immune_status == 'R' & agents$R_symptomatic) |
#        (agents$immune_status == 'W' & agents$W_symptomatic) |
#        (agents$immune_status == 'B' & agents$B_symptomatic)
#}

V2_decay = function(t) {
    0.91157392 * exp(-0.08904459 * t)
}

B_decay = function(t) {
    0.471669758 * exp(-0.083161719*t) + 0.326600870 *exp(-0.008970573*t)
}

R_decay = B_decay #although the protection functions are non-identical

V1_protection = function(t) {
    ifelse(t < 3, .36 * t / 3, .36)
}

V2_protection = function(t, prev) {
    ifelse(t < 2, (t / 2) * V2_decay(2) + ((2 - t) / 2) * prev, V2_decay(t))
}


B_protection = function(t, prev) {
    ifelse(t < 1, 
        t * .62 + (1 - t) * prev,
        ifelse(t < 2,
            (2 - t) * .62 + (t - 1) * B_decay(2), 
            B_decay(t)
        )
    )
}

R_protection = function(t) {
    ifelse(t < 2, 1, R_decay(t))
}

#R_protection = B_protection
B_protection = R_protection

net_symptomatic_protection = function (agents, start_time) {
    ais = agents$immune_status
    t = (start_time - agents$time_last_immunity_event) / 7 #relevant data is
                                                           #given in weeks
                                                           #TBD:de-7?
    t = pmax(t, 0) #kludge for testing; TBD: remove
    prev = agents$previous_immunity
    #print('on')
    #print(ais)
    #print(agents$immune_status)
    #print(agents$previous_immunity)
    #print(t)
    protection = ifelse(ais == 'FS',
        0,
        ifelse(ais == 'V1',
            V1_protection(t),
            ifelse(ais == 'V2',
                V2_protection(t, prev),
                ifelse(ais == 'B',
                    B_protection(t),
                    ifelse(ais == 'R',
                        R_protection(t),
                        NA
                    )
                )
            )
        )
    )
    #print(protection)
    #print('off')
    if(any(is.na(protection))) {
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

#debugging check
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

