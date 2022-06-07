# general-waning-functions-v2.2.0.R is part of Food INdustry CoViD Control Tool
# (FInd CoV Control), version 2.1.2.
# Copyright (C) 2020-2022 Cornell University.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

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

R_decay_45 = function(t) {
    pmin(1, default_R_decay(t - 45 + 14))
}

R_protection_45 = function(t, prev) {
    #prev unused; included for consistent interface
    ifelse(t < 45,
        1,
        R_decay_45(t)
    )
}

#R_protection = B_protection
#default_B_protection = default_R_protection

make_protection_functions = function(V1_protection, V2_protection, B_protection,
                                     R_protection) {

    net_symptomatic_protection = function(agents, start_time) {
        ais = agents$immune_status
        t = (start_time - agents$time_last_immunity_event)
        t = pmax(t, 0) #TBD (eventually): find a way to not need this kludge
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

protection_functions_45 = make_protection_functions(default_V1_protection,
                                                    default_V2_protection,
                                                    default_B_protection,
                                                    R_protection_45)
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

#For use when comparing results with v1.1.3
make_one_one_three = function() {
    V1_net_symptoms = 1 - .37
    V2_susceptibility = 1 - .65
    V2_net_symptoms = 1 - .88
    V2_symptoms = V2_net_symptoms / V2_susceptibility
    V2_exp = log(V2_symptoms) / log(V2_net_symptoms)
    V1_symptoms = V1_net_symptoms ** V2_exp #haven't seen them separated, so doing the best I can
    V1_susceptibility = V1_net_symptoms / V1_symptoms

    protection_functions = make_protection_functions(
        two_level_protection(1 - V1_net_symptoms, Inf),
        two_level_protection(1 - V2_net_symptoms, Inf),
        two_level_protection(1, Inf), #Since the only way to become boosted
                                      #without booster shots is vax + R
        two_level_protection(1, Inf)
    )

    net_symptomatic_protection = protection_functions$net_symptomatic_protection

   infection_protection = function(agents, start_time) {
        ais = agents$immune_status
        t = (start_time - agents$time_last_immunity_event)
        t = pmax(t, 0) #TBD (eventually): find a way to not need this kludge
        prev = agents$previous_immunity
        protection = ifelse(ais == 'FS',
            0,
            ifelse(ais == 'V1',
                1 - V1_susceptibility,
                ifelse(ais == 'V2',
                    1 - V2_susceptibility,
                    ifelse(ais == 'B',
                        1,
                        ifelse(ais == 'R',
                            1,
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

    symptom_protection = function(agents, start_time) {
        nsp = net_symptomatic_protection(agents, start_time)
        ip = infection_protection(agents, start_time)
        ifelse(nsp == 1, 1, 1 - (1 - nsp) / (1 - ip))
    }

    protection_functions$infection_protection = infection_protection
    protection_functions$symptom_protection = symptom_protection

    protection_functions
}

one_one_three_protection_functions = make_one_one_three()
