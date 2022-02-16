# ABM-v1.1.3.R is part of Food INdustry CoViD Control Tool
# (FInd CoV Control), version 1.1.3.
# Copyright (C) 2020-2021 Cornell University.
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


source('omicron-waning-functions.R')

unisolation_fn = function(agents, start_time) {
    isolation_duration = 5

    x_un_Isol = (
        agents$isolated &
        start_time - agents$time_isolated >= isolation_duration &
        agents$infection_status %in% c('NI', 'E', 'IA')
    )
    agents$isolated[x_un_Isol] = FALSE
    agents
}

isolation_fn = function(agents, start_time, rational_testing, testing_rate,
                        fractional_test_carried, N, IA_FNR, IP_FNR, IM_FNR, FPR,
                        agent_presence) {
    #convenience and efficiency
    isolated = agents$isolated
    infection_status = agents$infection_status

    #same number of calls -> functionally identical parameter sets give
    #identical results
    randomize_ties = sample(N)
    irrational_testing_mask = (sbern(N, testing_rate) == 1)
    detection_probability = ifelse(infection_status == 'IA',
        1 - IA_FNR,
        ifelse(infection_status == 'IP',
            1 - IP_FNR,
            ifelse(infection_status == 'IM',
                1 - IM_FNR,
                ifelse(infection_status %in% c('NI', 'E'),
                    FPR,
                    0 #technically wrong, but should be irrelevant
                )
            )
        )
    )
    conditional_detection_mask = sbern(N, detection_probability)

    if(sum(testing_rate) > 0) {        
        if(max(testing_rate) == 1) {
                testing_mask = agent_presence #TBD: is this quite right?
        } else if(rational_testing) {
            indices = order(agents$time_tested, randomize_ties) 
            eligible = (
                infection_status %in% c('NI', 'E', 'IA', 'IP', 'IM') &
                agent_presence &
                !isolated
            ) #NB: Note that this tacitly assumes at-work testing,
              #not at-home testing upon feeling sick
              #TBD: This may be redundant with how testing_rate is defined?
            indices = indices[eligible[indices]] 
            theoretical_number_of_tests = testing_rate * sum(agent_presence) +
                    fractional_test_carried
            #TBD: is this right? should this instead be = sum(testing_rate) +
            #fractional_test_carried?
            #But if my concern holds, then why does testing ever occur?
            #Is it just because of fractional_test_carried? But if so, why does
            #nominal testing rate ever matter -- surely, this ends up
            #being large enough that all possible testing occurs on all
            #shifts. This needs testing. It appears to be working right, but I
            #am not sure why.
            number_of_tests = min(floor(theoretical_number_of_tests),
                    length(indices))
            fractional_test_carried = theoretical_number_of_tests -
                    number_of_tests
            testing_mask = (1:N) %in% indices[1:number_of_tests]
        } else {
            testing_mask = irrational_testing_mask
        }

        agents$time_tested[testing_mask] = start_time

        x_to_Isol = testing_mask & conditional_detection_mask #this is all,
                                                              #right?
        agents$isolated[x_to_Isol] = TRUE
        agents$time_isolated[x_to_Isol] = start_time
    }
    
    list(agents = agents, fractional_test_carried = fractional_test_carried)
}

vaccinate = function(agents, N, vaccination_rate, vaccination_interval,
                     start_time, end_time, boosting_rate,
                     infection_0, immune_status_0, vax_status_0, isolated_0,
                     immunity_0) {

    #some of the assignments below could be condensed, but I'm trying to be
    #systematic about things
    
    #same number of calls -> functionally identical parameter sets give
    #identical results
    #note that vaccination_rate is a vector, not a scalar
    vaccination_mask = sbern(N, vaccination_rate)
    boosting_mask = sbern(N, boosting_rate)
    event_times = sunif(N, start_time, end_time)

    if(sum(vaccination_rate) > 0) {

        S_to_V1 = (infection_0 == 'NI' &
                   immune_status_0 == 'FS' &
                   vax_status_0 == 'NV' &
                   vaccination_mask &
                   !isolated_0
        )
        #agents = update(agents,
        #                mask = S_to_V1,
        #                immune_status = 'V1',
        #                vax_status = 'V1'
        agents$previous_immunity[S_to_V1] = immunity_0[S_to_V1]
        agents$time_V1[S_to_V1] = event_times[S_to_V1]
        agents$time_last_immunity_event[S_to_V1] = agents$time_V1[S_to_V1]
        agents$immune_status[S_to_V1] = 'V1'
        agents$vax_status[S_to_V1] = 'V1'

        V1_to_V2 = (infection_0 == 'NI' &
                    immune_status_0 == 'V1' &
                    vax_status_0 == 'V1' &
                    end_time - agents$time_V1 > vaccination_interval &
                    vaccination_rate > 0 &
                    !isolated_0
        )
        agents$previous_immunity[V1_to_V2] = immunity_0[V1_to_V2]
        agents$time_V2[V1_to_V2] = event_times[V1_to_V2]
        agents$time_last_immunity_event[V1_to_V2] = agents$time_V2[V1_to_V2]
        agents$immune_status[V1_to_V2] = 'V2'
        agents$vax_status[V1_to_V2] = 'V2'

        #boosting via shots earlier than boosters (+ natural Recovery)
        R_to_B_via_V1 = (infection_0 == 'NI' &
                         immune_status_0 == 'R' &
                         vax_status_0 == 'NV' &
                         vaccination_mask &
                         !isolated_0
        )
        agents$previous_immunity[R_to_B_via_V1] = immunity_0[R_to_B_via_V1]
        agents$time_B[R_to_B_via_V1] = event_times[R_to_B_via_V1]
        agents$time_last_immunity_event[R_to_B_via_V1] = agents$time_B[R_to_B_via_V1]
        agents$immune_status[R_to_B_via_V1] = 'B'
        agents$vax_status[R_to_B_via_V1] = 'V1'

        R_to_B_via_V2 = (infection_0 == 'NI' &
                         immune_status_0 == 'R' &
                         vax_status_0 == 'V1' &
                         end_time - agents$time_V1 > vaccination_interval &
                         vaccination_rate > 0 &
                         !isolated_0
        )
        agents$previous_immunity[R_to_B_via_V2] = immunity_0[R_to_B_via_V2]
        agents$time_B[R_to_B_via_V2] = event_times[R_to_B_via_V2]
        agents$time_last_immunity_event[R_to_B_via_V2] = agents$time_B[R_to_B_via_V2]
        agents$immune_status[R_to_B_via_V2] = 'B'
        agents$vax_status[R_to_B_via_V2] = 'V2'
    }# else {
    #    R_to_B_via_V1 = FALSE
    #    R_to_B_via_V2 = FALSE
    #}

    x_to_B_on_time = (infection_0 == 'NI' &
                      #any immune status
                      vax_status_0 == 'V2' &
                      end_time - agents$time_V2 > 152 &
                      #any vaccination rate
                      agents$boosting_on_time &
                      !isolated_0
    )
    agents$previous_immunity[x_to_B_on_time] = immunity_0[x_to_B_on_time]
    agents$time_B[x_to_B_on_time] = event_times[x_to_B_on_time]
    agents$time_last_immunity_event[x_to_B_on_time] = agents$time_B[x_to_B_on_time]
    agents$immune_status[x_to_B_on_time] = 'B'
    agents$vax_status[x_to_B_on_time] = 'B'

    #boost
    if(sum(boosting_rate) > 0) {
        #time_V2 is only set when receiving second dose
        #so is still valid in R or B (from R + V1 or V2)
        x_to_B_late = ((infection_0 == 'NI' &
                vax_status_0 == 'V2' & !isolated_0) &
                (end_time - agents$time_V2 > 152 + 1) & #5 months
                boosting_mask)
        agents$previous_immunity[x_to_B_late] = immunity_0[x_to_B_late]
        agents$time_B[x_to_B_late] = event_times[x_to_B_late]
        agents$time_last_immunity_event[x_to_B_late] = agents$time_B[x_to_B_late]
        agents$immune_status[x_to_B_late] = 'B'
        agents$vax_status[x_to_B_late] = 'B'
    } #else #{
    #    x_to_B_late = FALSE
    #}

    #x_to_B_any = R_to_B_via_V1 | R_to_B_via_V2 | x_to_B_on_time | x_to_B_late
    #agents$time_B[x_to_B_any] = event_times[x_to_B_any]
    #agents$previous_immunity[x_to_B_any] = immunity_0[x_to_B_any]
    #agents$time_last_immunity_event[x_to_B_any] = agents$time_B[x_to_B_any]
    #agents$immune_status[x_to_B_any] = 'B'
    #agents$vax_status[x_to_B_on_time] = 'B'
    #agents$vax_status[x_to_B_late] = 'B'
    #agents$vax_status[R_to_B_via_V1] = 'V1'
    #agents$vax_status[R_to_B_via_V2] = 'V2'


    immunity_1 = net_symptomatic_protection(agents, start_time)
    test_mask = immunity_1 < immunity_0
    if(any(test_mask)) {
        print(agents[test_mask,])
        print('Was:')
        print(infection_0[test_mask])
        print(immune_status_0[test_mask])
        print(vax_status_0[test_mask])
        print(isolated_0[test_mask])
        print(immunity_0[test_mask])
        stop('And here is the failure.')
    }

    agents
}

#infection_0 is *not* used here, because we want (theoretical, in practice
#extremely unlikely) < 1 shift duration phases to end in a non-"retroactive"
#fashion
progress_infection = function(agents, N, start_time, end_time, symptoms_0,
                              isolated_0, immunity_0) {
    xE_to_I = (agents$infection_status == 'E' &
               end_time - agents$time_E > agents$duration_E
    )
    xE_to_IP = (xE_to_I &
                agents$symptomatic &
                sbern(N, symptoms_0)
    )
    agents$time_IP[xE_to_IP] = agents$time_E[xE_to_IP] +
        agents$duration_E[xE_to_IP]

    xE_to_IA = xE_to_I & !xE_to_IP
    agents$time_IA[xE_to_IA] = agents$time_E[xE_to_IA] +
        agents$duration_E[xE_to_IA]
    agents$infection_status[xE_to_IA] = 'IA'

    IP_to_IM = (agents$infection_status == 'IP' &
                end_time - agents$time_IP > agents$duration_IP
    )
    agents$infection_status[IP_to_IM] = 'IM'
    agents$time_IM[IP_to_IM] = agents$time_IP[IP_to_IM] +
        agents$duration_IP[IP_to_IM]
    agents$time_isolated[IP_to_IM & isolated_0] =
        agents$time_IM[IP_to_IM & isolated_0]
    agents$infection_status[xE_to_IP] = 'IP'
    #TBD: above is awkward, and should ideally be in an isolation function
    #but doing this in practice is tricky

    IA_to_R =  (agents$infection_status == 'IA' &
                end_time - agents$time_IA > agents$duration_IA
    )
    agents$time_R[IA_to_R] = agents$time_IA[IA_to_R] +
        agents$duration_IA[IA_to_R]
    #other attribute updates below

    IM_to_x =  (agents$infection_status == 'IM' &
                end_time - agents$time_IM > agents$duration_IM
    )

    #TBD (eventually): Account for reduced chance of severe disease
    #conditional on symptomatic
    IM_to_IS = IM_to_x & agents$severe
    agents$time_IS[IM_to_IS] = agents$time_IM[IM_to_IS] +
        agents$duration_IM[IM_to_IS]
    agents$infection_status[IM_to_IS] = 'IS'

    IM_to_R =  IM_to_x & !(agents$severe)
    agents$time_R[IM_to_R] = agents$time_IM[IM_to_R] +
        agents$duration_IM[IM_to_R]

    IS_to_x = (agents$infection_status == 'IS' &
               end_time - agents$time_IS > agents$duration_IS
    )

    IS_to_IC = IS_to_x & agents$critical
    agents$time_IC[IS_to_IC] = agents$time_IS[IS_to_IC] +
        agents$duration_IS[IS_to_IC]
    agents$infection_status[IS_to_IC] = 'IC'

    IS_to_R =  IS_to_x & !(agents$critical)
    agents$time_R[IS_to_R] = agents$time_IS[IS_to_R] +
        agents$duration_IS[IS_to_R]

    IC_to_x = (agents$infection_status == 'IC' &
               end_time - agents$time_IC > agents$duration_IC
    )

    IC_to_D = IC_to_x & agents$death
    agents$time_D[IC_to_D] = agents$time_IC[IC_to_D] +
        agents$duration_IC[IC_to_D]
    agents$infection_status[IC_to_D] = 'D'

    IC_to_R = IC_to_x & !(agents$death)
    agents$time_R[IC_to_R] = agents$time_IC[IC_to_R] +
        agents$duration_IC[IC_to_R]

    x_to_R = IA_to_R | IM_to_R | IS_to_R | IC_to_R
    agents$previous_immunity[x_to_R] = immunity_0[x_to_R]
    #time_R assigned in each specific case above
    agents$time_last_immunity_event[x_to_R] = agents$time_R[x_to_R]
    agents$immune_status[x_to_R] = 'R'
    agents$infection_status[x_to_R] = 'NI'

    agents
}

ABM <- function(agents, contacts_list, lambda_list, schedule,
                virus_parameters, testing_parameters, #vaccine_parameters,
                vaccination_interval,
                scenario_parameters, steps, step_length_list, testing_rate_list,
                vaccination_rate_list,  agent_presence_list,
                quantitative_presence_list, #waning_parameters,
                boosting_rate) {

    N <-nrow(agents)

    Out1 <- data.frame(S = rep(0, steps),
                       E = rep(0, steps),
                       IA = rep(0, steps),
                       IP = rep(0, steps),
                       IM = rep(0, steps),
                       IS = rep(0, steps),
                       IC = rep(0, steps),
                       R = rep(0, steps),
                       RE = rep(0, steps),
                       V1 = rep(0, steps),
                       V2 = rep(0, steps),
                       V1E = rep(0, steps),
                       V2E = rep(0, steps),
                       S_isolated = rep(0, steps),
                       E_isolated = rep(0, steps),
                       IA_isolated = rep(0, steps),
                       IP_isolated = rep(0, steps),
                       IM_isolated = rep(0, steps),
                       R_isolated = rep(0, steps),
                       RE_isolated = rep(0, steps),
                       V1_isolated = rep(0, steps),
                       V2_isolated = rep(0, steps),
                       V1E_isolated = rep(0, steps),
                       V2E_isolated = rep(0, steps),
                       n_scheduled = rep(0, steps),
                       n_absent = rep(0, steps),
                       new_infections = rep(0, steps)
    )

    # Dump parameters to local variables -- centralized for easier tweaking
    # and to make it easier to verify consistent use of "get",
    # for easier debugging.
    #
    # This approach may or may not be replaced with use of "with" at some point
    # in the future.

    IA_FNR = get('asymptomatic_FNR', testing_parameters)
    IP_FNR = get('presymptomatic_FNR', testing_parameters)
    IM_FNR = get('mild_FNR',testing_parameters)
    FPR = get('FPR', testing_parameters)
    rational_testing = get('rational_testing', testing_parameters)
    p_trans_IA = get('p_trans_IA', virus_parameters)
    p_trans_IP = get('p_trans_IP', virus_parameters)
    p_trans_IM = get('p_trans_IM', virus_parameters)
    isolation_duration = get('isolation_duration', scenario_parameters)

    end_time = 0 # End of the last shift before simulation starts
    fractional_test_carried = 0

    # Move people through time
    for(k in 1:steps) {
        contacts = get(schedule[k], contacts_list)
        lambda = get(schedule[k], lambda_list)
        step_length = get(schedule[k], step_length_list)
        testing_rate = get(schedule[k], testing_rate_list)
        vaccination_rate = get(schedule[k], vaccination_rate_list)
        agent_presence = get(schedule[k], agent_presence_list)
        quantitative_presence = get(schedule[k], quantitative_presence_list)

        start_time = end_time 
        end_time = start_time + step_length


        agents = unisolation_fn(agents, start_time)

        ifl = isolation_fn(agents, start_time, rational_testing, testing_rate,
                           fractional_test_carried, N, IA_FNR, IP_FNR, IM_FNR, 
                           FPR, agent_presence)
        agents = ifl[['agents']]
        fractional_test_carried = ifl[['fractional_test_carried']]

        #2022-02-10: pulling out repeated calls that are intended to resolve on
        #the status of agent properties at the *start* of a step
        #Many of these ideally should be adjusted over the course of a step,
        #e.g., people infected during a step should ideally affect the
        #probability of subsequent infections. But in the current state of the
        #model, this is impractical. But pulling things out this way not only
        #should speed up processing a bit; it should also help to ensure
        #consistency (i.e., not generating side effects by accident).
        #This goes after deisolation and isolation because it's isolation status
        #*after* those processes that we want to use here.
        #These may be more than we need, but should at least be adequate
        infection_0 = agents$infection_status
        isolated_0 = agents$isolated
        immune_status_0 = agents$immune_status
        time_last_immunity_event_0 = agents$time_last_immunity_event
        previous_immunity_0 = agents$previous_immunity
        immunity_0 = net_symptomatic_protection(agents, start_time)
        susceptibility_0 = 1 - infection_protection(agents, start_time)
        symptoms_0 = 1 - symptom_protection(agents, start_time)
        vax_status_0 = agents$vax_status
        
        #TBD: Theoretically, check that recovery and vaccination in the same
        #step don't generate ridiculous results
        #in practice, I'm pretty sure this is impossible, with the way that
        #progress_infection is currently coded

        agents = vaccinate(agents, N, vaccination_rate, vaccination_interval,
                           start_time, end_time, boosting_rate,
                           infection_0, immune_status_0, vax_status_0,
                           isolated_0, immunity_0)

        infectiousness = (!isolated_0) * (
            (infection_0 == 'IA') * p_trans_IA +
            (infection_0 == 'IP') * p_trans_IP +
            (infection_0 == 'IM') * p_trans_IM
        )

        foi_contributions = contacts * infectiousness
        force_of_infection = colSums(foi_contributions)
        p_infection = 1 - exp(-force_of_infection * susceptibility_0)

        potential_times_E = sunif(N, start_time, end_time)

        #For simplicity, we'll do community transmission first, i.e.,
        #if someone would be infected from community transmission and from
        #within-company transmission in the same shift, community transmission
        #wins. In the long run, this may be changed to be probabilistic.
        #In practice, though, it's unlikely to matter much -- most scenarios
        #will have few if any shifts in which both probabilities are
        #non-negligible.
        #(In fact, none in the current version, except maybe for all-shift
        #floaters (check).)

        NI_to_E_community = (
            agents$infection_status == 'NI' &
            sbern(N, 1 - exp(-lambda * susceptibility_0)) &
            !isolated_0 
        )
        agents$infection_status[NI_to_E_community] = 'E'
        agents$time_E[NI_to_E_community] = potential_times_E[NI_to_E_community]


        NI_to_E = (
            agents$infection_status == 'NI' &
            sbern(N, p_infection) &
            !isolated_0
        )
        agents$infection_status[NI_to_E] = 'E'
        agents$time_E[NI_to_E] = potential_times_E[NI_to_E] 


        #TBD: move or otherwise fix this comment?
        # Putting the process of infection on hold a moment, to figure out who
        # among the already-infected needs to progress along their course of
        # infection (or recover), before the shift is over.
        #
        # Ideally, we probably want to incorporate transitions out of
        # infectiousness into calculation of transmission potentials, but later.
        agents = progress_infection(agents, N, start_time, end_time, symptoms_0,
                                    isolated_0, immunity_0)


        #TBD: We still need to recalculate durations for repeats of the same
        #event (now possible)

        #"Out1" records the sum of individuals in each state at time k
        #(i.e., during time from time=0 to time=nTime1)
        #this allows ploting trajectories for each state in one simulation.
        #"agents" shows demographic characteristics of all individuals in the
        #population and their infection status at time nTime1

        #TBD (ASAP): move this to an update_Out1 function
        #NB: TRUE == 1 for the purpose of summation
        infection_status_1 = agents$infection_status #but is this actually right?
                                                     #if we want absences, don't
        #we want status at the start of the shift
        #TBD: Figure this out
        immune_status_1 = agents$immune_status

        Out1$S[k] <-  sum(agents$infection_status == "NI" &
                        agents$immune_status == 'FS') 
        Out1$E[k] <-  sum(agents$infection_status == "E" &
                        agents$immune_status == 'FS')
        Out1$IA[k] <- sum(agents$infection_status == "IA")
        Out1$IP[k] <- sum(agents$infection_status == "IP")
        Out1$IM[k] <- sum(agents$infection_status == "IM")
        Out1$IS[k] <- sum(agents$infection_status == "IS")
        Out1$IC[k] <- sum(agents$infection_status == "IC")
        Out1$R[k] <-  sum(agents$infection_status == "NI" &
                        agents$immune_status == 'R')
        Out1$D[k] <-  sum(agents$infection_status == "D")
        Out1$V1[k] <-  sum(agents$infection_status == "NI" &
                        agents$immune_status == "V1")
        Out1$V2[k] <-  sum(agents$infection_status == "NI" &
                        agents$immune_status == "V2")
        Out1$V1E[k] <-  sum(agents$infection_status == "E" &
                            agents$immune_status == "V1")
        Out1$V2E[k] <-  sum(agents$infection_status == "E" &
                            agents$immune_status == "V2")
        Out1$RE[k] <- sum(agents$infection_status == "E" &
                        agents$immune_status == "R")
        Out1$S_isolated[k] <-  sum(agents$infection_status == "NI" &
                                agents$immune_status == 'FS' & isolated_0)
        Out1$E_isolated[k] <-  sum(agents$infection_status == "E" &
                                agents$immune_status == 'FS' & isolated_0)
        Out1$IA_isolated[k] <- sum(agents$infection_status == "IA" &
                                isolated_0)
        Out1$IP_isolated[k] <- sum(agents$infection_status == "IP" &
                                isolated_0)
        Out1$IM_isolated[k] <- sum(agents$infection_status == "IM" &
                                isolated_0)
        Out1$R_isolated[k] <-  sum(agents$infection_status == "NI" &
                                agents$immune_status == "R" & isolated_0)
        Out1$V1_isolated[k] <-  sum(agents$infection_status == "NI" &
                                    agents$immune_status == "V1" & isolated_0)
        Out1$V2_isolated[k] <-  sum(agents$infection_status == "NI" &
                                    agents$immune_status == "V2" & isolated_0)
        Out1$V1E_isolated[k] <-  sum(agents$infection_status == "E" &
                                    agents$immune_status == "V1" & isolated_0)
        Out1$V2E_isolated[k] <-  sum(agents$infection_status == "E" &
                                    agents$immune_status == "V2" & isolated_0)
        ####
        #note that *for now* I am treating between-shift floaters as present at a
        #deterministic 1/3 or 1/2, as the case may be
        ####
        Out1$n_scheduled[k] = sum(agent_presence)
        Out1$n_absent[k] = sum(agent_presence * (agents$infection_status %in%
                                    c('IS', 'IC', 'D') | isolated_0))
        Out1$RE_isolated[k] <-  sum(agents$infection_status == "E" &
                                    agents$immune_status == "R" & isolated_0)
        Out1$qn_scheduled[k] = sum(quantitative_presence)
        Out1$qn_absent[k] = sum(quantitative_presence *
                                (agents$infection_status %in% c('IS', 'IC', 'D') |
                                isolated_0))
        Out1$new_infections[k] = sum(NI_to_E_community + NI_to_E)
    
    }
    
    Out <- list("Out1" = Out1, "agents" = agents)
    
    return (Out) #return a list of objects
}

