# ABM-v2.2.0.R is part of Food INdustry CoViD Control Tool
# (FInd CoV Control), version 2.2.0.
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

unisolation_fn = function(agents, start_time, isolation_duration) {
    x_un_Isol = (
        agents$isolated &
        start_time - agents$time_isolated >= isolation_duration &
        agents$infection_status %in% c('NI', 'E', 'IA', 'IP')
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
    irrational_testing_mask = (sbern(N, testing_rate) == 1) & agent_presence
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
                testing_mask = agent_presence & !isolated
        } else if(rational_testing) {
            indices = order(agents$time_tested, randomize_ties) 
            eligible = (
                infection_status %in% c('NI', 'E', 'IA', 'IP', 'IM') &
                agent_presence &
                !isolated
            ) #NB: Note that this tacitly assumes at-work testing,
              #not at-home testing upon feeling sick
            indices = indices[eligible[indices]] 
            theoretical_number_of_tests = testing_rate * sum(agent_presence) +
                    fractional_test_carried
            number_of_tests = min(floor(theoretical_number_of_tests),
                    length(indices))
            fractional_test_carried = theoretical_number_of_tests -
                    number_of_tests
            testing_mask = (1:N) %in% indices[1:number_of_tests]
        } else {
            testing_mask = irrational_testing_mask
        }

        agents$time_tested[testing_mask] = start_time

        x_to_Isol = testing_mask & conditional_detection_mask
        if(testing_rate < 1 && any(x_to_Isol & !eligible)) {
            stop('ineligible')
        }
        if(any(x_to_Isol & agents$isolated)) {
            stop('double counting')
        }
        agents$isolated[x_to_Isol] = TRUE
        agents$time_isolated[x_to_Isol] = start_time
    } else {
        x_to_Isol = testing_mask = rep(FALSE, N)
    }
    
    list(agents = agents, tests_performed = sum(testing_mask), fractional_test_carried = fractional_test_carried)
}

# In a technical sense, the following function is totally redundant (and with
# the "exploded" style of listing numerous arguments to a function, it's not
# even shorter), *but* it helps to ensure that all subscripting is consistent,
# etc.
#
# Note that setting mask = TRUE will update the indicated values for all agents
update_agents = function(agents, mask, ...) {
    if(any(mask)) {

        N = dim(agents)[1]

        argv = list(...)

        # This block (before for(...)) is for debugging purposes, and should
        # probably be deleted from production code to save run time.
        argument_names = names(argv)
        column_names = names(agents)
        if(!all(argument_names %in% column_names)) { # i.e., if we are trying to
                                                     # "update" anything that
                                                     # doesn't already exist
            stop('Invalid column labels: ',
                argument_names[!(argument_names %in% column_names)])
        }
        if(!(is.logical(mask))) { # Guarding against accidental use of numeric
                                  # indices where logical is meant
            print('Non-logical mask alert:')
            print(mask)
            for(name in names(argv)) {
                cat(name, ':', argv[[name]], '\n')
            }
            stop('Non-logical mask:', mask)
        }
    
        for(name in names(argv)) {
            argument = argv[[name]]

            if(length(argument) == N) {
                agents[mask, name] = argument[mask] 
            } else if(length(argument) == 1) {
                agents[mask, name] = argument
            } else {
                stop('Invalid mask: ', mask)
            }
        }
    }
    agents
}

vaccinate = function(agents, N, vaccination_rate, vaccination_interval,
                     start_time, end_time, boosting_rate,
                     infection_status_0, immune_status_0, vax_status_0,
                     isolated_0, immunity_0,
                     net_symptomatic_protection,
                     boosting_interval,
                     complete_immunity_duration_R
            ) {

    #some of the assignments below could be condensed, but I'm trying to be
    #systematic about things
    
    #same number of calls -> functionally identical parameter sets give
    #identical results
    #note that vaccination_rate is a vector, not a scalar
    vaccination_mask = sbern(N, vaccination_rate)
    boosting_mask = sbern(N, boosting_rate)
    event_times = sunif(N, start_time, end_time)
    stealable = (immune_status_0 == 'R') &
        (event_times - agents$time_R < complete_immunity_duration_R)

    any_vaccination = rep(FALSE, N)

    if(sum(vaccination_rate) > 0) {

        S_to_V1 = (infection_status_0 == 'NI' &
                   immune_status_0 == 'FS' &
                   vax_status_0 == 'NV' &
                   vaccination_mask &
                   !isolated_0
        )
        agents = update_agents(agents = agents,
                               mask = S_to_V1,
                               previous_immunity = immunity_0,
                               time_V1 = event_times,
                               time_last_immunity_event = event_times,
                               immune_status = 'V1',
                               vax_status = 'V1'
        )

        V1_to_V2 = (infection_status_0 == 'NI' &
                    immune_status_0 == 'V1' &
                    vax_status_0 == 'V1' &
                    end_time - agents$time_V1 > vaccination_interval &
                    vaccination_rate > 0 &
                    !isolated_0
        )
        agents = update_agents(agents = agents,
                               mask = V1_to_V2,
                               previous_immunity = immunity_0,
                               time_V2 = event_times,
                               time_last_immunity_event = event_times,
                               immune_status = 'V2',
                               vax_status = 'V2'
        )

        #boosting via shots earlier than boosters (+ natural Recovery)
        R_to_B_via_V1 = (infection_status_0 == 'NI' &
                         immune_status_0 == 'R' &
                         vax_status_0 == 'NV' &
                         vaccination_mask &
                         !isolated_0
        )
        agents = update_agents(agents = agents,
                               mask = R_to_B_via_V1,
                               #previous_immunity = immunity_0,
                               time_V1 = event_times,
                               #time_last_immunity_event = event_times,
                               #immune_status = 'B',
                               vax_status = 'V1'
        )
        agents = update_agents(agents = agents,
                               mask = R_to_B_via_V1 & !stealable,
                               previous_immunity = immunity_0,
                               #time_V1 = event_times,
                               time_last_immunity_event = event_times,
                               immune_status = 'B'#,
                               #vax_status = 'V1'
        )

        R_to_B_via_V2 = (infection_status_0 == 'NI' &
                         immune_status_0 == 'R' &
                         vax_status_0 == 'V1' &
                         end_time - agents$time_V1 > vaccination_interval &
                         vaccination_rate > 0 &
                         !isolated_0
        )
        agents = update_agents(agents = agents,
                               mask = R_to_B_via_V2,
                               #previous_immunity = immunity_0,
                               time_V2 = event_times,
                               #time_last_immunity_event = event_times,
                               #immune_status = 'B',
                               vax_status = 'V2'
        )
        agents = update_agents(agents = agents,
                               mask = R_to_B_via_V2 & !stealable,
                               previous_immunity = immunity_0,
                               #time_V2 = event_times,
                               time_last_immunity_event = event_times,
                               immune_status = 'B'#,
                               #vax_status = 'V2'
        )

        any_vaccination = any_vaccination | S_to_V1 | V1_to_V2 | R_to_B_via_V1 |
            R_to_B_via_V2
    }

    x_to_B_on_time = (infection_status_0 == 'NI' &
                      #any immune status
                      vax_status_0 == 'V2' &
                      end_time - agents$time_V2 > boosting_interval &
                      #any vaccination rate
                      agents$boosting_on_time &
                      !isolated_0
    )
    agents = update_agents(agents = agents,
                           mask = x_to_B_on_time,
                           #previous_immunity = immunity_0,
                           time_B = event_times,
                           #time_last_immunity_event = event_times,
                           #immune_status = 'B',
                           vax_status = 'B'
    )
    agents = update_agents(agents = agents,
                           mask = x_to_B_on_time & !stealable,
                           previous_immunity = immunity_0,
                           #time_B = event_times,
                           time_last_immunity_event = event_times,
                           immune_status = 'B'#,
                           #vax_status = 'B'
    )

    any_vaccination = any_vaccination | x_to_B_on_time

    #boost
    if(sum(boosting_rate) > 0) {
        #time_V2 is only set when receiving second dose
        #so is still valid in R or B (from R + V1 or V2)
        x_to_B_late = ((infection_status_0 == 'NI' &
                vax_status_0 == 'V2' & !isolated_0) &
                (end_time - agents$time_V2 > boosting_interval + 1) & #5 months
                boosting_mask)
        agents = update_agents(agents = agents,
                               mask = x_to_B_late,
                               #previous_immunity = immunity_0,
                               time_B = event_times,
                               #time_last_immunity_event = event_times,
                               #immune_status = 'B',
                               vax_status = 'B'
        )
        agents = update_agents(agents = agents,
                               mask = x_to_B_late & !stealable,
                               previous_immunity = immunity_0,
                               #time_B = event_times,
                               time_last_immunity_event = event_times,
                               immune_status = 'B'#,
                               #vax_status = 'B'
        )
        any_vaccination = any_vaccination | x_to_B_late
    }


    # This block is for debugging and should be deleted in production code, to
    # save run time.
    immunity_1 = net_symptomatic_protection(agents, start_time)
    test_mask = immunity_1 < immunity_0
    if(any(test_mask)) {
        print(agents[test_mask,])
        print('Was:')
        print(infection_status_0[test_mask])
        print(immune_status_0[test_mask])
        print(vax_status_0[test_mask])
        print(isolated_0[test_mask])
        print(immunity_0[test_mask])
        stop('And here is the failure.')
    }

    list(agents = agents,
         doses = sum(any_vaccination)
    )
}

#infection_status_0 is *not* used here, because we want (theoretical, in
#practice extremely unlikely) < 1 shift duration phases to end in a
#non-"retroactive" fashion
progress_infection = function(agents, N, start_time, end_time, symptoms_0,
                              isolated_0, immunity_0) {
    # Note that, as currently coded, this function could generate odd results if
    # vaccination can confer stronger immunity than natural recovery *and* an
    # asymptomatic infected gets vaccinated and recovers on the same shift. But
    # the former condition is never true in our current production code, and the
    # latter should be rare in any event.

    xE_to_I = (agents$infection_status == 'E' &
               end_time - agents$time_E > agents$duration_E
    )
    xE_to_IP = (xE_to_I &
                sbern(N, symptoms_0 * agents$p_symptomatic)
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
        agents$time_IM[IP_to_IM & isolated_0] #resetting isolation duration upon
                                              #symptom onset
    agents$infection_status[xE_to_IP] = 'IP'

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
    #conditional on symptomatic disease due to history of vaccination or natural
    #infection
    severe = sbern(N, agents$p_severe)
    IM_to_IS = IM_to_x & severe
    agents$time_IS[IM_to_IS] = agents$time_IM[IM_to_IS] +
        agents$duration_IM[IM_to_IS]
    agents$infection_status[IM_to_IS] = 'IS'

    IM_to_R =  IM_to_x & !severe
    agents$time_R[IM_to_R] = agents$time_IM[IM_to_R] +
        agents$duration_IM[IM_to_R]

    IS_to_x = (agents$infection_status == 'IS' &
               end_time - agents$time_IS > agents$duration_IS
    )
    critical = sbern(N, agents$p_critical)
    IS_to_IC = IS_to_x & critical
    agents$time_IC[IS_to_IC] = agents$time_IS[IS_to_IC] +
        agents$duration_IS[IS_to_IC]
    agents$infection_status[IS_to_IC] = 'IC'

    IS_to_R =  IS_to_x & !critical
    agents$time_R[IS_to_R] = agents$time_IS[IS_to_R] +
        agents$duration_IS[IS_to_R]

    IC_to_x = (agents$infection_status == 'IC' &
               end_time - agents$time_IC > agents$duration_IC
    )

    death = sbern(N, agents$p_death)
    IC_to_D = IC_to_x & death
    agents$time_D[IC_to_D] = agents$time_IC[IC_to_D] +
        agents$duration_IC[IC_to_D]
    agents$infection_status[IC_to_D] = 'D'

    IC_to_R = IC_to_x & !death
    agents$time_R[IC_to_R] = agents$time_IC[IC_to_R] +
        agents$duration_IC[IC_to_R]

    x_to_R = IA_to_R | IM_to_R | IS_to_R | IC_to_R
    agents$previous_immunity[x_to_R] = immunity_0[x_to_R]
    #time_R assigned in each specific case above
    agents$time_last_immunity_event[x_to_R] = agents$time_R[x_to_R]
    agents$immune_status[x_to_R] = 'R'
    agents$infection_status[x_to_R] = 'NI'

    list(agents = agents, IP_to_IM = IP_to_IM)
}

ABM <- function(agents, contacts_list, lambda_list, schedule,
                virus_parameters, testing_parameters,
                vaccination_interval,
                scenario_parameters, steps, step_length_list, testing_rate_list,
                vaccination_rate_list,  agent_presence_list,
                quantitative_presence_list, 
                boosting_rate_list,
                protection_functions,
                kConstants) {

    N <-nrow(agents)

    Out1 = make_Out1(steps)
    
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
    isolation_duration = get('isolation_duration', kConstants)
    net_symptomatic_protection = get('net_symptomatic_protection',
                                     protection_functions)
    infection_protection = get('infection_protection', protection_functions)
    symptom_protection = get('symptom_protection', protection_functions)

    #constants
    boosting_interval = get('boosting_interval', kConstants)
    complete_immunity_duration_R = get('complete_immunity_duration_R',
                                       kConstants)

    #Creating initial conditions
    end_time = 0 # End of the last shift before simulation starts
    fractional_test_carried = 0

    # Move people through time
    for(k in 1:steps) {
        contacts = get(schedule[k], contacts_list)
        lambda = get(schedule[k], lambda_list)
        step_length = get(schedule[k], step_length_list)
        testing_rate = get(schedule[k], testing_rate_list)
        vaccination_rate = get(schedule[k], vaccination_rate_list)
        boosting_rate = get(schedule[k], boosting_rate_list)
        agent_presence = get(schedule[k], agent_presence_list)
        quantitative_presence = get(schedule[k], quantitative_presence_list)

        start_time = end_time 
        end_time = start_time + step_length


        agents = unisolation_fn(agents, start_time, isolation_duration)

        ifl = isolation_fn(agents, start_time, rational_testing, testing_rate,
                           fractional_test_carried, N, IA_FNR, IP_FNR, IM_FNR, 
                           FPR, agent_presence)
        agents = ifl[['agents']]
        tests_performed = ifl[['tests_performed']]
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
        infection_status_0 = agents$infection_status
        isolated_0 = agents$isolated
        immune_status_0 = agents$immune_status
        time_last_immunity_event_0 = agents$time_last_immunity_event
        previous_immunity_0 = agents$previous_immunity
        immunity_0 = net_symptomatic_protection(agents, start_time)
        susceptibility_0 = 1 - infection_protection(agents, start_time)
        symptoms_0 = 1 - symptom_protection(agents, start_time)
        vax_status_0 = agents$vax_status
        
        vl = vaccinate(agents, N, vaccination_rate, vaccination_interval,
                       start_time, end_time, boosting_rate,
                       infection_status_0, immune_status_0, vax_status_0,
                       isolated_0, immunity_0,
                       net_symptomatic_protection,
                       boosting_interval,
                       complete_immunity_duration_R
        )
        agents = vl[['agents']]
        doses = vl[['doses']]

        infectiousness = (!isolated_0) * (
            (infection_status_0 == 'IA') * p_trans_IA +
            (infection_status_0 == 'IP') * p_trans_IP +
            (infection_status_0 == 'IM') * p_trans_IM
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
        #(In fact, none in the current version, except for all-shift floaters.)

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


        # Ideally, we probably want to incorporate transitions out of
        # infectiousness (or into a different level of infectiousness) into
        # calculation of transmission potentials, but that's a task for a later
        # version.
        pil = progress_infection(agents, N, start_time, end_time, symptoms_0,
                                    isolated_0, immunity_0)
        agents = pil[['agents']]
        IP_to_IM = pil[['IP_to_IM']]


        #TBD (eventually): We still need to recalculate durations for repeats of
        #the same event (now possible).

        Out1 = update_Out1(Out1, k, agents, infection_status_0, isolated_0,
                           agent_presence, quantitative_presence,
                           NI_to_E_community, NI_to_E, doses, tests_performed, IP_to_IM)
    
    }

    #"Out1" records the sum of individuals in each state at time k
    #(i.e., during time from time=0 to time=nTime1)
    #this allows ploting trajectories for each state in one simulation.
    #"agents" shows demographic characteristics of all individuals in the
    #population and their infection status at time nTime1    
    Out <- list("Out1" = Out1, "agents" = agents)
    
    Out #return a list of objects
}

# TBD (eventually): Either trim the following functions to only include what we
# actually need for analyses, or at least systematize based on current handling
# of state (infection_status + immune_status + vax_status). But for now, this is
# fine (if excessive and kinda weird).


make_Out1 = function(steps) {
    data.frame(
        S = rep(0, steps),
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
        BE = rep(0, steps),
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
        new_infections = rep(0, steps),
        new_symptomatic_infections = rep(0, steps),
        doses = rep(0, steps),
        tests = rep(0, steps)
    )
}

update_Out1 = function(Out1, k, agents, infection_status_0, isolated_0,
                       agent_presence, quantitative_presence,
                       NI_to_E_community, NI_to_E, doses, tests_performed,
                       IP_to_IM) {
    #NB: TRUE == 1 for the purpose of summation
    infection_status_1 = agents$infection_status
    immune_status_1 = agents$immune_status

    f = function(infection_status, immune_status = NULL, mask = TRUE) {
        if(is.null(immune_status)) {
            immune_status = immune_status_1 #will always match
        }
        sum(
            infection_status_1 == infection_status &
            immune_status_1 == immune_status &
            mask
        )
    }

        Out1$S[k] <-  f('NI', 'FS') 
        Out1$E[k] <-  f('E', 'FS')
        Out1$IA[k] <- f('IA')
        Out1$IP[k] <- f('IP')
        Out1$IM[k] <- f('IM')
        Out1$IS[k] <- f('IS')
        Out1$IC[k] <- f('IC')
        Out1$R[k] <-  f('NI', 'R')
        Out1$RE[k] <- f('E', 'R')
        Out1$D[k] <-  f('D')
        Out1$V1[k] <-  f('NI', 'V1')
        Out1$V2[k] <-  f('NI', 'V2')
        Out1$V1E[k] <-  f('E', 'V1')
        Out1$V2E[k] <-  f('E', 'V2')
        Out1$BE[k] <- f('E', 'B')
        Out1$S_isolated[k] <-  f('NI', 'FS',  isolated_0)
        Out1$E_isolated[k] <-  f('E', 'FS',  isolated_0)
        Out1$IA_isolated[k] <- f('IA', NULL, isolated_0)
        Out1$IP_isolated[k] <- f('IP', NULL, isolated_0)
        Out1$IM_isolated[k] <- f('IM', NULL, isolated_0)
        Out1$R_isolated[k] <-  f('NI', 'R',  isolated_0)
        Out1$RE_isolated[k] <-  f('E', 'R',  isolated_0)
        Out1$V1_isolated[k] <-  f('NI', 'V1',  isolated_0)
        Out1$V2_isolated[k] <-  f('NI', 'V2',  isolated_0)
        Out1$V1E_isolated[k] <-  f('E', 'V1',  isolated_0)
        Out1$V2E_isolated[k] <-  f('E', 'V2',  isolated_0)

        absent = (infection_status_0 %in% c('IS', 'IC', 'D') | isolated_0)

        ####
        #note that *for now* I am treating between-shift floaters as present at
        #a deterministic 1/3 or 1/2, as the case may be
        ####

        Out1$n_scheduled[k] = sum(agent_presence)
        Out1$n_absent[k] = sum(agent_presence * absent)
        Out1$qn_scheduled[k] = sum(quantitative_presence)
        Out1$qn_absent[k] = sum(quantitative_presence * absent)
        Out1$new_infections[k] = sum(NI_to_E_community + NI_to_E)
        Out1$new_symptomatic_infections[k] = sum(IP_to_IM) #pause
        Out1$doses[k] = doses
        Out1$tests[k] = tests_performed

        Out1
}
