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

#TBD: Add fraction_boosted, boosting_probability_per_shift = .05 to actual
#pass-through

#TBD: I'm pretty sure this is failing to push all R -> B upon vaccination
    #TBD: Check if this is still true

source('omicron-waning-functions.R')

unisolation_fn = function(agents, start_time) {
    #add full proper parameter list later
    isolation_duration = 5 #kludge

    x_un_Isol = (
        agents$isolated &
        start_time - agents$time_isolated >= isolation_duration &
        agents$infection_status %in% c('NI', 'E', 'IA')
    )
    #this implies that if they have had symptoms, they must be recovered.
    #crude, but possibly tolerable
    #TBD (eventually): is there any estimate available on the probability of
    #having recovered from symptoms (for 24 hours) but not infection?
    agents$isolated[x_un_Isol] = FALSE
    agents
}

isolation_fn = function(agents, start_time, rational_testing, testing_rate,
                        fractional_test_carried, N, IA_FNR, IP_FNR, IM_FNR, FPR,
                        agent_presence) {
    #convenience and efficiency
    isolated = agents$isolated
    infection_status = agents$infection_status

    if(sum(testing_rate) > 0) {        
        if(max(testing_rate) == 1) { #Must use max(testing_rate) because at-work
                                     #testing can't happen for people not
                                     #currently at work
                testing_mask = agent_presence #for exact comparison purposes
        } else if(rational_testing) {
            #TBD: Look at this carefully, to be sure it's doing what it's
            #supposed to
            #In particular, should the ordering be done this way? Or should
            #ordering be done only among those who are actually present?
            indices = order(agents$time_tested, sample(N)) #second parameter
                                                           #randomizes ties
            eligible = (
                infection_status %in% c('NI', 'E', 'IA', 'IP', 'IM') &
                !isolated &
                agent_presence
            ) #NB: Note that this tacitly assumes at-work testing,
              #not at-home testing upon feeling sick
              #This may be redundant with how testing_rate is defined?
            indices = indices[eligible[indices]] 
            theoretical_number_of_tests = testing_rate * sum(agent_presence) +
                    fractional_test_carried
            number_of_tests = min(floor(theoretical_number_of_tests),
                    length(indices))
            fractional_test_carried = theoretical_number_of_tests -
                    number_of_tests
            testing_mask = (1:N) %in% indices[1:number_of_tests]
        } else {
            testing_mask = rbinom(N, 1, testing_rate) == 1
        }

        agents$time_tested[testing_mask] = start_time

        #TBD: Consider merging all of the below into a "probability of
        #detection" function

        # isolate
        # true positives
        #TBD: !isolated here and following should be redundant
        #should it be eliminated?
        Ix_to_Isol = (!isolated & testing_mask & (
                            (infection_status == 'IA' &
                                    rbinom(N, 1, 1 - IA_FNR)) |
                            (infection_status == 'IP' &
                                    rbinom(N, 1, 1 - IP_FNR)) |
                            (infection_status == 'IM' &
                                    rbinom(N, 1, 1 - IM_FNR))
                    ))
        
        # false positives
        # Note that FPR is currently assumed to be constant, but changing
        # the code for this would not be hard.
        NI_to_Isol = ((infection_status == 'NI' & !isolated) &
                        testing_mask & (rbinom(N, 1, FPR)))
        
        # Right, but for the wrong reason
        # (Presumably, our odds of detecting the exposed should not be
        #  be *lower* than our odds of "detecting" susceptibles.)
        xE_to_Isol = ((infection_status == 'E' & !isolated) &
                        testing_mask & (rbinom(N, 1, FPR)))

        #Actual transfer to isolation
        x_to_Isol = (Ix_to_Isol | NI_to_Isol | xE_to_Isol)

        agents$isolated[x_to_Isol] = TRUE
        agents$time_isolated[x_to_Isol] = start_time

        # Assuming for now that isolated do not get exposed.
        # Something similar to lambda (but smaller) may be appropriate if
        # isolation (of those (falsely) detected) is imperfect.
    }
    list(agents = agents, fractional_test_carried = fractional_test_carried)
}

vaccinate = function(agents, N, vaccination_rate, vaccination_interval,
                     start_time, end_time, boosting_rate,
                     infection_0, immune_status_0, vax_status_0, isolated_0,
                     immunity_0) {
    #vaccinate
    if(sum(vaccination_rate) > 0) {
        ####
        #modifying for facility model -- never mind, doesn't need modification!
        #all that needs changing is the definition of rates to vectors instead
        #of scalars! neat!
        #TBD: Check that this is actually implemented correctly
        ####
        # We are ignoring immune boosting (or symptom worsening) effects of
        # vaccination on the already infected for now => only S can
        # *effectively* be vaccinated.
        #TBD: Check that we aren't accidentally "retroactively" vaccinating
        #people upon recovery (and possibly demoting them from recent R to
        #older B)
        
        # For some historical scenarios, it might be nice to make
        # vaccination age-dependent, but this is low priority given our
        # primary focus on the present and future.
        #

        S_to_V1 = ((infection_0 == 'NI' &
                        immune_status_0 == 'FS' &
                        vax_status_0 == 'NV' & !isolated_0) &
                    (rbinom(N, 1, vaccination_rate)))
        # Following line tacitly assumes the times that they could get a 1st
        # dose & times they could get a 2nd dose are the same.
        #TBD: again, this should eventually be changed to better handle
        #people starting the run in V1 (when that becomes possible) and needing
        #to advance to V2, even if there isn't a vaccination intervention.
        #But that is not necessary now.
        V1_to_V2 = ((infection_0 == 'NI' &
                        immune_status_0 == 'V1' &
                        vax_status_0 == 'V1' & !isolated_0) &
                    ((end_time - agents$time_V1) > vaccination_interval) &
                    (vaccination_rate > 0))

        agents$time_V1[S_to_V1] = runif(sum(S_to_V1), start_time, end_time)
        #TBD(long_term): why not just drop the constant referencing of agents$
        #and just use these as vectors?
        agents$previous_immunity[S_to_V1] = immunity_0[S_to_V1] #technically not
                                                                #ideal, but safe
                #net_symptomatic_protection(
                #agents[S_to_V1,], agents$time_V1[S_to_V1])
        agents$time_last_immunity_event[S_to_V1] = agents$time_V1[S_to_V1]
        #because immune_status has not been updated yet, this is still valid
        agents$immune_status[S_to_V1] = 'V1'
        agents$vax_status[S_to_V1] = 'V1'

        agents$time_V2[V1_to_V2] = runif(sum(V1_to_V2), start_time, end_time)
        agents$previous_immunity[V1_to_V2] = immunity_0[V1_to_V2] #technically...
                #net_symptomatic_protection(
                #agents[V1_to_V2,], agents$time_V2[V1_to_V2])
        #because immune_status has not been updated yet, this is still valid
        agents$time_last_immunity_event[V1_to_V2] = agents$time_V2[V1_to_V2]
        agents$immune_status[V1_to_V2] = 'V2'
        agents$vax_status[V1_to_V2] = 'V2'


        #boosting via shots earlier than boosters
        R_to_B_via_V1 = ((infection_0 == 'NI' &
                immune_status_0 == 'R' & vax_status_0 == 'NV' &
                !isolated_0) & (rbinom(N, 1, vaccination_rate)))
        R_to_B_via_V2 = ((infection_0 == 'NI' &
                immune_status_0 == 'R' & vax_status_0 == 'V1' &
                !isolated_0) & (
                (end_time - agents$time_V1) > vaccination_interval) &
                        (vaccination_rate > 0))
    } else {
        R_to_B_via_V1 = FALSE
        R_to_B_via_V2 = FALSE
    }

    x_to_B_on_time = ((infection_0 == 'NI' &
            vax_status_0 == 'V2' & !isolated_0) &
            (end_time - agents$time_V2 > 152) & agents$boosting_on_time)

    #boost
    #TBD: For now, going to assume no boosting during infection
    #TBD: Again, check that retroactive vaccination does not occur
    #(or at least does not steal people from recent R to older B)
    if(sum(boosting_rate) > 0) {
        #time_V2 is only set when receiving second dose
        #so is still valid in R or B (from R + V1 or V2)
        x_to_B_late = ((infection_0 == 'NI' &
                vax_status_0 == 'V2' & !isolated_0) &
                (end_time - agents$time_V2 > 152 + 1) & #5 months
                (rbinom(N, 1, boosting_rate)))
    } else {
        x_to_B_late = FALSE
    }

    x_to_B_any = R_to_B_via_V1 | R_to_B_via_V2 | x_to_B_on_time | x_to_B_late
    agents$time_B[x_to_B_any] = runif(sum(x_to_B_any), start_time, end_time)
            #previous line is not perfect, but has advantages
    agents$previous_immunity[x_to_B_any] = immunity_0[x_to_B_any] #technically...
            #net_symptomatic_protection(
            #agents[x_to_B_any,], agents$time_B[x_to_B_any])
            #because immune_status has not been updated yet, this is still valid
    agents$time_last_immunity_event[x_to_B_any] = agents$time_B[x_to_B_any]
    #TBD(long_term): why not just drop the constant referencing of agents$
    #and just use these as vectors?
    agents$immune_status[x_to_B_any] = 'B'
    agents$vax_status[x_to_B_on_time] = 'B'
    agents$vax_status[x_to_B_late] = 'B'
    agents$vax_status[R_to_B_via_V1] = 'V1'
    agents$vax_status[R_to_B_via_V2] = 'V2'


    #TBD: Add time_B to AgentGen
    #TBD: Handling of ??
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

#TBD: arguably, infection_0 should *not* be used here
#Figure this out
progress_infection = function(agents, N, start_time, end_time, symptoms_0) {
    xE_to_I = ((agents$infection_status == 'E') &
                ((end_time - agents$time_E) > agents$duration_E))
    #TBD (possibly): Again, might want to simplify this
    xE_to_IP = xE_to_I & agents$symptomatic & rbinom(N, 1, symptoms_0)
    xE_to_IA = xE_to_I & !xE_to_IP

    IP_to_IM = ((agents$infection_status == 'IP') &
                ((end_time - agents$time_IP) > agents$duration_IP))
    IA_to_R =  ((agents$infection_status == 'IA') &
                ((end_time - agents$time_IA) > agents$duration_IA))
    IM_to_x =  ((agents$infection_status == 'IM') &
                ((end_time - agents$time_IM) > agents$duration_IM))
    #TBD (eventually): Account for reduced chance of severe disease
    #conditional on symptomatic
    IM_to_IS = IM_to_x & agents$severe
    IM_to_R =  IM_to_x & !(agents$severe)   #TBD (eventually): account for
                                            #possibility of greater immunity if
                                            #infected after vaccination,
                                            #previous infection, etc.
    IS_to_x = ((agents$infection_status == 'IS') &
                ((end_time - agents$time_IS) > agents$duration_IS))
    IS_to_IC = IS_to_x & agents$critical
    IS_to_R =  IS_to_x & !(agents$critical)
    IC_to_x = ((agents$infection_status == 'IC') &
                ((end_time - agents$time_IC) > agents$duration_IC))
    IC_to_D = IC_to_x & agents$death
    IC_to_R = IC_to_x & !(agents$death)

    x_to_R = IA_to_R | IM_to_R | IS_to_R | IC_to_R

    list(agents = agents, xE_to_IA = xE_to_IA, xE_to_IP = xE_to_IP,
         IP_to_IM = IP_to_IM, IM_to_IS = IM_to_IS, IS_to_IC = IS_to_IC,
         IC_to_D = IC_to_D, x_to_R = x_to_R, IA_to_R = IA_to_R,
         IM_to_R = IM_to_R, IS_to_R = IS_to_R, IC_to_R = IC_to_R)
    #redundant in order to simplify further handling
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
    #TBD: clean up
    #V1_susceptibility = get('V1_susceptibility', vaccine_parameters)
    #V2_susceptibility = get('V2_susceptibility', vaccine_parameters)
    vaccination_interval = vaccination_interval #get('vaccination_interval', vaccine_parameters)
    #R_susceptibility = get('R_susceptibility', waning_parameters) 
    #B_susceptibility = get('B_susceptibility', vaccine_parameters)

    end_time = 0 # End of the last shift before simulation starts

    fractional_test_carried = 0

    # Move people through time
    for(k in 1:steps) {
        contacts = get(schedule[k], contacts_list)
        lambda = get(schedule[k], lambda_list)
        step_length = get(schedule[k], step_length_list)
        testing_rate = get(schedule[k], testing_rate_list)
        vaccination_rate = get(schedule[k], vaccination_rate_list)
        ####
        #new parameter
        ####
        agent_presence = get(schedule[k], agent_presence_list)
        quantitative_presence = get(schedule[k], quantitative_presence_list)

        start_time = end_time 
        end_time = start_time + step_length


        # un-isolate
        #
        # Placed before isolation so that a positive test will kick them
        # outside the following "if" block, because someone could hit their
        # duration on their day off and start contacting other people
        # (outside of work) again.
        #
        # Note that it might be preferable to have the code for isolation notice
        # when someone is unisolated and immediately reisolated, and "merge" the
        # isolations. But that is not necesary at this time.
        #print('deisolation')
        agents = unisolation_fn(agents, start_time)
        #and now, (re)isolate
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
        vax_status_0 = agents$vax_status #TBD: Make sure I don't use this incorrectly!
        
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

        #putting the break here?, even though the above could logically be
        #incorporated into the infect function, to keep the saved value
        #conveniently out here
        #TBD: check if I ended up doing this

        #TBD: fix below block
        #technically, this should take account of vaccination during a shift,
        #but the change should be negligible
        #(and, conversely, technically the possibility of infection should take
        #account of waning over the course of the shift) -- but again, this is
        #negligible in practice
        p_infection = 1 - exp(-force_of_infection * susceptibility_0)
        #TBD: delete below once confirmed working
        #p_infection = 1 - exp(-force_of_infection)
        #p_infection_V1 = 1 - exp(-force_of_infection * V1_susceptibility)
        #p_infection_V2 = 1 - exp(-force_of_infection * V2_susceptibility)
        #p_infection_R = 1 - exp(-force_of_infection * R_susceptibility)
        #p_infection_B = 1 - exp(-force_of_infection * B_susceptibility)

        #TBD: move or otherwise fix this?
        # Putting the process of infection on hold a moment, to figure out who
        # among the already-infected needs to progress along their course of
        # infection (or recover), before the shift is over.
        #
        # Ideally, we probably want to incorporate transitions out of
        # infectiousness into calculation of transmission potentials, but later.
        #TBD: this needs to use symptoms_0
        pil = progress_infection(agents, N, start_time, end_time, symptoms_0)
        agents = pil$agents
        xE_to_IA = pil$xE_to_IA
        xE_to_IP = pil$xE_to_IP
        IP_to_IM = pil$IP_to_IM
        IM_to_IS = pil$IM_to_IS
        IS_to_IC = pil$IS_to_IC
        IC_to_D = pil$IC_to_D
        x_to_R = pil$x_to_R
        IA_to_R = pil$IA_to_R
        IM_to_R = pil$IM_to_R
        IS_to_R = pil$IS_to_R
        IC_to_R = pil$IC_to_R

        #For simplicity, we'll do community transmission first, i.e.,
        #if someone would be infected from community transmission and from
        #within-company transmission in the same shift, community transmission
        #wins. In the long run, this may be changed to be probabilistic.
        #In practice, though, it's unlikely to matter much -- most scenarios
        #will have few if any shifts in which both probabilities are
        #non-negligible.
        #(In fact, none in the current version, except maybe for all-shift
        #floaters (check).)


        
        NI_to_E_community = ((agents$infection_status == 'NI') &
                !isolated_0 & (
                rbinom(N, 1, 1 - exp(-lambda * susceptibility_0))
                                #TBD: delete below once this is confirmed working
                                #(agents$immune_status == 'FS' & rbinom(N, 1, 1 - exp(-lambda))) |
                                #(agents$immune_status == 'V1' & rbinom(N, 1, 1 - exp(-lambda * V1_susceptibility))) |
                                #(agents$immune_status == 'V2' & rbinom(N, 1, 1 - exp(-lambda * V2_susceptibility))) |
                                #(agents$immune_status == 'R' & rbinom(N, 1, 1 - exp(-lambda * R_susceptibility))) |
                                #(agents$immune_status == 'B' & rbinom(N, 1, 1 - exp(-lambda * B_susceptibility)))
                            ))
        #if(any(is.na(infection_protection(agents, start_time)))) {
        #    cat(start_time, end_time, '\n')
        #    print(agents[is.na(infection_protection(agents, start_time)),])
        #}
        #print('Never mind')

        #Old contact tracing code that needs to be revised and reactivated.
        #agents$infector_ID[S_to_E_community] = -1
        #agents$infector_state[S_to_E_community] = "UNKNOWN"
        agents$infection_status[NI_to_E_community] = 'E'

        #should ideally be a truncated exponential, but this is
        #adequate for now

        #cat(sum(NI_to_E_community), start_time, end_time,'\n')
        .GlobalEnv[['NI_to_E_community']] = NI_to_E_community
        .GlobalEnv[['start_time']] = start_time
        .GlobalEnv[['end_time']] = end_time
        agents$time_E[NI_to_E_community] = runif(sum(NI_to_E_community),
                                                 start_time, end_time)

        # Note that while we are now using continuous transition times, the
        # probability of infection is still based on infection status at the
        # start of a shift. This might be changed in the future.
        # Another possibility may even be to use a priority queue or some such,
        # but that's a matter for another time.

        #TBD (eventually): simplify
        #TBD: fix below block using susceptibility
        NI_to_E = (
            (agents$infection_status == 'NI') & #now redefined
            rbinom(N, 1, p_infection) &
            !isolated_0 #TBD: Is this really new!?!?!?
                    #TBD:delete below block once this is confirmed working
                    #(agents$immune_status == 'FS' & (rbinom(N, 1, p_infection) > 0)) |
                    #(agents$immune_status == 'V1' & (rbinom(N, 1, p_infection_V1) > 0)) |
                    #(agents$immune_status == 'V2' & (rbinom(N, 1, p_infection_V2) > 0)) |
                    #(agents$immune_status == 'R' & (rbinom(N, 1, p_infection_R) > 0)) |
                    #(agents$immune_status == 'B' & (rbinom(N, 1, p_infection_B) > 0))
        )

        if(sum(NI_to_E) > 0) { #necessitated by weird behavior of apply
                              #when given an empty matrix
            potential_infectors = foi_contributions[,NI_to_E]
            if(sum(NI_to_E) == 1) { # in this case, potential_infectors is a
                                    # vector instead of a matrix
                infectors = sample(1:N, 1, prob = potential_infectors /
                                            sum(potential_infectors))
            } else {
                infectors = apply(potential_infectors, 2,
                                  function(x) sample(1:N, 1, prob = x / sum(x)))
            }

            agents$infector_ID[NI_to_E] = agents$ID[infectors]
            agents$infector_state[NI_to_E] = paste(
                    agents$infection_status[infectors],
                    agents$immune_status[infectors],
                    agents$vax_status[infectors], sep = '_')
            #above is kludgey
            #but we're not currently using this for anything anyway
            agents$infection_status[NI_to_E] = 'E'
            #should ideally be a truncated exponential, but this is
            #adequate for now
            agents$time_E[NI_to_E] = runif(sum(NI_to_E), start_time, end_time)
        }


        agents$infection_status[xE_to_IA] = 'IA'
        agents$infection_status[xE_to_IP] = 'IP'
        agents$infection_status[x_to_R] = 'NI'
        #a lot of this should probably be moved inside progress_infection
        agents$previous_immunity[x_to_R] = net_symptomatic_protection(
                agents[x_to_R,], agents$time_last_immunity_event[x_to_R]) 
        agents$immune_status[x_to_R] = 'R'
        agents$infection_status[IP_to_IM] = 'IM'
        agents$infection_status[IM_to_IS] = 'IS'
        agents$infection_status[IS_to_IC] = 'IC'
        agents$infection_status[IC_to_D] = 'D'
        #TBD (eventually): figure out whether death should be considered an
        #immune status (probably not)

        agents$time_IA[xE_to_IA] = agents$time_E[xE_to_IA] +
                agents$duration_E[xE_to_IA]
        agents$time_IP[xE_to_IP] = agents$time_E[xE_to_IP] +
                agents$duration_E[xE_to_IP]
        agents$time_isolated[xE_to_IP & isolated_0] =
                agents$time_IP[xE_to_IP & isolated_0]
        #TBD: above is awkward, and should be in an isolation function
        #likewise below, and in the progression function
        agents$time_IM[IP_to_IM] = agents$time_IP[IP_to_IM] +
                agents$duration_IP[IP_to_IM]
        agents$time_IS[IM_to_IS] = agents$time_IM[IM_to_IS] +
                agents$duration_IM[IM_to_IS]
        agents$time_IC[IS_to_IC] = agents$time_IS[IS_to_IC] +
                agents$duration_IS[IS_to_IC]
        agents$time_D[IC_to_D] = agents$time_IC[IC_to_D] +
                agents$duration_IC[IC_to_D]

        #TBD (eventually): Simplify below? (with a "time last progression"
        #variable?)
        #below should really be moved (along with some of the above) into
        #progress_infection
        agents$time_R[IA_to_R] = agents$time_IA[IA_to_R] +
                agents$duration_IA[IA_to_R]
        agents$time_R[IM_to_R] = agents$time_IM[IM_to_R] +
                agents$duration_IM[IM_to_R]
        agents$time_R[IS_to_R] = agents$time_IS[IS_to_R] +
                agents$duration_IS[IS_to_R]
        agents$time_R[IC_to_R] = agents$time_IC[IC_to_R] +
                agents$duration_IC[IC_to_R]
        agents$time_last_immunity_event[x_to_R] = agents$time_R[x_to_R]

        #TBD: We still need to recalculate durations for repeats of the same
        #event (now possible)

        #"Out1" records the sum of individuals in each state at time k
        #(i.e., during time from time=0 to time=nTime1)
        #this allows ploting trajectories for each state in one simulation.
        #It is anticipated that a future version may record additional
        #information at each time step.
            #Actually, an development version did, but it was recording GB per
            #1000-run intervention, even after gzipping, so it has been dropped
            #for now (long term, we may wish to find a way to do this more
            #efficiently; in theory, this should probably not be that massive).
            #Note: One copy of agents is ~32k, so ~32 should be 1 MB ->
            #one run of 270 steps should be about 3 MB, so 1000 runs should be
            #about 3 GB *if uncompressed*. Perhaps the better question is why it
            #compresses poorly.
            #One might also wonder if selectively stripping attributes might
            #improve that? But that is a question for later.
        #"agents" shows demographic characteristics of all individuals in the
        #population and their infection status at time nTime1

    #TBD (soon): Remove these, once they're no longer necessary
        #maybe not so soon given issues with the size of agentss
    #TBD (ASAP): move this to an update_Out1 function
    #NB: TRUE == 1 for the purpose of summation
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

