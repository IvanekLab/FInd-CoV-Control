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


ABM <- function(agents, contacts_list, lambda_list, schedule,
                virus_parameters, testing_parameters, vaccine_parameters, scenario_parameters,
                steps, step_length_list, testing_rate_list, vaccination_rate_list) {

    N <-nrow(agents)

    Out1 <- data.frame(S = rep(0, steps),
                       E = rep(0, steps),
                       IA = rep(0, steps),
                       IP = rep(0, steps),
                       IM = rep(0, steps),
                       IS = rep(0, steps),
                       IC = rep(0, steps),
                       R = rep(0, steps),
                       D = rep(0, steps),
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
                       V1_isolated = rep(0, steps),
                       V2_isolated = rep(0, steps),
                       V1E_isolated = rep(0, steps),
                       V2E_isolated = rep(0, steps)
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
    V1_susceptibility = get('V1_susceptibility', vaccine_parameters)
    V2_susceptibility = get('V2_susceptibility', vaccine_parameters)
    vaccination_interval = get('vaccination_interval', vaccine_parameters)


    end_time = 0 # End of the last shift before simulation starts

    fractional_test_carried = 0

    # Move people through time
    for(k in 1:steps) {
        contacts = get(schedule[k], contacts_list)
        lambda = get(schedule[k], lambda_list)
        step_length = get(schedule[k], step_length_list)
        testing_rate = get(schedule[k], testing_rate_list)
        vaccination_rate = get(schedule[k], vaccination_rate_list)

        start_time = end_time
        end_time = start_time + step_length

        #vaccinate
        if(sum(vaccination_rate) > 0) {
            # We are ignoring immune boosting (or symptom worsening) effects of
            # vaccination on the already infected for now => only S can
            # *effectively* be vaccinated.
            
            # For some historical scenarios, it might be nice to make
            # vaccination age-dependent, but this is low priority given our
            # primary focus on the present and future.
            S_to_V1 = ((agents$state == 'S' & !(agents$isolated)) &
                       (rbinom(N, 1, vaccination_rate)))
            # Following line tacitly assumes the times that they could get a 1st
            # dose & times they could get a 2nd dose are the same.
            V1_to_V2 = ((agents$state == 'V1' & !(agents$isolated)) &
                        ((end_time - agents$time_V1) > vaccination_interval) &
                        (vaccination_rate > 0)) 
            agents$time_V1[S_to_V1] = runif(sum(S_to_V1), start_time, end_time)
            agents$time_V2[V1_to_V2] = runif(sum(V1_to_V2), start_time, end_time)
            agents$state[S_to_V1] = 'V1'
            agents$state[V1_to_V2] = 'V2'
        }

        # un-isolate
        #
        # Placed before isolation so that a positive test will kick them
        # outside the following "if" block, because someone could hit their
        # duration on their day off and start contacting other people
        # (outside of work) again.
        x_un_Isol = ((agents$isolated) & ((start_time - agents$time_isolated) >= isolation_duration))
        agents$isolated[x_un_Isol] = FALSE

        if(sum(testing_rate) > 0) {
            
            # Currently using a flag for whether someone is isolated or not, but
            # could use new compartments (e.g. state = Isolated_IA, instead of
            # state = IA and Isolated = True

            if(testing_rate == 1) {
                testing_mask = rep(TRUE, N) # for exact comparison purposes
            } else if(rational_testing) {
                indices = order(agents$time_tested, sample(N)) #second parameter randomizes ties
                eligible = ((agents$state %in% c('S', 'E', 'IA', 'IP', 'IM', 'R', 'V1', 'V2', 'V1E', 'V2E')) & !(agents$isolated)) #ideally, should need to be on the shift in question, but that's a matter for a later version of the code (when there are multiple shifts)
                indices = indices[eligible[indices]] 
                theoretical_number_of_tests = testing_rate * N + fractional_test_carried
                number_of_tests = min(floor(theoretical_number_of_tests), length(indices))
                fractional_test_carried = theoretical_number_of_tests - number_of_tests
                testing_mask = (1:N) %in% indices[1:number_of_tests]
            } else {
                testing_mask = rbinom(N, 1, testing_rate) == 1
            }

            agents$time_tested[testing_mask] = start_time


            # isolate
            # true positives
            IA_to_Isol = ((agents$state == 'IA' & !(agents$isolated)) &
                          testing_mask & (rbinom(N, 1, 1 - IA_FNR)))
            IP_to_Isol = ((agents$state == 'IP' & !(agents$isolated)) &
                          testing_mask & (rbinom(N, 1, 1 - IP_FNR)))
            IM_to_Isol = ((agents$state == 'IM' & !(agents$isolated)) &
                          testing_mask & (rbinom(N, 1, 1 - IM_FNR)))
            
            # false positives
            # Note that FPR is currently assumed to be constant, but changing
            # the code for this would not be hard.
            S_to_Isol = ((agents$state == 'S' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))
            R_to_Isol = ((agents$state == 'R' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))
            V1_to_Isol = ((agents$state == 'V1' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))
            V2_to_Isol = ((agents$state == 'V2' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))

            # Right, but for the wrong reason
            # (Presumably, our odds of detecting the exposed should not be
            #  be *lower* than our odds of "detecting" susceptibles.)
            E_to_Isol = ((agents$state == 'E' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))

            #Actual transfer to isolation
            x_to_Isol = (IA_to_Isol | IP_to_Isol | IM_to_Isol |
                         S_to_Isol | R_to_Isol | V1_to_Isol | V2_to_Isol |
                         E_to_Isol)

            agents$isolated[x_to_Isol] = TRUE
            agents$time_isolated[x_to_Isol] = start_time
    
            # Assuming for now that isolated do not get exposed.
            # Something similar to lambda (but smaller) may be appropriate if
            # isolation (of those (falsely) detected) is imperfect.
        }
    
        infectiousness = ((agents$state == 'IA' & !(agents$isolated)) * p_trans_IA +
                          (agents$state == 'IP' & !(agents$isolated)) * p_trans_IP +
                          (agents$state == 'IM' & !(agents$isolated)) * p_trans_IM)
        foi_contributions = contacts * infectiousness
        force_of_infection = colSums(foi_contributions)
        p_infection = 1 - exp(-force_of_infection)
        p_infection_V1 = 1 - exp(-force_of_infection * V1_susceptibility)
        p_infection_V2 = 1 - exp(-force_of_infection * V2_susceptibility)

        # Putting the process of infection on hold a moment, to figure out who
        # among the already-infected needs to progress along their course of
        # infection (or recover), before the shift is over.

        E_to_I = ((agents$state == 'E') &
                  ((end_time - agents$time_E) > agents$duration_E))
        E_to_IP = E_to_I & agents$symptomatic
        E_to_IA = E_to_I & !(agents$symptomatic)
        V1E_to_I = ((agents$state == 'V1E') &
                    ((end_time - agents$time_E) > agents$duration_E))
        V1E_to_IP = V1E_to_I & agents$V1_symptomatic
        V1E_to_IA = V1E_to_I & !(agents$V1_symptomatic)
        V2E_to_I = ((agents$state == 'V2E') &
                    ((end_time - agents$time_E) > agents$duration_E))
        V2E_to_IP = V2E_to_I & agents$V2_symptomatic
        V2E_to_IA = V2E_to_I & !(agents$V2_symptomatic)
        IP_to_IM = ((agents$state == 'IP') &
                    ((end_time - agents$time_IP) > agents$duration_IP))
        IA_to_R =  ((agents$state == 'IA') &
                    ((end_time - agents$time_IA) > agents$duration_IA))
        IM_to_x =  ((agents$state == 'IM') &
                    ((end_time - agents$time_IM) > agents$duration_IM))
        IM_to_IS = IM_to_x & agents$severe
        IM_to_R =  IM_to_x & !(agents$severe)
        IS_to_x = ((agents$state == 'IS') &
                    ((end_time - agents$time_IS) > agents$duration_IS))
        IS_to_IC = IS_to_x & agents$critical
        IS_to_R =  IS_to_x & !(agents$critical)
        IC_to_x = ((agents$state == 'IC') &
                    ((end_time - agents$time_IC) > agents$duration_IC))
        IC_to_D = IC_to_x & agents$death
        IC_to_R = IC_to_x & !(agents$death)

        x_to_R = IA_to_R | IM_to_R | IS_to_R | IC_to_R
        
        # Now back to infecting new people.

        #For simplicity, we'll do community transmission first, i.e.,
        #if someone would be infected from community transmission and from
        #within-company transmission in the same shift, community transmission
        #wins. In the long run, this may be changed to be probabilistic.
        #In practice, though, it's unlikely to matter much -- most scenarios
        #will have few if any shifts in which both probabilities are
        #non-negligible.
        #(In fact, none in the current version.)

        S_to_E_community = ((agents$state == 'S') & !(agents$isolated) &
                            (rbinom(N, 1, 1 - exp(-lambda))))
        V1_to_V1E_community = ((agents$state == 'V1') & !(agents$isolated) &
                               (rbinom(N, 1, 1 - exp(-lambda * V1_susceptibility))))
        V2_to_V2E_community = ((agents$state == 'V2') & !(agents$isolated) &
                               (rbinom(N, 1, 1 - exp(-lambda * V2_susceptibility))))
        #Old contact tracing code that needs to be revised and reactivated.
        #agents$infector_ID[S_to_E_community] = -1
        #agents$infector_state[S_to_E_community] = "UNKNOWN"
        agents$state[S_to_E_community] = 'E'
        agents$state[V1_to_V1E_community] = 'V1E'
        agents$state[V2_to_V2E_community] = 'V2E'
        #should ideally be a truncated exponential, but this is
        #adequate for now
        agents$time_E[S_to_E_community] = runif(sum(S_to_E_community),
                                                start_time, end_time)
        agents$time_E[V1_to_V1E_community] = runif(sum(V1_to_V1E_community),
                                                start_time, end_time)
        agents$time_E[V2_to_V2E_community] = runif(sum(V2_to_V2E_community),
                                                start_time, end_time)

        # Note that while we are now using continuous transition times, the
        # probability of infection is still based on infection status at the
        # start of a shift. This might be changed in the future.
        # Another possibility may even be to use a priority queue or some such,
        # but that's a matter for another time.
        S_to_E = (agents$state == 'S') & (rbinom(N, 1, p_infection) > 0)
        V1_to_V1E = (agents$state == 'V1') & (rbinom(N, 1, p_infection_V1) > 0)
        V2_to_V2E = (agents$state == 'V2') & (rbinom(N, 1, p_infection_V2) > 0)

        if(sum(S_to_E) > 0) { #necessitated by weird behavior of apply
                              #when given an empty matrix
            potential_infectors = foi_contributions[,S_to_E]
            if(sum(S_to_E) == 1) { # in this case, potential_infectors is a
                                   # vector instead of a matrix
                infectors = sample(1:N, 1, prob = potential_infectors /
                                            sum(potential_infectors))
            } else {
                infectors = apply(potential_infectors, 2,
                                  function(x) sample(1:N, 1, prob = x / sum(x)))
            }

            agents$infector_ID[S_to_E] = agents$ID[infectors]
            agents$infector_state[S_to_E] = agents$state[infectors]
            agents$state[S_to_E] = 'E'
            #should ideally be a truncated exponential, but this is
            #adequate for now
            agents$time_E[S_to_E] = runif(sum(S_to_E), start_time, end_time)
        }

        if(sum(V1_to_V1E) > 0) { #necessitated by weird behavior of apply
                              #when given an empty matrix
            potential_infectors = foi_contributions[,V1_to_V1E]
            if(sum(V1_to_V1E) == 1) { # in this case, potential_infectors is a
                                      # vector instead of a matrix
                infectors = sample(1:N, 1, prob = potential_infectors /
                                            sum(potential_infectors))
            } else {
                infectors = apply(potential_infectors, 2,
                                  function(x) sample(1:N, 1, prob = x / sum(x)))
            }

            agents$infector_ID[V1_to_V1E] = agents$ID[infectors]
            agents$infector_state[V1_to_V1E] = agents$state[infectors]
            agents$state[V1_to_V1E] = 'V1E'
            #should ideally be a truncated exponential, but this is
            #adequate for now
            agents$time_E[V1_to_V1E] = runif(sum(V1_to_V1E), start_time, end_time)
        }

        if(sum(V2_to_V2E) > 0) { #necessitated by weird behavior of apply
                                 #when given an empty matrix
            potential_infectors = foi_contributions[,V2_to_V2E]
            if(sum(V2_to_V2E) == 1) { # in this case, potential_infectors is a
                                      # vector instead of a matrix
                infectors = sample(1:N, 1, prob = potential_infectors /
                                            sum(potential_infectors))
            } else {
                infectors = apply(potential_infectors, 2,
                                  function(x) sample(1:N, 1, prob = x / sum(x)))
            }

            agents$infector_ID[V2_to_V2E] = agents$ID[infectors]
            agents$infector_state[V2_to_V2E] = agents$state[infectors]
            agents$state[V2_to_V2E] = 'V2E'
            #should ideally be a truncated exponential, but this is
            #adequate for now
            agents$time_E[V2_to_V2E] = runif(sum(V2_to_V2E), start_time, end_time)
        }


        agents$state[E_to_IA] = 'IA'
        agents$state[E_to_IP] = 'IP'
        agents$state[V1E_to_IA] = 'IA'
        agents$state[V1E_to_IP] = 'IP'
        agents$state[V2E_to_IA] = 'IA'
        agents$state[V2E_to_IP] = 'IP'
        agents$state[x_to_R] = 'R'
        agents$state[IP_to_IM] = 'IM'
        agents$state[IM_to_IS] = 'IS'
        agents$state[IS_to_IC] = 'IC'
        agents$state[IC_to_D] = 'D'

        agents$time_IA[E_to_IA] = agents$time_E[E_to_IA] + agents$duration_E[E_to_IA]
        agents$time_IP[E_to_IP] = agents$time_E[E_to_IP] + agents$duration_E[E_to_IP]
        agents$time_IA[V1E_to_IA] = agents$time_E[V1E_to_IA] + agents$duration_E[V1E_to_IA]
        agents$time_IP[V1E_to_IP] = agents$time_E[V1E_to_IP] + agents$duration_E[V1E_to_IP]
        agents$time_IA[V2E_to_IA] = agents$time_E[V2E_to_IA] + agents$duration_E[V2E_to_IA]
        agents$time_IP[V2E_to_IP] = agents$time_E[V2E_to_IP] + agents$duration_E[V2E_to_IP]
        agents$time_IM[IP_to_IM] = agents$time_IP[IP_to_IM] + agents$duration_IP[IP_to_IM]
        agents$time_IS[IM_to_IS] = agents$time_IM[IM_to_IS] + agents$duration_IM[IM_to_IS]
        agents$time_IC[IS_to_IC] = agents$time_IS[IS_to_IC] + agents$duration_IS[IS_to_IC]
        agents$time_D[IC_to_D] = agents$time_IC[IC_to_D] + agents$duration_IC[IC_to_D]
        agents$time_R[IA_to_R] = agents$time_IA[IA_to_R] + agents$duration_IA[IA_to_R]
        agents$time_R[IM_to_R] = agents$time_IM[IM_to_R] + agents$duration_IM[IM_to_R]
        agents$time_R[IS_to_R] = agents$time_IS[IS_to_R] + agents$duration_IS[IS_to_R]
        agents$time_R[IC_to_R] = agents$time_IC[IC_to_R] + agents$duration_IC[IC_to_R]

    #"Out1" records the sum of individuals in each state at time k (i.e., during time from time=1 to time=nTime1)
    #this allows ploting trajectories for each state in one simulation.
    #It is anticipated that a future version may record additional information at each time step.
    #"agents" shows demographic characetristics of all individuals in the population and their infection status at time nTime1

    Out1$S[k] <-  sum(agents$state == "S") #TRUE == 1 for the purpose of summation
    Out1$E[k] <-  sum(agents$state == "E")
    Out1$IA[k] <- sum(agents$state == "IA")
    Out1$IP[k] <- sum(agents$state == "IP")
    Out1$IM[k] <- sum(agents$state == "IM")
    Out1$IS[k] <- sum(agents$state == "IS")
    Out1$IC[k] <- sum(agents$state == "IC")
    Out1$R[k] <-  sum(agents$state == "R")
    Out1$D[k] <-  sum(agents$state == "D")
    Out1$V1[k] <-  sum(agents$state == "V1")
    Out1$V2[k] <-  sum(agents$state == "V2")
    Out1$V1E[k] <-  sum(agents$state == "V1E")
    Out1$V2E[k] <-  sum(agents$state == "V2E")
    Out1$S_isolated[k] <-  sum(agents$state == "S" & agents$isolated)
    Out1$E_isolated[k] <-  sum(agents$state == "E" & agents$isolated)
    Out1$IA_isolated[k] <- sum(agents$state == "IA" & agents$isolated)
    Out1$IP_isolated[k] <- sum(agents$state == "IP" & agents$isolated)
    Out1$IM_isolated[k] <- sum(agents$state == "IM" & agents$isolated)
    Out1$R_isolated[k] <-  sum(agents$state == "R" & agents$isolated)
    Out1$V1_isolated[k] <-  sum(agents$state == "V1" & agents$isolated)
    Out1$V2_isolated[k] <-  sum(agents$state == "V2" & agents$isolated)
    Out1$V1E_isolated[k] <-  sum(agents$state == "V1E" & agents$isolated)
    Out1$V2E_isolated[k] <-  sum(agents$state == "V2E" & agents$isolated)
  }

    Out <- list("Out1" = Out1, "agents" = agents) #create a list of objects to return
  
    return (Out) #return a list of objects
}

