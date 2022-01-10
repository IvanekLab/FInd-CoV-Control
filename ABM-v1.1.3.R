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
                steps, step_length_list, testing_rate_list, vaccination_rate_list,
                waning_parameters) {

    N <-nrow(agents)

    Out1 <- data.frame(S = rep(0, steps),
                       E = rep(0, steps),
                       IA = rep(0, steps),
                       IP = rep(0, steps),
                       IM = rep(0, steps),
                       IS = rep(0, steps),
                       IC = rep(0, steps),
                       R = rep(0, steps),
                       RE = rep(0, steps), #
                       D = rep(0, steps),
                       V1 = rep(0, steps),
                       V2 = rep(0, steps),
                       V1E = rep(0, steps),
                       V2E = rep(0, steps),
                       W = rep(0, steps),   #
                       WE = rep(0, steps),  #
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
                       W_isolated = rep(0, steps),  #
                       WE_isolated = rep(0, steps) #
    )

    agentss = list()

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
    W_susceptibility =  get('W_susceptibility', waning_parameters)
    #as with V1, V2, altered probability of severe disease given infection can be precalculated
    #likewise for waning time
    R_susceptibility = get('R_susceptibility', waning_parameters) # grouping together because they're being added together now (in the "waning" branch first commit
    waning_rate = get('waning_rate', waning_parameters)
    #and again for R



    end_time = 0 # End of the last shift before simulation starts

    fractional_test_carried = 0

    # Move people through time
    #print('ABM loop')
    for(k in 1:steps) {
        #print('step')
        contacts = get(schedule[k], contacts_list)
        lambda = get(schedule[k], lambda_list)
        step_length = get(schedule[k], step_length_list)
        testing_rate = get(schedule[k], testing_rate_list)
        vaccination_rate = get(schedule[k], vaccination_rate_list)

        start_time = end_time
        end_time = start_time + step_length

        #vaccinate
        #print('vaccination')
        if(sum(vaccination_rate) > 0) {
            # We are ignoring immune boosting (or symptom worsening) effects of
            # vaccination on the already infected for now => only S can
            # *effectively* be vaccinated.

            #This may be changing when booster doses are added; it fits somewhat
            #naturally with those.
            
            # For some historical scenarios, it might be nice to make
            # vaccination age-dependent, but this is low priority given our
            # primary focus on the present and future.
            #
            # Note that not including W_to_V[whatever] technically ignores some S -> I -> R -> W [-> V] trajectories
            # But including it would amount to allowing boosters for S -> V1 -> V2 -> W trajectories (among others)
            # So we'll leave it out for now
            # But this is another argument for more cleanly separating multiple aspects of state

            #2021-01-07 For consistency with previous results (for checking that
            #conversion is done correctly), I am for now keeping vaccination as
            #something that only occurs in state "S" (= NI + FS + NV)
            #But this must be fixed to sanely handle boosting!
            #so TBD: fix that (once conversion has been confirmed to be handled
            #correctly).
            S_to_V1 = ((agents$infection_status == 'NI' & agents$immune_status == 'FS' & agents$vax_status == 'NV' & !(agents$isolated)) &
                       (rbinom(N, 1, vaccination_rate)))
            # Following line tacitly assumes the times that they could get a 1st
            # dose & times they could get a 2nd dose are the same.
            #TBD: again, this should eventually be changed to better handle
            V1_to_V2 = ((agents$infection_status == 'NI' & agents$immune_status == 'V1' & agents$vax_status == 'V1' & !(agents$isolated)) &
                        ((end_time - agents$time_V1) > vaccination_interval) &
                        (vaccination_rate > 0)) 
            agents$time_V1[S_to_V1] = runif(sum(S_to_V1), start_time, end_time)
            agents$time_V2[V1_to_V2] = runif(sum(V1_to_V2), start_time, end_time)
            agents$immune_status[S_to_V1] = 'V1'
            agents$vax_status[S_to_V1] = 'V1'
            agents$immune_status[V1_to_V2] = 'V2'
            agents$vax_status[V1_to_V2] = 'V2'
        }

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
        x_un_Isol = ((agents$isolated) & ((start_time - agents$time_isolated) >= isolation_duration))
        agents$isolated[x_un_Isol] = FALSE

        #print('testing')
        if(sum(testing_rate) > 0) {
            
            # Currently using a flag for whether someone is isolated or not, but
            # could use new compartments (e.g. state = Isolated_IA, instead of
            # state = IA and Isolated = True

            if(testing_rate == 1) {
                testing_mask = rep(TRUE, N) # for exact comparison purposes
            } else if(rational_testing) {
                indices = order(agents$time_tested, sample(N)) #second parameter randomizes ties
                #eligible = ((agents$state %in% c('S', 'E', 'IA', 'IP', 'IM', 'R', 'RE', 'V1', 'V2', 'V1E', 'V2E', 'W', 'WE')) & !(agents$isolated)) #ideally, should need to be on the shift in question, but that's a matter for a later version of the code (when there are multiple shifts)
                eligible = ((agents$infection_status %in% c('NI', 'E', 'IA', 'IP', 'IM')) & !(agents$isolated)) #ideally, should need to be on the shift in question, but that's a matter for a later version of the code (when there are multiple shifts)
                # Actually, as I am doing these edits for the first commit on
                # the waning branch, there are already multiple shifts on the
                # facility model preliminary code (not incorporated into the
                # repo yet) Once that is in the repo, it may be worth doing a
                # "partial merge" (is that a thing? maybe if I split the changes
                # into two commits) to add that handling to this code, for ease 
                # of consistent future use. But that is not needed now.
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
            Ix_to_Isol = (!(agents$isolated) & testing_mask & (
                                (agents$infection_status == 'IA' & rbinom(N, 1, 1 - IA_FNR)) |
                                (agents$infection_status == 'IP' & rbinom(N, 1, 1 - IP_FNR)) |
                                (agents$infection_status == 'IM' & rbinom(N, 1, 1 - IM_FNR))
                        ))
            
            # false positives
            # Note that FPR is currently assumed to be constant, but changing
            # the code for this would not be hard.
            NI_to_Isol = ((agents$infection_status == 'NI' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))
            
            # Right, but for the wrong reason
            # (Presumably, our odds of detecting the exposed should not be
            #  be *lower* than our odds of "detecting" susceptibles.)
            xE_to_Isol = ((agents$infection_status == 'E' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))

            #Actual transfer to isolation
            x_to_Isol = (Ix_to_Isol | NI_to_Isol | xE_to_Isol)

            agents$isolated[x_to_Isol] = TRUE
            agents$time_isolated[x_to_Isol] = start_time
    
            # Assuming for now that isolated do not get exposed.
            # Something similar to lambda (but smaller) may be appropriate if
            # isolation (of those (falsely) detected) is imperfect.
        }
    
        #print('infectiousness')
        #TBD (possibly): add "relative contagiousness" and "relative
        #susceptibility" dimensions to agentss and update when updating
        #infection_status and immune_status, respectively
        #(or do a one-step update each cycle?)
        infectiousness = ((agents$infection_status == 'IA' & !(agents$isolated)) * p_trans_IA +
                          (agents$infection_status == 'IP' & !(agents$isolated)) * p_trans_IP +
                          (agents$infection_status == 'IM' & !(agents$isolated)) * p_trans_IM)
        foi_contributions = contacts * infectiousness
        force_of_infection = colSums(foi_contributions)
        p_infection = 1 - exp(-force_of_infection)
        p_infection_V1 = 1 - exp(-force_of_infection * V1_susceptibility)
        p_infection_V2 = 1 - exp(-force_of_infection * V2_susceptibility)
        p_infection_R = 1 - exp(-force_of_infection * R_susceptibility)
        p_infection_W = 1 - exp(-force_of_infection * W_susceptibility)

        # Putting the process of infection on hold a moment, to figure out who
        # among the already-infected needs to progress along their course of
        # infection (or recover), before the shift is over.
        #
        # Ideally, we probably want to incorporate transitions out of
        # infectiousness into calculation of transmission potentials, but later.

        xE_to_I = ((agents$infection_status == 'E') &
                  ((end_time - agents$time_E) > agents$duration_E))
        #TBD (possibly): Again, might want to simplify this
        xE_to_IP = xE_to_I & ((agents$immune_status == 'FS' & agents$symptomatic) |
                              (agents$immune_status == 'V1' & agents$V1_symptomatic) |
                              (agents$immune_status == 'V2' & agents$V2_symptomatic) |
                              (agents$immune_status == 'R' & agents$R_symptomatic) |
                              (agents$immune_status == 'W' & agents$W_symptomatic))
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
        IM_to_R =  IM_to_x & !(agents$severe)   #TBD (eventually): account for possibility of greater immunity if infected after vaccination, previous infection, etc.
        IS_to_x = ((agents$infection_status == 'IS') &
                    ((end_time - agents$time_IS) > agents$duration_IS))
        IS_to_IC = IS_to_x & agents$critical
        IS_to_R =  IS_to_x & !(agents$critical)
        IC_to_x = ((agents$infection_status == 'IC') &
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

        #TBD (eventually): simplify
        NI_to_E_community = ((agents$infection_status == 'NI') & !(agents$isolated) &
                             ((agents$immune_status == 'FS' & rbinom(N, 1, 1 - exp(-lambda)))) |
                             ((agents$immune_status == 'V1' & rbinom(N, 1, 1 - exp(-lambda * V1_susceptibility)))) |
                             ((agents$immune_status == 'V2' & rbinom(N, 1, 1 - exp(-lambda * V2_susceptibility)))) |
                             ((agents$immune_status == 'R' & rbinom(N, 1, 1 - exp(-lambda * R_susceptibility)))) |
                             ((agents$immune_status == 'W' & rbinom(N, 1, 1 - exp(-lambda * W_susceptibility)))))

        #Old contact tracing code that needs to be revised and reactivated.
        #agents$infector_ID[S_to_E_community] = -1
        #agents$infector_state[S_to_E_community] = "UNKNOWN"
        agents$infection_status[NI_to_E_community] = 'E'

        #should ideally be a truncated exponential, but this is
        #adequate for now

        #also, note that making a single "susceptibility" attribute per agent
        #would simplify a lot of this

        agents$time_E[NI_to_E_community] = runif(sum(NI_to_E_community),
                                                start_time, end_time)

        # Note that while we are now using continuous transition times, the
        # probability of infection is still based on infection status at the
        # start of a shift. This might be changed in the future.
        # Another possibility may even be to use a priority queue or some such,
        # but that's a matter for another time.

        #TBD (eventually): simplify
        NI_to_E = ((agents$infection_status == 'NI') &
                   (agents$immune_status == 'FS' & (rbinom(N, 1, p_infection) > 0)) |
                   (agents$immune_status == 'V1' & (rbinom(N, 1, p_infection_V1) > 0)) |
                   (agents$immune_status == 'V2' & (rbinom(N, 1, p_infection_V2) > 0)) |
                   (agents$immune_status == 'R' & (rbinom(N, 1, p_infection_R) > 0)) |
                   (agents$immune_status == 'W' & (rbinom(N, 1, p_infection_W) > 0)))

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
            agents$infector_state[NI_to_E] = paste(agents$infection_status[infectors], agents$immune_status[infectors], agents$vax_status[infectors], sep = '_') #kludgey, but we're not currently using this for anything anyway
            agents$infection_status[NI_to_E] = 'E'
            #should ideally be a truncated exponential, but this is
            #adequate for now
            agents$time_E[NI_to_E] = runif(sum(NI_to_E), start_time, end_time)
        }


        agents$infection_status[xE_to_IA] = 'IA'
        agents$infection_status[xE_to_IP] = 'IP'
        agents$infection_status[x_to_R] = 'NI'
        agents$immune_status[x_to_R] = 'R' #TBD (eventually): repeat infection or vax + infection possible boosting
        agents$infection_status[IP_to_IM] = 'IM'
        agents$infection_status[IM_to_IS] = 'IS'
        agents$infection_status[IS_to_IC] = 'IC'
        agents$infection_status[IC_to_D] = 'D' #TBD (eventually): figure out whether this should be considered an immune status as well

        agents$time_IA[xE_to_IA] = agents$time_E[xE_to_IA] + agents$duration_E[xE_to_IA]
        agents$time_IP[xE_to_IP] = agents$time_E[xE_to_IP] + agents$duration_E[xE_to_IP]
        agents$time_IM[IP_to_IM] = agents$time_IP[IP_to_IM] + agents$duration_IP[IP_to_IM]
        agents$time_IS[IM_to_IS] = agents$time_IM[IM_to_IS] + agents$duration_IM[IM_to_IS]
        agents$time_IC[IS_to_IC] = agents$time_IS[IS_to_IC] + agents$duration_IS[IS_to_IC]
        agents$time_D[IC_to_D] = agents$time_IC[IC_to_D] + agents$duration_IC[IC_to_D]
        #TBD (eventually): Simplify below? (with a "time last progression"
        #variable?)
        agents$time_R[IA_to_R] = agents$time_IA[IA_to_R] + agents$duration_IA[IA_to_R]
        agents$time_R[IM_to_R] = agents$time_IM[IM_to_R] + agents$duration_IM[IM_to_R]
        agents$time_R[IS_to_R] = agents$time_IS[IS_to_R] + agents$duration_IS[IS_to_R]
        agents$time_R[IC_to_R] = agents$time_IC[IC_to_R] + agents$duration_IC[IC_to_R]

        #and now waning
        R_to_W = ((agents$infection_status == 'NI' & agents$immune_status == 'R') &
                    ((end_time - agents$time_R) > agents$duration_R))
        V2_to_W = ((agents$infection_status == 'NI' & agents$immune_status == 'V2') &
                    ((end_time - agents$time_V2) > agents$duration_V2))
    
        x_to_W = R_to_W | V2_to_W

        agents$immune_status[x_to_W] = 'W'
        agents$time_W[R_to_W] = agents$time_R[R_to_W] + agents$duration_R[R_to_W]
        agents$time_W[V2_to_W] = agents$time_V2[V2_to_W] + agents$duration_V2[V2_to_W]


        #TBD: We still need to recalculate durations for repeats of the same event (now possible)
        #TBD: R infections (NB: isn't this already done!?)

    #"Out1" records the sum of individuals in each state at time k (i.e., during time from time=1 to time=nTime1)
    #this allows ploting trajectories for each state in one simulation.
    #It is anticipated that a future version may record additional information at each time step.
    #"agents" shows demographic characetristics of all individuals in the population and their infection status at time nTime1
    #TBD (soon): Remove these, once they're no longer necessary
    Out1$S[k] <-  sum(agents$infection_status == "NI" & agents$immune_status == 'FS') #TRUE == 1 for the purpose of summation
    Out1$E[k] <-  sum(agents$infection_status == "E" & agents$immune_status == 'FS')
    Out1$IA[k] <- sum(agents$infection_status == "IA")
    Out1$IP[k] <- sum(agents$infection_status == "IP")
    Out1$IM[k] <- sum(agents$infection_status == "IM")
    Out1$IS[k] <- sum(agents$infection_status == "IS")
    Out1$IC[k] <- sum(agents$infection_status == "IC")
    Out1$R[k] <-  sum(agents$infection_status == "NI" & agents$immune_status == 'R')
    Out1$D[k] <-  sum(agents$infection_status == "D")
    Out1$V1[k] <-  sum(agents$infection_status == "NI" & agents$immune_status == "V1")
    Out1$V2[k] <-  sum(agents$infection_status == "NI" & agents$immune_status == "V2")
    Out1$W[k] <- sum(agents$infection_status == "NI" & agents$immune_status == "W")
    Out1$V1E[k] <-  sum(agents$infection_status == "E" & agents$immune_status == "V1")
    Out1$V2E[k] <-  sum(agents$infection_status == "E" & agents$immune_status == "V2")
    Out1$WE[k] <- sum(agents$infection_status == "E" & agents$immune_status == "W")
    Out1$RE[k] <- sum(agents$infection_status == "E" & agents$immune_status == "R")
    Out1$S_isolated[k] <-  sum(agents$infection_status == "NI" & agents$immune_status == 'FS' & agents$isolated)
    Out1$E_isolated[k] <-  sum(agents$infection_status == "E" & agents$immune_status == 'FS' & agents$isolated)
    Out1$IA_isolated[k] <- sum(agents$infection_status == "IA" & agents$isolated)
    Out1$IP_isolated[k] <- sum(agents$infection_status == "IP" & agents$isolated)
    Out1$IM_isolated[k] <- sum(agents$infection_status == "IM" & agents$isolated)
    Out1$R_isolated[k] <-  sum(agents$infection_status == "NI" & agents$immune_status == "R" & agents$isolated)
    Out1$V1_isolated[k] <-  sum(agents$infection_status == "NI" & agents$immune_status == "V1" & agents$isolated)
    Out1$V2_isolated[k] <-  sum(agents$infection_status == "NI" & agents$immune_status == "V2" & agents$isolated)
    Out1$V1E_isolated[k] <-  sum(agents$infection_status == "E" & agents$immune_status == "V1" & agents$isolated)
    Out1$V2E_isolated[k] <-  sum(agents$infection_status == "E" & agents$immune_status == "V2" & agents$isolated)
    Out1$W_isolated[k] <- sum(agents$infection_status == "NI" & agents$immune_status == "W" & agents$isolated)
    Out1$WE_isolated[k] <-  sum(agents$infection_status == "E" & agents$immune_status == "W" & agents$isolated)
    Out1$RE_isolated[k] <-  sum(agents$infection_status == "E" & agents$immune_status == "R" & agents$isolated)
#print('Out1 completed')

    agentss[[k]] = agents
  }
    #print('ABM completed')

    Out <- list("Out1" = Out1, "agents" = agents, agentss = agentss) #create a list of objects to return
    #print(dim(Out1))
  
    return (Out) #return a list of objects
}

