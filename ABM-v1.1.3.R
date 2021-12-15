ABM <- function(agents, contacts_list, lambda_list, schedule,
                virus_parameters, testing_parameters, vaccine_parameters, scenario_parameters,
                steps, step_length_list, testing_rate_list, vaccination_rate_list, agent_presence_list, waning_parameters) {

    N <-nrow(agents)

    ##
    #Kludging in a hard-coded handling of special functions for testing purposes
    ##

    special_mask = c(rep(FALSE,32), TRUE, rep(FALSE, 40), TRUE, rep(FALSE,9), TRUE, rep(FALSE, 19))

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
                       n_scheduled = rep(0, steps),
                       n_absent = rep(0, steps),
                       W_isolated = rep(0, steps),  #
                       WE_isolated = rep(0, steps), #
                       n_special_scheduled = rep(0, steps),
                       n_special_absent = rep(0, steps)
    )

    #will be used for visualizing epidemics later
    #Out2 <- matrix(data = '', rows = N, columns = steps)

    #dump parameters to local variables -- centralized for easier tweaking
    #and to make it easier to verify consistent use of get, for easier debugging
    #not totally sure between this and using with, long term; will think about
    #it

    #Ignore these comments
    #TBD fix step length
    #TBD fix testing rate
    #testing_rate = get('testing_rate', par1) #probability of testing per shift . . . except this should be shift-specific
                                             #hang on
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


    #TRUE -> failure probabilities are per test, and tests of the same
    #individual on successive days are independent
    #FALSE -> failure probabilities are per individual, and all tests of the
    #same individual, in the same stage, will give the same result
    #independent_failures = get('independent_failures', par1)
    #currently treating this as universally true, based on CDC comments that
    #serial testing ups the odds of detecting asymptomatic cases

    end_time = 0

    fractional_test_carried = 0

    # Move people through time
    #print('ABM loop')
    for(k in 1:steps) {
        contacts = get(schedule[k], contacts_list)
#print('lambda')
        lambda = get(schedule[k], lambda_list)
#print('step_length')
        step_length = get(schedule[k], step_length_list)
#print('testing_rate')
        testing_rate = get(schedule[k], testing_rate_list)
#print('vax rate')
        vaccination_rate = get(schedule[k], vaccination_rate_list)
        ####
        #new parameter
        ####
        agent_presence = get(schedule[k], agent_presence_list)

        start_time = end_time #(k - 1) * step_length
#print('step_length')
        end_time = start_time + step_length
#print('vaccinate')
        #vaccinate
        #print('vaccination')
        if(sum(vaccination_rate) > 0) {
            ####
            #modifying for facility model -- never mind, doesn't need modification! all that needs changing is the definition of rates to vectors instead of scalars! neat!
            ####
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
            # But this is another argument for more cleaning separating multiple aspects of state
            S_to_V1 = ((agents$state == 'S' & !(agents$isolated)) &
                       (rbinom(N, 1, vaccination_rate)))
            V1_to_V2 = ((agents$state == 'V1' & !(agents$isolated)) &
                        ((end_time - agents$time_V1) > vaccination_interval) &
                        (vaccination_rate > 0)) #assume times that they could get 1st dose & times they could get 2nd dose are the same
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
        #
        # Note that it might be preferable to have the code for isolation notice
        # when someone is unisolated and immediately reisolated, and "merge" the
        # isolations. But that is not necesary at this time.
        #print('deisolation')
        x_un_Isol = ((agents$isolated) & ((start_time - agents$time_isolated) >= isolation_duration))
        agents$isolated[x_un_Isol] = FALSE

        #print('testing')
        if(sum(testing_rate) > 0) {
            
            #Currently using a flag for whether someone is isolated or not, but
            #could use new compartments (e.g. state = Isolated_IA, instead of
            #state = IA and Isolated = True

            #ignore these comments
            #TBD: variables needed:
            #rational_testing
            #agents$time_tested
            ####
            #modifying
            ####
            #if(testing_rate == 1) {
            if(max(testing_rate) == 1) {
                #testing_mask = rep(TRUE, N) #for exact comparison purposes
                testing_mask = agent_presence #for exact comparison purposes
            } else if(rational_testing) {
                #indices = sort(test_time, index.return = TRUE)[[2]] #ordered by time since last test (oldest to newest)
                                      #[[2]] = just indices
                indices = order(agents$time_tested, sample(N)) #second parameter randomizes ties
                ####
                #making use of new parameter (maybe should be further systematized)
                #for now, going to treat *for testing purposes only* between-shift floaters as always present for the first shift of the day . . .
                ####
                eligible = ((agents$state %in% c('S', 'E', 'IA', 'IP', 'IM', 'R', 'RE', 'V1', 'V2', 'V1E', 'V2E', 'W', 'WE')) & !(agents$isolated)) & agent_presence
                indices = indices[eligible[indices]] 
                #theoretical_number_of_tests = testing_rate * N + fractional_test_carried
                theoretical_number_of_tests = testing_rate * sum(agent_presence) + fractional_test_carried
                number_of_tests = min(floor(theoretical_number_of_tests), length(indices))
                fractional_test_carried = theoretical_number_of_tests - number_of_tests
                testing_mask = (1:N) %in% indices[1:number_of_tests]
            } else {
                testing_mask = rbinom(N, 1, testing_rate) == 1
            }

            agents$time_tested[testing_mask] = start_time


            #isolate
            #correct detections
            IA_to_Isol = ((agents$state == 'IA' & !(agents$isolated)) &
                          testing_mask & (rbinom(N, 1, 1 - IA_FNR)))
            IP_to_Isol = ((agents$state == 'IP' & !(agents$isolated)) &
                          testing_mask & (rbinom(N, 1, 1 - IP_FNR)))
            IM_to_Isol = ((agents$state == 'IM' & !(agents$isolated)) &
                          testing_mask & (rbinom(N, 1, 1 - IM_FNR)))
            
            #errors
            #note that FPR is currently assumed to be constant, but changing the
            #code for this would not be hard
            S_to_Isol = ((agents$state == 'S' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))
            R_to_Isol = ((agents$state == 'R' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))
            V1_to_Isol = ((agents$state == 'V1' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))
            V2_to_Isol = ((agents$state == 'V2' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))
            W_to_Isol = ((agents$state == 'W' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))

            #right, for the wrong reason
            #(presumably, our odds of detecting the exposed should not be
            #be *lower* than our odds of "detecting" susceptibles)
            E_to_Isol = ((agents$state == 'E' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))
            #we're missing V1E_to_Isol and V2E_to_Isol in v1.1.3
            #I'll fix that now, and see what others want to do about it
            #This cluster suggests a definite reason to treat exposure as
            #a separate dimension from immunity (prior to any current infection)
            V1E_to_Isol = ((agents$state == 'V1E' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))
            V2E_to_Isol = ((agents$state == 'V2E' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))
            #And now what I came here to do: adding waners
            WE_to_Isol =  ((agents$state == 'WE' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))
            #and recovered
            RE_to_Isol =  ((agents$state == 'RE' & !(agents$isolated)) &
                         testing_mask & (rbinom(N, 1, FPR)))
            #Actual transfer to isolation
            x_to_Isol = (IA_to_Isol | IP_to_Isol | IM_to_Isol |
                         S_to_Isol | R_to_Isol | V1_to_Isol | V2_to_Isol |
                         E_to_Isol | V1E_to_Isol | V2E_to_Isol | W_to_Isol | RE_to_Isol)

            agents$isolated[x_to_Isol] = TRUE
            agents$time_isolated[x_to_Isol] = start_time
    
            #assuming for now that isolated do not get exposed; something similar to
            #lambda (but smaller) may be appropriate if isolation (of those
            #(falsely) detected) is imperfect
    
        }
    
        infectiousness = ((agents$state == 'IA' & !(agents$isolated)) * p_trans_IA +
                          (agents$state == 'IP' & !(agents$isolated)) * p_trans_IP +
                          (agents$state == 'IM' & !(agents$isolated)) * p_trans_IM)
        #now the average number of infectious contacts that each susceptible
        #individual will make per day

        #modified 2021-10-11 02:10
        #profiling suggests over 50% (!) of run time may be in t(...)
        #so let's avoid doing that (especially since contacts is actually symmetric
        #changed two lines here
        #old_foi_contributions = t(t(contacts) * infectiousness) #i.e., entry ij is
                                                            #expected # of inf.
                                                            #contacts from j to i
                                                            #per day
        #old_force_of_infection = rowSums(old_foi_contributions)
        foi_contributions = contacts * infectiousness
        force_of_infection = colSums(foi_contributions)
        #the above two lines are equivalent to:
        #force_of_infection = contacts %*% infectiousness
#cat(all(t(old_foi_contributions) == foi_contributions), all(old_force_of_infection == force_of_infection))
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

        WE_to_I = ((agents$state == 'WE') &
                    ((end_time - agents$time_E) > agents$duration_E))
        WE_to_IP = WE_to_I & agents$W_symptomatic
        WE_to_IA = WE_to_I & !(agents$W_symptomatic)

        RE_to_I = ((agents$state == 'RE') &
                    ((end_time - agents$time_E) > agents$duration_E))
        RE_to_IP = RE_to_I & agents$R_symptomatic
        RE_to_IA = RE_to_I & !(agents$R_symptomatic)

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

        

        #For simplicity, we'll do community transmission first, i.e.,
        #if someone would be infected from community transmission and from
        #within-company transmission in the same shift, community transmission
        #wins. In the long run, this may be changed to be probabilistic.
        #In practice, though, it's unlikely to matter much -- most scenarios
        #will have few if any shifts in which both probabilities are
        #non-negligible (I think).

        #Switch from simply lambda to exp(-lambda) for more comparable
        #parameters "average" vs. "lambda"
        S_to_E_community = ((agents$state == 'S') & !(agents$isolated) &
                            (rbinom(N, 1, 1 - exp(-lambda))))
        V1_to_V1E_community = ((agents$state == 'V1') & !(agents$isolated) &
                               (rbinom(N, 1, 1 - exp(-lambda * V1_susceptibility))))
        V2_to_V2E_community = ((agents$state == 'V2') & !(agents$isolated) &
                               (rbinom(N, 1, 1 - exp(-lambda * V2_susceptibility))))
        W_to_WE_community = ((agents$state == 'W') & !(agents$isolated) &
                             (rbinom(N, 1, 1 - exp(-lambda * W_susceptibility))))
        R_to_RE_community = ((agents$state == 'R') & !(agents$isolated) &
                             (rbinom(N, 1, 1 - exp(-lambda * R_susceptibility))))
        #Old contact tracing code that needs to be revised and reactivated.
        #agents$infector_ID[S_to_E_community] = -1
        #agents$infector_state[S_to_E_community] = "UNKNOWN"
        agents$state[S_to_E_community] = 'E'
        agents$state[V1_to_V1E_community] = 'V1E'
        agents$state[V2_to_V2E_community] = 'V2E'
        agents$state[W_to_WE_community] = 'WE'
        agents$state[R_to_RE_community] = 'RE'
        #should ideally be a truncated exponential, but this is
        #adequate for now

        #also, note that making a single "susceptibility" attribute per agent
        #would simplify a lot of this
#print('times E')
        agents$time_E[S_to_E_community] = runif(sum(S_to_E_community),
                                                start_time, end_time)
        agents$time_E[V1_to_V1E_community] = runif(sum(V1_to_V1E_community),
                                                start_time, end_time)
        agents$time_E[V2_to_V2E_community] = runif(sum(V2_to_V2E_community),
                                                start_time, end_time)
        agents$time_E[W_to_WE_community] = runif(sum(W_to_WE_community),
                                                 start_time, end_time)
        agents$time_E[R_to_RE_community] = runif(sum(R_to_RE_community),
                                                 start_time, end_time)

        #note that while we are now using continuous transition times, the prob.
        #of infection is still based on infection status at the start of a shift
        #this might be changed in the future
        #another possibility may even be to use a priority queue or some such
        #but that's a matter for another time
        S_to_E = (agents$state == 'S') & (rbinom(N, 1, p_infection) > 0)
        V1_to_V1E = (agents$state == 'V1') & (rbinom(N, 1, p_infection_V1) > 0)
        V2_to_V2E = (agents$state == 'V2') & (rbinom(N, 1, p_infection_V2) > 0)
        W_to_WE = (agents$state == 'W') & (rbinom(N, 1, p_infection_W) > 0)
        R_to_RE = (agents$state == 'R') & (rbinom(N, 1, p_infection_R) > 0)
#print('infecting S')
        if(sum(S_to_E) > 0) { #necessitated by weird behavior of apply
                              #when given an empty matrix
            #modified 2021-10-11 02:10 (2/?))
            #changed one line here
            #old_potential_infectors = old_foi_contributions[S_to_E,]
            potential_infectors = foi_contributions[,S_to_E]
            #if(!all(t(old_potential_infectors) == potential_infectors)) {
            #    opi <<- old_potential_infectors
            #    pi <<- potential_infectors
            #    stop()
            #}
#cat(all(t(old_potential_infectors) == potential_infectors), '\n')
            if(sum(S_to_E) == 1) { # in this case, potential_infectors is a
                                   # vector instead of a matrix
                infectors = sample(1:N, 1, prob = potential_infectors /
                                            sum(potential_infectors))
#cat('Spacer\n')
            } else {
                #modified 2021-10-11 02:10 (3/?))
                #changed one (half of a broken) line here
                #seed_ = .Random.seed
                #old_infectors = apply(old_potential_infectors, 1,
                #                      function(x) sample(1:N, 1, prob = x / sum(x)))
                #set.seed(seed_)
                #cat(seed_ == .Random.seed)
                #.Random.seed <<- seed_
                infectors = apply(potential_infectors, 2,
                                  function(x) sample(1:N, 1, prob = x / sum(x)))
                #if(!(all(old_infectors == infectors))) {
                #    old_infectors <<- old_infectors
                #    infectors <<- infectors
                #    stop()
                #}
            }

            agents$infector_ID[S_to_E] = agents$ID[infectors]
            agents$infector_state[S_to_E] = agents$state[infectors]
            agents$state[S_to_E] = 'E'
            #should really be a truncated exponential, but this should be
            #adequate for now
            agents$time_E[S_to_E] = runif(sum(S_to_E), start_time, end_time)
        }
#print('infecting V1')
        if(sum(V1_to_V1E) > 0) { #necessitated by weird behavior of apply
                              #when given an empty matrix
            #modified 2021-10-11 02:10 (4/?))
            #changed one line here
            #potential_infectors = foi_contributions[V1_to_V1E,]
            potential_infectors = foi_contributions[,V1_to_V1E]
            if(sum(V1_to_V1E) == 1) { # in this case, potential_infectors is a
                                   # vector instead of a matrix
                infectors = sample(1:N, 1, prob = potential_infectors /
                                            sum(potential_infectors))
            } else {
                #modified 2021-10-11 02:10 (5/?))
                #changed one (half of a broken) line here
                #infectors = apply(potential_infectors, 1,
                infectors = apply(potential_infectors, 2,
                                  function(x) sample(1:N, 1, prob = x / sum(x)))
            }

            agents$infector_ID[V1_to_V1E] = agents$ID[infectors]
            agents$infector_state[V1_to_V1E] = agents$state[infectors]
            agents$state[V1_to_V1E] = 'V1E'
            #should really be a truncated exponential, but this should be
            #adequate for now
            agents$time_E[V1_to_V1E] = runif(sum(V1_to_V1E), start_time, end_time)
        }
#print('infecting V2')
        if(sum(V2_to_V2E) > 0) { #necessitated by weird behavior of apply
                              #when given an empty matrix
            #modified 2021-10-11 02:10 (6/?))
            #changed one line here
            #potential_infectors = foi_contributions[V2_to_V2E,]
            potential_infectors = foi_contributions[,V2_to_V2E]
            if(sum(V2_to_V2E) == 1) { # in this case, potential_infectors is a
                                   # vector instead of a matrix
                infectors = sample(1:N, 1, prob = potential_infectors /
                                            sum(potential_infectors))
            } else {
                #modified 2021-10-11 02:10 (7/?))
                #changed one (half of a broken) line here
                #infectors = apply(potential_infectors, 1,
                infectors = apply(potential_infectors, 2,
                                  function(x) sample(1:N, 1, prob = x / sum(x)))
            }

            agents$infector_ID[V2_to_V2E] = agents$ID[infectors]
            agents$infector_state[V2_to_V2E] = agents$state[infectors]
            agents$state[V2_to_V2E] = 'V2E'
            #should really be a truncated exponential, but this should be
            #adequate for now
            agents$time_E[V2_to_V2E] = runif(sum(V2_to_V2E), start_time, end_time)
        }
#print('infecting W')
        if(sum(W_to_WE) > 0) { #necessitated by weird behavior of apply
                                 #when given an empty matrix
            potential_infectors = foi_contributions[,W_to_WE]
            if(sum(W_to_WE) == 1) { # in this case, potential_infectors is a
                                      # vector instead of a matrix
                infectors = sample(1:N, 1, prob = potential_infectors /
                                            sum(potential_infectors))
            } else {
                infectors = apply(potential_infectors, 2,
                                  function(x) sample(1:N, 1, prob = x / sum(x)))
            }

            agents$infector_ID[W_to_WE] = agents$ID[infectors]
            agents$infector_state[W_to_WE] = agents$state[infectors]
            agents$state[W_to_WE] = 'WE'
            #should ideally be a truncated exponential, but this is
            #adequate for now
            agents$time_E[W_to_WE] = runif(sum(W_to_WE), start_time, end_time)
        }
#print('infecting R')
        if(sum(R_to_RE) > 0) { #necessitated by weird behavior of apply
                                 #when given an empty matrix
            potential_infectors = foi_contributions[,R_to_RE]
            if(sum(R_to_RE) == 1) { # in this case, potential_infectors is a
                                      # vector instead of a matrix
                infectors = sample(1:N, 1, prob = potential_infectors /
                                            sum(potential_infectors))
            } else {
                infectors = apply(potential_infectors, 2,
                                  function(x) sample(1:N, 1, prob = x / sum(x)))
            }

            agents$infector_ID[R_to_RE] = agents$ID[infectors]
            agents$infector_state[R_to_RE] = agents$state[infectors]
            agents$state[R_to_RE] = 'RE'
            #should ideally be a truncated exponential, but this is
            #adequate for now
            agents$time_E[R_to_RE] = runif(sum(R_to_RE), start_time, end_time)
        }


        agents$state[E_to_IA] = 'IA'
        agents$state[E_to_IP] = 'IP'
        agents$state[V1E_to_IA] = 'IA'
        agents$state[V1E_to_IP] = 'IP'
        agents$state[V2E_to_IA] = 'IA'
        agents$state[V2E_to_IP] = 'IP'
        agents$state[WE_to_IA] = 'IA'
        agents$state[WE_to_IP] = 'IP'
        agents$state[RE_to_IA] = 'IA'
        agents$state[RE_to_IP] = 'IP'
        agents$state[x_to_R] = 'R'
        agents$state[IP_to_IM] = 'IM'
        agents$state[IM_to_IS] = 'IS'
        agents$state[IS_to_IC] = 'IC'
        agents$state[IC_to_D] = 'D'
#print('setting times')
        agents$time_IA[E_to_IA] = agents$time_E[E_to_IA] + agents$duration_E[E_to_IA]
        agents$time_IP[E_to_IP] = agents$time_E[E_to_IP] + agents$duration_E[E_to_IP]
        agents$time_IA[V1E_to_IA] = agents$time_E[V1E_to_IA] + agents$duration_E[V1E_to_IA]
        agents$time_IP[V1E_to_IP] = agents$time_E[V1E_to_IP] + agents$duration_E[V1E_to_IP]
        agents$time_IA[V2E_to_IA] = agents$time_E[V2E_to_IA] + agents$duration_E[V2E_to_IA]
        agents$time_IP[V2E_to_IP] = agents$time_E[V2E_to_IP] + agents$duration_E[V2E_to_IP]
        agents$time_IA[WE_to_IA] = agents$time_E[WE_to_IA] + agents$duration_E[WE_to_IA]
        agents$time_IP[WE_to_IP] = agents$time_E[WE_to_IP] + agents$duration_E[WE_to_IP]
        agents$time_IA[RE_to_IA] = agents$time_E[RE_to_IA] + agents$duration_E[RE_to_IA]
        agents$time_IP[RE_to_IP] = agents$time_E[RE_to_IP] + agents$duration_E[RE_to_IP]
        agents$time_IM[IP_to_IM] = agents$time_IP[IP_to_IM] + agents$duration_IP[IP_to_IM]
        agents$time_IS[IM_to_IS] = agents$time_IM[IM_to_IS] + agents$duration_IM[IM_to_IS]
        agents$time_IC[IS_to_IC] = agents$time_IS[IS_to_IC] + agents$duration_IS[IS_to_IC]
        agents$time_D[IC_to_D] = agents$time_IC[IC_to_D] + agents$duration_IC[IC_to_D]
        agents$time_R[IA_to_R] = agents$time_IA[IA_to_R] + agents$duration_IA[IA_to_R]
        agents$time_R[IM_to_R] = agents$time_IM[IM_to_R] + agents$duration_IM[IM_to_R]
        agents$time_R[IS_to_R] = agents$time_IS[IS_to_R] + agents$duration_IS[IS_to_R]
        agents$time_R[IC_to_R] = agents$time_IC[IC_to_R] + agents$duration_IC[IC_to_R]

        #and now waning
        #print('waning')
        R_to_W = ((agents$state == 'R') &
                    ((end_time - agents$time_R) > agents$duration_R))
        V2_to_W = ((agents$state == 'V2') &
                    ((end_time - agents$time_V2) > agents$duration_V2))
    
        x_to_W = R_to_W | V2_to_W

        agents$state[x_to_W] = 'W'
        agents$time_W[R_to_W] = agents$time_R[R_to_W] + agents$duration_R[R_to_W]
        agents$time_W[V2_to_W] = agents$time_V2[V2_to_W] + agents$duration_V2[V2_to_W]


        #TBD: We still need to recalculate durations for repeats of the same event (now possible)
        #TBD: R infections

    #"Out1" records the sum of individuals in each state at time k (i.e., during time from time=1 to time=nTime1)
    #this allows ploting trajectories for each state in one simulation
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
    Out1$W[k] <- sum(agents$state == "W")
    Out1$V1E[k] <-  sum(agents$state == "V1E")
    Out1$V2E[k] <-  sum(agents$state == "V2E")
    Out1$WE[k] <- sum(agents$state == "WE")
    Out1$RE[k] <- sum(agents$state == "RE")
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
    ####
    #note that *for now* I am treating between-shift floaters as present at a deterministic 1/3
    ####
    Out1$n_scheduled[k] = sum(agent_presence)
    Out1$n_absent[k] = sum(agent_presence * (agents$state %in% c('IS', 'IC', 'D') | agents$isolated))
    Out1$W_isolated[k] <- sum(agents$state == "W" & agents$isolated)
    Out1$WE_isolated[k] <-  sum(agents$state == "WE" & agents$isolated)
    Out1$RE_isolated[k] <-  sum(agents$state == "RE" & agents$isolated)
    Out1$n_special_scheduled[k] = sum(agent_presence & special_mask) #in current kludged form, should always be 1 for work shifts -- check!
    Out1$n_special_absent[k] = sum((agent_presence & special_mask) * (agents$state %in% c('IS', 'IC', 'D') | agents$isolated))
  }
    #print('ABM completed')

    Out <- list("Out1" = Out1, "agents" = agents) #create a list of objects to return
  
    return (Out) #return a list of objects
}

