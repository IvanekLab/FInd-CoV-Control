# AgentGen-v2.2.0.R is part of Food INdustry CoViD Control Tool
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

# create database of all agents and their attributes
#The default age_probabilities vector is based on finding the MLE 4-parameter
#beta distribution that best fits age category data on agricultural workers

#replaced $state with $infection_status (NI rather than S), $immune_status (FS
#rather than S), $vax_status (NV for fully unvaccinated)

AgentGen <- function (N, E0 = 1, IA0 = 0, IP0 = 0, IM0 = 0,
                      initial_recovered = 0, initial_V1 = 0, initial_V2 = 0,
                      ffv_last_five_months, #TBD (eventually) make fraction vs.
                                            #number consistent across parameters
                                            #TBD (eventually): rename to be more
                                            #general?
                      age_probabilities = c(0.04, 0.26, 0.26, 0.21, 0.15, 0.07,
                                            0.01, 0),
                      fraction_boosted_ever = 0,
                      fraction_boosted_last_five_months = 0,
                      protection_functions,
                      kConstants
                      ) {
    #TBD (eventually): Either add back in the ability to use these or remove
    #them from the parameter list.
    if(max(IA0, IP0, initial_V1) > 0) {
        stop('Attempt to use buggy functionality in AgentGen.')
    }

    V1_protection = get('V1_protection', protection_functions)
    V2_protection = get('V2_protection', protection_functions)
    R_protection = get('R_protection', protection_functions)
    B_protection = get('B_protection', protection_functions)
    net_symptomatic_protection = get('net_symptomatic_protection',
                                     protection_functions)
    boosting_interval = get('boosting_interval', kConstants)
    second_shot_interval = get('second_shot_interval', kConstants)
    SEVERE_MULTIPLIER = get('SEVERE_MULTIPLIER', kConstants)
    R_question_period = get('R_question_period', kConstants)
    time_since_first_V2 = get('time_since_first_V2', kConstants)

    Age_Categories = c("10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
                       "70-79", "80+")

    ##duration_E calculations
    #Retrieving and calculating constants
    mu = get('mu', kConstants)
    sd = get('sd', kConstants)

    duration_IA_mean = get('duration_IA_mean', kConstants)
    duration_IA_shape = get('duration_IA_shape', kConstants)
    duration_IA_scale = duration_IA_mean / duration_IA_shape

    duration_IP_mean = get('duration_IP_mean', kConstants)
    duration_IP_shape = get('duration_IP_shape', kConstants)
    duration_IP_scale = duration_IP_mean / duration_IP_shape

    duration_IM_mean = get('duration_IM_mean', kConstants)
    duration_IM_shape = get('duration_IM_shape', kConstants)
    duration_IM_scale = duration_IM_mean / duration_IM_shape

    duration_IS_mean = get('duration_IS_mean', kConstants)
    duration_IS_shape = get('duration_IS_shape', kConstants)
    duration_IS_scale = duration_IS_mean / duration_IS_shape

    duration_IC_mean = get('duration_IC_mean', kConstants)
    duration_IC_shape = get('duration_IC_shape', kConstants)
    duration_IC_scale = duration_IC_mean / duration_IC_shape
    #END RETRIEVING AND CALCULATING CONSTANTS

    sdlog = sqrt(log(sd**2 / mu**2 + 1))
    mulog = log(mu) - (sdlog ** 2) / 2

    #cat('mu and sd:', mu, sd,
    #    '\nIP:', duration_IP_shape, duration_IP_scale,
    #    '\nIA:', duration_IA_shape, duration_IA_scale,
    #    '\nIM:', duration_IM_shape, duration_IM_scale,
    #    '\nIS:', duration_IS_shape, duration_IS_scale,
    #    '\nIC:', duration_IC_shape, duration_IC_scale, '\n'
    #)
    #stop('Pause here to confirm.')

    duration_IP = sgamma(N, shape=duration_IP_shape, scale=duration_IP_scale)
    duration_E = pmax(rlnorm(N, mulog, sdlog) - duration_IP, 0) 
    #Moghadas et al., 2020 & need for a non-negative duration
    #Create a population of susceptibles 
    agents <- data.frame(ID = 1:N,          #Unique ID number,
                                            #Allows contact tracing
                                            #(in a future version)
                         #state = "S",      #Initially susceptible
                                            #Will be overwritten later in this
                                            #function for some employees.
                         infection_status = 'NI', #not infected
                         immune_status = 'FS', #fully susceptible
                         vax_status = 'NV', #not vaccinated
                         infector_ID = 0,    
                         infector_state = '', #state of source of infection
                                              #(if any)
                         #The following are conceptualized as time *of*
                         #transition into the state in question, not time
                         #*since* transition. Thus, the value for a state that
                         #has not (yet) been reached (and hence, a transition
                         #that has not happened (yet) is infinity, not 0.
                         #This may in the future be changed to NA.
                         time_E = Inf,  
                         time_IA = Inf,  
                         time_IP = Inf,  
                         time_IM = Inf,
                         time_IS = Inf,
                         time_IC = Inf,
                         time_R = Inf,
                         time_V1 = Inf,
                         time_V2 = Inf,
                         time_B = Inf,
                         boosting_on_time = NA, # will be replaced later for
                                                # everyone
                         #boosting_on_time = 1:N %in% sample(
                         #   N,
                         #   round(N * boosting_on_time_probability)
                         #),
                         time_isolated = Inf,  #setup for time in Isolation
                                               #unlike some of the states listed
                                               #above, this is not a mutually
                                               #exclusive state, e.g., someone
                                               #can be both IM and isolated.
                         time_tested = -Inf,
                         #Unlike most times, we want the "last time at which
                         #this person was tested," for someone who has never
                         #been tested, to be LESS than the last time at which
                         #someone was tested who has ever been tested; hence the
                         #use of -Inf instead of Inf.
                         time_last_immunity_event = NA,
                         #using NAs here will trip a debug guard if these are
                         #not updated as they should be when individuals change
                         #immune_status
                         previous_immunity = NA,
                         isolated = FALSE, #initially
                         Age_Cat = sample(Age_Categories, N, replace = TRUE,
                                          prob = age_probabilities),
                         duration_E = duration_E,
                         duration_IA = sgamma(N, shape=duration_IA_shape, scale=duration_IA_scale),
                         # Moghadas et al., 2020
                         duration_IP = duration_IP,
                         duration_IM = sgamma(N, shape=duration_IM_shape, scale=duration_IM_scale),
                         #Michelle based on Kerr et al
                         duration_IS = sgamma(N, shape=duration_IS_shape, scale=duration_IS_scale),
                         #Michelle based on Kerr et al
                         duration_IC = sgamma(N, shape=duration_IC_shape, scale=duration_IC_scale),
                         #Michelle based on Kerr et al
                         stringsAsFactors = FALSE
                         #"stringsAsFactors = FALSE" is to allow transition into
                         #states that are not present at simulation start
    )
    #library('openssl')
    #library('repr')
    #print(sha256(repr(agents)))
    #stop('Pause here to confirm.')
    
    #pre-calculating all indices for clarity and ease of debugging
    #This block is based on the need to insure that any(index_E & index_IM) ==
    #FALSE . Its complexity is based on the R misfeature that if a == 0, then
    #1:a is not, as one might expect, numeric(), but c(1,0) .
    index_E_or_IM = sample(N, E0 + IM0)
    #There has to be a cleaner way to do this.
    if(E0 > 0 & IM0 > 0) {
        index_E = index_E_or_IM[1:E0]
        index_IM = index_E_or_IM[(E0+1):(E0+IM0)]
    } else if(E0 > 0) {
        index_E = index_E_or_IM
        index_IM = numeric()
    } else if(IM0 > 0) {
        index_E = numeric()
        index_IM = index_E_or_IM
    } else {
        index_E = numeric()
        index_IM = numeric()
    }
    #TBD (eventually): Take account of immunity in assigning initial infectees

    index_R = 1:N %in% sample(N, initial_recovered)
    index_V2 = 1:N %in% sample(N, initial_V2)
    initial_V2_last_five_months = round(ffv_last_five_months * N)
    if(initial_V2_last_five_months == 0) {
        index_V2_last_five_months = rep(FALSE, N)
    } else if(sum(index_V2) == initial_V2_last_five_months) {
        index_V2_last_five_months = index_V2
    } else { #implies sum(index_V2) > 1, so the following sample() call is safe
        index_V2_last_five_months = 1:N %in% sample((1:N)[index_V2],
                                                    initial_V2_last_five_months)
    }
    index_V2_older = index_V2 & !index_V2_last_five_months

    n_V2_older = sum(index_V2_older)
    n_boosted_ever = round(n_V2_older * fraction_boosted_ever)
    n_boosted_last_five_months = round(n_V2_older * fraction_boosted_last_five_months)
    n_boosted_older = n_boosted_ever - n_boosted_last_five_months

    if(n_boosted_ever == 0) {
        index_B = rep(FALSE, N) #= index_B_older = index_B_last_five_months
    } else if(n_boosted_ever == n_V2_older) {
        index_B = index_V2_older
    } else {#implies sum(index_V2_older) > 1, so this sample() call is safe
        index_B = 1:N %in% sample((1:N)[index_V2_older], n_boosted_ever)
    }

    if(n_boosted_last_five_months == 0) {
        index_B_last_five_months = rep(FALSE, N)
    } else if(n_boosted_last_five_months == n_boosted_ever) {
        index_B_last_five_months = index_B
    } else {#implies sum(index_B) > 1, so this sample() call is safe
        index_B_last_five_months = 1:N %in% sample((1:N)[index_B],
                                                   n_boosted_last_five_months)
    }

    index_B_older = index_B & !index_B_last_five_months

    agents$boosting_on_time = ifelse(index_B,
        TRUE,
        ifelse(index_V2_older,
            FALSE,
            rbinom(N, 1, fraction_boosted_ever)
        )
    )


    #Note: these can be allowed to not all be N, without eliminating the common
    #random variables benefits, as long as the number of calls remains constant
    #across all interventions.

    #since neither of these is affected by the other factors, it can be left
    #here for now
    agents$infection_status[index_E]= "E"
    agents$time_E[index_E]= -runif(E0, 0, agents$duration_E[index_E])

    agents$infection_status[index_IM]= "IM"
    agents$time_IM[index_IM]= -runif(IM0, 0, agents$duration_IM[index_IM])
    # These next two lines shouldn't matter, but seem harmless, and like a good
    # way to avoid the possibility of weird errors.
    agents$time_IP[index_IM] = (agents$time_IM[index_IM] -
                                agents$duration_IP[index_IM])
    agents$time_E[index_IM] = (agents$time_IP[index_IM] -
                                agents$duration_E[index_IM])

    #initial_recovered code moved from here to below V2 and boosted
    #because it's (marginally) simpler

    agents$immune_status[index_V2] = 'V2'
    agents$vax_status[index_V2] = 'V2'

    #Using uniform distributions for now; may change later.
    #TBD (eventually): better distributions
    agents$time_V2[index_V2_last_five_months] = runif(
        initial_V2_last_five_months, -boosting_interval, 0
    )
    agents$time_V2[index_V2_older] = runif(
        initial_V2 - initial_V2_last_five_months,
        -(time_since_first_V2),#fully vax starts in mid-december 2020
        -boosting_interval 
    )

    #Adjusting timing of vaccinations to match boosting timing.
    #Ideally, this would be handled in a more sophisticated fashion, but
    #this will do for now.
    agents$time_V2[index_B_older] = runif(n_boosted_older, -(time_since_first_V2), -(2*boosting_interval+1)) #TBD: Extend start date 2022-11-07
    agents$time_V2[index_B_last_five_months] = runif(n_boosted_last_five_months,
                                                     -2*boosting_interval, -boosting_interval)

    agents$time_last_immunity_event[index_V2] = agents$time_V2[index_V2]
    agents$time_V1[index_V2] = agents$time_V2[index_V2] - second_shot_interval
    #again, not perfect, but doesn't actually matter
    #(currently, and probably ever)
    agents$previous_immunity[index_V2] = V1_protection(
        (agents$time_V2[index_V2] - agents$time_V1[index_V2])
    )

    #index_B = (agents$vax_status == 'V2' &
    #           agents$time_V2 < -152 &
    #           agents$boosting_on_time)
    agents$immune_status[index_B] = 'B'
    agents$vax_status[index_B] = 'B'
    agents$time_B[index_B] = agents$time_V2[index_B] + boosting_interval #was: pmax(agents$time_V2[index_B] + 152, -152)
    agents$time_last_immunity_event[index_B] = agents$time_B[index_B]
    agents$previous_immunity[index_B] = V2_protection(
        agents$time_B[index_B] - agents$time_V2[index_B],
        agents$previous_immunity[index_V2] #i.e., the *previous* previous imm.
    )

    #particularly suboptimal at the moment, given the recent massive peak
    agents$time_R[index_R]= -runif(initial_recovered, 0, R_question_period)
    only_R = index_R & !index_V2
    agents$immune_status[only_R] = 'R'
    agents$time_last_immunity_event[only_R] = agents$time_R[only_R]
    agents$previous_immunity[only_R] = 0

    # NB: R_V1 and V1_R (without V2 or B) don't need to be addressed, because
    # the current version of the code does not allow anyone to have
    # vax_status == 'V1' at simulation start

    R_last = (index_R &
              index_V2 &
              agents$time_R > agents$time_last_immunity_event)
    agents$previous_immunity[R_last] = net_symptomatic_protection(
            agents[R_last,],
            agents$time_R[R_last]
    )
    agents$time_last_immunity_event[R_last] = agents$time_R[R_last]

    H_x_R_status = ifelse(agents$vax_status == 'NV',
        NA, #would be R, but this should never happen
        ifelse(agents$vax_status == 'V1',
            'H_V1_R',
            ifelse(agents$vax_status == 'V2',
                'H_V2_R',
                ifelse(agents$vax_status == 'B',
                    'H_B_R',
                    NA
                )
            )
        )
    )
    H_R_x_status = ifelse(agents$vax_status == 'NV',
        NA, #would be R, but this should never happen
        ifelse(agents$vax_status == 'V1',
            'H_R_V1',
            ifelse(agents$vax_status == 'V2',
                'H_R_V2',
                ifelse(agents$vax_status == 'B',
                    'H_R_B',
                    NA
                )
            )
        )
    )
    agents$immune_status[R_last] = H_x_R_status[R_last]

    R_V1_V2 = (index_R &
              index_V2 &
              (!agents$boosting_on_time) &
              agents$time_R < agents$time_V1
    )
    agents$previous_immunity[R_V1_V2] = B_protection(21, 0)
        #0 is often not technically correct, but doesn't matter for any of the
        #immunity functions we're considering
        #TBD-2023-06: Check if this is still true!
        #TBD-2023-06: Shouldn't we be checking for stealing here!? But for comparison, we will leave this for now
    #agents$time_last_immunity_event[R_V1_V2] #is unchanged
    agents$immune_status[R_V1_V2] = 'H_R_V2'

    V1_R_V2 = (index_R &
              index_V2 &
              (!agents$boosting_on_time) &
              agents$time_R >= agents$time_V1 &
              agents$time_R < agents$time_V2
    )
    agents$previous_immunity[V1_R_V2] = R_protection(   #TBD-2023-06: This should ideally be changed to a hybrid category
        (agents$time_V2[V1_R_V2] - agents$time_R[V1_R_V2])
    )
    #agents$time_last_immunity_event[V1_R_V2] #is unchanged
    #TBD-2023-06: Isn't there _always_ stealing here!? But for comparison, we will leave this for now
    agents$immune_status[V1_R_V2] = 'H_R_V2'

    V2_R_B = (index_R &
              index_V2 &
              (agents$boosting_on_time) &
              agents$time_R >= agents$time_V2 &
              agents$time_R < agents$time_B
    )
    agents$previous_immunity[V2_R_B] = R_protection(   #TBD-2023-06: This should ideally be changed to a hybrid category
        (agents$time_B[V2_R_B] - agents$time_R[V2_R_B])
    ) 
    #TBD-2023-06: Shouldn't we be checking for stealing here!? But for comparison, we will leave this for now
    agents$immune_status[V2_R_B] = 'H_R_B'
    #agents$time_last_immunity_event[V2_R_B] #is unchanged

    #R_V2_B changes nothing at all
    #Update 2023-06: Yes, it changes to a hybrid immune state
    R_V2_B = (index_R &
              index_V2 &
              (agents$boosting_on_time) &
              agents$time_R < agents$time_V2 #&
              #agents$time_R < agents$time_B
    )
    agents$immune_status[R_V2_B] = 'H_R_B'
    #TBD-2023-06: Ideally, we might wish to check whether earlier immunity was stolen? Not high priority

    #Import text file of disease progression probabilities
    Probability_Matrix <- read.csv('Probability_Matrix.csv')
  
    #Merge parameters for disease progression (Psymptomatic, Psevere, and
    #Pcritical) from table into data frame
    agents=merge.data.frame(agents, Probability_Matrix, by="Age_Cat",
                            all.x=TRUE)

    #pre-calculating, as with times
    #agents$symptomatic = rbinom(N, 1, agents$p_symptomatic) > 0
    agents$p_severe = SEVERE_MULTIPLIER * agents$p_severe
    #agents$critical = rbinom(N, 1, agents$p_critical) > 0
    #agents$death = rbinom(N, 1, agents$p_death) > 0
                                                                
    agents = agents[sample(nrow(agents)),] # this is a reshuffled database,
                                           # to randomize order of agents
                                           # reminder: Age_Cat is first column

  return(agents) 
}
