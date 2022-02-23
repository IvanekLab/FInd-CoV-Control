# AgentGen-v1.1.3.R is part of Food INdustry CoViD Control Tool
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

# create database of all agents and their attributes
#The default age_probabilities vector is based on finding the MLE 4-parameter
#beta distribution that best fits age category data on agricultural workers

#replaced $state with $infection_status (NI rather than S), $immune_status (FS
#rather than S), $vax_status (currently same as immune status for fully
#unvaccinated)

AgentGen <- function (N, E0 = 1, IA0 = 0, IP0 = 0, IM0 = 0,
                      initial_recovered = 0, initial_V1 = 0, initial_V2 = 0,
                      initial_B = 0,
                      age_probabilities = c(0.04, 0.26, 0.26, 0.21, 0.15, 0.07,
                                            0.01, 0),
                      SEVERE_MULTIPLIER = 1,
                      boosting_on_time_probability = 0,
                      protection_functions) {
    #TBD: Fix handling of IM0 (currently, it is simply ignored! -- this was not
    #true in earlier versions; it's a kludge introduced during incorporation of
    #swiss cheese code.

    #TBD (eventually): Either add back in the ability to use these or remove
    #them from the parameter list.
    if(max(IA0, IP0, initial_V1, initial_B) > 0) {
        stop('Attempt to use buggy functionality in AgentGen.')
    }

    V1_protection = get('V1_protection', protection_functions)
    V2_protection = get('V2_protection', protection_functions)
    R_protection = get('R_protection', protection_functions)
    B_protection = get('B_protection', protection_functions)
    net_symptomatic_protection = get('net_symptomatic_protection',
                                     protection_functions)

    Age_Categories = c("10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
                       "70-79", "80+")


    ##duration_E calculations
    mu = 5.2
    sd = .1
    sdlog = sqrt(log(sd**2 / mu**2 + 1))
    mulog = log(mu) - (sdlog ** 2) / 2
    duration_IP = rgamma(N, shape=1.058, scale=2.174)
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
                         mixing_propensity = 1, #leaving this in as a trivial
                                                #property, for easier
                                                #reintroduction later
                                                #currently, it is unused
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
                         boosting_on_time = 1:N %in% sample(
                            N,
                            round(N * boosting_on_time_probability)
                         ),
                         time_isolated = Inf,  #setup for time in Isolation
                                               #unlike the states listed above,
                                               #this is not a mutually exclusive
                                               #state, e.g., someone who can be
                                               #both IM and isolated.
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
                         duration_IA = rgamma(N, shape=5, scale=1),
                         # Moghadas et al., 2020
                         duration_IP = duration_IP, # Moghadas et al., 2020
                         duration_IM = rgamma(N, shape=16, scale=0.5),
                         #Michelle based on Kerr et al
                         duration_IS = rgamma(N, shape=34.0278, scale=0.4114),
                         #Michelle based on Kerr et al
                         duration_IC = rgamma(N, shape=34.0278, scale=0.4114),
                         #Michelle based on Kerr et al
                         stringsAsFactors = FALSE
                         #"stringsAsFactors = FALSE" is to allow other states later on
    ) 

    #pre-calculating all indices for clarity and ease of debugging
    index_E_or_IM = sample(N, E0 + IM0) #1:N %in% sample(N, E0 + IM0)
    #index_E = 1:N %in% sample((1:N)[index_E_or_IM], E0)
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

    #Note: these can be allowed to not all be N, as long as they're constant with
            #each interventions parameters

    #since neither of these is affected by the other factors, it can be left
    #here for now
    agents$infection_status[index_E]= "E"
    agents$time_E[index_E]= -runif(E0, 0, agents$duration_E[index_E])

    agents$infection_status[index_IM]= "IM"
    #cat('\n')
    #print(index_E_or_IM)
    #print(index_E)
    #print(index_IM)
    #print(sum(index_IM))
    #print(agents$time_IM[index_IM])
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

    #for time_R, at least the split around 6 months is right,
    #even if the rest of the distro isn't

    #let's try for 50th percentile again -- not wonderful, but tolerable
    #half-way point is around april 30, 2021 (just under 9 months ago), so a
    #uniform distribution would imply 18 months, but vaccination started around
    #December 13, 2020, not much more than 12 months
    #but wait, that's total doses anyway
    #this gives us april 18
    #okay, you know what? fuck it. this look similar in shape and rough location
    #to the doses limited plot, which suggests a very roughly piecewise
    #solution:
    #before july 20 (= -188 days, call it 190), but since dec 13 2020
    #(= -407 days, call it 410) 332 million doses = 62%, call it 60
    #total 535 million doses
    agents$time_V2[index_V2] = -ifelse(rbinom(initial_V2,1,0.6),
                                       runif(initial_V2, 0, 188),
                                       runif(initial_V2, 188, 410))
    agents$time_last_immunity_event[index_V2] = agents$time_V2[index_V2]
    agents$time_V1[index_V2] = agents$time_V2[index_V2] - 21
    #again, not perfect, but doesn't actually matter
    #(currently, and probably ever)
    agents$previous_immunity[index_V2] = V1_protection(
        (agents$time_V2[index_V2] - agents$time_V1[index_V2])
    )

    index_B = (agents$vax_status == 'V2' &
               agents$time_V2 < -152 &
               agents$boosting_on_time)
    agents$immune_status[index_B] = 'B'
    agents$vax_status[index_B] = 'B'
    agents$time_B[index_B] = pmax(agents$time_V2[index_B] + 152, -92)
    #TBD: Fix this and other distributions to use new questions.
    #TBD: Pull out distributions to separate functions or even a separate file?
    agents$time_last_immunity_event[index_B] = agents$time_B[index_B]
    agents$previous_immunity[index_B] = V2_protection(
        (agents$time_B[index_B] - agents$time_V2[index_B]), 0)
    #TBD: correct technical error / generate standard "on-schedule" previous
    #immunities

    agents$time_R[index_R]= -runif(initial_recovered, 0, 365)
    #adequate for now; may require revision when immune waning is added
    #indeed, no longer really adequate, but good enough for debugging purposes
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
    agents$immune_status[R_last] = 'R'

    R_V1_V2 = (index_R &
              index_V2 &
              (!agents$boosting_on_time) &
              agents$time_R < agents$time_V1
    )
    agents$previous_immunity[R_V1_V2] = B_protection(21, 0)
        #0 often not technically correct, but doesn't matter for any of the fn
        #we're considering
    #agents$time_last_immunity_event[R_V1_V2] #is unchanged
    agents$immune_status[R_V1_V2] = 'B'

    V1_R_V2 = (index_R &
              index_V2 &
              (!agents$boosting_on_time) &
              agents$time_R >= agents$time_V1 &
              agents$time_R < agents$time_V2
    )
    agents$previous_immunity[V1_R_V2] = R_protection(
        (agents$time_V2[V1_R_V2] - agents$time_R[V1_R_V2])
    )
    #agents$time_last_immunity_event[V1_R_V2] #is unchanged
    agents$immune_status[V1_R_V2] = 'B'

    V2_R_B = (index_R &
              index_V2 &
              (agents$boosting_on_time) &
              agents$time_R >= agents$time_V2 &
              agents$time_R < agents$time_B
    )
    agents$previous_immunity[V2_R_B] = R_protection(
        (agents$time_B[V2_R_B] - agents$time_R[V2_R_B])
    ) 
    #agents$time_last_immunity_event[V2_R_B] #is unchanged

    #R_V2_B changes nothing at all

    #Import text file of disease progression probabilities
    Probability_Matrix <- read.csv('Probability_Matrix.csv')
  
    #Merge parameters for disease progression (Psymptomatic, Psevere, and
    #Pcritical) from table into data frame
    agents=merge.data.frame(agents, Probability_Matrix, by="Age_Cat",
                            all.x=TRUE)

    #pre-calculating, as with times
    agents$symptomatic = rbinom(N, 1, agents$p_symptomatic) > 0
    agents$severe = rbinom(N, 1, SEVERE_MULTIPLIER * agents$p_severe) > 0
    agents$critical = rbinom(N, 1, agents$p_critical) > 0
    agents$death = rbinom(N, 1, agents$p_death) > 0
                                                                
    agents = agents[sample(nrow(agents)),] # this is a reshuffled database,
                                           # to randomize order of agents
                                           # reminder: Age_Cat is first column

  return(agents) 
}
