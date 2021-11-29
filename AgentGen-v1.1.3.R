# create database of all agents and their attributes
#The default age_probabilities vector is based on finding the MLE 4-parameter
#beta distribution that best fits age category data on agricultural workers
AgentGen <- function (N, E0 = 1, IA0 = 0, IP0 = 0, IM0 = 0, initial_recovered = 0,
                         initial_V1 = 0, initial_V2 = 0,
                       age_probabilities = c(0.04, 0.26, 0.26, 0.21, 0.15, 0.07,
                                             0.01, 0),
                      SEVERE_MULTIPLIER = 1) {
    Age_Categories = c("10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
                       "70-79", "80+")


    ##duration_E calculations
    mu = 5.2
    sd = .1
    sdlog = sqrt(log(sd**2 / mu**2 + 1))
    mulog = log(mu) - (sdlog ** 2) / 2
    duration_IP = rgamma(N, shape=1.058, scale=2.174)
    duration_E = pmax(rlnorm(N, mulog, sdlog) - duration_IP, 0)  #Moghadas et al., 2020 & need for a non-negative duration
    #Create a population of susceptibles 
    agents <- data.frame(ID = 1:N,          #Unique ID number,
                                            #Allows contact tracing
                         state = "S",       #Initially susceptible
                         infector_ID = 0,    
                         infector_state = '', #state of source of infection (if any)
                         #Mixing = runif(nPop1,0,1),
                         mixing_propensity = 1, #leaving this in as a trivial
                                                #property, for easier
                                                #reintroduction later
                                                #currently, it is unused
                         #I'm going to reconceptualize the following as time
                         #*of* transition, rather than time *since* transition
                         #Thus, the value of a transition that has not happened
                         #is infinity, not 0.
                         time_E = Inf ,  #setup for time in E
                         time_IA = Inf,  #setup for time in IA
                         time_IP = Inf,  #setup for time in IP
                         time_IM = Inf,  #setup for time in IM
                         time_IS = Inf,  #setup for time in IS
                         time_IC = Inf,  #setup for time in IC
                         time_R = Inf,
                         time_V1 = Inf,
                         time_V2 = Inf,
                         time_isolated = Inf,  #setup for time in Isolation
                         time_tested = -Inf,    #Unlike most times, we want the "last time at which this person was tested," for someone who has never been tested, to be LESS than the last time at which someone was tested who has ever been tested
                         isolated = FALSE, #initially
                         Age_Cat = sample(Age_Categories, N, replace = TRUE,
                                          prob = age_probabilities),
                         # Some of these require changes, but later
                         #duration_E   = round(log(rlnorm(N, 5.2, 0.1))- 2.3),
                         ##lognormal and 2.3 from Moghadas et al 2020 <- This
                         ##was literally just always 3
                         #also, removed unnecessary rounding
                         duration_E = duration_E,
                         duration_IA = rgamma(N, shape=5, scale=1),#round(rgamma(N, shape=1.058, scale=2.174)), # Moghadas et al., 2020
                         duration_IP = duration_IP, #rgamma(N, shape=1.058, scale=2.174), # Moghadas et al., 2020
                         duration_IM = rgamma(N, shape=16, scale=0.5), #Michelle based on Kerr et al
                         duration_IS = rgamma(N, shape=34.0278, scale=0.4114),  #Michelle based on Kerr et al
                         duration_IC = rgamma(N, shape=34.0278, scale=0.4114),  #Michelle based on Kerr et al
                         stringsAsFactors = FALSE ) #"stringsAsFactors = FALSE" is to allow other states later on

    #pre-calculating these for clarity and ease of debugging
    #as a bonus, the use of seq_len in this fashion means that guarding the
    #operations with if statements is not necessary
    index_E = seq_len(E0)
    index_IA = E0 + seq_len(IA0)
    index_IP = E0 + IA0 + seq_len(IP0)
    index_IM = E0 + IA0 + IP0 + seq_len(IM0)
    index_R = E0 + IA0 + IP0 + IM0 + seq_len(initial_recovered)
    index_V2 = E0 + IA0 + IP0 + IM0 + initial_recovered + seq_len(initial_V2)
    index_V1 = E0 + IA0 + IP0 + IM0 + initial_recovered + initial_V2 + seq_len(initial_V1)

    agents$state[index_E]= "E"
    agents$time_E[index_E]= -runif(E0, 0, agents$duration_E[index_E])

    agents$state[index_IA] = "IA"
    agents$time_IA[index_IA]= -runif(IA0, 0, agents$duration_IA[index_IA])

    agents$state[index_IP]= "IP"
    agents$time_IP[index_IP]= -runif(IP0, 0, agents$duration_IP[index_IP])

    agents$state[index_IM]= "IM"
    agents$time_IM[index_IM]= -runif(IM0, 0, agents$duration_IM[index_IM])

    agents$state[index_R] = 'R'
    agents$time_R[index_R]= -runif(initial_recovered, 0, 365)

    agents$state[index_V1] = 'V1'
    agents$time_V1[index_V1] = -runif(initial_V1, 0, 21) #not a perfect model of reality, but good enough

    agents$state[index_V2] = 'V2'
    agents$time_V2[index_V2] = -runif(initial_V2, 0, 21) #even worse, but shouldn't actually matter for anything
    agents$time_V1[index_V2] = agents$time_V2[index_V2] - 21 #again, not perfect, but whatever

    #generating transition times before the most recent
    #maybe not necessary, but seems like a good guard against weird bugs
    agents$time_IP[index_IM] = (agents$time_IM[index_IM] -
                                agents$duration_IP[index_IM])
    agents$time_E[index_IP] = (agents$time_IP[index_IP] -
                               agents$duration_E[index_IP])
    agents$time_E[index_IA] = (agents$time_IA[index_IA] -
                               agents$duration_E[index_IA])

    #Import text file of disease progression probabilities
    Probability_Matrix <- read.csv('Probability_Matrix.csv')
  
    #Merge parameters for disease progression (Psymptomatic, Psevere, and Pcritical from table into data frame)
    agents=merge.data.frame(agents, Probability_Matrix, by="Age_Cat", all.x=TRUE)

    #added to better model TBD
    #agents$early_detectable = rbinom(N, 1, 0.5) #TBD make parameter

    #pre-calculating, as with times
    agents$symptomatic = rbinom(N, 1, agents$p_symptomatic) > 0
    agents$severe = rbinom(N, 1, SEVERE_MULTIPLIER * agents$p_severe) > 0
    agents$critical = rbinom(N, 1, agents$p_critical) > 0
    agents$death = rbinom(N, 1, agents$p_death) > 0
                                                                
    agents = agents[sample(nrow(agents)),] ## this is reshuffled database to randomize order of agents but Age_Cat is first column (remember to move if problem)
  
  return(agents) 
  
}
