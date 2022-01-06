# main-produce-farm-v1.1.3.R is part of Food INdustry CoViD Control Tool
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


#ABM model
#individual based
#each person/agent has properties = parameters, attributes behaviours
#model each person individually
main_produce_farm_fn = function() { #the goal is to get more meaningful debug data
    #print('main fn')
library(Rlab)
 
source("AgentGen-v1.1.3.R")
source("ContactsGen-v1.1.3.R")
source("ABM-v1.1.3.R")

#General note: foo = get('bar', baz) is similar to foo = baz[['bar']], *except*
#that it will throw an error if baz has no element named 'bar', instead of
#setting foo = NULL
#This helps to make errors due to typos, or to incomplete updating following
#code refactoring, happen where the mistake was made, instead of propagating
#silently. Consequently, I'm working on shifting to this idiom (even if it is a
#bit less "R-like") whereever there isn't a reason not to. "Fail early, fail
#loudly!"


#These shouldn't vary on a scenario-by-scenario or facility-by-facility basis,
#with the possible exception of "what if a more human-to-human transmissible
#strain evolves and becomes dominant"--type scenarios.
    #2021-07-18 Oh hey, foreshadowing!
#Otherwise, modification should occur only in response to updates in knowledge
#about viral properties.
#Default values from Moghadas et al. 2020 (gives range of .0575 to .0698 for
#p_trans_IP)
VirusParameters = function(p_trans_IP = .0575, relative_trans_IA = .11, relative_trans_IM = .44) {
    #p_trans_IP = p_trans_IP 
    p_trans_IA = relative_trans_IA * p_trans_IP
    p_trans_IM = relative_trans_IM * p_trans_IP

    list(p_trans_IP = p_trans_IP,
         p_trans_IA = p_trans_IA,
         p_trans_IM = p_trans_IM
    )
}

virus_parameters = VirusParameters()

if(exists('DELTA_VAX') && DELTA_VAX == TRUE) {
    V1_net_symptoms = 1 - .37
    V2_susceptibility = 1 - .65
    V2_net_symptoms = 1 - .88
    V2_symptoms = V2_net_symptoms / V2_susceptibility
    V2_exp = log(V2_symptoms) / log(V2_net_symptoms)
    V1_symptoms = V1_net_symptoms ** V2_exp #haven't seen them separated, so doing the best I can
    V1_susceptibility = V1_net_symptoms / V1_symptoms

    vaccine_parameters = list(    
    V1_susceptibility = V1_susceptibility,
    V2_susceptibility = V2_susceptibility,
    V1_symptoms = V1_symptoms,
    V2_symptoms = V2_symptoms,
    vaccination_interval = 21 # based on Pfizer 
    )
} else {
    vaccine_parameters = list(    
        V1_susceptibility = 1 - .8,
        V2_susceptibility = 1 - .9,
        V1_symptoms = (1 - .88) / (1 - .8), #purely default; if I've seen an estimate of this, I do not recall it
        V2_symptoms = (1 - .94) / (1 - .9),
        vaccination_interval = 21 
    )
}


ScenarioParameters = function(work_R0, dormitory_R0, days, housing_dormitory,
                              work_testing_rate, isolation_duration,
                              home_vaccination_rate, lambda = 0, crews_by_team,
                              crew_sizes, rates, virus_params) {
    p_trans_IP = get('p_trans_IP', virus_params)
    p_trans_IA = get('p_trans_IA', virus_params)
    p_trans_IM = get('p_trans_IM', virus_params)

    #marginal values based on default age distribution
    #should probably be made more flexible later, but this should do for now
    p_symptomatic = 0.671
    duration_IA = 5
    duration_IP = 1.058 * 2.174
    duration_IM = 8
    #p_trans_IS = .89 * .0575, #from Moghadas et al. 2020, but
                #irrelevant (for now, at least), since we are assuming all severe cases are
                #hospitalized

    #R0 per contact per day
    r0pcpd = p_symptomatic * (duration_IP * p_trans_IP + duration_IM * p_trans_IM) + (1 - p_symptomatic) * duration_IA * p_trans_IA
    #r0pcpd = 0.2349583

    work_contacts = work_R0 / r0pcpd
    dormitory_contacts = dormitory_R0 / r0pcpd
    par1 = list(average = work_contacts,
                lambda = lambda, #force of infection "from the community" per home
                                #shift, if in community housing
                nTime1 = days,  #duration of one simulation iteration, in days
                crews_by_team = crews_by_team,
                crew_sizes = crew_sizes,
                rates = rates,
                dormitory_intensity = dormitory_contacts,
                housing_dormitory = housing_dormitory,
                isolation_duration = isolation_duration,
                home_vaccination_rate = home_vaccination_rate,
                work_testing_rate = work_testing_rate
    )
    par1
}

##########################################
###     set scenario and output files 
############################################
scenario_parameters = ScenarioParameters(work_R0 = net_work_R0,
                                         dormitory_R0 = dormitory_R0,
                                         days = days,
                                         housing_dormitory = TRUE, 
                                         work_testing_rate = work_testing_rate,
                                         isolation_duration = isolation_duration, 
                                         home_vaccination_rate = vaccination_rate, 
                                         lambda = community_foi, 
                                         crews_by_team = crews_by_team, 
                                         crew_sizes = crew_sizes, 
                                         rates = example_rates,
                                         virus_params = virus_parameters)


###################################
##################################

#create a nPop1 x nPop1 matrix for contacts in the population and extract list of individuals met by each Agent
work_contacts <- ContactsGen(scenario_parameters$crews_by_team, scenario_parameters$crew_sizes,
                             scenario_parameters$rates, scenario_parameters$average)

sleep_contacts = matrix(0, N, N)

if(scenario_parameters$housing_dormitory) {
    dormitory_contacts = matrix(scenario_parameters$dormitory_intensity/N, N, N)
} else {
    dormitory_contacts = sleep_contacts
}
lambda_home = scenario_parameters$lambda


contacts_list = list(work = work_contacts * 7/5, #to account for two days off per week
                     home = dormitory_contacts * 7 / 9, #to account for two home shifts per day off
                     sleep = sleep_contacts)
lambda_list = list(work = 0,
                   home = lambda_home,
                   sleep = 0)
vaccination_rate_list = list(work = 0,
                             home = scenario_parameters$home_vaccination_rate * 7 / 9, #to account for two home shifts per day off
                             sleep = 0)
workday = c('work', 'home', 'sleep')
day_off = c('home', 'home', 'sleep')
week = c(rep(workday, 5), rep(day_off, 2))
schedule = rep(week, ceiling(days/7))[1:(3 * days)]
step_length_list = list(home = 1/3, work = 1/3, sleep = 1/3)
testing_rate_list = list(home = 0, work = get('work_testing_rate', scenario_parameters), sleep = 0)
steps = scenario_parameters$nTime1 * 3
step_index = (1:steps) * (1/3) #step_length

#waning parameter time
#assuming waning to full susceptibility to infection, but no loss of protection against severe infection conditional upon infection
#loosely based on https://pubmed.ncbi.nlm.nih.gov/34619098/
waning_parameters = list(W_susceptibility = 1, 
                         R_susceptibility = vaccine_parameters$V2_susceptibility,
                         W_symptoms = vaccine_parameters$V2_symptoms,
                         R_symptoms = vaccine_parameters$V2_symptoms,
                         waning_rate = log(.88 / .47) / (4/12 * 365.2425))

#print(waning_parameters)

###### code to run simulation with num_sims iterations
if(!exists('FIXED_SEED') || FIXED_SEED == TRUE) {
    set.seed(-778276078) #random 32-bit signed integer generated using atmospheric noise
                         #for reproducible output
}
full_output = array(0, c(steps, 29, num_sims))
fuller_output = list()

#print('main loop')
sys_time_start = Sys.time()
for (i in 1:num_sims) {
    #print('iteration')
    agents <- AgentGen(N, E0 = n_exposed, IA0 = 0, IP0 = 0, IM0 = n_mild, initial_recovered = initial_recovered, initial_V1 = initial_V1, initial_V2 = initial_V2, SEVERE_MULTIPLIER = SEVERE_MULTIPLIER)
#print('agents done')
    agents$V1_symptomatic = agents$symptomatic & rbinom(N, 1, vaccine_parameters$V1_symptoms)
    agents$V2_symptomatic = agents$symptomatic & rbinom(N, 1, vaccine_parameters$V2_symptoms)
    #print('into it')
    agents$W_symptomatic = agents$symptomatic & rbinom(N, 1, waning_parameters$W_symptoms) #need to add these
    agents$R_symptomatic = agents$symptomatic & rbinom(N, 1, waning_parameters$R_symptoms) #need to add these
    #print('half')
    agents$duration_V2 = rexp(N, waning_parameters$waning_rate)
    agents$duration_R = rexp(N, waning_parameters$waning_rate)
                                                
#print('model')
    model <- ABM(agents, contacts_list = contacts_list,
                 lambda_list = lambda_list, schedule = schedule,
                 virus_parameters, testing_parameters, vaccine_parameters, scenario_parameters,
                 steps = steps, step_length_list = step_length_list,
                 testing_rate_list = testing_rate_list,
                 vaccination_rate_list = vaccination_rate_list,
                 waning_parameters = waning_parameters)
    agents = model$agents
    output = model$Out1

    agentss = model$agentss
    fuller_output[[i]] = agentss

    full_output[,,i] = as.matrix(output) #this works; for whatever reason,
                                         #as.array does not
} # for (i in 1:num_sims)

sys_time_end = Sys.time()
cat(sys_time_end - sys_time_start, 'for', row_name,'\n')

colnames(full_output) = colnames(output)
saveRDS(full_output, full_output_save_name)
saveRDS(fuller_output, paste0(full_output_save_name,'-fuller'))
} #main_produce_farm_fn 
