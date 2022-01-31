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
library(Rlab)
 
source("AgentGen-v1.1.3.R")
#source("ContactsGen-v1.1.3.R")
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

#if(exists('DELTA_VAX') && DELTA_VAX == TRUE) {
#print('variants')
if(variant == 'delta') {
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
    #TBD: need B_susceptibility and B_symptoms
    )
    stop('delta')
} else if(variant == 'omicron') { #lacking better data, do the simplest thing for now
#    print('in')
    V2_susceptibility = sqrt(1-.6)
    V2_symptoms = V2_susceptibility
    V1_net = 1#V2_susceptibility #going worst case here, for sanity (shouldn't matter for these figures)
    V1_susceptibility = sqrt(V1_net)
    V1_symptoms = V1_susceptibility
    B_susceptibility = sqrt(1-.7)
    B_symptoms = B_susceptibility
    vaccination_interval = 21

    vaccine_parameters = list(    
    V1_susceptibility = V1_susceptibility,
    V2_susceptibility = V2_susceptibility,
    V1_symptoms = V1_symptoms,
    V2_symptoms = V2_symptoms,
    vaccination_interval = 21,
    B_symptoms = B_symptoms,
    B_susceptibility = B_susceptibility
    )
#    print('out')
} else if(variant == '2020') {
    vaccine_parameters = list(    
        V1_susceptibility = 1 - .8,
        V2_susceptibility = 1 - .9,
        V1_symptoms = (1 - .88) / (1 - .8), #purely default; if I've seen an estimate of this, I do not recall it
        V2_symptoms = (1 - .94) / (1 - .9),
        vaccination_interval = 21, 
        B_susceptibility = 0, #there's not going to be any data on this, so screw it
        B_symptoms = 0
    #TBD: need B_susceptibility and B_symptoms
)
    #stop()
} else {
    stop('Undefined variant.')
}
#print('Done')

#2022-01-16 removed rates
ScenarioParameters = function(work_R0, dormitory_R0, days, housing_dormitory,
                              work_testing_rate, isolation_duration,
                              home_vaccination_rate, lambda = 0, crews_by_team,
                              crew_sizes, virus_params) {
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
                                         virus_params = virus_parameters)


###################################
##################################

#create a nPop1 x nPop1 matrix for contacts in the population and extract list of individuals met by each Agent
####
#edits for facility here
####
#work_contacts <- ContactsGen(scenario_parameters$crews_by_team, scenario_parameters$crew_sizes,
#                             scenario_parameters$rates, scenario_parameters$average)

source('custom-contacts-gen-general.R')
contacts_matrices = facility_contacts_gen(workers_per_line = workers_per_crew, #keeping old names for analogous parameters in full_run *for now*
                                          n_lines = crews_per_supervisor,
                                          n_production_shifts = supervisors,
                                          n_shift_floaters = n_shift_floaters,
                                          n_cleaners = n_cleaners,
                                          n_all_floaters = n_all_floaters)
production_shift_1 = contacts_matrices[['production_shift_1']]
production_shift_2 = contacts_matrices[['production_shift_2']]
cleaning_shift_full = contacts_matrices[['cleaning_shift_full']]
shift_sum =  contacts_matrices[['shift_sum']]
N <<- dim(production_shift_1)[1] #little kludgey

####
#Need to make proper sleep vs. at-home distinction for Shared housing
####
sleep_contacts = matrix(0, N, N)

#if(scenario_parameters$housing_dormitory) {
#    dormitory_contacts = matrix(scenario_parameters$dormitory_intensity/N, N, N)
#} else {
#    dormitory_contacts = sleep_contacts
#}
lambda_home = scenario_parameters$lambda

#contacts_list = list(work = work_contacts * 7/5, #to account for two days off per week
#                     home = dormitory_contacts * 7 / 9, #to account for two home shifts per day off
#                     sleep = sleep_contacts)
#print('step 1')

scaling_factor = scenario_parameters[['average']] / colSums(shift_sum)[1]
contacts_list = list(ps_1 = production_shift_1 * scaling_factor,
                     ps_2 = production_shift_2 * scaling_factor,
                     cs = cleaning_shift_full * scaling_factor,
                     weekend_ps_1 = sleep_contacts,
                     weekend_ps_2 = sleep_contacts,
                     weekend_cs = sleep_contacts)
#lambda_list = list(work = 0,
#                   home = lambda_home,
#                   sleep = 0)
####
#following is wrong, but a starting point
####
#print('step 2')

####
#need to systematize this instead of just kludging it for the demo value
####
psX_only_size = 1 + workers_per_crew * crews_per_supervisor + n_shift_floaters
if(supervisors > 1) {
    on_ps_1 = c(1/3, rep(1, psX_only_size), rep(0, psX_only_size), rep(0, n_cleaners), rep(1/3, n_all_floaters))
    on_ps_2 = c(1/3, rep(0, psX_only_size), rep(1, psX_only_size), rep(0, n_cleaners), rep(1/3, n_all_floaters))
    on_cs = c(1/3, rep(0, 2 * psX_only_size), rep(1, n_cleaners), rep(1/3, n_all_floaters))
} else {
    on_ps_1 = c(1/2, rep(1, psX_only_size), rep(0, n_cleaners), rep(1/2, n_all_floaters))
    on_ps_2 = rep(0, 1 + psX_only_size + n_cleaners + n_all_floaters)
    on_cs = c(1/2, rep(0, psX_only_size), rep(1, n_cleaners), rep(1/2, n_all_floaters))
} 


#on_ps_1 = c(1/3, rep(1, 41), rep(0, 41), rep(0, 10), rep(1/3, 10))
#on_ps_2 = c(1/3, rep(0, 41), rep(1, 41), rep(0, 10), rep(1/3, 10))
#on_cs =   c(1/3, rep(0, 41), rep(0, 41), rep(1, 10), rep(1/3, 10))

###
#working on proper dormitory_contacts parameters
#treating dormitory as the shift after work (or work shift and shift after work on the weekend)
#Actually, this really belongs in ContactsGen or its replacement
#Because it's actually non-trivial . . . except that I see that the dormitory contacts parameter in the core v1.1.4 version has a small flaw (diagonals are not excluded). So that needs to be fixed there; for now, we can do the simple thing here
###
#NB: Diagonals are intentionally not excluded here for maximum comparability
#at_home_ps_1 = rbind(c(1/3, rep(0, 31), rep(0, 31), rep(1/3, 10), rep(1/9, 10)),


####
#new parameter
#putting all between-shift floaters at start of day for testing purposes only
####
agent_presence_list = list(ps_1 = ifelse(ceiling(on_ps_1), TRUE, FALSE),
                           ps_2 = ifelse(floor(on_ps_2), TRUE, FALSE),
                           cs =   ifelse(floor(on_cs), TRUE, FALSE),
                           weekend_ps_1 = FALSE,
                           weekend_ps_2 = FALSE,
                           weekend_cs = FALSE)

quantitative_presence_list = list(ps_1 = on_ps_1,
                           ps_2 = on_ps_2,
                           cs =   on_cs,
                           weekend_ps_1 = 0,
                           weekend_ps_2 = 0,
                           weekend_cs = 0)

####
#
####
lambda_list = list(ps_1 = on_cs * lambda_home,
                   ps_2 = on_ps_1 * lambda_home,
                   cs = on_ps_2 * lambda_home,
                   weekend_ps_1 = (on_cs + on_ps_1) * lambda_home,
                   weekend_ps_2 = (on_ps_1 + on_ps_2) * lambda_home,
                   weekend_cs = (on_ps_2 + on_cs) * lambda_home)

#vaccination_rate_list = list(work = 0,
#                             home = scenario_parameters$home_vaccination_rate * 7 / 9, #to account for two home shifts per day off
#                             sleep = 0)
#print('step 3')

####
#going to regularize this to 1/7 chance each day, the shift after work (or 1/21 chance every shift for between-shift floaters)
#this is needed for sanity (and might not be a bad idea to retcon on produce farm
####


vaccination_rate_list = list(ps_1 = on_cs * scenario_parameters$home_vaccination_rate,
                             ps_2 = on_ps_1 * scenario_parameters$home_vaccination_rate,
                             cs   = on_ps_2 * scenario_parameters$home_vaccination_rate)
vaccination_rate_list[['weekend_ps_1']] = vaccination_rate_list[['ps_1']]
vaccination_rate_list[['weekend_ps_2']] = vaccination_rate_list[['ps_2']]
vaccination_rate_list[['weekend_cs']] = vaccination_rate_list[['cs']]

#workday = c('work', 'home', 'sleep')
#day_off = c('home', 'home', 'sleep')
#print('step 4')
workday = c('ps_1', 'ps_2', 'cs')
day_off = c('weekend_ps_1', 'weekend_ps_2', 'weekend_cs')
week = c(rep(workday, 5), rep(day_off, 2))
schedule = rep(week, ceiling(days/7))[1:(3 * days)]
#step_length_list = list(home = 1/3, work = 1/3, sleep = 1/3)
step_length_list = list(ps_1 = 1/3, ps_2 = 1/3, cs = 1/3, weekend_ps_1 = 1/3, weekend_ps_2 = 1/3, weekend_cs = 1/3)
#testing_rate_list = list(home = 0, work = get('work_testing_rate', scenario_parameters), sleep = 0)
testing_rate_list = list(ps_1 = on_ps_1 * get('work_testing_rate', scenario_parameters),
                         ps_2 = on_ps_2 * get('work_testing_rate', scenario_parameters),
                         cs =   on_cs * get('work_testing_rate', scenario_parameters),
                         weekend_ps_1 = 0,
                         weekend_ps_2 = 0,
                         weekend_cs = 0)
steps = scenario_parameters$nTime1 * 3
step_index = (1:steps) * (1/3) #step_length

#waning parameter time
#assuming waning to full susceptibility to infection, but no loss of protection against severe infection conditional upon infection
#loosely based on https://pubmed.ncbi.nlm.nih.gov/34619098/
waning_parameters = list(W_susceptibility = 1, 
                         R_susceptibility = vaccine_parameters$V2_susceptibility,
                         W_symptoms = 1, #vaccine_parameters$V2_symptoms, #worst-case (sort of)
                         R_symptoms = vaccine_parameters$V2_symptoms
                         #waning_rate = log(.88 / .47) / (4/12 * 365.2425)
                         #waning_rate = log(.7/.45) / (10/52 * 365.2425) #not
                         #directly necessary, if an explicit distribution is
                         #used
)

#print(waning_parameters)

###### code to run simulation with num_sims iterations
if(!exists('FIXED_SEED') || FIXED_SEED == TRUE) {
    set.seed(-778276078) #random 32-bit signed integer generated using atmospheric noise
                         #for reproducible output
}
#full_output = array(0, c(steps, 35, num_sims)) #TBD: This might not be enough
                                               #TBD: Adjust code to do this on
                                               #run 1
    #doing this now
#fuller_output = list()

#print('main loop')
sys_time_start = Sys.time()
for (i in 1:num_sims) {
    #print('iteration')
    agents <- AgentGen(N, E0 = n_exposed, IA0 = 0, IP0 = 0, IM0 = n_mild, initial_recovered = initial_recovered, initial_V1 = initial_V1, initial_V2 = initial_V2, SEVERE_MULTIPLIER = SEVERE_MULTIPLIER)
#print('agents done')
    agents$V1_symptomatic = agents$symptomatic & rbinom(N, 1, vaccine_parameters$V1_symptoms)
    agents$V2_symptomatic = agents$symptomatic & rbinom(N, 1, vaccine_parameters$V2_symptoms)
    agents$W_symptomatic = agents$symptomatic & rbinom(N, 1, waning_parameters$W_symptoms) #need to add these
    agents$R_symptomatic = agents$symptomatic & rbinom(N, 1, waning_parameters$R_symptoms) #need to add these
    agents$B_symptomatic = agents$symptomatic & rbinom(N, 1, vaccine_parameters$B_symptoms)
    #print('half')
    if(variant == 'omicron') {
        agents$duration_V2 = runif(N, 0, 19 * 7) #rough approximation from update 33 #rexp(N, waning_parameters$waning_rate)
        agents$duration_R = runif(N, 0, 19 * 7) #imitating above in absence of better data #rexp(N, waning_parameters$waning_rate)
        agents$duration_B = runif(N, 0, 28 * 7) #from update 33
    } else if(variant == 'delta') {
        #below is old delta parameters, for the moment
        #TBD: what goes here based on update 33?
        agents$duration_V2 = rexp(N, log(.88 / .47) / (4/12 * 365.2425))#waning_parameters$waning_rate)
        agents$duration_R =  rexp(N, log(.88 / .47) / (4/12 * 365.2425))#waning_parameters$waning_rate)
    } else if(variant == '2020') {
        agents$duration_V2 = 10/12 * 365.2425 #TBD: Find better value
        agents$duration_R = 10/12 * 365.2425
        agents$duration_B = Inf #Lol, whatever (i.e., good enough until a better waning mechanism is implemented anyway
        #TBD: fix 2020
    } else {
        stop(paste('unimplemented variant:', variant))
    }
                                                
    model <- ABM(agents, contacts_list = contacts_list,
                 lambda_list = lambda_list, schedule = schedule,
                 virus_parameters, testing_parameters, vaccine_parameters, scenario_parameters,
                 steps = steps, step_length_list = step_length_list,
                 testing_rate_list = testing_rate_list,
                 vaccination_rate_list = vaccination_rate_list,
                 agent_presence_list = agent_presence_list,
                 quantitative_presence_list = quantitative_presence_list,
                 waning_parameters = waning_parameters,
                 boosting_rate = boosting_rate)
    agents = model$agents
    output = model$Out1

    #agentss = model$agentss
    #fuller_output[[i]] = agentss
    if(i == 1) {
        full_output = array(0, c(steps, dim(as.matrix(output))[2], num_sims)) #guarantee it gets created with the right number of return variables
    }
    full_output[,,i] = as.matrix(output) #this works; for whatever reason,
                                         #as.array does not
} # for (i in 1:num_sims)

sys_time_end = Sys.time()
cat(sys_time_end - sys_time_start, 'for', row_name,'\n')

colnames(full_output) = colnames(output)
saveRDS(full_output, full_output_save_name)
#saveRDS(fuller_output, paste0(full_output_save_name,'-fuller'))
} #main_produce_farm_fn 
