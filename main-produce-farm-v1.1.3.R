#ABM model
#individual based
#each person/agent has properties = parameters, attributes behaviours
#model each person individually
main_produce_farm_fn = function() { #will it work? the goal is to get more meaningful debug data
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
    #2021-07-18 Oh hey, forshadowing
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

#    cat(V2_susceptibility, V2_symptoms, V1_susceptibility, V1_symptoms,'\n')

    vaccine_parameters = list(    
    V1_susceptibility = V1_susceptibility,
    V2_susceptibility = V2_susceptibility,
    V1_symptoms = V1_symptoms,
    V2_symptoms = V2_symptoms,
    vaccination_interval = 21 #changed from 14 days to 21 days based on practice with Pfizer 
    )
} else {
    vaccine_parameters = list(    
        V1_susceptibility = 1 - .8,
        V2_susceptibility = 1 - .9,
        V1_symptoms = (1 - .88) / (1 - .8), #purely default; if I've seen an estimate of this, I do not recall it
        V2_symptoms = (1 - .94) / (1 - .9),
        vaccination_interval = 21 #changed from 14 days to 21 days based on practice with Pfizer 
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
                                         home_vaccination_rate = vaccination_rate, #to account for two home intervals on Day off 
                                         lambda = community_foi, #0.012, #0.012 baseline 
                                         crews_by_team = crews_by_team, 
                                         crew_sizes = crew_sizes, 
                                         rates = example_rates,
                                         virus_params = virus_parameters)


###################################
##################################

#create a nPop1 x nPop1 matrix for contacts in the population and extract list of individuals met by each Agent
####
#edits for facility here
####
#work_contacts <- ContactsGen(scenario_parameters$crews_by_team, scenario_parameters$crew_sizes,
#                             scenario_parameters$rates, scenario_parameters$average)

source('custom-contacts-gen-comparable.R')
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
on_ps_1 = c(1/3, rep(1, 31), rep(0, 31), rep(0, 10), rep(1/3, 10))
on_ps_2 = c(1/3, rep(0, 31), rep(1, 31), rep(0, 10), rep(1/3, 10))
on_cs =   c(1/3, rep(0, 31), rep(0, 31), rep(1, 10), rep(1/3, 10))

####
#new parameter
#putting all between-shift floaters at start of day for testing purposes only
####
agent_presence_list = list(ps_1 = ifelse(ceiling(on_ps_1), TRUE, FALSE),
                           ps_2 = ifelse(floor(on_ps_2), TRUE, FALSE),
                           cs =   ifelse(floor(on_ps_2), TRUE, FALSE),
                           weekend_ps_1 = FALSE,
                           weekend_ps_2 = FALSE,
                           weekend_cs = FALSE)

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


###### code to run simulation with num_sims iterations
#https://stackoverflow.com/questions/20730537/add-new-row-to-matrix-one-by-one/20730711
if(!exists('FIXED_SEED') || FIXED_SEED == TRUE) {
    set.seed(-778276078) #random 32-bit signed integer generated using atmospheric noise
                         #for reproducible output during development/debugging
                         #should be commented out for production use
}
####
#23 -> 25
####
full_output = array(0, c(steps, 25, num_sims))

sys_time_start = Sys.time()
for (i in 1:num_sims) {
    agents <- AgentGen(N, E0 = n_exposed, IA0 = 0, IP0 = 0, IM0 = n_mild, initial_recovered = initial_recovered, initial_V1 = initial_V1, initial_V2 = initial_V2, SEVERE_MULTIPLIER = SEVERE_MULTIPLIER)

    agents$V1_symptomatic = agents$symptomatic & rbinom(N, 1, vaccine_parameters$V1_symptoms)
    agents$V2_symptomatic = agents$symptomatic & rbinom(N, 1, vaccine_parameters$V2_symptoms)
#print('pre-ABM')
    model <- ABM(agents, contacts_list = contacts_list,
                 lambda_list = lambda_list, schedule = schedule,
                 virus_parameters, testing_parameters, vaccine_parameters, scenario_parameters,
                 steps = steps, step_length_list = step_length_list,
                 testing_rate_list = testing_rate_list,
                 vaccination_rate_list = vaccination_rate_list,
                 agent_presence_list = agent_presence_list)
#print('post-ABM')
    agents = model$agents
    output = model$Out1

    full_output[,,i] = as.matrix(output) #this works; for whatever reason,
                                         #as.array does not
} # for (i in 1:num_sims)

sys_time_end = Sys.time()
cat(sys_time_end - sys_time_start, 'for', row_name,'\n')

colnames(full_output) = colnames(output)
saveRDS(full_output, full_output_save_name)
} #main_produce_farm_fn 
