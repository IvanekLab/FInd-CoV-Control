#Usage example, with default values:
#full_run(10, 3, 3, 90, 'Private', NULL, 'Intermediate', 'Intermediate', 1, 0, .2, .4, 'iFoods-Private')
#Or for shared housing:
#full_run(10, 3, 3, 90, 'Shared', 'Intermediate', 'NULL', 'Intermediate', 1, 0, .2, .4, 'iFoods-Public')

#Note: Only one of social_distancing_shared_housing and community_transmission
#will be used; a good practice is to set the other one to something invalid
#(e.g., NULL) as a failsafe.
#Set analyze_only to TRUE to reanalyze an existing output set with modified
#analyze.R

safe.integer = function(s) {
    i = strtoi(s)
    if(is.na(i)) {
        stop(paste('Not a valid integer:', s))
    }
    i
}

safe.numeric = function(s) {
    r = as.numeric(s)
    if(is.na(r)) {
        stop(paste('Not a valid real number:', s))
    }
    r
}

safe.logical = function(s) {
    b = as.logical(s)
    if(is.na(b)) {
        stop(paste('Not a valid boolean:', s))
    }
    b
}

full_run = function(workers_per_crew, crews_per_supervisor, supervisors,
                    n_shift_floaters, n_cleaners, n_all_floaters,
                    days, employee_housing, social_distancing_shared_housing,
                    community_transmission, social_distancing_work,
                    n_no_symptoms, n_mild, fraction_recovered,
                    fraction_fully_vaccinated,
                    working_directory,
                    folder_name,
                    unique_id, 
                    DELTA = TRUE,
                    analyze_only = 'FALSE',
                    SEVERE_MULTIPLIER = '2',
                    PARALLEL = 'FALSE') {
    setwd(working_directory)

    workers_per_crew = safe.integer(workers_per_crew)
    crews_per_supervisor = safe.integer(crews_per_supervisor)
    supervisors = safe.integer(supervisors)
    n_shift_floaters = safe.integer(n_shift_floaters)
    n_cleaners = safe.integer(n_cleaners)
    n_all_floaters = safe.integer(n_all_floaters)
    days = safe.integer(days)
    n_no_symptoms = safe.integer(n_no_symptoms)
    n_mild = safe.integer(n_mild)

    fraction_recovered = safe.numeric(fraction_recovered)
    fraction_fully_vaccinated = safe.numeric(fraction_fully_vaccinated)
    DELTA = safe.logical(DELTA)
    SEVERE_MULTIPLIER = safe.numeric(SEVERE_MULTIPLIER)

    analyze_only = safe.logical(analyze_only)
    PARALLEL = safe.logical(PARALLEL)

    if(workers_per_crew == 10 &&
       crews_per_supervisor == 3 &&
       supervisors == 3 &&
       days == 90 &&
       employee_housing == 'Shared' &&
       social_distancing_shared_housing == 'Intermediate' &&
       #community_transmission == NULL &&
       social_distancing_work == 'Intermediate' &&
       n_no_symptoms == 1 &&
       n_mild == 0 &&
       fraction_recovered == .116 &&
       fraction_fully_vaccinated == .627) {
        unique_id = paste(unique_id, 'baseline', sep = '')
    }

    crews_by_team = rep(crews_per_supervisor, supervisors) 
    crew_sizes = rep(workers_per_crew, crews_per_supervisor * supervisors) 
    N = sum(crew_sizes) + length(crew_sizes) + supervisors + 1 
    
    #days -- done

    if((tolower(employee_housing) == 'private') || (tolower(employee_housing) == 'individual')) {
        housing_dormitory = FALSE
        dormitory_R0 = 0 

        if(tolower(community_transmission) == 'low') {
            double_wrap_community_foi = 0.0005
        } else if(tolower(community_transmission) == 'intermediate'){
            double_wrap_community_foi = 0.001
        } else if(tolower(community_transmission) == 'high'){
            double_wrap_community_foi = 0.002
        } else {
            stop(paste('Invalid community_transmission:', community_transmission))
        }
        if(DELTA) {
            double_wrap_community_foi = 2 * double_wrap_community_foi
        }
    } else if(tolower(employee_housing) == 'shared') {
        housing_dormitory = TRUE
        double_wrap_community_foi = 0

        if(tolower(social_distancing_shared_housing) == 'high') {
            dormitory_R0 = 0.5 
        } else if(tolower(social_distancing_shared_housing) == 'intermediate') {
            dormitory_R0 = 1 
        } else if(tolower(social_distancing_shared_housing) == 'low') {
            dormitory_R0 = 2 
        } else {
            stop(paste('Invalid social_distancing_shared_housing:', social_distancing_shared_housing))
        }
        if(DELTA) {
            dormitory_R0 = 2 * dormitory_R0
        }
    } else {
        stop(paste('Invalid employee_housing:', employee_housing))
    }

    if(tolower(social_distancing_work) == 'high') {           #fixed 2021-08-17
        double_wrap_baseline_work_R0 = 2
    } else if(tolower(social_distancing_work) == 'intermediate') {
        double_wrap_baseline_work_R0 = 3
    } else if(tolower(social_distancing_work) == 'low') {  #fixed 2021-08-17
        double_wrap_baseline_work_R0 = 4
    } else {
        stop(paste('Invalid social_distancing_work:', social_distancing_work))
    }
    if(DELTA) {
        double_wrap_baseline_work_R0 = double_wrap_baseline_work_R0 * 2
        DELTA_VAX = TRUE
    }

    n_exposed = n_no_symptoms # done, although this should ideally be split up into exposed, pre-symptomatic, and asymptomatic; the difference is small, though
    #n_mild -- done
    #although note that we might consider in the future allowing initial exposed and mild to be drawn from the vaccinated
    #fraction_recovered -- done
    #fraction_fully_vaccinated -- done

    #subdirectory = paste(set_name, '-files/', sep = '')
    subdirectory = paste(folder_name, '/', sep = '')
    dir.create(subdirectory)
    #wd = getwd()
    #setwd(subdirectory)
    if(analyze_only) { # these should be saved in a separate file once we start having more complex schedules
    #    steps = days * 3
    #    step_index = (1:steps) * (1/3)
    } else {
        source('double-wrapped-v1.1.3.R', local = TRUE)
        double_wrapped_fn()
    }
    steps = days * 3
    step_index = (1:steps) * (1/3)
    source('analyze-v1.1.3.R', local = TRUE)
    analyze_fn()
    #setwd(wd)
}

FIXED_SEED = TRUE
VERSION = '1.1.3'
double_wrap_num_sims = 10#00


#note that several of these parameters are not actually used (no longer true?)
full_run('10', '3', '2', '10', '10', '10', '90', 'Shared', 'Intermediate', 'Intermediate', 'Intermediate', '1', '0', '.116', '.627', working_directory = '.', 'facility-added-interface', 'comparable', TRUE, analyze_only = 'FALSE', SEVERE_MULTIPLIER = '2', PARALLEL = TRUE)

