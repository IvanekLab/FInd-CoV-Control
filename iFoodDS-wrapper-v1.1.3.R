#Usage example, with default values:
#TBD (eventually): fill in

#Note: Only one of social_distancing_shared_housing and community_transmission
#will be used; a good practice is to set the other one to something invalid
#(e.g., NULL) as a failsafe.
#Set analyze_only to TRUE to reanalyze an existing output set with modified
#analyze.R

source('general-waning-functions.R')

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

#removing default values from testing code to ensure that accidental omission
#doesn't generate stupid results
full_run = function(
                    farm_or_facility,
                    workers_per_crew,
                    crews_per_supervisor,
                    supervisors,
                    n_shift_floaters,
                    n_cleaners,
                    n_all_floaters,
                    days,
                    employee_housing,
                    social_distancing_shared_housing,
                    community_transmission,
                    social_distancing_work,
                    n_no_symptoms,
                    n_mild,
                    fraction_recovered,
                    fraction_fully_vaccinated,
                    ffv_last_five_months,
                    fraction_boosted, #TBD (eventually): possibly rename?
                    working_directory,
                    folder_name,
                    unique_id, 
                    variant,
                    analyze_only,
                    PARALLEL,
                    protection_functions
) {
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

    variant = tolower(variant)
    if(variant == 'delta') {
        #DELTA = safe.logical(DELTA)
        #SEVERE_MULTIPLIER = safe.numeric(SEVERE_MULTIPLIER)
        #DELTA = TRUE
        SEVERE_MULTIPLIER = 2
    } else if(variant == 'omicron') {
        SEVERE_MULTIPLIER = 1.2
    } else if(variant == '2020'){
        SEVERE_MULTIPLIER = 1
    } else {
        stop(paste('Unsupported variant:', variant))
    }

    analyze_only = safe.logical(analyze_only)
    PARALLEL = safe.logical(PARALLEL)

    
    farm_or_facility == tolower(farm_or_facility)
    if(farm_or_facility == 'farm') {

        if(
            workers_per_crew == 10 &&
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
            fraction_fully_vaccinated == .627
        ) {
            unique_id = paste(unique_id, 'baseline', sep = '')
        }

        crews_by_team = rep(crews_per_supervisor, supervisors) 
        crew_sizes = rep(workers_per_crew, crews_per_supervisor * supervisors)

        N = sum(crew_sizes) + length(crew_sizes) + supervisors + 1 
    } else if(farm_or_facility == 'facility') {

        #TBD (eventually): add "default" facility conditions

        N = (1 +
             supervisors * (1 + workers_per_crew * crews_per_supervisor +
                            n_shift_floaters) +
             n_cleaners + n_all_floaters
        )
        crews_by_team = rep(crews_per_supervisor, supervisors) 
        crew_sizes = rep(workers_per_crew, crews_per_supervisor * supervisors)

    } else {
        stop('Invalid value for farm_or_facility: ', farm_or_facility)
    }
    
    if((tolower(employee_housing) == 'private') ||
       (tolower(employee_housing) == 'individual')) {
        housing_dormitory = FALSE
        dormitory_R0 = 0 

        if(tolower(community_transmission) == 'low') {
            double_wrap_community_foi = 0.0005
        } else if(tolower(community_transmission) == 'intermediate'){
            double_wrap_community_foi = 0.001
        } else if(tolower(community_transmission) == 'high'){
            double_wrap_community_foi = 0.01
        } else {
            stop(paste('Invalid community_transmission:',
                       community_transmission))
        }
        if(variant == 'delta') {
            double_wrap_community_foi = 2 * double_wrap_community_foi
        } else if (variant == 'omicron') {
            double_wrap_community_foi = 4 * double_wrap_community_foi
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
            stop(paste('Invalid social_distancing_shared_housing:',
                       social_distancing_shared_housing))
        }
        if(variant == 'delta') {
            dormitory_R0 = 2 * dormitory_R0
        } else if (variant == 'omicron') {
            dormitory_R0 = 4 * dormitory_R0
        }
    } else {
        stop(paste('Invalid employee_housing:', employee_housing))
    }

    if(tolower(social_distancing_work) == 'high') {           
        double_wrap_baseline_work_R0 = 2
    } else if(tolower(social_distancing_work) == 'intermediate') {
        double_wrap_baseline_work_R0 = 3
    } else if(tolower(social_distancing_work) == 'low') {  
        double_wrap_baseline_work_R0 = 4
    } else {
        stop(paste('Invalid social_distancing_work:', social_distancing_work))
    }

    if(variant == 'delta') {
        double_wrap_baseline_work_R0 = double_wrap_baseline_work_R0 * 2
        #DELTA_VAX = TRUE
    } else if (variant == 'omicron') {
            #double_wrap_baseline_work_R0 = double_wrap_baseline_work_R0 * 4
        double_wrap_baseline_work_R0 = double_wrap_baseline_work_R0 #* 7/3
        #kludged for sane values aiming for 7; make more precise determination
    }

    n_exposed = n_no_symptoms # this should perhaps be split up at some point
                              # into exposed, pre-symptomatic, and asymptomatic;
                              # the difference is small, though

    subdirectory = paste(folder_name, '/', sep = '')
    dir.create(subdirectory)
    if(analyze_only) { # these should be saved in a separate file
                       # once we start having more complex schedules
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
}

FIXED_SEED = TRUE
VERSION = '1.1.3'
double_wrap_num_sims = 1000

#note that several of these parameters are not actually used (no longer true?)
#separating into one variable per line for comments and diffing
#here using all variable names explicitly, so that errors fail loudly instead of
#giving weird bugs
#(note that a word diff ignoring whitespace vs. function definition is now
#relatively straightforward, if function definition is similarly formatted):
#copy the relevant signatures to two files, strip out comments, strip out ,s and
#then run
#git diff --no-index --word-diff --ignore-all-space a.txt b.txt
common_parameters = list(
    workers_per_crew = '10',    # FM: workers per line
    crews_per_supervisor = '3', # FM: / lines per shift
    days = '90',
    social_distancing_work = 'Intermediate',
    n_no_symptoms = '1',        #i.e., exposed 
    n_mild = '0',
    working_directory = '.',
    folder_name = 'server-copied',   # relative to working directory
    analyze_only = 'TRUE',
    PARALLEL = TRUE,
    fraction_recovered = 0.69,
    fraction_fully_vaccinated = 0.71,
    ffv_last_five_months = 0.09,
    fraction_boosted = 0.45,
    protection_functions = default_protection_functions,
    variant = 'omicron'
)

additional_facility_parameters = list(
    farm_or_facility = 'facility',
    supervisors = '2',          # FM: shifts
    n_shift_floaters ='10',     # FM only (for farm model, will require NULL/NA)
    n_cleaners = '10',          # FM only (for farm model, will require NULL/NA)
    n_all_floaters = '10',      # FM only (for farm model, will require NULL/NA)
    employee_housing = 'Private', 
    social_distancing_shared_housing = NULL,
    community_transmission = 'Intermediate',
    
    unique_id = 'facility-default-v14'
)

additional_farm_parameters = list(
    farm_or_facility = 'farm',
    supervisors = '3',          # FM: shifts
    n_shift_floaters ='0',      # FM only (for farm model, will require NULL/NA)
    n_cleaners = '0',           # FM only (for farm model, will require NULL/NA)
    n_all_floaters = '0',       # FM only (for farm model, will require NULL/NA)
    employee_housing = 'Shared', 
    social_distancing_shared_housing = 'Intermediate',
    community_transmission = NULL,
    
    unique_id = 'farm-default-v14' #actually lower, but going for consistency
)

do.call(full_run, c(common_parameters, additional_farm_parameters))
do.call(full_run, c(common_parameters, additional_facility_parameters))

