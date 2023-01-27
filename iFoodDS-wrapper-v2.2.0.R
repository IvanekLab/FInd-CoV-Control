# iFoodDS-wrapper-v2.2.0.R is part of Food INdustry CoViD Control Tool
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

#Usage example, with default values:
#TBD (eventually): fill in

#Note: Only one of social_distancing_shared_housing and community_transmission
#will be used; a good practice is to set the other one to something invalid
#(e.g., NULL) as a failsafe.
#Set analyze_only to TRUE to reanalyze an existing output set with modified
#analyze.R

source('general-waning-functions-v2.2.0.R')
source('constants.R') #constants for sensitivity testing, consistency, clarity, etc.

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
                    fraction_boosted_ever,
                    fraction_boosted_last_five_months,
                    working_directory,
                    folder_name,
                    unique_id, 
                    variant,
                    analyze_only,
                    PARALLEL,
                    protection_functions,
                    output_per_week, #= 1680000, # / (5 * (1 + (supervisors > 1))) #N * 60.1 * 4 #wrong, but it's okay
                    hourly_wage, # = 13.89
                    size,# = 1000
                    #sensitivity_variable,
                    #sensitivity_multiplier
                    kConstants
) {
    setwd(working_directory)

    workers_per_crew = safe.integer(workers_per_crew)
    crews_per_supervisor = safe.integer(crews_per_supervisor)
    supervisors = safe.integer(supervisors)
    days = safe.integer(days)
    n_no_symptoms = safe.integer(n_no_symptoms)
    n_mild = safe.integer(n_mild)

    fraction_recovered = safe.numeric(fraction_recovered)
    fraction_fully_vaccinated = safe.numeric(fraction_fully_vaccinated)
    ffv_last_five_months = safe.numeric(ffv_last_five_months)
    fraction_boosted_ever = safe.numeric(fraction_boosted_ever)
    fraction_boosted_last_five_months = safe.numeric(fraction_boosted_last_five_months)
    output_per_week = safe.numeric(output_per_week)
    hourly_wage = safe.numeric(hourly_wage)

    analyze_only = safe.logical(analyze_only)
    PARALLEL = safe.logical(PARALLEL)

    #variant = tolower(variant)
    #if(variant == 'delta') {
        #DELTA = safe.logical(DELTA)
        #SEVERE_MULTIPLIER = safe.numeric(SEVERE_MULTIPLIER)
        #DELTA = TRUE
    #    SEVERE_MULTIPLIER = 2
    #} else if(variant == 'omicron') {
    #    SEVERE_MULTIPLIER = 1.2
    #} else if(variant == '2020'){
    #    SEVERE_MULTIPLIER = 1
    #} else {
    #    stop(paste('Unsupported variant:', variant))
    #}
    SEVERE_MULTIPLIER = get('SEVERE_MULTIPLIER', kConstants)
    
    farm_or_facility == tolower(farm_or_facility)
    if(farm_or_facility == 'farm') {
        if(
            workers_per_crew == 10 &&
            crews_per_supervisor == 3 &&
            supervisors == 3 &&
            #n_shift_floaters == NULL &&
            #n_cleaners == NULL &&
            #n_all_floaters == NULL &&
            days == 90 &&
            tolower(employee_housing) == 'shared' &&
            tolower(social_distancing_shared_housing) == 'intermediate' &&
            #community_transmission == NULL &&
            tolower(social_distancing_work) == 'intermediate' &&
            n_no_symptoms == 1 &&
            n_mild == 0 &&
            fraction_recovered == 0.69 &&
            fraction_fully_vaccinated == 0.71 &&
            ffv_last_five_months == 0.09 &&
            fraction_boosted_ever == 0.45 &&
            fraction_boosted_last_five_months == 0.45
            ### Provisionally ignoring output_per_week, hourly_wage, and, iff
            ### applicable, size when assessing "defaultness"
        ) {
            unique_id = paste(unique_id, 'baseline', sep = '')
        }

        crews_by_team = rep(crews_per_supervisor, supervisors) 
        crew_sizes = rep(workers_per_crew, crews_per_supervisor * supervisors)

        N = sum(crew_sizes) + length(crew_sizes) + supervisors + 1 
    } else if(farm_or_facility == 'facility') {
        
        n_shift_floaters = safe.integer(n_shift_floaters)
        n_cleaners = safe.integer(n_cleaners)
        n_all_floaters = safe.integer(n_all_floaters) - 1 #because the internal mechanics of the code are a holdover from a definition that excluded the manager
        size = safe.numeric(size)

        if(
            workers_per_crew == 10 &&
            crews_per_supervisor == 3 &&
            supervisors == 2 &&
            n_shift_floaters == 10 &&
            n_cleaners == 10 &&
            n_all_floaters == 10 &&
            days == 90 &&
            tolower(employee_housing) %in% c('individual', 'private') &&
            #social_distancing_shared_housing == NULL &&
            tolower(community_transmission) == 'intermediate' &&
            tolower(social_distancing_work) == 'intermediate' &&
            n_no_symptoms == 1 &&
            n_mild == 0 &&
            fraction_recovered == 0.69 &&
            fraction_fully_vaccinated == 0.71 &&
            ffv_last_five_months == 0.09 &&
            fraction_boosted_ever == 0.45 &&
            fraction_boosted_last_five_months == 0.45
            ### Provisionally ignoring output_per_week, hourly_wage, and, if
            ### applicable, size when assessing "defaultness"
        ) {
            unique_id = paste(unique_id, 'baseline', sep = '')
        }

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
            double_wrap_community_foi = 1e-5#0.0005
        } else if(tolower(community_transmission) == 'intermediate'){
            double_wrap_community_foi = 0.001
        } else if(tolower(community_transmission) == 'high'){
            double_wrap_community_foi = 0.01
        } else {
            double_wrap_community_foi = safe.numeric(community_transmission)
            #stop(paste('Invalid community_transmission:',
            #           community_transmission))
        }
        if(variant == 'delta' || variant == 'omicron') {
            double_wrap_community_foi = 2 * double_wrap_community_foi
        #} else if (variant == 'omicron') {
        #    double_wrap_community_foi = 4 * double_wrap_community_foi
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
            dormitory_R0 = safe.numeric(social_distancing_shared_housing)
            #stop(paste('Invalid social_distancing_shared_housing:',
            #           social_distancing_shared_housing))
        }
        if(variant == 'delta' || variant == 'omicron') {
            dormitory_R0 = 2 * dormitory_R0
        #} else if (variant == 'omicron') {
        #    dormitory_R0 = 4 * dormitory_R0
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
        double_wrap_baseline_work_R0 = safe.numeric(social_distancing_work)
        #stop(paste('Invalid social_distancing_work:', social_distancing_work))
    }

    if(variant == 'delta' || variant == 'omicron') {
        double_wrap_baseline_work_R0 = double_wrap_baseline_work_R0 * 2
        #DELTA_VAX = TRUE
    #} else if (variant == 'omicron') {
    #        #double_wrap_baseline_work_R0 = double_wrap_baseline_work_R0 * 4
    #    double_wrap_baseline_work_R0 = double_wrap_baseline_work_R0 #* 7/3
    #    #kludged for sane values aiming for 7; make more precise determination
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
        source('double-wrapped-v2.2.0.R', local = TRUE)
        #if(!is.null(sensitivity_variable)) {
        #    if(sensitivity_variable %in% names(kConstants)) {
        #        kConstants[[sensitivity_variable]] = sensitivity_multiplier * kConstants[[sensitivity_variable]]
        #    } else {
        #        stop('Not a valid sensitivity_variable: ', sensitivity_variable)
        #    }
        #}
        protection_functions = make_protection_functions_general(kConstants)
        double_wrapped_fn(kConstants, protection_functions)
    }
    steps = days * 3
    step_index = (1:steps) * (1/3)
    source('analyze-v2.2.0.R', local = TRUE)
    analyze_fn()
}

FIXED_SEED = TRUE
VERSION = '2.2.0'
double_wrap_num_sims = 100#0

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
    folder_name = 'scenario-analysis-take-2',#'debugging-tests',   # relative to working directory
    analyze_only = 'FALSE',
    PARALLEL = TRUE,
    fraction_recovered = 0.69,
    fraction_fully_vaccinated = 0.71,
    ffv_last_five_months = 0.09,
    fraction_boosted_ever = 0.45,
    fraction_boosted_last_five_months = 0.45,
    #protection_functions = protection_functions_45,
    variant = 'omicron'
)

additional_facility_parameters = list(
    farm_or_facility = 'facility',
    supervisors = '2',          # FM: shifts
    n_shift_floaters ='10',     # FM only (for farm model, will require NULL/NA)
    n_cleaners = '10',          # FM only (for farm model, will require NULL/NA)
    n_all_floaters = '11',      # FM only (for farm model, will require NULL/NA)
    employee_housing = 'Private', 
    social_distancing_shared_housing = NULL,
    community_transmission = 'Intermediate',
    #unique_id = 'facility-pass-1',
    output_per_week = 784346.67, #1680000 , #N * 60.1 * 4 #wrong, but it's okay
    hourly_wage = 13.89,
    size = 1000

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
    #unique_id = 'farm-default-v24', #actually lower, but going for consistency
    output_per_week = 247612.00,#1680000, #/ (5 * (1 + (supervisors > 1))) #N * 60.1 * 4 #wrong, but it's okay
    hourly_wage = 13.89,
    size = NA
)

double_wrap_num_sims = 1000

do.call(full_run, c(common_parameters, additional_farm_parameters, list(unique_id = 'farm-shared-random-start', kConstants = kConstants)))
do.call(full_run, c(common_parameters, additional_facility_parameters, list(unique_id = 'facility-individual-random-start', kConstants = kConstants)))
#double_wrap_num_sims = 10
#all_params = c(common_parameters, additional_facility_parameters, list(unique_id = 'facility-individual', kConstants = kConstants))
#all_params[['analyze_only']] = TRUE
#do.call(full_run, all_params)
#stop('OKAY!')
#double_wrap_num_sims = 1000
#do.call(full_run, all_params)
#stop('Hold on here for now.')
all_params = c(common_parameters, additional_farm_parameters, list(unique_id = 'farm-individual-random-start', kConstants = kConstants))
all_params$employee_housing = 'individual'
all_params$social_distancing_shared_housing = NULL
all_params$community_transmission = 'intermediate'
do.call(full_run, all_params)
all_params = c(common_parameters, additional_facility_parameters, list(unique_id = 'facility-shared-random-start', kConstants = kConstants))
all_params$employee_housing = 'shared'
all_params$social_distancing_shared_housing = 'intermediate'
all_params$community_transmission = NULL
do.call(full_run, all_params)

#do.call(full_run, c(common_parameters, additional_farm_parameters))
#common_parameters[['analyze_only']] = TRUE
#do.call(full_run, c(common_parameters, additional_facility_parameters))
#kConstants_ = kConstants"

run_67 = function(common_parameters, additional_facility_parameters,
                  additional_farm_parameters, kConstants, farm_or_facility,
                  changed_parameters, parameters_to_test = NULL) {
    farm_or_facility == tolower(farm_or_facility)
    if(farm_or_facility == 'farm') {
        additional_parameters = additional_farm_parameters
    } else if(farm_or_facility == 'facility') {
        additional_parameters = additional_facility_parameters
    } else {
        stop('Invalid farm_or_facility:', farm_or_facility)
    }

    all_parameters = c(common_parameters, additional_parameters)
    all_parameters[names(changed_parameters)] = changed_parameters
    all_parameters[['kConstants']] = kConstants
    #extra_parameters = list()
    #for(name in names(changed_parameters)) {
        #if(name %in% names(common_parameters)) {
        #    common_parameters[[name]] = changed_parameters[[name]]
        #} else if(name %in% names(additional_parameters)) {
        #} else{
        #    additional_parameters[[name]] = changed_parameters[[name]]
        #}
    #}

    writeLines('\nNULL')
    ccl = check_consistency(kConstants)
    if(!get('consistent', ccl)) {
        stop('ERROR! BASE PARAMETERS INCONSISTENT!')
    }
    #additional_facility_parameters[['employee_housing']] = 'Shared'
    #additional_facility_parameters[['social_distancing_shared_housing']] = 'Intermediate'
    #additional_facility_parameters[['community_transmission']] = NULL 

    do.call(full_run, all_parameters)
    #stop('Test here.')
    #for (sensitivity_variable in c('SEVERE_MULTIPLIER',
    #                               'R_question_period',
    #                               'time_since_first_V2',
    #                               'p_trans_IP',
    #                               'p_trans_IA',
    #                               'p_trans_IM')) {
    #cat('ASS\nASS\nASS\nASS!\n')
    unique_id = all_parameters[['unique_id']]
    if(is.null(parameters_to_test)) {
        parameters_to_test = names(kConstants)
    }
    for(sensitivity_variable in parameters_to_test) {
        for(sensitivity_multiplier in c(0.5, 1.5)) {
            kConstants_ = kConstants
            #if(!is.null(sensitivity_variable)) {
            if(sensitivity_variable %in% names(kConstants_)) {
                kConstants_[[sensitivity_variable]] = sensitivity_multiplier * kConstants_[[sensitivity_variable]]
            } else {
                stop('Not a valid sensitivity_variable: ', sensitivity_variable)
            }
            ccl = check_consistency(kConstants_, altered_single_parameter = sensitivity_variable)
            kConstants_fixed = get('fixed_constants', ccl)
            if(!get('consistent', ccl) && !get('fixed', ccl)) {
                stop('Unfixable constants')
            }
            sensitivity_multiplier = get(sensitivity_variable, kConstants_fixed) / get(sensitivity_variable, kConstants)
            #}
            writeLines(paste0('###\n###\n', sensitivity_variable, ' x ', sensitivity_multiplier, '\n###\n###\n'))

            all_parameters[['kConstants']] = kConstants_
            all_parameters[['unique_id']] = paste0(unique_id, '-', sensitivity_variable, '-', sensitivity_multiplier)
            do.call(full_run, all_parameters)
        }
    }
}

"run_67(common_parameters, additional_facility_parameters,
       additional_farm_parameters, kConstants, 'facility',
       list(unique_id = 'facility'
       )
)

run_67(common_parameters, additional_facility_parameters,
       additional_farm_parameters, kConstants, 'facility',
       list(unique_id = 'facility-start-of-epidemic',
            fraction_recovered = 0,
            fraction_fully_vaccinated = 0,
            ffv_last_five_months = 0,
            fraction_boosted_ever = 0,
            fraction_boosted_last_five_months = 0
       )
)

run_67(common_parameters, additional_facility_parameters,
       additional_farm_parameters, kConstants, 'facility',
       list(unique_id = 'facility-no-vax',
            fraction_fully_vaccinated = 0,
            ffv_last_five_months = 0,
            fraction_boosted_ever = 0,
            fraction_boosted_last_five_months = 0
       )
)

run_67(common_parameters, additional_facility_parameters,
       additional_farm_parameters, kConstants, 'facility',
       list(unique_id = 'facility-no-recovered',
            fraction_recovered = 0
       )
)

run_67(common_parameters, additional_facility_parameters,
       additional_farm_parameters, kConstants, 'facility',
       list(unique_id = 'farmlike-facility',
            employee_housing = 'Shared', 
            social_distancing_shared_housing = 'Intermediate',
            community_transmission = NULL
       )
)

run_67(common_parameters, additional_facility_parameters,
       additional_farm_parameters, kConstants, 'facility',
       list(unique_id = 'farmlike-facility-start-of-epidemic',
            fraction_recovered = 0,
            fraction_fully_vaccinated = 0,
            ffv_last_five_months = 0,
            fraction_boosted_ever = 0,
            fraction_boosted_last_five_months = 0,
            employee_housing = 'Shared', 
            social_distancing_shared_housing = 'Intermediate',
            community_transmission = NULL
       )
)

run_67(common_parameters, additional_facility_parameters,
       additional_farm_parameters, kConstants, 'facility',
       list(unique_id = 'farmlike-facility-no-vax',
            fraction_fully_vaccinated = 0,
            ffv_last_five_months = 0,
            fraction_boosted_ever = 0,
            fraction_boosted_last_five_months = 0,
            employee_housing = 'Shared', 
            social_distancing_shared_housing = 'Intermediate',
            community_transmission = NULL
       )
)

run_67(common_parameters, additional_facility_parameters,
       additional_farm_parameters, kConstants, 'facility',
       list(unique_id = 'farmlike-facility-no-recovered',
            fraction_recovered = 0,
            employee_housing = 'Shared', 
            social_distancing_shared_housing = 'Intermediate',
            community_transmission = NULL
       )
)"

