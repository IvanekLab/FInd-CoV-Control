# iFoodDS-wrapper-v1.1.3.R is part of Food INdustry CoViD Control Tool
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


# Main function to source(...), followed by a call to full_run(...)

#Note: Only one of social_distancing_shared_housing and community_transmission
#will be used; a good practice is to set the other one to something invalid
#(e.g., NULL) as a failsafe.
#Setting analyze_only to TRUE allows one to reanalyze an existing output set
#with a modified analyze.R; this is not used in production code, but is useful
#for development.

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

#in production use (with the web interface), all of the parameters with
#default values (DELTA, analyze_only, SEVERE_MULTIPLIER, and PARALLEL are
#left unchanged.
full_run = function(workers_per_crew, crews_per_supervisor, supervisors,
                    fraction_boosted, #TBD: implement
                    boosting_rate, #.05 arbitrary, probably high
                    days, employee_housing, social_distancing_shared_housing,
                    community_transmission, social_distancing_work,
                    n_no_symptoms, n_mild, fraction_recovered,
                    fraction_fully_vaccinated, # TBD: This is defined FOR NOW as fraction fully vax and NOT boosted
                    working_directory,
                    folder_name,
                    unique_id, 
                    variant = 'Delta',
                    analyze_only = 'FALSE',
                    #SEVERE_MULTIPLIER = '2',
                    PARALLEL = 'FALSE') {
    setwd(working_directory)

    workers_per_crew = safe.integer(workers_per_crew)
    crews_per_supervisor = safe.integer(crews_per_supervisor)
    supervisors = safe.integer(supervisors)
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
        SEVERE_MULTIPLIER = 1.5
    } else if(variant == '2020'){
        SEVERE_MULTIPLIER = 1
    } else {
        stop(paste('Unsupported variant:', variant))
    }

    analyze_only = safe.logical(analyze_only)
    PARALLEL = safe.logical(PARALLEL)

    if(workers_per_crew == 10 &&
       crews_per_supervisor == 3 &&
       supervisors == 3 &&
       days == 90 &&
       employee_housing == 'Shared' &&
       social_distancing_shared_housing == 'Intermediate' &&
       #community_transmission == NULL && #value doesn't matter if employee_housing == 'Shared'
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
            stop(paste('Invalid social_distancing_shared_housing:', social_distancing_shared_housing))
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
            double_wrap_baseline_work_R0 = double_wrap_baseline_work_R0 * 4
    }

    n_exposed = n_no_symptoms # this should ideally be split up into exposed, pre-symptomatic, and asymptomatic, and may be in a future version; the difference is small, though, and this is sufficient for now.

    subdirectory = paste(folder_name, '/', sep = '')
    dir.create(subdirectory)
    if(analyze_only) {
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
double_wrap_num_sims = 100#0

f = function(i, profile_p, double_wrap_num_sims = double_wrap_num_sims) {
	if(profile_p) {
		rp_filename =paste0(i, '-all-batch-2.Rprof')
		Rprof(rp_filename)
	}
	full_run(i, i, i, '90', 'Shared', 'Low', 'NULL', 'Low', '1', '0', '.116', '.627', working_directory = '.', paste0(i, '-all-batch-2'), paste0(i, '-all-batch-2'), analyze_only = 'FALSE', SEVERE_MULTIPLIER = '2', DELTA = TRUE)
	if(profile_p) {
		Rprof(NULL)
		summaryRprof(rp_filename)
	}
}
full_run('10', '3', '3', '90', 'Shared', 'Intermediate', 'NULL', 'Intermediate', '1', '0', '.116', '.627', working_directory = '.', '../data/waning-branch-2022-01-07', 'state-split--crudely-fixed-grouping-x100', TRUE, analyze_only = 'FALSE', SEVERE_MULTIPLIER = '2', PARALLEL = TRUE)
