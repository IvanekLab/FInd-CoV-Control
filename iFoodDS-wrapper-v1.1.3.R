#Usage example, with default values:
#TBD: fill in

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

#removing default values from testing code to ensure that accidental omission
#doesn't generate stupid results
full_run = function(
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
                    fraction_fully_vaccinated, # TBD: This is defined FOR NOW as
                                               # fraction fully vax
                                               #and NOT boosted
                                               #this is not ideal; it means that
                                               # we have to work around a bunch
                                               # of things. but to try to do it
                    # the other way *before* blending in the swiss cheese fixes
                    # is to court madness
                                               #TBD: figure out where the next
                                               #comment line comes from
                                               #going to want this to be
                                               #consistent with 
                    fraction_boosted, #TBD: finish implementing? (is this done?)
                    boosting_rate, #.05 arbitrary, probably high
                    working_directory,
                    folder_name,
                    unique_id, 
                    variant,
                    analyze_only,
                    PARALLEL
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
    #N = sum(crew_sizes) + length(crew_sizes) + supervisors + 1 
    N = 1 + supervisors * (1 + workers_per_crew * crews_per_supervisor +
                           n_shift_floaters) +
                    n_cleaners + n_all_floaters
    
    #days -- done

    if((tolower(employee_housing) == 'private') ||
       (tolower(employee_housing) == 'individual')) {
        housing_dormitory = FALSE
        dormitory_R0 = 0 

        if(tolower(community_transmission) == 'low') {
            double_wrap_community_foi = 0.0005
        } else if(tolower(community_transmission) == 'intermediate'){
            double_wrap_community_foi = 0.001
        } else if(tolower(community_transmission) == 'high'){
            double_wrap_community_foi = 0.002
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
        #TBD: reinstate increased level; just trying to sanity-check this now
        #so now at R0 = 3
        #later
    }

    n_exposed = n_no_symptoms # done, although this should ideally be split up
                              # into exposed, pre-symptomatic, and asymptomatic;
                              # the difference is small, though
    #n_mild -- done
    #although note that we might consider in the future allowing initial exposed
    #and mild to be drawn from the vaccinated
    #TBD: change the above once swiss-cheesing is fully fixed
    #fraction_recovered -- done
    #fraction_fully_vaccinated -- done

    #subdirectory = paste(set_name, '-files/', sep = '')
    subdirectory = paste(folder_name, '/', sep = '')
    dir.create(subdirectory)
    #wd = getwd()
    #setwd(subdirectory)
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
    #setwd(wd)
}

FIXED_SEED = TRUE
VERSION = '1.1.3'
double_wrap_num_sims = 5#100#0

#note that several of these parameters are not actually used (no longer true?)
#separating into one variable per line for comments and diffing
#here using all variable names explicitly, so that errors fail loudly instead of
#giving weird bugs
#(note that a word diff ignoring whitespace vs. function definition is now
#relatively straightforward, if function definition is similarly formatted):
#copy the relevant signatures to two files, strip out comments, strip out ,s and
#then run
#git diff --no-index --word-diff --ignore-all-space a.txt b.txt
full_run(
         workers_per_crew = '4', # FM: workers per line
         crews_per_supervisor = '2', # FM: / lines per shift
         supervisors = '2', # FM: shifts
         n_shift_floaters ='4', # FM only (for with farm model,
                                 # will require NULL/NA)
         n_cleaners = '4', # FM only (for farm model, will require NULL/NA)
         n_all_floaters = '4', # FM only (for farm model, will require NULL/NA)
         days = '90',
         employee_housing = 'Shared', 
         social_distancing_shared_housing = 'Intermediate',
         community_transmission = 'Intermediate',
         social_distancing_work = 'Intermediate',
         n_no_symptoms = '1', #i.e., exposed (TBD: not asymp/presymp --
                              #should perhaps alter language?)
         n_mild = '0',
         fraction_recovered = 0,#'.116', # TBD: Swiss Cheese it
                                      # TBD: For now,
                                      # do calculations here by hand
         fraction_fully_vaccinated = 0,#'.627',  #  TBD: (for now: and not boosted?
                                              #(check))
         fraction_boosted = .5,
         boosting_rate = 0,
         working_directory = '.',
         folder_name = 'facility-added-interface', # relative to working
                                                   # directory
         unique_id = 'no-recovered-no-vaccinated-fb_.5',
         variant = 'omicron',
         analyze_only = 'FALSE',
         PARALLEL = TRUE
)
