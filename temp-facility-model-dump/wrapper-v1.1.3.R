#revising the use of double_wrap_* variables to make solo use of wrapper.R less
#confusing

# behavior of wrapper.R is controlled by other files using several tests for
# all-caps variables. If running wrapper.R directly, the following should
# evaluate to FALSE:
# exists('ANALYZE') && ANALYZE == TRUE
# exists('DOUBLE_WRAPPED') && DOUBLE_WRAPPED == TRUE

# If using double-wrapped.R to run a batch of scenarios, a number of
# parameters may be overwritten by the "double_wrapped" variables; as a reminder
# of this, they are marked with comments of the form:
# batch mode: [double_wrapped variable name]
# These comments can be ignored entirely if not intending to ever use batch mode

# Number of individuals initially in various states other than completely
# susceptible
wrapper_fn = function() {   #will it work? the goal is to get more meaningful debug data

initial_recovered = 0 # batch mode: double_wrap_initial_recovered
initial_V1 = 0        # batch mode: double_wrap_initial_V1
initial_V2 = 0        # batch mode: double_wrap_initial_V2

###########################################
# Flags controlling various interventions #
###########################################

# Based on the current approach we're using, I have commented out the flags for
# "named" social distancing and biosafety interventions, along with one of the
# two "theoretical" interventions (leaving the other). 


# Social distancing interventions
#spacing_gt_6_ft = FALSE
#physical_barriers = FALSE
theoretical_social_distancing_R0_reduction = 0

# Cohorting will go here when it is implemented

#biosafety interventions
#masks_and_shields = FALSE
#improved_ventilation = FALSE
#air_cleaning_filtering = FALSE
#theoretical_biosafety_R0_reduction = 0

#surveillance -- currently, these cannot both be true, but that limitation will
#soon be lifted
temperature_screening = FALSE
temperature_threshold = 37.1    # batch mode: double_wrap_temp_test
                                # values currently recognized: 37.1, 37.5, 38,
                                # 38.5 (degress celsius)
                                # combining other values with
                                # temperature_testing = TRUE will cause an error
viral_testing_rate = 0.3        # batch mode: double_wrap_viral_test_rate
rational_testing = TRUE # TRUE is not currently implemented, but will soon be
isolation_duration = 14         # batch mode: double_wrap_isolation_duration

#vaccination
vaccination_rate = 0            # batch mode: double_wrap_vax_rate
rational_vaccination = FALSE #TRUE is not currently implemented, but will soon be

#General setting parameters
#baseline_work_R0 = 4   #for a high-risk setting
                  # batch mode: double_wrap_baseline_R0
#community_foi = 0 #per shift at home
                  # batch mode: double_wrap_community_foi 


##############################
# Program control parameters #
##############################
num_sims = 100 # number of simulations; 100 takes 1-2 minutes (less on a faster machine); for production use, most likely want 1000+
               # batch mode: double_wrap_num_sims


################################################################
# Code to allow batch running, by overriding parameters        #
# set in this file with those set in the invoking file.        #
# If not using batch mode, this section may safely be ignored. #
################################################################

if(exists('DOUBLE_WRAPPED') && DOUBLE_WRAPPED == TRUE) {
    initial_recovered = double_wrap_initial_recovered
    theoretical_social_distancing_R0_reduction = double_wrap_reduction
    temperature_screening = (double_wrap_temp_test != FALSE)
    temperature_threshold = double_wrap_temp_test
    viral_testing_rate = double_wrap_viral_test_rate
    vaccination_rate = double_wrap_vax_rate
    community_foi = double_wrap_community_foi
    num_sims = double_wrap_num_sims
    isolation_duration = double_wrap_isolation_duration
    baseline_work_R0 = double_wrap_baseline_work_R0
    initial_V1 = double_wrap_initial_V1
    initial_V2 = double_wrap_initial_V2
    rational_testing = double_wrap_rational_testing
}

#############################################################################
# Derived values; simple use will not require modification below this point #
#############################################################################

social_distancing_R0_factor = (#(1 - 0.30 * spacing_gt_6_ft) *
                               #(1 - 0.20 * physical_barriers) *
                               (1 - theoretical_social_distancing_R0_reduction)
                              )
#biosafety_R0_factor = ((1 - 0.6 * masks_and_shields) *
#                       (1 - 0.3 * improved_ventilation) *
#                       (1 - 0.5 * air_cleaning_filtering) *
#                       (1 - theoretical_biosafety_R0_reduction)
#                      )

total_R0_factor = social_distancing_R0_factor #* biosafety_R0_factor

net_work_R0 = total_R0_factor * baseline_work_R0
#for now, assuming that R0 reductions only apply at work (will discuss further)

if(temperature_screening) {
    if(viral_testing_rate != 0) {
        stop() #both at once is not yet implemented
    }

    if(temperature_threshold == 37.1) {
 #       print('37.1')
        sensitivity = .63
        specificity = .95
    } else if (temperature_threshold == 37.5) {
 #       print('37.5')
        sensitivity = .32
        specificity = .99
    } else if (temperature_threshold == 38) {
  #      print('38')
        sensitivity = .18
        specificity = 1.00
    } else if (temperature_threshold == 38.5) {
   #     print('38.5')
        sensitivity = .08
        specificity = 1.00
    } else {
        stop('Invalid screening temperature')
    }
    
    testing_parameters = list(
        #following several parameters taken from Cashore et al. 2020,
        #citing Kojima et al. 2020 and To et al. 2020. assuming them equal
        #for now, will run just fine with alternate assumptions
        ### the following are parameters for virus testing
        asymptomatic_FNR = specificity, #presumably, we'll catch a few by chance
        presymptomatic_FNR = specificity,
        mild_FNR = 1 - sensitivity,
        FPR = 1 - specificity,
        rational_testing = rational_testing
    )

    work_testing_rate = 1
} else {
    testing_parameters = list(
        #following several parameters taken from Cashore et al. 2020,
        #citing Kojima et al. 2020 and To et al. 2020. assuming them equal
        #for now, will run just fine with alternate assumptions
        ### the following are parameters for virus testing
        asymptomatic_FNR = 1 - 0.9,
        presymptomatic_FNR = 1 - 0.9,
        mild_FNR = 1 - 0.9,
        FPR = 1 - 0.9995,
        rational_testing = rational_testing
    )
    work_testing_rate = viral_testing_rate
}

#########################
# filename construction #
#########################

filename_core = paste(subdirectory, unique_id, '_community-', community_foi, ',work_R0-', baseline_work_R0, sep = '')

R0_factor_string = paste()
if(total_R0_factor != 1) {
   filename_core = paste(filename_core , 'x(1-', 1 - total_R0_factor, ')', sep = '')
}

if(dormitory_R0 > 0) {
    filename_core = paste(filename_core, ',dormitory_R0-', dormitory_R0, sep = '')
}

filename_core = paste(filename_core, ',E0-', n_exposed, sep = '')

if(n_mild > 0) {
    filename_core = paste(filename_core, ',IM0-', n_mild, sep = '')
}

if(temperature_screening) {
    filename_core = paste(filename_core, ',T.test-', temperature_threshold, sep = '')
}

if(viral_testing_rate != 0) {
    filename_core = paste(filename_core, ',v.test-', viral_testing_rate, sep = '')
    if(rational_testing) {
        filename_core = paste(filename_core, '-rational', sep = '')
    }
}

if(vaccination_rate != 0) {
    filename_core = paste(filename_core, ',vax-rate', vaccination_rate, sep = '')
}

if(isolation_duration != 14) {
    filename_core = paste(filename_core, ',isol-dur-', isolation_duration, sep = '')
}

if(initial_recovered > 0) {
    filename_core = paste(filename_core, ',initial_recovered-', initial_recovered, sep = '')
}

if(initial_V2 > 0) {
    filename_core = paste(filename_core, ',initial_V2-', initial_V2, sep = '')
}

if(initial_V1 > 0) {
    filename_core = paste(filename_core, ',initial_V1-', initial_V1, sep = '')
}

filename_core = paste(filename_core, ',n_sims-', num_sims, sep = '')
#table_name = paste(filename_core, '.csv', sep = '')
full_output_save_name = paste(filename_core, '_full-output.rds', sep = '')

#and now let's run the model
if(!(exists('ANALYZE') && ANALYZE == TRUE)) {
    #print('pre')
    source('main-produce-farm-v1.1.3.R', local = TRUE)
    #print('IN')
    main_produce_farm_fn()
    #print('OUT')
} else {
    #print('ooh ahh ahh ahh ahh')
}

full_output_save_name
} #wrapper_fn
