# NB: A lot fo the following block of text is probably no longer entirely true.

################################################################################
# Generates parameter sets and, by default, runs the model for each by         #
# invoking wrapper.R with DOUBLE_WRAPPED == TRUE.                              #
#                                                                              #
# If sourced from another file (e.g., analyze.R), with ANALYZE == TRUE, then   #
# it merely uses wrapper.R to generate filenames (without actually running     #
# simulations),                                                                #
# for use by the invoking file. This allows one to insure that the collection  #
# of parameter sets to analyze matche the collection of parameter sets to run, #
# by first sourcing double-wrapped.R directly, then (after it has finished     #
# running) sourcing analyze.R, without modifying double-wrapped.R in between.  #
#                                                                              #
# To run multiple collections in parallel (e.g., to take full advantage of     #
# multiple processors), one moderately easy solution is to save for each       #
# collection:                                                                  #
# - a modified double-wrapped.R, that will generate the desired collection,    #
#   with a suitably modified name (e.g., 'double-wrapped-foo.R')               #
# - a modified analyze.R, with:                                                #
# - - the line "source('double-wrapped.R')" replaced accordingly               #
#     (e.g., with "source('double-wrapped-foo.R')")                            #
# - - the line "set_name = 'baseline'" replaced with a name designating the    #
#     desired collection (e.g., "set_name = 'foo'")                            #
# and then:                                                                    #
# - source the modified double-wrapped.R                                       #
#   (e.g., "source('double-wrapped-foo.R')")                                   #
# - source the modified analyze.R (e.g., "source('analyze-foo.R')")            #
################################################################################

double_wrapped_fn = function() { #will it work? the goal is to get more meaningful debug data

double_wrap_isolation_duration = 14
double_wrap_rational_testing = TRUE

double_wrap_initial_recovered = round(fraction_recovered * N)

#below is kinda kludgey, but should work
double_wrap_initial_V2 = round(N * fraction_fully_vaccinated)
double_wrap_initial_V1 = 0 
if(n_exposed + n_mild + double_wrap_initial_recovered + double_wrap_initial_V2 > N) {
    stop(paste('Exposed + mild + recovered + fully vaccinated > Total number of employees:\n',
               n_exposed, '+', n_mild, '+', double_wrap_initial_recovered, '+', double_wrap_initial_V2, '>', N))
}

#trying to cut down on number of scenarios a *little*
temperature_thresholds = c(38, 37.5, 37.1) #given an observed specificity of 100% at 38, why lose further sensitivity at 38.5?
viral_test_rates = c(0.05, 0.3, 1.0)
vax_rates = c(0.01, 0.04, 0.16) # when introducing rational vaccination, a lower max will likely be sensible
R0_reductions = c(0.2, 0.4, 0.8)

k_max = 1 + length(temperature_thresholds) + length(viral_test_rates) + length(vax_rates) + length(R0_reductions)
interventions.predict = rep('', k_max)
mean_output_filenames = rep('', k_max)
median_output_filenames = rep('', k_max)
full_output_filenames = rep('', k_max)
q1_output_filenames = rep('', k_max)
q3_output_filenames = rep('', k_max)
infected_sd_output_filenames = rep('', k_max)


row.names<-c(     "Baseline",
                  "Temperature Screening, 38.0°C",
                  "Temperature Screening, 37.5°C",
                  "Temperature Screening, 37.1°C",
                  "Virus Test, p = 0.05 / Working Day",
                  "Virus Test, p = 0.3 / Working Day",
                  "Virus Test, p = 1.0 / Working Day",
                  "Vaccination, p = 0.01 / Day",
                  "Vaccination, p = 0.04 / Day",
                  "Vaccination, p = 0.16 / Day",
                  "Soc. Dist./Biosafety: -20% R₀",
                  "Soc. Dist./Biosafety: -40% R₀",
                  "Soc. Dist./Biosafety: -80% R₀"
)
if(length(row.names) != k_max) {
    stop('Row names does not have the right length')
}

c4 = c('black', 'blue3', 'turquoise1', 'red2', 'yellow2')

colors = c('black',
           c4[2],
           c4[2],
           c4[2],
           c4[3],
           c4[3],
           c4[3],
           c4[4],
           c4[4],
           c4[4],
           c4[5],
           c4[5],
           c4[5])

ltys = c(1,
         1,
         2,
         3,
         1,
         2,
         3,
         1,
         2,
         3,
         1,
         2,
         3)

parameter_sets = data.frame(double_wrap_reduction = rep(0, k_max),
                            double_wrap_temp_test = rep(FALSE, k_max),
                            double_wrap_viral_test_rate = rep(0, k_max),
                            double_wrap_vax_rate = rep(0, k_max))

for(h in 2:(1 + length(temperature_thresholds))) {
 parameter_sets[h, 'double_wrap_temp_test'] = temperature_thresholds[h - 1]
}

for(i in (h + 1):(h + length(viral_test_rates))) {
    parameter_sets[i, 'double_wrap_viral_test_rate'] = viral_test_rates[i - h]
}
for(j in (i + 1):(i + length(vax_rates))) {
    parameter_sets[j, 'double_wrap_vax_rate'] = vax_rates[j - i]
}
for(k in (j + 1):(j + length(R0_reductions))) {
    parameter_sets[k, 'double_wrap_reduction'] = R0_reductions[k - j]
}

DOUBLE_WRAPPED = TRUE
if(!(exists('ANALYZE') && ANALYZE == TRUE)) {
    loop_time_start = Sys.time()
}

library(foreach)

if(PARALLEL) {
    library(doParallel)
    registerDoParallel(6)
} #if not, %dopar% is equivalent to %do% (with a warning)

full_output_filenames = foreach(i=1:k_max, .combine = c, .inorder=TRUE) %dopar% {
    parameter_set = parameter_sets[i,]
    double_wrap_reduction = parameter_set$double_wrap_reduction
    double_wrap_temp_test = parameter_set$double_wrap_temp_test
    double_wrap_viral_test_rate = parameter_set$double_wrap_viral_test_rate
    double_wrap_vax_rate = parameter_set$double_wrap_vax_rate
    row_name = row.names[i]
    source('wrapper-v1.1.3.R', local = TRUE)
#    print('in')
    full_output_save_name = wrapper_fn() # returns full_output_save_name
#    print('out')
#    if(exists('ANALYZE') && ANALYZE == TRUE) {
#        interventions.predict[i] = table_name
#        mean_output_filenames[i] = mean_output_table_name
#        median_output_filenames[i] = median_output_table_name
#        q1_output_filenames[i] = q1_output_table_name
#        q3_output_filenames[i] = q3_output_table_name
#        infected_sd_output_filenames[i] = infected_sd_output_table_name
    #full_output_filenames[i] = full_output_save_name
    #full_output_save_name
#    }
    #Rprof(NULL)
    full_output_save_name
}
#print('off')
if(!(exists('ANALYZE') && ANALYZE == TRUE)) {
    loop_time_end = Sys.time()
    print('Total for all interventions:')
    print(loop_time_end - loop_time_start)
}
DOUBLE_WRAPPED = FALSE

list(row.names, colors, ltys, full_output_filenames)
} #double_wrapped_fn
