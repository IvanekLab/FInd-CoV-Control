# double-wrapped-v1.1.3.R is part of Food INdustry CoViD Control Tool
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

#NB: The following summary should be reasonably accurate, but may be outdated
#in some minor details.

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
# This is, indeed, what iFoodDS-wrapper.R does, provided that analyze_only ==  #
# FALSE.                                                                       #
################################################################################

double_wrapped_fn = function() { #the (current) goal is to get more meaningful debug data

double_wrap_isolation_duration = 14
double_wrap_rational_testing = TRUE

double_wrap_initial_recovered = round(fraction_recovered * N)

#below is kinda kludgey, but works
double_wrap_initial_V2 = round(N * fraction_fully_vaccinated) #TBD: Remember, this needs a fix once things are redefined with swiss-cheese
double_wrap_initial_V1 = 0 

#TBD: Check if swiss-cheese is handling V2 + R individuals correctly, in terms of setting their vax status!
#TBD: Fix swiss-cheese's handling of vaccination of (NV, V1) + (R, W) individuals, if this is not already correct (since we will be merging, it should be safe to make this note here)

double_wrap_initial_B = 0#round(N * fraction_boosted) #fix in swiss-cheese
    #TBD: remove this variable entirely OR rework the handling in AgentGen

if(n_exposed + n_mild + double_wrap_initial_recovered + double_wrap_initial_V2 + double_wrap_initial_B > N) {
    stop(paste('Exposed + mild + recovered + fully vaccinated + boosted > Total number of employees:\n',
               n_exposed, '+', n_mild, '+', double_wrap_initial_recovered, '+', double_wrap_initial_V2, '+', double_wrap_initial_B, '>', N))
}

temperature_thresholds = c(38)#, 37.5, 37.1) #given an observed specificity of 100% at 38, why lose further sensitivity at 38.5?
viral_test_rates = c(0.05, 0.3, 1.0)
vax_rates = c(0.01, 0.04, 0.16) 
R0_reductions = c(0.2, 0.4, 0.8)

k_max = 1 + length(temperature_thresholds) + length(viral_test_rates) + length(vax_rates) + length(R0_reductions) + 2 #+2 is a kludge to allow the final two interventions

row.names<-c(     "Baseline",
                  "Temperature Screening, 38.0°C",
                  #"Temperature Screening, 37.5°C",
                  #"Temperature Screening, 37.1°C",
                  "Virus Test, p = 0.05 / Working Day",
                  "Virus Test, p = 0.3 / Working Day",
                  "Virus Test, p = 1.0 / Working Day",
                  "Vaccination, p = 0.01 / Day",
                  "Vaccination, p = 0.04 / Day",
                  "Vaccination, p = 0.16 / Day",
                  "Soc. Dist./Biosafety: -20% R₀",
                  "Soc. Dist./Biosafety: -40% R₀",
                  "Soc. Dist./Biosafety: -80% R₀",
                  'Boosting, p = 0.04 / day', #kludge
                  'Vax + Boosting, p = 0.04/day' #kludge
)
if(length(row.names) != k_max) {
    stop('Row names does not have the right length')
}

c4 = c('black', 'blue3', 'turquoise1', 'red2', 'yellow2')

colors = c('black',
           c4[2],
           #c4[2],
           #c4[2],
           c4[3],
           c4[3],
           c4[3],
           c4[4],
           c4[4],
           c4[4],
           c4[5],
           c4[5],
           c4[5],
'limegreen', #kludge
'limegreen')

ltys = c(1,
         1,
         #2,
         #3,
         1,
         2,
         3,
         1,
         2,
         3,
         1,
         2,
         3,
1, #kludge
2)

parameter_sets = data.frame(double_wrap_reduction = rep(0, k_max),
                            double_wrap_temp_test = rep(FALSE, k_max),
                            double_wrap_viral_test_rate = rep(0, k_max),
                            double_wrap_vax_rate = rep(0, k_max),
                            double_wrap_boosting_rate = rep(0, k_max)) #kludge?

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

parameter_sets[k+1,'double_wrap_boosting_rate'] = 0.04
parameter_sets[k+2,c('double_wrap_vax_rate','double_wrap_boosting_rate')] = 0.04


DOUBLE_WRAPPED = TRUE
if(!(exists('ANALYZE') && ANALYZE == TRUE)) {
    loop_time_start = Sys.time()
}

library(foreach)

if(PARALLEL) {
    library(doParallel)
    #cl = makePSOCKcluster(4, outfile = "") #for use on "grenade" only; revert to 6 or 5 for use on "corsair"
    registerDoParallel(4)#(cl) 
} #if not, %dopar% is equivalent to %do% (with a warning)
  #in the current version, we use %do% explicitly anyway
  #but this may change in a future version

full_output_filenames = foreach(i=1:k_max, .combine = c, .inorder=TRUE, .verbose = TRUE) %dopar% {
#for(i in 1:k_max) {
    parameter_set = parameter_sets[i,]
    double_wrap_reduction = parameter_set$double_wrap_reduction
    double_wrap_temp_test = parameter_set$double_wrap_temp_test
    double_wrap_viral_test_rate = parameter_set$double_wrap_viral_test_rate
    double_wrap_vax_rate = parameter_set$double_wrap_vax_rate
    double_wrap_boosting_rate = parameter_set$double_wrap_boosting_rate
        boosting_rate = double_wrap_boosting_rate #kludgey as hell
    row_name = row.names[i]
    source('wrapper-v1.1.3.R', local = TRUE)
    full_output_save_name = wrapper_fn() # returns full_output_save_name
    full_output_save_name
}
if(!(exists('ANALYZE') && ANALYZE == TRUE)) {
    loop_time_end = Sys.time()
    print('Total for all interventions:')
    print(loop_time_end - loop_time_start)
}
DOUBLE_WRAPPED = FALSE

list(row.names, colors, ltys, full_output_filenames)
} #double_wrapped_fn
