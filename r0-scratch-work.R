library(rpart)

#sd_ = read.csv('r0s-short.csv')

#sd_ = read.csv('r0s-short.csv')
#sd_$value = sd_$value >= 1

n_rows = 33*3*16*5
n_distinct = 67 * 16 * 5


sd_ = read.csv('r0s-long.csv')

n_rows = 33*3*16*5 * 100
n_distinct = 67 * 16 * 5 * 100

df = data.frame(
    #parameter_set = rep('', n_distinct),
    setting = character(n_distinct),#rep('', n_distinct),
    housing = character(n_distinct),#rep('', n_distinct),
    fraction_recovered = rep(0.73, n_distinct),
    fraction_fully_vaccinated = rep(0.71, n_distinct),
    #intervention = rep('', n_distinct),
    temp_screening = logical(n_distinct), #rep('', n_distinct),
    virus_testing = numeric(n_distinct),
    r0_reduction = numeric(n_distinct),
    vax_boost_rate = numeric(n_distinct), #rep('', n_distinct),

    value = numeric(n_distinct)#rep(NA, n_distinct)
)
for(parameter in names(kConstants)) {
    df[[parameter]] = rep(1, n_distinct)
}

next_df_row = 1
#done = character(0)

done = numeric(0)

for(row_number in 1:n_rows) {
    row_ = sd_[row_number,]
    parameter_set = row_$parameter_set
    parameter_set_split = unlist(strsplit(parameter_set, '-'))
    intervention = row_$intervention
    if(substr(parameter_set,1, 4) == 'farm') {
        df[next_df_row, 'housing'] = 'Shared' #if this row is a duplicate, these values will just be overwritten next loop
    } else if(substr(parameter_set,1, 8) == 'facility'){
        df[next_df_row, 'housing'] = 'Individual'
    } else {
        stop('Invalid housing in parameter_set: ', parameter_set)
    }

    if('farm' %in% parameter_set_split) {
        df[next_df_row, 'setting'] = 'Farm'
    } else if('facility' %in% parameter_set_split) {
        df[next_df_row, 'setting'] = 'Facility'
    } else {
        stop('Invalid setting in parameter_set: ', parameter_set)
    }

    if(any(c('start', 'recovered') %in% parameter_set_split)) { #i.e., 'start-of-epidemic' or 'no-recovered'
        df[next_df_row, 'fraction_recovered'] = 0
    }

    if(any(c('start', 'vax') %in% parameter_set_split)) { #i.e., 'start-of-epidemic' or 'no-recovered'
        df[next_df_row, 'fraction_fully_vaccinated'] = 0
    }

    if(intervention == "Temperature Screening, 38.0°C") {
        df[next_df_row, 'temp_screening'] = TRUE
    } else if(intervention == "Virus Test, p = 0.3 / Working Day") {
        df[next_df_row, 'virus_testing'] = 0.3
    } else if(intervention == "Phys. Dist./Biosafety: -40% R₀") {
        df[next_df_row, 'r0_reduction'] = 0.4
    } else if(intervention == 'Vax + Boosting, p = 0.02/day') {
        df[next_df_row, 'vax_boost_rate'] = 0.02
    } else if(intervention == "Baseline") {
    } else {
        stop('Invalid intervention: ', intervention)
    }

    df[next_df_row, 'value'] = row_$value

    if(row_$multiplier != 1) {
        df[next_df_row, row_$sensitivity_variable] = row_$multiplier
        next_df_row = next_df_row + 1
    } else {
        key = paste0(row_$parameter_set, row_$intervention)
        #if(!(key %in% done)) {
        #    done = c(done, key)
        #    next_df_row = next_df_row + 1
        #}
        if(!(key %in% names(done))) {
            done[[key]] = 1
            next_df_row = next_df_row + 1
        } else if(done[[key]] < 100) {
            done[[key]] = done[[key]] + 1
            next_df_row = next_df_row + 1
        }
    }
}

tree = rpart(value ~ setting + housing + fraction_recovered + fraction_fully_vaccinated + temp_screening + virus_testing + r0_reduction + vax_boost_rate + isolation_duration + mu + sd + duration_IP_mean + duration_IP_shape + duration_IA_mean + duration_IA_shape + duration_IM_mean + duration_IM_shape + duration_IS_mean + duration_IS_shape + duration_IC_mean + duration_IC_shape + p_trans_IP + p_trans_IA + p_trans_IM + boosting_interval + complete_immunity_duration_R + second_shot_interval + max_V1_protection + V2_ramp_time + V2_magnitude + V2_decay_rate + B_ramp_time_1 + B_ramp_time_2 + B_mid_ramp_protection + B_magnitude_1 + B_magnitude_2 + B_decay_rate_1 + B_decay_rate_2 + SEVERE_MULTIPLIER + R_question_period + time_since_first_V2, data = df)

#png('tree-short-r0s-expanded.png', width = 1600, height = 900)
#png('tree-short-binary.png', width = 1600, height = 900)
png('tree-long-r0s-expanded.png', width = 1600, height = 900)
plot(tree)
text(tree)
dev.off()

