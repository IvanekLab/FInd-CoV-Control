source('constants.R')

symptomatic_infections = function(df) {
    apply(df[,'new_symptomatic_infections',], 2, sum)
}

shiftwise_unavailable = function(data) {
    apply(data[,'qn_absent',], 2, sum)
}

limited_runs_index = c(1,2,4,9,13)

row.names<-c(
    "Baseline",
    "Temperature Screening, 38.0°C",
    "Virus Test, p = 0.05 / Working Day",
    "Virus Test, p = 0.3 / Working Day",
    "Virus Test, p = 1.0 / Working Day",
    "Vaccination, p = 0.02 / Day",
    "Vaccination, p = 0.04 / Day",
    "Phys. Dist./Biosafety: -20% R₀",
    "Phys. Dist./Biosafety: -40% R₀",
    "Phys. Dist./Biosafety: -80% R₀",
    'Boosting, p = 0.02 / day',
    'Boosting, p = 0.04 / day',
    'Vax + Boosting, p = 0.02/day'
)[limited_runs_index]




sensitivity_csvs = function(
    folder_name,
    unique_ids,
    is_baselines,
    community_transmissions,
    work_R0s,
    dormitory_R0s,
    E0,
    initial_recovereds,
    initial_V2s,
    n_sims,
    sensitivity_variables
) {
    max_j = length(unique_ids)

    rds_filename = function(prepended_key, index_i, index_j) { #j being the index to all the parameters that are plurals
        work_id_multiplier = ifelse(index_i == 4,
            'x(1-0.4)',
            ''
        )
        testing_string = ifelse(index_i == 2,
            ',T.test-38',
            ifelse(index_i == 3,
                ',v.test-0.3-rational',
                ''
            )
        )
        vax_string = ifelse(index_i == 5,
            ',vax-rate0.02',
            ''
        )
        dormitory_R0_string = ifelse(dormitory_R0s[index_j] == 0,
            '',
            paste0(',dormitory_R0-', dormitory_R0s[index_j])
        )
        baseline_string = ifelse(is_baselines[index_j],
            'baseline',
            ''
        )
        initial_V2_string = ifelse(initial_V2s[index_j] == 0,
            '',
            paste0(',initial_V2-', initial_V2s[index_j])
        )
        initial_recovered_string = ifelse(initial_recovereds[index_j] == 0,
            '',
            paste0(',initial_recovered-', initial_recovereds[index_j])
        )
        paste0(
            folder_name, '/', unique_ids[index_j],
            prepended_key,
            baseline_string,
            '_community-', community_transmissions[index_j],
            ',work_R0-', work_R0s[index_j],
            work_id_multiplier,
            dormitory_R0_string,
            ',E0-', E0,
            vax_string,
            testing_string,
            initial_recovered_string,
            initial_V2_string,
            ',n_sims-', n_sims,
            'index_i-', index_i,
            '_full-output.rds'
        )
    }

    make_batch = function(d, key, index_j) {
        prepended_key = ifelse(key == '',
            '',
            paste0('-', key)
        )
        for(i in 1:5) {
            filename = rds_filename(prepended_key, i, index_j)
            d[[paste0(i, key)]] = readRDS(filename)
        }
        d
    }
    dd = list()
    for(index_j in 1:max_j) {
        dd[[index_j]] = make_batch(list(), '', index_j)
    }

    get_real_multiplier = function(sensitivity_variable,
                                   theoretical_multiplier) {
        kConstants_ = kConstants
        kConstants_[[sensitivity_variable]] = theoretical_multiplier * kConstants_[[sensitivity_variable]]
        ccl = check_consistency(kConstants_, altered_single_parameter = sensitivity_variable)
        kConstants_fixed = get('fixed_constants', ccl)
        if(!get('consistent', ccl) && !get('fixed', ccl)) {
            stop('Unfixable constants')
        }
        get(sensitivity_variable, kConstants_fixed) / get(sensitivity_variable, kConstants)
    }

    sensitivity_multipliers = c(0.5, 1, 1.5)
    for(sensitivity_variable in sensitivity_variables) {
        real_multipliers = sapply(sensitivity_multipliers, function(m) get_real_multiplier(sensitivity_variable, m))
        for(sensitivity_multiplier in real_multipliers) {
            if(sensitivity_multiplier != 1) {
                key = paste0(sensitivity_variable, '-', sensitivity_multiplier)
                for(index_j in 1:max_j) {
                    dd[[index_j]] = make_batch(dd[[index_j]], key, index_j)
                }
            }
        }
    }

    make_csv = function(filename, outcome_fn) {
        values = data.frame(parameter_set = character(), sensitivity_variable = character(), multiplier = numeric(), intervention = character(), value = numeric())

        for(j in 1:max_j) {
            for(sensitivity_variable in sensitivity_variables) {
                real_multipliers = sapply(
                    sensitivity_multipliers,
                    function(m) get_real_multiplier(sensitivity_variable, m)
                )
                for(multiplier in real_multipliers) {
                    for(i in 1:5) {
                        intervention = row.names[i]
                        if(multiplier == 1) {
                            key = paste0(i)
                        } else {
                            key = paste0(i, sensitivity_variable, '-', multiplier)
                        }
                        value = mean(outcome_fn(dd[[j]][[key]]))
                        values = rbind(values, data.frame(parameter_set = unique_ids[j], sensitivity_variable = sensitivity_variable, multiplier = multiplier, intervention = intervention, value = value))
                    }
                }
            }
        }
        write.csv(values, filename)
    }

    make_csv('sensitivity_symptomatic_infections.csv', symptomatic_infections)
    make_csv('sensitivity_shifts_unavailable.csv', shiftwise_unavailable)
}

l = sensitivity_csvs(
    'sensitivity-2022-11-22',
    c('farm', 'farm-start-of-epidemic', 'farm-dec-11'),
    c(TRUE, FALSE, FALSE),
    c(0, 0, 0),
    c(6, 6, 6),
    c(2, 2, 2),
    1,
    c(71, 0, 23),
    c(73, 0, 0),
    100,
    c("isolation_duration", "duration_IP_mean", "duration_IP_shape",
      "duration_IA_mean", "duration_IM_mean", "duration_IS_mean", "p_trans_IP",
      "p_trans_IM", "boosting_interval", "complete_immunity_duration_R",
      "B_magnitude_1", "B_magnitude_2", "B_decay_rate_1", "B_decay_rate_2",
      "SEVERE_MULTIPLIER", "R_question_period")
)
