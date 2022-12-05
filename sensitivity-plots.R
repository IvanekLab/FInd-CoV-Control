source('constants.R')

symptomatic_infections = function(df) {
    apply(df[,'new_symptomatic_infections',], 2, sum)
}

shiftwise_unavailable = function(data) {
    apply(data[,'qn_absent',], 2, sum)
}

limited_runs_index = c(1,2,4,9,13)
c4 = c('black', 'blue3', 'lightblue1', 'red2', 'gray80', 'darkgreen', 'yellow2')

colors = c(
    c4[1],
    c4[2],
    c4[3], c4[3], c4[3],
    c4[4], c4[4],
    c4[5], c4[5], c4[5],
    c4[6], c4[6],
    c4[7]
)[limited_runs_index]

colors[3] = 'red2'
colors[4] = 'darkgreen'

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



sensitivity_fn = function(
    folder_name,
    unique_id,
    community_transmission,
    work_R0,
    dormitory_R0,
    E0,
    initial_recovered,
    initial_V2,
    n_sims
) {
    rds_filename = function(prepended_key, index_i) {
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
        initial_recovered_string = ifelse(initial_recovered > 0,
            paste0(',initial_recovered-', initial_recovered),
            ''
        )
        initial_V2_string = ifelse(initial_V2 > 0,
            paste0(',initial_V2-', initial_V2),
            ''
        )
        paste0(
            folder_name, '/', unique_id,
            prepended_key,
            '_community-', community_transmission,
            ',work_R0-', work_R0,
            work_id_multiplier,
            ',dormitory_R0-', dormitory_R0,
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

    make_batch = function(d, key) {
        prepended_key = ifelse(key == '',
            '',
            paste0('-', key)
        )
        for(i in 1:5) {
            d[[paste0(i, key)]] = readRDS(rds_filename(prepended_key, i))
        }
        d
    }
    d = make_batch(list(), '')
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

    for(sensitivity_variable in names(kConstants)) {
        real_multipliers = sapply(sensitivity_multipliers, function(m) get_real_multiplier(sensitivity_variable, m))
        for(sensitivity_multiplier in real_multipliers) {
            if(sensitivity_multiplier != 1) {
                key = paste0(sensitivity_variable, '-', sensitivity_multiplier)
                d = make_batch(d, key)
            }
        }
    }
#d_test <<- d
    make_paneled_plot = function(filename, outcome_fn, ylab) {
        si = lapply(d, outcome_fn)
        si_mean_max = max(sapply(si, mean)) 
        si_mean_min = min(sapply(si, mean))

        png(filename, height = 200*5, width = 200*7)
        layout(matrix(c(1:29, 34, 30:34), ncol = 7))

        for(sensitivity_variable in names(kConstants)) {
            real_multipliers = sapply(
                sensitivity_multipliers,
                function(m) get_real_multiplier(sensitivity_variable, m)
            )

            for(i in 1:5) {
                keys = sapply(
                    real_multipliers,
                    function(m) {
                        ifelse(m == 1,
                            paste0(i),
                            paste0(i, sensitivity_variable, '-', m)
                        )
                    }
                )

                values = sapply(keys, function(s) mean(si[[s]]))

                if(i == 1) {
                    plot(
                        real_multipliers,
                        values,
                        ylim = c(0, si_mean_max),
                        xlim = c(0.5, 1.5),
                        xlab = paste0(sensitivity_variable, ' (multiplier)'),
                        ylab = ylab,
                        type = 'b',
                        lwd = 4
                    )
                } else {
                    points(
                        real_multipliers,
                        values,
                        type = 'b',
                        col = colors[i],
                        lwd = 4
                    )
                }
            }
        }
        plot(NULL, xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
        legend("bottom", row.names, lwd = 4, col = colors)
        dev.off()

    }
    make_paneled_plot('duplicate-farmlike-facility-betterer-sensitivity-plots-si.png', symptomatic_infections, 'Mean total symptomatic infections')
    make_paneled_plot('duplicate-farmlike-facility-betterer-sensitivity-plots-su.png', shiftwise_unavailable, 'Mean total shifts unavailable')
}

#sensitivity_fn('sensitivity-2022-10-29', 'farmlike-facility-pass-6', 0, 6, 2, 1, 71, 73, 100)

rds_filename = function(
    prepended_key, index_i, index_j,
    max_j,
    folder_name,
    unique_ids,
    is_baselines,
    community_transmissions,
    work_R0s,
    dormitory_R0s,
    E0,
    initial_recovereds,
    initial_V2s,
    n_sims
) { #j being the index to all the parameters that are plurals
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

make_batch = function(d, key, index_j,
    max_j,
    folder_name,
    unique_ids,
    is_baselines,
    community_transmissions,
    work_R0s,
    dormitory_R0s,
    E0,
    initial_recovereds,
    initial_V2s,
    n_sims
) {
    prepended_key = ifelse(key == '',
        '',
        paste0('-', key)
    )
    for(i in 1:5) {
        #print('Confirm')
        #cat(prepended_key, ':', i, ':', dormitory_R0s, '\n')
        filename = rds_filename(prepended_key, i, index_j, max_j, folder_name,
                                unique_ids, is_baselines,
                                community_transmissions, work_R0s,
                                dormitory_R0s, E0, initial_recovereds,
                                initial_V2s, n_sims)
        #print('Deconfirm')
        d[[paste0(i, key)]] = readRDS(filename)
    }
    d
}


get_real_multiplier = function(sensitivity_variable,
                               theoretical_multiplier,
                               kConstants) {
    kConstants_ = kConstants
    kConstants_[[sensitivity_variable]] = theoretical_multiplier * kConstants_[[sensitivity_variable]]
    ccl = check_consistency(kConstants_, altered_single_parameter = sensitivity_variable)
    kConstants_fixed = get('fixed_constants', ccl)
    if(!get('consistent', ccl) && !get('fixed', ccl)) {
        stop('Unfixable constants')
    }
    get(sensitivity_variable, kConstants_fixed) / get(sensitivity_variable, kConstants)
}

make_dd = function(
    max_j,
    sensitivity_multipliers,
    kConstants,
    folder_name,
    unique_ids,
    is_baselines,
    community_transmissions,
    work_R0s,
    dormitory_R0s,
    E0,
    initial_recovereds,
    initial_V2s,
    n_sims
) {
    dd = list()
    for(index_j in 1:max_j) {
        #cat('\n\n', index_j, ':', dormitory_R0s, '\n\n')
        dd[[index_j]] = make_batch(list(), '', index_j, max_j, folder_name,
                                   unique_ids, is_baselines,
                                   community_transmissions, work_R0s,
                                   dormitory_R0s, E0, initial_recovereds,
                                   initial_V2s, n_sims)
    }

    for(sensitivity_variable in names(kConstants)) {
        real_multipliers = sapply(sensitivity_multipliers, function(m) get_real_multiplier(sensitivity_variable, m, kConstants))
        for(sensitivity_multiplier in real_multipliers) {
            if(sensitivity_multiplier != 1) {
                key = paste0(sensitivity_variable, '-', sensitivity_multiplier)
                for(index_j in 1:max_j) {
                    dd[[index_j]] = make_batch(dd[[index_j]], key, index_j,
                                               max_j,
                                               folder_name, unique_ids,
                                               is_baselines,
                                               community_transmissions,
                                               work_R0s, dormitory_R0s, E0,
                                               initial_recovereds, initial_V2s,
                                               n_sims)
                }
            }
        }
    }
    dd
}

make_paneled_plot = function(filename, outcome_fn, ylab, dd, kConstants,
                             sensitivity_multipliers, max_j) {
    si_mean_max = max(sapply(
        dd,
        function(dd_j) {
            max(sapply(
                dd_j,
                function(dd_j_scenario) {
                    mean(outcome_fn(dd_j_scenario))
                }
            ))
        }
    ))
    si_mean_min = min(sapply(
        dd,
        function(dd_j) {
            min(sapply(
                dd_j,
                function(dd_j_scenario) {
                    mean(outcome_fn(dd_j_scenario))
                }
            ))
        }
    ))

    png(filename, height = 200*5, width = 200*7)
    layout(matrix(c(1:29, 34, 30:34), ncol = 7))

    #figuring out bounds:
    greatest_positive_difference = 0
    greatest_negative_difference = 0
    for(sensitivity_variable in names(kConstants)) {
        real_multipliers = sapply(
            sensitivity_multipliers,
            function(m) get_real_multiplier(sensitivity_variable, m, kConstants)
        )

        for(i in 1:5) {
            null_value = mean(outcome_fn(dd[[1]][[paste0(i)]]))
            keys = sapply(
                real_multipliers,
                function(m) {
                    ifelse(m == 1,
                        paste0(i),
                        paste0(i, sensitivity_variable, '-', m)
                    )
                }
            )
            for(j in 1:max_j) {
                values = sapply(keys, function(key) mean(outcome_fn(dd[[j]][[key]])))
                this_greatest_positive_difference = log(values[3] / values[2])
                this_greatest_negative_difference = log(values[1] / values[2])
                if(this_greatest_positive_difference >= greatest_positive_difference) {
                    greatest_positive_difference = this_greatest_positive_difference
                }
                if(this_greatest_negative_difference <= greatest_negative_difference) {
                    greatest_negative_difference = this_greatest_negative_difference
                }
            }
        }
    }

#print(greatest_negative_difference)
#print(greatest_positive_difference)

    greatest_differences = c()
    greatest_difference_indices_matrix = c()

    #actually doing it
    #for(sensitivity_variable in names(kConstants)) {
    for(sensitivity_index in 1:length(kConstants)) {
        sensitivity_variable = names(kConstants)[sensitivity_index]
        cat(sensitivity_index, ':', sensitivity_variable, '\n')
        real_multipliers = sapply(
            sensitivity_multipliers,
            function(m) get_real_multiplier(sensitivity_variable, m, kConstants)
        )
        
        greatest_difference_all_5 = 0
        greatest_difference_indices = rep(0, 5)

        for(i in 1:5) {
            greatest_difference = 0
            gd_j = NULL
        
            keys = sapply(
                real_multipliers,
                function(m) {
                    ifelse(m == 1,
                        paste0(i),
                        paste0(i, sensitivity_variable, '-', m)
                    )
                }
            )

            for(j in 1:max_j) {
                values = sapply(keys, function(key) mean(outcome_fn(dd[[j]][[key]])))
                this_greatest_difference = max(sapply(
                    values,
                    function(value) {
                        max(sapply(
                            values,
                            function(value_) {
                                abs(log(value) - log(value_))
                            }
                        ))
                    }
                ))
                #print(this_greatest_difference)
                if(this_greatest_difference >= greatest_difference) {
                    greatest_difference = this_greatest_difference
                    gd_j = j
                    #print(gd_j)
                }
            }
            if(greatest_difference > greatest_difference_all_5) {
                greatest_difference_all_5 = greatest_difference
            }
            greatest_difference_indices[i] = gd_j


            values = sapply(keys, function(key) mean(outcome_fn(dd[[gd_j]][[key]])))
            null_value = mean(outcome_fn(dd[[gd_j]][[paste0(i)]]))
            if(i == 1) {
                plot(
                    real_multipliers,
                    log(values) - log(null_value),
                    ylim = c(greatest_negative_difference, greatest_positive_difference),
                    xlim = c(0.5, 1.5),
                    xlab = paste0(sensitivity_variable, ' (multiplier)'),
                    ylab = ylab,
                    type = 'b',
                    lwd = 4
                )
            } else {
                points(
                    real_multipliers,
                    log(values) - log(null_value),
                    type = 'b',
                    col = colors[i],
                    lwd = 4
                )
            }
        }
        greatest_differences = c(greatest_differences, greatest_difference_all_5)
        greatest_difference_indices_matrix = rbind(greatest_difference_indices_matrix, greatest_difference_indices)
    }
    plot(NULL, xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    legend("bottom", row.names, lwd = 4, col = colors)
    dev.off()
    list(gd = greatest_differences, gdim = greatest_difference_indices_matrix)
}

panelwise_interesting_sensitivity_fn = function(
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
    dd = NULL
) {
    max_j = length(unique_ids)
    sensitivity_multipliers = c(0.5, 1, 1.5)

    if(is.null(dd)) {
        dd = make_dd(max_j, sensitivity_multipliers, kConstants,
                     folder_name, unique_ids, is_baselines,
                     community_transmissions, work_R0s, dormitory_R0s, E0,
                     initial_recovereds, initial_V2s, n_sims)
    }
    #dd <<- dd

    l_si = make_paneled_plot('v5-summary-sensitivity-plots-si.png',
                             symptomatic_infections,
                             'Symptomatic infections (multiplier)', dd,
                             kConstants, sensitivity_multipliers, max_j)
    l_su = make_paneled_plot('v5-summary-sensitivity-plots-su.png',
                             shiftwise_unavailable, 
                             'Shifts unavailable (multiplier)', dd,
                             kConstants, sensitivity_multipliers, max_j)
    #l_tc = make_paneled_plot('v5-summary-sensitivity-plots-tc.png', total_cost, 'Total cost (multiplier)')

    #list(gd_si = l_si$gd, gd_su = l_su$gd, gd_tc = l_tc$gd, gdim_si = l_si$gdim, gdim_su = l_su$gdim, gdim_tc = l_tc$gdim, dd = dd)
    list(gd_si = l_si$gd, gd_su = l_su$gd, gdim_si = l_si$gdim, gdim_su = l_su$gdim, dd = dd)
}

dd = readRDS('saved_dd.RDS')

l = panelwise_interesting_sensitivity_fn(
    'sensitivity-2022-11-22',
    c('farm', 'facility', 'facilitylike-farm', 'farmlike-facility',
      'farm-start-of-epidemic', 'facility-start-of-epidemic', 'facilitylike-farm-start-of-epidemic', 'farmlike-facility-start-of-epidemic',
      'farm-dec-11', 'facility-dec-11', 'facilitylike-farm-dec-11', 'farmlike-facility-dec-11',
      'no-recovered-farm',
      'facility-no-recovered', 'facilitylike-farm-no-recovered', 'farmlike-facility-no-recovered'),
    c(TRUE, TRUE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE,
      FALSE, 
      FALSE, FALSE, FALSE),
    c(0, 0.002, 0.002, 0,
      0, 0.002, 0.002, 0,
      0, 0.002, 0.002, 0,
      0,
      0.002, 0.002, 0),
    c(6, 6, 6, 6,
      6, 6, 6, 6,
      6, 6, 6, 6,
      6,
      6, 6, 6),
    c(2, 0, 0, 2,
      2, 0, 0, 2,
      2, 0, 0, 2,
      2,
      0, 0, 2),
    1,
    c(71, 71, 71, 71,
      0, 0, 0, 0,
      23, 23, 23, 23,
      0,
      0, 0, 0),
    c(73, 73, 73, 73,
      0, 0, 0, 0,
      0, 0, 0, 0,
      73, 
      73, 73, 73),
    100,
    dd = dd
)
v = pmax(l$gd_si, l$gd_su)
print(sort(v))
cutoff = sort(v)[16]
print(names(kConstants)[v >= cutoff])
#dd = l$dd
#saveRDS(dd, 'saved_dd.RDS')
# [1] 0.02943518 0.03385564 0.05364339 0.05606389 0.07001799 0.07627487
# [7] 0.07748810 0.08334730 0.10040950 0.11874043 0.12788328 0.18079600
#[13] 0.19454219 0.20439436 0.26785635 0.30954087 0.31035215 0.31138418
#[19] 0.33153395 0.34520592 0.35681882 0.38269800 0.41976294 0.46789730
#[25] 0.49411372 0.59414166 0.63206588 0.65465846 0.77133011 0.94025349
#[31] 0.94748234 1.20980732 1.22516004
# [1] "isolation_duration"           "mu"
# [3] "duration_IP_mean"             "duration_IP_shape"
# [5] "duration_IA_mean"             "duration_IM_mean"
# [7] "duration_IS_mean"             "p_trans_IP"
# [9] "p_trans_IA"                   "p_trans_IM"
#[11] "boosting_interval"            "complete_immunity_duration_R"
#[13] "B_magnitude_1"                "B_magnitude_2"
#[15] "B_decay_rate_1"               "B_decay_rate_2"
#[17] "SEVERE_MULTIPLIER"            "R_question_period"

