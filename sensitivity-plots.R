source('constants.R')

#TBD: FIX THIS
#facility
#output_per_week = 784346.67
#farm
output_per_week = 1680000
supervisors = 3
output_per_shift = output_per_week / (5 * (1 + (supervisors > 1)))
hourly_wage = 13.89
N = 103


symptomatic_infections = function(df, mask, limited_i) { #raw_i not used
    apply(df[,'new_symptomatic_infections',], 2, sum)
}

shiftwise_unavailable = function(df, mask, limited_i) { #raw_i not used
    apply(df[,'qn_absent',], 2, sum)
}

shiftwise_scheduled = function(data) {
    data[,'qn_scheduled',]
}

#shiftwise_scheduled = function(data) {
#    data[,'qn_scheduled',]
#}

shiftwise_unavailable_fraction = function(df) {
    #print(summary(df))
    #print(dimnames(df))
    df[,'qn_absent',] / df[,'qn_scheduled',]
}

#shiftwise_short = function(data) {
#    shiftwise_unavailable_fraction(data) > .15
#}

shiftwise_production_loss = function(df, mask) {
    #print(summary(df))
    #print(dimnames(df))
    #print('---')
    #print(mask)
    df = df[mask,,]
    fraction_available = 1 - shiftwise_unavailable_fraction(df)
    adjusted_fraction_available = pmin(fraction_available / 0.85, 1)
    fractional_production = adjusted_fraction_available^0.437
    fractional_loss = 1 - fractional_production

    #apply(
    fractional_loss * output_per_shift
    #, 2, sum)
}


limited_runs_index = c(1,2,4,9,13)

temperature_screening_cost = function(data) {
    thermometer_cost_each <- 20 # $20 per thermometer 
    KN95_cost <- 1 # $1 per mask per day 
    face_shield_cost <- 3 # $3 per face shield. Changing every 30 days. ($0.1/day) 
    ts_time <- 3 # 3 seconds for each screening
    ts_limit <- 5 #screening should be completed under 5 minute

    scheduled = shiftwise_scheduled(data)
    available = scheduled - shiftwise_unavailable(data)
    screeners = ceiling(scheduled) / (ts_limit * 60 / ts_time)
    ts_time <- available * ts_time / screeners / 3600   # Actual daily screening time in hours
    compensation <- ts_time * screeners * hourly_wage * 2 # have to pay the screeners, and the people being screened
    screener_training_cost = ceiling(N/100) * hourly_wage #max(screeners) * hourly_wage # 1hour training cost for screeners
    thermometer_cost <- max(screeners) * thermometer_cost_each

    initial_cost = screener_training_cost + thermometer_cost
    ongoing_cost = compensation + (KN95_cost + face_shield_cost/30) * screeners

    ongoing_cost[1] = ongoing_cost[1] + initial_cost

    ifelse(is.na(ongoing_cost), 0, ongoing_cost) #needs modification if we ever end up plotting over time
}

virus_testing_cost = function(data) {
    #array(0, c(dim(data)[1], dim(data)[3]))
    vt_kit <- 10 # $10 per test
    vt_time <- 1/4 # 15 minutes waiting assumed
    #vt_prod <- output_per_week / 5 / 8 * vt_time #production value during 15 min ts_time (5days/wk, 8hr/day)

    # Average wage compensation + kit cost over simulation
    vt <- data[,'tests',] # number of tests
    vt_cost <- vt * (vt_time * hourly_wage + vt_kit) # total cost

    vt_cost
}

vaccination_cost = function(data) {
    #array(0, c(dim(data)[1], dim(data)[3]))
    ############## Vaccination ############
    # 0.75 hour paid sick leave per vaccination  
    # no production loss
    data[,'doses',] * hourly_wage * 0.75
}

R0_reduction_cost = function(data, kludge_index) {
    face_shield <- 3 # $3 per face shield. Changing every month (30 days)
    KN95 <- 1 # $1 per N95 per shift
    air_cleaner <- 1000 # 1 air cleaner per 1000 sqft
    life <- 3 * 365 # 3year life of air_cleaner
    bi_available <- shiftwise_scheduled(data) - shiftwise_unavailable(data)
    bi_avilable = ifelse(is.na(bi_available), 0, bi_available)
#    array(0, c(dim(data)[1], dim(data)[3]))
    if(kludge_index == 8) {
        bi_cost = KN95 * bi_available
    } else if(kludge_index == 9 || (kludge_index == 10 && farm_or_facility == 'farm')) {
        bi_cost = ((KN95 + face_shield/30) * bi_available)
    } else if(kludge_index == 10) {
        bi_cost = ((KN95 + face_shield/30) * bi_available) + size/1000 * air_cleaner / life 
    } else {
        stop(kludge_index)
    }
    #print(dim(bi_cost))

    bi_cost
}

intervention_expenses_function = function(data, limited_i) {
    i = limited_runs_index[limited_i]
    if(i == 1) {
        array(0, c(dim(data)[1], dim(data)[3]))
    } else if(i == 2) {
        temperature_screening_cost(data)
    } else if(i %in% 3:5) {
        virus_testing_cost(data)
    } else if(i %in% c(6:7, 11:13)) {
        vaccination_cost(data)
    } else {
        R0_reduction_cost(data, i)
    }
}

g = function(data, mask, limited_i) {
    #print('spla')
    #print(mask)
    #print('alps')
    fd = shiftwise_production_loss(data, mask)
    r = intervention_expenses_function(data, limited_i)
    total = apply(fd, 2, sum) + apply(r, 2, sum)
    "cat(
        '\n\n\nlimited_i:', limited_i, '\ni:', limited_runs_index[limited_i],
        # '\nfd:', fd,
        '\napply(fd, 2, sum):', apply(fd, 2, sum),
        '\nmean(apply(fd, 2, sum)):', mean(apply(fd, 2, sum)),
        # '\nr:', r,
        '\napply(r, 2, sum):', apply(r, 2, sum), '\nmean(apply(r, 2, sum)):',
        mean(apply(r, 2, sum)), '\ntotal:', total, '\n\n\n'
    )"
    #if(limited_i == 5) {
    #    stop('Stopped here.')
    #}
    total
}

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
    n_sims,
    summary_names,
    summary_fns, #CONTINUE HERE
    masks
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
        data_ = readRDS(filename)
        key_ = paste0(i, key)
        d[[key_]] = list()
        #print(summary_names)
        #print(summary_fns)
        for(k in 1:length(summary_names)) {
            #print('chnug')
            #print(masks)
            #print(masks[index_j])
            #print('gunch')
            d[[key_]][[summary_names[k]]] = summary_fns[[k]](data_, masks[[index_j]], i)
            #cat('outcomes:', d[[key_]][[summary_names[k]]], '\n')
        }
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
    n_sims,
    summary_names,
    summary_fns,
    masks
) {
    dd = list()
    for(index_j in 1:max_j) {
        #cat('\n\n', index_j, ':', dormitory_R0s, '\n\n')
        dd[[index_j]] = make_batch(list(), '', index_j, max_j, folder_name,
                                   unique_ids, is_baselines,
                                   community_transmissions, work_R0s,
                                   dormitory_R0s, E0, initial_recovereds,
                                   initial_V2s, n_sims, summary_names, summary_fns,
                                   masks)
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
                                               n_sims,
                                               summary_names, summary_fns,
                                               masks)
                }
            }
        }
    }
    dd
}

select = function(..., selection_mode) {
}

make_paneled_plot = function(filename, outcome_name, ylab, dd, kConstants,
                             sensitivity_multipliers, max_j, selection_mode) {
    #print(outcome_name)
    png(filename, height = 200*5, width = 200*7)
    layout(matrix(c(1:29, 34, 30:34), ncol = 7))

    #figuring out bounds:
    greatest_positive_difference = 0
    greatest_negative_difference = 0
    variables_to_exclude = list()
    for(sensitivity_variable in names(kConstants)) {
        real_multipliers = sapply(
            sensitivity_multipliers,
            function(m) get_real_multiplier(sensitivity_variable, m, kConstants)
        )

        for(i in 1:5) {
            null_value = dd[[1]][[paste0(i)]][[outcome_name]]
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
                values = sapply(keys, function(key) dd[[j]][[key]][[outcome_name]])
                #print('on')
                #print(values)
                #print('off')
                this_greatest_positive_difference = max(0, log(values[3] / values[2]), log(values[1] / values[2]), na.rm = TRUE)
                this_greatest_negative_difference = min(0, log(values[3] / values[2]), log(values[1] / values[2]), na.rm = TRUE)
                if(this_greatest_positive_difference >= greatest_positive_difference) {
                    #print('Positive')
                    #print(values)
                    #print(this_greatest_positive_difference)
                    #print('End positive')
                    if(this_greatest_positive_difference == Inf) {
                        cat('\nPositive Infinite:\n', sensitivity_variable, '\n', i, '\n', j, '\n', values, '\n\n')
                        variables_to_exclude = c(variables_to_exclude, sensitivity_variable)
                    } else {
                        greatest_positive_difference = this_greatest_positive_difference
                    }
                }
                if(this_greatest_negative_difference <= greatest_negative_difference) {
                    #print('Negative')
                    #print(values)
                    #print(this_greatest_negative_difference)
                    #print('End negative')
                    if(this_greatest_negative_difference == Inf) {
                        cat('\nNegative Infinite:\n', sensitivity_variable, '\n', i, '\n', j, '\n', values, '\n\n')
                        variables_to_exclude = c(variables_to_exclude, sensitivity_variable)
                    } else {
                        greatest_negative_difference = this_greatest_negative_difference
                    }
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
    print(variables_to_exclude)
    for(sensitivity_index in 1:length(kConstants)) {
        sensitivity_variable = names(kConstants)[sensitivity_index]
        #cat(sensitivity_index, ':', sensitivity_variable, '\n')
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
                values = sapply(keys, function(key) dd[[j]][[key]][[outcome_name]])
                this_greatest_difference = max(sapply(
                    values,
                    function(value) {
                        max(0,
                            sapply(
                                values,
                                function(value_) {
                                    abs(log(value) - log(value_))
                                }
                            ),
                            na.rm = TRUE
                        )
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


            values = sapply(keys, function(key) dd[[gd_j]][[key]][[outcome_name]])
            null_value = dd[[gd_j]][[paste0(i)]][[outcome_name]]
            if(greatest_negative_difference == greatest_positive_difference ||
               greatest_negative_difference == -Inf ||
               greatest_positive_difference == Inf) {
                stop('FAILURE.')
            #} else if(sensitivity_variable %in% variables_to_exclude) {
            #    if(i == 1) {
            #        cat('SKIPPED:', sensitivity_index, ':', sensitivity_variable, '\n')
            #        plot.new()
            #    } else {
            #        cat('\tand again\n')
            #    }
            } else {
                #cat('PLOTTED:', sensitivity_index, ':', sensitivity_variable, '\n')
                #print('on')
                #print(greatest_negative_difference)
                #print('mid')
                #print(greatest_positive_difference)
                #print(values)
                #print(log(values))
                #print('off')
                if(i == 1) {
                    if(null_value == 0) {
                        log_differences = sign(values) * 10 * (greatest_positive_difference - greatest_negative_difference)
                    } else {
                        log_differences = log(values) - log(null_value)
                    }
                    #KLUDGE TIME!
                    #to_print = FALSE
                    #if(any(log_differences %in% c(-Inf, Inf))) {
                    #    to_print = TRUE
                    #    cat(sensitivity_variable, ': 1\n\t', log_differences, '\n', sep = '')
                    #}
                    #log_differences = ifelse(log_differences == Inf,
                    #   10 * (greatest_positive_difference - greatest_negative_difference),
                    #   ifelse(log_differences == -Inf,
                    #       -10 * (greatest_positive_difference - greatest_negative_difference),
                    #       log_differences
                    #   )
                    #)
                    #if(to_print) {
                    #    cat('\t', log_differences, '\n', sep = '')
                    #}
                    #END KLUDGE TIME
                    plot(
                        real_multipliers,
                        #log(values) - log(null_value),
                        log_differences,
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
        }
        greatest_differences = c(greatest_differences, greatest_difference_all_5)
        greatest_difference_indices_matrix = rbind(greatest_difference_indices_matrix, greatest_difference_indices)
    }
    plot(NULL, xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    legend("bottom", row.names, lwd = 4, col = colors)
    dev.off()
    print('PLOT COMPLETE')
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
    dd = NULL,
    summary_names,
    summary_fns,
    masks
) {
    max_j = length(unique_ids)
    sensitivity_multipliers = c(0.5, 1, 1.5)

    if(is.null(dd)) {
        dd = make_dd(max_j, sensitivity_multipliers, kConstants,
                     folder_name, unique_ids, is_baselines,
                     community_transmissions, work_R0s, dormitory_R0s, E0,
                     initial_recovereds, initial_V2s, n_sims,
                     summary_names, summary_fns, masks)
    }
    #dd <<- dd

    l_si = make_paneled_plot('v10-summary-sensitivity-plots-si.png',
                             'symptomatic_infections',
                             'Symptomatic infections (multiplier)', dd,
                             kConstants, sensitivity_multipliers, max_j)
    l_su = make_paneled_plot('v10-summary-sensitivity-plots-su.png',
                             'shifts_unavailable', 
                             'Shifts unavailable (multiplier)', dd,
                             kConstants, sensitivity_multipliers, max_j)
    l_tc = make_paneled_plot('v10-summary-sensitivity-plots-tc.png',
                             'total_cost',
                             'Total cost (multiplier)', dd,
                             kConstants, sensitivity_multipliers, max_j)

    #list(gd_tc = l_tc$gd, gdim_tc = l_tc$gdim, dd = dd)
    list(gd_si = l_si$gd, gd_su = l_su$gd, gd_tc = l_tc$gd, gdim_si = l_si$gdim, gdim_su = l_su$gdim, gdim_tc = l_tc$gdim, dd = dd)
    #list(gd_si = l_si$gd, gd_su = l_su$gd, gdim_si = l_si$gdim, gdim_su = l_su$gdim, dd = dd)
}

#dd = readRDS('saved_dd.RDS')

facility_production_mask = c(sapply(0:13, function(y) 21 * y + c(sapply(0:4, function(x) 3*x + 1:2))))[1:130]
farm_production_mask = c(sapply(0:13, function(y) 21 * y + c(sapply(0:4, function(x) 3*x + 1))))[1:65]

masks = rep(list(farm_production_mask, facility_production_mask), 8)

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
    dd = NULL,
    c('symptomatic_infections', 'shifts_unavailable', 'total_cost'),
    c(function(x, mask, i) mean(symptomatic_infections(x, mask, i)),
      function(x, mask, i) mean(shiftwise_unavailable(x, mask, i)),
      function(x, mask, i) mean(g(x, mask, i))),
    masks
)
v = pmax(l$gd_si, l$gd_su, l$gd_tc)
print(sort(v))
cutoff = sort(v)[15]
print(names(kConstants)[v >= cutoff])
dd = l$dd
saveRDS(dd, 'saved_dd_7.RDS')
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

