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
            ',initial_recovered-', initial_recovered,
            ',initial_V2-', initial_V2,
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
#d <<- d
#print(summary(d))
    get_real_multiplier = function(sensitivity_variable,
                                   theoretical_multiplier) {
        #if(theoretical_multiplier == 1) { # This shouldn't be necessary
        #    return(1)
        #}
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
#print('split')
    for(sensitivity_variable in names(kConstants)) {
        real_multipliers = sapply(sensitivity_multipliers, function(m) get_real_multiplier(sensitivity_variable, m))
        for(sensitivity_multiplier in real_multipliers) {
            if(sensitivity_multiplier != 1) {
                key = paste0(sensitivity_variable, '-', sensitivity_multiplier)
                d = make_batch(d, key)
#print(summary(d))
            }
        }
    }
d_test <<- d
#print('derp')
    make_paneled_plot = function(filename, outcome_fn, ylab) {
#print(summary(d))
        si = lapply(d, outcome_fn)
#print('bing')
        si_mean_max = max(sapply(si, mean)) 
        si_mean_min = min(sapply(si, mean))

        png(filename, height = 200*5, width = 200*7)
        layout(matrix(c(1:29, 34, 30:34), ncol = 7))
#print('bleeb')    
        for(sensitivity_variable in names(kConstants)) {
            real_multipliers = sapply(
                sensitivity_multipliers,
                function(m) get_real_multiplier(sensitivity_variable, m)
            )

            #key_05 = paste0(sensitivity_variable, '-', sensitivity_multiplier_05)
            #key_15 = paste0(sensitivity_variable, '-', sensitivity_multiplier_15)
#print('hung')        
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
#print('dwang!')
                #keys = c(
                #    paste0(i,key_05),
                #    i,
                #    paste0(i,key_15)
                #)
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
#print('weeb')
    make_paneled_plot('duplicate-farmlike-facility-betterer-sensitivity-plots-si.png', symptomatic_infections, 'Mean total symptomatic infections')
#print('wob')
    make_paneled_plot('duplicate-farmlike-facility-betterer-sensitivity-plots-su.png', shiftwise_unavailable, 'Mean total shifts unavailable')
}

#sensitivity_fn('sensitivity-2022-10-29', 'farmlike-facility-pass-6', 0, 6, 2, 1, 71, 73, 100)


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
    n_sims
) {
    max_j = length(unique_ids)
    #dormitory_R0s = sapply(
    #    dormitory_R0s,
    #    function(i) {
    #        ifelse(i == 0),
    #        '',
    #        i
    #    }
    #)

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
            ',initial_recovered-', initial_recovereds[index_j],
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
            #print(filename)
            d[[paste0(i, key)]] = readRDS(filename)#rds_filename(prepended_key, i, index_j))
        }
        d
    }
    dd = list()
    for(index_j in 1:max_j) {
        #unique_id = unique_ids[index_j]
        dd[[index_j]] = make_batch(list(), '', index_j)
    }
#d <<- d
#print(summary(d))
    get_real_multiplier = function(sensitivity_variable,
                                   theoretical_multiplier) {
        #if(theoretical_multiplier == 1) { # This shouldn't be necessary
        #    return(1)
        #}
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
#print('split')
    for(sensitivity_variable in names(kConstants)) {
        real_multipliers = sapply(sensitivity_multipliers, function(m) get_real_multiplier(sensitivity_variable, m))
        for(sensitivity_multiplier in real_multipliers) {
            if(sensitivity_multiplier != 1) {
                key = paste0(sensitivity_variable, '-', sensitivity_multiplier)
                for(index_j in 1:max_j) {
                    dd[[index_j]] = make_batch(dd[[index_j]], key, index_j)
                }
#print(summary(d))
            }
        }
    }

    dd_ = list()
    for(name in names(dd[[1]])) {
        dd_[[name]] = lapply(
            1:max_j,
            function(index_j) {
                dd[[index_j]][[name]]
            }
        )
    }

    #dn_1 = unique(sapply(names(dd_), function(name) substr(name, 2, nchar(name))))
    #dn_2 = list()
    
    #for(k in 1:(length(dn_1))) {#############
    #    dn_2[[k]] = c(
    #}

    dd_ <<- dd_
    dd <<- dd

#stop('Good enough for now')
#print('derp')
    make_paneled_plot = function(filename, outcome_fn, ylab) {
#print(summary(d))
        #si = lapply(
        #    dd,
        #    function(l) {
        #        lapply(l, symptomatic_infections)#outcome_fn)
        #    }
        #)
        #si_ = sapply(dd, function)
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
#print('bing')
        #si_mean_max = max(sapply(si, mean)) 
        #si_mean_min = min(sapply(si, mean))

        png(filename, height = 200*5, width = 200*7)
        layout(matrix(c(1:29, 34, 30:34), ncol = 7))
#print('bleeb')


        #figuring out bounds:
        greatest_positive_difference = 0
        greatest_negative_difference = 0
        for(sensitivity_variable in names(kConstants)) {
            real_multipliers = sapply(
                sensitivity_multipliers,
                function(m) get_real_multiplier(sensitivity_variable, m)
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
                    this_greatest_positive_difference = max(sapply(
                        values,
                        function(value) {
                            log(value) - log(null_value)
                        }
                    ))
                    this_greatest_negative_difference = min(sapply(
                        values,
                        function(value) {
                            log(value) - log(null_value)
                        }
                    ))
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

        #actually doing it
        for(sensitivity_variable in names(kConstants)) {
            real_multipliers = sapply(
                sensitivity_multipliers,
                function(m) get_real_multiplier(sensitivity_variable, m)
            )
            
            greatest_difference_all_5 = 0

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
                    print(this_greatest_difference)
                    if(this_greatest_difference >= greatest_difference) {
                        greatest_difference = this_greatest_difference
                        gd_j = j
                        print(gd_j)
                    }
                }
                if(greatest_difference > greatest_difference_all_5) {
                    greatest_difference_all_5 = greatest_difference
                }


                values = sapply(keys, function(key) mean(outcome_fn(dd[[gd_j]][[key]])))
                null_value = mean(outcome_fn(dd[[gd_j]][[paste0(i)]]))
                if(i == 1) {
                    plot(
                        real_multipliers,
                        log(values) - log(null_value),
                        #ylim = c(-greatest_difference, greatest_difference)#c(log(si_mean_min) - log(null_value), log(si_mean_max) - log(null_value)),
                        ylim = c(greatest_negative_difference, greatest_positive_difference),
                        xlim = c(0.5, 1.5),
                        xlab = paste0(sensitivity_variable, ' (multiplier)'),
                        ylab = ylab,
                        #main = unique_ids[gd_j],
                        type = 'b',
                        lwd = 4#9#8#4
                    )
                } else {
                    points(
                        real_multipliers,
                        log(values) - log(null_value),
                        type = 'b',
                        col = colors[i],
                        lwd = 4#9 - 2*i#4
                    )
                }
            }
            greatest_differences = c(greatest_differences, greatest_difference_all_5)
        }
        plot(NULL, xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
        legend("bottom", row.names, lwd = 4, col = colors)
        dev.off()
        greatest_differences
    }
#print('weeb')
    greatest_si_differences = make_paneled_plot('v4-summary-sensitivity-plots-si.png', symptomatic_infections, 'Symptomatic infections (multiplier)')
#print('wob')
    greatest_su_differences = make_paneled_plot('v4-summary-sensitivity-plots-su.png', shiftwise_unavailable, 'Shifts unavailable (multiplier)')
    list(si = greatest_si_differences, su = greatest_su_differences)
}

l = panelwise_interesting_sensitivity_fn(
    'sensitivity-2022-10-29',
    c('farmlike-facility-pass-6', 'facility-pass-6', 'farm-pass-6', 'no-vax-farm-pass-6', 'no-vax-farmlike-facility-pass-6',
      'no-vax-no-recovered-farm-pass-6','no-recovered-farm-pass-6', 'simple-no-boost-farm-pass-6'),
    c(FALSE, TRUE, TRUE, FALSE, FALSE,
      FALSE, FALSE, FALSE),
    c(0, 0.002, 0, 0, 0,
      0, 0, 0),
    c(6, 6, 6, 6, 6,
      6, 6, 6),
    c(2, 0, 2, 2, 2,
      2, 2, 2),
    1,
    c(71, 71, 71, 71, 71,
      0, 0, 71),
    c(73, 73, 73, 0, 0,
      0, 73, 73),
    100
)
