source('constants.R')

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

shiftwise_unavailable_fraction = function(df) {
    #print(summary(df))
    #print(dimnames(df))
    df[,'qn_absent',] / df[,'qn_scheduled',]
}

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
    sensitivity_variables,
    summary_names,
    summary_fns,
    masks
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
            data_ = readRDS(filename)
            key_ = paste0(i, key)
            d[[key_]] = list()
            for(k in 1:length(summary_names)) {
                d[[key_]][[summary_names[k]]] = summary_fns[[k]](data_, masks[[index_j]], i)
            }
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

    make_csv = function(filename, outcome_name) {
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
                        #value = mean(outcome_fn(dd[[j]][[key]]))
                        value = dd[[j]][[key]][[outcome_name]]
                        values = rbind(values, data.frame(parameter_set = unique_ids[j], sensitivity_variable = sensitivity_variable, multiplier = multiplier, intervention = intervention, value = value))
                    }
                }
            }
        }
        write.csv(values, filename)
    }

    make_csv('v7-sensitivity_symptomatic_infections.csv', 'symptomatic_infections')
    make_csv('v7-sensitivity_shifts_unavailable.csv', 'shifts_unavailable')
    make_csv('v7-sensitivity_total_cost.csv', 'total_cost')
}

facility_production_mask = c(sapply(0:13, function(y) 21 * y + c(sapply(0:4, function(x) 3*x + 1:2))))[1:130]
farm_production_mask = c(sapply(0:13, function(y) 21 * y + c(sapply(0:4, function(x) 3*x + 1))))[1:65]

masks = rep(list(farm_production_mask, facility_production_mask), 8)

l = sensitivity_csvs(
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
    names(kConstants),            
    c('symptomatic_infections', 'shifts_unavailable', 'total_cost'),
    c(function(x, mask, i) mean(symptomatic_infections(x, mask, i)),
      function(x, mask, i) mean(shiftwise_unavailable(x, mask, i)),
      function(x, mask, i) mean(g(x, mask, i))),
    masks
)
