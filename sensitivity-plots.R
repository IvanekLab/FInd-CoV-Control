source('constants.R')

symptomatic_infections = function(df) {
    apply(df[,'new_symptomatic_infections',], 2, sum)
}

shiftwise_unavailable = function(data) {
    apply(data[,'qn_absent',], 2, sum)
}

d = list()
d[['1']] = readRDS('sensitivity-2022-10-29/facility-pass-6baseline_community-0.002,work_R0-6,E0-1,initial_recovered-71,initial_V2-73,n_sims-100index_i-1_full-output.rds')
d[['2']] = readRDS('sensitivity-2022-10-29/facility-pass-6baseline_community-0.002,work_R0-6,E0-1,T.test-38,initial_recovered-71,initial_V2-73,n_sims-100index_i-2_full-output.rds')
d[['3']] = readRDS('sensitivity-2022-10-29/facility-pass-6baseline_community-0.002,work_R0-6,E0-1,v.test-0.3-rational,initial_recovered-71,initial_V2-73,n_sims-100index_i-3_full-output.rds')
d[['4']] = readRDS('sensitivity-2022-10-29/facility-pass-6baseline_community-0.002,work_R0-6x(1-0.4),E0-1,initial_recovered-71,initial_V2-73,n_sims-100index_i-4_full-output.rds')
d[['5']] = readRDS('sensitivity-2022-10-29/facility-pass-6baseline_community-0.002,work_R0-6,E0-1,vax-rate0.02,initial_recovered-71,initial_V2-73,n_sims-100index_i-5_full-output.rds')
#adding '_' to the end of key is necessary, to avoid R's obnoxious special casing: 
#From ?names: The name ‘""’ is special: it is used to indicate that there is no
#name associated with an element of a (atomic or generic) vector. Subscripting
#by ‘""’ will match nothing (not even elements which have no name). – 
#nicola
#Sep 23, 2016 at 18:07
for(sensitivity_variable in names(kConstants)) {
    for(sensitivity_multiplier in c(0.5, 1.5)) {
        kConstants_ = kConstants
        kConstants_[[sensitivity_variable]] = sensitivity_multiplier * kConstants_[[sensitivity_variable]]
        ccl = check_consistency(kConstants_, altered_single_parameter = sensitivity_variable)
        kConstants_fixed = get('fixed_constants', ccl)
        if(!get('consistent', ccl) && !get('fixed', ccl)) {
            stop('Unfixable constants')
        }
        sensitivity_multiplier = get(sensitivity_variable, kConstants_fixed) / get(sensitivity_variable, kConstants)
        key = paste0(sensitivity_variable, '-', sensitivity_multiplier)
        d[[paste0(1,key)]] = readRDS(paste0('sensitivity-2022-10-29/facility-pass-6-', key, 'baseline_community-0.002,work_R0-6,E0-1,initial_recovered-71,initial_V2-73,n_sims-100index_i-1_full-output.rds')) #"baseline" is included because sensitivitiy variable & multiplier are not yet in the test for whether that name should be included
        d[[paste0(2,key)]] = readRDS(paste0('sensitivity-2022-10-29/facility-pass-6-', key, 'baseline_community-0.002,work_R0-6,E0-1,T.test-38,initial_recovered-71,initial_V2-73,n_sims-100index_i-2_full-output.rds')) #"baseline" is included because sensitivitiy variable & multiplier are not yet in the test for whether that name should be included
        d[[paste0(3,key)]] = readRDS(paste0('sensitivity-2022-10-29/facility-pass-6-', key, 'baseline_community-0.002,work_R0-6,E0-1,v.test-0.3-rational,initial_recovered-71,initial_V2-73,n_sims-100index_i-3_full-output.rds')) #"baseline" is included because sensitivitiy variable & multiplier are not yet in the test for whether that name should be included
        d[[paste0(4,key)]] = readRDS(paste0('sensitivity-2022-10-29/facility-pass-6-', key, 'baseline_community-0.002,work_R0-6x(1-0.4),E0-1,initial_recovered-71,initial_V2-73,n_sims-100index_i-4_full-output.rds')) #"baseline" is included because sensitivitiy variable & multiplier are not yet in the test for whether that name should be included
        d[[paste0(5,key)]] = readRDS(paste0('sensitivity-2022-10-29/facility-pass-6-', key, 'baseline_community-0.002,work_R0-6,E0-1,vax-rate0.02,initial_recovered-71,initial_V2-73,n_sims-100index_i-5_full-output.rds')) #"baseline" is included because sensitivitiy variable & multiplier are not yet in the test for whether that name should be included
    }
}

si = list()
su = list()
for(key in names(d)) {
    si[[key]] = symptomatic_infections(d[[key]])
    su[[key]] = shiftwise_unavailable(d[[key]])
}

si_mean_max = max(sapply(si, mean)) 
su_mean_max = max(sapply(su, mean))
si_mean_min = min(sapply(si, mean)) 
su_mean_min = min(sapply(su, mean))

limited_runs_index = c(1,2,4,9,13)
c4 = c('black', 'blue3', 'lightblue1', 'red2', 'gray80', 'darkgreen', 'yellow2')

colors = c(c4[1],
           c4[2],
           c4[3],
           c4[3],
           c4[3],
           c4[4],
           c4[4],
           c4[5],
           c4[5],
           c4[5],
           c4[6],
           c4[6],
           c4[7]
)[limited_runs_index]

row.names<-c(     "Baseline",
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

png('betterer-sensitivity-plots-si.png', height = 200*5, width = 200*7)
layout(matrix(c(1:29, 34, 30:34), ncol = 7))
for(sensitivity_variable in names(kConstants)) {
    kConstants_ = kConstants
    kConstants_[[sensitivity_variable]] = 0.5 * kConstants_[[sensitivity_variable]]
    ccl = check_consistency(kConstants_, altered_single_parameter = sensitivity_variable)
    kConstants_fixed = get('fixed_constants', ccl)
    if(!get('consistent', ccl) && !get('fixed', ccl)) {
        stop('Unfixable constants')
    }
    sensitivity_multiplier_05 = get(sensitivity_variable, kConstants_fixed) / get(sensitivity_variable, kConstants)

    key_05 = paste0(sensitivity_variable, '-', sensitivity_multiplier_05)
    kConstants_ = kConstants
    kConstants_[[sensitivity_variable]] = 1.5 * kConstants_[[sensitivity_variable]]
    ccl = check_consistency(kConstants_, altered_single_parameter = sensitivity_variable)
    kConstants_fixed = get('fixed_constants', ccl)
    if(!get('consistent', ccl) && !get('fixed', ccl)) {
        stop('Unfixable constants')
    }
    sensitivity_multiplier_15 = get(sensitivity_variable, kConstants_fixed) / get(sensitivity_variable, kConstants)
    key_15 = paste0(sensitivity_variable, '-', sensitivity_multiplier_15)

    for(i in 1:5) {
        keys = c(
            paste0(i,key_05),
            i,
            paste0(i,key_15)
        )
        if(i == 1) {
            plot(
                c(sensitivity_multiplier_05, 1, sensitivity_multiplier_15),
                sapply(keys, function(s) mean(si[[s]])),
                ylim = c(0, si_mean_max),
                xlim = c(0.5, 1.5),
                xlab = paste0(sensitivity_variable, ' (multiplier)'),
                ylab = 'Mean total symptomatic infections',
                type = 'b',
                lwd = 4
            )
        } else {
            points(
                c(sensitivity_multiplier_05, 1, sensitivity_multiplier_15),
                sapply(keys, function(s) mean(si[[s]])),
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
#print('here at least?')
png('betterer-sensitivity-plots-su.png', height = 200*5, width = 200*7)
layout(matrix(c(1:29, 34, 30:34), ncol = 7))
for(sensitivity_variable in names(kConstants)) {
    print(sensitivity_variable)
    kConstants_ = kConstants
    kConstants_[[sensitivity_variable]] = 0.5 * kConstants_[[sensitivity_variable]]
    ccl = check_consistency(kConstants_, altered_single_parameter = sensitivity_variable)
    kConstants_fixed = get('fixed_constants', ccl)
    if(!get('consistent', ccl) && !get('fixed', ccl)) {
        stop('Unfixable constants')
    }
    sensitivity_multiplier_05 = get(sensitivity_variable, kConstants_fixed) / get(sensitivity_variable, kConstants)
    key_05 = paste0(sensitivity_variable, '-', sensitivity_multiplier_05)

    kConstants_ = kConstants
    kConstants_[[sensitivity_variable]] = 1.5 * kConstants_[[sensitivity_variable]]
    ccl = check_consistency(kConstants_, altered_single_parameter = sensitivity_variable)
    kConstants_fixed = get('fixed_constants', ccl)
    if(!get('consistent', ccl) && !get('fixed', ccl)) {
        stop('Unfixable constants')
    }
    sensitivity_multiplier_15 = get(sensitivity_variable, kConstants_fixed) / get(sensitivity_variable, kConstants)
    key_15 = paste0(sensitivity_variable, '-', sensitivity_multiplier_15)

    for(i in 1:5) {
        keys = c(
            paste0(i,key_05),
            i,
            paste0(i,key_15)
        )
        if(i == 1) {
            plot(
                c(sensitivity_multiplier_05, 1, sensitivity_multiplier_15),
                sapply(keys, function(s) mean(su[[s]])),
                ylim = c(0, su_mean_max),
                xlim = c(0.5, 1.5),
                xlab = paste0(sensitivity_variable, ' (multiplier)'),
                ylab = 'Mean total shifts unavailable',
                type = 'b',
                lwd = 4
            )
        } else {
            points(
                c(sensitivity_multiplier_05, 1, sensitivity_multiplier_15),
                sapply(keys, function(s) mean(su[[s]])),
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
