symptomatic_infections = function(df) {
    apply(df[,'new_symptomatic_infections',], 2, sum)
}

shiftwise_unavailable_fraction = function(data) {
    apply(data[,'qn_absent',] / data[,'qn_scheduled',], 2, sum)
}

d = list()
d[['1']] = readRDS('sensitivity-2022-10-29/facility-pass-4baseline_community-0.002,work_R0-6,E0-1,initial_recovered-71,initial_V2-73,n_sims-1000index_i-1_full-output.rds')
d[['2']] = readRDS('sensitivity-2022-10-29/facility-pass-4baseline_community-0.002,work_R0-6,E0-1,T.test-38,initial_recovered-71,initial_V2-73,n_sims-1000index_i-2_full-output.rds')
d[['3']] = readRDS('sensitivity-2022-10-29/facility-pass-4baseline_community-0.002,work_R0-6,E0-1,v.test-0.3-rational,initial_recovered-71,initial_V2-73,n_sims-1000index_i-3_full-output.rds')
d[['4']] = readRDS('sensitivity-2022-10-29/facility-pass-4baseline_community-0.002,work_R0-6x(1-0.4),E0-1,initial_recovered-71,initial_V2-73,n_sims-1000index_i-4_full-output.rds')
d[['5']] = readRDS('sensitivity-2022-10-29/facility-pass-4baseline_community-0.002,work_R0-6,E0-1,vax-rate0.02,initial_recovered-71,initial_V2-73,n_sims-1000index_i-5_full-output.rds')
#adding '_' to the end of key is necessary, to avoid R's obnoxious special casing: 
#From ?names: The name ‘""’ is special: it is used to indicate that there is no
#name associated with an element of a (atomic or generic) vector. Subscripting
#by ‘""’ will match nothing (not even elements which have no name). – 
#nicola
#Sep 23, 2016 at 18:07
for(sensitivity_variable in c('isolation_duration', 'mu', 'sd')) {
    for(sensitivity_multiplier in c(0.5, 1.5)) {
        key = paste0(sensitivity_variable, '-', sensitivity_multiplier)
        d[[paste0(1,key)]] = readRDS(paste0('sensitivity-2022-10-29/facility-pass-4-', key, 'baseline_community-0.002,work_R0-6,E0-1,initial_recovered-71,initial_V2-73,n_sims-1000index_i-1_full-output.rds')) #"baseline" is included because sensitivitiy variable & multiplier are not yet in the test for whether that name should be included
        d[[paste0(2,key)]] = readRDS(paste0('sensitivity-2022-10-29/facility-pass-4-', key, 'baseline_community-0.002,work_R0-6,E0-1,T.test-38,initial_recovered-71,initial_V2-73,n_sims-1000index_i-2_full-output.rds')) #"baseline" is included because sensitivitiy variable & multiplier are not yet in the test for whether that name should be included
        d[[paste0(3,key)]] = readRDS(paste0('sensitivity-2022-10-29/facility-pass-4-', key, 'baseline_community-0.002,work_R0-6,E0-1,v.test-0.3-rational,initial_recovered-71,initial_V2-73,n_sims-1000index_i-3_full-output.rds')) #"baseline" is included because sensitivitiy variable & multiplier are not yet in the test for whether that name should be included
        d[[paste0(4,key)]] = readRDS(paste0('sensitivity-2022-10-29/facility-pass-4-', key, 'baseline_community-0.002,work_R0-6x(1-0.4),E0-1,initial_recovered-71,initial_V2-73,n_sims-1000index_i-4_full-output.rds')) #"baseline" is included because sensitivitiy variable & multiplier are not yet in the test for whether that name should be included
        d[[paste0(5,key)]] = readRDS(paste0('sensitivity-2022-10-29/facility-pass-4-', key, 'baseline_community-0.002,work_R0-6,E0-1,vax-rate0.02,initial_recovered-71,initial_V2-73,n_sims-1000index_i-5_full-output.rds')) #"baseline" is included because sensitivitiy variable & multiplier are not yet in the test for whether that name should be included
    }
}

si = list()
suf = list()
for(key in names(d)) {
    si[[key]] = symptomatic_infections(d[[key]])
    suf[[key]] = symptomatic_infections(d[[key]])
}

si_mean_max = max(sapply(si, mean)) 
suf_mean_max = max(sapply(suf, mean))

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

png('sample-sensitivity-plots.png', height = 900, width = 1600)
layout(matrix(1:6, ncol = 3))
for(sensitivity_variable in c('isolation_duration', 'mu', 'sd')) {
    for(i in 1:5) {
        keys = c(
            paste0(i,sensitivity_variable, '-', '0.5'),
            i,
            paste0(i,sensitivity_variable, '-', '1.5')
        )
        if(i == 1) {
            plot(
                (1:3)/2,
                sapply(keys, function(s) mean(si[[s]])),
                ylim = c(0, si_mean_max),
                xlab = paste0(sensitivity_variable, ' (multiplier)'),
                ylab = 'Mean total symptomatic infections',
                type = 'b'
            )
        } else {
            points(
                (1:3)/2,
                sapply(keys, function(s) mean(si[[s]])),
                type = 'b',
                col = colors[i]
            )
        }
    }
    for(i in 1:5) {
        keys = c(
            paste0(i,sensitivity_variable, '-', '0.5'),
            i,
            paste0(i,sensitivity_variable, '-', '1.5')
        )
        if(i == 1) {
            plot(
                (1:3)/2,
                sapply(keys, function(s) mean(suf[[s]])),
                ylim = c(0, suf_mean_max),
                xlab = paste0(sensitivity_variable, ' (multiplier)'),
                ylab = 'Mean total shifts unavailable',
                type = 'b',
                lwd = 2
            )
        } else {
            points(
                (1:3)/2,
                sapply(keys, function(s) mean(suf[[s]])),
                type = 'b',
                col = colors[i],
                lwd = 2
            )
        }
    }
}
legend("bottomright", row.names, lwd = 4, col = colors)
dev.off()
