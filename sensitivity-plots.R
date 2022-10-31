symptomatic_infections = function(df) {
    apply(df[,'new_symptomatic_infections',], 2, sum)
}

shiftwise_unavailable_fraction = function(data) {
    apply(data[,'qn_absent',] / data[,'qn_scheduled',], 2, sum)
}

d = list()
d[['_']] = readRDS('sensitivity-2022-10-29/facility-pass-1baseline_community-0.002,work_R0-6,E0-1,initial_recovered-71,initial_V2-73,n_sims-1000index_i-1_full-output.rds')
#adding '_' to the end of key is necessary, to avoid R's obnoxious special casing: 
#From ?names: The name ‘""’ is special: it is used to indicate that there is no
#name associated with an element of a (atomic or generic) vector. Subscripting
#by ‘""’ will match nothing (not even elements which have no name). – 
#nicola
#Sep 23, 2016 at 18:07
for(sensitivity_variable in c('duration_IA', 'duration_IP', 'duration_IM')) {
    for(sensitivity_multiplier in c(0.5, 1.5)) {
        key = paste0(sensitivity_variable, '-', sensitivity_multiplier)
        d[[key]] = readRDS(paste0('sensitivity-2022-10-29/facility-pass-1-', key, 'baseline_community-0.002,work_R0-6,E0-1,initial_recovered-71,initial_V2-73,n_sims-1000index_i-1_full-output.rds')) #"baseline" is included because sensitivitiy variable & multiplier are not yet in the test for whether that name should be included
    }
}

si = list()
suf = list()
for(key in names(d)) {
    si[[key]] = symptomatic_infections(d[[key]])
    suf[[key]] = symptomatic_infections(d[[key]])
}

si_mean_max = max(sapply(si, function(x) quantile(x, 0.975))) #now misnamed

layout(matrix(1:3, ncol = 3))
for(sensitivity_variable in c('duration_IA', 'duration_IP', 'duration_IM')) {
    keys = c(
        paste0(sensitivity_variable, '-', '0.5'),
        '_',
        paste0(sensitivity_variable, '-', '1.5')
    )
    plot(
        (1:3)/2,
        sapply(keys, function(s) mean(si[[s]])),
        ylim = c(0, si_mean_max),
        xlab = paste0(sensitivity_variable, ' (multiplier)'),
        ylab = 'Mean total symptomatic infections',
        type = 'b'
    )
    #for(quantile_ in c(0.01, 0.1, 0.25, 0.75, 0.9, 0.99)) {
    for(quantile_ in c(0.025, 0.25, 0.75, 0.975)) {
        points(
            (1:3)/2,
            sapply(keys, function(s) quantile(si[[s]], quantile_)),
            type = 'b',
            lty = ifelse(quantile_ %in% c(0.25, 0.75),
                2,
                3
            )
        )
    }
}

