library(rpart)

#setwd('./scenario-analysis-take-2/')
#df1 = data.frame(

days = 90
N = 103

shiftwise_unavailable = function(data) {
    data[,'qn_absent',] 
}

shiftwise_scheduled = function(data) {
    data[,'qn_scheduled',]
}

shiftwise_unavailable_fraction = function(data) {
    data[,'qn_absent',] / data[,'qn_scheduled',]
}

shiftwise_short = function(data) {
    shiftwise_unavailable_fraction(data) > .15
}

shiftwise_production_loss = function(data) {
    fraction_available = 1 - shiftwise_unavailable_fraction(data)
    adjusted_fraction_available = pmin(fraction_available / 0.85, 1)
    fractional_production = adjusted_fraction_available^0.437
    fractional_loss = 1 - fractional_production

    fractional_loss * output_per_shift
}

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


generate_intervention_expenses_function = function() {
    i = 0

    function(data) {
        i <<- i + 1
        #print(i)
        #i_ = limited_runs_index[ceiling(i / double_wrap_num_sims)]
        #i_ = ceiling(i / double_wrap_num_sims)
        i_ = i
        #print(i)
        #print(limited_runs_index)
        #cat(i, ':', i_, '\n')
        if(i_ == 1) {
            array(0, c(dim(data)[1], dim(data)[3]))
        } else if(i_ == 2) {
            temperature_screening_cost(data)
        } else if(i_ %in% 3:5) {
            virus_testing_cost(data)
        } else if(i_ %in% c(6:7, 11:13)) {
            vaccination_cost(data)
        } else {
            R0_reduction_cost(data, i_)
        }
    }
}

intervention_fragments = c(
    '',
    'T.test-38,',
    'v.test-0.05-rational,', 
    'v.test-0.3-rational,', 
    'v.test-1-rational,',
    'vax-rate0.02,',
    'vax-rate0.04,',
    '',
    '',
    '',
    '',
    '',
    'vax-rate0.02,'
)

virus_tests = c(0, 0, 0.05, 0.3, 1.0, rep(0, 8))
r0_reductions = c(rep(0, 7), 0.2, 0.4, 0.8, 0, 0, 0)

#testing_rate = c(NA, NA, 0.05, 0.3, 1)

"intervention_fragment = function(i) {
    ifelse(i == 1,
        '',
        ifelse(i == 2,
            'T.test-38,',
            ifelse(i in 3:5,
                paste0('v.test-', testing_rate[i], ','
            )
        )
    )"

get_filename = function(housing, setting, vaccinated, recovered, i) {
    unique_id = paste0(
        setting, '-',
        housing,
        '-vaccinated_', vaccinated,
        '-recovered_', recovered
    )
    work_R0_fragment = ifelse(i == 8,
        'work_R0-6x(1-0.2),',
        ifelse(i == 9,
            'work_R0-6x(1-0.4),',
            ifelse(i == 10,
                'work_R0-6x(1-0.8),',
                'work_R0-6,'
            )
        )
    )

    filename = paste0(
        'sat2wi-clean/',
        unique_id,
        ifelse(
            vaccinated == TRUE & recovered == TRUE &
            (
                (setting == 'farm' & housing == 'shared') |
                (setting == 'facility' & housing == 'individual')
            ),
            'baseline',
            ''
        ),
        '_',
        ifelse(housing == 'individual',
            paste0('community-0.002,', work_R0_fragment),
            paste0('community-0,', work_R0_fragment, 'dormitory_R0-2,')
        ),
        'E0-1,',
        intervention_fragments[i],
        ifelse(recovered,
            'initial_recovered-71,',
            ''
        ),
        ifelse(vaccinated,
            'initial_V2-73,',
            ''
        ),
        'n_sims-1000index_i-',
        i,
        '_full-output.rds'
    )
    filename
}

df = NULL
for(housing in c('shared', 'individual')) {
    for(setting in c('farm', 'facility')) {
        for(vaccinated in c(FALSE, TRUE)) {
            for(recovered in c(FALSE, TRUE)) {
                cat('\n', housing, setting, vaccinated, recovered, '\n\n')
                intervention_expenses_function = generate_intervention_expenses_function()
                if(setting == 'farm') {
                    output_per_shift = 247612.00 / 5
                    hourly_wage = 13.89
                    size = NA
                } else {
                    output_per_shift = 784346.67 / 10
                    hourly_wage = 13.89
                    size = 1000
                }
                farm_or_facility = setting
                g = function(data) {
                    ad_hoc_production_mask = rep(c(TRUE, TRUE, FALSE), days)
                    fd = shiftwise_production_loss(data[ad_hoc_production_mask,,, drop = FALSE])
                    fd = ifelse(is.na(fd), 0, fd)
                    r = intervention_expenses_function(data)
                    r[ad_hoc_production_mask] = r[ad_hoc_production_mask] + fd
                    r
                }

                for(i in 1:13) {
                    filename = get_filename(housing, setting, vaccinated, recovered, i)
                    df_ = readRDS(filename)
                    symptomatic_infections = apply(df_[,'new_symptomatic_infections',],2, sum)
                    worker_shifts_unavailable = apply(df_[,'qn_absent',],2, sum)
                    total_cost = apply(g(df_), 2, sum)
                    #browser()

                    boosting = ifelse(i %in% c(11, 13),
                        0.02,
                        ifelse(i == 12,
                            0.04,
                            0
                        )
                    )
                    temperature_screening = ifelse(i == 2, TRUE, FALSE)
                    vax = ifelse(i %in% c(6, 13),
                        0.02,
                        ifelse(i == 7,
                            0.04,
                            0
                        )
                    )
                    virus_test = virus_tests[i]
                    r0_reduction = r0_reductions[i]
                
#tree = rpart(symptomatic_infections ~ df1$settings + df1$housing + df1$boosting + df1$temperature_screening + df1$vax + df1$virus_test + df1$r0_reduction)

                    df__ = data.frame(housing = housing, setting = setting, vaccinated = vaccinated, recovered = recovered, symptomatic_infections = symptomatic_infections, boosting = boosting, temperature_screening = temperature_screening, vax = vax, virus_test = virus_test, r0_reduction = r0_reduction, worker_shifts_unavailable = worker_shifts_unavailable, total_cost = total_cost)
                    if(is.null(df)) {
                        df = df__
                    } else {
                        df = rbind(df, df__)
                    }
                }
            }
        }
    }
}

df$boosting = factor(df$boosting)
df$temperature_screening = factor(df$temperature_screening)
df$vax = factor(df$vax)
df$virus_test = factor(df$virus_test)
df$r0_reduction = factor(df$r0_reduction)
df$recovered = factor(df$recovered)
df$vaccinated = factor(df$vaccinated)

png('symptomatic-tree.png', height = 900, width = 1600)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df)
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('unavailable-tree.png', height = 900, width = 1600)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df)
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('total-cost-tree-fixed-p.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df)
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('symptomatic-tree--facility.png', height = 900, width = 1600)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',])
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('unavailable-tree--facility.png', height = 900, width = 1600)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',])
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('total-cost-tree--facility.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',])
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('symptomatic-tree--farm.png', height = 900, width = 1600)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'farm',])
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('unavailable-tree--farm.png', height = 900, width = 1600)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'farm',])
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('total-cost-tree--farm.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'farm',])
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

"png('symptomatic-tree-balanced-infty.png', height = 900, width = 1600)
tree = rpart(symptomatic_infections ~ housing + vaccinated + recovered + virus_test + r0_reduction, data = df, control=rpart.control(minsplit=1, minbucket=1, cp=0)) #tried 0.001, 0.00001
plot(tree)
text(tree, pretty = 1, cex = 0.5)
dev.off()

png('symptomatic-tree-maximal.png', height = 4500, width = 8000)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df, control=rpart.control(minsplit=1, minbucket=1, cp=0))
plot(tree)
text(tree, pretty = 1)
dev.off()


png('symptomatic-tree-maximal-minus-setting.png', height = 4500, width = 8000)
tree = rpart(symptomatic_infections ~ housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df, control=rpart.control(minsplit=1, minbucket=1, cp=0))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()







png('unavailable-tree-maximum.png', height = 900, width = 1600)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df, control=rpart.control(minsplit=1, minbucket=1, cp=0.001))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()"

