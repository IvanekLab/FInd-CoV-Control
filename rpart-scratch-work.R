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

                    df__ = data.frame(housing = housing, setting = setting, vaccinated = vaccinated, recovered = recovered, symptomatic_infections = symptomatic_infections, boosting = boosting, temperature_screening = temperature_screening, vax = vax, virus_test = virus_test, r0_reduction = r0_reduction, worker_shifts_unavailable = worker_shifts_unavailable, total_cost = total_cost, run_number = 1:1000)
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
df$run_number = factor(df$run_number)

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

####
# Now, let's try eliminating bin sizes, and confirm this gives the same result
####
png('any-size-symptomatic-tree.png', height = 900, width = 1600)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df, control=rpart.control(minsplit=1, minbucket=1))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('any-size-unavailable-tree.png', height = 900, width = 1600)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df, control=rpart.control(minsplit=1, minbucket=1))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('any-size-total-cost-tree-fixed-p.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df, control=rpart.control(minsplit=1, minbucket=1))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('any-size-symptomatic-tree--facility.png', height = 900, width = 1600)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('any-size-unavailable-tree--facility.png', height = 900, width = 1600)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('any-size-total-cost-tree--facility.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('any-size-symptomatic-tree--farm.png', height = 900, width = 1600)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'farm',], control=rpart.control(minsplit=1, minbucket=1))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('any-size-unavailable-tree--farm.png', height = 900, width = 1600)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'farm',], control=rpart.control(minsplit=1, minbucket=1))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('any-size-total-cost-tree--farm.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'farm',], control=rpart.control(minsplit=1, minbucket=1))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

###
#Okay. Time to start trying smaller cp values. Default is 0.01, and current
#situation is:
#Symptomatic: transposition (under virus_test in (0, 0.05) & recovered = TRUE)
#of housing=shared and r0_reduction=0.8 means that we have:
#   Overall:    Housing == shared;                             <= Need to split
#               housing == individual & r0_reduction == 0.8
#               housing == individual & r0_reduction != 0.8
#   Facility:   r0_reduction = 0.8                             <= Need to split
#               housing == shared & r0_reduction != 0.8
#               housing == individual & r0_reduction != 0.8
#   Farm:       [as overall]
###

png('cp_0.001-symptomatic-tree.png', height = 900, width = 1600)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df, control=rpart.control(minsplit=1, minbucket=1, cp = 0.001))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('cp_0.001-symptomatic-tree--facility.png', height = 900, width = 1600)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp = 0.001))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('cp_0.001-symptomatic-tree--farm.png', height = 900, width = 1600)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'farm',], control=rpart.control(minsplit=1, minbucket=1, cp = 0.001))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

# Overall and farm are now splitting both; farm still not splitting
# r0_reduction. Other splits look good, I think?

png('cp_0.0001-symptomatic-tree--facility.png', height = 900, width = 1600)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp = 0.0001))
plot(tree)
text(tree, pretty = 1)
dev.off()

#no

png('cp_1e-5-symptomatic-tree--facility.png', height = 1800, width = 3200)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp = 1e-5))
plot(tree)
text(tree, pretty = 1)
dev.off()

#now splitting, but only if vaccinated is FALSE . . .
#and there's a lot of crap now. Tentatively figure that this doesn't work. What
#about unavailable and total_cost

#Unavailable is clean anyway

#Total costs:
#   Virus test == 1 always unsplit
#   0 and 0.05 (together) do not split further in overall and facility, but
#   split by r0_reduction == 0? in farm
#   Virus test == 0.3 splits by setting == farm and then (if _facility_, only) by
#   recovered == TRUE in overall.
#   In farm, it doesn't split at all (consistent with overall)
#   In facility, it splits by recovered == TRUE _and then_ by (if recovered ==
#   _FALSE_ only) by vaccinated
#
#   So the additional splits we need are:
#   0 and 0.05 by r0_reduction == 0? (needed in facility)
#   0.3 by recovered (and 0.3 & recovered == FALSE by vaccinated) (in farm)
#   So test this:

png('cp_1e-3-total-cost-tree', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df, control=rpart.control(minsplit=1, minbucket=1, cp = 1e-3))
plot(tree)
text(tree, pretty = 1)
dev.off()


png('cp_1e-3-total-cost-tree--farm.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'farm',], control=rpart.control(minsplit=1, minbucket=1, cp = 1e-3))
plot(tree)
text(tree, pretty = 1)
dev.off()

png('cp_1e-3-total-cost-tree--facility.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp = 1e-3))
plot(tree)
text(tree, pretty = 1)
dev.off()

#This is almost there. The needed splits from above are both there, but now
#facility is splitting (virus test == 0.3 & recovered == FALSE & vaccinated ==
#FALSE) by housing == individual, and farm is not.
#(Also, both farm and facility are splitting (0 or 0.05 and r0_reduction == 0)
#by virus test == 0 now. Which is fine.)
#Let's start by trying to _reduce_ sensitivity:

png('cp_5e-3-total-cost-tree--farm.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'farm',], control=rpart.control(minsplit=1, minbucket=1, cp = 5e-3))
plot(tree)
text(tree, pretty = 1)
dev.off()

png('cp_5e-3-total-cost-tree--facility.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp = 5e-3))
plot(tree)
text(tree, pretty = 1)
dev.off()

#facility is splitting by r0_reduction now (and neither is splitting again by
#virus test), but farm is not splitting 0.3 at all. So increasing the
#sensitivity again

png('cp_3e-3-total-cost-tree--farm.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'farm',], control=rpart.control(minsplit=1, minbucket=1, cp = 3e-3))
plot(tree)
text(tree, pretty = 1)
dev.off()

png('cp_3e-3-total-cost-tree--facility.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp = 3e-3))
plot(tree)
text(tree, pretty = 1)
dev.off()

#farm is still not splitting 0.3,and facility is now splitting by (0 or 0.05 and
#r0_reduction = 0) by virus test == 0 and farm is not (yet), but facility is not
#yet splitting by housing. So increase sensitivity again.

png('cp_2e-3-total-cost-tree--farm.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'farm',], control=rpart.control(minsplit=1, minbucket=1, cp = 2e-3))
plot(tree)
text(tree, pretty = 1)
dev.off()

png('cp_2e-3-total-cost-tree--facility.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp = 2e-3))
plot(tree)
text(tree, pretty = 1)
dev.off()

#farm and facility are now both splitting by virus test. 
#farm is now splitting by recovered, but not yet by vaccinated (if recovered is
#false)
#facility still not (yet) splitting by housing
#so reduce again

png('cp_0.0015-total-cost-tree--farm.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'farm',], control=rpart.control(minsplit=1, minbucket=1, cp = 0.0015))
plot(tree)
text(tree, pretty = 1)
dev.off()

png('cp_0.0015-total-cost-tree--facility.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp = 0.0015))
plot(tree)
text(tree, pretty = 1)
dev.off()

#bingo!
#and now the "overall" one


png('cp_0.0015-total-cost-tree.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df, control=rpart.control(minsplit=1, minbucket=1, cp = 0.0015))
plot(tree)
text(tree, pretty = 1)
dev.off()

#Looks decent, although "recovered" and "vaccinated" splits only occur within
#the facility branch (given that even at 1e-3, only the "recovered" split occurs
#in the farm branch, this is not too surprising.
#Now what about the other outcomes, starting with symptomatic

png('cp_0.0015-symptomatic-tree.png', height = 900, width = 1600)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df, control=rpart.control(minsplit=1, minbucket=1, cp = 0.0015))
plot(tree)
text(tree, pretty = 1, cex = 1)
dev.off()

png('cp_0.0015-symptomatic-tree--facility.png', height = 900, width = 1600)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp = 0.0015))
plot(tree)
text(tree, pretty = 1, cex = 1)
dev.off()

png('cp_0.0015-symptomatic-tree--farm.png', height = 900, width = 1600)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'farm',], control=rpart.control(minsplit=1, minbucket=1, cp = 0.0015))
plot(tree)
text(tree, pretty = 1, cex = 1)
dev.off()

#facility still isn't splitting by r0_reduction = 0.8
#but farm now _is_ splitting housing = shared!
#and both are splitting (housing == shared & r0_reduction != 0.8) by vaccinated
#and the rest agrees. So this is a modest improvement.

#now what about unavailable? does it become broken?

png('cp_0.0015-unavailable-tree.png', height = 900, width = 1600)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df, control=rpart.control(minsplit=1, minbucket=1, cp = 0.0015))
plot(tree)
text(tree, pretty = 1, cex = 1)
dev.off()

png('cp_0.0015-unavailable-tree--facility.png', height = 900, width = 1600)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp = 0.0015))
plot(tree)
text(tree, pretty = 1, cex = 1)
dev.off()

png('cp_0.0015-unavailable-tree--farm.png', height = 900, width = 1600)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'farm',], control=rpart.control(minsplit=1, minbucket=1, cp = 0.0015))
plot(tree)
text(tree, pretty = 1, cex = 1)
dev.off()

#looks good _except_ that virus_testing in (0, 1) -> temperature_screening ==
#TRUE splits on recovered for farm (and overall) and housing for facility.
#split on recovered for farm (node number 5) appears to have an "improve" of 0.07
#but it's labeled as having a "complexity parameter" of 0.002065031 (which
#still, if that's the necessary threshold, suggests that reducing sensitivity is
#not the answer here. So can I figure out the complexity parameter for splits of
#nodes 10 and 11 (and what predictor they would split on)
#it doesn't give me information here. But what if do the infinite thing:

tree_ = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'farm',], control=rpart.control(minsplit=1, minbucket=1, cp = 0))

#10 (recovered == TRUE) splits on housing at 0.001356923 (promising)
#11 (recovered == FALSE) splits on housing at 0.0006048121 (not so much)

tree__ = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp = 0))

#10 (housing == shared) splits on recovered at 0.001116043 (maybe ...)
#11 (housing == individual) splits on recovered at 0.0005926515 (again, not so
#much.)

#executive decision: Leaving things here for now.

#partial resumption: What happens (a) if I cross-validate more, but without any
#cp, (b) if I do Poisson regression (without any cp)
png('2023-03-07/unavailable.png', height = 900, width = 1600)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('2023-03-07/unavailable-maximal.png', height = 4500, width = 8000)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp = 0))
plot(tree)
text(tree, pretty = 1)
dev.off()

png('2023-03-07/unavailable-poisson.png', height = 900, width = 1600)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1), method = 'poisson')
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

png('2023-03-07/unavailable-poisson-maximal.png', height = 4500, width = 8000)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp = 0), method = 'poisson')
plot(tree)
text(tree, pretty = 1)
dev.off()

#Maximals are awful, even for Poisson. But could we make a consensus tree? And
#would that make sense?

#and what about using run numbers?
png('2023-03-07/unavailable-with-run-numbers.png', height = 4500, width = 8000)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction + run_number, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

#bit of a clusterfuck, naturally, but . . .

#  1) root 104000 773055600.0  72.88082
#    2) virus_test=0,1 88000 175151500.0  50.54544
#      4) temperature_screening=FALSE 80000 125756500.0  44.84490
#        8) virus_test=0 72000  66884820.0  39.92323 *
#        9) virus_test=1 8000  41431180.0  89.13996
#         18) housing=shared 4000   1378991.0  24.61350 *
#         19) housing=individual 4000   6742876.0 153.66640 *
#      5) temperature_screening=TRUE 8000  20798450.0 107.55080 *
#    3) virus_test=0.05,0.3 16000 312551100.0 195.72540
#      6) housing=shared 8000 189651300.0 145.68890
#       12) run_number= ...  75063450.0 103.08220 *
#       13) run_number= ... 97122300.0 196.92950
#         26) recovered=TRUE 1816  31260420.0 145.88290 *
#         27) recovered=FALSE 1816  56397770.0 247.97610
#           54) virus_test=0.05 908   3881447.0 180.58740 *
#           55) virus_test=0.3 908  44269420.0 315.36490
#            110) run_number= ...  30619720.0 230.22010
#              220) vaccinated=TRUE 290  11178910.0 162.23560
#                440) run_number= ...    252033.9  24.43262 *
#                441) run_number= ...    776712.7 416.22550 *
#              221) vaccinated=FALSE 290  16760120.0 298.20460
#                442) run_number= ...    179960.0  25.05149 *
#                443) run_number= ... 643437.7 499.38920 *
#            111) run_number= ... 2009607.0 465.92580 *
#      7) housing=individual 8000  82841370.0 245.76190

#Essentially, this is similar to the original tree
#Without run numbers, we have

#node), split, n, deviance, yval
#      * denotes terminal node

# 1) root 104000 773055600  72.88082  
#   2) virus_test=0,1 88000 175151500  50.54544  
#     4) temperature_screening=FALSE 80000 125756500  44.84490  
#       8) virus_test=0 72000  66884820  39.92323 *
#       9) virus_test=1 8000  41431180  89.13996  
#        18) housing=shared 4000   1378991  24.61350 *
#        19) housing=individual 4000   6742876 153.66640 *
#     5) temperature_screening=TRUE 8000  20798450 107.55080 *
#   3) virus_test=0.05,0.3 16000 312551100 195.72540  
#     6) housing=shared 8000 189651300 145.68890  
#      12) recovered=TRUE 4000  57806620 108.89380 *
#      13) recovered=FALSE 4000 121013700 182.48390 *
#     7) housing=individual 8000  82841370 245.76190  
#      14) virus_test=0.05 4000   6385420 178.08720 *
#      15) virus_test=0.3 4000  39817110 313.43650  
#        30) recovered=TRUE 2000  12827150 260.45330 *
#        31) recovered=FALSE 2000  15761100 366.41970 *

#Potentially interesting results (with run_number vs. without):
#   (1) node 7 doesn't split in the 
#   (2) node 6's split on recovered is _below_ the split on run_number

#What if we use differences vs. baseline (Poisson probably does not make any sense here)
#We will also try using run_number - this should appear very far down the tree
#if at all (since its effects on baseline will be absorbed into our diffing).

diff_df = NULL
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
                    if(i == 1) {
                        symptomatic_infections_1 = symptomatic_infections
                        worker_shifts_unavailable_1 = worker_shifts_unavailable
                        total_cost_1 = total_cost
                    } else {
                        df__ = data.frame(housing = housing, setting = setting, vaccinated = vaccinated, recovered = recovered, symptomatic_infections = symptomatic_infections - symptomatic_infections_1, boosting = boosting, temperature_screening = temperature_screening, vax = vax, virus_test = virus_test, r0_reduction = r0_reduction, worker_shifts_unavailable = worker_shifts_unavailable - worker_shifts_unavailable_1, total_cost = total_cost - total_cost_1, run_number = 1:1000)
                        if(is.null(diff_df)) {
                            diff_df = df__
                        } else {
                            diff_df = rbind(diff_df, df__)
                        }
                    }
                }
            }
        }
    }
}

diff_df$boosting = factor(diff_df$boosting)
diff_df$temperature_screening = factor(diff_df$temperature_screening)
diff_df$vax = factor(diff_df$vax)
diff_df$virus_test = factor(diff_df$virus_test)
diff_df$r0_reduction = factor(diff_df$r0_reduction)
diff_df$recovered = factor(diff_df$recovered)
diff_df$vaccinated = factor(diff_df$vaccinated)
diff_df$run_number = factor(diff_df$run_number)


png('2023-03-07/unavailable-differences.png', height = 900, width = 1600)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = diff_df[diff_df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()


png('2023-03-07/unavailable-differences-with-run-numbers.png', height = 900, width = 1600)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction + run ~ number, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1))
plot(tree)
text(tree, pretty = 1, cex = 2)
dev.off()

#per discussion with renata, let's try a maximal tree, followed by handing
#pruning.
#say, symptomatic, at a minimum difference of 10 (anywhere), 5 if smaller is <
#20, 1 if smaller is < 5

png('2023-03-07/symptomatic-maximal.png', height = 4500, width = 8000)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp=0))
plot(tree)
text(tree, pretty = 1)
dev.off()

#Okay, so with a strict requirement of a difference of 10, we get the following:
to_prune = function(frame, i, difference = 10) {
    j = which(row.names(frame) == i)
    #cat(i, ':', j)
    if(frame[j,1] == '<leaf>') {
        i
    } else {
        left = to_prune(frame, 2 * i, difference)
        right = to_prune(frame, 2 * i + 1, difference)
        if(length(left) * length(right) == 1) {
            if(abs(frame[which(row.names(frame) == 2 * i), 5] - frame[which(row.names(frame) == 2 * i + 1), 5]) >= difference) {
                c(left, right)
            } else {
                i
            }
        } else {
            c(left, right)
        }
    }
}

png('2023-03-07/symptomatic-hand-pruned.png', height = 900, width = 1600)
tree = rpart(symptomatic_infections ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp=0))
#snipped_tree = snip.rpart(tree, toss = c(8,9,20,42,43,22,46,47,12,13,28,29,60,61,31))
snipped_tree = snip.rpart(tree, toss = to_prune(tree$frame, 1))
plot(snipped_tree)
text(snipped_tree, pretty = 1, cex = 1.5)
dev.off()

#Looks pretty good. 10 is probably too low for unavailable, but hey, let's tree
#it.

png('2023-03-07/unavailable-hand-pruned-10.png', height = 900, width = 1600)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp=0))
#snipped_tree = snip.rpart(tree, toss = c(8,9,20,42,43,22,46,47,12,13,28,29,60,61,31))
snipped_tree = snip.rpart(tree, toss = to_prune(tree$frame, 1))
plot(snipped_tree)#, uniform = TRUE)
text(snipped_tree, pretty = 1, cex = 0.8)
dev.off()

#maybe try 50?
png('2023-03-07/unavailable-hand-pruned-50.png', height = 900, width = 1600)
tree = rpart(worker_shifts_unavailable ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp=0))
#snipped_tree = snip.rpart(tree, toss = c(8,9,20,42,43,22,46,47,12,13,28,29,60,61,31))
snipped_tree = snip.rpart(tree, toss = to_prune(tree$frame, 1, difference = 50))
plot(snipped_tree)
text(snipped_tree, pretty = 1, cex = 1.5)
dev.off()

#Similar heuristics might suggest 10,000, but let's try 1,000 first

png('2023-03-07/total-cost-hand-pruned-1k.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp=0))
#snipped_tree = snip.rpart(tree, toss = c(8,9,20,42,43,22,46,47,12,13,28,29,60,61,31))
snipped_tree = snip.rpart(tree, toss = to_prune(tree$frame, 1, difference = 1000))
plot(snipped_tree)
text(snipped_tree, pretty = 1, cex = 1)
dev.off()

png('2023-03-07/total-cost-hand-pruned-10k.png', height = 900, width = 1600)
tree = rpart(total_cost ~ setting + housing + vaccinated + recovered + boosting + temperature_screening + vax + virus_test + r0_reduction, data = df[df[,'setting'] == 'facility',], control=rpart.control(minsplit=1, minbucket=1, cp=0))
#snipped_tree = snip.rpart(tree, toss = c(8,9,20,42,43,22,46,47,12,13,28,29,60,61,31))
snipped_tree = snip.rpart(tree, toss = to_prune(tree$frame, 1, difference = 10000))
plot(snipped_tree)
text(snipped_tree, pretty = 1, cex = 1)
dev.off()

#stopping here for now; next task is to split total_cost into intervention
#expenses and production loss

#something else to do at some point: incorporate start day
#another: More cross-validation


"png('symptomatic-tree-balanced-infty.png', height = 900, width = 1600)
tree = rpart(symptomatic_infections ~ housing + vaccinated + recovered + virus_test + r0_reduction, data = df, control=rpart.control(minsplit=1, minbucket=1, cp=0)) #tried 0.001, 0.00001
plot(tree)
text(tree, pretty = 1, cex = 0.5)
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

