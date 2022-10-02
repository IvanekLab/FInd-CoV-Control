# analyze-v2.2.0.R is part of Food INdustry CoViD Control Tool
# (FInd CoV Control), version 2.2.0.
# Copyright (C) 2020-2022 Cornell University.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

output_per_shift = output_per_week / (5 * (1 + (supervisors > 1))) #N * 60.1 * 4 #wrong, but it's okay
#hourly_wage = 13.89
#size = 1000

library('vioplot')

######## analyze model predictions
analyze_fn = function() {  #this may, in the future, be revised to provide
                           #better encapsulation; for now, it simply serves
                           #to provide more meaningful debugging data

ANALYZE = TRUE
source('double-wrapped-v2.2.0.R', local = TRUE)
list_ = double_wrapped_fn()
row.names = list_[[1]]
colors = list_[[2]]
ltys = list_[[3]]
full_output_filenames = list_[[4]]

#again, this sort of information should be stored in saved files once we have
#more complex weeks, but for now, it's fine

if(farm_or_facility == 'farm') {
    production_shift_size = N
    cleaning_shift_size = 0
    workday = c('work', 'home', 'home')
    day_off = c('home', 'home', 'home')
    week = c(rep(workday, 5), rep(day_off, 2))
    schedule = rep(week, ceiling(days/7))[1:(3 * days)]
    production_shifts = work_shifts = (schedule == 'work')

} else {

    psX_only_size = 1 + workers_per_crew * crews_per_supervisor + n_shift_floaters
    if(supervisors > 1) {
        on_ps_1 = c(1/3, rep(1, psX_only_size), rep(0, psX_only_size),
                    rep(0, n_cleaners), rep(1/3, n_all_floaters))
        on_ps_2 = c(1/3, rep(0, psX_only_size), rep(1, psX_only_size),
                    rep(0, n_cleaners), rep(1/3, n_all_floaters))
        on_cs = c(1/3, rep(0, 2 * psX_only_size), rep(1, n_cleaners),
                rep(1/3, n_all_floaters))
        workday = c('work', 'work', 'work')
    } else {
        on_ps_1 = c(1/2, rep(1, psX_only_size), rep(0, n_cleaners),
                    rep(1/2, n_all_floaters))
        on_ps_2 = rep(0, 1 + psX_only_size + n_cleaners + n_all_floaters)
        on_cs = c(1/2, rep(0, psX_only_size), rep(1, n_cleaners),
                rep(1/2, n_all_floaters))
        workday = c('work', 'home', 'work')
    } 
    day_off = c('home', 'home', 'sleep')
    week = c(rep(workday, 5), rep(day_off, 2))
    schedule = rep(week, ceiling(days/7))[1:(3 * days)]
    work_shifts = (schedule == 'work')
    
    production_shift_size = sum(on_ps_1)
    cleaning_shift_size =  sum(on_cs)
}
    
#summary plots
combine = function(data, outcome_fn, summary_fn) { 
    dimnames(data) = list(rep(NA, dim(data)[1]),
                          colnames(data),
                          rep(NA, dim(data)[3])
    )
    #above is a bit kludgey, but it works -- at some point in the future,
    #we may explicitly save data with dimnames
    outcomes = outcome_fn(data)
    summarized = apply(outcomes, 1, summary_fn)
    summarized
}

#outcome_fn's
infected = function(data) {
    data[,'IA',] + data[,'IP',] + data[,'IM',] + data[,'IS',] + data[,'IC',]
}

symptomatic = function(data) {
    data[,'IM',] + data[,'IS',] + data[,'IC',]
}


#The following several functions may be combined at some point in the future.
#main_title is unused in production code, but is kept for consistency with
#special-purpose internal versions.
oneplot = function(
                   filename,
                   outcome_fn,
                   primary_summary_fn,
                   ylim,
                   ylab,
                   main_title = NULL,
                   mask = NA
                   ) {
    png(paste(subdirectory, unique_id, '_', filename, '_', VERSION, '.png',
              sep = ''),
        height = 1000, width = 1000)

    if(!is.na(mask)[1]) {
        step_index = step_index[mask]
    }

    #bit of a kludge, but should ensure sane limits
    ys = list()
    for (i in 1:length(full_output_filenames)) {
        full_output = readRDS(full_output_filenames[i])

        if(!is.na(mask)[1]) {
            full_output = full_output[mask,,]
        }
        ys[[i]] = combine(full_output, outcome_fn, primary_summary_fn)
    }
    #print(max(unlist(ys)))
    for(i in 1:length(full_output_filenames)) {
        if(i == 1) {
            par(mar = c(5,5,4,2))
            plot(step_index, ys[[i]], type = 'l', col = colors[i],
                 ylim = c(min(ylim[1], min(sapply(ys, function(x) min(x)))),
                          max(ylim[2], max(sapply(ys, function(x) max(x))))
                        ),
                 lwd = 4,
                 xlab = "Day", ylab = ylab, cex.axis = 1.5, cex.lab = 1.5,
                 lty = ltys[i])
            title(main=main_title, cex.main = 3)
            existing_ticks = axTicks(1)

            #If the maximum x-value being plotted is far enough from the maximum
            #automatically-generated tick label, add a tick and corresponding
            #label at the maximum x-value. The value of .036 was found
            #experimentally, and may need to be adjusted for a different
            #resolution, different cex.axis, different taste, etc. Or just drop
            #this part if you'd rather just allow the data to extend beyond the
            #final tick (as it does on the y-axis).
            if((days - existing_ticks[length(existing_ticks)]) / days > .036) { 
                axis(1, at=days, cex.axis = 1.5)
            }
        } else {
            points(step_index, ys[[i]], col = colors[i], lwd = 4, type = 'l', lty = ltys[i])
        }
    }
    legend("topright",inset = .06, row.names, lwd = 4,
           col = colors, lty = ltys, y.intersp = 1, cex = 1.5)
    dev.off()
    return(max(sapply(ys,max))) #to get maxes for the plots with forced same
                                #axes (in special purpose internal versions;
                                #kept here for consistency)
}

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

new_infections = function(data) {
    data[,'new_infections',]
}

new_symptomatic_infections = function(data) {
    data[,'new_symptomatic_infections',]
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
}

end_boxplot = function(
                       filename,
                       outcome_fn,
                       xlab,
                       average = FALSE,
                       xlim = NULL,
                       percent = FALSE,
                       main_title = NULL,
                       mask = NA,
                       function_ = boxplot
                       ) {
    png(paste(subdirectory, unique_id, '_', filename, '_', VERSION, '.png', sep = ''), height = 1000, width = 1000)

    if(!is.na(mask)[1]) {
        step_index = step_index[mask]
    }

    means = numeric(length(full_output_filenames))
    for (i in 1:length(full_output_filenames)) {
        full_output = readRDS(full_output_filenames[i])
        
        if(!is.na(mask)[1]) {
            full_output = full_output[mask,,]
        }

        dimnames(full_output) = list(rep(NA, dim(full_output)[1]), colnames(full_output), rep(NA, dim(full_output)[3])) #kludge
        outcomes = outcome_fn(full_output)
        outcomes = apply(outcomes, 2, cumsum)
        #cat('max of outcomes is:', max(outcomes), '\n')

        final = as.vector(outcomes[dim(full_output)[1],])
        if(average) {
            final = final / length(step_index)
        }
        #cat('max of final is:', max(final), '\n')
        
        means[i] = mean(final, na.rm = TRUE)
        #cat('max of means is:', max(means), '\n')

        if(i == 1) {
            all_outcomes = data.frame(intervention = row.names[i], outcome = final)
        } else {
            all_outcomes = rbind(all_outcomes, data.frame(intervention = row.names[i], outcome = final))
        }
    }

    all_outcomes$intervention = factor(all_outcomes$intervention, levels = unique(all_outcomes$intervention), ordered = TRUE)

    par(mar = c(5,23,4,2))
    if(percent) {
        par(xaxt="n")
    }
    #cat('max value:', max(all_outcomes$outcome),'\n')
    #cat('sum:', sum(all_outcomes$outcome), '\n')
    #cat('mean:', mean(all_outcomes$outcome), '\n')
    if(identical(function_, vioplot)) {
        par(cex.lab = 1.5)
    }
    function_(outcome ~ intervention, data = all_outcomes, horizontal = TRUE, las = 1, xlab = xlab, ylim = xlim, col = c('white', colors[-1]), cex.axis = 1.5, cex.names=1.5, cex.lab=1.5, ylab = '', na.action = na.pass)
    title(main=main_title, cex.main = 3)
    if(percent) {
        par(xaxt='s')
        axis(1, at=pretty(c(all_outcomes$outcome,xlim)), paste0(lab=pretty(c(all_outcomes$outcome,xlim)) * 100, ' %'), las=TRUE, cex.axis = 1.5, cex.lab=1.5)
    }
    points(means, 1:length(full_output_filenames), cex =2, pch = 8)
    dev.off()
}


scatter_plot = function(filename,
                       outcome_fn_x,
                       xlab,
                       mask_x = NA,
                       outcome_fn_y,
                       ylab,
                       mask_y = NA,
                       main_title = NULL
                       ) {
    png(paste(subdirectory, unique_id, '_', filename, '_', VERSION, '.png', sep = ''), height = 1000, width = 1300)

    step_index_x = step_index_y = step_index

    if(!is.na(mask_x)[1]) {
        step_index_x = step_index[mask_x]
    }
    if(!is.na(mask_y)[1]) {
        step_index_y = step_index[mask_y]
    }

    ###
        
    means_x = numeric(length(full_output_filenames))        
    means_y = numeric(length(full_output_filenames))
    for (i in 1:length(full_output_filenames)) {
        full_output_x = full_output_y = readRDS(full_output_filenames[i])
        #print('one')
        #print(dim(full_output_x))
        if(!is.na(mask_x)[1]) {
            full_output_x = full_output_x[mask_x,,]
        }
        #print('two')
        #print(dim(full_output_x))
        if(!is.na(mask_y)[1]) {
            full_output_y = full_output_y[mask_y,,]
        }
        #print('three')
        #print(dim(full_output_y))

        dimnames(full_output_x) = list(rep(NA, dim(full_output_x)[1]), colnames(full_output_x), rep(NA, dim(full_output_x)[3])) #kludge
        dimnames(full_output_y) = list(rep(NA, dim(full_output_y)[1]), colnames(full_output_y), rep(NA, dim(full_output_y)[3])) #kludge
        outcomes_x = outcome_fn_x(full_output_x)
        outcomes_x = apply(outcomes_x, 2, cumsum)
        outcomes_y = outcome_fn_y(full_output_y)
        outcomes_y = apply(outcomes_y, 2, cumsum)
        #cat('max of outcomes is:', max(outcomes), '\n')

        final_x = as.vector(outcomes_x[dim(full_output_x)[1],])
        final_y = as.vector(outcomes_y[dim(full_output_y)[1],])
        #if(average) {
        #    final = final / length(step_index)
        #}
        #cat('max of final is:', max(final), '\n')
        
        means_x[i] = mean(final_x, na.rm = TRUE)
        means_y[i] = mean(final_y, na.rm = TRUE)
        #cat('max of means is:', max(means), '\n')

        #if(i == 1) {
        #    all_outcomes = data.frame(intervention = row.names[i], outcome = final)
        #} else {
        #    all_outcomes = rbind(all_outcomes, data.frame(intervention = row.names[i], outcome = final))
        #}
    }

    #all_outcomes$intervention = factor(all_outcomes$intervention, levels = unique(all_outcomes$intervention), ordered = TRUE)

    #par(mar = c(5,23,4,2))
    #if(percent) {
    #    par(xaxt="n")
    #}
    #cat('max value:', max(all_outcomes$outcome),'\n')
    #cat('sum:', sum(all_outcomes$outcome), '\n')
    #cat('mean:', mean(all_outcomes$outcome), '\n')
    par(mar = c(5,5,4,32), xpd=TRUE)
    plot(means_x, means_y, xlab = xlab, ylab = ylab, col = colors, cex.axis = 2, #cex.names=1.5,
         cex.lab=2, pch = ltys, lwd = 12)
    #boxplot(outcome ~ intervention, data = all_outcomes, horizontal = TRUE, las = 1, xlab = xlab, ylim = xlim, col = c('white', colors[-1]), cex.axis = 1.5, cex.names=1.5, cex.lab=1.5, ylab = '', na.action = na.pass)
    title(main=main_title, cex.main = 3)
    #if(percent) {
    #    par(xaxt='s')
    #    axis(1, at=pretty(all_outcomes$outcome), paste0(lab=pretty(all_outcomes$outcome) * 100, ' %'), las=TRUE, cex.axis = 1.5, cex.lab=1.5)
    #}
    #points(means, 1:length(full_output_filenames), cex =2, pch = 8)
    legend("topright", row.names, lty = 0, lwd = 12,
           col = colors, pch = ltys, y.intersp = 1, cex = 2, inset = c(-0.585,0))
    dev.off()

}



first_x_boxplot = function(
                           filename,
                           outcome_fn,
                           xlab,
                           xlim = NULL,
                           mask = NA,
                           function_ = boxplot
                           ) {
    if(!is.null(filename)) {
        png(paste(subdirectory, unique_id, '_', filename, '_', VERSION, '.png', sep = ''), height = 1000, width = 1000)
    }

    if(!is.na(mask)[1]) {
        step_index = step_index[mask]
    }

    means = numeric(length(full_output_filenames))
    for (i in 1:length(full_output_filenames)) {
        full_output = readRDS(full_output_filenames[i])

        if(!is.na(mask)[1]) {
            full_output = full_output[mask,,]
        }

        dimnames(full_output) = list(rep(NA, dim(full_output)[1]), colnames(full_output), rep(NA, dim(full_output)[3])) #kludge
        outcomes = outcome_fn(full_output)
        
        first = apply(outcomes, 2, function(v) ifelse(length(which(v)) > 0, step_index[which(v)[1]], NA))
        
        means[i] = mean(first, na.rm = TRUE)

        if(i == 1) {
            all_outcomes = data.frame(intervention = row.names[i], outcome = first)
        } else {
            all_outcomes = rbind(all_outcomes, data.frame(intervention = row.names[i], outcome = first))
        }
    }

    all_outcomes$intervention = factor(all_outcomes$intervention, levels = unique(all_outcomes$intervention), ordered = TRUE)

    par(mar = c(5,23,4,2))
    function_(outcome ~ intervention, data = all_outcomes, horizontal = TRUE, las = 1, xlab = xlab, ylim = xlim, col = c('white', colors[-1]), cex.axis = 1.5, cex.names=1.5, cex.lab=1.5, ylab = '', na.action = na.pass)
    debug_all_outcomes <<- all_outcomes
    debug_means <<- means
    points(means, 1:length(full_output_filenames), cex =2, pch = 8)
    if(!is.null(filename)) {
        dev.off()
    }
}


end_barplot = function(
                       filename, 
                       outcome_fn, 
                       xlab, 
                       average = FALSE, 
                       xlim = NULL, 
                       percent = FALSE, 
                       mask = NA
                       ) {
    if(!is.null(filename)) {
        png(paste(subdirectory, unique_id, '_', filename, '_', VERSION, '.png', sep = ''), height = 1000, width = 1000)
    }
    
    if(!is.na(mask)[1]) {
        step_index = step_index[mask]
    }

    all_outcomes = numeric(length(full_output_filenames))
    names(all_outcomes) = row.names
    for (i in 1:length(full_output_filenames)) {
        full_output = readRDS(full_output_filenames[i])

        if(!is.na(mask)[1]) {
            full_output = full_output[mask,,]
        }

        dimnames(full_output) = list(rep(NA, dim(full_output)[1]), colnames(full_output), rep(NA, dim(full_output)[3])) #kludge
        outcomes = outcome_fn(full_output)

        if(average) {
            fraction = mean(apply(outcomes, 2, max)) #kludge, should ideally be joined to another thing (but works adequately for now)
        } else {
            fraction = sum(apply(outcomes, 2, max)) #kludge, should be joined to another thing, extra kludgey with this name (but works adequately for now)
        }
        
        all_outcomes[i] = fraction
    }
    par(mar = c(5,23,4,2))
    if(percent) {
        par(xaxt="n")
    }
    barplot(all_outcomes, horiz = TRUE, las = 1, xlab = xlab, xlim = xlim, col = colors, cex.axis = 1.5, cex.names=1.5, cex.lab=1.5)
    if(percent) {
        par(xaxt='s')
        axis(1, at=pretty(c(all_outcomes,xlim)), paste0(lab=pretty(c(all_outcomes,xlim)) * 100, ' %'), las=TRUE, cex.axis = 1.5, cex.lab=1.5)
    }
    if(!is.null(filename)) {
        dev.off()
    }
}


oneplot('Infected', infected, mean, c(0,0), paste('People Infectious (out of ', N, ' total)', sep = ''))
oneplot('Symptomatic', symptomatic, mean, c(0,0), paste('People Symptomatically Infected (out of ', N, ' total)', sep = ''))


l = length(work_shifts)
production_shifts = work_shifts & ((1:l) %% 3 != 0)
cleaning_shifts =  work_shifts & ((1:l) %% 3 == 0)

oneplot('Unavailable-production', shiftwise_unavailable, mean, c(0,0), paste('People Unavailable to Work their Scheduled Production Shift (out of ', round(production_shift_size,2), ' total)', sep = ''), mask = production_shifts)

main_title = ''

end_boxplot('Average-Unavailable-production', shiftwise_unavailable, xlab = paste('Average Absences per Production Shift (out of ', round(production_shift_size,2), ' workers)'), average = TRUE, main_title = main_title, mask = production_shifts)
end_boxplot('Average-Unavailable-production-violin', shiftwise_unavailable, xlab = paste('Average Absences per Production Shift (out of ', round(production_shift_size,2), ' workers)'), average = TRUE, main_title = main_title, mask = production_shifts, function_ = vioplot)

end_boxplot('Total-Infections', new_infections, xlab = paste('Total Infections (among ', N, 'total workers)'), average = FALSE, main_title = main_title)
end_boxplot('Total-Infections-violin', new_infections, xlab = paste('Total Infections (among ', N, 'total workers)'), average = FALSE, main_title = main_title, function_ = vioplot)

end_boxplot('Total-Symptomatic-Infections', new_symptomatic_infections, xlab = paste('Total Symptomatic Infections (among', N, 'total workers)'), average = FALSE, main_title = main_title)
end_boxplot('Total-Symptomatic-Infections-violin', new_symptomatic_infections, xlab = paste('Total Symptomatic Infections (among', N, 'total workers)'), average = FALSE, main_title = main_title, function_ = vioplot)

end_boxplot('Fraction-Short-production', shiftwise_short, xlab = 'Percentage of Production Shifts Short (> 15% of workers absent)', average = TRUE, xlim = c(0,1), percent = TRUE, main_title = main_title, mask = production_shifts)
end_boxplot('Fraction-Short-production-violin', shiftwise_short, xlab = 'Percentage of Production Shifts Short (> 15% of workers absent)', average = TRUE, xlim = c(0,1), percent = TRUE, main_title = main_title, mask = production_shifts, function_ = vioplot)

end_barplot('Ever-Short-production', shiftwise_short, xlab = 'Production Shift(s) Ever Short (percentage of runs)', average = TRUE, xlim = c(0,1), percent = TRUE, mask = production_shifts)

first_x_boxplot('First-Day-Short-production', shiftwise_short, xlab = 'First Day Short (among runs that are ever short)', xlim = c(1, days), mask = production_shifts)
#first_x_boxplot('First-Day-Short-production-violin', shiftwise_short, xlab = 'First Day Short (among runs that are ever short)', xlim = c(1, days), mask = production_shifts, function_ = vioplot)

#print(output_per_shift)

oneplot('Production-Loss', shiftwise_production_loss, mean, c(0,0), 'Production Loss (Dollars ($) per production shift)', mask = production_shifts)
end_boxplot('Total-Production-Loss', shiftwise_production_loss, xlab = 'Total Production Loss in Dollars ($)', mask = production_shifts)
end_boxplot('Total-Production-Loss-vioplot', shiftwise_production_loss, xlab = 'Total Production Loss in Dollars ($)', mask = production_shifts, function_ = vioplot)
end_boxplot('Total-Intervention-Expenses', generate_intervention_expenses_function(), xlab = 'Total intervention expenses in Dollars ($)"')
end_boxplot('Total-Intervention-Expenses-vioplot', generate_intervention_expenses_function(), xlab = 'Total intervention expenses in Dollars ($)"', function_ = vioplot)

intervention_expenses_function = generate_intervention_expenses_function()
#below is massively kludged, to deal with production loss fn not handling
#cleaning shifts, and cost needing to handle all shifts
#follow by the need to have two dimensions for handling in end_boxplot
g = function(data) {
    fd = shiftwise_production_loss(data[production_shifts,,])
    #fd = ifelse(is.na(fd), 0, fd)
    #cat('blorp\n')
    r = intervention_expenses_function(data)
    r[production_shifts] = r[production_shifts] + fd
    #r = rbind(apply(fd,2,sum) + apply(f(data),2,sum))
    #print(dim(r))
    r
}
end_boxplot('Total-Cost', g, xlab = 'Total Cost (Intervention Expenses + Production Losses) in Dollars ($)')
intervention_expenses_function = generate_intervention_expenses_function()
end_boxplot('Total-Cost-violin', g, xlab = 'Total Cost (Intervention Expenses + Production Losses) in Dollars ($)', function_ = vioplot)
#print('before')
#scatter_plot(filename = 'Scatterplot--Production-Losses',
#                       outcome_fn_x = shiftwise_production_loss,
#                       xlab = "Total Production Losses in Dollars ($)",
#                       mask_x = production_shifts,
#                       outcome_fn_y = function(data) data[,'new_infections',],
#                       ylab = 'Total Infections',
#                       mask_y = NA,
#                       main_title = NULL
#                       ) 
#f = shameful_kludge()
#g = function(data) {
    #print('barrier')
    #print(dim(data))
#    fd = shiftwise_production_loss(data[production_shifts,,])
    #print(length(fd))
    #print(length(production_shifts))
    #fd = ifelse(is.na(fd), 0, fd)
    #cat('blorp\n')
#    r = f(data)
    #print(length(r))
#    r[production_shifts] = r[production_shifts] + fd
    #r = rbind(apply(fd,2,sum) + apply(f(data),2,sum))
    #print(dim(r))
#    r
#}
#print('mid')
#scatter_plot(filename = 'Scatterplot--Total-Cost',
#                       outcome_fn_x = g,
#                       xlab = "Total Cost (Intervention Expenses + Production Losses) in Dollars ($)",
#                       mask_x = NA, #production_shifts,
#                       outcome_fn_y = function(data) data[,'new_infections',],
#                       ylab = 'Total Infections',
#                       mask_y = NA,
#                       main_title = NULL
#                       ) 
#print('post')

if(farm_or_facility == 'facility') {
    oneplot('Unavailable-cleaning', shiftwise_unavailable, mean, c(0,0), paste('People Unavailable to Work their Scheduled Cleaning Shift (out of ', round(cleaning_shift_size,2), ' total)', sep = ''), mask = cleaning_shifts)
    end_boxplot('Average-Unavailable-cleaning', shiftwise_unavailable, xlab = paste('Average Absences per Cleaning Shift (out of ', round(cleaning_shift_size,2), ' workers)'), average = TRUE, main_title = main_title, mask = cleaning_shifts)
    end_boxplot('Average-Unavailable-cleaning-violin', shiftwise_unavailable, xlab = paste('Average Absences per Cleaning Shift (out of ', round(cleaning_shift_size,2), ' workers)'), average = TRUE, main_title = main_title, mask = cleaning_shifts, function_ = vioplot)
    end_boxplot('Fraction-Short-cleaning', shiftwise_short, xlab = 'Percentage of Cleaning Shifts Short (> 15% of workers absent)', average = TRUE, xlim = c(0,1), percent = TRUE, main_title = main_title, mask = cleaning_shifts)
    end_boxplot('Fraction-Short-cleaning-violin', shiftwise_short, xlab = 'Percentage of Cleaning Shifts Short (> 15% of workers absent)', average = TRUE, xlim = c(0,1), percent = TRUE, main_title = main_title, mask = cleaning_shifts, function_ = vioplot)
    end_barplot('Ever-Short-cleaning', shiftwise_short, xlab = 'Cleaning Shift Ever Short (percentage of runs)', average = TRUE, xlim = c(0,1), percent = TRUE, mask = cleaning_shifts)
    first_x_boxplot('First-Day-Short-cleaning', shiftwise_short, xlab = 'First Day Short (among runs that are ever short)', xlim = c(1, days), mask = cleaning_shifts)
#    first_x_boxplot('First-Day-Short-cleaning-violin', shiftwise_short, xlab = 'First Day Short (among runs that are ever short)', xlim = c(1, days), mask = cleaning_shifts, function_ = vioplot)
}

#sample_data = function() {
#    interventions = length(full_output_filenames)
#    for (i in 1:interventions) {
#        full_output = readRDS(full_output_filenames[i])
#
#        if(i == 1) {
#            steps = dim(full_output)[1]
#            reps = dim(full_output)[3]
#            unavailable_array = array(0, c(interventions, steps, reps))
#            scheduled_array = array(0, c(interventions, steps, reps))
#            total_infections_array = array(0, c(interventions, reps))
#            doses_array = array(0, c(interventions, steps, reps))
#            tests_array = array(0, c(interventions, steps, reps))
#        }
#        unavailable_array[i,,] = shiftwise_unavailable(full_output)
#        scheduled_array[i,,] = shiftwise_scheduled(full_output)
#        total_infections_array[i,] = apply(full_output[,'new_infections',], 2, sum)
#        doses_array[i,,] = full_output[,'doses',]
#        tests_array[i,,] = full_output[,'tests',]
#    }
#
#    l = list(N = N,
#             days = days,
#             unavailable = unavailable_array,
#             scheduled = scheduled_array,
#             total_infections = total_infections_array,
#             doses = doses_array,
#             tests = tests_array,
#             intervention_names = row.names,
#             work_shifts = work_shifts)
#    saveRDS(l, paste(subdirectory, unique_id, '_econ_data_', VERSION, '.rds', sep = '') )
#}

#sample_data() #restore to generate more data for daisy's econ model

#if(farm_or_facility == 'farm') {
#    source('farm_updated_1.R', local = TRUE)
#    f_farm(subdirectory, unique_id, VERSION, output_per_week = N * 60.1 * 40, hourly_wage = 13.89)
#} else { #i.e., facility
#    source('processing.R', local = TRUE)
#}

ANALYZE = FALSE

} #analyze_fn
