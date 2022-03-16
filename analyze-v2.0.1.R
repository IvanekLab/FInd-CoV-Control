# analyze-v2.0.1.R is part of Food INdustry CoViD Control Tool
# (FInd CoV Control), version 2.0.1.
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
######## analyze model predictions
analyze_fn = function() {  #this may, in the future, be revised to provide
                           #better encapsulation; for now, it simply serves
                           #to provide more meaningful debugging data

ANALYZE = TRUE
source('double-wrapped-v2.0.1.R', local = TRUE)
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

new_infections = function(data) {
    data[,'new_infections',]
}

end_boxplot = function(
                       filename,
                       outcome_fn,
                       xlab,
                       average = FALSE,
                       xlim = NULL,
                       percent = FALSE,
                       main_title = NULL,
                       mask = NA
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

        final = as.vector(outcomes[dim(full_output)[1],])
        if(average) {
            final = final / length(step_index)
        }
        
        means[i] = mean(final, na.rm = TRUE)

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
    boxplot(outcome ~ intervention, data = all_outcomes, horizontal = TRUE, las = 1, xlab = xlab, ylim = xlim, col = c('white', colors[-1]), cex.axis = 1.5, cex.names=1.5, cex.lab=1.5, ylab = '', na.action = na.pass)
    title(main=main_title, cex.main = 3)
    if(percent) {
        par(xaxt='s')
        axis(1, at=pretty(all_outcomes$outcome), paste0(lab=pretty(all_outcomes$outcome) * 100, ' %'), las=TRUE, cex.axis = 1.5, cex.lab=1.5)
    }
    points(means, 1:length(full_output_filenames), cex =2, pch = 8)
    dev.off()
}

first_x_boxplot = function(
                           filename,
                           outcome_fn,
                           xlab,
                           xlim = NULL,
                           mask = NA
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
    boxplot(outcome ~ intervention, data = all_outcomes, horizontal = TRUE, las = 1, xlab = xlab, ylim = xlim, col = c('white', colors[-1]), cex.axis = 1.5, cex.names=1.5, cex.lab=1.5, ylab = '', na.action = na.pass)
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

l = length(work_shifts)
production_shifts = work_shifts & ((1:l) %% 3 != 0)
cleaning_shifts =  work_shifts & ((1:l) %% 3 == 0)

oneplot('Unavailable-production', shiftwise_unavailable, mean, c(0,0), paste('People Unavailable to Work their Scheduled Production Shift (out of ', production_shift_size, ' total)', sep = ''), mask = production_shifts)

main_title = ''

end_boxplot('Average-Unavailable-production', shiftwise_unavailable, xlab = paste('Average Absences per Production Shift (out of ', production_shift_size, ' workers)'), average = TRUE, main_title = main_title, mask = production_shifts)

end_boxplot('Total-Infections', new_infections, xlab = paste('Total Infections (among ', N, 'total workers)'), average = FALSE, main_title = main_title)

end_boxplot('Fraction-Short-production', shiftwise_short, xlab = 'Percentage of Production Shifts Short (> 15% of workers absent)', average = TRUE, xlim = c(0,1), percent = TRUE, main_title = main_title, mask = production_shifts)

end_barplot('Ever-Short-production', shiftwise_short, xlab = 'Production Shift(s) Ever Short (percentage of runs)', average = TRUE, xlim = c(0,1), percent = TRUE, mask = production_shifts)

first_x_boxplot('First-Day-Short-production', shiftwise_short, xlab = 'First Day Short (among runs that are ever short)', xlim = c(1, days), mask = production_shifts)

if(farm_or_facility == 'facility') {
    oneplot('Unavailable-cleaning', shiftwise_unavailable, mean, c(0,0), paste('People Unavailable to Work their Scheduled Cleaning Shift (out of ', cleaning_shift_size, ' total)', sep = ''), mask = cleaning_shifts)
    end_boxplot('Average-Unavailable-cleaning', shiftwise_unavailable, xlab = paste('Average Absences per Cleaning Shift (out of ', cleaning_shift_size, ' workers)'), average = TRUE, main_title = main_title, mask = cleaning_shifts)
    end_boxplot('Fraction-Short-cleaning', shiftwise_short, xlab = 'Percentage of Cleaning Shifts Short (> 15% of workers absent)', average = TRUE, xlim = c(0,1), percent = TRUE, main_title = main_title, mask = cleaning_shifts)
    end_barplot('Ever-Short-cleaning', shiftwise_short, xlab = 'Cleaning Shift Ever Short (percentage of runs)', average = TRUE, xlim = c(0,1), percent = TRUE, mask = cleaning_shifts)
    first_x_boxplot('First-Day-Short-cleaning', shiftwise_short, xlab = 'First Day Short (among runs that are ever short)', xlim = c(1, days), mask = cleaning_shifts)
}

sample_data = function() {
    interventions = length(full_output_filenames)
    for (i in 1:interventions) {
        full_output = readRDS(full_output_filenames[i])

        if(i == 1) {
            steps = dim(full_output)[1]
            reps = dim(full_output)[3]
            unavailable_array = array(0, c(interventions, steps, reps))
            scheduled_array = array(0, c(interventions, steps, reps))
            total_infections_array = array(0, c(interventions, reps))
            doses_array = array(0, c(interventions, steps, reps))
            tests_array = array(0, c(interventions, steps, reps))
        }
        unavailable_array[i,,] = shiftwise_unavailable(full_output)
        scheduled_array[i,,] = shiftwise_scheduled(full_output)
        total_infections_array[i,] = apply(full_output[,'new_infections',], 2, sum)
        doses_array[i,,] = full_output[,'doses',]
        tests_array[i,,] = full_output[,'tests',]
    }

    l = list(N = N,
             days = days,
             unavailable = unavailable_array,
             scheduled = scheduled_array,
             total_infections = total_infections_array,
             doses = doses_array,
             tests = tests_array,
             intervention_names = row.names,
             work_shifts = work_shifts)
    saveRDS(l, paste(subdirectory, unique_id, '_econ_data_', VERSION, '.rds', sep = '') )
}

sample_data() #restore to generate more data for daisy's econ model

ANALYZE = FALSE

} #analyze_fn
