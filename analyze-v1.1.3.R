# analyze-v1.1.3.R is part of Food INdustry CoViD Control Tool
# (FInd CoV Control), version 1.1.3.
# Copyright (C) 2020-2021 Cornell University.
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
source('double-wrapped-v1.1.3.R', local = TRUE)
list_ = double_wrapped_fn()
row.names = list_[[1]]
colors = list_[[2]]
ltys = list_[[3]]
full_output_filenames = list_[[4]]

#again, this sort of information should be stored in saved files once we have
#more complex weeks, but for now, it's fine

psX_only_size = 1 + workers_per_crew * crews_per_supervisor + n_shift_floaters
if(supervisors > 1) {
    on_ps_1 = c(1/3, rep(1, psX_only_size), rep(0, psX_only_size), rep(0, n_cleaners), rep(1/3, n_all_floaters))
    on_ps_2 = c(1/3, rep(0, psX_only_size), rep(1, psX_only_size), rep(0, n_cleaners), rep(1/3, n_all_floaters))
    on_cs = c(1/3, rep(0, 2 * psX_only_size), rep(1, n_cleaners), rep(1/3, n_all_floaters))
    workday = c('work', 'work', 'work')
} else {
    on_ps_1 = c(1/2, rep(1, psX_only_size), rep(0, n_cleaners), rep(1/2, n_all_floaters))
    on_ps_2 = rep(0, 1 + psX_only_size + n_cleaners + n_all_floaters)
    on_cs = c(1/2, rep(0, psX_only_size), rep(1, n_cleaners), rep(1/2, n_all_floaters))
    workday = c('work', 'home', 'work')
} 
day_off = c('home', 'home', 'sleep')
week = c(rep(workday, 5), rep(day_off, 2))
schedule = rep(week, ceiling(days/7))[1:(3 * days)]
work_shifts = (schedule == 'work')

production_shift_size = sum(on_ps_1)
cleaning_shift_size =  sum(on_cs)

####
#not the most efficient approach, but it will do
#note that this probably needs to be modified to work with segmented
####
#boxes = lapply(1:ceiling(days/7), function(i) c(5 + (7 * (i - 1)), -10, 7 * i, 60))
boxes = lapply(1:ceiling(days/7), function(i) c(5.5 + (7 * (i - 1)), -10, 7 * i + 0.5, 140))

#summary plots
combine = function(data, outcome_fn, summary_fn, summation_mode) { #, using_agentss = FALSE) {
    if(!(summation_mode %in% c(FALSE, 'after', 'before'))) {
        stop('Invalid summation mode')
    }
    #if(!using_agentss) {
        dimnames(data) = list(rep(NA, dim(data)[1]), colnames(data), rep(NA, dim(data)[3])) #a bit kludgey, but it works -- at some point in the future, we may explicitly save data with dimnames
    #}
    outcomes = outcome_fn(data)
    if(summation_mode == 'before') {
        outcomes = apply(outcomes, 2, cumsum) / (steps / days) #slightly awkward "phrasing", but may guard against future errors better than a simple "3"
    }
    summarized = apply(outcomes, 1, summary_fn)
    if(summation_mode == 'after') { #unlikely to be a good idea (and will not occur if the code is run unmodified)
        print('Are you sure this is a good idea?')
        summarized = cumsum(summarized) / (steps / days) #slightly awkward "phrasing", but may guard against future errors better than a simple "3"
    }
    summarized
}

#outcome_fn's
infected = function(data) {
    data[,'IA',] + data[,'IP',] + data[,'IM',] + data[,'IS',] + data[,'IC',]
}

#fuller_infected_one_agents = function(agents) {
#    s = sum(agents$infection_status %in% c('IA', 'IP', 'IM', 'IS', 'IC'))
#    s
#}

#fuller_infected_one_intervention = function(oi_data) {
#    s = sapply(oi_data, fuller_infected_one_agents)
#    s
#}


#fuller_infected = function(data) {
#    sapply(data, fuller_infected_one_intervention)
#}

hospitalized_dead = function(data) {
    data[,'IS',] + data[,'IC',] + data[,'D',]
}


unavailable = function(data) {
    (hospitalized_dead(data) + data[,'S_isolated',] + data[,'E_isolated',] +
                                data[,'IA_isolated',] + data[,'IP_isolated',] +
                                data[,'IM_isolated',] + data[,'R_isolated',] +
                                data[,'V1_isolated',] + data[,'V2_isolated',] +
                                data[,'V1E_isolated',] + data[,'V2E_isolated',] +
                                #data[,'W_isolated',] + data[,'WE_isolated',] +
                                data[,'RE_isolated',]
    )
}

short = function(data) {
    unavailable(data) > .15 * N
}


#The following several functions may be combined at some point in the future.
#main_title is unused in production code, but is kept for consistency with
#special-purpose internal versions.
####
#added na.rm option to allow alternate method of plotting work absences
#segmented allows carving up discrete shift segments
#daily_sum adds consecutive trios of shifts (only makes sense for some functions)
####
#oneplot = function(filename, outcome_fn, primary_summary_fn, ylim, ylab, summation_mode = FALSE, work_only = FALSE, main_title = NULL, use_agentss = FALSE) {
oneplot = function(filename, outcome_fn, primary_summary_fn, ylim, ylab, summation_mode = FALSE, work_only = FALSE, main_title = NULL, na.rm = FALSE, segmented = FALSE, daily_sum = FALSE, box_it = FALSE, mask = NA) {
    png(paste(subdirectory, unique_id, '_', filename, '_', VERSION, '.png', sep = ''), height = 1000, width = 1000)

    if(work_only) {
        step_index = step_index[work_shifts]
    }
    smear = (1:(length(step_index) / 3)) * 3

    if(!is.na(mask)[1]) {
        step_index = step_index[mask]
    }

    #TBD: NB: smear and mask not yet implemented

    if(daily_sum) {
        step_index = step_index[smear]
    }

    #bit of a kludge, but should ensure sane limits
    ys = list()
print('starting the loop')
    for (i in 1:length(full_output_filenames)) {
#        if(use_agentss) {
#            full_output = readRDS(paste0(full_output_filenames[i],'-fuller'))
#            if(work_only) {
#                stop('Combining work_only and use_agentss not yet implemented')
#            }
#        } else {
print(full_output_filenames)
        full_output = readRDS(full_output_filenames[i])
        if(work_only) {
            full_output = full_output[work_shifts,,]
            if(summation_mode != FALSE){
                full_output = full_output
            }
        }
        if(!is.na(mask)[1]) {
            full_output = full_output[mask,,]
        }
        if(daily_sum) {
                full_output = full_output[smear,,] + full_output[smear - 1,,] + full_output[smear - 2,,]
        }
        ys[[i]] = combine(full_output, outcome_fn, primary_summary_fn, summation_mode)#, using_agentss = use_agentss)
    }
    for(i in 1:length(full_output_filenames)) {
        if(i == 1) {
            par(mar = c(5,5,4,2))
            #print('Mins')
            #print(sapply(ys, min))
            #print('Maxs')
            #print(sapply(ys, max))
            ####
            #allowing for use of na.rm = TRUE
            ####
            if(segmented) {
                #cat('i = 1\n-----\nys:', ys[[i]],'\n')
                for(j in 1:length(step_index)) {
                    if(j == 1) {
                        #cat(c('1:', c(0, step_index[1]), ':', rep(ys[[i]][1], 2), '\n'))
                        plot(c(0, step_index[1]), rep(ys[[i]][1], 2), type = 'l', col = colors[i],
                             ylim = c(min(ylim[1], min(sapply(ys, function(x) min(x, na.rm = na.rm)))),
                                      max(ylim[2], max(sapply(ys, function(x) max(x, na.rm = na.rm))))),
                             xlim = c(0, max(step_index)), lwd = 4,
                             xlab = "Day", ylab = ylab, cex.axis = 1.5, cex.lab = 1.5,
                             lty = ltys[i])
                    } else {
                        #cat(c(j, ':', step_index[(j-1):j], ':', rep(ys[[i]][j], 2), '\n'))
                        #cat('points(c(', step_index[(j-1)], ',', step_index[j], '),c(', ys[[i]][j], ',', ys[[i]][j], '), col = "black", lwd = 4, type = "l", lty = 1)\n')
                        points(step_index[(j-1):j], rep(ys[[i]][j], 2), col = colors[i], lwd = 4, type = 'l', lty = ltys[i])
                    }
                }
                #dev.off()
                #stop('LOL')
            } else {
                plot(step_index, ys[[i]], type = 'l', col = colors[i],
                     ylim = c(min(ylim[1], min(sapply(ys, function(x) min(x, na.rm = na.rm)))),
                              max(ylim[2], max(sapply(ys, function(x) max(x, na.rm = na.rm))))), lwd = 4,
                     xlab = "Day", ylab = ylab, cex.axis = 1.5, cex.lab = 1.5,
                     lty = ltys[i])
            }
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
            if(segmented) {
                for(j in 1:length(step_index)) {
                    if(j == 1) {
                        points(c(0, step_index[1]), rep(ys[[i]][j], 2), col = colors[i], lwd = 4, type = 'l', lty = ltys[i])
                    } else {
                        points(step_index[(j-1):j], rep(ys[[i]][j], 2), col = colors[i], lwd = 4, type = 'l', lty = ltys[i])
                    }
                }
            } else {
                points(step_index, ys[[i]], col = colors[i], lwd = 4, type = 'l', lty = ltys[i])
            }
        }
    }
    if(box_it) {
        for(box_ in boxes) {
            rect(box_[1], box_[2], box_[3], box_[4], col = 'gray90', border = NA)
        }
    }
    legend("topright",inset = .06, row.names, lwd = 4,
           col = colors, lty = ltys, y.intersp = 1, cex = 1.5)
    dev.off()
    return(max(sapply(ys,max))) #to get maxes for the plots with forced same axes (in special purpose internal versions; kept here for consistency)
}

shiftwise_unavailable = function(data) {
    data[,'qn_absent',] 
}

shiftwise_unavailable_masked = function(data) {
    ifelse(data[,'qn_scheduled',] == 0, NaN, data[,'qn_absent',])
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

#shiftwise_special_unavailable = function(data) {
#    data[,'n_special_absent',]
#}

#shiftwise_special_unavailable_fraction = function(data) {
#    data[,'n_special_absent',] / data[,'n_special_scheduled',]
#}

#shiftwise_special_short = function(data) {
#    shiftwise_special_unavailable_fraction(data) > .15
#}

#shiftwise_any_short = function(data) {
#    shiftwise_short(data) | shiftwise_special_short(data)
#}

end_boxplot = function(filename, outcome_fn, xlab, summation_mode = 'before', work_only = FALSE, average = FALSE, xlim = NULL, percent = FALSE, main_title = NULL, mask = NA) {
    png(paste(subdirectory, unique_id, '_', filename, '_', VERSION, '.png', sep = ''), height = 1000, width = 1000)
    if(work_only) {
        step_index = step_index[work_shifts]
    }

    if(!is.na(mask)[1]) {
        step_index = step_index[mask]
    }

    means = numeric(length(full_output_filenames))
    for (i in 1:length(full_output_filenames)) {
        full_output = readRDS(full_output_filenames[i])
        if(work_only) {
            full_output = full_output[work_shifts,,]
        }
        if(!is.na(mask)[1]) {
            full_output = full_output[mask,,]
        }

        dimnames(full_output) = list(rep(NA, dim(full_output)[1]), colnames(full_output), rep(NA, dim(full_output)[3])) #kludge
        outcomes = outcome_fn(full_output)
        if(summation_mode == 'before') {
            outcomes = apply(outcomes, 2, cumsum)
        }
        final = as.vector(outcomes[dim(full_output)[1],])
        if(average) {
            final = final / length(step_index)
        }
        
        means[i] = mean(final, na.rm = TRUE)
        if(summation_mode == 'after') { #unlikely to be a good idea (and will not occur if the code is run unmodified)
            stop('Invalid for this function.')
        }
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

count_consecutive_without = function(v, so_far = 0) {
    if(length(v) == 0) {
        return(so_far)
    }
    if(v[1]) {
        return(max(so_far, count_consecutive_without(v[-1])))
    } else {
        return(count_consecutive_without(v[-1], so_far + 1))
    }
}

first_x_boxplot = function(filename, outcome_fn, xlab, summation_mode = FALSE, work_only = FALSE, default = days + 1, xlim = NULL, consecutive_without = FALSE) {
    if(!is.null(filename)) {
        png(paste(subdirectory, unique_id, '_', filename, '_', VERSION, '.png', sep = ''), height = 1000, width = 1000)
    }
    if(work_only) {
        step_index = step_index[work_shifts]
    }

    means = numeric(length(full_output_filenames))
    for (i in 1:length(full_output_filenames)) {
        full_output = readRDS(full_output_filenames[i])
        if(work_only) {
            full_output = full_output[work_shifts,,]
        }

        dimnames(full_output) = list(rep(NA, dim(full_output)[1]), colnames(full_output), rep(NA, dim(full_output)[3])) #kludge
        outcomes = outcome_fn(full_output)
        if(consecutive_without) {
            first = apply(outcomes, 2, count_consecutive_without)
        } else {
            first = apply(outcomes, 2, function(v) ifelse(length(which(v)) > 0, step_index[which(v)[1]], default))
        }
        means[i] = mean(first, na.rm = TRUE)
        if(summation_mode != FALSE) { #unlikely to be a good idea (and will not occur if the code is run unmodified)
            stop('Invalid for this function.')
        }
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

anti_max = function(v) {
    !(max(v))
}

end_barplot = function(filename, outcome_fn, xlab, summation_mode = FALSE, work_only = FALSE, default = days + 1, average = FALSE, xlim = NULL, percent = FALSE, anti_this = FALSE, mask = NA) {
    if(!is.null(filename)) {
        png(paste(subdirectory, unique_id, '_', filename, '_', VERSION, '.png', sep = ''), height = 1000, width = 1000)
    }
    if(work_only) {
        step_index = step_index[work_shifts]
    }
    
    if(!is.na(mask)[1]) {
        step_index = step_index[mask]
    }

    all_outcomes = numeric(length(full_output_filenames))
    names(all_outcomes) = row.names
    for (i in 1:length(full_output_filenames)) {
        full_output = readRDS(full_output_filenames[i])
        if(work_only) {
            full_output = full_output[work_shifts,,]
        }

        if(!is.na(mask)[1]) {
            full_output = full_output[mask,,]
        }

        dimnames(full_output) = list(rep(NA, dim(full_output)[1]), colnames(full_output), rep(NA, dim(full_output)[3])) #kludge
        outcomes = outcome_fn(full_output)
        if(anti_this) {
            thing_to_call = anti_max
        } else {
            thing_to_call = max
        }

        if(average) {
            fraction = mean(apply(outcomes, 2, thing_to_call)) #kludge, should ideally be joined to another thing (but works adequately for now)
        } else {
            fraction = sum(apply(outcomes, 2, thing_to_call)) #kludge, should be joined to another thing, extra kludgey with this name (but works adequately for now)
        }
        if(summation_mode != FALSE) { #unlikely to be a good idea (and will not occur if the code is run unmodified)
            stop('Invalid for this function.')
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

print('okay')

oneplot('Infected', infected, mean, c(0,0), paste('People Infectious (out of ', N, ' total)', sep = ''))#, main_title = 'Delta')

print('not')

l = length(work_shifts)
production_shifts = work_shifts & ((1:l) %% 3 != 0)
cleaning_shifts =  work_shifts & ((1:l) %% 3 == 0)

oneplot('Unavailable-production', shiftwise_unavailable, mean, c(0,0), paste('People Unavailable to Work their Scheduled Production Shift (out of ', production_shift_size, ' total)', sep = ''), mask = production_shifts)#, main_title = 'Delta')
oneplot('Unavailable-cleaning', shiftwise_unavailable, mean, c(0,0), paste('People Unavailable to Work their Scheduled Cleaning Shift (out of ', cleaning_shift_size, ' total)', sep = ''), mask = cleaning_shifts)#, main_title = 'Delta')

#oneplot('Unavailable_with_fixed_weekends', shiftwise_unavailable_masked, mean, c(0,0), 'Scheduled Workers Unavailable', work_only = TRUE, na.rm = TRUE, daily_sum = TRUE, box_it = TRUE)
#if(DELTA) {
#     main_title = 'Delta'
# } else {
#     main_title = '2020 Strains'
# }

#print('So far . . .')
#oneplot('fuller-Infected', fuller_infected, mean, c(0,0), paste('People Infectious (out of ', N, ' total)', sep = ''), use_agentss = TRUE)
#print('Foiled!')

main_title = ''

end_boxplot('Average-Unavailable-production', shiftwise_unavailable, xlab = paste('Average Absences per Production Shift (out of ', production_shift_size, ' workers)'), average = TRUE, main_title = main_title, mask = production_shifts)
end_boxplot('Average-Unavailable-cleaning', shiftwise_unavailable, xlab = paste('Average Absences per Cleaning Shift (out of ', cleaning_shift_size, ' workers)'), average = TRUE, main_title = main_title, mask = cleaning_shifts)

end_boxplot('Total-Infections', new_infections, xlab = paste('Total Infections (among ', N, 'total workers)'), average = FALSE, main_title = main_title)

end_boxplot('Fraction-Short-production', shiftwise_short, xlab = 'Percentage of Production Shifts Short (> 15% of workers absent)', average = TRUE, xlim = c(0,1), percent = TRUE, main_title = main_title, mask = production_shifts)
end_boxplot('Fraction-Short-cleaning', shiftwise_short, xlab = 'Percentage of Cleaning Shifts Short (> 15% of workers absent)', average = TRUE, xlim = c(0,1), percent = TRUE, main_title = main_title, mask = cleaning_shifts)

end_barplot('Ever-Short-production', shiftwise_short, xlab = 'Production Shift(s) Ever Short (percentage of runs)', average = TRUE, xlim = c(0,1), percent = TRUE, mask = production_shifts)
end_barplot('Ever-Short-cleaning', shiftwise_short, xlab = 'Cleaning Shift Ever Short (percentage of runs)', average = TRUE, xlim = c(0,1), percent = TRUE, mask = cleaning_shifts)

#TBD: Make this function work correctly with the facility model (i.e., with not
#everyone being on every work shift)
sample_data = function() {
    interventions = length(full_output_filenames)
    for (i in 1:interventions) {
        full_output = readRDS(full_output_filenames[i])

        if(i == 1) {
            steps = dim(full_output)[1]
            reps = dim(full_output)[3]
            unavailable_array = array(0, c(interventions, steps, reps))
            total_infections_array = array(0, c(interventions, reps))
        }
        #print(i)
        #print(dim(full_output))
        #print(dim(unavailable_array))
        #print(dim(unavailable(full_output)))
        unavailable_array[i,,] = unavailable(full_output)
        total_infections_array[i,] = apply(full_output[,'new_infections',], 2, sum)
    }

    l = list(N = N,
             days = days,
             unavailable = unavailable_array,
             total_infections = total_infections_array,
             intervention_names = row.names,
             work_shifts = work_shifts)
    saveRDS(l, 'sample_data-new.rds')
}

sample_data()

ANALYZE = FALSE

} #analyze_fn
