######## analyze model predictions
analyze_fn = function() {  #will it work? the goal is to get more meaningful debug data

ANALYZE = TRUE
source('double-wrapped-v1.1.3.R', local = TRUE)
list_ = double_wrapped_fn()
row.names = list_[[1]]
colors = list_[[2]]
ltys = list_[[3]]
full_output_filenames = list_[[4]]

#again, this sort of information should be stored in saved files once we have
#more complex weeks, but for now, it's fine
workday = c('work', 'home', 'sleep')
day_off = c('home', 'home', 'sleep')
week = c(rep(workday, 5), rep(day_off, 2))
schedule = rep(week, ceiling(days/7))[1:(3 * days)]
work_shifts = (schedule == 'work')

#summary plots
combine = function(data, outcome_fn, summary_fn, summation_mode) {
    if(!(summation_mode %in% c(FALSE, 'after', 'before'))) {
        stop('Invalid summation mode')
    }
    dimnames(data) = list(rep(NA, dim(data)[1]), colnames(data), rep(NA, dim(data)[3])) #kludge -- ideally should save data with dimnames, but left like this for now, for consistency with recent past versions
    outcomes = outcome_fn(data)
    if(summation_mode == 'before') {
        outcomes = apply(outcomes, 2, cumsum) / (steps / days) #awkward, but more likely for me to notice to fix it than a simple "3"
    }
    summarized = apply(outcomes, 1, summary_fn)
    if(summation_mode == 'after') { #unlikely to be a good idea
        print('Are you sure this is a good idea?')
        summarized = cumsum(summarized) / (steps / days) #awkward, but more likely for me to notice to fix it than a simple "3"
    }
    summarized
}

#outcome_fn's
infected = function(data) {
    data[,'IA',] + data[,'IP',] + data[,'IM',] + data[,'IS',] + data[,'IC',]
}

hospitalized_dead = function(data) {
    data[,'IS',] + data[,'IC',] + data[,'D',]
}


unavailable = function(data) {
    (hospitalized_dead(data) + data[,'S_isolated',] + data[,'E_isolated',] +
                                data[,'IA_isolated',] + data[,'IP_isolated',] +
                                data[,'IM_isolated',] + data[,'R_isolated',] +
                                data[,'V1_isolated',] + data[,'V2_isolated',] +
                                data[,'V1E_isolated',] + data[,'V2E_isolated',]
    )
}

short = function(data) {
    unavailable(data) > .15 * N
}


#The following several functions should really be combined at some point
#main_title is currently unused, was used for "Delta" vs. "2020 strains" in compare-and-contrast ppt
oneplot = function(filename, outcome_fn, primary_summary_fn, ylim, ylab, summation_mode = FALSE, work_only = FALSE, main_title = NULL) {
    png(paste(subdirectory, unique_id, '_', filename, '_', VERSION, '.png', sep = ''), height = 1000, width = 1000)
    if(work_only) {
        step_index = step_index[work_shifts]
    }

    #bit of a kludge, but should ensure sane limits
    ys = list()

    for (i in 1:length(full_output_filenames)) {
        full_output = readRDS(full_output_filenames[i])
        if(work_only) {
            full_output = full_output[work_shifts,,]
            if(summation_mode != FALSE){ # * 3 for "person-days" as work days, rather than 24 person hours of actual missed shift {
                full_output = full_output #* (steps / days) #this would only be needed for an average, right?
            }
        }
        ys[[i]] = combine(full_output, outcome_fn, primary_summary_fn, summation_mode)
    }
    for(i in 1:length(full_output_filenames)) {
        if(i == 1) {
            par(mar = c(5,5,4,2))
            plot(step_index, ys[[i]], type = 'l', col = colors[i],
                 ylim = c(min(ylim[1], min(sapply(ys, min))),
                          max(ylim[2], max(sapply(ys,max)))), lwd = 4,
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
        legend("topright",inset = .06, row.names, lwd = 4,
                col = colors, lty = ltys, y.intersp = 1, cex = 1.5)
    }
    dev.off()
    return(max(sapply(ys,max))) #lazy way to get maxes for the plots with forced same axes
}

end_boxplot = function(filename, outcome_fn, xlab, summation_mode = 'before', work_only = FALSE, average = FALSE, xlim = NULL, percent = FALSE, main_title = NULL) {
    png(paste(subdirectory, unique_id, '_', filename, '_', VERSION, '.png', sep = ''), height = 1000, width = 1000)
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
        if(summation_mode == 'before') {
            outcomes = apply(outcomes, 2, cumsum)
        }
        final = as.vector(outcomes[dim(full_output)[1],])
        if(average) {
            final = final / length(step_index)
        }
        
        means[i] = mean(final, na.rm = TRUE)
        if(summation_mode == 'after') { #unlikely to be a good idea
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
        if(summation_mode != FALSE) { #unlikely to be a good idea
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

end_barplot = function(filename, outcome_fn, xlab, summation_mode = FALSE, work_only = FALSE, default = days + 1, average = FALSE, xlim = NULL, percent = FALSE, anti_this = FALSE) {
    if(!is.null(filename)) {
        png(paste(subdirectory, unique_id, '_', filename, '_', VERSION, '.png', sep = ''), height = 1000, width = 1000)
    }
    if(work_only) {
        step_index = step_index[work_shifts]
    }
    
    all_outcomes = numeric(length(full_output_filenames))
    names(all_outcomes) = row.names
    for (i in 1:length(full_output_filenames)) {
        full_output = readRDS(full_output_filenames[i])
        if(work_only) {
            full_output = full_output[work_shifts,,]
        }

        dimnames(full_output) = list(rep(NA, dim(full_output)[1]), colnames(full_output), rep(NA, dim(full_output)[3])) #kludge
        outcomes = outcome_fn(full_output)
        if(anti_this) {
            thing_to_call = anti_max
        } else {
            thing_to_call = max
        }

        if(average) {
            fraction = mean(apply(outcomes, 2, thing_to_call)) #kludge, should be joined to another thing
        } else {
            fraction = sum(apply(outcomes, 2, thing_to_call)) #kludge, should be joined to another thing, extra kludgey with this name
        }
        if(summation_mode != FALSE) { #unlikely to be a good idea
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

#m1 = oneplot('Infected', infected, mean, c(0,0), paste('People Infectious (out of ', N, ' total)', sep = ''))
oneplot('Infected', infected, mean, c(0,0), paste('People Infectious (out of ', N, ' total)', sep = ''))#, main_title = 'Delta')
#m2 = oneplot('Unavailable', unavailable, mean, c(0,0), paste('People Unavailable to Work (out of ', N, ' total)', sep = ''), work_only = TRUE)
oneplot('Unavailable', unavailable, mean, c(0,0), paste('People Unavailable to Work (out of ', N, ' total)', sep = ''), work_only = TRUE)#, main_title = 'Delta')
#oneplot('force-common-infected', infected, mean, c(0,max(m1,m2)), paste('People Infectious (out of ', N, ' total)', sep = ''))
#oneplot('force-common-unavailable', unavailable, mean, c(0,max(m1,m2)), paste('People Unavailable to Work (out of ', N, ' total)', sep = ''), work_only = TRUE)
#oneplot('force-N-infected', infected, mean, c(0,N), paste('People Infectious (out of ', N, ' total)', sep = ''))
#oneplot('force-N-unavailable', unavailable, mean, c(0,N), paste('People Unavailable to Work (out of ', N, ' total)', sep = ''), work_only = TRUE)

#if(DELTA) {
#     main_title = 'Delta'
# } else {
#     main_title = '2020 Strains'
# }

main_title = ''

#cat('Fraction Recovered:', fraction_recovered, '\n')

#if(fraction_recovered != 0) {
#	cat('non-0 branch\n\n')
#	infected_force = 13
#	unavailable_force = 50#35
#} else {
#	cat('0 branch\n\n')
#	infected_force = 50
#	unavailable_force = 50#40
#}
# oneplot('force-infected', infected, mean, c(0,infected_force), paste('People Infectious (out of ', N, ' total)', sep = ''), main_title = main_title)
# oneplot('force-unavailable', unavailable, mean, c(0,unavailable_force), paste('People Unavailable to Work (out of ', N, ' total)', sep = ''), main_title = main_title, work_only = TRUE)

#end_boxplot('average-unavailable-titled', unavailable, xlab = paste('Average Absences per Shift (out of ', N, ' workers)'), work_only = TRUE, average = TRUE, main_title = main_title, xlim = c(0,35))
#end_boxplot('fraction-short-titled', short, xlab = 'Percentage of Shifts Short (> 15% of workers absent)', work_only = TRUE, average = TRUE, xlim = c(0,1), main_title = main_title, percent = TRUE)

end_boxplot('Average-Unavailable', unavailable, xlab = paste('Average Absences per Shift (out of ', N, ' workers)'), work_only = TRUE, average = TRUE, main_title = main_title)
end_boxplot('Fraction-Short', short, xlab = 'Percentage of Shifts Short (> 15% of workers absent)', work_only = TRUE, average = TRUE, xlim = c(0,1), percent = TRUE, main_title = main_title)

#png(paste(set_name, 'first-day-short-paneled.png', sep = '-'), height = 1000, width = 2000)
#par(mfrow = c(1,2))
end_barplot('Ever-Short', short, xlab = 'Ever Short (percentage of runs)', work_only = TRUE, average = TRUE, xlim = c(0,1), percent = TRUE)
first_x_boxplot('First-Day-Short', short, xlab = 'First Day Short (among runs that are ever short)', work_only = TRUE, default = NA, xlim = c(1, days))
#dev.off()

#png(paste(set_name, 'consecutive-unshort-paneled.png', sep = '-'), height = 1000, width = 2000)
#par(mfrow = c(1,2))
#end_barplot(NULL, short, xlab = 'Never Short (percentage of runs)', work_only = TRUE, average = TRUE, xlim = c(0,1), percent = TRUE, anti_this = TRUE)
#first_x_boxplot(NULL, short, xlab = 'Longest consecutive sequence of work shifts without a shortage', work_only = TRUE, default = NA, xlim = c(1, sum(work_shifts)), consecutive_without = TRUE)
#dev.off()


ANALYZE = FALSE

} #analyze_fn
