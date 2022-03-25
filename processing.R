##### Covid-19 Project Economic Analysis #####
##### Facility model #####

# Setting up connections and parameters
# Connection to infection model
##### Covid-19 Project Economic Analysis #####
##### Farm model #####

# Setting up connections and parameters
# User inputs (place filler)


# Connection to infection model
###########################

f_farm <- function(output_per_week, hourly_wage, size) {
    #for now
    #shift <- 2 # Number of shift

    data = readRDS(paste(subdirectory, unique_id, '_econ_data_', VERSION, '.rds', sep = '') )
    days <- data$days
    unavailable <- data$unavailable
    scheduled <- data$scheduled
    infections <- data$total_infections
    doses <- data$doses
    tests <- data$tests
    interventions <- data$intervention_names
    work_shifts <- data$work_shifts
    shift = ifelse(data$work_shifts[2], 2, 1) #kludge
    if (shift == 1) {
        production_shifts <- rep(c(rep(c("TRUE", "FALSE", "FALSE"), times = 5), rep("FALSE", times = 6)), 
                                 length.out = length(work_shifts))
    } else { 
        production_shifts <- rep(c(rep(c("TRUE", "TRUE", "FALSE"), times = 5), rep("FALSE", times = 6)), 
                                 length.out = length(work_shifts))
    }
    available <- scheduled - unavailable 
    avoided_infection <- -sweep(infections[,],2,infections[1,])
    absence_rate <- unavailable/scheduled
    absence_rate_1 <- absence_rate[,which(production_shifts == "TRUE"),]  #absence rate- production shift only
    work_days <- sum(rep(c(1,1,1,1,1,0,0),length.out = days)) #number of work days in the simulated period

  
    alpha_L <- 0.437 #labor return to scale - 1% labor change -> 0.501% change to output 




    ##################### Output (no overtime) #################
    f_production <- function(a) {
        if (a <= 0.15) {
            return(0)
        } else {
            return((a - 0.15) * alpha_L * (output_per_week / 5 / shift))
        }
    }
    out_loss <- apply(absence_rate_1, c(1,2,3) , f_production) 
    out1 <- apply(out_loss, c(1,2), mean) # 13 x (work day*shift), average daily output loss over iterations
    if (shift == 2) {
        out1 <- sapply(seq(1,work_days*shift-1, by= shift),function(i) rowSums(out1[,i:(i+1)]))
    }
  
    out2 <- apply(out_loss,c(3), rowMeans) # 13 x iteration, total output loss per iteration 
  
  
  
    # Plot 1 - Average daily production loss  
    color <- c('black', 'blue', rep('cyan', times=3),'red','red', 'grey','grey','grey','darkgreen','darkgreen','yellow')
    type <- c(1,1,1,2,3,1,2,1,2,3,1,2,1)
  
    png(paste(subdirectory, unique_id, '_', 'Production_Loss', '_', VERSION, '.png',
              sep = ''), width=1000, height = 1000)
    par(mar = c(4,4,4,10),xpd=TRUE)
    plot(0, type = 'n',xlim = c(1,work_days), ylim = c(0, max(out1)), xlab = "Work Days", ylab = "Estimated Production Loss in Dollar($)", cex.axis = 1.5, cex.lab = 1.5)
    for (i in 1:13){
        lines(c(1:work_days), out1[i,], col = color[i], lty = type[i], type = 'l', lwd = 4)
    }
    legend("topright", legend = data$intervention_names, lty = type, col = color, cex = 1.5, lwd =4)
    dev.off()
  
    # Plot 2 - Box plot
    png(paste(subdirectory, unique_id, '_', 'Total_Production_Loss', '_', VERSION, '.png',
                sep = ''), width=1000, height = 1000)
    par(mar = c(4,15,4,4),xpd=TRUE)
    boxplot(t(out2),  names = data$intervention_names, horizontal = TRUE, las = 1, col = color)
    title("Estimated Total Production Loss in Dollar($)")
    dev.off()
    
    ##################### Intervention ###################
    ########### Temperature screening ############
    thermometer <- 20 # $20 per thermometer 
    KN95 <- 1 # $1 per mask per day 
    face_shield <- 3 # $3 per face shield. Changing every 30 days. ($0.1/day) 
    ts_time <- 3 # 3 seconds for each screening
    ts_limit <- 5 #screening should be completed under 5 minute
    
    
    available2 <- tests[2,,] 
    screener <- ceiling (scheduled[2,,]/ (ts_limit * 60 / ts_time)) # number of screeners based on people scheduled
    ts_time2 <- available2 * ts_time / screener / 3600   # Actual daily screening time in hours
    screener_compensation <- ts_time2 * screener * hourly_wage
    screener_training <- max(screener) * hourly_wage # 1hour training cost for screeners
    thermometer_cost <- max(screener) * thermometer 
    
    cost1 <- screener_training + thermometer_cost 
    cost2 <- screener_compensation + (KN95 + face_shield/30) * screener  
    
    ts1 <- rowMeans(cost2) #average shift cost across iteration without setip cost
    ts2 <- aggregate(ts1, by = list(rep(c(1:days), each=3)), sum, na.rm = TRUE)[,2] #average daily cost without setup cost
    ts_cost <- replace(ts2, 1, ts2[1]+cost1) #adding setup cost to day 1
    
    
    ############ Virus testing ############
    vt_kit <- 10 # $10 per test
    vt_time <- 1/4 # 15 minutes waiting assumed
    vt_prod <- output_per_week / 5 / 8 * vt_time #production value during 15 min ts_time (5days/wk, 8hr/day)
    
    # Average wage compensation + kit cost over simulation
    for (i in 1:3) {
        assign(paste0("vt_",i), rowMeans(tests[2+i,,] * vt_time * hourly_wage + tests[2+i,,] * vt_kit))
    }
    
    # Production loss
    # 5, 30, or 100% - no loss, 30% absence rate, 100% loss
    vt_ploss_2 <- rep(c(1,1,1,1,1,0,0), length.out = days) # work day = 1, weekend = 0
    vt_ploss_2 <- vt_ploss_2 * (alpha_L * 0.15 * vt_prod) # production loss with 30% absence 
    vt_ploss_3 <- rep(c(1,1,1,1,1,0,0), length.out = days)
    vt_ploss_3 <- vt_ploss_3 * vt_prod 
    
    # Total
    vt_cost_1 <- aggregate(vt_1, by = list(rep(c(1:days), each=3)), sum)[2]
    vt_cost_2 <- vt_ploss_2 + aggregate(vt_2, by = list(rep(c(1:days), each=3)), sum)[2]
    vt_cost_3 <- vt_ploss_3 + aggregate(vt_3, by = list(rep(c(1:days), each=3)), sum)[2]
    
    ############## Vaccination ############
    # 0.75 hour paid sick leave per vaccination  
    # no production loss
    
    for (i in c(6,7,11,12,13)) {
        assign(paste0("vc_cost_",i), aggregate(rowMeans(doses[i,,] * hourly_wage * 0.75), by = list(rep(c(1:days), each=3)), sum)[,2])
    } 
    
    
    ############ Biosafety Intervention ############
    # Low-intensity: KN95 masks per day
    # Medium/High -intensity: KN95 + one face shield per month 
    face_shield <- 3 # $3 per face shield. Changing every month (30 days)
    KN95 <- 1 # $1 per N95 per shift
    air_cleaner <- 1000 # 1 air cleaner per 1000 sqft
    life <- 3 * 365 # 3year life of air_cleaner
    
    bi_available <- available[9,,] #work day availability
    bi_available[which(data$work_shifts==FALSE),] <- 0
    bi_cost_low <- aggregate(rowMeans(KN95 * bi_available), by = list(rep(c(1:days), each=3)), sum)[2]  #average daily cost over iterations
    bi_cost_med <- aggregate(rowMeans((KN95 + face_shield/30) * bi_available), by = list(rep(c(1:days), each=3)), sum)[2] #average daily cost over iterations
    air_cleaner_cost <- size/1000 * air_cleaner / life # daily cost of air_cleaner
    bi_cost_hi <- aggregate(rowMeans((KN95 + face_shield/30) * bi_available), 
                            by = list(rep(c(1:days), each=3)), sum)[2] + air_cleaner_cost
    #average daily cost over iterations
  
  
    # Plot 3 Average daily intervention cost 
    all <- data.frame(ts_cost, vt_cost_1, vt_cost_2, vt_cost_3, 
                        vc_cost_6, vc_cost_7, bi_cost_low, bi_cost_med, bi_cost_med, 
                        vc_cost_11, vc_cost_12, vc_cost_13)
    all <-lapply(all, as.numeric)
    ylim = c(0,max(unlist(all)))
    #png(paste0(getwd(),"/Plot/Facility_plot_3_unsummed_log.png"), width=1000, height = 1000)
    #par(mar = c(4,4,4,10),xpd=TRUE)
    #plot(0, type='n', xlab = "Days", ylab = "Estimated Cumulative Intervention Cost in Dollar($)", xlim=c(1,days), ylim = ylim, log = 'y')
    #for (i in 1:12) {
    #  lines(c(1:days), cumsum(all[[i]]), col = color[i], lty = type[i], type = 'l')
    #}
    #legend("topright", inset=c(-0.33,0), legend = interventions, lty = type, col = color, cex = 0.5)
    #dev.off()
    png(paste(subdirectory, unique_id, '_', 'Cumulative_Intervention_Cost_Unsummed', '_', VERSION, '.png',
                sep = ''), width=1000, height = 1000)
    par(mar = c(4,4,4,10),xpd=TRUE)
    plot(0, type='n', xlab = "Days", ylab = "Estimated Cumulative Intervention Cost in Dollar($)", xlim=c(1,days), ylim = ylim)
    for (i in 1:12) {
        lines(c(1:days), all[[i]], col = color[i], lty = type[i], type = 'l')
    }
    legend("topright", inset=c(-0.33,0), legend = interventions, lty = type, col = color, cex = 0.5)
    dev.off()
    
    cumsum <- lapply(all, cumsum)
    ylim <- c(0,max(unlist(cumsum)))
    # farm_color <- c('blue', rep('cyan', times=3),'red','red', 'grey','grey','darkgreen','darkgreen','yellow')
    # farm_type <- c(1,1,2,3,1,2,1,2,1,2,1)
    # farm_intervention_names = c("Temperature Screening, 38.0°C", 
    #                             "Virus Test, p = 0.05 / Working Day", "Virus Test, p = 0.3 / Working Day", 
    #                             "Virus Test, p = 1.0 / Working Day", 
    #                             "Vaccination, p = 0.02 / Day",  "Vaccination, p = 0.04 / Day", 
    #                             "Soc. Dist./Biosafety: -20% R₀", "Soc. Dist./Biosafety: -40% - -80% R₀", 
    #                             "Boosting, p = 0.02 / day", "Boosting, p = 0.04 / day",
    #                             "Vax + Boosting, p = 0.02/day")  
  
  
    png(paste(subdirectory, unique_id, '_', 'Cumulative_Intervention_Cost', '_', VERSION, '.png',
                sep = ''), width=1000, height = 1000)
    par(mar = c(4,4,4,10),xpd=TRUE)
    plot(0, type='n', xlab = "Days", ylab = "Estimated Cumulative Intervention Cost in Dollar($)", xlim=c(1,days), ylim = ylim)
    for (i in 1:12) {
        lines(c(1:days), cumsum(all[[i]]), col = color[i], lty = type[i], type = 'l')
    }
    legend("topright", inset=c(-0.33,0), legend = interventions, lty = type, col = color, cex = 0.5)
    dev.off()
    
    
    # Plot 4 Total cost over the period
    average_avoided_infection <- rowMeans(-sweep(infections,2,infections[1,]))
    total_production_loss <- rowSums(out1)
    total_intervention_cost <- unlist(lapply(all, sum))
    total <- c(total_production_loss[1],total_production_loss[2:13]+total_intervention_cost) 
    
    png(paste(subdirectory, unique_id, '_', 'Avoided_vs_Cost', '_', VERSION, '.png',
                sep = ''), width=1000, height = 1000)
    par(mar = c(4,4,4,10),xpd=TRUE)
    dot <- c(9,15,15,16,17,15,16,15,16,17,15,16,15)
    plot(average_avoided_infection, total, col = color, pch = dot,
        xlab = "Average Avoided Infections", ylab = "Estimated Total Cost in Dollar($)",type = 'p')
    legend("topright", inset=c(-0.33,0), legend = data$intervention_names, pch = dot, col = color, cex = 0.5)
    dev.off()
}
    
f_farm(output_per_week = 1680000, # Total output per week
       hourly_wage = 14.75, # Average hourly wage of a worker
       size <- 1000
) # indoor area
