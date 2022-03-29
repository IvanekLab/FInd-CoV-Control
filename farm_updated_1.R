##### Covid-19 Project Economic Analysis #####

f_farm <- function(subdirectory, unique_id, VERSION, output_per_week, hourly_wage) {
  
    # Setting up connections and parameters
    # Connection to infection model
    data = readRDS(paste(subdirectory, unique_id, '_econ_data_', VERSION, '.rds', sep = '') )
    days <- data$days 
    unavailable <- data$unavailable
    scheduled <- data$scheduled
    available <- scheduled - unavailable
    infections <- data$total_infections
    doses <- data$doses
    tests <- data$tests
    interventions <- data$intervention_names
    
    work_shifts <- data$work_shifts #which(data$work_shifts==TRUE) 
    work_days <- sum(work_shifts) #length(work_shifts) 
    avoided_infection <- -sweep(infections,2,infections[1,])
    absence_rate <- unavailable/scheduled
    absence_rate <- absence_rate[,work_shifts,]
    #  day <- rep(1:days, each=3)[work_shifts]
    work_shift_per_week <- 5 
    alpha_L <- 0.437 #labor return to scale - 1% labor change -> 0.437% change to output 
  
    #output_per_week = data$N * 60.1 * 40
    #hourly_wage <- 13.89
  
  
    ##################### Output (no overtime) #################
    f_production <- function(a) {
        if (a <= 0.15) {
            return(0)
        } else {
            return((a - 0.15) * alpha_L * (output_per_week / work_shift_per_week))
        }
    }
  
    out_loss <- apply(absence_rate, c(1,2,3) , f_production) 
    out1 <- apply(out_loss, c(1,2), mean) # 13 * day, average daily output loss over iterations
    out2 <- apply(out_loss,c(3), rowMeans) # 13 * iteration, total output loss per iteration 
  
    # Plot 1 - Average daily production loss  
    color <- c('black', 'blue', rep('cyan', times=3),'red','red', 'grey','grey','grey','darkgreen','darkgreen','yellow')
    type <- c(1,1,1,2,3,1,2,1,2,3,1,2,1)
  
    png(paste(subdirectory, unique_id, '_', 'Production_Loss', '_', VERSION, '.png',
              sep = ''), width=1000, height = 1000)
    plot(0, type = 'n',xlim = c(1,days), ylim = c(0, max(out1)), xlab = "Work Days", ylab = "Estimated Production Loss in Dollar($)")
    print(max(out1))
    for (i in 1:13){
        lines(which(work_shifts)/3, out1[i,], col = color[i], lty = type[i], type = 'l', lwd = 4)
    }
    legend("topright", legend = interventions, lty = type, col = color, cex = 1.5, lwd = 4)
    dev.off()
  
    # Plot 2 - Box plot
    png(paste(subdirectory, unique_id, '_', 'Total_Production_Loss', '_', VERSION, '.png',
              sep = ''), width=1000, height = 1000)
    par(mar = c(5,23,4,2))
    boxplot(t(out2), names = interventions, horizontal = TRUE, las = 1, col = color, xlab = "Total Production Loss in Dollar($)", cex.axis = 1.5, cex.names = 1.5, ylab = '', na.action = na.pass)
    dev.off()

    ##################### Intervention ###################
    ########### Temperature screening ############
    thermometer <- 20 # $20 per thermometer 
    KN95 <- 1 # $1 per mask per day 
    face_shield <- 3 # $3 per face shield. Changing every 30 days. ($0.1/day) 
    ts_time <- 3 # 3 seconds for each screening
    ts_limit <- 5 #screening should be completed under 5 minute
  
  
    available2 <- tests[2,,] 
    screener <- ceiling(scheduled[2,,]/ (ts_limit * 60 / ts_time)) # number of screeners based on people scheduled
    ts_time2 <- available2 * ts_time / screener / 3600   # Actual daily screening time in hours
    screener_compensation <- ts_time2 * screener * hourly_wage
    screener_training <- max(screener) * hourly_wage # 1 hour training cost for screeners
    thermometer_cost <- max(screener) * thermometer 
  
    cost1 <- screener_training + thermometer_cost # setup cost 
    cost2 <- colSums(screener_compensation, na.rm = TRUE) + (KN95 + face_shield/30) * max(screener) * work_days # compensation+PPE
  
    ts_cost <- cost1 + cost2 # Total ts cost over simulated period for each iteration

    ############ Virus testing ############
    vt_kit <- 10 # $10 per test
    vt_time <- 1/4 # 0.25 hour waiting time
    vt_prod <- output_per_week / 5 / 8 * vt_time #production value in 15 min ts_time (5days/wk, 8hr/day)
  
    # Average wage compensation + kit cost over simulation
    vt <- apply(tests[3:5,,], c(1,3), sum) # total number of tests for each iteration
    vt_cost <- vt * (vt_time * hourly_wage + vt_kit) # total cost for each iteration
  
    # Production loss
    # 5 % - no loss
    # 30% - 30% absence rate
    # 100% - 15 minutes production loss
    #vt_ploss_2 <- alpha_L * 0.15 * vt_prod * work_days # total production loss with 30% absence 
    #vt_ploss_3 <- vt_prod * work_days
    
  
    # Total cost
    vt_cost_1 <- vt_cost[1,] 
    vt_cost_2 <- vt_cost[2,] + vt_ploss_2
    vt_cost_3 <- vt_cost[3,] + vt_ploss_3

    ############## Vaccination ############
    # 0.75 hour paid sick leave per vaccination  
    # no production loss
    vc_cost <- apply(doses[c(6,7,11,12,13),,], c(1,3), sum) * hourly_wage * 0.75
  
    ########### Biosafety Intervention ############
    # Low-intensity: KN95 masks per day
    # Medium/High -intensity: KN95 + one face shield per month 
  
    face_shield <- 3 # $3 per face shield. Changing every month (30 days)
    KN95 <- 1 # $1 per N95 per shift
    bi_available_low <- colSums(available[8,work_shifts,]) #total available work for each iterations
    bi_available_med <- colSums(available[9,work_shifts,])
    bi_available_hi <- colSums(available[10,work_shifts,])
  
    bi_cost_low <- bi_available_low * KN95
    bi_cost_med <- bi_available_med * (KN95 + face_shield/30)
    bi_cost_hi <- bi_available_med * (KN95 + face_shield/30)
  
    # Plot 3 Cumulative intervention cost 
    total_intervention <- data.frame(ts_cost, vt_cost_1, vt_cost_2, vt_cost_3, 
                        vc_cost[1,], vc_cost[2,], bi_cost_low, bi_cost_med, bi_cost_hi, 
                        vc_cost[3,], vc_cost[4,], vc_cost[5])
 
  
    # Plot 3 Cumulative intervention cost 
    png(paste(subdirectory, unique_id, '_', 'Cumulative_Intervention_Cost', '_', VERSION, '.png',
              sep = ''), width=1000, height = 1000)
    par(mar = c(5,23,4,2))
    boxplot(total_intervention, horizontal = TRUE, names = interventions[2:13], 
            las = 1, col = color[2:13], xlab = "Total Intervention Cost in Dollar($)",  cex.axis = 1.5, cex.names = 1.5, ylab = '', na.action = na.pass)
    dev.off()
  
    # Plot 4 Total cost over the period
    average_avoided_infection <- rowMeans(-sweep(infections,2,infections[1,]))
    total_production_loss <- rowSums(out1)
    total_intervention_cost <- unlist(lapply(total_intervention, mean))
    total <- c(total_production_loss[1],total_production_loss[2:13]+total_intervention_cost) 
    
    png(paste(subdirectory, unique_id, '_', 'Avoided_vs_Cost', '_', VERSION, '.png',
              sep = ''), width=1000, height = 1000)
    dot <- c(9,15,15,16,17,15,16,15,16,17,15,16,15)
    par(xpd=TRUE, mar = c(6, 6, 6, 14))
    plot(average_avoided_infection, total, col = color, pch = dot,
        xlab = "Average Avoided Infections", ylab = "Estimated Total Cost in Dollar($)",type = 'p', cex = 1.5)
    legend("topleft", legend = data$intervention_names, pch = dot, col = color, cex = 1.5)
    dev.off()
  
}
  
#f_farm('debugging-tests/', 'farm-default-v17', '2.1.0', output_per_week =
#        data$N * 60.1 * 40, hourly_wage <- 13.89)
  
