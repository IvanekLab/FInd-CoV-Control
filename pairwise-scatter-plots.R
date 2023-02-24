d1 = apply(readRDS('sat2-with-iii/farm-sharedbaseline_community-0,work_R0-6,dormitory_R0-2,E0-1,initial_recovered-71,initial_V2-73,n_sims-1000index_i-1_full-output.rds')[,'new_symptomatic_infections',], 2, sum)

d1iii = apply(readRDS('sat2-with-iii/farm-sharedbaseline_community-0,work_R0-6,dormitory_R0-2,E0-1,initial_recovered-71,initial_V2-73,n_sims-1000index_i-1_full-output.rds')[,'iii',], 2, sum)


filenames = c(
    'sat2-with-iii/farm-sharedbaseline_community-0,work_R0-6,dormitory_R0-2,E0-1,T.test-38,initial_recovered-71,initial_V2-73,n_sims-1000index_i-2_full-output.rds',
    'sat2-with-iii/farm-sharedbaseline_community-0,work_R0-6,dormitory_R0-2,E0-1,v.test-0.05-rational,initial_recovered-71,initial_V2-73,n_sims-1000index_i-3_full-output.rds',
    'sat2-with-iii/farm-sharedbaseline_community-0,work_R0-6,dormitory_R0-2,E0-1,v.test-0.3-rational,initial_recovered-71,initial_V2-73,n_sims-1000index_i-4_full-output.rds',
    'sat2-with-iii/farm-sharedbaseline_community-0,work_R0-6,dormitory_R0-2,E0-1,v.test-1-rational,initial_recovered-71,initial_V2-73,n_sims-1000index_i-5_full-output.rds',
    'sat2-with-iii/farm-sharedbaseline_community-0,work_R0-6,dormitory_R0-2,E0-1,vax-rate0.02,initial_recovered-71,initial_V2-73,n_sims-1000index_i-6_full-output.rds',
    'sat2-with-iii/farm-sharedbaseline_community-0,work_R0-6,dormitory_R0-2,E0-1,vax-rate0.04,initial_recovered-71,initial_V2-73,n_sims-1000index_i-7_full-output.rds',
    'sat2-with-iii/farm-sharedbaseline_community-0,work_R0-6x(1-0.2),dormitory_R0-2,E0-1,initial_recovered-71,initial_V2-73,n_sims-1000index_i-8_full-output.rds',
    'sat2-with-iii/farm-sharedbaseline_community-0,work_R0-6x(1-0.4),dormitory_R0-2,E0-1,initial_recovered-71,initial_V2-73,n_sims-1000index_i-9_full-output.rds',
    'sat2-with-iii/farm-sharedbaseline_community-0,work_R0-6x(1-0.8),dormitory_R0-2,E0-1,initial_recovered-71,initial_V2-73,n_sims-1000index_i-10_full-output.rds',
    'sat2-with-iii/farm-sharedbaseline_community-0,work_R0-6,dormitory_R0-2,E0-1,initial_recovered-71,initial_V2-73,n_sims-1000index_i-11_full-output.rds',
    'sat2-with-iii/farm-sharedbaseline_community-0,work_R0-6,dormitory_R0-2,E0-1,initial_recovered-71,initial_V2-73,n_sims-1000index_i-12_full-output.rds',
    'sat2-with-iii/farm-sharedbaseline_community-0,work_R0-6,dormitory_R0-2,E0-1,vax-rate0.02,initial_recovered-71,initial_V2-73,n_sims-1000index_i-13_full-output.rds'
)

d = lapply(filenames,  function(s) readRDS(s))
d_ = lapply(d, function(x) x[,'new_symptomatic_infections',])
d__ = lapply(d_, function(x) apply(x, 2, sum))

d_iii = lapply(d, function(x) x[,'iii',])
d__iii = lapply(d_iii, function(x) apply(x, 2, sum))

#library(png)
#library(ggplot2)
#library(grid)
#library(cowplot)

row.names<-c(
        "Baseline",
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
    )[-1]

png('pairwise-scatterplots-with-iii-vs-3x-iii-relative.png', height = 3 * 1000, width = 12 * 1000)
l = 0:11 * 3 + 1
layout(matrix(c(l, l + 1, l + 2), byrow = T, ncol = 12))
#for(x in d__) {
for(i in 1:12) {
    x = d__[[i]]
    xiii = d__iii[[i]]
    par(mar = c(12, 12, 8, 8), mgp = c(8, 2, 0))
    #plot(d1, x, main = row.names[i], xlim = c(0, 71), ylim = c(0, 73), lwd = 2, cex.axis = 4, cex.names=4, cex.lab=4, cex.main=4, xlab = 'y_{j0m}', ylab = 'y_{jkm}')
    #points(c(0, 73), c(0, 73), type = 'l', col = 'blue', lwd = 2)
    #points(c(0, 73), c(0, 0), type = 'l', col = 'green', lwd = 2)
    y = (d1 - x) / d1 #NaN if d1 == 0, which is fine

    plot(d1iii, y, main = row.names[i], xlim = c(0, 19), ylim = c(-1/3, 1),
         lwd = 2, cex.axis = 4, cex.names=4, cex.lab=4, cex.main=4, xlab = 'iii_{j0m}', ylab = 'y_{jkm} - y_{j0m}')
    points(c(0, 19), c(0, 0), type = 'l', col = 'blue', lwd = 2)
    #points(d1iii, -d1, type = 'l', col = 'green', lwd = 2)
    points(c(0, 19), c(1, 1), type = 'l', col = 'green', lwd = 2)
    abline(v = 1)

    plot(xiii, y, main = row.names[i], xlim = c(0, 19), ylim = c(-1/3, 1),
         lwd = 2, cex.axis = 4, cex.names=4, cex.lab=4, cex.main=4, xlab = 'iii_{jkm}', ylab = 'y_{jkm} - y_{j0m}')
    points(c(0, 19), c(0, 0), type = 'l', col = 'blue', lwd = 2)
    points(c(0, 19), c(1, 1), type = 'l', col = 'green', lwd = 2)

    plot(xiii - d1iii, y, xlim = c(-18, 5), ylim = c(-1/3, 1),
         lwd = 2, cex.axis = 4, cex.names=4, cex.lab=4, cex.main=4, xlab = 'iii_{jkm} - iii_{j0m}', ylab = 'y_{jkm} - y_{j0m}')
    points(c(-18, 5), c(0, 0), type = 'l', col = 'blue', lwd = 2)
    points(c(-18, 5), c(1, 1), type = 'l', col = 'green', lwd = 2)
    
}
dev.off()

