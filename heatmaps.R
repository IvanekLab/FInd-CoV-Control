library(viridis)
library(fields)
civ = cividis(1000)
civ[1] = '#000000'
source('ContactsGen-v2.2.0.R')
farm = ContactsGen(rep(3,3), rep(10,9), example_rates, 1)
#so farm structure is 1,(2,(3,4:13),(14,15:24),(25))
#leaving farm structure alone for now
png('farm-heatmap.png', height = 1000, width = 1000)
image.plot(farm, col = civ[1:513], xaxt = 'n', yaxt = 'n', legend.lab = 'Relative Contact Rate', legend.cex = 4, legend.width = 3, axis.args = list(lwd.ticks = 0, at = c(0, max(farm)), labels = rep('',2)), zlim = c(0,max(farm)))
dev.off()
source('custom-contacts-gen-comparable-v2.2.0.R')
v = c(1:22, 33:42, 23:32,94:103,43:63,74:83,64:73,84:93)
ssra = shift_sum[v,v]
png('facility-heatmap.png', height = 1000, width = 1000)
image.plot(ssra, col = civ, xaxt = 'n', yaxt = 'n', legend.lab = 'Relative Contact Rate', legend.cex = 4, legend.width = 3, axis.args = list(lwd.ticks = 0, at = c(0, max(shift_sum)), labels = rep('',2)), zlim = c(0,max(shift_sum)))
dev.off()

#> max(farm/sum(farm))/max(shift_sum/sum(shift_sum))
#[1] 0.5131786

png('facility-heatmap-original-order.png', height = 1000, width = 1000)
image.plot(shift_sum, col = civ, xaxt = 'n', yaxt = 'n', legend.lab = 'Relative Contact Rate', legend.cex = 4, legend.width = 3, axis.args = list(lwd.ticks = 0, at = c(0, max(shift_sum)), labels = rep('',2)), zlim = c(0,max(shift_sum)))
dev.off()
