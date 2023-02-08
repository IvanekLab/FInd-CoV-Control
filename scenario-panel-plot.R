image11 = readPNG('scenario-analysis-take-2/farm-shared-random-startbaseline_Total-Symptomatic-Infections-violin_2.2.0.png')
image12 = readPNG('scenario-analysis-take-2/farm-shared-random-startbaseline_Average-Unavailable-production-violin_2.2.0.png')
image13 = readPNG('scenario-analysis-take-2/farm-shared-random-startbaseline_Total-Cost-violin_2.2.0.png')

image21 = readPNG('scenario-analysis-take-2/facility-shared-random-start_Total-Symptomatic-Infections-violin_2.2.0.png')
image22 = readPNG('scenario-analysis-take-2/facility-shared-random-start_Average-Unavailable-production-violin_2.2.0.png')
image23 = readPNG('scenario-analysis-take-2/facility-shared-random-start_Total-Cost-violin_2.2.0.png')

image31 = readPNG('scenario-analysis-take-2/facility-individual-random-startbaseline_Total-Symptomatic-Infections-violin_2.2.0.png')
image32 = readPNG('scenario-analysis-take-2/facility-individual-random-startbaseline_Average-Unavailable-production-violin_2.2.0.png')
image33 = readPNG('scenario-analysis-take-2/facility-individual-random-startbaseline_Total-Cost-violin_2.2.0.png')

image41 = readPNG('scenario-analysis-take-2/farm-individual-random-start_Total-Symptomatic-Infections-violin_2.2.0.png')
image42 = readPNG('scenario-analysis-take-2/farm-individual-random-start_Average-Unavailable-production-violin_2.2.0.png')
image43 = readPNG('scenario-analysis-take-2/farm-individual-random-start_Total-Cost-violin_2.2.0.png')


plot_11 = ggplot() + annotation_custom(grid::rasterGrob(image11, width = unit(1, 'npc'), height = unit(1, 'npc')))
plot_12 = ggplot() + annotation_custom(grid::rasterGrob(image12, width = unit(1, 'npc'), height = unit(1, 'npc')))
plot_13 = ggplot() + annotation_custom(grid::rasterGrob(image13, width = unit(1, 'npc'), height = unit(1, 'npc')))

plot_21 = ggplot() + annotation_custom(grid::rasterGrob(image21, width = unit(1, 'npc'), height = unit(1, 'npc')))
plot_22 = ggplot() + annotation_custom(grid::rasterGrob(image22, width = unit(1, 'npc'), height = unit(1, 'npc')))
plot_23 = ggplot() + annotation_custom(grid::rasterGrob(image23, width = unit(1, 'npc'), height = unit(1, 'npc')))

plot_31 = ggplot() + annotation_custom(grid::rasterGrob(image31, width = unit(1, 'npc'), height = unit(1, 'npc')))
plot_32 = ggplot() + annotation_custom(grid::rasterGrob(image32, width = unit(1, 'npc'), height = unit(1, 'npc')))
plot_33 = ggplot() + annotation_custom(grid::rasterGrob(image33, width = unit(1, 'npc'), height = unit(1, 'npc')))

plot_41 = ggplot() + annotation_custom(grid::rasterGrob(image41, width = unit(1, 'npc'), height = unit(1, 'npc')))
plot_42 = ggplot() + annotation_custom(grid::rasterGrob(image42, width = unit(1, 'npc'), height = unit(1, 'npc')))
plot_43 = ggplot() + annotation_custom(grid::rasterGrob(image43, width = unit(1, 'npc'), height = unit(1, 'npc')))

#plot_grid(plot_11, plot_12, plot_21, plot_22)

#plot_11 = ggplot() + annotation_custom(grid::rasterGrob(image11, width = unit(1, 'npc'), height = unit(1, 'npc')))
#plot_12 = ggplot() + annotation_custom(grid::rasterGrob(image12, width = unit(1, 'npc'), height = unit(1, 'npc')))
#plot_13 = ggplot() + annotation_custom(grid::rasterGrob(image13, width = unit(1, 'npc'), height = unit(1, 'npc')))


#plot_21 = ggplot() + annotation_custom(grid::rasterGrob(image21, width = unit(1, 'npc'), height = unit(1, 'npc')))
#plot_22 = ggplot() + annotation_custom(grid::rasterGrob(image22, width = unit(1, 'npc'), height = unit(1, 'npc')))

png('scenario-panel-plots-version-2.png', height = 4*1000, width = 3 * 1300)
plot_grid(plot_11, plot_12, plot_13,
          plot_21, plot_22, plot_23,
          plot_31, plot_32, plot_33,
          plot_41, plot_42, plot_43,
          nrow = 4#,
          #byrow = FALSE
)
dev.off()

