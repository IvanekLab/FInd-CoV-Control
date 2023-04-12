library(png)
library(ggplot2)
library(grid)
library(cowplot)

filenames = c(
    # 'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_v4-Infected_2.2.0.png',
    # 'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_diffable-Total-Infections-violin_2.2.0.png',
    # 'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_pairwise-differences-Total-Infections-violin_2.2.0.png',

    'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_v4-Symptomatic_2.2.0.png',
    'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_diffable-Total-Symptomatic-Infections-violin_2.2.0.png',
    'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_pairwise-differences-Total-Symptomatic-Infections-violin_2.2.0.png',

    'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_v4-Unavailable-production_2.2.0.png',
    'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_diffable-Average-Unavailable-production-violin_2.2.0.png',
    'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_pairwise-differences-Average-Unavailable-production-violin_2.2.0.png'
)

png('new-figure-2-2023-04-03.png', width = 3000, height = 2000)
l = NULL
for(filename in filenames) {
    this_image = readPNG(filename)
    this_plot = ggplot() + annotation_custom(
        grid::rasterGrob(
            this_image,
            width = unit(1, 'npc'),
            height = unit(1, 'npc')
        )
    )
    l = c(l, list(this_plot))
}
print(plot_grid(plotlist = l, nrow = 2))#3))
dev.off()


#alternatively
#library(png)
#library(ggplot2)
#library(grid)
#library(cowplot)

filenames = c(
    # 'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_v4b-Average-Unavailable-production-violin_2.2.0.png',
    'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_v4b-Total-Intervention-Expenses-vioplot_2.2.0.png',
    'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_v4b-Total-Production-Loss-vioplot_2.2.0.png',
    'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_v4b-Total-Cost-violin_2.2.0.png'
)

png('new-figure-3-2023-04-06.png', width = 3000, height = 1000)
l = NULL
for(filename in filenames) {
    this_image = readPNG(filename)
    this_plot = ggplot() + annotation_custom(
        grid::rasterGrob(
            this_image,
            width = unit(1, 'npc'),
            height = unit(1, 'npc')
        )
    )
    l = c(l, list(this_plot))
}
print(plot_grid(plotlist = l, nrow = 1))#2))
dev.off()

