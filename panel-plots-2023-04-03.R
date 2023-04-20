library(png)
library(ggplot2)
library(grid)
library(cowplot)

filenames = c(
    # 'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_v4-Infected_2.2.0.png',
    # 'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_diffable-Total-Infections-violin_2.2.0.png',
    # 'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_pairwise-differences-Total-Infections-violin_2.2.0.png',

    'sat2wi-clean/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Symptomatic_2.2.0.png',
    'sat2wi-clean/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Total-Symptomatic-Infections-violin_2.2.0.png',
    'sat2wi-clean/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_pairwise-differences-Total-Symptomatic-Infections-violin_2.2.0.png',

    'sat2wi-clean/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_low-mode-pairwise-differences-Total-Symptomatic-Infections-violin_2.2.0.png',
    'sat2wi-clean/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_high-mode-pairwise-differences-Total-Symptomatic-Infections-violin_2.2.0.png',
    'sat2wi-clean/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_high-mode-pairwise-percent-differences-Total-Symptomatic-Infections-violin_2.2.0.png'

    # 'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_v4-Unavailable-production_2.2.0.png',
    # 'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_diffable-Average-Unavailable-production-violin_2.2.0.png',
    # 'sat2wi-clean/facility-shared-vaccinated_TRUE-recovered_TRUE_pairwise-differences-Average-Unavailable-production-violin_2.2.0.png'
)

png('large--counter-alternative--new-figure-2-2023-04-11.png', width = 3000, height = 2000)
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

filenames = c(
    'sat2wi-clean/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Unavailable-production_2.2.0.png',
    'sat2wi-clean/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Average-Unavailable-production-violin_2.2.0.png',
    'sat2wi-clean/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_pairwise-differences-Average-Unavailable-production-violin_2.2.0.png'
)



png('large--counter-alternative--new-figure-3a-2023-04-11.png', width = 3000, height = 1000)
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
print(plot_grid(plotlist = l, nrow = 1))#3))
dev.off()


#counter-alternatively
#library(png)
#library(ggplot2)
#library(grid)
#library(cowplot)

filenames = c(
    'sat2wi-clean/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Total-Intervention-Expenses-violin_2.2.0.png',
    'sat2wi-clean/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Total-Production-Loss-violin_2.2.0.png',
    'sat2wi-clean/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Total-Cost-violin_2.2.0.png'
)

png('large--counter-alternative--new-figure-3b-2023-04-11.png', width = 3000, height = 1000)
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

