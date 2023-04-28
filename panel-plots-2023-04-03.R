library(png)
library(ggplot2)
library(grid)
library(cowplot)

filenames = c(
    'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Symptomatic-incidence_2.2.0.png',
    'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Symptomatic_2.2.0.png',
    'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Symptomatic-Fraction-Non-Zero_2.2.0.png',
    # 'elaborated-unavailability/facility-shared-vaccinated_TRUE-recovered_TRUE_v4-Infected_2.2.0.png',
    # 'elaborated-unavailability/facility-shared-vaccinated_TRUE-recovered_TRUE_diffable-Total-Infections-violin_2.2.0.png',
    # 'elaborated-unavailability/facility-shared-vaccinated_TRUE-recovered_TRUE_pairwise-differences-Total-Infections-violin_2.2.0.png',

    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Symptomatic_2.2.0.png',
    'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Total-Symptomatic-Infections-violin_2.2.0.png',
    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_pairwise-differences-Total-Symptomatic-Infections-violin_2.2.0.png',

    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_low-mode-pairwise-differences-Total-Symptomatic-Infections-violin_2.2.0.png',
    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_high-mode-pairwise-differences-Total-Symptomatic-Infections-violin_2.2.0.png',
    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_high-mode-pairwise-percent-differences-Total-Symptomatic-Infections-violin_2.2.0.png',


    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_zero-pairwise-differences-Total-Symptomatic-Infections-violin_2.2.0.png',
    'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_non-zero-pairwise-differences-Total-Symptomatic-Infections-violin_2.2.0.png',
    'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_non-zero-pairwise-percent-differences-Total-Symptomatic-Infections-violin_2.2.0.png'

    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Symptomatic_2.2.0.png',
    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Total-Symptomatic-Infections-prevalence-violin_2.2.0.png',
    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_non-zero-pairwise-differences-Total-Symptomatic-Infections-prevalence-violin_2.2.0.png',
    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_non-zero-pairwise-percent-differences-Total-Symptomatic-Infections-prevalence-violin_2.2.0.png'

    # 'elaborated-unavailability/facility-shared-vaccinated_TRUE-recovered_TRUE_v4-Unavailable-production_2.2.0.png',
    # 'elaborated-unavailability/facility-shared-vaccinated_TRUE-recovered_TRUE_diffable-Average-Unavailable-production-violin_2.2.0.png',
    # 'elaborated-unavailability/facility-shared-vaccinated_TRUE-recovered_TRUE_pairwise-differences-Average-Unavailable-production-violin_2.2.0.png'
)

png('large--counter-alternative--new-figure-2-2023-04-26.png', width = 3000, height = 2000)
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
print(plot_grid(plotlist = l, nrow = 2))
dev.off()

#stop('Got the first plot!')

filenames = c(
    'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Unavailable-incidence_2.2.0.png',
    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Total-Unavailable-incidence-violin_2.2.0.png',
    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_pairwise-differences-Average-Unavailable-production-violin_2.2.0.png',

    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_low-mode-pairwise-differences-Total-Unavailable-incidence-violin_2.2.0.png',
    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_high-mode-pairwise-differences-Total-Unavailable-incidence-violin_2.2.0.png',
    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_high-mode-pairwise-percent-differences-Total-Unavailable-incidence-violin_2.2.0.png',

    'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Unavailable-production_2.2.0.png',
    'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Total-Unavailable-production-violin_2.2.0.png',
    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_pairwise-differences-Average-Unavailable-production-violin_2.2.0.png',

    'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_low-mode-pairwise-differences-Total-Unavailable-production-violin_2.2.0.png',
    'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_high-mode-pairwise-differences-Total-Unavailable-production-violin_2.2.0.png',
    'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_high-mode-pairwise-percent-differences-Total-Unavailable-production-violin_2.2.0.png',
    NA,
    'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Unavailable-production-Fraction-Non-Zero_2.2.0.png',
    NA
    


    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_zero-pairwise-differences-Average-Unavailable-production-violin_2.2.0.png',
    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_non-zero-pairwise-differences-Average-Unavailable-production-violin_2.2.0.png',
    # 'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_non-zero-pairwise-percent-differences-Average-Unavailable-production-violin_2.2.0.png'
)



png('large--counter-alternative--new-figure-3a-2023-04-26.png', width = 3000, height = 3000)
l = NULL
for(filename in filenames) {
    if(is.na(filename)) {
        this_plot = ggplot()
    } else {
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
}
print(plot_grid(plotlist = l, nrow = 3))
dev.off()


#counter-alternatively
#library(png)
#library(ggplot2)
#library(grid)
#library(cowplot)

filenames = c(
    'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Total-Intervention-Expenses-violin_2.2.0.png',
    'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Total-Production-Loss-violin_2.2.0.png',
    'elaborated-unavailability/large--counter-alternative--facility-shared-vaccinated_TRUE-recovered_TRUE_Total-Cost-violin_2.2.0.png'
)

png('large--counter-alternative--new-figure-3b-2023-04-20.png', width = 3000, height = 1000)
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

