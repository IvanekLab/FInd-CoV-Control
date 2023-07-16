library(png)
library(ggplot2)
library(grid)
library(cowplot)

filenames = c(
    'bobrovitz-test--scenario--safer/bobrovitzfacility-shared-vaccinated_TRUE-recovered_TRUE_Symptomatic-incidence_2.2.0.png',
    'bobrovitz-test--scenario--safer/bobrovitzfacility-shared-vaccinated_TRUE-recovered_TRUE_Symptomatic_2.2.0.png',
    'bobrovitz-test--scenario--safer/bobrovitzfacility-shared-vaccinated_TRUE-recovered_TRUE_Symptomatic-Fraction-Non-Zero_2.2.0.png',

    'bobrovitz-test--scenario--safer/bobrovitzfacility-shared-vaccinated_TRUE-recovered_TRUE_Total-Symptomatic-Infections-violin_2.2.0.png',



    'bobrovitz-test--scenario--safer/bobrovitzfacility-shared-vaccinated_TRUE-recovered_TRUE_non-zero-pairwise-differences-Total-Symptomatic-Infections-violin_2.2.0.png',
    'bobrovitz-test--scenario--safer/bobrovitzfacility-shared-vaccinated_TRUE-recovered_TRUE_non-zero-pairwise-percent-differences-Total-Symptomatic-Infections-violin_2.2.0.png'


)

png('figures-2023-07-10/figure-2-2023-07-03.png', width = 3000, height = 2000)
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
    'bobrovitz-test--scenario--safer/bobrovitzfacility-shared-vaccinated_TRUE-recovered_TRUE_Unavailable-production_2.2.0.png',
    'bobrovitz-test--scenario--safer/bobrovitzfacility-shared-vaccinated_TRUE-recovered_TRUE_Total-Unavailable-production-violin_2.2.0.png',
    'bobrovitz-test--scenario--safer/bobrovitzfacility-shared-vaccinated_TRUE-recovered_TRUE_Unavailable-production-Fraction-Non-Zero_2.2.0.png',

    'bobrovitz-test--scenario--safer/bobrovitzfacility-shared-vaccinated_TRUE-recovered_TRUE_zero-pairwise-differences-Total-Unavailable-production-violin_2.2.0.png',
    'bobrovitz-test--scenario--safer/bobrovitzfacility-shared-vaccinated_TRUE-recovered_TRUE_non-zero-pairwise-differences-Total-Unavailable-production-violin_2.2.0.png',
    'bobrovitz-test--scenario--safer/bobrovitzfacility-shared-vaccinated_TRUE-recovered_TRUE_non-zero-pairwise-percent-differences-Total-Unavailable-production-violin_2.2.0.png'
)



png('figures-2023-07-10/figure-3a-2023-07-03.png', width = 3000, height = 2000)
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
print(plot_grid(plotlist = l, nrow = 2))
dev.off()

filenames = c(
    'bobrovitz-test--scenario--safer/bobrovitzfacility-shared-vaccinated_TRUE-recovered_TRUE_Total-Intervention-Expenses-violin_2.2.0.png',
    'bobrovitz-test--scenario--safer/bobrovitzfacility-shared-vaccinated_TRUE-recovered_TRUE_Total-Production-Loss-violin_2.2.0.png',
    'bobrovitz-test--scenario--safer/bobrovitzfacility-shared-vaccinated_TRUE-recovered_TRUE_Total-Cost-violin_2.2.0.png'
)

png('figures-2023-07-10/figure-3b-2023-07-03.png', width = 3000, height = 1000)
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

