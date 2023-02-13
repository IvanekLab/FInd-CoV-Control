library(png)
library(ggplot2)
library(grid)
library(cowplot)

images = list()
plots = list()  

setting = 'farm'
housing = 'shared'
for(prefix in c('', 'larger-')) {
    for(suffix in c('', '-start-of-epidemic')) {
        key_base = paste0(prefix, suffix)
        is_baseline = (key_base == '')
        filename_base = paste0(
            'scenario-analysis-take-2/', prefix, 'farm-shared-random-start',
            suffix, ifelse(is_baseline, 'baseline_', '_')
        )
        cat(key_base, ':', filename_base, '\n')
        for(outcome in c(
                        'Total-Symptomatic-Infections',
                        'Average-Unavailable-production',
                        'Total-Cost'
        )) {
            filename = paste0(
                filename_base,
                outcome,
                '-violin_2.2.0.png'
            )
            key = paste0(key_base, ': ', outcome)
            images[[key]] = readPNG(filename)
            plots[[key]] = ggplot() + annotation_custom(
                grid::rasterGrob(
                    images[[key]],
                    width = unit(1, 'npc'),
                    height = unit(1, 'npc')
                )
            )
        }
    }
}

png('scenario-panel-plots-pass-5.png', height = 4*1000, width = 3 * 1300)
keys = c(
    ': Total-Symptomatic-Infections',
    ': Average-Unavailable-production',
    ': Total-Cost',
    'larger-: Total-Symptomatic-Infections',
    'larger-: Average-Unavailable-production',
    'larger-: Total-Cost',
    '-start-of-epidemic: Total-Symptomatic-Infections',
    '-start-of-epidemic: Average-Unavailable-production',
    '-start-of-epidemic: Total-Cost',
    'larger--start-of-epidemic: Total-Symptomatic-Infections',
    'larger--start-of-epidemic: Average-Unavailable-production',
    'larger--start-of-epidemic: Total-Cost'    
)
print(plot_grid(plotlist = plots[keys], nrow = 4))
dev.off()

