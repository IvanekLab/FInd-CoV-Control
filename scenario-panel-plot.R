#Annotation scheme:
#   capital for setting, lower-case for value
#   (old idea, ignore) lower-case for (tentative) "default," capital for altered
#Setting
#   r: farm
#   c: facility
#Housing:
#   s: shared
#   i: individual
#Vaccination:
#   d: default
#   0: no vaccination
#   1: max vaccination (not yet present)
#Recovery:
#   d: default
#   0: no recovered
#   1: max recovered (not yet present)

library(png)
library(ggplot2)
library(grid)
library(cowplot)

images = list()
plots = list()

for(setting in c('farm', 'facility')) {#, 'facility')) {
        #key = paste0('S', setting[3])
        #filename = paste0('scenario-analysis-take-2/',
    for(housing in c('shared', 'individual')) {
        for(vaccination in c('', 'no-vax')) {
            for(recovery in c('', 'no-recovered')) {
                vax_key = ifelse(vaccination == '',
                    'd',
                    ifelse(vaccination == 'no-vax',
                        '0',
                        '1'
                    )
                )
                recovery_key = ifelse(recovery == '',
                    'd',
                    ifelse(recovery == 'no-recovered',
                        '0',
                        '1'
                    )
                )
                #merged_vax_recovered_filename = ifelse(vax_key == '0' && recovery_key == '0'
                if(vax_key == '0' && recovery_key == '0') {
                    merged_fragment = '-start-of-epidemic'
                } else if(vax_key == 'd' && recovery_key == 'd') {
                    merged_fragment = ''
                } else {
                    merged_fragment = paste0('-', vaccination, recovery)
                }
                key_base = paste0(
                    'S:', substr(setting, 3, 3), ', ',
                    'H:', substr(housing, 1, 1), ', ',
                    'V:', vax_key, ', ',
                    'R:', recovery_key
                )
                is_baseline = (
                    (
                        (setting == 'farm' && housing == 'shared') ||
                        (setting == 'facility' && housing == 'individual')
                    ) &&
                    merged_fragment == ''
                )
                filename_base = paste0(
                    'scenario-analysis-take-2/',
                    setting, '-',
                    housing, '-random-start',
                    merged_fragment,
                    ifelse(is_baseline, 'baseline_', '_')
                )
                cat(key_base, ':', filename_base, '\n')
                if(setting == 'farm' || (merged_fragment == '')) {
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
        }
    }
}

png('scenario-panel-plots-pass-1.png', height = 4*1000, width = 3 * 1300)
keys = c(
    'S:r, H:s, V:d, R:d: Total-Symptomatic-Infections',
    'S:r, H:s, V:d, R:d: Average-Unavailable-production',
    'S:r, H:s, V:d, R:d: Total-Cost',
    'S:c, H:s, V:d, R:d: Total-Symptomatic-Infections',
    'S:c, H:s, V:d, R:d: Average-Unavailable-production',
    'S:c, H:s, V:d, R:d: Total-Cost',
    'S:c, H:i, V:d, R:d: Total-Symptomatic-Infections',
    'S:c, H:i, V:d, R:d: Average-Unavailable-production',
    'S:c, H:i, V:d, R:d: Total-Cost',
    'S:r, H:i, V:d, R:d: Total-Symptomatic-Infections',
    'S:r, H:i, V:d, R:d: Average-Unavailable-production',
    'S:r, H:i, V:d, R:d: Total-Cost'    
)
print(plot_grid(plotlist = plots[keys], nrow = 4))
dev.off()
