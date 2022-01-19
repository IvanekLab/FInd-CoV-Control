tt = matrix(1, nrow = 10, ncol = 10)
ttd = diag(1, nrow = 10)

suprow = matrix(11/29, nrow = 1, ncol = 30)
supcol = t(suprow)

crew = tt - ttd
diff_crew = tt / 10

team_minus_sup = rbind(cbind(crew, diff_crew, diff_crew),
                       cbind(diff_crew, crew, diff_crew),
                       cbind(diff_crew, diff_crew, crew))
team = rbind(cbind(0, suprow),
             cbind(supcol, team_minus_sup))

diff_team = matrix(.1, nrow = 31, ncol = 31)

teams = team#rbind(cbind(team, diff_team),
        #      cbind(diff_team, team))

shift_floaters_contact_rate = rowSums(teams)[1] / 30

shift_floaters = crew * shift_floaters_contact_rate

shift_floater_tall = matrix(shift_floaters_contact_rate, nrow = 31, ncol = 10)
shift_floater_wide = t(shift_floater_tall)

production_shift = rbind(cbind(teams, shift_floater_tall),
                         cbind(shift_floater_wide, shift_floaters))

#empty_ps = matrix(0, nrow = 72, ncol = 72)

#other_production_shift = matrix(0, ncol = 72, nrow = 72)

#production_shift_1 = rbind(cbind(production_shift, other_production_shift),
#                           cbind(other_production_shift, other_production_shift))

#production_shift_2 = rbind(cbind(production_shift, other_production_shift),
#                           cbind(other_production_shift, other_production_shift))

cleaning_shift_contact_rate = rowSums(production_shift)[1] / 9 #19

cleaning_shift = matrix(cleaning_shift_contact_rate, nrow = 10, ncol = 10) - diag(cleaning_shift_contact_rate, nrow = 10)


all_floaters_contact_rate = rowSums(cleaning_shift)[1] / (41 * 2 + 10 + 3 * (10 - 1 + 1) - (10 + 1)) #+1 because of the manager
                                                                                          #3* because they encounter each other all 3 shifts

all_floaters = all_floaters_contact_rate * crew


production_shift_1_minus_manager = rbind(cbind(production_shift,
                                               matrix(0, nrow = 41, ncol = 41 + 10),
                                               matrix(all_floaters_contact_rate, nrow = 41, ncol = 10)),
                                         matrix(0, nrow = 41 + 10, ncol = 2 * 41 + 10 + 10),
                                         cbind(matrix(all_floaters_contact_rate, nrow = 10, ncol = 41),
                                               matrix(0, nrow = 10, ncol = 41 + 10),
                                               all_floaters))

manager_row_1 = cbind(matrix(all_floaters_contact_rate, nrow = 1, ncol = 41),
                      matrix(0, nrow = 1, ncol = 41 + 10),
                      matrix(all_floaters_contact_rate, nrow = 1, ncol = 10))
manager_col_1 = t(manager_row_1)

production_shift_1 = rbind(cbind(0, manager_row_1),                                 
                           cbind(manager_col_1, production_shift_1_minus_manager))

production_shift_2_minus_manager = rbind(matrix(0, nrow = 41, ncol = 2 * 41 + 10 + 10),
                                         cbind(matrix(0, nrow = 41, ncol = 41),
                                               production_shift,
                                               matrix(0, nrow = 41, ncol = 10),
                                               matrix(all_floaters_contact_rate, nrow = 41, ncol = 10)),
                                         matrix(0, nrow = 10, ncol = 2 * 41 + 10 + 10),
                                         cbind(matrix(0, nrow = 10, ncol = 41),
                                               matrix(all_floaters_contact_rate, nrow = 10, ncol = 41),
                                               matrix(0, nrow = 10, ncol = 10),
                                               all_floaters))

manager_row_2 = cbind(matrix(0, nrow = 1, ncol = 41),
                      matrix(all_floaters_contact_rate, nrow = 1, ncol = 41),
                      matrix(0, nrow = 1, ncol = 10),
                      matrix(all_floaters_contact_rate, nrow = 1, ncol = 10))
manager_col_2 = t(manager_row_2)


production_shift_2 = rbind(cbind(0, manager_row_2),                                 
                           cbind(manager_col_2, production_shift_2_minus_manager))

manager_row_c = cbind(matrix(0, nrow = 1, ncol = 2 * 41), matrix(all_floaters_contact_rate, nrow = 1, ncol = 10 + 10))
manager_col_c = t(manager_row_c)

cleaning_shift_without_manager = rbind(matrix(0, nrow = 2 * 41, ncol = 2 * 41 + 10 + 10),
                                       cbind(matrix(0, nrow = 10, ncol = 2 * 41),
                                             cleaning_shift,
                                             matrix(all_floaters_contact_rate, nrow = 10, ncol = 10)),
                                       cbind(matrix(0, nrow = 10, ncol = 2 * 41),
                                             matrix(all_floaters_contact_rate, nrow = 10, ncol = 10),
                                             all_floaters))

cleaning_shift_full = rbind(cbind(0, manager_row_c),
                            cbind(manager_col_c, cleaning_shift_without_manager))

shift_sum = production_shift_1 + production_shift_2 + cleaning_shift_full
#rowSums(shift_sum)
#image(shift_sum)

#png('ps1.png', height=1000, width=1000)
#image(production_shift_1, main = 'Production Shift 1')
#dev.off()
#png('ps2.png', height=1000, width=1000)
#image(production_shift_2, main = 'Production Shift 2')
#dev.off()
#png('cs1.png', height=1000, width=1000)
#image(cleaning_shift_full, main = 'Cleaning Shift')
#dev.off()
#png('sum.png', height=1000, width=1000)
#image(shift_sum, main = 'Sum across all shifts')
#dev.off()

