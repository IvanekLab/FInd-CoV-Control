kConstants = list( #using Google-style notation for now
    isolation_duration = 5, #days
    mu = 5.2, #for log-linear duration_{E+IP}
    sd = .1, #||
    duration_IP_mean = 1.058 * 2.174,
    duration_IP_shape = 1.058,
    duration_IA_mean = 1 * 5,
    duration_IA_shape = 5,
    duration_IM_mean = 0.5 * 16,
    duration_IM_shape = 16,
    duration_IS_mean = 0.4114 * 34.0278,
    duration_IS_shape = 34.0278,
    duration_IC_mean = 0.4114 * 34.0278,
    duration_IC_shape = 34.0278,
    p_trans_IP = .0575, #should not actually affect anything
    relative_trans_IA = .11,
    relative_trans_IM = .44
)
