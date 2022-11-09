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
    #trying altering p_trans_Ix variables separately
    p_trans_IP = .0575,
    p_trans_IA = .0575 * .11,
    p_trans_IM = .0575 * .44,
    boosting_interval = 152,
    complete_immunity_duration_R = 45,
    second_shot_interval = 21, #days
    max_V1_protection = 0.36,
    V2_ramp_time = 14, #days
    V2_magnitude = 0.9115739,
    V2_decay_rate = 0.08904459 / 7, #1/days; / 7 converts from 1/weeks
    B_ramp_time_1 = 7, # days
    B_ramp_time_2 = 7, # days, between B_ramp_time_1 and end of ramp
    B_mid_ramp_protection = .62,
    B_magnitude_1 = 0.471669758,
    B_magnitude_2 = 0.326600870,
    B_decay_rate_1 = 0.083161719 / 7,
    B_decay_rate_2 = 0.008970573 / 7,
    SEVERE_MULTIPLIER = 1.2,
    R_question_period = 365, #days
    time_since_first_V2 = 365 + 61
)

#variables_to_exclude = list('boosting_interval') #to avoid clash with when we assume vaccination starts (which needs to be updated)

#Note: Does the obvious checks, but may miss some subtler issues (hopefully not)
#Also doesn't check for existence of all constants, only those which are used
#in consistency checking (relevant if constructing a set of constants "from
#scratch"
check_consistency = function(constants, altered_single_parameter == NULL, altered_parameters = NULL) {
    #p_trans_IP = get('p_trans_IP', constants)
    #p_trans_IA = get('p_trans_IA', constants)
    #p_trans_IM = get('p_trans_IM', constants)
    #if(p_trans_IP + p_trans_IA + p_trans_IM == 0) { #Inversely proportional to contact rates
    #    if(is.null(
    #    return list(consistent = FALSE,
    #                parameters_to_change = list(p_trans_IP = .0575))
    #}
    #Thought: What about setting transmission rates individually?
    #TBD: This (and the analogous bit in AgentGen-v2.2.0.R) should probably
    #just be 2 * boosting_interval
    if(2 * boosting_interval + 1 < time_since_first_V2)
}

