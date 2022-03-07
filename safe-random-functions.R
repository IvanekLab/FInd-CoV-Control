#safe versions of runif and rbinom (restricted to a Bernoulli distribution
#since that's all I actually need) to allow a guarantee that the same number of
#calls to the underlying PRNG (hence rand() calls) will be made for a given N,
#regardless of the other parameters used. This is necessary because otherwise, a
#call to runif(N, 0, 0), for example, will not actually make any calls to the
#underlying PRNG, while a call to runif(N, 0, 1) will. This also allows for the
#possibility of eventually adding some manner of latin hypercube functionality.


random_functions_and_counter = function() {
    set_seed = NA
    runif_0_1_calls_counter = NA

    safe_set_seed = function(seed) {
        set.seed(seed)
        set_seed <<- seed
        runif_0_1_calls_counter <<- 0
    }

    sunif = function(N, min, max) {
        X = runif(N, 0, 1)
        runif_0_1_calls_counter <<- runif_0_1_calls_counter + N
        X * (max - min) + min
    }

    sbern = function(N, prob) {
        X = runif(N, 0, 1)
        runif_0_1_calls_counter <<- runif_0_1_calls_counter + N
        1 * (X < prob) #strict < ensures that prob = 0 always gives 0,
                       #prob = 1 always gives 1
    }

    print_rand_state = function(s) {
        cat('\n\n', s, '\nseed:', set_seed, '\ncalls:', runif_0_1_calls_counter,
            '\ncurrent state, abbreviated:', .Random.seed[1:3], '\n\n')
    }

    list(
         safe_set_seed = safe_set_seed,
         sunif = sunif,
         sbern = sbern,
         print_rand_state = print_rand_state
    )
}

if(!exists('SAFE_RANDOM_FUNCTIONS_INITIALIZED')) {
    rfac_l = random_functions_and_counter()
    safe_set_seed = rfac_l[['safe_set_seed']]
    sunif = rfac_l[['sunif']]
    sbern = rfac_l[['sbern']]
    print_rand_state = rfac_l[['print_rand_state']]
    SAFE_RANDOM_FUNCTIONS_INITIALIZED = TRUE
}
