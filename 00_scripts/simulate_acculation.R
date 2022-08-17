simulate_acculation <-
function(init_value, N, mean, stdev) {

    tibble(returns = c(init_value, 1 + rnorm(N, mean, stdev))) %>%
        mutate(growth = accumulate(returns, function(x, y) x*y)) %>%
        select(growth)

}
