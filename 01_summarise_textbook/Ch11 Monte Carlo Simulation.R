## CHAPTER 11 Monte Carlo Simulation ----

## Goal ----
# Simulate future portfolio returns

## Set up ----

# Core
library(tidyverse)
library(tidyquant)

# Import Excel files
library(readxl)

# time series
library(timetk)


## 1 Simulating growht of a dollar ----

# Import data
portfolio_returns_rebalanced_monthly_tbl <- read_rds("00_data/Ch03_portfolio_returns_rebalanced_monthly_tbl.rds")

# Get mean portfolio return
mean_port_return <- mean(portfolio_returns_rebalanced_monthly_tbl$returns)

# Get standard deviation of portfolio returns
stddev_port_return <- sd(portfolio_returns_rebalanced_monthly_tbl$returns)

# Construct a normal distribution
simulated_monthly_returns <- rnorm(120, mean_port_return, stddev_port_return)

# Add a dollar
simulated_returns_add_1 <- tibble(returns = c(1, 1 + simulated_monthly_returns))

# Calculate the cumulative growth of a dollar
simulated_growth <- simulated_returns_add_1 %>%
    mutate(growth = accumulate(returns, function(x, y) x*y)) %>%
    select(growth)

# Check the compound annual growth rate
cagr <- ((simulated_growth$growth[nrow(simulated_growth)]^(1/10)) - 1) * 100
cagr


## 2 Several simulation functions ----

simulate_acculation <- function(init_value, N, mean, stdev) {

    tibble(returns = c(init_value, 1 + rnorm(N, mean, stdev))) %>%
        mutate(growth = accumulate(returns, function(x, y) x*y)) %>%
        select(growth)

}

dump(list = c("simulate_acculation"), file = "00_scripts/simulate_acculation.R")

simulate_acculation(1, 120, mean_port_return, stddev_port_return)

## 3 Running multiple simulations ----

# Create a vector of 1s as a starting point
sims <- 51
starts <- rep(1, sims) %>%
    set_names(paste("sim", 1:sims, sep = ""))

starts

# Simulate
monte_carlo_sim_51 <- starts %>%

    # Simulate
    map_dfc(simulate_acculation,
            N     = 120,
            mean  = mean_port_return,
            stdev = stddev_port_return) %>%

    # Add the column, month
    mutate(month = seq(1:nrow(.))) %>%

    # Arrange column names
    select(month, everything()) %>%
    set_names(c("month", names(starts))) %>%

    pivot_longer(cols = -month, names_to = "sim", values_to = "growth")

## 4 Visualizing simulations with ggplot ----

monte_carlo_sim_51 %>%

    ggplot(aes(x = month, y = growth, col = sim)) +
    geom_line() +
    theme(legend.position = "none")

# Simplify the plot

sim_summary <- monte_carlo_sim_51 %>%

    group_by(sim) %>%
    summarise(growth = last(growth)) %>%
    ungroup() %>%

    summarise(max = max(growth),
              median = median(growth),
              min = min(growth))


monte_carlo_sim_51 %>%

    group_by(sim) %>%
    filter(last(growth) == sim_summary$max |
           last(growth) == sim_summary$median |
           last(growth) == sim_summary$min) %>%

    # Plot
    ggplot(aes(month, growth, col = sim)) +
    geom_line() +
    theme()


# Calculate the quantiles for simulated values

probs <- c(.005, .025, .25, .5, .75, .975, .995)

monte_carlo_sim_51 %>%

    group_by(sim) %>%
    summarise(growth = last(growth)) %>%
    ungroup() %>%
    pull(growth) %>%

    # Find the quantiles
    quantile(probs = probs) %>%
    round(2)


## 5 Visualizing simulations with highcharter ----
