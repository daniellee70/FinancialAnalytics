## CHAPTER 4 Risk: Standard Deviation ----

## Goal ----
# Measure portfolio risk using standard deviation.

## Set up ----

# Core
library(tidyverse)
library(tidyquant)



# 1 Import stock prices ----

asset_returns_long_tbl <- read_rds("00_data/Ch02_asset_returns_long_tbl.rds")

portfolio_returns_rebalanced_monthly_tbl <-
    read_rds("00_data/Ch03_portfolio_returns_rebalanced_monthly_tbl.rds")

# 2 Standard deviation ----

portfolio_sd_tidyquant_builtin_percent <- portfolio_returns_rebalanced_monthly_tbl %>%

    tq_performance(Ra = returns,
                   Rb = NULL,
                   performance_fun = table.Stats) %>%
    select(Stdev) %>%
    mutate(tq_sd = round(Stdev, 4) * 100)

portfolio_sd_tidyquant_builtin_percent


# Figure 4.1 Dispersion of Portfolio Returns ----

portfolio_returns_rebalanced_monthly_tbl %>%

    ggplot(aes(date, returns)) +
    geom_point(color = "cornflowerblue", size = 2) +

    labs(title = "Scatterplot of Returns by Date") +
    theme(plot.title = element_text(hjust = 0.5))

# Figure 4.2 Scatter of Returns Colored by Distance from Mean ----

sd_plot <- sd(portfolio_returns_rebalanced_monthly_tbl$returns)
mean_plot <- mean(portfolio_returns_rebalanced_monthly_tbl$returns)

portfolio_returns_rebalanced_monthly_tbl %>%

    mutate(hist_col = case_when(
        returns > mean_plot + sd_plot ~ "high",
        returns < mean_plot - sd_plot ~ "middle",
        TRUE                          ~ "low"
    )) %>%

    # Plot
    ggplot(aes(date, returns, col = hist_col)) +
    geom_point(size = 2) +

    labs(title = "Colored Scatter") +
    theme(plot.title = element_text(hjust = 0.5))


# Figure 4.3 Scatter of Returns with Line at Standard Deviation ----

sd_plot <- sd(portfolio_returns_rebalanced_monthly_tbl$returns)
mean_plot <- mean(portfolio_returns_rebalanced_monthly_tbl$returns)

portfolio_returns_rebalanced_monthly_tbl %>%

    mutate(hist_col = case_when(
        returns > mean_plot + sd_plot ~ "high",
        returns < mean_plot - sd_plot ~ "middle",
        TRUE                          ~ "low"
    )) %>%

    # Plot
    ggplot(aes(date, returns, col = hist_col)) +
    geom_point(size = 2) +

    labs(title = "Colored Scatter with Line") +
    theme(plot.title = element_text(hjust = 0.5)) +

    # Add lines
    geom_hline(yintercept = mean_plot + sd_plot, linetype = "dotted", color = "purple") +
    geom_hline(yintercept = mean_plot - sd_plot, linetype = "dotted", color = "purple")

# Figure 4.4 Asset and Portfolio Standard Deviation Comparison ----

portfolio_returns_rebalanced_monthly_tbl

asset_returns_sd_tbl <- asset_returns_long_tbl %>%

    group_by(asset) %>%
    tq_performance(Ra = returns,
                   Rb = NULL,
                   performance_fun = table.Stats) %>%

    select(asset, Stdev) %>%
    ungroup() %>%

    # Add portfolio sd
    add_row(tibble(asset = "Portfolio",
                  Stdev = sd(portfolio_returns_rebalanced_monthly_tbl$returns)))

asset_returns_sd_tbl %>%

    # Plot
    ggplot(aes(asset, Stdev, col = asset)) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = asset),
                             data = asset_returns_sd_tbl %>%
                                 filter(asset == "Portfolio")) +

    labs(title = "")

# Figure 4.5 Expected Returns versus Risk ----

asset_returns_sd_mean_tbl <- asset_returns_long_tbl %>%

    group_by(asset) %>%
    tq_performance(Ra = returns,
                   Rb = NULL,
                   performance_fun = table.Stats) %>%

    select(asset, Mean = ArithmeticMean, Stdev) %>%
    ungroup() %>%

    add_row(tibble(asset = "Portfolio",
                   Mean  = mean(portfolio_returns_rebalanced_monthly_tbl$returns),
                   Stdev = sd(portfolio_returns_rebalanced_monthly_tbl$returns)))


asset_returns_sd_mean_tbl %>%

    ggplot(aes(Stdev, Mean, col = asset)) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = asset))

# 3 Rolling standard deviation ----
# Why rolling sd?
# Suppose that we have 10 years of data and calculated standard deviation for every six months.
# Consider two different scenarios: 1) sd of each six-month period is always 3% and
# 2) sd for each six-month period fluctuated between 0% and 6%.
# It's possible that both scenarios have the same 3% sd for the entire period, which are not the same
# Rolling sd can show us what might have caused spikes in volatility
# and consider dynamically rebalancing the portfolio to better manage the volatility

# Assign a value to winder
window <- 24

port_rolling_sd_tbl <- portfolio_returns_rebalanced_monthly_tbl %>%

    tq_mutate(select = returns,
              mutate_fun = rollapply,
              width      = window,
              FUN        = sd,
              col_rename = "rolling_sd") %>%
    select(date, rolling_sd) %>%
    na.omit()

# Figure 4.7 Rolling Volatility ggplot ----

port_rolling_sd_tbl %>%

    ggplot(aes(date, rolling_sd)) +
    geom_line(color = "cornflowerblue") +

    scale_y_continuous(labels = scales::percent) +
    scale_x_date(breaks = scales::breaks_pretty(n = 7))+

    labs(title = "24-Month Rolling Volatility",
         x = NULL,
         y = NULL) +
    theme(plot.title = element_text(hjust = 0.5))





