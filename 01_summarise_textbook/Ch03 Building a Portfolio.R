## CHAPTER 3 Returns: BUIDLING A PORTFOLIO ----

## Goal ----
# Collect individual returns into a portfolio by assigning a weight to each stock

## Set up ----

# Core
library(tidyverse)
library(tidyquant)

# Import Excel files
library(readxl)

# time series
library(timetk)


## 1 Import stock prices ----

asset_returns_long_tbl <- read_rds("00_data/Ch02_asset_returns_long_tbl.rds")

## 2 Assign a weight to each asset ----

symbols <- asset_returns_long_tbl %>% distinct(asset) %>% pull()

w <- c(0.25,
       0.25,
       0.20,
       0.20,
       0.10)

w_tbl <- tibble(symbols, w)

## 3 Build a portfolio ----

portfolio_returns_rebalanced_monthly_tbl <- asset_returns_long_tbl %>%

    tq_portfolio(assets_col   = asset,
                 returns_col  = returns,
                 weights      = w_tbl,
                 col_rename   = "returns",
                 rebalance_on = "months")

portfolio_returns_rebalanced_monthly_tbl

write_rds(portfolio_returns_rebalanced_monthly_tbl,
          "00_data/Ch03_portfolio_returns_rebalanced_monthly_tbl.rds")


## 4 Plot ----

portfolio_returns_rebalanced_monthly_tbl %>%

    ggplot(aes(x = date, y = returns)) +
    geom_point(color = "cornflower blue") +

    # Formatting
    scale_x_date(breaks = scales::breaks_pretty(n = 6)) +

    labs(title = "Portfolio Returns Scatter",
         y = "monthly return")


portfolio_returns_rebalanced_monthly_tbl %>%

    ggplot(aes(returns)) +
    geom_histogram(fill = "cornflower blue",
                   binwidth = 0.005) +

    labs(title = "Portfolio Returns Distribution",
         y = "count",
         x = "returns")


portfolio_returns_rebalanced_monthly_tbl %>%

    ggplot(aes(returns)) +
    geom_histogram(fill = "cornflower blue",
                   binwidth = 0.01) +
    geom_density(aes(returns)) +

    labs(title = "Portfolio Histogram and Density",
         y = "distribution",
         x = "monthly returns")
