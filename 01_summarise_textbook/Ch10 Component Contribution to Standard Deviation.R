## CHAPTER 10 Component Contribution to Standard Deviation ----

## Goal ----
# Examine how each asset contributes to portfolio standard deviation
# This is to ensure that our risk is not concentrated in any one asset

## Set up ----

# Core
library(tidyverse)
library(tidyquant)
library(readr)

# Time series
library(lubridate)
library(tibbletime)
library(timetk)

# modeling
library(broom)


# 1 Import stock prices ----

asset_returns_long_tbl <- read_rds("00_data/wrangled_data/Ch02_asset_returns_long_tbl.rds")

portfolio_returns_tbl <-
    read_rds("00_data/wrangled_data/Ch03_portfolio_returns_rebalanced_monthly_tbl.rds")

source("00_scripts/convert_prices_to_returns.R")

# 2 Component Contribution Step-by-Step

# Transform data into wide form
asset_returns_wide_tbl <- asset_returns_long_tbl %>%

    pivot_wider(names_from = asset, values_from = returns) %>%

    column_to_rownames(var = "date")

# Covariance of asset returns
covariance_matrix <- cov(asset_returns_wide_tbl)

# Standard deviation of portfolio
w <- c(0.25, 0.25, 0.2, 0.2, 0.1)

sd_portfolio <- sqrt(t(w) %*% covariance_matrix %*% w)
sd_portfolio

# Marginal contribution
marginal_contribution <- w %*% covariance_matrix / sd_portfolio[1,1]
marginal_contribution

# Component contribution
component_contribution <- marginal_contribution * w
component_contribution

rowSums(component_contribution)

# Component contribution in percentage
component_percentages <- (component_contribution / sd_portfolio[1,1]) %>%
    round(3) %>%
    as_tibble()

component_percentages

component_percentages %>%

    as_tibble() %>%
    gather(key = "asset", value = "contribution")


# 3 Component Contribution with a Custom Function ----

# Transform data into wide form
asset_returns_wide_tbl <- asset_returns_long_tbl %>%

    pivot_wider(names_from = asset, values_from = returns) %>%

    column_to_rownames(var = "date")

# Custom function
calculate_component_contribution <- function(asset_returns_wide_tbl, w) {

    # Covariance of asset returns
    covariance_matrix <- cov(asset_returns_wide_tbl)

    # Standard deviation of portfolio
    sd_portfolio <- sqrt(t(w) %*% covariance_matrix %*% w)

    # Marginal contribution
    marginal_contribution <- w %*% covariance_matrix / sd_portfolio[1,1]

    # Component contribution
    component_contribution <- marginal_contribution * w

    # Component contribution in percentage
    (component_contribution / sd_portfolio[1,1]) %>%
        round(3) %>%
        as_tibble()

}

asset_returns_wide_tbl %>% calculate_component_contribution(w = c(0.25,0.25,0.2,0.2,0.1))

# 4 Visualizing Component Contribution -----

# Figure 10.1 Contribution to Standard Deviation ----
asset_returns_wide_tbl %>%

    calculate_component_contribution(w = c(0.25,0.25,0.2,0.2,0.1)) %>%
    gather(key = "asset", value = "contribution") %>%

    ggplot(aes(asset, contribution)) +
    geom_col(fill = "cornflowerblue")

# Figure 10.2 Weight versus Contribution ----
asset_returns_wide_tbl %>%

    calculate_component_contribution(w = c(0.25,0.25,0.2,0.2,0.1)) %>%
    gather(key = "asset", value = "contribution") %>%
    add_column(weights = c(0.25,0.25,0.2,0.2,0.1)) %>%
    pivot_longer(cols = c(contribution, weights), names_to = "type", values_to = "value") %>%

    ggplot(aes(asset, value, fill = type)) +
    geom_col(position = "dodge") +

    labs(title = "Percent Contribution to Volatility",
         y = "percent",
         x = "asset") +
    theme(plot.title = element_text(hjust = 0.5))


# 5 Rolling Component Contribution -----

calculate_comp_contrib_by_window <- function(asset_returns_wide_tbl,
                                             start = 1,
                                             window = 24,
                                             weights) {

    # 1 Define start date
    start_date <- rownames(asset_returns_wide_tbl)[start]

    # 2 Define end date
    end_date <- rownames(asset_returns_wide_tbl)[start + window]

    # 3 Subset df
    df_subset <- asset_returns_wide_tbl %>%

        rownames_to_column(var = "date") %>%

        filter(date >= start_date & date < end_date) %>%

        column_to_rownames(var = "date")

    # 4 Calculate component contribution
    component_percentages <-df_subset %>%
        calculate_component_contribution(w = weights)

    # 5 Add end date to df
    component_percentages %>%

        mutate(date = ymd(end_date)) %>%
        select(date, everything())

}

# Check the custom function
asset_returns_wide_tbl %>% calculate_comp_contrib_by_window(start = 1, window = 24,
                                                            w = c(0.25,0.25,0.2,0.2,0.1))


asset_returns_wide_tbl %>% calculate_comp_contrib_by_window(start = 2, window = 24,
                                                            w = c(0.25,0.25,0.2,0.2,0.1))

dump(list = c("calculate_component_contribution",
              "calculate_comp_contrib_by_window"),
     file = "00_scripts/calculate_comp_contrib_to_portfolio_volatility.R")

# Iterate the custom function
w <- c(0.25,0.25,0.2,0.2,0.1)
window <- 24

rolling_comp_contrib_tbl <- 1:(nrow(asset_returns_wide_tbl) - window) %>%

    map_df(.x = ., .f = ~calculate_comp_contrib_by_window(asset_returns_wide_tbl,
                                                          start = .x,
                                                          weights = w,
                                                          window = window))
rolling_comp_contrib_tbl

# 6 Visualizing Component Contribution -----

# Figure 10.3 Component Contribution ggplot ----
rolling_comp_contrib_tbl %>%

    # Transform data to long form
    pivot_longer(cols = -date, names_to = "asset", values_to = "contribution") %>%

    # Plot
    ggplot(aes(date, contribution, color = asset)) +
    geom_line() +

    scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
    scale_y_continuous(labels = scales::percent_format()) +

    annotate(geom = "text",
             x = as.Date("2016-07-01"),
             y = 0.03,
             color = "red", size = 5,
             label = str_glue("AGG dips below zero sometimes, indicating
                              it reduces the portfolio volatility."))

# Figure 10.4 Stacked Component Contribution ggplot ----
rolling_comp_contrib_tbl %>%

    # Transform data to long form
    pivot_longer(cols = -date, names_to = "asset", values_to = "contribution") %>%

    # Plot
    ggplot(aes(date, contribution, fill = asset)) +
    geom_area() +

    scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
    scale_y_continuous(labels = scales::percent_format()) +

    annotate(geom = "text",
             x = as.Date("2016-07-01"),
             y = 0.08,
             color = "red", size = 5,
             label = str_glue("AGG dips below zero sometimes, indicating
                              it reduces the portfolio volatility."))











