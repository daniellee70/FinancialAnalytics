## CHAPTER 6 Risk: Kurtosis ----

## Goal ----
# Measure portfolio risk using kurtosis
# Skewness is the extent to which returns appear in the tails of their distribution.
# The normal distribution has a kurtosis of 3.
# A distribution is said to have a negative kurtosis when it is smaller than 3 and vice versa.
# It matters to investors because a negative kurtosis means fewer returns in the tails,
# which probably means less risk because most investments have negatively skewed

## Set up ----

# Core
library(tidyverse)
library(tidyquant)


# 1 Import stock prices ----

asset_returns_long_tbl <- read_rds("00_data/Ch02_asset_returns_long_tbl.rds")

portfolio_returns_rebalanced_monthly_tbl <-
    read_rds("00_data/Ch03_portfolio_returns_rebalanced_monthly_tbl.rds")

portfolio_density_plot <- read_rds("00_data/fig/Ch05_portfolio_density_plot.rds")

# 2 Kurtosis ----
mean <- mean(portfolio_returns_rebalanced_monthly_tbl$returns)
median <- median(portfolio_returns_rebalanced_monthly_tbl$returns)
sd <- sd(portfolio_returns_rebalanced_monthly_tbl$returns)

sd_neg_shaded_area <- ggplot_build(portfolio_density_plot)$data[[1]] %>%
    filter(x < mean - 2*sd)

sd_pos_shaded_area <- ggplot_build(portfolio_density_plot)$data[[1]] %>%
    filter(x > mean + 2*sd)


portfolio_density_plot_shaded <- portfolio_density_plot +

    # positive shaded area
    geom_area(aes(x = x, y = y),
              fill = "pink", alpha = 0.5,
              data = sd_pos_shaded_area) +

    # negative shaded area
    geom_area(aes(x = x, y = y),
              fill = "pink", alpha = 0.5,
              data = sd_neg_shaded_area) +

    labs(y = "density")


# Figure 6.1 Kurtosis Density ggplot ----

portfolio_returns_rebalanced_monthly_tbl %>%

    ggplot(aes(returns)) +
    geom_histogram(alpha = 0.7,
                   binwidth = .003,
                   fill = "cornflowerblue",
                   color = "cornflowerblue")

# Figure 6.2 Kurtosis Density ggplot with Lines ----


mean_line_data <- ggplot_build(portfolio_density_plot)$data[[1]] %>%
    filter(x < mean)

median_line_data <- ggplot_build(portfolio_density_plot)$data[[1]] %>%
    filter(x < median)


portfolio_density_plot_shaded +

    # line for mean
    geom_segment(aes(x = mean, y = 0,
                     xend = mean, yend = density),
                 linetype = "dotted", color = "red", size = 1,
                 data = mean_line_data) +
    annotate(geom = "text",
             x = mean, y = 5,
             label = "mean", color = "red", size = 5, alpha = 0.5,
             angle = 90, fontface = "plain", vjust = -1.75) +

    # line for median
    geom_segment(aes(x = median, y = 0,
                     xend = median, yend = density),
                 linetype = "dotted", color = "red", size = 1,
                 data = median_line_data) +
    annotate(geom = "text",
             x = median, y = 5,
             label = "median", color = "black", size = 5, alpha = 0.5,
             angle = 90, fontface = "plain", vjust = 1.75)


# Figure 6.3 Asset and Portfolio Kurtosis Comparison ----

asset_returns_kurtosis_tbl <- asset_returns_long_tbl %>%

    # kurtosis for each asset
    summarise(kt = kurtosis(returns)) %>%

    # kurtosis of portfolio
    add_row(tibble(asset = "Portfolio",
                   kt = kurtosis(portfolio_returns_rebalanced_monthly_tbl$returns)))

asset_returns_kurtosis_tbl %>%

    ggplot(aes(asset, kt)) +
    geom_point() +

    # Add label
    ggrepel::geom_text_repel(aes(label = asset), color = "cornflowerblue", size = 5,
                             data = asset_returns_kurtosis_tbl %>%
                                 filter(asset == "Portfolio")) +

    labs(y = "kurtosis")


# 3 Rolling kurtosis ----

# Assign a value to winder
window <- 24

port_rolling_kurtosis_tbl <- portfolio_returns_rebalanced_monthly_tbl %>%

    tq_mutate(select = returns,
              mutate_fun = rollapply,
              width      = window,
              FUN        = kurtosis,
              col_rename = "rolling_kurtosis") %>%
    select(date, rolling_kurtosis) %>%
    na.omit()

# Figure 6.5 Rolling kurtosis ggplot ----

port_rolling_kurtosis_tbl %>%

    ggplot(aes(date, rolling_kurtosis)) +
    geom_line(color = "cornflowerblue") +

    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_date(breaks = scales::breaks_pretty(n = 7)) +

    labs(title = paste0("Rolling ", window, "-Month Kurtosis"),
         x = NULL,
         y = "kurtosis") +
    theme(plot.title = element_text(hjust = 0.5)) +

    annotate(geom = "text",
             x = as.Date("2016-12-01"), y = 3,
             color = "red", size = 5,
             label = str_glue("The risk level skyrocketed at the end of the period
                              with the 24-month kurtosis rising above three."))





