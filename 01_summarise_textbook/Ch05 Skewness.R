## CHAPTER 5 Risk: Skewness ----

## Goal ----
# Measure portfolio risk using skewness.
# Skewness is the extent to which returns are asymmetric around the mean.
# It is important because a positively skewed distribution means large positive returns
# are more likely while a negatively skewed distribution implies large negative returns
# are more likely.

## Set up ----

# Core
library(tidyverse)
library(tidyquant)



# 1 Import stock prices ----

asset_returns_long_tbl <- read_rds("00_data/Ch02_asset_returns_long_tbl.rds")

portfolio_returns_rebalanced_monthly_tbl <-
    read_rds("00_data/Ch03_portfolio_returns_rebalanced_monthly_tbl.rds")

# 2 Skewness ----

portfolio_returns_rebalanced_monthly_tbl %>%

    tq_performance(Ra = returns,
                   Rb = NULL,
                   performance_fun = table.Stats) %>%
    select(Skewness)


# Figure 5.1 Returns histogram ----

portfolio_returns_rebalanced_monthly_tbl %>%

    ggplot(aes(returns)) +
    geom_histogram(alpha = 0.7,
                   binwidth = .003,
                   fill = "cornflowerblue",
                   color = "cornflowerblue")

# Figure 5.2 Shaded histogram returns ----

portfolio_returns_rebalanced_monthly_tbl %>%

    # Create a new variable for shade
    mutate(returns_extreme_neg = if_else(returns < mean(returns) - 2*sd(returns),
                                   "yes",
                                   "no")) %>%

    # Plot
    ggplot(aes(returns, fill = returns_extreme_neg)) +
    geom_histogram(alpha = .7,
                   binwidth = .003) +

    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    scale_fill_tq() +

    labs(x = "monthly returns")


# Figure 5.3 Density plot skewness ----

portfolio_density_plot <- portfolio_returns_rebalanced_monthly_tbl %>%

    ggplot(aes(returns)) +
    geom_density(color = "cornflowerblue")

portfolio_density_plot

write_rds(portfolio_density_plot, "00_data/fig/Ch05_portfolio_density_plot.rds")

# Figure 5.4 Density plot with shaded area ----

shaded_area_data <- ggplot_build(g)$data[[1]] %>%

    filter(x < mean(portfolio_returns_rebalanced_monthly_tbl$returns))

portfolio_density_plot + geom_area(aes(x = x, y = y),
              fill = "pink",
              alpha = 0.5,
              data = shaded_area_data)


# Figure 5.5 Density plot with shaded lines ----

mean <- mean(portfolio_returns_rebalanced_monthly_tbl$returns)
median <- median(portfolio_returns_rebalanced_monthly_tbl$returns)

median_line_data <- ggplot_build(g)$data[[1]] %>%

    filter(x < median)

portfolio_density_plot + geom_area(aes(x = x, y = y),
              fill = "pink",
              alpha = 0.5,
              data = shaded_area_data) +

    # Insert a line for mean
    geom_segment(aes(x = mean, y = 0,
                     xend = mean, yend = density),
                 linetype = "dotted", size = 1, color = "red",
                 data = shaded_area_data) +
    annotate(geom = "text",
             x = mean, y = 5,
             label = "mean", color = "red", fontface = "plain",
             angle = 90, alpha = 0.8, vjust = -1.75) +

    # Insert a line for median
    geom_segment(aes(x = median, y = 0,
                     xend = median, yend = density),
                 linetype = "dotted", size = 1, color = "black",
                 data = median_line_data) +
    annotate(geom = "text",
             x = median, y = 5,
             label = "median", color = "black", fontface = "plain",
             angle = 90, alpha = 0.8, vjust = 1.75) +

    labs(title = "Density Plot Illustrating Skewness",
         y = "Density") +
    theme(plot.title = element_text(hjust = 0.5))


# Figure 5.6 Asset and portfolio skewness comparison ----

asset_returns_skew_tbl <- asset_returns_long_tbl %>%

    # skewness for each asset
    group_by(asset) %>%
    summarise(skew = skewness(returns)) %>%
    ungroup() %>%

    # skewness of portfolio
    add_row(tibble(asset = "Portfolio",
                  skew = skewness(portfolio_returns_rebalanced_monthly_tbl$returns)))

asset_returns_skew_tbl %>%

    ggplot(aes(asset, skew, color = asset)) +
    geom_point() +

    # Add label for portfolio
    ggrepel::geom_text_repel(aes(label = asset),
                             data = asset_returns_skew_tbl %>%
                                 filter(asset == "Portfolio"),
                             size = 5,
                             show.legend = FALSE) +
    labs(y = "skewness")


# 3 Rolling skewness ----
# Why rolling sd?
# To check anything unusual in the portfolio's historical risk

# Assign a value to winder
window <- 24

port_rolling_sd_tbl <- portfolio_returns_rebalanced_monthly_tbl %>%

    tq_mutate(select = returns,
              mutate_fun = rollapply,
              width      = window,
              FUN        = skewness,
              col_rename = "rolling_skew") %>%
    select(date, rolling_skew) %>%
    na.omit()

# Figure 4.8 Rolling skewness ggplot ----

port_rolling_sd_tbl %>%

    ggplot(aes(date, rolling_skew)) +
    geom_line(color = "cornflowerblue") +
    geom_hline(yintercept = 0, linetype = "dotted", size = 2) +

    scale_y_continuous(limits = c(-1,1),
                       breaks = scales::pretty_breaks(n = 10)) +
    scale_x_date(breaks = scales::breaks_pretty(n = 7))+

    labs(title = paste0("Rolling ", window, "-Month Skew"),
         x = NULL,
         y = "skewness") +
    theme(plot.title = element_text(hjust = 0.5)) +

    annotate(geom = "text",
             x = as.Date("2016-09-01"), y = 0.7,
             color = "red", size = 5,
             label = str_glue("The 24-month skewness is positive for about half of the lifetime,
                              even though the overall skewness is negative"))





