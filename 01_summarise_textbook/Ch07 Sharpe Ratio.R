## CHAPTER 7 Portfolio Theory: Sharpe Ratio ----

## Goal ----
# Measure which portfolio is expected to yield a higher return adjusted for risk using the Sharpe Ratio.
# The Sharpe Ratio is the mean of the excess portfolio returns (returns - risk free rate)
# divided by the standard deviation of the excess returns.

## Set up ----

# Core
library(tidyverse)
library(tidyquant)


# 1 Import stock prices ----

asset_returns_long_tbl <- read_rds("00_data/Ch02_asset_returns_long_tbl.rds")

portfolio_returns_rebalanced_monthly_tbl <-
    read_rds("00_data/Ch03_portfolio_returns_rebalanced_monthly_tbl.rds")

source("00_scripts/convert_prices_to_returns.R")

# 2 Sharpe Ratio ----

# Risk free rate
rfr <- 0.0003

portfolio_sharpe_tbl <- portfolio_returns_rebalanced_monthly_tbl %>%

    tq_performance(Ra = returns,
                   Rf = rfr,
                   performance_fun = SharpeRatio,
                   FUN = "StdDev")

portfolio_sharpe_tbl

# 3 Visualize Sharpe Ratio ----


# Figure 7.1 Scatter Returns around Risk Free Rate ----

portfolio_returns_rebalanced_monthly_tbl %>%

    # Transform data
    mutate(returns_excess = if_else(returns > rfr, "above_rfr", "below_rfr")) %>%

    ggplot(aes(date, returns, color = returns_excess)) +
    geom_point(show.legend = FALSE) +

    # risk free rate
    geom_hline(yintercept = rfr, linetype = "dotted", size = 1, color = "cornflowerblue") +

    # election date
    geom_vline(xintercept = as.Date("2016-11-30"), size = 1, color = "cornflowerblue") +

    # formatting
    scale_x_date(breaks = scales::pretty_breaks(n = 7)) +

    # labeling
    annotate(geom = "text",
             x = as.Date("2017-01-01"), y = -0.04,
             label = "Election", angle = 90, size = 5) +
    annotate(geom = "text",
             x = as.Date("2017-06-01"), y = -0.01,
             label = str_glue("No returns below the RFR
                              after the 2016 election"),
             color = "red", size = 5) +
    labs(y = "percent monthly returns",
         x = NULL)

# Figure 7.2 Returns Histogram with Risk-Free Rate ggplot ----

portfolio_returns_rebalanced_monthly_tbl %>%

    ggplot(aes(returns)) +
    geom_histogram(binwidth = 0.01, fill = "cornflowerblue", alpha = 0.5) +
    geom_vline(xintercept = rfr, color = "green", size = 1) +

    annotate(geom= "text",
             x = rfr + 0.002, y = 13,
             label = "risk free rate", angle = 90, size = 5) +
    labs(y = "count")

# Figure 7.3 Sharpe versus Standard Deviation ----

sharpe_sd_tbl <- asset_returns_long_tbl %>%

    summarise(sharpe = mean(returns - rfr) / sd(returns - rfr),
              sd     = sd(returns - rfr)) %>%

    # Add portfolio
    add_row(tibble(asset = "Portfolio",
                   sharpe = portfolio_sharpe_tbl[1] %>% pull(),
                   sd     = sd(portfolio_returns_rebalanced_monthly_tbl$returns)))

sharpe_sd_tbl %>%

    # Plot
    ggplot(aes(sd, sharpe, color = asset)) +
    geom_point() +

    ggrepel::geom_text_repel(aes(label = asset), size = 5,
                             data = sharpe_sd_tbl %>%
                                 filter(asset == "Portfolio"),
                             show.legend = FALSE) +

    labs(y = "Sharpe Ratio",
         x = "standard deviation")


# 4 Rolling Sharpe Ratio ----

# Custom function
# necessary because we would not be able to specify FUN = "StdDev" otherwise

calculate_rolling_sharpeRatio <- function(df) {

    SharpeRatio(df,
                Rf = rfr,
                FUN = "StdDev")

}

dump(list = "calculate_rolling_sharpeRatio",
     file = "00_scripts/calculate_rolling_sharpeRatio.R")

# Set the length of periods for rolling calculation
window <- 24

# Calculate rolling sharpe ratios
rolling_sharpe_tbl <- portfolio_returns_rebalanced_monthly_tbl %>%

    tq_mutate(select = returns,
              mutate_fun = rollapply,
              width = window,
              align = "right",
              FUN = calculate_rolling_sharpeRatio,
              col_rename = "sharpeRatio") %>%
    na.omit()

rolling_sharpe_tbl


# Figure 7.5 Rolling Sharpe ggplot ----

rolling_sharpe_tbl %>%

    ggplot(aes(date, sharpeRatio)) +
    geom_line(color = "cornflowerblue") +

    labs(title = paste0("Rolling ", window, "-Month Sharpe Ratio"),
         y = "rolling Sharpe Ratio",
         x = NULL) +
    theme(plot.title = element_text(hjust = 0.5)) +

    annotate(geom = "text",
             x = as.Date("2016-06-01"), y = 0.5,
             label = "This portfolio has done quite well since 2016.",
             size = 5, color = "red")


# For Shiny ----
# Rolling Sharpe Ratio for S&P500

rolling_sharpe_tbl_SPY <- tq_get("SPY",
                                 get = "stock.prices",
                                 from = "2012-12-31") %>%
    select(date, asset = symbol, prices = adjusted) %>%

    # Convert prices to returns
    convert_prices_to_returns(period_returns = "monthly") %>%

    # Sharpe Ratio
    tq_mutate(select = returns,
              mutate_fun = rollapply,
              width = window,
              align = "right",
              FUN = calculate_rolling_sharpeRatio,
              col_rename = "sharpeRatio") %>%
    na.omit()

rolling_sharpe_tbl_SPY


rolling_sharpe_tbl_all <- rolling_sharpe_tbl %>%
    mutate(asset = "Portfolio") %>%

    bind_rows(rolling_sharpe_tbl_SPY)


rolling_sharpe_tbl_all %>%

    ggplot(aes(date, sharpeRatio, color = asset)) +
    geom_line() +

    labs(title = paste0("Rolling ", window, "-Month Sharpe Ratio"),
         y = "rolling Sharpe Ratio",
         x = NULL,
         color = NULL) +
    theme(plot.title = element_text(hjust = 0.5))

# Calculate Sharpe Ratio of portfolio
portfolio_sharpe_tbl


# Calculate Sharpe Ratio of S&P500
market_sharpe_tbl <- tq_get("SPY") %>%
    select(date, asset = symbol, prices = adjusted) %>%

    # Convert prices to returns
    convert_prices_to_returns(period_returns = "monthly") %>%

    # Calculate Sharpe Ratio
    tq_performance(Ra = returns,
                   Rf = rfr,
                   performance_fun = SharpeRatio,
                   FUN = "StdDev")

market_sharpe_tbl


