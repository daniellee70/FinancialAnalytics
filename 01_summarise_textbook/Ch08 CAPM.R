## CHAPTER 8 Portfolio Theory: CAPM ----

## Goal ----


## Set up ----

# Core
library(tidyverse)
library(tidyquant)


# 1 Import stock prices ----

asset_returns_long_tbl <- read_rds("00_data/Ch02_asset_returns_long_tbl.rds")

portfolio_returns_tbl <-
    read_rds("00_data/Ch03_portfolio_returns_rebalanced_monthly_tbl.rds")

source("00_scripts/convert_prices_to_returns.R")

# 2 CAPM and market returns ----

# Get market returns
market_returns_tbl <- tq_get("SPY",
                             get = "stock.prices",
                             from = "2012-12-31",
                             to = "2017-12-31") %>%
    select(date, asset = symbol, prices = adjusted) %>%

    # Convert prices to returns
    convert_prices_to_returns(period_returns = "monthly")

# Combine market returns with portfolio returns
portfolio_market_returns_tbl <- portfolio_returns_tbl %>%

    # Add market returns
    mutate(market_returns = market_returns_tbl %>% pull(returns))

# 3 Calculating CAPM Beta ----

# A complete list of functions for performance_fun()
# tq_performance_fun_options()

portfolio_market_returns_tbl %>%

    tq_performance(Ra = returns,
                   Rb = market_returns,
                   performance_fun = CAPM.beta)


# 4 Visualizing CAPM ----

# Figure 8.1 Scatter portfolio v. Market ----

scatter_market_portfolio_fig <- portfolio_market_returns_tbl %>%

    ggplot(aes(market_returns, returns)) +
    geom_point(color = "cornflowerblue") +

    labs(x = "market returns",
         y = "portfolio returns")

scatter_market_portfolio_fig

# Figure 8.2 Scatter with regression line from ggplot ----

scatter_regLine_fig <- scatter_market_portfolio_fig +

    geom_smooth(method = "lm", se = FALSE,
                size = 1.5, color = tidyquant::palette_light()[3])

scatter_regLine_fig

# Figure 8.3 Scatter with regression line from Beta estimate ----



# Figure 8.4 Scatter with both regression lines ----



# Figure 8.5 Actual versus fitted returns ----

portfolio_market_returns_tbl %>%

    # Run regression
    lm(returns ~ market_returns, data = .) %>%

    # Get fitted
    broom::augment() %>%

    # Add date %>%
    mutate(date = portfolio_market_returns_tbl$date) %>%

    # Transform data to long format
    pivot_longer(cols = c(returns, .fitted),
                 names_to = "type",
                 values_to = "returns") %>%

    # Plot
    ggplot(aes(date, returns, color = type)) +
    geom_line()



portfolio_market_returns_tbl %>%

    # Run regression
    lm(returns ~ market_returns, data = .) %>%

    # Get fitted
    broom::augment() %>%

    # Plot
    ggplot(aes(returns, .fitted)) +
    geom_point() +

    labs(x = "actual returns",
         y = "fitted returns")
