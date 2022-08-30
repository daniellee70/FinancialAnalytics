# TESTINg

## CHAPTER 2 Returns: ASSET PRICES TO RETURNS ----

## Goal ----
# Take raw prices of five individual stocks and transform them into monthly returns

## Set up ----

# Core
library(tidyverse)
library(tidyquant)

# Import Excel files
library(readxl)

# time series
library(timetk)


## 1 Import stock prices ----

## Choose stocks

symbols <- c("SPY", "EFA", "IJS", "EEM", "AGG")

### Using tq_get() ----
# prices <- tq_get(x = symbols,
#                  get = "stock.prices",
#                  from = "2012-12-31",
#                  to = "2017-12-31")

### Using a flat file ----
prices <- read_excel("00_data/prices_fiveStocks.xlsx",
                     col_types = c("text", "numeric",
                                   "numeric", "numeric",
                                   "numeric", "numeric")) %>%
    mutate(date = ymd(date)) %>%

    # Convert data to a long form
    pivot_longer(cols = -date, names_to = "asset", values_to = "prices")

## 2 Convert prices to returns ----

asset_returns_tbl <- prices %>%

    # Calculate monthly returns
    group_by(asset) %>%
    tq_transmute(select = prices,
                 mutate_fun = periodReturn,
                 period = "monthly",
                 type = "log") %>%
    slice(-1) %>%
    ungroup() %>%

    # remane
    set_names(c("asset", "date", "returns"))

# period_returns = c("yearly", "quarterly", "monthly", "weekly")

convert_prices_to_returns <- function(data, period_returns) {

    asset_returns_tbl <- data %>%

        # Calculate monthly returns
        group_by(asset) %>%
        tq_transmute(select = prices,
                     mutate_fun = periodReturn,
                     period = period_returns,
                     type = "log") %>%
        slice(-1) %>%
        ungroup() %>%

        # remane
        set_names(c("asset", "date", "returns"))

    return(asset_returns_tbl)

}

asset_returns_long_tbl <- convert_prices_to_returns(prices, period_returns = "monthly")

asset_returns_long_tbl

dump("convert_prices_to_returns", file = "00_scripts/convert_prices_to_returns.R")

# Save data
write_rds(asset_returns_long_tbl, "00_data/Ch02_asset_returns_long_tbl.rds")


## 3 Make plot ----


# Plot
asset_returns_long_tbl %>%

    ggplot(aes(x = returns)) +
    geom_density(aes(col = asset), alpha = 1) +
    geom_histogram(aes(fill = asset), alpha = 0.45, binwidth = 0.01) +
    facet_wrap(~asset) +
    guides(fill = "none") +

    labs(title = "Monthly Returns since 2013",
         x = "distribution",
         y = "monthly returns") +
    theme_update(plot.title = element_text(hjust = 0.5))








