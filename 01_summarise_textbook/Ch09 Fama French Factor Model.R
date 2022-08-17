## CHAPTER 9 Portfolio Theory: Fama French Factor Model ----

## Goal ----
# We extend CAPM by regressing portfolio returns on several variables, in addition to market returns.

# 1. market returns
# 2. firm size
# 3. firm value, a firm's book-to-market ratio

# With the firm value in the equation, we investigate how much of the returns are the result of
# including stocks with a high book-to-market ratio.

## Set up ----

# Core
library(tidyverse)
library(tidyquant)
library(readr)

# Time series
library(lubridate)
library(tibbletime)

# modeling
library(broom)


# 1 Import stock prices ----

asset_returns_long_tbl <- read_rds("00_data/wrangled_data/Ch02_asset_returns_long_tbl.rds")

portfolio_returns_tbl <-
    read_rds("00_data/wrangled_data/Ch03_portfolio_returns_rebalanced_monthly_tbl.rds")

source("00_scripts/convert_prices_to_returns.R")

# 2 Importing and wrangling Fama-French data ----
## 2.1 Download from the web using the code ----

## FF website
# http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html

# Create a temporary file, which delete itself upon exiting the R session
temp <- tempfile()

# Create the URL

base <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
factor <- "F-F_Research_Data_Factors"
format <- "_CSV.zip"

full_url <- paste0(base, factor, format)

# Download data into the temp file
download.file(full_url, temp)

# Unzip, read the data, and save it
# The textbook uses unz() inside the read_csv but is no longer necessary
Global_3_Factors_raw_tbl <- readr::read_csv(temp, skip = 3)

## Alternative of importing FF data
# Save the file in the folder and load it
# Downloaded on 5/14/2022

# Global_3_Factors_raw_tbl <- readr::read_csv("00_data/raw_data/F-F_Research_Data_Factors.CSV", skip = 3)

## 2.2 date ----

Global_3_Factors_tbl <- Global_3_Factors_raw_tbl %>%

    # Convert date
    mutate(date = ymd(parse_date_time(...1, "%Y%m"))) %>%

    # remove NAs, annual values
    filter(!is.na(date)) %>%
    select(date, everything(), -...1) %>%

    # Convert date to the last day of the month
    mutate(date = rollforward(date))

Global_3_Factors_tbl

## 2.3 Merge with portfolio returns ----

ff_portfolio_returns_tbl <- portfolio_returns_tbl %>%

    # Move all dates to the last day of the month
    mutate(date = rollforward(date)) %>%

    left_join(Global_3_Factors_tbl, by = "date") %>%

    # Convert percent to decimal
    mutate(MKT_RF = `Mkt-RF` / 100,
           SMB = SMB / 100,
           HML = HML / 100,
           RF = RF / 100,
           R_excess = round(returns - RF, 4)) %>%
    select(date, R_excess, MKT_RF, SMB, HML)

ff_portfolio_returns_tbl

## 2.4 Modeling ----

ff_model_tbl <- ff_portfolio_returns_tbl %>%

    lm(R_excess ~ MKT_RF + SMB + HML, data = .) %>%

    broom::tidy(conf.int = TRUE, conf.level = .95) %>%

    mutate(across(where(is.numeric), ~round(., 3))) %>%

    select(term, beta = estimate, p.value, conf.low, conf.high)

ff_model_tbl

# 3 Visualizing Fama-French ----

## Figure 9.1 Fama-French factor betas ----

ff_model_tbl %>%

    filter(term != "(Intercept)") %>%

    ggplot() +
    geom_errorbar(aes(x = term, y = beta,
                      ymin = conf.low, ymax = conf.high,
                      color = term),
                  show.legend = FALSE) +

    labs(title    = "FF 3-Factor Coefficients",
         subtitle = "balanced portfolio",
         caption  = "data source: Fama-French website",
         x        = NULL,
         y        = "coefficient") +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption  = element_text(hjust = 0))


# 4 Rolling Fama-French ----

# Define a rolling function
window <- 24

calculaterolling_lm <- rollify(.f = function(R_excess, MKT_RF, SMB, HML) lm(R_excess ~ MKT_RF + SMB + HML),
                      window = window, unlist = FALSE)

# Calculate rolling FF
rolling_ff_lm_tbl <- ff_portfolio_returns_tbl %>%

    # Calculate rolling FF
    mutate(rolling_ff = calculaterolling_lm(R_excess, MKT_RF, SMB, HML)) %>%

    # Remove the first 23 rows
    filter(!is.na(rolling_ff)) %>%

    select(date, rolling_ff)

# Extract betas
rolling_ff_betas_tbl <- rolling_ff_lm_tbl %>%

    # Extract coefficients from lm object
    mutate(tidied = map(.x = rolling_ff, .f = ~broom::tidy(., conf.int = TRUE))) %>%
    unnest(tidied) %>%
    filter(term != "(Intercept)") %>%
    select(date, factor = term, beta = estimate, conf.low, conf.high)

rolling_ff_betas_tbl

# Extract R-squared
rolling_ff_rsquared_tbl <- rolling_ff_lm_tbl %>%

    # Extract R-squared from lm object
    mutate(glanced = map(.x = rolling_ff, .f = broom::glance)) %>%
    unnest(glanced) %>%
    select(date, r.squared, adj.r.squared, p.value) %>%
    mutate(r.squared = r.squared %>% round(3))

rolling_ff_rsquared_tbl

# 5 Visualizing Rolling Fama-French ----

## Figure 9.2 Rolling Factor Betas ----
rolling_ff_betas_tbl%>%

    ggplot(aes(date, beta, color = factor)) +
    geom_line() +

    labs(title = paste0(window, "-Month Rolling FF Factor Betas"),
         x = NULL) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90))

## Figure 9.3 Rolling FF R-squared ----
rolling_ff_rsquared_tbl %>%

    ggplot(aes(date, r.squared)) +
    geom_line(color = "cornflowerblue") +

    scale_x_date(breaks = scales::pretty_breaks(n = 7)) +

    labs(title = "Rolling FF 3-FActor R-Squared",
         x = NULL,
         y = "R-squared") +
    theme(plot.title = element_text(hjust = 0.5))
