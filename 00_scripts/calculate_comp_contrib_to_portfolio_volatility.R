calculate_component_contribution <-
function(asset_returns_wide_tbl, w) {

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
calculate_comp_contrib_by_window <-
function(asset_returns_wide_tbl,
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
