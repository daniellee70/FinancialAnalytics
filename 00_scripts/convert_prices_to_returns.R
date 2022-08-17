convert_prices_to_returns <-
function(data, period_returns) {

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
