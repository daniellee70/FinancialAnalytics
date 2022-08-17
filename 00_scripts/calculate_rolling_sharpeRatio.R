calculate_rolling_sharpeRatio <-
function(df) {

    SharpeRatio(df,
                Rf = rfr,
                FUN = "StdDev")

}
