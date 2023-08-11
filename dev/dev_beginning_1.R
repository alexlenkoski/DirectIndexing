rm(list = ls())

library(data.table)
library(quantmod)

return_dataset = function(v){
    getSymbols(v)
    dt = data.table(get(v))
    dt[, date:= as.Date(time(get(v)))]
    dt[, year:= year(date)]
    dt[, month:= month(date)]
    dt[, mday:= mday(date)]
    dt[, P := get(paste0(v,".Adjusted"))]
    return(dt)
}

v_follow = c("AAPL", "GE")

dt_v = list()
for(j in 1:length(v_follow)){
    dt_v[[j]] = return_dataset(v_follow[j])
}

dt_p = merge(dt_v[[1]][,.(date, "AAPL" = P)],
             dt_v[[2]][,.(date, "GE" = P)],
             by = c("date"))

dt_p[, r_aapl := log(AAPL) - log(shift(AAPL,1,NA,"lag"))]
dt_p[, r_ge := log(GE) - log(shift(GE, 1, NA, "lag"))]

dt_spy = return_dataset("OEF")

dt_p_spy = merge(dt_p,
                 dt_spy[,.(date, "SPY" = P)],
                 by = "date")
dt_p_spy[, r_spy := log(SPY) - log(shift(SPY, 1, NA, "lag"))]
