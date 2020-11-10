# ------------------------------------------------------------------------------------------
# Function for printing list of top 5 max values from globtemp.R dataframe 
# The values are rounded using round(val, 2)
# 
# November 2020
# lineb@met.no
# 
# ------------------------------------------------------------------------------------------

top5 <- function(period, refs, refe, df_val, df_y, new_mean, df_name){
    cat(paste0("---- ", period, " top 5 ", df_name, " (relative to ", refs, "-", refe, ") ----\n "))
    cat(paste0(c(1:5), ": ", format(round(rev(tail(df_val[order(df_val)], n = 5)) - new_mean, 2), nsmall = 2), " (", rev(tail(df_y[order(df_val)], n = 5)), ")\n"))
  }

