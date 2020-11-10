# ------------------------------------------------------------------------------------------
# Function for plotting one globtemp.R dataframe using ggplot2 geom_point()
# 
# November 2020
# lineb@met.no
# 
# ------------------------------------------------------------------------------------------

plot_singular_bars <- function(p, df_val, df_y, df_col, new_mean, df_name, last_data_point) {
    p <- p +
         labs(subtitle = paste0(df_name,"\n (last data ", last_data_point, ")")) +
         geom_col(aes(y = df_val - new_mean, x = df_y), width = 0.8, fill = df_col)
    return(p)
  }

