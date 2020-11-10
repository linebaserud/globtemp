# ------------------------------------------------------------------------------------------
# Function for plotting one globtemp.R dataframe using ggplot2 geom_point()
# 
# November 2020
# lineb@met.no
# 
# ------------------------------------------------------------------------------------------

plot_singular_points <- function(p, df_val, df_y, df_col, new_mean, df_name, last_data_point) {
    p <- p +
         labs(subtitle = paste0(df_name,"\n (last data ", last_data_point, ")")) +
         geom_line(aes(y = df_val - new_mean, x = df_y), size = 1, linetype = "solid", color = "black") +
         geom_point(aes(y = df_val - new_mean, x = df_y), pch = 16, size = 4, color = df_col)
    return(p)
  }




