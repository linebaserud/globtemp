# ------------------------------------------------------------------------------------------
# Function for plotting one globtemp.R dataframe using ggplot2 geom_point()
# 
# November 2020
# lineb@met.no
# 
# ------------------------------------------------------------------------------------------

plot_plural_lines <- function(p, df_val, df_y, new_mean, line_color, df_name, last_data_point, year_min, year_max, text_pos_offset, max_all) {
    p <- p +
         geom_line(aes(y = df_val - new_mean, x = df_y), size = 1.5, linetype = "solid", color = line_color, na.rm = TRUE) +
         annotate("text", x = year_min + (year_max - year_min)/2 - text_pos_offset, y = (ceiling(max_all / 0.5) * 0.5) - 0.05,
                          label = df_name, color = line_color, size = 6) +
         annotate("text", x = year_min + (year_max - year_min)/2 - text_pos_offset, y = (ceiling(max_all / 0.5) * 0.5) - 0.15,
                          label = paste0("(last data ", last_data_point, ")"), color = line_color, size = 4)
    return(p)
  }

