############################################################################################################
#
# Function for plotting timeseries of global temperature anomalies 
# (with option for adjusting the reference period)
# 
# February/March 2020
# lineb@met.no
#
############################################################################################################

# load libraries and functions ----------------------------------------------------------------

rm(list = ls())

suppressPackageStartupMessages(library('lubridate'))
suppressPackageStartupMessages(library('ggplot2'))

fun_list <- c("anom2anom.R",
              "distr_col.R",
              "readCopernicus.R",
              "readHadCRUT.R",
              "readNASA.R")
for (fun in fun_list) {source(paste0("functions/", fun))}

# main function -------------------------------------------------------------------------------

globtemp <- function(datasets, refs, refe, period, plot_type, save_option, save_name){

  # get data ----------------------------------------------------------------------------------
  
  filename_nasa <- NA  
  filename_copernicus <- NA   
  filename_hadcrut_yearly <- NA   
  filename_hadcrut_monthly <- NA  

  y1 <- data.frame()
  year_max <- data.frame()
  max_all <- data.frame()
  min_all <- data.frame()

  # NASA/GISS
  if (!is.na(match('NASA', datasets))) {
    out <- readNASA(filename_nasa, period) 
    D <- out[[1]]
    refs_nasa <- 1951
    refe_nasa <- 1980
    y1_nasa <- 1880
    textNASA_short <- "NASA/GISS"
    textNASA <- "NASA/GISS GISTEMP v4 (NOAA GHCN v4 and ERSST v5)"
    if (refs_nasa == refs & refe_nasa == refe) {m_new <- 0}                           # if ref period equal to original...
    if (refs_nasa != refs | refe_nasa != refe) {m_new <- anom2anom(D, refs, refe)}    # ...else calculate offset
    D <- cbind(D, distr_col(D, m_new))                                                # add column with colors for plotting
    y1 <- rbind(y1, y1_nasa)
    year_max <- rbind(year_max, tail(D$y, n = 1))  
    max_all <- rbind(max_all, max(D$val - m_new, na.rm = TRUE))
    min_all <- rbind(min_all, min(D$val - m_new, na.rm = TRUE))
  }

  # Copernicus
  if (!is.na(match('Copernicus', datasets))) {
    out2 <- readCopernicus(filename_copernicus, period)
    D2 <- out2[[1]]
    colnames(D2) <- c("y", "val")
    refs_era5 <- 1981
    refe_era5 <- 2010
    y1_era5 <- 1979 
    textCopernicus_short <- "ERA5 Copernicus"
    textCopernicus <- "ERA5 Copernicus Climate Change Service/ECMWF "
    if (y1_era5 > refs) {textCopernicus <- "** WARNING **:  ERA5 data do not cover entire reference period"} 
    if (refs_era5 == refs & refe_era5 == refe){m2_new <- 0}                            # if ref period equal to original...
    if (refs_era5 != refs | refe_era5 != refe){m2_new <- anom2anom(D2, refs, refe)}    # ...else calculate offset
    D2 <- cbind(D2, distr_col(D2, m2_new))                                         # add column with colors for plotting
    y1 <- rbind(y1, ceiling(y1_era5 / 10) * 10)
    year_max <- rbind(year_max, tail(D2$y, n = 1))  
    max_all <- rbind(max_all, max(D2$val - m2_new, na.rm = TRUE)) 
    min_all <- rbind(min_all, min(D2$val - m2_new, na.rm = TRUE)) 
  }

  # HadCRUT4
  if (!is.na(match('HadCRUT', datasets))){
    if (period != 'Yearly') {out3 <- readHadCRUT(filename_hadcrut_monthly, period)}
    if (period == 'Yearly') {out3 <- readHadCRUT(filename_hadcrut_yearly, period)}
    D3 <- out3[[1]]; 
    refs_hadcrut <- 1961
    refe_hadcrut <- 1990
    y1_hadcrut <- 1850
    textHadCRUT_short <- "HadCRUT4"
    textHadCRUT <- "HadCRUT4: CRUTEM4 surface air temperature + HadSST3 sea-surface temperature"
    if (refs_hadcrut == refs & refe_hadcrut == refe) {m3_new <- 0}                           # if chosen ref period equal to original...
    if (refs_hadcrut != refs | refe_hadcrut != refe) {m3_new <- anom2anom(D3, refs, refe)}   # ...else calculate offset
    D3 <- cbind(D3, distr_col(D3, m3_new))                                         # add column with colors for plotting
    y1 <- rbind(y1, y1_hadcrut)
    year_max <- rbind(year_max, tail(D3$y, n = 1))  
    max_all <- rbind(max_all, max(D3$val - m3_new, na.rm = TRUE))
    min_all <- rbind(min_all, min(D3$val - m3_new, na.rm = TRUE))
  }  

  y1 <- min(y1)             # adaptive scaling x-axis min
  year_max <- max(year_max) # adaptive scaling x-axis max
  max_all <- max(max_all)   # adaptive scaling y-axis max
  min_all <- min(min_all)   # adaptive scaling y-axis min

  # functions for plotting ----------------------------------------------------------------------

  top5 <- function(period, df_val, df_y, new_mean, df_name){
    cat(paste0("---- ", period, " top 5 ", df_name, " (relative to ", refs, "-", refe, ") ----\n "))
    cat(paste0(c(1:5), ": ", format(round(rev(tail(df_val[order(df_val)], n = 5)) - new_mean, 2), nsmall = 2), " (", rev(tail(df_y[order(df_val)], n = 5)), ")\n")) 
  }

  plot_singular_points <- function(p, df_val, df_y, df_col, new_mean, df_name, last_data_point) {
    p <- p +
         labs(subtitle = paste0(df_name,"\n (last data ", last_data_point, ")")) +
         geom_line(aes(y = df_val - new_mean, x = df_y), size = 1, linetype = "solid", color = "black") +
         geom_point(aes(y = df_val - new_mean, x = df_y), pch = 16, size = 4, color = df_col)
    return(p)
  }

  plot_singular_bars <- function(p, df_val, df_y, df_col, new_mean, df_name, last_data_point) {
    p <- p +
         labs(subtitle = paste0(df_name,"\n (last data ", last_data_point, ")")) +
         geom_col(aes(y = df_val - new_mean, x = df_y), width = 0.8, fill = df_col)
    return(p)
  }

  plot_plural_lines <- function(p, df_val, df_y, new_mean, line_color, df_name, last_data_point, text_pos_offset) {
    p <- p +
         geom_line(aes(y = df_val - new_mean, x = df_y), size = 1.5, linetype = "solid", color = line_color, na.rm = TRUE) +
         annotate("text", x = y1 + (year_max - y1)/2 - text_pos_offset, y = (ceiling(max_all / 0.5) * 0.5) - 0.05, 
                          label = df_name, color = line_color, size = 6) +
         annotate("text", x = y1 + (year_max - y1)/2 - text_pos_offset, y = (ceiling(max_all / 0.5) * 0.5) - 0.15, 
                          label = paste0("(last data ", last_data_point, ")"), color = line_color, size = 4)
    return(p)
  }

  # plot data ----------------------------------------------------------------------------------

  cat("-----------------------------------------------------------------------------------------------------------------------------\n\n")

  p <- ggplot() +
       xlab("") +
       ylab("") +
       geom_hline(yintercept = 0, color = "grey") +
       geom_vline(xintercept = 2020, color = "grey") +
       geom_segment(aes(x = refs, xend = refe, y = 0, yend = 0), color = "grey60", size = 3) + 
       labs(title = paste(period, "global temperature anomalies (°C)  relative to", refs, "-", refe))

  # NASA
  # plot as single dataset
  if (!is.na(match('NASA', datasets)) & length(datasets) == 1) {
    if (plot_type == 'points') p <- plot_singular_points(p, D$val, D$y, D$col, m_new, textNASA, out[2]) 
    if (plot_type == 'bars')   p <- plot_singular_bars(p, D$val, D$y, D$col, m_new, textNASA, out[2])
    if (plot_type != 'bars' & plot_type != 'points') cat(paste0('** WARNING **: set plot_type to "points" or "bars" \n\n'))
    top5(period, D$val, D$y, m_new, textNASA_short)
  }
  # plot as part of comparison several datasets
  if (!is.na(match('NASA', datasets)) & length(datasets) > 1){
    p <- plot_plural_lines(p, D$val, D$y, m_new, "orange", textNASA_short, out[2], 45) 
    top5(period, D$val, D$y, m_new, textNASA_short) 
  }

  # Copernicus
  # plot as single dataset
  if (!is.na(match('Copernicus', datasets)) & length(datasets) == 1){
    # check if dataset covers reference period, plot or print warning
    if (refs >= y1_era5){
      if (plot_type == 'points')  p <- plot_singular_points(p, D2$val, D2$y, D2$col, m2_new, textCopernicus, out2[2]) 
      if (plot_type == 'bars')    p <- plot_singular_bars(p, D2$val, D2$y, D2$col, m2_new, textCopernicus, out2[2]) 
      if (plot_type != 'bars' & plot_type != 'points') cat(paste0('** WARNING **: set plot_type to "points" or "bars" \n\n'))
      top5(period, D2$val, D2$y, m2_new, textCopernicus_short) 
    } else {
      p <- p + labs(subtitle = paste(textCopernicus)) + theme(plot.subtitle = element_text(color = "red"))
    } 
  }
  # plot as part of comparison several datasets
  if (!is.na(match('Copernicus', datasets)) & length(datasets) > 1){ 
    # check if dataset covers reference period, plot or print warning
    if (refs >= y1_era5){
      p <- plot_plural_lines(p, D2$val, D2$y, m2_new, "red", textCopernicus_short, out2[2], 5)
      top5(period, D2$val, D2$y, m2_new, textCopernicus_short) 
    } else {
      p <- p + labs(subtitle = paste(textCopernicus)) + theme(plot.subtitle = element_text(color = "red"))
    }
  }

  # HadCRUT
  # plot as single dataset
  if (!is.na(match('HadCRUT', datasets)) & length(datasets) == 1){
    if (plot_type == 'points') p <- plot_singular_points(p, D3$val, D3$y, D3$col, m3_new, textHadCRUT, out3[2]) 
    if (plot_type == 'bars')   p <- plot_singular_bars(p, D3$val, D3$y, D3$col, m3_new, textHadCRUT, out3[2]) 
    if (plot_type != 'bars' & plot_type != 'points') cat(paste0('** WARNING **: set plot_type to "points" or "bars" \n\n'))
    top5(period, D3$val, D3$y, m3_new, textHadCRUT_short) 
  }
  # plot as part of comparison several datasets
  if (!is.na(match('HadCRUT',datasets)) & length(datasets) > 1){
    p <- plot_plural_lines(p, D3$val, D3$y, m3_new, "brown", textHadCRUT_short, out3[2], -35) 
    top5(period, D3$val, D3$y, m3_new, textHadCRUT_short) 
  }

  p <- p + theme(axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5)
               , axis.text.y = element_text(size = 20)
               , panel.background = element_rect(fill = "transparent") 
               , plot.background = element_rect(fill = "transparent", color = NA) 
               , panel.grid.major = element_blank() 
               , panel.grid.minor = element_blank() 
               , legend.background = element_rect(fill = "transparent") 
               , legend.box.background = element_rect(fill = "transparent")
               , plot.title = element_text(hjust = 0.5, size = 18)
               , plot.subtitle = element_text(hjust = 0.5, size = 14))


  p <- p + 
       scale_x_continuous(limits = c(y1 - 10, year_max + 1), breaks = seq(y1 - 10, year_max + 1, 10)) +
       scale_y_continuous(limits = c(floor(min_all / 0.5) * 0.5, ceiling(max_all / 0.5) * 0.5), breaks = seq(floor(min_all / 0.5) * 0.5, ceiling(max_all / 0.5) * 0.5, 0.5))

  print(p)
  if (save_option){ggsave(plot = p, file = save_name, bg = "transparent", width = 11, height = 6)}

  cat("-----------------------------------------------------------------------------------------------------------------------------\n")
  
}





