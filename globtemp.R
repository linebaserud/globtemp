############################################################################################################
#
# Function for plotting timeseries of global temperature anomalies 
# (with option for adjusting the reference period)
# 
# February/March/November 2020
# lineb@met.no
#
############################################################################################################

# load libraries and functions ----------------------------------------------------------------

rm(list = ls())

suppressPackageStartupMessages(library('lubridate'))
suppressPackageStartupMessages(library('ggplot2'))

fun_list <- c("anom2anom.R",
              "distr_col.R",
              "read_era5.R",
              "read_hadcrut4.R",
              "read_nasa.R",
              "top5.R",
              "plot_singular_points.R",
              "plot_singular_bars.R",
              "plot_plural_lines.R")

for (fun in fun_list) {source(paste0("functions/", fun))}

# main function -------------------------------------------------------------------------------

globtemp <- function(datasets, refs, refe, period, plot_type, save_option, save_name){

  # get data ----------------------------------------------------------------------------------
  
  filename_nasa <- NA  
  filename_copernicus <- NA   
  filename_hadcrut_yearly <- NA   
  filename_hadcrut_monthly <- NA  

  year_min <- data.frame()
  year_max <- data.frame()
  val_max  <- data.frame()
  val_min  <- data.frame()

  # NASA/GISS
  if (!is.na(match('NASA', datasets))) {
    out <- read_nasa(filename_nasa, period) 
    D <- out[[1]]
    refs_nasa <- 1951
    refe_nasa <- 1980
    year_min_nasa <- 1880
    text_nasa_short <- "NASA/GISS"
    text_nasa <- "NASA/GISS GISTEMP v4 (NOAA GHCN v4 and ERSST v5)"
    if (refs_nasa == refs & refe_nasa == refe) {m_new <- 0}                                  # if ref period equal to original...
    if (refs_nasa != refs | refe_nasa != refe) {m_new <- anom2anom(D, refs, refe)}           # ...else calculate offset
    D <- cbind(D, distr_col(D, m_new))                                                       # add column with colors for plotting
    year_min <- rbind(year_min, year_min_nasa)
    year_max <- rbind(year_max, tail(D$y, n = 1))  
    val_max  <- rbind(val_max, max(D$val - m_new, na.rm = TRUE))
    val_min  <- rbind(val_min, min(D$val - m_new, na.rm = TRUE))
  }

  # ERA5 Copernicus
  if (!is.na(match('ERA5', datasets))) {
    out2 <- read_era5(filename_copernicus, period)
    D2 <- out2[[1]]
    colnames(D2) <- c("y", "val")
    refs_era5 <- 1981
    refe_era5 <- 2010
    year_min_era5 <- 1979 
    text_era5_short <- "ERA5 Copernicus"
    text_era5 <- "ERA5 Copernicus Climate Change Service/ECMWF "
    if (year_min_era5 > refs) {text_era5 <- "** WARNING **:  ERA5 data do not cover entire reference period"} 
    if (refs_era5 == refs & refe_era5 == refe){m2_new <- 0}                                  # if ref period equal to original...
    if (refs_era5 != refs | refe_era5 != refe){m2_new <- anom2anom(D2, refs, refe)}          # ...else calculate offset
    D2 <- cbind(D2, distr_col(D2, m2_new))                                                   # add column with colors for plotting
    year_min <- rbind(year_min, ceiling(year_min_era5 / 10) * 10)
    year_max <- rbind(year_max, tail(D2$y, n = 1))  
    val_max  <- rbind(val_max, max(D2$val - m2_new, na.rm = TRUE)) 
    val_min  <- rbind(val_min, min(D2$val - m2_new, na.rm = TRUE)) 
  }

  # HadCRUT4
  if (!is.na(match('HadCRUT', datasets))){
    if (period != 'Yearly') {out3 <- read_hadcrut4(filename_hadcrut_monthly, period)}
    if (period == 'Yearly') {out3 <- read_hadcrut4(filename_hadcrut_yearly, period)}
    D3 <- out3[[1]]; 
    refs_hadcrut <- 1961
    refe_hadcrut <- 1990
    year_min_hadcrut <- 1850
    text_hadcrut_short <- "HadCRUT4"
    text_hadcrut <- "HadCRUT4: CRUTEM4 surface air temperature + HadSST3 sea-surface temperature"
    if (refs_hadcrut == refs & refe_hadcrut == refe) {m3_new <- 0}                           # if chosen ref period equal to original...
    if (refs_hadcrut != refs | refe_hadcrut != refe) {m3_new <- anom2anom(D3, refs, refe)}   # ...else calculate offset
    D3 <- cbind(D3, distr_col(D3, m3_new))                                                   # add column with colors for plotting
    year_min <- rbind(year_min, year_min_hadcrut)
    year_max <- rbind(year_max, tail(D3$y, n = 1))  
    val_max  <- rbind(val_max, max(D3$val - m3_new, na.rm = TRUE))
    val_min  <- rbind(val_min, min(D3$val - m3_new, na.rm = TRUE))
  }  

  year_min <- min(year_min)  # adaptive scaling x-axis min
  year_max <- max(year_max)  # adaptive scaling x-axis max
  val_max  <- max(val_max)   # adaptive scaling y-axis max
  val_min  <- min(val_min)   # adaptive scaling y-axis min

  cat("-----------------------------------------------------------------------------------------------------------------------------\n\n")

  # plot data ----------------------------------------------------------------------------------

  p <- ggplot() +
       xlab("") +
       ylab("") +
       geom_hline(yintercept = 0, color = "grey") +
       geom_segment(aes(x = refs, xend = refe, y = 0, yend = 0), color = "grey60", size = 3) + 
       labs(title = paste(period, "global temperature anomalies (°C)  relative to", refs, "-", refe))

  # NASA
  # plot as single dataset
  if (!is.na(match('NASA', datasets)) & length(datasets) == 1) {
    if (plot_type == 'points') p <- plot_singular_points(p, D$val, D$y, D$col, m_new, text_nasa, out[2]) 
    if (plot_type == 'bars')   p <- plot_singular_bars(p, D$val, D$y, D$col, m_new, text_nasa, out[2])
    if (plot_type != 'bars' & plot_type != 'points') cat(paste0('** WARNING **: set plot_type to "points" or "bars" \n\n'))
    top5(period, refs, refe, D$val, D$y, m_new, text_nasa_short)
  }
  # plot as part of comparison several datasets
  if (!is.na(match('NASA', datasets)) & length(datasets) > 1){
    p <- plot_plural_lines(p, D$val, D$y, m_new, "orange", text_nasa_short, out[2], year_min, year_max, 45, val_max) 
    top5(period, refs, refe, D$val, D$y, m_new, text_nasa_short) 
  }

  # ERA5 Copernicus
  # plot as single dataset
  if (!is.na(match('ERA5', datasets)) & length(datasets) == 1){
    # check if dataset covers reference period, plot or print warning
    if (refs >= year_min_era5){
      if (plot_type == 'points')  p <- plot_singular_points(p, D2$val, D2$y, D2$col, m2_new, text_era5, out2[2]) 
      if (plot_type == 'bars')    p <- plot_singular_bars(p, D2$val, D2$y, D2$col, m2_new, text_era5, out2[2]) 
      if (plot_type != 'bars' & plot_type != 'points') cat(paste0('** WARNING **: set plot_type to "points" or "bars" \n\n'))
      top5(period, refs, refe, D2$val, D2$y, m2_new, text_era5_short) 
    } else {
      p <- p + labs(subtitle = paste(text_era5)) + theme(plot.subtitle = element_text(color = "red"))
    } 
  }
  # plot as part of comparison several datasets
  if (!is.na(match('ERA5', datasets)) & length(datasets) > 1){ 
    # check if dataset covers reference period, plot or print warning
    if (refs >= year_min_era5){
      p <- plot_plural_lines(p, D2$val, D2$y, m2_new, "red", text_era5_short, out2[2], year_min, year_max, 5, val_max)
      top5(period, refs, refe, D2$val, D2$y, m2_new, text_era5_short) 
    } else {
      p <- p + labs(subtitle = paste(text_era5)) + theme(plot.subtitle = element_text(color = "red"))
    }
  }

  # HadCRUT
  # plot as single dataset
  if (!is.na(match('HadCRUT', datasets)) & length(datasets) == 1){
    if (plot_type == 'points') p <- plot_singular_points(p, D3$val, D3$y, D3$col, m3_new, text_hadcrut, out3[2]) 
    if (plot_type == 'bars')   p <- plot_singular_bars(p, D3$val, D3$y, D3$col, m3_new, text_hadcrut, out3[2]) 
    if (plot_type != 'bars' & plot_type != 'points') cat(paste0('** WARNING **: set plot_type to "points" or "bars" \n\n'))
    top5(period, refs, refe, D3$val, D3$y, m3_new, text_hadcrut_short) 
  }
  # plot as part of comparison several datasets
  if (!is.na(match('HadCRUT',datasets)) & length(datasets) > 1){
    p <- plot_plural_lines(p, D3$val, D3$y, m3_new, "brown", text_hadcrut_short, out3[2], year_min, year_max, -35, val_max) 
    top5(period, refs, refe, D3$val, D3$y, m3_new, text_hadcrut_short) 
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
       scale_x_continuous(limits = c(year_min - 10, year_max + 1), breaks = seq(year_min - 10, year_max + 1, 10)) +
       scale_y_continuous(limits = c(floor(val_min / 0.5) * 0.5, ceiling(val_max / 0.5) * 0.5), breaks = seq(floor(val_min / 0.5) * 0.5, ceiling(val_max / 0.5) * 0.5, 0.5))

  print(p)
  if (save_option){ggsave(plot = p, file = save_name, bg = "transparent", width = 11, height = 6)}

  cat("-----------------------------------------------------------------------------------------------------------------------------\n")
  
}





