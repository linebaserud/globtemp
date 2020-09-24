############################################################################################################
#
# Plotting routine for timeseries of temperature anomalies with option for adjusting the reference period
#
# created: February/March 2020
# author: lineb@met.no
#
############################################################################################################

# load libraries and functions ----------------------------------------------------------------

rm(list = ls())

library('lubridate')
library("ggplot2")

fun_list<-c("anom2anom.R",
            "distr_col.R",
            "readCopernicus.R",
            "readHadCRUT.R",
            "readNASA.R")
for (fun in fun_list) {source(paste0("functions/", fun))}

# main function -------------------------------------------------------------------------------

globtemp <- function(datasets, refs, refe, period, orig, save_option, save_name){

  # get data ----------------------------------------------------------------------------------
  
  filenameNASA <- NA  #"data/GLB.Ts+dSST_Jan2020.csv"
  filenameCop <- NA   #"data/ts_1month_anomaly_Global_ea_2t_202001_v01.csv"
  filenameHadYearly <- NA   #"data/Hadcrut4_annual_09032020"
  filenameHadMonthly <- NA  #"data/Hadcrut4_06022020"

  y1 <- data.frame()
  max_all <- data.frame()
  min_all <- data.frame()

  # NASA/GISS
  if (!is.na(match('NASA', datasets))) {
    out <- readNASA(filenameNASA, period) 
    D=out[[1]]
    refsNASA <- 1951
    refeNASA <- 1980
    y1NASA <- 1880
    if (refsNASA == refs & refeNASA == refe) {m_new <- 0}                      # if ref period equal to original...
    if (refsNASA != refs | refeNASA != refe) {m_new <- anom2anom(D,refs,refe)} # ...else change ref period
    textNASA <- "NASA/GISS GHCN-v4 1880-12/2019 + SST: ERSST v5 1880-12/2019 (original reference period: 1951-1980)"
    D <- cbind(D, distr_col(D, m_new))                                           # add column with colors for plotting
    y1 <- rbind(y1, y1NASA)
    max_all <- rbind(max_all, max(D$val - m_new, na.rm = TRUE))
    min_all <- rbind(min_all, min(D$val - m_new, na.rm = TRUE))
  }

  # Copernicus
  if (!is.na(match('Copernicus', datasets))) {
    out2 <- readCopernicus(filenameCop, period)
    D2 <- out2[[1]]
    refsCop <- 1981
    refeCop <- 2010
    y1Cop <- 1979 
    yeCop <- D2$y[]
    textCopernicus <- "ERA5 Copernicus Climate Change Service/ECMWF (original reference period: 1981-2010)"
    if (y1Cop > refs) {textCopernicus <- "** WARNING **: Copernicus data does not cover entire reference period"} 
    colnames(D2) <- c("y", "val")
    if (refsCop == refs & refeCop == refe){m2_new <- 0}                       # if ref period equal to original...
    if (refsCop != refs | refeCop != refe){m2_new <- anom2anom(D2, refs, refe)} # ...else change ref period
    D2 <- cbind(D2, distr_col(D2, m2_new))                                       # add column with colors for plotting
    y1 <- rbind(y1, y1Cop)
    max_all <- rbind(max_all, max(D2$val - m2_new, na.rm = TRUE)) 
    min_all <- rbind(min_all, min(D2$val - m2_new, na.rm = TRUE)) 
  }

  # HadCRUT4
  if (!is.na(match('HadCRUT',datasets))){
    if (period != 'Yearly') {out3 <- readHadCRUT(filenameHadMonthly, period)}
    if (period == 'Yearly') {out3 <- readHadCRUT(filenameHadYearly, period)}
    D3=out3[[1]]; 
    refsHad <- 1961
    refeHad <- 1990
    y1Had <- 1850
    textHadCRUT <- "HadCRUT4: CRUTEM4 surface air temperature + HadSST3 sea-surface temperature (original reference period: 1961-1990)"
    if (refsHad == refs & refeHad == refe) {m3_new <- 0}                       # if chosen ref period equal to original...
    if (refsHad != refs | refeHad != refe) {m3_new <- anom2anom(D3, refs, refe)} # ...else change ref period
    D3 <- cbind(D3, distr_col(D3, m3_new))                                       # add column with colors for plotting
    y1 <- rbind(y1, y1Had)
    max_all <- rbind(max_all, max(D3$val - m3_new, na.rm = TRUE))
    min_all <- rbind(min_all, min(D3$val - m3_new, na.rm = TRUE))
  }  

  y1 <- min(y1)            # adaptive scaling x-axis
  max_all <- max(max_all)  # adaptive scaling y-axis max
  min_all <- min(min_all)  # adaptive scaling y-axis min

  # plot data ----------------------------------------------------------------------------------

  cat("-----------------------------------------------------------------------------------------------------------------------------\n")
  print(paste0("Top 5 relative to ", refs, "-", refe, ":")) 
  cat("\n")  

  p <- ggplot() +
       xlab("") +
       ylab("") +
       geom_hline(yintercept = 0, color = "grey") +
       geom_segment(aes(x=refs,xend=refe,y=0,yend=0),color = "grey",size=2) + 
       labs(title = paste(period, "global temperature anomalies (°C)  relative to", refs, "-", refe))

  # NASA
  if (!is.na(match('NASA', datasets)) & length(datasets) == 1) {
    p=p+ 
      labs(subtitle=paste(textNASA))+
      geom_line(aes(y=D$val-m_new,x=D$y),size=1,linetype="solid",color="black")+ 
      geom_point(aes(y=D$val-m_new,x=D$y),pch=16,size=4,color=D$col)
    if (orig){p=p+geom_line(aes(y=D$val,x=D$y),size=1,linetype="solid",color="grey")}                              
  print(paste0("---- ",period, " top 5 NASA/GISS ----\n "))
  cat(paste0(c(1:5),": ",rev(tail(D$val[order(D$val)],n=5)), " (", rev(tail(D$y[order(D$val)],n=5)),")\n"))
  }
  if (!is.na(match('NASA',datasets)) & length(datasets)>1){
    p=p+geom_line(aes(y=D$val-m_new,x=D$y),size=1.5,linetype="solid",color="orange")
    p=p+annotate("text",x=y1+(2020-y1)/2-35,y=0.95,label="NASA/GISS",color="orange",size=6)  
    p=p+annotate("text",x=y1+(2020-y1)/2-35,y=0.85,label=paste0("(data until ",out[2],")"),color="orange",size=4)  
  cat(paste0("---- ",period, " top 5 NASA/GISS ----\n "))
  cat(paste0(c(1:5),": ",format(round(rev(tail(D$val[order(D$val)],n=5)),2),nsmall=2), " (", rev(tail(D$y[order(D$val)],n=5)),")\n"))
  }

  # Copernicus
  if (!is.na(match('Copernicus',datasets)) & length(datasets)==1){
    if (refs > y1Cop){
      p=p+
        labs(subtitle=paste(textCopernicus))+
        geom_line(aes(y=D2$val-m2_new,x=D2$y),size=1,linetype="solid",color="black")+ 
        geom_point(aes(y=D2$val-m2_new,x=D2$y),pch=16,size=4,color=D2$col)
    }
    if (orig){p=p+geom_line(aes(y=D2$val,x=D2$y),size=1,linetype="solid",color="grey")}                            
    if (refs < y1Cop){p=p+labs(subtitle=paste(textCopernicus))+theme(plot.subtitle = element_text(color = "red"))} 
  cat(paste0("---- ",period, " top 5 Copernicus ----\n "))
  cat(paste0(c(1:5),": ",rev(tail(D2$val[order(D2$val)],n=5)), " (", rev(tail(D2$y[order(D2$val)],n=5)),")\n"))
  }
  if (!is.na(match('Copernicus',datasets)) & length(datasets)>1){
    p=p+annotate("text",x=y1+(2020-y1)/2-5,y=0.95,label="Copernicus ERA5",color="red",size=6)  
    p=p+annotate("text",x=y1+(2020-y1)/2-5,y=0.85,label=paste0("(data until ",out2[2],")"),color="red",size=4)  
    if (refs > y1Cop){p=p+geom_line(aes(y=D2$val-m2_new,x=D2$y),size=1.5,linetype="solid",color="red",na.rm=TRUE)}  
    if (refs < y1Cop){p=p+labs(subtitle=paste(textCopernicus))+theme(plot.subtitle = element_text(color = "red"))}  
  cat(paste0("---- ",period, " top 5 Copernicus ----\n "))
  cat(paste0(c(1:5),": ",format(round(rev(tail(D2$val[order(D2$val)],n=5)),2),nsmall=2), " (", rev(tail(D2$y[order(D2$val)],n=5)),")\n"))
  }

  # HadCRUT
  if (!is.na(match('HadCRUT',datasets)) & length(datasets)==1){
    p=p+
      labs(subtitle=paste(textHadCRUT))+
      geom_line(aes(y=D3$val-m3_new,x=D3$y),size=1,linetype="solid",color="black",na.rm=TRUE)+
      geom_point(aes(y=D3$val-m3_new,x=D3$y),pch=16,size=4,color=D3$col,na.rm=TRUE)
    if (orig){p=p+geom_line(aes(y=D3$val,x=D3$y),size=1,linetype="solid",color="grey")}                               
  cat(paste0("---- ",period, " top 5 HadCRUT ----\n "))
  cat(paste0(c(1:5),": ",rev(tail(D3$val[order(D3$val)],n=5)), " (", rev(tail(D3$y[order(D3$val)],n=5)),")\n"))
  }
  if (!is.na(match('HadCRUT',datasets)) & length(datasets)>1){
    p=p+geom_line(aes(y=D3$val-m3_new,x=D3$y),size=1.5,linetype="solid",color="brown",na.rm=TRUE)
    p=p+annotate("text",x=y1+(2020-y1)/2+25,y=0.95,label="HadCRUT4",color="brown",size=6)  
    p=p+annotate("text",x=y1+(2020-y1)/2+25,y=0.85,label=paste0("(data until ",out3[2],")"),color="brown",size=4)  
  cat(paste0("---- ",period, " top 5 HadCRUT ----\n "))
  cat(paste0(c(1:5),": ",format(round(rev(tail(D3$val[order(D3$val)],n=5)),2),nsmall=2), " (", rev(tail(D3$y[order(D3$val)],n=5)),")\n"))
  }
  p=p+theme(
      axis.text.x=element_text(size=20,angle = 90,vjust=0.5)
    , axis.text.y=element_text(size=20)
    , panel.background = element_rect(fill = "transparent") 
    , plot.background = element_rect(fill = "transparent", color = NA) 
    , panel.grid.major = element_blank() 
    , panel.grid.minor = element_blank() 
    , legend.background = element_rect(fill = "transparent") 
    , legend.box.background = element_rect(fill = "transparent")
    , plot.title = element_text(hjust = 0.5,size=18)
    , plot.subtitle = element_text(hjust = 0.5,size=14))

  p=p+scale_x_continuous(limits=c(y1-10,2020),breaks=seq(y1-10,2020,10))
  p=p+scale_y_continuous(limits=c(floor(min_all/0.5)*0.5,ceiling(max_all/0.5)*0.5),breaks=seq(floor(min_all/0.5)*0.5,ceiling(max_all/0.5)*0.5,0.5))

  print(p)
  if (save_option){ggsave(plot=p,file=save_name, bg = "transparent",width = 11, height = 6)}

  cat("-----------------------------------------------------------------------------------------------------------------------------\n")

}





