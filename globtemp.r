############################################################################################################
#
# Plotting routine for timeseries of temperature anomalies with option for adjusting the reference period
#
# created: February/March 2020
# author: lineb@met.no
#
############################################################################################################

# load libraries and functions ----------------------------------------------------------------

rm(list=ls())

library("ggplot2")

fun_list<-c("anom2anom.r",
            "distrCol.r",
            "readCopernicus.r",
            "readHadCRUT.r",
            "readNASA.r")
for (fun in fun_list){source(paste0("functions/",fun))}

# main function -------------------------------------------------------------------------------

globtemp <- function(datasets,refs,refe,period,orig,save_option,save_name){

  # get data ----------------------------------------------------------------------------------

  filenameNASA <- "data/GLB.Ts+dSST_Jan2020.csv"
  filenameCop <-"data/ts_1month_anomaly_Global_ea_2t_202001_v01.csv"
  filenameHadYearly <-"data/Hadcrut4_annual_09032020"
  filenameHadMonthly <-"data/Hadcrut4_06022020"

  y1 <- data.frame()

  # NASA/GISS
  if (!is.na(match('NASA',datasets))){
    D <- readNASA(filenameNASA,period) 
    refsNASA <- 1951
    refeNASA <- 1980
    y1NASA <- 1880
    if (refsNASA == refs & refeNASA == refe){m_new <- 0}                      # if ref period equal to original...
    if (refsNASA != refs | refeNASA != refe){m_new <- anom2anom(D,refs,refe)} # ...else change ref period
    textNASA <- "NASA/GISS GHCN-v4 1880-12/2019 + SST: ERSST v5 1880-12/2019 (original reference period: 1951-1980)"
    D <- cbind(D,distrCol(D,m_new))                                           # add colomn with colors for plotting
    y1 <- rbind(y1,y1NASA)
  }

  # Copernicus
  if (!is.na(match('Copernicus',datasets))){
    D2 <- readCopernicus(filenameCop,period)
    refsCop <- 1981
    refeCop <- 2010
    y1Cop <- 1979 
    textCopernicus <- "ERA5 Copernicus Climate Change Service/ECMWF (original reference period: 1981-2010)"
    if (y1Cop > refs){textCopernicus <- "** WARNING **: Copernicus data does not cover entire reference period"} 
    colnames(D2) <- c("y", "val")
    if (refsCop == refs & refeCop == refe){m2_new <- 0}                       # if ref period equal to original...
    if (refsCop != refs | refeCop != refe){m2_new <- anom2anom(D2,refs,refe)} # ...else change ref period
    D2 <- cbind(D2,distrCol(D2,m2_new))                                       # add colomn with colors for plotting
    y1 <- rbind(y1,y1Cop)
  }

  # HadCRUT4
  if (!is.na(match('HadCRUT',datasets))){
    if (period != 'Yearly'){D3 <- readHadCRUT(filenameHadMonthly,period)}
    if (period == 'Yearly'){D3 <- readHadCRUT(filenameHadYearly,period)}
    refsHad <- 1961
    refeHad <- 1990
    y1Had <- 1850
    textHadCRUT <- "HadCRUT4: CRUTEM4 surface air temperature + HadSST3 sea-surface temperature (original reference period: 1961-1990)"
    if (refsHad == refs & refeHad == refe){m3_new <- 0}                       # if chosen ref period equal to original...
    if (refsHad != refs | refeHad != refe){m3_new <- anom2anom(D3,refs,refe)} # ...else change ref period
    D3 <- cbind(D3,distrCol(D3,m3_new))                                       # add colomn with colors for plotting
    y1 <- rbind(y1,y1Had)
  }  

  y1 <- min(y1) # scaling for x-axis

  # plot data ----------------------------------------------------------------------------------

  p=ggplot()+
    xlab("")+
    ylab("")+
    geom_hline(yintercept=0,color="black")+
    labs(title=paste(period, "temperature anomalies (°C)  relative to",refs,"-",refe))

  # NASA
  if (!is.na(match('NASA',datasets)) & length(datasets)==1){
    p=p+ 
      labs(subtitle=paste(textNASA))+
      geom_line(aes(y=D$val-m_new,x=D$y),size=1,linetype="solid",color="black")+ 
      geom_point(aes(y=D$val-m_new,x=D$y),pch=16,size=4,color=D$col)
    if (orig){p=p+geom_line(aes(y=D$val,x=D$y),size=1,linetype="solid",color="grey")}                              
  }
  if (!is.na(match('NASA',datasets)) & length(datasets)>1){
    p=p+geom_line(aes(y=D$val-m_new,x=D$y),size=1.5,linetype="solid",color="orange")
    p=p+annotate("text",x=y1+(2020-y1)/2-35,y=0.95,label="NASA/GISS",color="orange",size=6)  
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
  }
  if (!is.na(match('Copernicus',datasets)) & length(datasets)>1){
    p=p+annotate("text",x=y1+(2020-y1)/2-5,y=0.95,label="Copernicus",color="red",size=6)  
    if (refs > y1Cop){p=p+geom_line(aes(y=D2$val-m2_new,x=D2$y),size=1.5,linetype="solid",color="red")}  
    if (refs < y1Cop){p=p+labs(subtitle=paste(textCopernicus))+theme(plot.subtitle = element_text(color = "red"))}  
  }

  # HadCRUT
  if (!is.na(match('HadCRUT',datasets)) & length(datasets)==1){
    p=p+
      labs(subtitle=paste(textHadCRUT))+
      geom_line(aes(y=D3$val-m3_new,x=D3$y),size=1,linetype="solid",color="black")+
      geom_point(aes(y=D3$val-m3_new,x=D3$y),pch=16,size=4,color=D3$col)
    if (orig){p=p+geom_line(aes(y=D3$val,x=D3$y),size=1,linetype="solid",color="grey")}                               
  }
  if (!is.na(match('HadCRUT',datasets)) & length(datasets)>1){
    p=p+geom_line(aes(y=D3$val-m3_new,x=D3$y),size=1.5,linetype="solid",color="brown")
    p=p+annotate("text",x=y1+(2020-y1)/2+25,y=0.95,label="HadCRUT4",color="brown",size=6)  
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

  p=p + scale_x_continuous(limits=c(y1-10,2020),breaks=seq(y1-10,2020,10))+
        scale_y_continuous(limits=c(-1,1),breaks=seq(-1,1,0.5)) 

  print(p)
  if (save_option){ggsave(plot=p,file=save_name, bg = "transparent",width = 11, height = 6)}
}
