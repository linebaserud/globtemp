# Wrapper for plotting timeseries after adjusting the reference period
# created Feb 7 2020
# author: lineb@met.no
#

#compare hadcru calc of monthly to yearly with yearly files
#coipernicus period/jan vs yearly, yearly function do not taek nan
# filenames in here to be able to update easy when new comes!!!
# from all if to use else?
# add warning eg hadcrut annual 2020 just jan value...

# libraries and functions ----------------------------------------------------------------

rm(list=ls())

library("ggplot2")

source("functions/anom2anom.R")
source("functions/distrCol.R")
source("functions/readCopernicus.R")
source("functions/readHadCRUT.R")
source("functions/readNASA.R")
source("functions/yearlyCopernicus")

# user input -----------------------------------------------------------------------------

datasets <- c('NASA') # choose dataset(s), e.g. 'NASA', 'Copernicus', and/or 'HAdCRUT' 
refs<-1961            # choose start reference period
refe<-1990            # choose end reference period
period <- 'Yearly'    # type of data to plot, e.g. 'Yearly', 'Jan', 'Feb', ...

# get data ----------------------------------------------------------------------------------

# NASA/GISS
if (!is.na(match('NASA',datasets))){
  D <- readNASA("GLB.Ts+dSST_Jan2020.csv",period) 
  refsNASA <- 1951
  refeNASA <- 1980
  year1 <- 1880
  if (refsNASA == refs & refeNASA == refe){m_new <- 0} # if choosen ref period is equal to original in dataset
  if (refsNASA != refs | refeNASA != refe){m_new <- anom2anom(D,refs,refe)} # else change ref period
  textNASA <- "NASA/GISS GHCN-v4 1880-12/2019 + SST: ERSST v5 1880-12/2019 (original reference period: 1951-1980)"
  D <- cbind(D,distrCol(D,m_new)) # add colomn with colors for plotting
}

# Copernicus
if (!is.na(match('Copernicus',datasets))){
  D2 <- readCopernicus("ts_1month_anomaly_Global_ea_2t_202001_v01.csv",period) # read copernicus
  #D2 <- data.frame(y=D2in$y,val=D2in$Jan)
  
  if (period== 'Yearly'){D2<-yearly}
 
  refsCop <- 1981
  refeCop <- 2010
  year1 <- 1979 
  textCopernicus <- "ERA5 Copernicus Climate Change Service/ECMWF (original reference period: 1981-2010)"
  if (D2$y[1] > refs){textCopernicus <- "**Warning**: NO Copernicus data for choosen reference period"} # add before and do not plot
  if (refsCop == refs & refeCop == refe){m2_new <- 0}
  if (refsCop != refs | refeCop != refe){m2_new <- anom2anom(D2,refs,refe)}
  D2 <- cbind(D2,distrCol(D2,m2_new)) # add colomn with colors for plotting
}

# HadCRUT4
if (!is.na(match('HadCRUT',datasets))){
  if (period != 'Yearly'){
    D3in <- readHadCRUT("Hadcrut4_06022020",period)
    D3 <- data.frame(y=D3in$y,val=D3in$val)
  }
  if (period== 'Yearly'){
    D3in <- read.table(file="Hadcrut4_annual_09032020")
    D3 <- data.frame(y=D3in$V1,val=D3in$V2) 
  }
  refsHad <- 1961
  refeHad <- 1990
  year1 <- 1850
  textHadCRUT <- "HadCRUT4: CRUTEM4 surface air temperature + HadSST3 sea-surface temperature (original reference period: 1961-1990)"
  if (refsHad == refs & refeHad == refe){m3_new <- 0} # if choosen ref period is equal to original in dataset
  if (refsHad != refs | refeHad != refe){m3_new <- anom2anom(D3,refs,refe)} # else change ref period
  D3 <- cbind(D3,distrCol(D3,m3_new)) # add colomn with colors for plotting
}

# plot data ----------------------------------------------------------------------------------

p=ggplot()+
  xlab("")+
  ylab("")+
  geom_hline(yintercept=0,color="black")+
  labs(title=paste(period, "temperature anomalies relative to",refs,"-",refe))

# NASA
if (!is.na(match('NASA',datasets))){
  p=p+ 
    labs(subtitle=paste(textNASA))+
    geom_line(aes(y=D$val-m_new,x=D$y),size=1,linetype="solid",color="black")+ 
    geom_point(aes(y=D$val-m_new,x=D$y),pch=16,size=4,color=D$col)+
    geom_line(aes(y=D$val,x=D$y),size=1,linetype="solid",color="grey") 
}

# Copernicus
if (!is.na(match('Copernicus',datasets)) & refs > D2$y[1]){
  p=p+
    labs(subtitle=paste(textCopernicus))+
    geom_line(aes(y=D2$val-m2_new,x=D2$y),size=1,linetype="dashed",color="green")+ 
    geom_point(aes(y=D2$val-m2_new,x=D2$y),pch=23,size=4,color="green",fill=D2$col)
}
if (refs < D2$y[1]){
    p=p+
    labs(subtitle=paste(textCopernicus))
}

# HadCRUT
if (!is.na(match('HadCRUT',datasets))){
  p=p+
    labs(subtitle=paste(textHadCRUT))+
    geom_line(aes(y=D3$val-m3_new,x=D3$y),size=1,linetype="solid",color="black")+
    geom_point(aes(y=D3$val-m3_new,x=D3$y),pch=16,size=4,color=D3$col)
}

p=p+theme(
      axis.text.x=element_text(size=20,angle = 90,vjust=0.5)
    , axis.text.y=element_text(size=20)
    , panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent")) # get rid of legend panel bg  

p=p + scale_x_continuous(limits=c(year1-10,2020),breaks=seq(year1-10,2020,10))+
      scale_y_discrete(limits=c(-0.5,0,0.5,1),expand = c(0.05,0.05))

print(p)
#ggsave(plot=p,file="plots/global_NASA_09032020.png", bg = "transparent",width = 11, height = 6) # save


