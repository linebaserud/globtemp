# Wrapper for plotting timeseries after adjusting the reference period
# created Feb 7 2020
# author: lineb@met.no
#

#compare hadcru calc of monthly to yearly with yearly files
#coipernicus period/jan vs yearly, yearly function do not taek nan
# from all if to use else?
# add warning eg hadcrut annual 2020 just jan value...
# if datasets longer than 1 orig to F?

# libraries and functions ----------------------------------------------------------------

rm(list=ls())

library("ggplot2")

fun_list<-c("anom2anom.r",
            "distrCol.r",
            "readCopernicus.r",
            "readHadCRUT.r",
            "readNASA.r")
for (fun in fun_list){source(paste0("functions/", fun))}

# user input -----------------------------------------------------------------------------

filenameNASA <- "data/GLB.Ts+dSST_Jan2020.csv"
filenameCop <-"data/ts_1month_anomaly_Global_ea_2t_202001_v01.csv"
filenameHadYearly <-"data/Hadcrut4_annual_09032020"
filenameHadMonthly <-"data/Hadcrut4_06022020"

datasets <- c('HadCRUT') # dataset(s) to plot, e.g. 'NASA', 'Copernicus', and/or 'HAdCRUT' 

refs<-1981            # start reference period
refe<-2010            # end reference period
period <- 'Yearly'    # type of data, e.g. 'Yearly', 'Jan', or 'Feb', ...

orig<-F               # T/F plot data relative to original reference period

# get data ----------------------------------------------------------------------------------

# NASA/GISS
if (!is.na(match('NASA',datasets))){
  D <- readNASA(filenameNASA,period) 
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
  D2 <- readCopernicus(filenameCop,period)
  refsCop <- 1981
  refeCop <- 2010
  year1 <- 1979 
  textCopernicus <- "ERA5 Copernicus Climate Change Service/ECMWF (original reference period: 1981-2010)"
  if (D2$y[1] > refs){textCopernicus <- "**Warning**: NO Copernicus data for choosen reference period"} # add before and do not plot
  colnames(D2) <- c("y", "val")
  if (refsCop == refs & refeCop == refe){m2_new <- 0}                       # if ref period equal to original...
  if (refsCop != refs | refeCop != refe){m2_new <- anom2anom(D2,refs,refe)} # ...else change ref period
  D2 <- cbind(D2,distrCol(D2,m2_new))                                       # add colomn with colors for plotting
}

# HadCRUT4
if (!is.na(match('HadCRUT',datasets))){
  if (period != 'Yearly'){D3 <- readHadCRUT(filenameHadMonthly,period)}
  if (period == 'Yearly'){D3 <- readHadCRUT(filenameHadYearly,period)}
  refsHad <- 1961
  refeHad <- 1990
  year1 <- 1850
  textHadCRUT <- "HadCRUT4: CRUTEM4 surface air temperature + HadSST3 sea-surface temperature (original reference period: 1961-1990)"
  if (refsHad == refs & refeHad == refe){m3_new <- 0}                       # if chosen ref period equal to original...
  if (refsHad != refs | refeHad != refe){m3_new <- anom2anom(D3,refs,refe)} # ...else change ref period
  D3 <- cbind(D3,distrCol(D3,m3_new))                                       # add colomn with colors for plotting
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
    if (orig){geom_line(aes(y=D$val,x=D$y),size=1,linetype="solid",color="grey")} # relative to  original reference period
}

# Copernicus
if (!is.na(match('Copernicus',datasets)) & refs > D2$y[1]){
  p=p+
    labs(subtitle=paste(textCopernicus))+
    geom_line(aes(y=D2$val-m2_new,x=D2$y),size=1,linetype="dashed",color="green")+ 
    geom_point(aes(y=D2$val-m2_new,x=D2$y),pch=23,size=4,color="green",fill=D2$col)
    if (orig){geom_line(aes(y=D2$val,x=D2$y),size=1,linetype="solid",color="grey")} # relative to  original reference period
}
if (refs < D2$y[1]){p=p+labs(subtitle=paste(textCopernicus))}                       # print warning

# HadCRUT
if (!is.na(match('HadCRUT',datasets))){
  p=p+
    labs(subtitle=paste(textHadCRUT))+
    geom_line(aes(y=D3$val-m3_new,x=D3$y),size=1,linetype="solid",color="black")+
    geom_point(aes(y=D3$val-m3_new,x=D3$y),pch=16,size=4,color=D3$col)
    if (orig){geom_line(aes(y=D3$val,x=D3$y),size=1,linetype="solid",color="grey")} # relative to original reference period
}

p=p+theme(
      axis.text.x=element_text(size=20,angle = 90,vjust=0.5)
    , axis.text.y=element_text(size=20)
    , panel.background = element_rect(fill = "transparent") 
    , plot.background = element_rect(fill = "transparent", color = NA) 
    , panel.grid.major = element_blank() 
    , panel.grid.minor = element_blank() 
    , legend.background = element_rect(fill = "transparent") 
    , legend.box.background = element_rect(fill = "transparent"))  

p=p + scale_x_continuous(limits=c(year1-10,2020),breaks=seq(year1-10,2020,10))+
      scale_y_discrete(limits=c(-0.5,0,0.5,1),expand = c(0.05,0.05))

print(p)
#ggsave(plot=p,file="plots/global_NASA_09032020.png", bg = "transparent",width = 11, height = 6) # save figure


