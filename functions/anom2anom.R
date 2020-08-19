# ---------------------------------
# function: anom2anom
# description: re-calculates anomalies relative to desired normal period
#
# input: data frame of year and anomaly
# output: mean anomaly over new normal period that can be used to adjust the timeseries of old anomaly values
#
# created Feb 7 2020
# author: lineb@met.no 
#
# ---------------------------------

anom2anom <- function(data,new_anom_start,new_anom_end){
  df=data.frame()
  tempy<-as.character(data$y)
  for (n in 1:length(tempy)){
      if(as.numeric(tempy[n])>=new_anom_start && as.numeric(tempy[n]<=new_anom_end)){
        tempdf<-data.frame(y=data$y[n],x=data$val[n])
        df<- rbind(df,tempdf)
      }
  }
m_new <- mean(df$x)
}



