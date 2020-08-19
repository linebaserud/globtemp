# -----------------------------------------------
# function: distr_col
# description: distributes colors red (above), blue (below), black (equal) relative to normal
#
# input: data frame with values (optional: anomaly shift)
# output: data frame with colors
#
# created Feb 7 2020
# author: lineb@met.no
#
# -----------------------------------------------

distr_col <- function(data,anomaly_shift=NA){
  if (!is.na(anomaly_shift)){data$val <- data$val-anomaly_shift}
  vcol=data.frame()
  for (n in 1:length(data$val)){
          if(!is.na(data$val[n]) && data$val[n]<0){
            tmpcol<-data.frame(col="blue")
            vcol <- rbind(vcol,tmpcol)
       } else if (!is.na(data$val[n]) && data$val[n]==0){
           tmpcol<-data.frame(col="black")
           vcol <- rbind(vcol,tmpcol)
       } else if (!is.na(data$val[n]) && data$val[n]>0){
           tmpcol<-data.frame(col="red")
           vcol <- rbind(vcol,tmpcol)
       } else if (is.na(data$val[n])){
           tmpcol<-data.frame(col=NA)
           vcol <- rbind(vcol,tmpcol)
       } 
  }
  return(vcol)
}
