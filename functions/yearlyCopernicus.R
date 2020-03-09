# ---------------------------------
# function: yearlyCopernicus
# description: calculates yearly anomalies from monthly values
#
# input: data frame of year and montly anomalies
# output: yearly anomalies
#
# created Mar 4 2020
# author: lineb@met.no 
#

# check if can use any form od apply!!

# ---------------------------------
yearlyCopernicus <- function(D2in){
  D2y <- data.frame()
  for (ix in 1:length(D2in[,1])){
    tmp_D2y <- sum(D2in[ix,])
    D2y <- rbind(D2y,tmp_D2y)
  }
  return(D2y)
}


