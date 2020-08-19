# function: readCopernicus
# description: gives either montly or yearly values depending on input
# input: filename, period (e.g. 'Jan' or 'Yearly')
# output: dataframe with values
#
# created Mar 13 2020
# author: lineb@met.no

# ----------------------------------------------------------------------------------------------------

library('lubridate')

readCopernicus <- function(filename,period){

  # read from file
  if(!is.na(filename)){Din <- read.table(file=filename,header=TRUE,skip=2,sep=",")}

  # find newest data from web
  if(is.na(filename)){
    file_url = paste0("https://climate.copernicus.eu/sites/default/files/",format(Sys.Date(),"%Y-%m"),"/ts_1month_anomaly_Global_ERA5_2T_",format(as.Date(floor_date(Sys.Date(),"month")-months(1)),"%Y%m"),"_v01.csv") 
    Din <- try(read.csv(url(file_url),header=TRUE,skip=8,sep=","))
    print(paste("Reading Copernicus ERA5 data from:",file_url))
    if (class(Din) == "try-error") {
      print("Data not available for previous month, trying the one before...")
      file_url = paste0("https://climate.copernicus.eu/sites/default/files/",format(as.Date(floor_date(Sys.Date(),"month")-months(1)),"%Y-%m"),"/ts_1month_anomaly_Global_ERA5_2T_",format(as.Date(floor_date(Sys.Date(),"month")-months(2)),"%Y%m"),"_v01.csv")
      Din <- read.csv(url(file_url),header=TRUE,skip=8,sep=",")
      print(paste("Reading Copernicus ERA5 data from:",file_url))
    }
  }

  rr=c("01","02","03","04","05","06","07","08","09","10","11","12")
  for (ind in 1:length(rr)){
    mnd=data.frame()
    for (n in 1:length(Din$Month)){
      tempstr<-as.character(Din$Month[n])
      if(endsWith(tempstr,rr[ind])){
        tmp_mnd<-data.frame(m=Din$Month[n],glob=Din$global[n],y=as.numeric(substr(tempstr,1,4)))
        mnd <- rbind(mnd,tmp_mnd)
      }
    }
    if (rr[ind]=="01"){jan=mnd}
    if (rr[ind]=="02"){feb=mnd}
    if (rr[ind]=="03"){mar=mnd}
    if (rr[ind]=="04"){apr=mnd}
    if (rr[ind]=="05"){may=mnd}
    if (rr[ind]=="06"){jun=mnd}
    if (rr[ind]=="07"){jul=mnd}
    if (rr[ind]=="08"){aug=mnd}
    if (rr[ind]=="09"){sep=mnd}
    if (rr[ind]=="10"){oct=mnd}
    if (rr[ind]=="11"){nov=mnd}
    if (rr[ind]=="12"){dec=mnd}
  }
  if (period != 'Yearly'){
    if (period == 'January'){D2 <- data.frame(y=jan$y,val=jan$glob)}
    if (period == 'February'){D2 <- data.frame(y=feb$y,val=feb$glob)}
    if (period == 'March'){D2 <- data.frame(y=mar$y,val=mar$glob)}
    if (period == 'April'){D2 <- data.frame(y=apr$y,val=apr$glob)}
    if (period == 'May'){D2 <- data.frame(y=may$y,val=may$glob)}
    if (period == 'June'){D2 <- data.frame(y=jun$y,val=jun$glob)}
    if (period == 'July'){D2 <- data.frame(y=jul$y,val=jul$glob)}
    if (period == 'August'){D2 <- data.frame(y=aug$y,val=aug$glob)}
    if (period == 'September'){D2 <- data.frame(y=sep$y,val=sep$glob)}
    if (period == 'October'){D2 <- data.frame(y=oct$y,val=oct$glob)}
    if (period == 'November'){D2 <- data.frame(y=nov$y,val=nov$glob)}
    if (period == 'December'){D2 <- data.frame(y=dec$y,val=dec$glob)}
  }
  if (period == 'Yearly'){
    D2y <- data.frame()
    for (ix in 1:length(jan[,3])){
      tmp_D2y <- sum(jan[ix,2],feb[ix,2],mar[ix,2],apr[ix,2],may[ix,2],jun[ix,2],jul[ix,2],aug[ix,2],sep[ix,2],oct[ix,2],nov[ix,2],dec[ix,2])
      D2y <- rbind(D2y,tmp_D2y)
    }
    D2<-data.frame(y=jan[,3],val=D2y/12)
  }
  return(D2)
}







