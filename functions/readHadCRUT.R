# read hadcrut

readHadCRUT <- function(filename,period){
  cat("-----------------------------------------------------------------------------------------------------------------------------\n")
  # read from file or url
  if(!is.na(filename)){Din <- read.table(file=filename)}
  if(is.na(filename) & period == 'Yearly'){
    Din <- read.table(url("https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.annual_ns_avg.txt"),header=FALSE)
    print("Reading HadCRUT4 data from: https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.annual_ns_avg.txt")
  }
  if(is.na(filename) & period != 'Yearly'){
    Din <- read.table(url("https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.monthly_ns_avg.txt"),header=FALSE)
    print("Reading HadCRUT4 data from https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.monthly_ns_avg.txt")
    #last_Had <- as.character(tail(Din$V1, n = 1))
  }

  if (period!='Yearly'){
    rr=c("01","02","03","04","05","06","07","08","09","10","11","12")
    for (ind in 1:length(rr)){
      mnd=data.frame()
      for (n in 1:length(Din$V1)){
        tempstr<-as.character(Din$V1[n])
        if(endsWith(tempstr,rr[ind])){
          tmp_mnd<-data.frame(m=Din$V1[n],glob=Din$V2[n],y=as.numeric(substr(tempstr,1,4)))
          mnd <- rbind(mnd,tmp_mnd)
        }
      }
    if (rr[ind]=="01"){jan=data.frame(glob=mnd$glob,y=mnd$y)}
    if (rr[ind]=="02"){feb=data.frame(glob=mnd$glob,y=mnd$y)}
    if (rr[ind]=="03"){mar=data.frame(glob=mnd$glob,y=mnd$y)}
    if (rr[ind]=="04"){apr=data.frame(glob=mnd$glob,y=mnd$y)}
    if (rr[ind]=="05"){may=data.frame(glob=mnd$glob,y=mnd$y)}
    if (rr[ind]=="06"){jun=data.frame(glob=mnd$glob,y=mnd$y)}
    if (rr[ind]=="07"){jul=data.frame(glob=mnd$glob,y=mnd$y)}
    if (rr[ind]=="08"){aug=data.frame(glob=mnd$glob,y=mnd$y)}
    if (rr[ind]=="09"){sep=data.frame(glob=mnd$glob,y=mnd$y)}
    if (rr[ind]=="10"){okt=data.frame(glob=mnd$glob,y=mnd$y)}
    if (rr[ind]=="11"){nov=data.frame(glob=mnd$glob,y=mnd$y)}
    if (rr[ind]=="12"){dec=data.frame(glob=mnd$glob,y=mnd$y)}
    }
    nan_mth=data.frame(y=jan$y[length(jan$y)],glob=as.numeric('NAN'))
    if (length(jan$glob)>length(feb$glob)){feb=rbind(feb,nan_mth)}
    if (length(jan$glob)>length(mar$glob)){mar=rbind(mar,nan_mth)}
    if (length(jan$glob)>length(apr$glob)){apr=rbind(apr,nan_mth)}
    if (length(jan$glob)>length(may$glob)){may=rbind(may,nan_mth)}
    if (length(jan$glob)>length(jun$glob)){jun=rbind(jun,nan_mth)}
    if (length(jan$glob)>length(jul$glob)){jul=rbind(jul,nan_mth)}
    if (length(jan$glob)>length(aug$glob)){aug=rbind(aug,nan_mth)}
    if (length(jan$glob)>length(sep$glob)){sep=rbind(sep,nan_mth)}
    if (length(jan$glob)>length(okt$glob)){okt=rbind(okt,nan_mth)}
    if (length(jan$glob)>length(nov$glob)){nov=rbind(nov,nan_mth)}
    if (length(jan$glob)>length(dec$glob)){dec=rbind(dec,nan_mth)}
    
    if (period=='January') {Dout=data.frame(y=jan$y,val=jan$glob, mth <- "01")}
    if (period=='February') {Dout=data.frame(y=feb$y,val=feb$glob, mth <- "02")}
    if (period=='March') {Dout=data.frame(y=mar$y,val=mar$glob, mth <- "03")}
    if (period=='April') {Dout=data.frame(y=apr$y,val=apr$glob, mth <- "04")}
    if (period=='May') {Dout=data.frame(y=may$y,val=may$glob, mth <- "05")}
    if (period=='June') {Dout=data.frame(y=jun$y,val=jun$glob, mth <- "06")}
    if (period=='July') {Dout=data.frame(y=jul$y,val=jul$glob, mth <- "07")}
    if (period=='August') {Dout=data.frame(y=aug$y,val=aug$glob, mth <- "08")}
    if (period=='September') {Dout=data.frame(y=sep$y,val=sep$glob, mth <- "09")}
    if (period=='October') {Dout=data.frame(y=oct$y,val=oct$glob, mth <- "10")}
    if (period=='November') {Dout=data.frame(y=nov$y,val=nov$glob, mth <- "11")}
    if (period=='December') {Dout=data.frame(y=dec$y,val=dec$glob, mth <- "12")}

    tmp1=Dout$val[!is.na(Dout$val)]
    tmp2=Dout$y[!is.na(Dout$val)]
    tmp3=Dout$mth[!is.na(Dout$val)]
    Dout <- data.frame(y=tmp2,val=tmp1,mth=tmp3)

    ly_Had <- tail(Dout$y, n=1)
    lm_Had <- tail(Dout$mth, n=1)
    last_Had <- paste0(ly_Had,"/",lm_Had)
  }

  if (period=='Yearly'){
    Dout=data.frame(y=Din$V1,val=Din$V2)
    check_last_Had <- read.table(url("https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.monthly_ns_avg.txt"),header=FALSE)
    last_Had <- as.character(tail(check_last_Had$V1, n = 1))
    rm(check_last_Had)
  }
  return(list(Dout,last_Had))
}



