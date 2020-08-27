# read hadcrut

readHadCRUT <- function(filename,period){

  # read from file or url
  if(!is.na(filename)){Din <- read.table(file=filename)}
  if(is.na(filename) & period == 'Yearly'){
    Din <- read.table(url("https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.annual_ns_avg.txt"),header=FALSE)
    print("Reading HadCRUT4 data from: https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.annual_ns_avg.txt")

   # check_last_Had <- read.table(url("https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.monthly_ns_avg.txt"),header=FALSE)
   # last_Had <- as.character(tail(check_last_Had$V1, n = 1))
   # rm(check_last_Had)
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
    
    if (period=='January') {Dout=data.frame(y=jan$y,val=jan$glob)}
    if (period=='February') {Dout=data.frame(y=feb$y,val=feb$glob)}
    if (period=='March') {Dout=data.frame(y=mar$y,val=mar$glob)}
    if (period=='April') {Dout=data.frame(y=apr$y,val=apr$glob)}
    if (period=='May') {Dout=data.frame(y=may$y,val=may$glob)}
    if (period=='June') {Dout=data.frame(y=jun$y,val=jun$glob)}
    if (period=='July') {Dout=data.frame(y=jul$y,val=jul$glob)}
    if (period=='August') {Dout=data.frame(y=aug$y,val=aug$glob)}
    if (period=='September') {Dout=data.frame(y=sep$y,val=sep$glob)}
    if (period=='October') {Dout=data.frame(y=oct$y,val=oct$glob)}
    if (period=='November') {Dout=data.frame(y=nov$y,val=nov$glob)}
    if (period=='December') {Dout=data.frame(y=dec$y,val=dec$glob)}

    #last_Had <- as.character(tail(Dout$y, n = 1))
   # last_Had=tail(Dout$y[!is.na(Dout$y)],1)
   # print(last_Had)
  }
  check_last_Had <- read.table(url("https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.monthly_ns_avg.txt"),header=FALSE)
  last_Had <- as.character(tail(check_last_Had$V1, n = 1))
  rm(check_last_Had)


  if (period=='Yearly'){
    Dout=data.frame(y=Din$V1,val=Din$V2)
  }
  return(list(Dout,last_Had))
}
