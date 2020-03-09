# read hadcrut

readHadCRUT <- function(filename,period){
  Din <- read.table(file=filename)
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
  if (rr[ind]=="01"){jan=mnd}
  if (rr[ind]=="02"){feb=mnd}
  if (rr[ind]=="03"){mar=mnd}
  if (rr[ind]=="04"){apr=mnd}
  if (rr[ind]=="05"){may=mnd}
  if (rr[ind]=="06"){jun=mnd}
  if (rr[ind]=="07"){jul=mnd}
  if (rr[ind]=="08"){aug=mnd}
  if (rr[ind]=="09"){sep=mnd}
  if (rr[ind]=="10"){okt=mnd}
  if (rr[ind]=="11"){nov=mnd}
  if (rr[ind]=="12"){dec=mnd}
  }
  if (length(jan$glob)>length(feb$glob)){feb=rbind(feb,'NAN')}
  if (length(jan$glob)>length(mar$glob)){mar=rbind(mar,'NAN')}
  if (length(jan$glob)>length(apr$glob)){apr=rbind(apr,'NAN')}
  if (length(jan$glob)>length(may$glob)){may=rbind(may,'NAN')}
  if (length(jan$glob)>length(jun$glob)){jun=rbind(jun,'NAN')}
  if (length(jan$glob)>length(jul$glob)){jul=rbind(jul,'NAN')}
  if (length(jan$glob)>length(aug$glob)){aug=rbind(aug,'NAN')}
  if (length(jan$glob)>length(sep$glob)){sep=rbind(sep,'NAN')}
  if (length(jan$glob)>length(okt$glob)){okt=rbind(okt,'NAN')}
  if (length(jan$glob)>length(nov$glob)){nov=rbind(nov,'NAN')}
  if (length(jan$glob)>length(dec$glob)){dec=rbind(dec,'NAN')}


  if (period=='Jan') {Dout=data.frame(y=jan$y,val=jan$glob)}
  if (period=='Feb') {Dout=data.frame(y=feb$y,val=feb$glob)}
  if (period=='Mar') {Dout=data.frame(y=mar$y,val=mar$glob)}
  if (period=='Apr') {Dout=data.frame(y=apr$y,val=apr$glob)}
  if (period=='May') {Dout=data.frame(y=may$y,val=may$glob)}
  if (period=='Jun') {Dout=data.frame(y=jun$y,val=jun$glob)}
  if (period=='Jul') {Dout=data.frame(y=jul$y,val=jul$glob)}
  if (period=='Aug') {Dout=data.frame(y=aug$y,val=aug$glob)}
  if (period=='Sep') {Dout=data.frame(y=sep$y,val=sep$glob)}
  if (period=='Oct') {Dout=data.frame(y=oct$y,val=oct$glob)}
  if (period=='Nov') {Dout=data.frame(y=nov$y,val=nov$glob)}
  if (period=='Dec') {Dout=data.frame(y=dec$y,val=dec$glob)}

#  Dout <- data.frame(y=jan$y,Jan=jan$glob,Feb=feb$glob,Mar=mar$glob,Apr=apr$glob,May=may$glob,Jun=jun$glob,Jul=jul$glob,Aug=aug$glob,Sep=sep$glob,Okt=okt$glob,Nov=nov$glob,Dec=dec$glob) # data frame all months
  #
  
  return(Dout)
}
