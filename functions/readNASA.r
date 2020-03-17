# read NASA

readNASA <- function(filename,period){
  Din <- read.table(file=filename,header=TRUE,skip=1,sep=",")
  
  if (period=='Jan') {tmp_in=Din$Jan}
  if (period=='Feb') {tmp_in=Din$Feb}
  if (period=='Mar') {tmp_in=Din$Mar}
  if (period=='Apr') {tmp_in=Din$Apr}
  if (period=='May') {tmp_in=Din$May}
  if (period=='Jun') {tmp_in=Din$Jun}
  if (period=='Jul') {tmp_in=Din$Jul}
  if (period=='Aug') {tmp_in=Din$Aug}
  if (period=='Sep') {tmp_in=Din$Sep}
  if (period=='Oct') {tmp_in=Din$Oct}
  if (period=='Nov') {tmp_in=Din$Nov}
  if (period=='Dec') {tmp_in=Din$Dec}
  if (period=='Yearly') {tmp_in=Din$J.D}

  D <- data.frame(y=Din$Year,val=tmp_in)
 
  tmp1<-as.numeric(as.character(D$val[which(D$val!="***")]))
  tmp2<-D$y[which(D$val!="***")]
 
   D <-data.frame(y=tmp2,val=tmp1) # Remove missing values "***"
  



  return(D)
}


