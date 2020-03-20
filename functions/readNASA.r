# read NASA

readNASA <- function(filename,period){
  Din <- read.table(file=filename,header=TRUE,skip=1,sep=",")
  
  if (period=='January') {tmp_in=Din$Jan}
  if (period=='February') {tmp_in=Din$Feb}
  if (period=='March') {tmp_in=Din$Mar}
  if (period=='April') {tmp_in=Din$Apr}
  if (period=='May') {tmp_in=Din$May}
  if (period=='June') {tmp_in=Din$Jun}
  if (period=='July') {tmp_in=Din$Jul}
  if (period=='August') {tmp_in=Din$Aug}
  if (period=='September') {tmp_in=Din$Sep}
  if (period=='October') {tmp_in=Din$Oct}
  if (period=='Noveber') {tmp_in=Din$Nov}
  if (period=='December') {tmp_in=Din$Dec}
  if (period=='Yearly') {tmp_in=Din$J.D}

  D <- data.frame(y=Din$Year,val=tmp_in)
 
  tmp1<-as.numeric(as.character(D$val[which(D$val!="***")]))
  tmp2<-D$y[which(D$val!="***")]
 
   D <-data.frame(y=tmp2,val=tmp1) # Remove missing values "***"
  



  return(D)
}


