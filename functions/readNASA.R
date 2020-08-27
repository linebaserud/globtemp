# read NASA

readNASA <- function(filename,period){
  # read from file or web
  if(!is.na(filename)){Din <- read.table(file=filename,header=TRUE,skip=1,sep=",")}
  if(is.na(filename)){
    Din <- read.csv(url("https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv"),header=TRUE,skip=1,sep=",")
    print("Reading NASA/GISS data from https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv")  
  }

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
 
  if (period =='Yearly'){ 
    for (i in (1:length(tmp_in))){
      if (tmp_in[i]=="***"){
        ly=Din[i,1]
        ysf=Din[i,which(Din[i,2:13]!="***")+1]
        ysf_mean =round(sum(ysf)/length(ysf),2)
        print(paste0("Calculating ",ly," ",period," value ..."))
     
      if (nchar(as.character(length(ysf)))>1){lm=as.character(length(ysf))} 
      else {lm=paste0("0",as.character(length(ysf)))}
      
      }
    }
  D=rbind(D,data.frame(y=ly,val=ysf_mean))
  } else {
      ly=NA
      lm=NA
  }


  return(list(D,last_NASA=paste0(ly,"/",lm)))
}


