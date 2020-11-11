# read NASA

read_nasa <- function(filename,period){
  cat("-----------------------------------------------------------------------------------------------------------------------------\n")
  # read from file or web
  if(!is.na(filename)){Din <- read.table(file=filename,header=TRUE,skip=1,sep=",")}
  if(is.na(filename)){
    Din <- read.csv(url("https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv"),header=TRUE,skip=1,sep=",")
    print("Reading NASA/GISS data from https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv")  
  }

  if (period=='January') {tmp_in=Din$Jan; mth <- "01"}
  if (period=='February') {tmp_in=Din$Feb; mth <- "02"}
  if (period=='March') {tmp_in=Din$Mar; mth <- "03"}
  if (period=='April') {tmp_in=Din$Apr; mth <- "04"}
  if (period=='May') {tmp_in=Din$May; mth <- "05"}
  if (period=='June') {tmp_in=Din$Jun; mth <-"06"}
  if (period=='July') {tmp_in=Din$Jul; mth <- "07"}
  if (period=='August') {tmp_in=Din$Aug; mth <-"08"}
  if (period=='September') {tmp_in=Din$Sep; mth <- "09"}
  if (period=='October') {tmp_in=Din$Oct; mth <- "10"}
  if (period=='Noveber') {tmp_in=Din$Nov; mth <- "11"}
  if (period=='December') {tmp_in=Din$Dec; mth <- "12"}
  if (period=='Yearly') {tmp_in=Din$J.D; mth <- "Yearly"}

  D <- data.frame(y=Din$Year,val=tmp_in,mth)
 
  # Remove missing values (marked as "***")
  tmp1<-as.numeric(as.character(D$val[which(D$val!="***")]))
  tmp2<-D$y[which(D$val!="***")]
  tmp3<-D$mth[which(D$val!="***")]
  D <-data.frame(y=tmp2,val=tmp1,mth=tmp3) 
 
  # calc yearly value based on available monthly values
  if (period =='Yearly'){  
    for (i in (1:length(tmp_in))){
      if (tmp_in[i]=="***"){
        ly_NASA=Din[i,1]
        ysf=Din[i,which(Din[i,2:13]!="***")+1]
        ysf_mean =round(sum(ysf)/length(ysf),2)
        print(paste0("Calculating ",ly_NASA," yearly value based on available monthly values..."))
     
      if (nchar(as.character(length(ysf)))>1){lm_NASA=as.character(length(ysf))} 
      else {lm_NASA=paste0("0",as.character(length(ysf)))}
      
      }
    }
  D=rbind(D,data.frame(y=ly_NASA,val=ysf_mean,mth='Yearly'))
  } else {
      ly_NASA=tail(D$y,n=1)
      lm_NASA=tail(D$mth,n=1)
  }


  return(list(D,last_NASA=paste0(ly_NASA,"/",lm_NASA)))
}


