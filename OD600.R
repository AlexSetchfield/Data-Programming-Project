OD600<-function(ODwan,accurate,t0,OD0,t1,OD1){ #two options, accurate =T or F
  y<-NULL                                        #if accurate=F, only have to input ODwan,t0 are needed
  tw<-NULL                                       #if accurate=T,have to input all arguments
  tx<-NULL
  th<-NULL
  tmin<-NULL
  A<-2.5
  if(accurate==F){    #if accurate==F, the parameters will be setted
  maxrate<-0.00144
  l<-180
  tw<-l-(A*log(-log(ODwan/A))-A)/(maxrate*exp(1)) #the equation convert OD to time
  tx<-tw-t0
  if (tx>60){
    th<-trunc(tx/60)
    tmin<-tx-th*60
    print(paste("Need",th,"Hour",round(tmin,0),"min to reach OD",ODwan))
  } else{
    print(paste("Need",round(tx,0),"min to reach OD",ODwan))
  }
  } else if (accurate==T){    #if accurate==T, the parameters will be calculated by inputted t0,OD0,t1,OD1
  maxrate<-(OD1-OD0)/(t1-t0)  #maxrate
  l<-OD0-maxrate*t0
  tw<-l-(A*log(-log(ODwan/A))-A)/(maxrate*exp(1)) #the equation convert OD to time
  tx<-tw-t0
  if (tx>60){
    th<-trunc(tx/60)
    tmin<-tx-th*60
    print(paste("Need",th,"Hour",round(tmin,0),"min to reach OD",ODwan))
  } else{
    print(paste("Need",round(tx,0),"min to reach OD",ODwan))
  }
  } else {   #if accurate is not specified,
    maxrate<-0.00144
    l<-180
    tw<-l-(A*log(-log(ODwan/A))-A)/(maxrate*exp(1)) #the equation convert OD to time
    tx<-tw-t
    if (tx>60){
      th<-trunc(tx/60)
      tmin<-tx-th*60
      print(paste("Need",th,"Hour",round(tmin,0),"min to reach OD",ODwan))
    } else{
      print(paste("Need",round(tx,0),"min to reach OD",ODwan))
  }
  }
}
