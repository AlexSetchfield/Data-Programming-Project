
A<-2.5
maxrate<-0.055
l<-35
t<-seq(0,200, by=0.5) # min


t<-50
y<-A*exp(-exp(maxrate*exp(1)*(l-t)/A+1)) #the equation time to OD 
plot(t,y)

t<-NULL

t<-(A/(maxrate*exp(1)))*log(exp(1),log(exp(1),y/A))-l-1

t<- l-(A/maxrate*exp(1))*(log(-log(y/A))-1)

t<-l-(A*log(-log(y/A))-A)/(maxrate*exp(1)) #the equation convert OD to time 

a*b
