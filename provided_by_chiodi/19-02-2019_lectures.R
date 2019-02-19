x11()
x=c(1,3,4,6,8,9,10,15,20,22)
draw1(x)
x1=rnorm(20, mean = -1)
x2=rnorm(20, mean = +1)
x=c(x1,x2)
draw1(x)
n=20
x=rnorm(20)
h=sd(x)*(4/(3*n))^(1/5)
x11()
draw1(x)
x11()
hist(rnormal.mix2(100000),nc=100)

#read date from file
dat=read.table("ese_antr1.txt", head=T)
#show the data 
str(dat)

#get specific date from name, assign to vector an draw histogram 
x=dat$lungh
hist(x)

#take 20 sample values from name randomly (sorted)
x1=sort(sample(complete(dat$lungh),20))
#compute and plot empirical distribution function
ripart(x1, main="20", xlab="height")

#take 500 sample values from name randomly (sorted)
x2=sort(sample(complete(dat$lungh),500))

#compute and plot empirical distribution function
ripart(x2, main="500", xlab="height")

library(MLANP)
simul.normalmix()
hist(rnormal.mix2(100))

#simul.normalmix2(n=100,ikey=TRUE)

simul.normalmix2(n=100,ikey=FALSE)
simul.normalmix2
simul.normalmix2(n=100,ncamp=500,ikey=FALSE,t=0,hs=0.25)

str(s1)
eqm.sim

hvec=seq(.1,2.0,by=.1)
e1=numeric(0)
for(h in hvec){
  e=eqm.sim(hs=h,ncamp=500)
  e1=c(e1,e)
}
plot(hvec,e1,type="l")


hvec=seq(.1,2.0,by=.1)
e1=numeric(0)
for(h in hvec){
  e=eqm.sim(hs=h,n=400,ncamp=500)
  e1=c(e1,e)
}
x11()
plot(hvec,e1,type="l")










