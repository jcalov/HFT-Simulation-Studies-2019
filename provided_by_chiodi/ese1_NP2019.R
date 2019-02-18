# requires neonati2.csv  ese_antr1.txt
# used in lez1
library(MLANP)
data()


# ecdf(x)

dat=read.table("ese_antr1.txt",head=T)



x1=sort(sample(complete(dat$lungh),20))
x2=sort(sample(complete(dat$lungh), 500)    )

 ripart(x1,main="n=20",xlab="height")
 ripart(x2,main="n=500",xlab="height")
ripart(dat$lungh,main="n=1427",xlab="height")

dat2=read.table("neonati2.csv",dec=",",sep=";",head=T)

x=complete(dat2$lunghezza)
ripart(x,main="n=24927",xlab="height at birth",xlim=c(400,600))

x11()
hist(x,nc=100,xlim=c(200,600))

#l=hist(x,nc=500,plot=FALSE)
#hist(x,nc=500,plot=TRUE)

#lines(l$mids,l$density)



