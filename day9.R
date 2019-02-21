#
# Session 2019-02-21
#

# library by Marcello Chiodi
library(MLANP)

help("ksmooth")
library(KernSmooth)


# load data from external file provided by Marcello Chiodi
dat = read.table("ese_antr1.txt", head = T)



# compare groups
#  N = normal
#  L = long
#  B = fat
#  A = ? 
table(dat$HABITUS)
plot( density(dat$lungh[dat$HABITUS == "N"]), col = 1, type = "l")
lines(density(dat$lungh[dat$HABITUS == "L"]), col = 2)
lines(density(dat$lungh[dat$HABITUS == "A"]), col = 3)
lines(density(dat$lungh[dat$HABITUS == "B"]), col = 4)


height = dat$lungh
weight = dat$peso

smoothy_bandwidth = 6.0
smoothy_kernel = "normal"
polynomy_bandwidth = 3.0
polynomy_degree = 2

smoothy = ksmooth(height, weight, kernel = smoothy_kernel, bandwidth = smoothy_bandwidth)
polynomy = locpoly(height, weight, bandwidth = polynomy_bandwidth, degree = polynomy_degree)

plot(height, weight, col = "gray")
lines(smoothy, col = "red")
lines(polynomy, col = "blue")
lines(lowess(height, weight), col = "green")

legend("topleft", legend=c("ksmooth", "locpoly", "lowess"), col=c("red", "blue", "green"), lty="solid", cex = 1.0)


help("MLA.explor.plot2D")
MLA.explor.plot2D(height, weight)

help("lowess")





#
# last exercise
#


dat2 = dat[,4:8]

pairs(dat2)

MLA.explor.pairs(dat2[,c(1,3,4,5)])

x11()
plot(dat2[,c(3,5)], col = dat2[,2])
lines(lowess(dat2[dat2$HABITUS == "A",c(3,5)]), col = 1, lwd = 137)
lines(lowess(dat2[dat2$HABITUS == "B",c(3,5)]), col = 2, lwd = 37)
lines(lowess(dat2[dat2$HABITUS == "L",c(3,5)]), col = 3, lwd = 37)
lines(lowess(dat2[dat2[,2] == "N",c(3,5)]), col = 4, lwd = 37)
lines(lowess(dat2[,c(3,5)]), col = "gray", lwd = 13)



# height over age
plot(dat2[,c(1,3)], col = dat2[,2])
for (i in 1:nlevels(dat2$HABITUS)) {
  lines(lowess(dat2[dat2$HABITUS == levels(dat2$HABITUS)[i],c(1,3)]), col = i, lwd = 4)
}
lines(lowess(dat2[,c(1,3)]), col = "gray", lwd = 2)

