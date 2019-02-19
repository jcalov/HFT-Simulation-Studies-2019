#
# Session 2019-02-19
#

# library by Marcello Chiodi
library(MLANP)


# load data from external file provided by Marcello Chiodi
# and make things
dat = read.table("ese_antr1.txt", head = T)
x = dat$lungh
hist(x, nclass = 40, probability = TRUE)
# draw default kernel density estimation
lines(density(x), col = "red")
# draw kernel density estimation with an adjust factor for default bandwidth
# used bandwidth is actually bw * adjust
lines(density(x, adjust = 2.0), col = "green")
lines(density(x, adjust = 0.5), col = "blue")

# to be more impressive to others^^
# to give more ordered impression
plot(density(x, adjust = 2.0), type = "l", col = "blue")

# with bandwidth calculated by silverman rule
h = (4 / (3 * length(x)))^0.2 * sd(x)
hist(x, nclass = 40, probability = TRUE)
lines(density(x), col = "red")
lines(density(x, bw = h), col = "blue")



# simulations like on slide 89
# colored lines are explained on slide 106
help(simul.normalmix2)
simul.normalmix2(n = 100)
hist(rnormal.mix2(100))
#simul.normalmix2(n = 100, ikey = TRUE)


# more examples (playing with parameters)
# n is sample size
# ncamp is number of generated samples
# tpause is pause between drawing samples
# hs is bandwidth h
simul.normalmix2(n = 100, ncamp = 1000, tpause = 0, hs = 0.05)
simul.normalmix2(n = 100, ncamp = 1000, tpause = 0, hs = 0.25)
simul.normalmix2(n = 100, ncamp = 1000, tpause = 0, hs = 0.50)
simul.normalmix2(n = 100, ncamp = 1000, tpause = 0, hs = 1.00)
simul.normalmix2(n = 100, ncamp = 1000, tpause = 0, hs = 2.00)
simul.normalmix2(n = 10000, ncamp = 1000, tpause = 0)



#
# afternoon...
#

# now we work with the data and without plots
s1 = simul.normalmix2(n = 25, ncamp = 500, plot = FALSE, ikey = FALSE, tpause = 0, hs = 0.25)
# content of s1
# n:
#   sample size
# ncamp:
#   number of generated samples
# xgrid:
#   x values to calcute and draw the things
# mat:
#   ncamp rows, 100 colums
#   each row corresponds to one sample (black lines)
#   (y-values of xgrid x-values)
# true:
#   (green line)
# m:
#   average of all samples on corrsponding x value of xgrid (centered red line)

# calculate mean square errors (MSE = EQM in italian)
hvec = seq(0.1, 1.0, by = 0.05)
e1 = numeric(0) # initialize numeric vector of size 0
for (h in hvec) {
  e = eqm.sim(hs = h, ncamp = 5000)
  e1 = c(e1, e)
  cat(c(h, ":", e, "\t\t"))
}
# errors corresponding to bandwidths
he = cbind(hvec, e1)
plot(he, type = "l")


# again with other examples
# increasing of n gives a lower optimum h
hvec = seq(0.1, 1.0, by = 0.1)
e1 = numeric(0) # initialize numeric vector of size 0
for (h in hvec) {
  e = eqm.sim(hs = h, n = 400, ncamp = 500)
  e1 = c(e1, e)
  cat(c(h, ":", e, "\t\t"))
}
plot(hvec, e1, type = "l")






###########################################
# notes for Marcello Chiodi               #
#                                         #
# * problem of vertical scale of draw1()  #
# * problem of data                       #
# * single sample on the horizontal scale #
# * function eqm.sim                      #
#     change sum(...) to mean(...)        #
###########################################