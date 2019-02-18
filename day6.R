#
# Session 2019-02-18
#

# library by Marcello Chiodi
library(MLANP)

# Histograms to play with
n = 10000000
x = rnorm(n)
hist(x, nclass = 100)

# Kernel estimator by Chiodi (GUI)
# compare slides 61++
help("draw1")
draw1(n = 10)
x11()


# another example
x = c(1, 3, 4, 6, 8, 9, 10, 15, 20, 22)
draw1(x)
x11()

# another example
x1 = rnorm(20, mean = -1)
x2 = rnorm(20, mean = +1)
x = c(x1, x2)
draw1(x)
x11()


# calculate bandwidth h after silverman rule (slide 127)
x = rnorm(20)
n = length(x)
h = (4 / (3 * n))^(1 / 5) * sd(x)
draw1(x = x, hmin = h / 10, hmax = h * 2)
x11()


# mixture of two normal distributions
# \[ f(x) = p \, f_1(x) + (1-p) \, f_2(x) \]
help("rnormal.mix2")
hist(rnormal.mix2(100000), nc = 100)

# rnormal.mix2 with draw1()
n = 100
x = rnormal.mix2(n)
h = (4 / (3 * n))^(1 / 5) * sd(x)
h
draw1(x, hmin = h, hmax = h)
x11()


# load data from external file provided by Marcello Chiodi
# and make things
dat = read.table("ese_antr1.txt", head = T)
x = dat$lungh
hist(x)
x1 = sort(sample(complete(dat$lungh), 20))
# plot of histogram and density function (step function)
ripart(x1, main = "n=20", xlab = "height")

n = length(x)

draw1(x)
x11()

# manually scale data for plotting
z = (x - mean(x)) / sd(x)
# calculate bandwidth with silverman rule
h = (4 / (3 * length(z)))^(1 / 5) * sd(z)
draw1(z)
x11()
ripart(z)
