#
# Session 2019-02-20
#

# library by Marcello Chiodi
library(MLANP)

# create plots of 1-, 2- and 3- dimensional distributions
help("dimensions")
dimensions()
dimensions(distribution = "normal")
dimensions(n = 1000, distribution = "normal")




#
# afternoon exercise
#

# load data from external file provided by Marcello Chiodi
dat = read.table("ese_antr1.txt", head = T)

# use following properties and plot a histogram with density:
#  * age
#  * lungh = lengths
#  * peso = weight
#  * torace = chest
#  * span

plotHistogramWithDensites <- function(
    x, 
    dataName = "very interesting data", 
    densityAdjustFactors = c(1.0, 2.0, 0.5), 
    kernels = c("gaussian", "epanechnikov")
    ) {
  
  # probability = TRUE makes integral equals one
  hist(x, nclass = round(length(dat$AGE)/40), probability = TRUE, main = dataName)
  
  # draw kernel density estimation with given adjust factors for default bandwidth
  # used bandwidth is actually bw * adjust
  legends = vector()
  legendColors = vector()
  colorCounter = 1
  for (i in 1:length(densityAdjustFactors)) {
    for (j in 1:length(kernels)) {
      lineColor = colorCounter
      colorCounter = colorCounter + 1
      lineLegend = paste(kernels[j], densityAdjustFactors[i])
      legends = c(legends, lineLegend)
      legendColors = c(legendColors, lineColor)
      # TODO for loop for bandwidth bw --- , bw = "ucv" (for cross validation)
      lines(density(x, adjust = densityAdjustFactors[i], kernel = kernels[j]), col = lineColor, lwd = 1)
    }
  }
  
  legend("topright", legend=legends, col=legendColors, lty="solid", cex = 0.8)
}
plotHistogramWithDensites(dat$peso, dataName =  "weight in kg")

pdf("bla_blubb_1.pdf")
plotHistogramWithDensites(dat$AGE, dataName =  "age in years", densityAdjustFactors = c(10, 2, .5, 5, 2, .1))
plotHistogramWithDensites(dat$lungh, dataName = "length in cm")
plotHistogramWithDensites(dat$peso, dataName =  "weight in kg")
plotHistogramWithDensites(dat$TORACECM, dataName =  "chest")
plotHistogramWithDensites(dat$SPANCM, dataName =  "span")
plotHistogramWithDensites(dat$SPANCM)
plotHistogramWithDensites(dat$SPANCM, densityAdjustFactors = c(1.0))
dev.off()



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
