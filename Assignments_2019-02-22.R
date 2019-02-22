#
#
# Assignments from 2019-02-22
#
#
groupName = "MOSC"

pdf(paste("Group_", groupName, "_Assignments_2019-02-22.pdf", sep = ""))




#
#
# Assignment 1.1
#
# read the file sample1 200 obs  6 variables
#
#
sample1 = read.table("sample1.txt", head = T)


#
#
# Assignment 1.2
#
# draw the density estimation of the variables weight and height
#
#
weight = sample1$weight
hist(weight, probability = TRUE)
lines(density(weight, adjust = 1.0), col = "blue")
lines(density(weight, adjust = 0.5), col = "gray")
lines(density(weight, adjust = 2.0), col = "green")
legend("topleft", legend = c("default", "raw", "smooth"), col = c("blue", "gray", "green"), lty = 1)

height = sample1$height
hist(height, probability = TRUE)
lines(density(height, adjust = 1.0), col = "blue")
lines(density(height, adjust = 0.5), col = "gray")
lines(density(height, adjust = 2.0), col = "green")
legend("topleft", legend = c("default", "raw", "smooth"), col = c("blue", "gray", "green"), lty = 1)


#
#
# Assignment 1.3
#
# make the same thing separately for the two groups defined by status
#
#
legendaryPlot <- function(dat, columnNumber, datName, legendPosition = "topleft", adjustFactor = 2.1) {
  hist(dat, probability = TRUE, main = datName, xlab = datName)
  lines(density(dat, adjust = adjustFactor), col = "gray")
  lines(density(sample1[sample1$status == TRUE,columnNumber], adjust = adjustFactor), col = "blue")
  lines(density(sample1[sample1$status == FALSE,columnNumber], adjust = adjustFactor), col = "red")
  legend(legendPosition, legend = c("all data", "status: TRUE", "status: FALSE"), col = c("gray", "blue", "red"), lty = 1)
}

legendaryPlot(sample1$weight, 3, "weight")
legendaryPlot(sample1$height, 2, "height")


#
#
# Assignment 1.4
#
# try to draw the two densities for each var on the same graph
#
#
legendaryPlot(sample1$week, 1, "week")
legendaryPlot(sample1$var5, 5, "var5", legendPosition = "topright")
legendaryPlot(sample1$head, 6, "head", adjustFactor = 4.7)


#
#
# Assignment 2
#
# draw the non parametric regression lines using the 
# height as a function of week for the whole sample 
# and for the two groups defined by status
#
#
plot(sample1[,c(1,2)], col = as.integer(sample1[,4]) * 2 + 2)
lines(lowess(sample1[,c(1,2)]), col = "gray")
lines(lowess(sample1[sample1$status,c(1,2)]), col = 4, lwd = 1)
lines(lowess(sample1[!sample1$status,c(1,2)]), col = 2, lwd = 1)
legend("topleft", legend = c("all data", "status: TRUE", "status: FALSE"), col = c("gray", "blue", "red"), lty = 1)




# close PDF file
dev.off()