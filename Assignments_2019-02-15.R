#
#
# Assignments from 2019-02-15
#
#
groupName = "MOSC"

pdf(paste("Group_", groupName, "_Assignments_2019-02-15.pdf", sep = ""))

#
#
# Assignment 1
#
#
integrateApprox <- function(nSamples = 1000, xMin = 0, xMax = 1, stepWidth = 0.001, subAssignment = "a") {
  
  h = switch(subAssignment,
              a = function(x) (x^1 + exp(x)) / (1 + x^2),
              b = function(x) x^2,
              c = function(x) sin(x^2 + 1)
  )
  
  xVec = seq(xMin, xMax, stepWidth)
  yVec = h(xVec)
  
  yMin = min(yVec)
  yMax = max(yVec)
  
  xRandom = runif(nSamples, min=xMin, max=xMax)
  yRandom = runif(nSamples, min=yMin, max=yMax)
  
  xRandom2 = c(xMin, xRandom, xMax)
  yRandom2 = c(yMin, yRandom, yMax)
  
  plotTitle = paste("Assignment 1", subAssignment, ": Montecarlo hit-or-miss \n#samples = ", nSamples, sep = "")
  
  # We don´t transform x intervall because it is not necessary for the assignment (x \in (0, 1))
  
  # Ratio of random points under the function h(x) to all random points
  integralEstimation = sum(yRandom <= h(xRandom)) / nSamples

  # Error like on slide 16 of the presentation
  estimatedErrorMin = (xMax - xMin) * (integralEstimation - 1.96 * sqrt((integralEstimation * (1 - integralEstimation)) / nSamples))
  estimatedErrorMax = (xMax - xMin) * (integralEstimation + 1.96 * sqrt((integralEstimation * (1 - integralEstimation)) / nSamples))
  
  # Print to console  
  cat(plotTitle)
  cat(paste("  Integral estimation: "), integralEstimation)
  cat(paste("  Confidence intervall: from"), estimatedErrorMin, "to", estimatedErrorMax)
  
  # plot function
  plot(c(xMin, xVec, xMax), c(yMin, yVec, yMax), col="black", type="l", xlim=c(xMin, xMax), main = plotTitle, xaxs="i", yaxs="i", xlab="x", ylab="h(x)")
  
  # plot points
  points(xRandom, yRandom, col = (yRandom <= h(xRandom)) * 1 + 2, pch=".")
  
}

integrateApprox(subAssignment = "a", nSamples = 10000)
integrateApprox(subAssignment = "b", nSamples = 10000)
integrateApprox(subAssignment = "c", nSamples = 10000)



#
#
# Assignment 2
#
#
generateNumbers <- function(nSamples = 10000, subAssignment = "a") {
  
  probabilities = switch(subAssignment,
             a = c(.3, .4, .3),
             b = c(.7, .18, .12)
  )
  
  probabilitiesUniformed = probabilities / max(probabilities)
  
  U = runif(nSamples)
  V = runif(nSamples)
  I = trunc(length(probabilities) * U + 1)
  
  result = probabilitiesUniformed[I] > V
  
  plotTitle = paste("Assignment 2", subAssignment, ": Acceptance rejection \n#samples = ", nSamples, "\np1=", probabilities[1], ", p2=", probabilities[2], ", p3=", probabilities[3], sep = "")
  
  # plot histogram
  hist(I[result], main = plotTitle)
  
}

generateNumbers(nSamples = 1000, subAssignment = "a")
generateNumbers(nSamples = 10000, subAssignment = "a")
generateNumbers(nSamples = 1000000, subAssignment = "a")
generateNumbers(nSamples = 1000, subAssignment = "b")
generateNumbers(nSamples = 10000, subAssignment = "b")
generateNumbers(nSamples = 1000000, subAssignment = "b")



#
#
# Assignment 3
# only fully implemented part a
#
draWSamples <- function(n = 1000, m = 5, lambda = 5, numberClasses = 50, subAssignment = "a") {
  
  xMin = 0
  xMax = 1
  stepWidth = 0.01
  
  randomNumbers = switch(subAssignment,
                         a = rpois(n * m, lambda),
                         b = runif(n * m)
  )
  
  
  randomNumbers = matrix(randomNumbers, n, m)
  
  means = apply(randomNumbers, 1, mean)
  meanAll = mean(means)
  standardDeviation = sd(means)
  
  
  xVec = seq(xMin, xMax, by = stepWidth)
  
  
  plotTitle = paste("Assignment 3", subAssignment, ": n= ", n, ", m=", m, sep = "")
  
  # plot histogram
  hist(means, nclass = numberClasses, freq = FALSE, main = plotTitle)
  
}

draWSamples(subAssignment = "a", n = 10000, m = 5, lambda = 5)
draWSamples(subAssignment = "a", n = 1000, m = 5, lambda = 7)
draWSamples(subAssignment = "a", n = 100000, m = 5, lambda = 3)


# close PDF file
dev.off()