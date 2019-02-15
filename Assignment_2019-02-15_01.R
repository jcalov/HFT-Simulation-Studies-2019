# Assignment 1 from 2019-02-15


integrateApprox <- function(nSamples = 1000, xMin = 0, xMax = 1, stepWidth = 0.001, subAssignment = "a") {
  
  h = switch(subAssignment,
              a = function(x) (x^1 + exp(x)) / (1 + x^2),
              b = function(x) x^2,
              c = function(x) sin(x^2 + 1)
  )
  
  xVec = seq(xMin, xMax, stepWidth)
  yVec = h(xvec)
  
  yMin = min(yVec)
  yMax = max(yVec)
  
  xRandom = runif(nSamples, min=xMin, max=xMax)
  yRandom = runif(nSamples, min=yMin, max=yMax)
  
  xRandom2 = c(xMin, xRandom, xMax)
  yRandom2 = c(yMin, yRandom, yMax)
  
  plotTitle = paste("Assignment 1", subAssignment, ": Montecarlo hit-or-miss \n#samples = ", nSamples, sep = "")
  
  # plot function
  plot(c(xMin, xVec, xMax), c(yMin, yVec, yMax), col="black", type="l", xlim=c(xMin, xMax), main = plotTitle, xaxs="i", yaxs="i", xlab="x", ylab="h(x)")
  
  # plot points
  points(xRandom, yRandom, col = (yRandom <= h(xRandom)) * 1 + 2, pch=".")
  
  # We don´t transform x intervall because it is not necessary for the assignment (x \in (0, 1))
  
  # Ratio of random points under the function h(x) to all random points
  integralEstimation = sum(yRandom <= h(xRandom)) / nSamples

  # Error like on slide 16 of the presentation
  estimatedErrorMin = (xMax - xMin) * (integralEstimation - 1.96 * sqrt((integralEstimation * (1 - integralEstimation)) / nSamples))
  estimatedErrorMax = (xMax - xMin) * (integralEstimation + 1.96 * sqrt((integralEstimation * (1 - integralEstimation)) / nSamples))
  
  # Print to console  
  cat(plotTitle)
  cat(paste("Integral estimation: "), integralEstimation)
  cat(paste("Confidence intervall: from"), estimatedErrorMin, "to", estimatedErrorMax)
  
}

integrateApprox(subAssignment = "a", nSamples = 10000)
integrateApprox(subAssignment = "b", nSamples = 10000)
integrateApprox(subAssignment = "c", nSamples = 10000)
