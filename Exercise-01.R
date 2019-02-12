#
# Exercise 1
#
# generate numbers
# m = 100000
# choose ditributions (normal, poisson, ...)
# berechne zufallszahlen mit r____-Funktion
# Berechne ( M - E(X)) / sqrt( Var(X) / m ), das "standardized mean of empirical mean"
# 

makeStuff <- function (n, random_numbers, m.teo, v.teo, name) {
  
  # arithmethisches Mittel
  m = mean(random_numbers)
  
  # Varianz
  v = var(random_numbers)
  
  # result
  result = (m - m.teo) / sqrt(v.teo / n)
  
  # plot histogram
  x11()
  hist(random_numbers, main = c(name, " ", result))
  
  return(result)
}

myUniform <- function(n){ # m Anzahl Zufallszahlen
  m.teo = 0.5 # Theoretischer Erwartungswert
  v.teo = 1 / 12 # Theoretische Varianz
  
  # generate random numbers
  random_numbers = runif(n)
  
  result = makeStuff(n, random_numbers, m.teo, v.teo, name = "Gleichverteilung")
}


myPoisson <- function(n, lambda){ # m Anzahl Zufallszahlen
  m.teo = lambda # Theoretischer Erwartungswert
  v.teo = lambda # Theoretische Varianz
  
  # generate random numbers
  random_numbers = rpois(n, lambda)
  
  makeStuff(n, random_numbers, m.teo, v.teo, name = "Poisson-Verteilung")
}




myNormal <- function(n){ # n Anzahl Zufallszahlen
  m.teo = 0 # Theoretischer Erwartungswert
  v.teo = 1 # Theoretische Varianz
  
  # generate random numbers
  random_numbers = rnorm(n)
  
  makeStuff(n, random_numbers, m.teo, v.teo, name = "Standardnormalverteilung")
}


myExponential <- function(n, lambda){ # n Anzahl Zufallszahlen
  m.teo = 1 / lambda # Theoretischer Erwartungswert
  v.teo = 1 / (lambda^2) # Theoretische Varianz
  
  # generate random numbers
  random_numbers = rnorm(n)
  
  makeStuff(n, random_numbers, m.teo, v.teo, name = "Exponentialverteilung")
}

n = 10000000
myUniform(n)
myPoisson(n, lambda = 3)
myExponential(n, lambda = 3)
myNormal(n)





