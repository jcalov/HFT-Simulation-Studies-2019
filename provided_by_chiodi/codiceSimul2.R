unigenera<-function(n=1,distribution="uniform",par=c(0,1)){
	x=switch(distribution,
		"uniform"	=	runif(n,min=par[1],max=par[2]),
		"normal"	=	rnorm(n,mean=par[1],sd=par[2]),
		"gamma"		=	rgamma(n,shape=par[1],rate=par[2]),
		"beta"		=	rbeta(n,par[1],par[2]),
		"binomial"	=	rbinom(n,par[1],par[2]),
		"poisson"	=	rpois(n,par[1]),
		"logistic"	=	rlogis(n, location = par[1], scale =par[2]),
		"student"	=	rt(n,par[1]),
		"weibull"	=	rweibull(n, par[1], scale = par[2]),
		"lognormal"	=	rlnorm(n, meanlog = par[1], sdlog = par[2])
							)
		return(x)
		}
