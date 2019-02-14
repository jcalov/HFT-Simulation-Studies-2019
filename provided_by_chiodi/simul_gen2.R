#----------------------------------------------------------------------------------------
#
#	Da studiare:
#	Output di testo su finestra indipendente (o su porzione di finestra)
#
#	riadattamento di quasi tutte le procedure di simul2000
#	Background colorato
#	
#	in bivsimul aggiungere rette di regressione	
#----------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------
#	simulR: un ambiente in R di simulazioni didattiche interattive
#	marcello chiodi: prima idea e prime linee di codice 21-7-2004
#	
#	sulla falsariga di Simul2000
#----------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------
#
# Dichiarazioni di ambiente generale
#
#----------------------------------------------------------------------------------------

options(windowsBuffered=FALSE)


paramdefaults=	list(	bivnormalstd	=c(0),
			bivuniformstd	=c(0,1,0,1),
			bivdirichlet	=c(1.1,1.1,1.1),
			uniform		=c(0,1),
			normal		=c(0,1),
			gamma		=c(1,1),
			beta		=c(1.1,1.1),
			binomial	=c(5,.5),
			poisson		=c(1,1),
			logistic	=c(1,1),
			student		=c(2),
			weibull		=c(1,1),
			lognormal	=c(0,1)

			)

limitdefaults=	list(	bivnormalstd	=c(-3.5,3.5,-3.5,3.5),
			bivuniformstd	=c(0,1,0,1),
			bivdirichlet	=c(0,1,0,1),
			uniform		=c(0,1),
			normal		=c(-4,4),
			gamma		=c(0,10),
			beta		=c(0,1),
			binomial	=c(0,1),
			poisson		=c(0,10),
			logistic	=c(-4,4),
			student		=c(-5,5),
			weibull		=c(0,10),
			lognormal	=c(0,10)

			)

#	massimi di n etc.	

		sup.n		=	1000
		sup.nsamples	=	10000000


#----------------------------------------------------------------------------------------
#
#	routines di simulazione a UNA variabile
#
#	per ogni distribuzione "distr" è necessario definire una serie di defaults:
#
#	paramdefaults[[distr]]
#	limitdefaults[[distr]]
#	una definizione di estrazione di vettori pseudo-casuali in unigenera (o bivgenera per le bivariate)
#
#
#
#----------------------------------------------------------------------------------------

univsimul<-function(nsamples=100000,
                    nparz=trunc(nsamples/10), n=1,
			modello="normal",parm=c(0,1),
			nclass	=	50,
			nscreen     =c(1,1),
			newscreen   =TRUE,
			iscreen     =c(1,1),
			singlesample=	FALSE,
			teoplot	=	TRUE,
			tpause	=	0.01,
			colorhist	=	"yellow",
			colorborder	=	"blue",
			colorteo	=	"red",
			colorbg		=	"white",
			dimpoint	=	0.8
			){
	
	npointsteo=300

	colorbg		="grey"

#	graphics.off() 
	
	options(windowsBuffered=(tpause==0))

	lim			<-	limitdefaults[[modello]]

	if(missing(parm))	parm	<-	paramdefaults[[modello]]
	


 
	nsamples	=	min(sup.nsamples,trunc(abs(nsamples)))
	n		=	min(sup.n,trunc(abs(n)))
	nparz		=	trunc(abs(nparz))
	niter		=	trunc(abs(nsamples/nparz))
	nsamples	=	niter*nparz

# par(mfrow=c(2,2))           

	x<-array(0,0)
#	plot.new()
	for(i in 1:niter){	xparz=unigenera(nparz*n,distribution=modello,par=parm)

			if(singlesample){
#				screen(2)
#				plot(c(1,3,4,8))
				
					}
			x<-c(x,xparz)
			nattual	=	length(x)

#		split.screen(c(nscreen,1))
			
erase.screen()

		h	=	hist(x,breaks=nclass,
					col	=	colorhist,
					border	=	colorborder,
					main=paste("random numbers from a ",modello," n=",nattual)				)
			if(teoplot){
				z	=	h$breaks
				nhist	=	length(z)
				zinterval=	z[2]-z[1]
				zteo	=	seq(from=z[1],to=z[nhist],length=npointsteo)
				yteo	=	nattual*zinterval*unidens(zteo,distribution=modello,
						par=parm)
				lines(zteo,yteo,col=colorteo,type="l")
					}
#				close.screen(all = TRUE)  
				}
#
#	FINE ciclo for per la visualizzazione degli istogrammi di simulazione a UNA variabile
#		
			}

#----------------------------------------------------------------------------------------
#
#	FINE routine univsimul di simulazione a UNA variabile
#
#----------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------
#
#	routines di simulazione a DUE variabili
#
#----------------------------------------------------------------------------------------



bivsimul<-function(npoints=100,modello="bivnormalstd",parm=0,nparz=npoints,
			tpause=0.01,color="blue",colorbg="white",dimpoint=0.8){
	
	drawcontour=(modello=="bivnormalstd")

	graphics.off() 
	
	options(windowsBuffered=(tpause==0))

	lim	<-	limitdefaults[[modello]]

	if(missing(parm))	parm	<-	paramdefaults[[modello]]

	
#   grid.display.list(on=TRUE)
#
#   device independent  ??
#	x11()
#
	x11()
	plot(0,0,"n",xlim=c(lim[1],lim[2]),ylim=c(lim[3],lim[4]))





# contour lines
# curve di livello con il contour plot teorico


if(drawcontour){
	 x=seq(lim[1],lim[2], length=50)
	 y=seq(lim[3],lim[4], length=50)


	 z=matrix(0,length(x),length(y))


	 	 for(i in 1:length(x)){
 			for(j in 1:length(y)){z[i,j]=bivnormquadform(c(x[i],y[j]),r=parm[1])
			}}

	contour(x,y,z,add=TRUE,levels=qchisq(seq(0.1,0.9,by=0.1),df=2)	)
		}




	for(i in 1:npoints){
		x<-bivgenera(distribution=modello,par=parm)
		points(x,col=color,pch=20,cex=dimpoint)
		Sys.sleep(tpause)
		}
	options(windowsBuffered=TRUE)
	}

bivgenera<-function(n=1,distribution="bivuniformstd",par=c(0,1)){
	x<-switch(distribution,
		"bivuniformstd"	=	runifbivstd(n),
		"bivnormalstd"	=	rnormbivstd(n,rho=par[1]),
		"bivdirichlet"	=	rdirichlet(n,par)				)
		return(x)
		}


bivnormquadform	<-function(x,mu=c(0,0),s=c(1,1),r=0){
		z<-(x-mu)/s
		d<- (z%*%z-2*r*z[1]*z[2])/(1-r^2)
		return(as.vector(d))
		}


unigenera<-function(n=1,distribution="uniform",par=c(0,1)){
	x=switch(distribution,
		uniform		=	runif(n,min=par[1],max=par[2]),
		normal		=	rnorm(n,mean=par[1],sd=par[2]),
		gamma		=	rgamma(n,shape=par[1],rate=par[2]),
		beta		=	rbeta(n,par[1],par[2]),
		binomial	=	rbinom(n,par[1],par[2]),
		poisson		=	rpois(n,par[1]),
		logistic	=	rlogis(n, location = par[1], scale =par[2]),
		student		=	rt(n,par[1]),
		weibull		=	rweibull(n, par[1], scale = par[2]),
		lognormal	=	rlnorm(n, meanlog = par[1], sdlog = par[2])
							)
		return(x)
		}

unidens<-function(z,distribution="uniform",par=c(0,1)){
	x=switch(distribution,
		uniform		=	dunif(z,min=par[1],max=par[2]),
		normal		=	dnorm(z,mean=par[1],sd=par[2]),
		gamma		=	dgamma(z,shape=par[1],rate=par[2]),
		beta		=	dbeta(z,par[1],par[2]),
		binomial		=	dbinom(z,par[1],par[2]),
		poisson		=	dpois(z,par[1]),
		logistic		=	dlogis(z, location = par[1], scale=par[2]),
		student		=	dt(z,par[1]),
		weibull		=	dweibull(z, par[1], scale = par[2]),
		lognormal	=	dlnorm(z, meanlog = par[1], sdlog = par[2])
							)
		return(x)
		}


# rdirichlet(n,a) 
#
# random number generation of a matrix of n rows
# each from a (k-1)-components dirichlet distribution
# with k parameters given by the elements of the k-components of a
#


rnormbivstd<-function(n,rho=0){
			x<-rnorm(2*n)
			dim(x)<-c(n,2)
			x[,2]<-rho*x[,1]+sqrt(1-rho^2)*x[,2]
			return(x)
			}

runifbivstd<-function(n){
			x<-runif(2*n)
			dim(x)<-c(n,2)
			return(x)
			}


rdirichlet<-function(n,a){
	k	<-length(a)
	x	<-rgamma(n*k,a)
	dim(x)	<-c(k,n)
	y	<-colSums(x)
	x<-t(x/outer(array(1,k),y))
	y<-x[,seq(k-1)]
	dim(y)<-c(n,k-1)
	return(y)}

gridmatrix<-function(a,digits=3){
#		grid.newpage()
		tt	<-	format(a,digits=digits)
		ca	<-	(1:dim(a)[2]-0.5)/dim(a)[2]
		ra	<-	1-(1:dim(a)[1]-0.5)/dim(a)[1]
		xa	<-	outer(ra^0,ca)
		ya	<-	outer(ra,ca^0)
 		grid.text(tt,x=xa,y=ya)
				}


                                #
MLA.first4	= function(x){
		m=	mean(x)
		m2=	var(x)
		m3=	mean((x-m)^3)
		m4=	mean((x-m)^4)
		g1=	m3/(m2^1.5)
		g2=	m4/(m2^2)
		return(list(moments=c(m,m2,m3,m4),b=c(g1,g2)))
		}

#split.screen(c(2,2))
#sc<-1
#screen(sc)
#sc<-sc+1
	
