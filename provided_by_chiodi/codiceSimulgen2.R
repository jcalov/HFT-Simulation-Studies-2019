
univariate.samples<-	function(nsamples=10000,n=5,modello="normal",parm=c(0,1)){
			if(missing(parm))	parm	<-	paramdefaults[[modello]]
	nsamples	=	min(sup.nsamples,trunc(abs(nsamples)))
	n		=	min(sup.n,trunc(abs(n)))
	x		=	unigenera(nsamples*n,distribution=modello,par=parm)
	x		=	matrix(x,nsamples,n)
			return(list(x=x,m=apply(x,1,mean), median=apply(x,1,median),
				central=apply(x,1,central),var.n=(n-1)*apply(x,1,var)/n
))	}			
