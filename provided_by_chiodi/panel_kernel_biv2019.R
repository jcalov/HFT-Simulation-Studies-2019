
#rgl.snapshot("rgl1.png")



####################
library(MASS)
library(ks)
library(rgl)

kde2dr=function (x, y, h, r=cor(x,y),n = 25, lims = c(range(x), range(y))) 
{
    nx <- length(x)
    if (length(y) != nx) 
        stop("data vectors must be the same length")
    if (any(!is.finite(x)) || any(!is.finite(y))) 
        stop("missing or infinite values in the data are not allowed")
    if (any(!is.finite(lims))) 
        stop("only finite values are allowed in 'lims'")
    n <- rep(n, length.out = 2L)
    gx <- seq.int(lims[1L], lims[2L], length.out = n[1L])
    gy <- seq.int(lims[3L], lims[4L], length.out = n[2L])
    h <- if (missing(h)) 
        c(bandwidth.nrd(x), bandwidth.nrd(y))
    else rep(h, length.out = 2L)
    h <- h/4
    r1=sqrt(1-r*r)
    ax <- outer(gx, x, "-")/h[1L]
    ay <- outer(gy, y, "-")/h[2L]
    z <- tcrossprod(matrix(dnorm(ax), , nx), matrix(dnorm(r*ax+r1*ay), 
        , nx))/(nx * h[1L] * h[2L])
    list(x = gx, y = gy, z = z)
}



n=200
xsim1=matrix(rnorm(n),n/2,2)
xsim2=matrix(rnorm(n,m=2),n/2,2)
xx=matrix(0,n,2)
xx[,1]=c(xsim1[,1],xsim2[,1])
xx[,2]=c(xsim1[,2],xsim2[,2])
dens= kde2d(xx[,1],xx[,2],n=25)
piano=dens$z^0
std01=function(x) x/max(x)
zz=std01(dens$z)


##################################
kernel.biv.plot=function(x,y,h=2*bw.nrd(x),r=cor(x,y),ngrid=31,col1="black",col2="green",alpha1=0.9,alpha2=0.6)    {
dens= kde2dr(x,y,h=h,r=r,n=ngrid)
n=length(x)
zz =dens$z/max(dens$z)

rgl.surface(dens$x,dens$y,4*zz,fog=F,col=col1,alpha=alpha1,back="lines",front="lines")

rgl.surface(dens$x,dens$y,4*zz, col=rgb(zz,0,1-zz),
alpha=alpha2,fog=FALSE,pixmap="rgb",lit=FALSE,shin=0.0) 

rgl.points(cbind(x,array(0,n),y),col=rgb(0,1,0) )

rgl.surface(dens$x,dens$y,0*zz, col=rgb(zz,0,1-zz) ,alpha=alpha2,fog=FALSE,pixmap="rgb",lit=FALSE,shin=0.0)
title3d(main=paste("h=",as.character(round(h,2))),col=1)
}


library(rpanel)
bg3d("white")
bandwidth2<- function(panel) {# f2
rgl.bringtotop()
rgl.clear()

kernel.biv.plot(panel$x,panel$y, exp(panel$h),r=panel$rho,ngrid=41)
panel
}


rgl.open()
bg3d("white")

pannello <- rp.control(title="bandwidth2",x=xx[,1],y =xx[,2],size=c(500,100), h =bw.nrd(xx[,1]),rho=0)
rp.slider(pannello, h, -2, 1.5, title="log(h)", showvalue=TRUE,bandwidth2)
rp.slider(pannello, rho, -0.99, 0.99, showvalue=TRUE,bandwidth2)
# rgl.snapshot( filename, fmt="png", top=TRUE )
#   rgl.snapshot( "kernelbiv1.png", fmt="png", top=TRUE )

