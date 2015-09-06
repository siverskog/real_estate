########################################################################################
##### REAL ESTATE PROJECT
##### By Jonathan Siverskog
##### 2015-09-04
#####
##### PART 00: DEMO FOR FLEXIBLE COPULA FROM ZIMMER (2012) AND HO ET AL. (2015)
#####
########################################################################################

install <- function(packages) {
  for(i in 1:length(packages)) {
    if(!is.element(packages[i], installed.packages()[,1])) install.packages(packages[i])
    library(packages[i], character.only = TRUE)
  }
  warnings()
}

packages <- c("foreign", "fGarch", "np", "boot", "copula")

install(packages)

########################################################################################
##### SETUP ###############################################################
########################################################################################

data <- read.csv(file = "https://raw.githubusercontent.com/siverskog/real_estate/master/housing_data.csv", header = TRUE)
x.name <- "california"
y.name <- "arizona"

ckertype <- "gaussian"
nmulti <- 30
bwtype <- "adaptive_nn"
n.eval <- 100
grid <- seq(-2, 2, by=0.1)
num.R <- 499
tipo <- 1

########################################################################################
##### APPLY AR-GARCH-FILTER TO DATA ########################################################################
########################################################################################

x <- ts(data[,x.name])
y <- ts(data[,y.name])
ar.garch.x <- garchFit(formula=~arma(1,0)+garch(1,1),data=x,trace=FALSE)
ar.garch.y <- garchFit(formula=~arma(1,0)+garch(1,1),data=y,trace=FALSE)
res.x <- ar.garch.x@residuals/ar.garch.x@sigma.t
res.y <- ar.garch.y@residuals/ar.garch.y@sigma.t
data.res <- data.frame(res.x, res.y)

########################################################################################
##### COMPUTE BANDWIDTH FOR CDF ########################################################
########################################################################################

bw.cdf <- npudistbw(~ res.x + res.y,
                    data = data.res,
                    ckertype = ckertype,
                    bwmethod = "cv.cdf",
                    nmulti = nmulti,
                    bwtype = bwtype)

########################################################################################
##### FUNCTION TO PASS THROUGH BOOTSTRAP  ##############################################
########################################################################################

func <- function(data, x.name, y.name, bw.cdf, grid, bwtype){
  
  x <- ts(data[,x.name])
  y <- ts(data[,y.name])
  ar.garch.x <- garchFit(formula=~arma(1,0)+garch(1,1),data=x,trace=FALSE)
  ar.garch.y <- garchFit(formula=~arma(1,0)+garch(1,1),data=y,trace=FALSE)
  res.x <- ar.garch.x@residuals/ar.garch.x@sigma.t
  res.y <- ar.garch.y@residuals/ar.garch.y@sigma.t
  data.res <- data.frame(res.x, res.y)
  
  ucdf.x <- npudistbw(~res.x,data=data.res,bws=bw.cdf$bw[1],bandwidth.compute=FALSE,bwtype=bwtype)
  u.x <- fitted(npudist(bws=ucdf.x,newdata=data.frame(res.x=grid)))
  ucdf.y <- npudistbw(~res.y,data=data.res,bws=bw.cdf$bw[1],bandwidth.compute=FALSE,bwtype=bwtype)
  u.y <- fitted(npudist(bws=ucdf.y,newdata=data.frame(res.y=grid)))
  u.x.y <- cbind(u.x,u.y)
  copula.x.y <- npcopula(bws=bw.cdf,data=data.res,u=u.x.y)
  C <- diag(matrix(copula.x.y$copula,length(grid),length(grid)))
  
  return(c((C/u.y)[1:(ceiling(length(grid)/2))],((1-u.x-u.y+C)/(1-u.y))[(ceiling(length(grid)/2)):length(grid)]))
  
}

########################################################################################
##### BOOTSTRAP ########################################################################
########################################################################################

boot.copula <- tsboot(data,func,
                      R = 500,
                      l = 20,
                      sim = "fixed",
                      n.sim = nrow(data),
                      x.name = "arizona",
                      y.name = "california",
                      bw.cdf = bw.cdf,
                      grid = grid,
                      bwtype = bwtype)

warnings()

boot.copula.m <- apply(boot.copula$t,2,FUN=mean)
boot.copula.sd <- apply(boot.copula$t,2,FUN=sd)
boot.copula.q <- t(apply(boot.copula$t,2,FUN=quantile,probs=c(0.05,0.95)))

########################################################################################
##### EMPIRICAL COPULA ###############################################################
########################################################################################

x.ucdf <- ecdf(data.res$res.x)
em.u.x <- x.ucdf(grid)
y.ucdf <- ecdf(data.res$res.y)
em.u.y <- y.ucdf(grid)

U.x.y <- pobs(x=cbind(data.res$res.x,data.res$res.y))
u.x.y <- as.matrix(expand.grid(em.u.x,em.u.y))
C.n.x.y <- C.n(u=u.x.y,U=U.x.y)
Cn <- diag(matrix(C.n.x.y,length(grid),length(grid)))

########################################################################################
##### SMOOTH NONPARAMETIRC COPULA ######################################################
########################################################################################

ucdf.x <- npudistbw(~res.x,data=data.res,bws=bw.cdf$bw[1],bandwidth.compute=FALSE,bwtype=bwtype)
u.x <- fitted(npudist(bws=ucdf.x,newdata=data.frame(res.x=grid)))
ucdf.y <- npudistbw(~res.y,data=data.res,bws=bw.cdf$bw[1],bandwidth.compute=FALSE,bwtype=bwtype)
u.y <- fitted(npudist(bws=ucdf.y,newdata=data.frame(res.y=grid)))
u.x.y <- cbind(u.x,u.y)
copula.x.y <- npcopula(bws=bw.cdf,data=data.res,u=u.x.y)
C <- diag(matrix(copula.x.y$copula,length(grid),length(grid)))

########################################################################################
##### PLOT #############################################################################
########################################################################################

if(tipo==1){ylimit <-range((C/u.y)[1:21]+1.96*boot.copula.sd[1:21],
                            (C/u.y)[1:21]-1.96*boot.copula.sd[1:21],
                            ((1-u.y-u.x+C)/(1-u.y))[21:41]+1.96*boot.copula.sd[22:42],
                            ((1-u.y-u.x+C)/(1-u.y))[21:41]-1.96*boot.copula.sd[22:42])
}else{ylimit <- range(boot.copula.q[1:42,])}

plot(NULL,xlim=c(-2,2),ylim=ylimit,xlab="Filtered Price Change",ylab="",main=paste(y.name, "/", x.name, sep = ""),las=1)

if(tipo==1){
  L.l <- c((C/u.y)[1:21]-1.96*boot.copula.sd[1:21]); U.l <- c((C/u.y)[1:21]+1.96*boot.copula.sd[1:21])
  L.r <- c(((1-u.y-u.x+C)/(1-u.y))[21:41]-1.96*boot.copula.sd[22:42]); U.r <- c(((1-u.y-u.x+C)/(1-u.y))[21:41]+1.96*boot.copula.sd[22:42])
  polygon(c(grid[1:21],rev(grid[1:21])),c(L.l,rev(U.l)),col="lightgray",border=FALSE)
  polygon(c(grid[21:41],rev(grid[21:41])),c(L.r,rev(U.r)),col="lightgray",border=FALSE)
}else{
  lines(grid[1:21],boot.copula.q[1:21,1],lwd=3,lty=2)
  lines(grid[1:21],boot.copula.q[1:21,2],lwd=3,lty=2)
  lines(grid[21:41],boot.copula.q[22:42,1],lwd=3,lty=2)
  lines(grid[21:41],boot.copula.q[22:42,2],lwd=3,lty=2)
}

points(grid[1:21],(Cn/em.u.y)[1:21], type = "b", pch = "+", lty = 3,col="black")
points(grid[21:41],((1-em.u.y-em.u.x+Cn)/(1-em.u.y))[21:41], type="b", pch="+", lty=3, col="black")
lines(grid[1:21],(C/u.y)[1:21],lwd=2,lty=1,col="red")
lines(grid[21:41],((1-u.y-u.x+C)/(1-u.y))[21:41],lwd=2,lty=1,col="red")
abline(v=0,lty=1,lwd=1)

