########################################################################################
##### REAL ESTATE PROJECT
##### By Jonathan Siverskog
##### 2015-08-28
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
##### LOAD AND PLOT DATA ###############################################################
########################################################################################

datos <- read.csv(file = "https://raw.githubusercontent.com/siverskog/real_estate/master/housing_data.csv", header = TRUE)

AZ <- ts(datos$arizona,start=c(1975,2),end=c(2009,1),frequency=4)
CA <- ts(datos$california,start=c(1975,2),end=c(2009,1),frequency=4)
FL <- ts(datos$florida,start=c(1975,2),end=c(2009,1),frequency=4)
NV <- ts(datos$nevada,start=c(1975,2),end=c(2009,1),frequency=4)

par(mar = c(2,3,2,1), mfrow = c(2,2))
plot(AZ, las = 1, main = "AZ")
plot(CA, las = 1, main = "CA")
plot(FL, las = 1, main = "FL")
plot(NV, las = 1, main = "NV")

########################################################################################
##### APPLY AR-GARCH-FILTER ############################################################
########################################################################################

ar.garch.AZ <- garchFit(formula=~arma(1,0)+garch(1,1),data=AZ,trace=FALSE)
ar.garch.CA <- garchFit(formula=~arma(1,0)+garch(1,1),data=CA,trace=FALSE)
ar.garch.FL <- garchFit(formula=~arma(1,0)+garch(1,1),data=FL,trace=FALSE)
ar.garch.NV <- garchFit(formula=~arma(1,0)+garch(1,1),data=NV,trace=FALSE)

res.AZ <- ar.garch.AZ@residuals/ar.garch.AZ@sigma.t
res.CA <- ar.garch.CA@residuals/ar.garch.CA@sigma.t
res.FL <- ar.garch.FL@residuals/ar.garch.FL@sigma.t
res.NV <- ar.garch.NV@residuals/ar.garch.NV@sigma.t

par(mar = c(2,3,2,1), mfrow = c(2,2))
plot(res.AZ, las = 1, main = "AZ", type = "l")
plot(res.CA, las = 1, main = "CA", type = "l")
plot(res.FL, las = 1, main = "FL", type = "l")
plot(res.NV, las = 1, main = "NV", type = "l")

datos.res <- data.frame(res.AZ=res.AZ,res.CA=res.CA,res.FL=res.FL,res.NV=res.NV)

########################################################################################
##### ... ############################################################
########################################################################################

ckertype <- "gaussian"
nmulti <- 30
bwtype <- "adaptive_nn"
n.eval <- 100

bw.cdf.CA.NV <- npudistbw(~res.CA+res.NV,data=datos.res,ckertype=ckertype,bwmethod="cv.cdf",nmulti=nmulti,bwtype=bwtype)
bw.cdf.CA.AZ <- npudistbw(~res.CA+res.AZ,data=datos.res,ckertype=ckertype,bwmethod="cv.cdf",nmulti=nmulti,bwtype=bwtype)
bw.cdf.CA.FL <- npudistbw(~res.CA+res.FL,data=datos.res,ckertype=ckertype,bwmethod="cv.cdf",nmulti=nmulti,bwtype=bwtype)
bw.cdf.NV.AZ <- npudistbw(~res.NV+res.AZ,data=datos.res,ckertype=ckertype,bwmethod="cv.cdf",nmulti=nmulti,bwtype=bwtype)
bw.cdf.NV.FL <- npudistbw(~res.NV+res.FL,data=datos.res,ckertype=ckertype,bwmethod="cv.cdf",nmulti=nmulti,bwtype=bwtype)
bw.cdf.AZ.FL <- npudistbw(~res.AZ+res.FL,data=datos.res,ckertype=ckertype,bwmethod="cv.cdf",nmulti=nmulti,bwtype=bwtype)

bw.pdf.CA.NV <- npudensbw(~res.CA+res.NV,data=datos.res,ckertype=ckertype,bwmethod="cv.ls",nmulti=nmulti,bwtype=bwtype)
bw.pdf.CA.AZ <- npudensbw(~res.CA+res.AZ,data=datos.res,ckertype=ckertype,bwmethod="cv.ls",nmulti=nmulti,bwtype=bwtype)
bw.pdf.CA.FL <- npudensbw(~res.CA+res.FL,data=datos.res,ckertype=ckertype,bwmethod="cv.ls",nmulti=nmulti,bwtype=bwtype)
bw.pdf.NV.AZ <- npudensbw(~res.NV+res.AZ,data=datos.res,ckertype=ckertype,bwmethod="cv.ls",nmulti=nmulti,bwtype=bwtype)
bw.pdf.NV.FL <- npudensbw(~res.NV+res.FL,data=datos.res,ckertype=ckertype,bwmethod="cv.ls",nmulti=nmulti,bwtype=bwtype)
bw.pdf.AZ.FL <- npudensbw(~res.AZ+res.FL,data=datos.res,ckertype=ckertype,bwmethod="cv.ls",nmulti=nmulti,bwtype=bwtype)

save(list=ls(),file="bw0.Rdata")

########################################################################################
##### ... ############################################################
########################################################################################

rm(list=ls())
load(file="bw0.Rdata")
num.R <- 499

colas <- function(X,bw.cdf.CA.AZ,bw.pdf.CA.AZ,
                  bw.cdf.CA.NV,bw.pdf.CA.NV,
                  bw.cdf.CA.FL,bw.pdf.CA.FL,
                  bw.cdf.AZ.FL,bw.pdf.AZ.FL,
                  bw.cdf.NV.FL,bw.pdf.NV.FL,
                  bw.cdf.NV.AZ,bw.pdf.NV.AZ,grid.z.seq,bwtype){
  AZ <- ts(X$arizona,start=c(1975,2),end=c(2009,1),frequency=4)
  CA <- ts(X$california,start=c(1975,2),end=c(2009,1),frequency=4)
  FL <- ts(X$florida,start=c(1975,2),end=c(2009,1),frequency=4)
  NV <- ts(X$nevada,start=c(1975,2),end=c(2009,1),frequency=4)
  
  ar.garch.AZ <- garchFit(formula=~arma(1,0)+garch(1,1),data=AZ,trace=FALSE)
  ar.garch.CA <- garchFit(formula=~arma(1,0)+garch(1,1),data=CA,trace=FALSE)
  ar.garch.FL <- garchFit(formula=~arma(1,0)+garch(1,1),data=FL,trace=FALSE)
  ar.garch.NV <- garchFit(formula=~arma(1,0)+garch(1,1),data=NV,trace=FALSE)
  
  res.AZ <- ar.garch.AZ@residuals/ar.garch.AZ@sigma.t
  res.CA <- ar.garch.CA@residuals/ar.garch.CA@sigma.t
  res.FL <- ar.garch.FL@residuals/ar.garch.FL@sigma.t
  res.NV <- ar.garch.NV@residuals/ar.garch.NV@sigma.t
  
  datos.res <- data.frame(res.AZ=res.AZ,res.CA=res.CA,res.FL=res.FL,res.NV=res.NV)
  
  bw.CA.ucdf <- npudistbw(~res.CA,data=datos.res,bws=bw.cdf.CA.AZ$bw[1],bandwidth.compute=FALSE,bwtype=bwtype)
  u.CA <- fitted(npudist(bws=bw.CA.ucdf,newdata=data.frame(res.CA=grid.z.seq)))
  bw.AZ.ucdf <- npudistbw(~res.AZ,data=datos.res,bws=bw.cdf.CA.AZ$bw[2],bandwidth.compute=FALSE,bwtype=bwtype)
  u.AZ <- fitted(npudist(bws=bw.AZ.ucdf,newdata=data.frame(res.AZ=grid.z.seq)))
  u.CA.AZ <- cbind(u.CA,u.AZ)
  copula.CA.AZ <- npcopula(bws=bw.cdf.CA.AZ,data=datos.res,u=u.CA.AZ)
  C.u.CA.u.AZ <- diag(matrix(copula.CA.AZ$copula,length(grid.z.seq),length(grid.z.seq)))
  
  bw.CA.ucdf <- npudistbw(~res.CA,data=datos.res,bws=bw.cdf.CA.NV$bw[1],bandwidth.compute=FALSE,bwtype=bwtype)
  u.CA <- fitted(npudist(bws=bw.CA.ucdf,newdata=data.frame(res.CA=grid.z.seq)))
  bw.NV.ucdf <- npudistbw(~res.NV,data=datos.res,bws=bw.cdf.CA.NV$bw[2],bandwidth.compute=FALSE,bwtype=bwtype)
  u.NV <- fitted(npudist(bws=bw.NV.ucdf,newdata=data.frame(res.NV=grid.z.seq)))
  u.CA.NV <- cbind(u.CA,u.NV)
  copula.CA.NV <- npcopula(bws=bw.cdf.CA.NV,data=datos.res,u=u.CA.NV)
  C.u.CA.u.NV <- diag(matrix(copula.CA.NV$copula,length(grid.z.seq),length(grid.z.seq)))
  
  bw.CA.ucdf <- npudistbw(~res.CA,data=datos.res,bws=bw.cdf.CA.FL$bw[1],bandwidth.compute=FALSE,bwtype=bwtype)
  u.CA <- fitted(npudist(bws=bw.CA.ucdf,newdata=data.frame(res.CA=grid.z.seq)))
  bw.FL.ucdf <- npudistbw(~res.FL,data=datos.res,bws=bw.cdf.CA.FL$bw[2],bandwidth.compute=FALSE,bwtype=bwtype)
  u.FL <- fitted(npudist(bws=bw.FL.ucdf,newdata=data.frame(res.FL=grid.z.seq)))
  u.CA.FL <- cbind(u.CA,u.FL)
  copula.CA.FL <- npcopula(bws=bw.cdf.CA.FL,data=datos.res,u=u.CA.FL)
  C.u.CA.u.FL <- diag(matrix(copula.CA.FL$copula,length(grid.z.seq),length(grid.z.seq)))
  
  bw.FL.ucdf <- npudistbw(~res.FL,data=datos.res,bws=bw.cdf.AZ.FL$bw[1],bandwidth.compute=FALSE,bwtype=bwtype)
  u.FL <- fitted(npudist(bws=bw.FL.ucdf,newdata=data.frame(res.FL=grid.z.seq)))
  bw.AZ.ucdf <- npudistbw(~res.AZ,data=datos.res,bws=bw.cdf.AZ.FL$bw[2],bandwidth.compute=FALSE,bwtype=bwtype)
  u.AZ <- fitted(npudist(bws=bw.AZ.ucdf,newdata=data.frame(res.AZ=grid.z.seq)))
  u.AZ.FL <- cbind(u.FL,u.AZ)
  copula.AZ.FL <- npcopula(bws=bw.cdf.AZ.FL,data=datos.res,u=u.AZ.FL)
  C.u.FL.u.AZ <- diag(matrix(copula.AZ.FL$copula,length(grid.z.seq),length(grid.z.seq)))
  
  bw.FL.ucdf <- npudistbw(~res.FL,data=datos.res,bws=bw.cdf.NV.FL$bw[1],bandwidth.compute=FALSE,bwtype=bwtype)
  u.FL <- fitted(npudist(bws=bw.FL.ucdf,newdata=data.frame(res.FL=grid.z.seq)))
  bw.NV.ucdf <- npudistbw(~res.NV,data=datos.res,bws=bw.cdf.NV.FL$bw[2],bandwidth.compute=FALSE,bwtype=bwtype)
  u.NV <- fitted(npudist(bws=bw.NV.ucdf,newdata=data.frame(res.NV=grid.z.seq)))
  u.NV.FL <- cbind(u.FL,u.NV)
  copula.NV.FL <- npcopula(bws=bw.cdf.NV.FL,data=datos.res,u=u.NV.FL)
  C.u.FL.u.NV <- diag(matrix(copula.NV.FL$copula,length(grid.z.seq),length(grid.z.seq)))
  
  bw.AZ.ucdf <- npudistbw(~res.AZ,data=datos.res,bws=bw.cdf.NV.AZ$bw[1],bandwidth.compute=FALSE,bwtype=bwtype)
  u.AZ <- fitted(npudist(bws=bw.AZ.ucdf,newdata=data.frame(res.AZ=grid.z.seq)))
  bw.NV.ucdf <- npudistbw(~res.NV,data=datos.res,bws=bw.cdf.NV.AZ$bw[2],bandwidth.compute=FALSE,bwtype=bwtype)
  u.NV <- fitted(npudist(bws=bw.NV.ucdf,newdata=data.frame(res.NV=grid.z.seq)))
  u.NV.AZ <- cbind(u.AZ,u.NV)
  copula.NV.AZ <- npcopula(bws=bw.cdf.NV.AZ,data=datos.res,u=u.NV.AZ)
  C.u.AZ.u.NV <- diag(matrix(copula.NV.AZ$copula,length(grid.z.seq),length(grid.z.seq)))
  
  return(c((C.u.CA.u.AZ/u.AZ)[1:21],((1-u.AZ-u.CA+C.u.CA.u.AZ)/(1-u.AZ))[21:41],
           (C.u.CA.u.NV/u.NV)[1:21],((1-u.NV-u.CA+C.u.CA.u.NV)/(1-u.NV))[21:41],
           (C.u.CA.u.FL/u.FL)[1:21],((1-u.FL-u.CA+C.u.CA.u.FL)/(1-u.FL))[21:41],
           (C.u.FL.u.AZ/u.AZ)[1:21],((1-u.AZ-u.FL+C.u.FL.u.AZ)/(1-u.AZ))[21:41],
           (C.u.FL.u.NV/u.NV)[1:21],((1-u.NV-u.FL+C.u.FL.u.NV)/(1-u.NV))[21:41],
           (C.u.AZ.u.NV/u.NV)[1:21],((1-u.NV-u.AZ+C.u.AZ.u.NV)/(1-u.NV))[21:41]))
}


z.min <- -2; z.max <- 2
grid.z.seq <- seq(z.min, z.max, by=0.1)
ckertype <- "gaussian"
bwtype <- "adaptive_nn"

boot.copula <- tsboot(datos,colas,R=num.R,l=20,sim="fixed",n.sim=140,
                      bw.cdf.CA.AZ=bw.cdf.CA.AZ,bw.pdf.CA.AZ=bw.pdf.CA.AZ,
                      bw.cdf.CA.NV=bw.cdf.CA.NV,bw.pdf.CA.NV=bw.pdf.CA.NV,
                      bw.cdf.CA.FL=bw.cdf.CA.FL,bw.pdf.CA.FL=bw.pdf.CA.FL,
                      bw.cdf.AZ.FL=bw.cdf.AZ.FL,bw.pdf.AZ.FL=bw.pdf.AZ.FL,
                      bw.cdf.NV.FL=bw.cdf.NV.FL,bw.pdf.NV.FL=bw.pdf.NV.FL,
                      bw.cdf.NV.AZ=bw.cdf.NV.AZ,bw.pdf.NV.AZ=bw.pdf.NV.AZ,grid.z.seq=grid.z.seq,bwtype=bwtype)

warnings()

save(list=ls(),file="boot0.Rdata")

########################################################################################
##### ... ############################################################
########################################################################################

rm(list=ls())

load(file="boot0.Rdata")
parametrico <- read.csv("parametric_results.csv")
tipo <- 1

png(filename = "Figure_em_4_0.png", width = 150, height = 297, units = "mm", res = 600)
#postscript(file="Figure_em_4_0.ps",horizontal=FALSE,paper="letter")
#layout(matrix(1:6, 3, 2, byrow=TRUE), respect=TRUE)
par(mfrow = c(3,2))

boot.copula.m <- apply(boot.copula$t,2,FUN=mean)
boot.copula.sd <- apply(boot.copula$t,2,FUN=sd)
boot.copula.q <- t(apply(boot.copula$t,2,FUN=quantile,probs=c(0.05,0.95)))

#------------------------------#
#--------- CA - AZ ------------#
#------------------------------#
CA.ucdf <- ecdf(datos.res$res.CA)
em.u.CA <- CA.ucdf(grid.z.seq)
AZ.ucdf <- ecdf(datos.res$res.AZ)
em.u.AZ <- AZ.ucdf(grid.z.seq)
U.CA.AZ <- pobs(x=cbind(datos.res$res.CA,datos.res$res.AZ))
u.CA.AZ <- as.matrix(expand.grid(em.u.CA,em.u.AZ))
C.n.CA.AZ <- C.n(u=u.CA.AZ,U=U.CA.AZ)
C.n.u.CA.u.AZ <- diag(matrix(C.n.CA.AZ,length(grid.z.seq),length(grid.z.seq)))

bw.CA.ucdf <- npudistbw(~res.CA,data=datos.res,bws=bw.cdf.CA.AZ$bw[1],bandwidth.compute=FALSE,bwtype=bwtype)
u.CA <- fitted(npudist(bws=bw.CA.ucdf,newdata=data.frame(res.CA=grid.z.seq)))
bw.AZ.ucdf <- npudistbw(~res.AZ,data=datos.res,bws=bw.cdf.CA.AZ$bw[2],bandwidth.compute=FALSE,bwtype=bwtype)
u.AZ <- fitted(npudist(bws=bw.AZ.ucdf,newdata=data.frame(res.AZ=grid.z.seq)))
u.CA.AZ <- cbind(u.CA,u.AZ)
copula.CA.AZ <- npcopula(bws=bw.cdf.CA.AZ,data=datos.res,u=u.CA.AZ)

C.u.CA.u.AZ <- diag(matrix(copula.CA.AZ$copula,length(grid.z.seq),length(grid.z.seq)))
if(tipo==1){ylimite <-range((C.u.CA.u.AZ/u.AZ)[1:21]+1.96*boot.copula.sd[1:21],
                            (C.u.CA.u.AZ/u.AZ)[1:21]-1.96*boot.copula.sd[1:21],
                            ((1-u.AZ-u.CA+C.u.CA.u.AZ)/(1-u.AZ))[21:41]+1.96*boot.copula.sd[22:42],
                            ((1-u.AZ-u.CA+C.u.CA.u.AZ)/(1-u.AZ))[21:41]-1.96*boot.copula.sd[22:42])
}else{ylimite <- range(boot.copula.q[1:42,])}

plot(NULL,xlim=c(-2,2),ylim=ylimite,xlab="Filtered Price Change",ylab="",main="California/Arizona",las=1)
rug(res.CA,col="black"); rug(res.AZ,col="lightgray")
if(tipo==1){
  L.l <- c((C.u.CA.u.AZ/u.AZ)[1:21]-1.96*boot.copula.sd[1:21]); U.l <- c((C.u.CA.u.AZ/u.AZ)[1:21]+1.96*boot.copula.sd[1:21])
  L.r <- c(((1-u.AZ-u.CA+C.u.CA.u.AZ)/(1-u.AZ))[21:41]-1.96*boot.copula.sd[22:42]); U.r <- c(((1-u.AZ-u.CA+C.u.CA.u.AZ)/(1-u.AZ))[21:41]+1.96*boot.copula.sd[22:42])
  polygon(c(grid.z.seq[1:21],rev(grid.z.seq[1:21])),c(L.l,rev(U.l)),col="lightgray",border=FALSE)
  polygon(c(grid.z.seq[21:41],rev(grid.z.seq[21:41])),c(L.r,rev(U.r)),col="lightgray",border=FALSE)
}else{
  lines(grid.z.seq[1:21],boot.copula.q[1:21,1],lwd=3,lty=2)
  lines(grid.z.seq[1:21],boot.copula.q[1:21,2],lwd=3,lty=2)
  lines(grid.z.seq[21:41],boot.copula.q[22:42,1],lwd=3,lty=2)
  lines(grid.z.seq[21:41],boot.copula.q[22:42,2],lwd=3,lty=2)
}
points(grid.z.seq[1:21],(C.n.u.CA.u.AZ/em.u.AZ)[1:21],lwd=3,type="b",pch=16,col="black")
points(grid.z.seq[21:41],((1-em.u.AZ-em.u.CA+C.n.u.CA.u.AZ)/(1-em.u.AZ))[21:41],lwd=3,type="b",pch=16,col="black")
lines(parametrico$tail,parametrico$CA_AZ_m,lwd=3,lty=1,col="black")
lines(grid.z.seq[1:21],(C.u.CA.u.AZ/u.AZ)[1:21],lwd=3,lty=1,col="gray40")
lines(grid.z.seq[21:41],((1-u.AZ-u.CA+C.u.CA.u.AZ)/(1-u.AZ))[21:41],lwd=3,lty=1,col="gray40")
abline(v=0,lty=1,lwd=2)

#------------------------------#
#--------- CA - NV ------------#
#------------------------------#
CA.ucdf <- ecdf(datos.res$res.CA)
em.u.CA <- CA.ucdf(grid.z.seq)
AZ.ucdf <- ecdf(datos.res$res.NV)
em.u.NV <- AZ.ucdf(grid.z.seq)
U.CA.NV <- pobs(x=cbind(datos.res$res.CA,datos.res$res.NV))
u.CA.NV <- as.matrix(expand.grid(em.u.CA,em.u.NV))
C.n.CA.NV <- C.n(u=u.CA.NV,U=U.CA.NV)
C.n.u.CA.u.NV <- diag(matrix(C.n.CA.NV,length(grid.z.seq),length(grid.z.seq)))

bw.CA.ucdf <- npudistbw(~res.CA,data=datos.res,bws=bw.cdf.CA.NV$bw[1],bandwidth.compute=FALSE,bwtype=bwtype)
u.CA <- fitted(npudist(bws=bw.CA.ucdf,newdata=data.frame(res.CA=grid.z.seq)))
bw.NV.ucdf <- npudistbw(~res.NV,data=datos.res,bws=bw.cdf.CA.NV$bw[2],bandwidth.compute=FALSE,bwtype=bwtype)
u.NV <- fitted(npudist(bws=bw.NV.ucdf,newdata=data.frame(res.NV=grid.z.seq)))
u.CA.NV <- cbind(u.CA,u.NV)
copula.CA.NV <- npcopula(bws=bw.cdf.CA.NV,data=datos.res,u=u.CA.NV)

C.u.CA.u.NV <- diag(matrix(copula.CA.NV$copula,length(grid.z.seq),length(grid.z.seq)))
if(tipo==1){ylimite <-range((C.u.CA.u.NV/u.NV)[1:21]+1.96*boot.copula.sd[43:63],
                            (C.u.CA.u.NV/u.NV)[1:21]-1.96*boot.copula.sd[43:63],
                            ((1-u.NV-u.CA+C.u.CA.u.NV)/(1-u.NV))[21:41]+1.96*boot.copula.sd[64:84],
                            ((1-u.NV-u.CA+C.u.CA.u.NV)/(1-u.NV))[21:41]-1.96*boot.copula.sd[64:84])
}else{ylimite <- range(boot.copula.q[43:84,])}


plot(NULL,xlim=c(-2,2),ylim=ylimite,xlab="Filtered Price Change",ylab="",main="California/Nevada",las=1)
rug(res.CA,col="black"); rug(res.NV,col="lightgray")
if(tipo==1){
  L.l <- c((C.u.CA.u.NV/u.NV)[1:21]-1.96*boot.copula.sd[43:63]); U.l <- c((C.u.CA.u.NV/u.NV)[1:21]+1.96*boot.copula.sd[43:63])
  L.r <- c(((1-u.NV-u.CA+C.u.CA.u.NV)/(1-u.NV))[21:41]-1.96*boot.copula.sd[64:84]); U.r <- c(((1-u.NV-u.CA+C.u.CA.u.NV)/(1-u.NV))[21:41]+1.96*boot.copula.sd[64:84])
  polygon(c(grid.z.seq[1:21],rev(grid.z.seq[1:21])),c(L.l,rev(U.l)),col="lightgray",border=FALSE)
  polygon(c(grid.z.seq[21:41],rev(grid.z.seq[21:41])),c(L.r,rev(U.r)),col="lightgray",border=FALSE)
}else{
  lines(grid.z.seq[1:21],boot.copula.q[43:63,1],lwd=3,lty=2)
  lines(grid.z.seq[1:21],boot.copula.q[43:63,2],lwd=3,lty=2)
  lines(grid.z.seq[21:41],boot.copula.q[64:84,1],lwd=3,lty=2)
  lines(grid.z.seq[21:41],boot.copula.q[64:84,2],lwd=3,lty=2)
}
points(grid.z.seq[1:21],(C.n.u.CA.u.NV/em.u.NV)[1:21],lwd=3,type="b",pch=16,col="black")
points(grid.z.seq[21:41],((1-em.u.NV-em.u.CA+C.n.u.CA.u.NV)/(1-em.u.NV))[21:41],lwd=3,type="b",pch=16,col="black")
lines(parametrico$tail,parametrico$CA_NV_m,lwd=3,lty=1,col="black")
lines(grid.z.seq[1:21],(C.u.CA.u.NV/u.NV)[1:21],lwd=3,lty=1,col="gray40")
lines(grid.z.seq[21:41],((1-u.NV-u.CA+C.u.CA.u.NV)/(1-u.NV))[21:41],lwd=3,lty=1,col="gray40")
abline(v=0,lty=1,lwd=2)

#------------------------------#
#--------- CA - FL ------------#
#------------------------------#
CA.ucdf <- ecdf(datos.res$res.CA)
em.u.CA <- CA.ucdf(grid.z.seq)
AZ.ucdf <- ecdf(datos.res$res.FL)
em.u.FL <- AZ.ucdf(grid.z.seq)
U.CA.FL <- pobs(x=cbind(datos.res$res.CA,datos.res$res.FL))
u.CA.FL <- as.matrix(expand.grid(em.u.CA,em.u.FL))
C.n.CA.FL <- C.n(u=u.CA.FL,U=U.CA.FL)
C.n.u.CA.u.FL <- diag(matrix(C.n.CA.FL,length(grid.z.seq),length(grid.z.seq)))

bw.CA.ucdf <- npudistbw(~res.CA,data=datos.res,bws=bw.cdf.CA.FL$bw[1],bandwidth.compute=FALSE,bwtype=bwtype)
u.CA <- fitted(npudist(bws=bw.CA.ucdf,newdata=data.frame(res.CA=grid.z.seq)))
bw.FL.ucdf <- npudistbw(~res.FL,data=datos.res,bws=bw.cdf.CA.FL$bw[2],bandwidth.compute=FALSE,bwtype=bwtype)
u.FL <- fitted(npudist(bws=bw.FL.ucdf,newdata=data.frame(res.FL=grid.z.seq)))
u.CA.FL <- cbind(u.CA,u.FL)
copula.CA.FL <- npcopula(bws=bw.cdf.CA.FL,data=datos.res,u=u.CA.FL)

C.u.CA.u.FL <- diag(matrix(copula.CA.FL$copula,length(grid.z.seq),length(grid.z.seq)))
if(tipo==1){ylimite <-range((C.u.CA.u.FL/u.FL)[1:21]+1.96*boot.copula.sd[85:105],
                            (C.u.CA.u.FL/u.FL)[1:21]-1.96*boot.copula.sd[85:105],
                            ((1-u.FL-u.CA+C.u.CA.u.FL)/(1-u.FL))[21:41]+1.96*boot.copula.sd[106:126],
                            ((1-u.FL-u.CA+C.u.CA.u.FL)/(1-u.FL))[21:41]-1.96*boot.copula.sd[106:126])
}else{ylimite <- range(boot.copula.q[85:126,])}

plot(NULL,xlim=c(-2,2),ylim=ylimite,xlab="Filtered Price Change",ylab="",main="California/Florida",las=1)
rug(res.CA,col="black"); rug(res.FL,col="lightgray")
if(tipo==1){
  L.l <- c((C.u.CA.u.FL/u.FL)[1:21]-1.96*boot.copula.sd[85:105]); U.l <- c((C.u.CA.u.FL/u.FL)[1:21]+1.96*boot.copula.sd[85:105])
  L.r <- c(((1-u.FL-u.CA+C.u.CA.u.FL)/(1-u.FL))[21:41]-1.96*boot.copula.sd[106:126]); U.r <- c(((1-u.FL-u.CA+C.u.CA.u.FL)/(1-u.FL))[21:41]+1.96*boot.copula.sd[106:126])
  polygon(c(grid.z.seq[1:21],rev(grid.z.seq[1:21])),c(L.l,rev(U.l)),col="lightgray",border=FALSE)
  polygon(c(grid.z.seq[21:41],rev(grid.z.seq[21:41])),c(L.r,rev(U.r)),col="lightgray",border=FALSE)
}else{
  lines(grid.z.seq[1:21],boot.copula.q[85:105,1],lwd=3,lty=2)
  lines(grid.z.seq[1:21],boot.copula.q[85:105,2],lwd=3,lty=2)
  lines(grid.z.seq[21:41],boot.copula.q[106:126,1],lwd=3,lty=2)
  lines(grid.z.seq[21:41],boot.copula.q[106:126,2],lwd=3,lty=2)
}
points(grid.z.seq[1:21],(C.n.u.CA.u.FL/em.u.FL)[1:21],lwd=3,type="b",pch=16,col="black")
points(grid.z.seq[21:41],((1-em.u.FL-em.u.CA+C.n.u.CA.u.FL)/(1-em.u.FL))[21:41],lwd=3,type="b",pch=16,col="black")

lines(parametrico$tail,parametrico$CA_FL_m,lwd=3,lty=1,col="black")
lines(grid.z.seq[1:21],(C.u.CA.u.FL/u.FL)[1:21],lwd=3,lty=1,col="gray40")
lines(grid.z.seq[21:41],((1-u.FL-u.CA+C.u.CA.u.FL)/(1-u.FL))[21:41],lwd=3,lty=1,col="gray40")
abline(v=0,lty=1,lwd=2)

#------------------------------#
#--------- FL - AZ ------------#
#------------------------------#
FL.ucdf <- ecdf(datos.res$res.FL)
em.u.FL <- FL.ucdf(grid.z.seq)
AZ.ucdf <- ecdf(datos.res$res.AZ)
em.u.AZ <- AZ.ucdf(grid.z.seq)
U.FL.AZ <- pobs(x=cbind(datos.res$res.FL,datos.res$res.AZ))
u.FL.AZ <- as.matrix(expand.grid(em.u.FL,em.u.AZ))
C.n.FL.AZ <- C.n(u=u.FL.AZ,U=U.FL.AZ)
C.n.u.FL.u.AZ <- diag(matrix(C.n.FL.AZ,length(grid.z.seq),length(grid.z.seq)))

bw.FL.ucdf <- npudistbw(~res.FL,data=datos.res,bws=bw.cdf.AZ.FL$bw[1],bandwidth.compute=FALSE,bwtype=bwtype)
u.FL <- fitted(npudist(bws=bw.FL.ucdf,newdata=data.frame(res.FL=grid.z.seq)))
bw.AZ.ucdf <- npudistbw(~res.AZ,data=datos.res,bws=bw.cdf.AZ.FL$bw[2],bandwidth.compute=FALSE,bwtype=bwtype)
u.AZ <- fitted(npudist(bws=bw.AZ.ucdf,newdata=data.frame(res.AZ=grid.z.seq)))
u.AZ.FL <- cbind(u.FL,u.AZ)
copula.AZ.FL <- npcopula(bws=bw.cdf.AZ.FL,data=datos.res,u=u.AZ.FL)

C.u.FL.u.AZ <- diag(matrix(copula.AZ.FL$copula,length(grid.z.seq),length(grid.z.seq)))
if(tipo==1){ylimite <-range((C.u.FL.u.AZ/u.AZ)[1:21]+1.96*boot.copula.sd[127:147],
                            (C.u.FL.u.AZ/u.AZ)[1:21]-1.96*boot.copula.sd[127:147],
                            ((1-u.AZ-u.FL+C.u.FL.u.AZ)/(1-u.AZ))[21:41]+1.96*boot.copula.sd[148:168],
                            ((1-u.AZ-u.FL+C.u.FL.u.AZ)/(1-u.AZ))[21:41]-1.96*boot.copula.sd[148:168])
}else{ylimite <- range(boot.copula.q[127:168,])}

plot(NULL,xlim=c(-2,2),ylim=ylimite,xlab="Filtered Price Change",ylab="",main="Florida/Arizona",las=1)
rug(res.FL,col="black"); rug(res.AZ,col="lightgray")
if(tipo==1){
  L.l <- c((C.u.FL.u.AZ/u.AZ)[1:21]-1.96*boot.copula.sd[127:147]); U.l <- c((C.u.FL.u.AZ/u.AZ)[1:21]+1.96*boot.copula.sd[127:147])
  L.r <- c(((1-u.AZ-u.FL+C.u.FL.u.AZ)/(1-u.AZ))[21:41]-1.96*boot.copula.sd[148:168]); U.r <- c(((1-u.AZ-u.FL+C.u.FL.u.AZ)/(1-u.AZ))[21:41]+1.96*boot.copula.sd[148:168])
  polygon(c(grid.z.seq[1:21],rev(grid.z.seq[1:21])),c(L.l,rev(U.l)),col="lightgray",border=FALSE)
  polygon(c(grid.z.seq[21:41],rev(grid.z.seq[21:41])),c(L.r,rev(U.r)),col="lightgray",border=FALSE)
}else{
  lines(grid.z.seq[1:21],boot.copula.q[127:147,1],lwd=3,lty=2)
  lines(grid.z.seq[1:21],boot.copula.q[127:147,2],lwd=3,lty=2)
  lines(grid.z.seq[21:41],boot.copula.q[148:168,1],lwd=3,lty=2)
  lines(grid.z.seq[21:41],boot.copula.q[148:168,2],lwd=3,lty=2)
}
points(grid.z.seq[1:21],(C.n.u.FL.u.AZ/em.u.AZ)[1:21],lwd=3,type="b",pch=16,col="black")
points(grid.z.seq[21:41],((1-em.u.AZ-em.u.FL+C.n.u.FL.u.AZ)/(1-em.u.AZ))[21:41],lwd=3,type="b",pch=16,col="black")
lines(parametrico$tail,parametrico$FL_AZ_m,lwd=3,lty=1,col="black")
lines(grid.z.seq[1:21],(C.u.FL.u.AZ/u.AZ)[1:21],lwd=3,lty=1,col="gray40")
lines(grid.z.seq[21:41],((1-u.AZ-u.FL+C.u.FL.u.AZ)/(1-u.AZ))[21:41],lwd=3,lty=1,col="gray40")
abline(v=0,lty=1,lwd=2)


#------------------------------#
#--------- FL - NV ------------#
#------------------------------#
FL.ucdf <- ecdf(datos.res$res.FL)
em.u.FL <- FL.ucdf(grid.z.seq)
NV.ucdf <- ecdf(datos.res$res.NV)
em.u.NV <- NV.ucdf(grid.z.seq)
U.FL.NV <- pobs(x=cbind(datos.res$res.FL,datos.res$res.NV))
u.FL.NV <- as.matrix(expand.grid(em.u.FL,em.u.NV))
C.n.FL.NV <- C.n(u=u.FL.NV,U=U.FL.NV)
C.n.u.FL.u.NV <- diag(matrix(C.n.FL.NV,length(grid.z.seq),length(grid.z.seq)))

bw.FL.ucdf <- npudistbw(~res.FL,data=datos.res,bws=bw.cdf.NV.FL$bw[1],bandwidth.compute=FALSE,bwtype=bwtype)
u.FL <- fitted(npudist(bws=bw.FL.ucdf,newdata=data.frame(res.FL=grid.z.seq)))
bw.NV.ucdf <- npudistbw(~res.NV,data=datos.res,bws=bw.cdf.NV.FL$bw[2],bandwidth.compute=FALSE,bwtype=bwtype)
u.NV <- fitted(npudist(bws=bw.NV.ucdf,newdata=data.frame(res.NV=grid.z.seq)))
u.NV.FL <- cbind(u.FL,u.NV)
copula.NV.FL <- npcopula(bws=bw.cdf.NV.FL,data=datos.res,u=u.NV.FL)

C.u.FL.u.NV <- diag(matrix(copula.NV.FL$copula,length(grid.z.seq),length(grid.z.seq)))
if(tipo==1){ylimite <-range((C.u.FL.u.NV/u.NV)[1:21]+1.96*boot.copula.sd[169:189],
                            (C.u.FL.u.NV/u.NV)[1:21]-1.96*boot.copula.sd[169:189],
                            ((1-u.NV-u.FL+C.u.FL.u.NV)/(1-u.NV))[21:41]+1.96*boot.copula.sd[190:210],
                            ((1-u.NV-u.FL+C.u.FL.u.NV)/(1-u.NV))[21:41]-1.96*boot.copula.sd[190:210])
}else{ylimite <- range(boot.copula.q[169:210,])}

plot(NULL,xlim=c(-2,2),ylim=ylimite,xlab="Filtered Price Change",ylab="",main="Florida/Nevada",las=1)
rug(res.FL,col="black"); rug(res.NV,col="lightgray")
if(tipo==1){
  L.l <- c((C.u.FL.u.NV/u.NV)[1:21]-1.96*boot.copula.sd[169:189]); U.l <- c((C.u.FL.u.NV/u.NV)[1:21]+1.96*boot.copula.sd[169:189])
  L.r <- c(((1-u.NV-u.FL+C.u.FL.u.NV)/(1-u.NV))[21:41]-1.96*boot.copula.sd[190:210]); U.r <- c(((1-u.NV-u.FL+C.u.FL.u.NV)/(1-u.NV))[21:41]+1.96*boot.copula.sd[190:210])
  polygon(c(grid.z.seq[1:21],rev(grid.z.seq[1:21])),c(L.l,rev(U.l)),col="lightgray",border=FALSE)
  polygon(c(grid.z.seq[21:41],rev(grid.z.seq[21:41])),c(L.r,rev(U.r)),col="lightgray",border=FALSE)
}else{
  lines(grid.z.seq[1:21],boot.copula.q[169:189,1],lwd=3,lty=2)
  lines(grid.z.seq[1:21],boot.copula.q[169:189,2],lwd=3,lty=2)
  lines(grid.z.seq[21:41],boot.copula.q[190:210,1],lwd=3,lty=2)
  lines(grid.z.seq[21:41],boot.copula.q[190:210,2],lwd=3,lty=2)
}
points(grid.z.seq[1:21],(C.n.u.FL.u.NV/em.u.NV)[1:21],lwd=3,type="b",pch=16,col="black")
points(grid.z.seq[21:41],((1-em.u.NV-em.u.FL+C.n.u.FL.u.NV)/(1-em.u.NV))[21:41],lwd=3,type="b",pch=16,col="black")
lines(parametrico$tail,parametrico$FL_NV_m,lwd=3,lty=1,col="black")
lines(grid.z.seq[1:21],(C.u.FL.u.NV/u.NV)[1:21],lwd=3,lty=1,col="gray40")
lines(grid.z.seq[21:41],((1-u.NV-u.FL+C.u.FL.u.NV)/(1-u.NV))[21:41],lwd=3,lty=1,col="gray40")
abline(v=0,lty=1,lwd=2)

#------------------------------#
#--------- AZ - NV ------------#
#------------------------------#
AZ.ucdf <- ecdf(datos.res$res.AZ)
em.u.AZ <- AZ.ucdf(grid.z.seq)
NV.ucdf <- ecdf(datos.res$res.NV)
em.u.NV <- NV.ucdf(grid.z.seq)
U.AZ.NV <- pobs(x=cbind(datos.res$res.AZ,datos.res$res.NV))
u.AZ.NV <- as.matrix(expand.grid(em.u.AZ,em.u.NV))
C.n.AZ.NV <- C.n(u=u.AZ.NV,U=U.AZ.NV)
C.n.u.AZ.u.NV <- diag(matrix(C.n.AZ.NV,length(grid.z.seq),length(grid.z.seq)))

bw.AZ.ucdf <- npudistbw(~res.AZ,data=datos.res,bws=bw.cdf.NV.AZ$bw[1],bandwidth.compute=FALSE,bwtype=bwtype)
u.AZ <- fitted(npudist(bws=bw.AZ.ucdf,newdata=data.frame(res.AZ=grid.z.seq)))
bw.NV.ucdf <- npudistbw(~res.NV,data=datos.res,bws=bw.cdf.NV.AZ$bw[2],bandwidth.compute=FALSE,bwtype=bwtype)
u.NV <- fitted(npudist(bws=bw.NV.ucdf,newdata=data.frame(res.NV=grid.z.seq)))
u.NV.AZ <- cbind(u.AZ,u.NV)
copula.NV.AZ <- npcopula(bws=bw.cdf.NV.AZ,data=datos.res,u=u.NV.AZ)

C.u.AZ.u.NV <- diag(matrix(copula.NV.AZ$copula,length(grid.z.seq),length(grid.z.seq)))
if(tipo==1){ylimite <-range((C.u.AZ.u.NV/u.NV)[1:21]+1.96*boot.copula.sd[211:231],
                            (C.u.AZ.u.NV/u.NV)[1:21]-1.96*boot.copula.sd[211:231],
                            ((1-u.NV-u.AZ+C.u.AZ.u.NV)/(1-u.NV))[21:41]+1.96*boot.copula.sd[232:252],
                            ((1-u.NV-u.AZ+C.u.AZ.u.NV)/(1-u.NV))[21:41]-1.96*boot.copula.sd[232:252])
}else{ylimite <- range(boot.copula.q[211:252,])}

plot(NULL,xlim=c(-2,2),ylim=ylimite,xlab="Filtered Price Change",ylab="",main="Arizona/Nevada",las=1)
rug(res.AZ,col="black"); rug(res.NV,col="lightgray")
if(tipo==1){
  L.l <- c((C.u.AZ.u.NV/u.NV)[1:21]-1.96*boot.copula.sd[211:231]); U.l <- c((C.u.AZ.u.NV/u.NV)[1:21]+1.96*boot.copula.sd[211:231])
  L.r <- c(((1-u.NV-u.AZ+C.u.AZ.u.NV)/(1-u.NV))[21:41]-1.96*boot.copula.sd[232:252]); U.r <- c(((1-u.NV-u.AZ+C.u.AZ.u.NV)/(1-u.NV))[21:41]+1.96*boot.copula.sd[232:252])
  polygon(c(grid.z.seq[1:21],rev(grid.z.seq[1:21])),c(L.l,rev(U.l)),col="lightgray",border=FALSE)
  polygon(c(grid.z.seq[21:41],rev(grid.z.seq[21:41])),c(L.r,rev(U.r)),col="lightgray",border=FALSE)
}else{
  lines(grid.z.seq[1:21],boot.copula.q[211:231,1],lwd=3,lty=2)
  lines(grid.z.seq[1:21],boot.copula.q[211:231,2],lwd=3,lty=2)
  lines(grid.z.seq[21:41],boot.copula.q[232:252,1],lwd=3,lty=2)
  lines(grid.z.seq[21:41],boot.copula.q[232:252,2],lwd=3,lty=2)
}
points(grid.z.seq[1:21],(C.n.u.AZ.u.NV/em.u.NV)[1:21],lwd=3,type="b",pch=16,col="black")
points(grid.z.seq[21:41],((1-em.u.NV-em.u.AZ+C.n.u.AZ.u.NV)/(1-em.u.NV))[21:41],lwd=3,type="b",pch=16,col="black")
lines(parametrico$tail,parametrico$AZ_NV_m,lwd=3,lty=1,col="black")
lines(grid.z.seq[1:21],(C.u.AZ.u.NV/u.NV)[1:21],lwd=3,lty=1,col="gray40")
lines(grid.z.seq[21:41],((1-u.NV-u.AZ+C.u.AZ.u.NV)/(1-u.NV))[21:41],lwd=3,lty=1,col="gray40")
abline(v=0,lty=1,lwd=2)

dev.off()
