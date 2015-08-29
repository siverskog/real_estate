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

packages <- c("foreign", "fGarch", "np")

install(packages)

########################################################################################
##### ... ##############################################################################
########################################################################################



datos <- read.csv(file="D:/your_local_folder/housing_data.csv",header=TRUE)
AZ <- ts(datos$arizona,start=c(1975,2),end=c(2009,1),frequency=4)
CA <- ts(datos$california,start=c(1975,2),end=c(2009,1),frequency=4)
FL <- ts(datos$florida,start=c(1975,2),end=c(2009,1),frequency=4)
NV <- ts(datos$nevada,start=c(1975,2),end=c(2009,1),frequency=4)

ar.garch.AZ <- garchFit(formula=~arma(1,0)+garch(1,1),data=AZ,trace=FALSE)
ar.garch.CA <- garchFit(formula=~arma(1,0)+garch(1,1),data=CA,trace=FALSE)
ar.garch.FL <- garchFit(formula=~arma(1,0)+garch(1,1),data=FL,trace=FALSE)
ar.garch.NV <- garchFit(formula=~arma(1,0)+garch(1,1),data=NV,trace=FALSE)

res.AZ <- ar.garch.AZ@residuals/ar.garch.AZ@sigma.t
res.CA <- ar.garch.CA@residuals/ar.garch.CA@sigma.t
res.FL <- ar.garch.FL@residuals/ar.garch.FL@sigma.t
res.NV <- ar.garch.NV@residuals/ar.garch.NV@sigma.t

ckertype <- "gaussian"
nmulti <- 30
bwtype <- "adaptive_nn"
n.eval <- 100

datos.res <- data.frame(res.AZ=res.AZ,res.CA=res.CA,res.FL=res.FL,res.NV=res.NV)

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

save(list=ls(),file="D:/your_local_folder/bw0.Rdata")
