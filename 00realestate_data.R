########################################################################################
##### REAL ESTATE PROJECT
##### By Jonathan Siverskog
##### 2015-08-16
#####
##### PART 00: DATA
#####
########################################################################################

data <- read.table(file = "realestate.csv", sep = ";", dec = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
time <- as.Date(data[,"Time"])
data <- data[,-c(1, 11)]
rownames(data) <- time
data <- data[time>"2004-01-01",]
time <- as.Date(rownames(data))
data <- data[,order(colnames(data))]
colnames(data)[9] <- "UNITED KINGDOM"
names <- colnames(data)

pri <- log(data)

ret <- as.data.frame(diff(as.matrix(pri)))
vol <- abs(ret)

rm(time)

########################################################################################
##### SUBSAMPLES #######################################################################

break1 <- "2008-09-15"

ret <- list(pt = ret,
            p1 = ret[row.names(ret)<break1,],
            p2 = ret[row.names(ret)>=break1,])

vol <- list(pt = vol,
            p1 = vol[row.names(vol)<break1,],
            p2 = vol[row.names(vol)>=break1,])

pri <- list(pt = pri,
            p1 = pri[row.names(pri)<break1,],
            p2 = pri[row.names(pri)>=break1,])

data <- list(ret = ret, vol = vol)
save(data, file = "realestate.RData")

########################################################################################
##### PLOT #############################################################################
########################################################################################

source("99realestate_functions.R")

.plot(pri, file = "figure01_pri.png", res = 600)
.plot(ret, file = "figure01a_ret.png", res = 600)
.plot(vol, file = "figure01b_vol.png", res = 600)

rm(list = ls())
