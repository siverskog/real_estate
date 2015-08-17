########################################################################################
##### REAL ESTATE PROJECT
##### Jonathan Siverskog
##### 2015-06-15
########################################################################################

load("realestate.RData")

#install.packages("waveslim")
library(waveslim)

########################################################################################
##### SHANNON ENTROPY ##################################################################
########################################################################################

shannon <- function(c) {
  
  a <- log(c^2)
  a[abs(a)==Inf] <- 0
  return(-sum(c^2*a))
  
}


min.ent <- function(y) {
  
  z <- format(round(y, 3), nsmall = 3)
  z[which.min(y)] <- paste(format(round(y[which.min(y)], 3), nsmall = 3), "*", sep = "")
  return(z)
  
}

########################################################################################
##### MODWT ############################################################################
########################################################################################

########################################################################################
##### RETURN ###########################################################################

### SETUP ###

x <- data$ret$pt
max <- 10

wav <- list()
entropy <- list()

### WAVELET TRANSFORM ###

for(i in 1:ncol(x)) {
  
  wav[[i]] <- modwt(x = x[,i], n.levels = max, wf = "la8", boundary = "periodic")
  
}

### SHANNON ENTROPY ###

for(i in 1:length(wav)) {
  
  entropy[[i]] <- unlist(lapply(wav[[i]], FUN = shannon))
  
}

### MAKE TABLE ###

raw <- unlist(apply(x, 2, FUN = shannon))
det <- do.call(cbind, entropy)
tab_ret <- rbind(raw, det)
tab_ret <- tab_ret[-nrow(tab_ret),]
min_ret <- apply(tab_ret, 2, which.min)-1
  
tab <- list()
tab$a<- apply(tab_ret, 2, min.ent)

########################################################################################
##### VOLATILITY #######################################################################

### SETUP ###

x <- data$vol$pt
max <- 10

wav <- list()
entropy <- list()

### WAVELET TRANSFORM ###

for(i in 1:ncol(x)) {
  
  wav[[i]] <- modwt(x = x[,i], n.levels = max, wf = "la8", boundary = "periodic")
  
}
length(wav[[1]])

### SHANNON ENTROPY ###

for(i in 1:length(wav)) {
  
  entropy[[i]] <- unlist(lapply(wav[[i]], FUN = shannon))
  
}

### MAKE TABLE ###

raw <- unlist(apply(x, 2, FUN = shannon))
det <- do.call(cbind, entropy)
tab_vol <- rbind(raw, det)
tab_vol <- tab_vol[-nrow(tab_vol),]
min_vol <- apply(tab_vol, 2, which.min)-1

tab$b<- apply(tab_vol, 2, min.ent)

rm(det, x, entropy, i, max, raw, wav)

########################################################################################
##### LATEX TABLE ######################################################################
########################################################################################

source("https://raw.githubusercontent.com/siverskog/latex_table/master/latex_table.R")

names <- colnames(data$ret$pt)
names[9:10] <- c("U.K.", "U.S.")

h <- list()
h$a <- header(list("RETURN", names), cmid = FALSE)
h$b <- header(list("VOLATILITY", names), cmid = FALSE)

min_ent <- latex.table(h, tab, 10)
write(min_ent, file = "table03_entropy.tex")

########################################################################################
##### WAVELET TRANSFORM ################################################################
########################################################################################

wav <- list(list(), list())
min_ent <- list(min_ret, min_vol)

for(i in 1:length(data)) {
  x <- data[[i]]$pt
  for(j in 1:ncol(x)) {
    wav[[i]][[j]] <- modwt(x = x[,i], n.levels = min_ent[[i]][j], wf = "la8", boundary = "periodic")
  }
}

########################################################################################
##### PLOT FUNCTION ####################################################################
########################################################################################

.plot <- function(time, x, col = "deepskyblue3", main) {
    
    plot(time, x, type = "l", col = col, las = 1, ylab = NA, xlab = NA, xaxt = "n", main = main)
    axis.Date(side = 1, at = seq(as.Date("2004-01-01"), as.Date("2016-01-01"), by = "2 years"))
    axis.Date(side = 1, at = seq(as.Date("2004-01-01"), as.Date("2016-01-01"), by = "years"), labels = FALSE)
    axis.Date(side = 1, at = seq(as.Date("2004-01-01"), as.Date("2016-06-01"), by = "quarters"), labels = FALSE, tck = -0.03)
}

plot.wav <- function(wav, raw, time, name = "plot.png", print = TRUE) {
  
  if(print) { png(file = name, units = "mm", width = 247, height = 140, res = 600) }
  
  names <- c(paste(rep("D", length(wav)-1), 1:(length(wav)-1), sep = ""), paste("A", (length(wav)-1), sep = ""))
  
  par(mar = c(2,3,2,1), mfrow = c(ceiling((length(wav)+1)/2),2))
  
  .plot(time, raw, main = "RAW", col = "blue")
  
  for(i in 1:length(wav)) {
    
    if(i==length(wav)) {
      .plot(time, wav[[i]], main = names[i], col = "darkorchid2")
    } else {
      .plot(time, wav[[i]], main = names[i], col = "deepskyblue3")
    }
  }
  
  if(print) { dev.off() }
  
}

########################################################################################
##### TEST #############################################################################
########################################################################################

time <- as.Date(row.names(data$ret$pt))
x <- wav[[1]][[10]]
y <- data$ret$pt[,10]

plot.wav(x, y, time, name = "figure02a_us.png")

x <- wav[[2]][[10]]
y <- data$vol$pt[,10]

plot.wav(x, y, time, name = "figure02b_us.png")

########################################################################################
##### SAVE DATA ########################################################################
########################################################################################

save(wav, file = "realestate_wav.RData")
rm(list = ls())
