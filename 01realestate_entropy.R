########################################################################################
##### REAL ESTATE PROJECT
##### Jonathan Siverskog
##### 2015-06-15
########################################################################################

wd <- "C:\\Users\\Jonathan\\Dropbox\\Projekt Gazi\\06_Housing_Wavelet"
setwd(wd)
load("realestate.RData")

#install.packages("entropy")
#library(entropy)

#install.packages("wavelets")
#library(wavelets)

#install.packages("wavethresh")
#ibrary(wavethresh)

#install.packages("wmtsa")
#library(wmtsa)

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

tmp <- ret
max <- 8
wav <- list(list(), list(), list(), list())
entropy <- list(list(), list(), list(), list())
min.entropy <- list()

### LOOP OVER SUB-SAMPLES ###

for(j in 1:length(tmp)) {
  
  x <- tmp[[j]]
  
  ### WAVELET TRANSFORM ###
  
  for(i in 1:ncol(x)) {
    
    wav[[j]][[i]] <- modwt(x = x[,i], n.levels = max, wf = "la8", boundary = "periodic")
    
  }
  
  ### SHANNON ENTROPY ###
  
  for(i in 1:length(wav[[j]])) {
    
    entropy[[j]][[i]] <- unlist(lapply(wav[[j]][[i]], FUN = shannon))
    
  }
  
  ### MAKE TABLE ###
  
  raw <- unlist(apply(x, 2, FUN = shannon))
  det <- do.call(cbind, entropy[[j]])
  tab <- rbind(raw, det)
  tab <- tab[-nrow(tab),]
  min.entropy[[j]] <- apply(tab, 2, min.ent)
  
}

### FORMAT TABLE ###

tab <- do.call(cbind, min.entropy)
tab <- tab[,order(colnames(tab))]
colnames(tab) <- rep(c("PT", "P1", "P2", "P3"),10)

tab4a <- tab[,1:20]
tab4b <- tab[,21:40]

tab4a <- xtable(x = tab4a)
align(tab4a) <- rep("l", ncol(tab4a)+1)
digits(tab4a) <- rep(0, ncol(tab4a)+1)

tab4b <- xtable(x = tab4b)
align(tab4b) <- rep("l", ncol(tab4b)+1)
digits(tab4b) <- rep(0, ncol(tab4b)+1)

print.xtable(tab4a, file = "table04a_entropy_ret.tex", floating = FALSE, booktabs = TRUE)
print.xtable(tab4b, file = "table04b_entropy_ret.tex", floating = FALSE, booktabs = TRUE)

########################################################################################
##### VOLATILITY #######################################################################

### SETUP ###

tmp <- vol
max <- 8
wav <- list(list(), list(), list(), list())
entropy <- list(list(), list(), list(), list())
min.entropy <- list()

### LOOP OVER SUB-SAMPLES ###

for(j in 1:length(tmp)) {
  
  x <- tmp[[j]]
  
  ### WAVELET TRANSFORM ###
  
  for(i in 1:ncol(x)) {
    
    wav[[j]][[i]] <- modwt(x = x[,i], n.levels = max, wf = "la8", boundary = "periodic")
    
  }
  
  ### SHANNON ENTROPY ###
  
  for(i in 1:length(wav[[j]])) {
    
    entropy[[j]][[i]] <- unlist(lapply(wav[[j]][[i]], FUN = shannon))
    
  }
  
  ### MAKE TABLE ###
  
  raw <- unlist(apply(x, 2, FUN = shannon))
  det <- do.call(cbind, entropy[[j]])
  tab <- rbind(raw, det)
  tab <- tab[-nrow(tab),]
  min.entropy[[j]] <- apply(tab, 2, min.ent)
  
}

### FORMAT TABLE ###

tab <- do.call(cbind, min.entropy)
tab <- tab[,order(colnames(tab))]
colnames(tab) <- rep(c("PT", "P1", "P2", "P3"),10)

tab5a <- tab[,1:20]
tab5b <- tab[,21:40]

tab5a <- xtable(x = tab5a)
align(tab5a) <- rep("l", ncol(tab5a)+1)
digits(tab5a) <- rep(0, ncol(tab5a)+1)

tab5b <- xtable(x = tab5b)
align(tab5b) <- rep("l", ncol(tab5b)+1)
digits(tab5b) <- rep(0, ncol(tab5b)+1)

print.xtable(tab5a, file = "table05a_entropy_vol.tex", floating = FALSE, booktabs = TRUE)
print.xtable(tab5b, file = "table05b_entropy_vol.tex", floating = FALSE, booktabs = TRUE)

########################################################################################
##### PLOT FUNCTION ####################################################################
########################################################################################

plot.wav <- function(wav, raw, time, name = "plot.png", print = TRUE) {
  
  if(print) { png(file = name, units = "mm", width = 247, height = 140, res = 300) }
  
  names <- c(paste(rep("D", length(wav)-1), 1:(length(wav)-1), sep = ""), paste("A", (length(wav)-1), sep = ""))
  
  par(mar = c(2,3,2.5,1), mfrow = c(ceiling((length(wav)+1)/2),2))
  
  plot(time, raw, type = "l", las = 1, ylab = NA, xlab = NA, main = "RAW")
  
  for(i in 1:length(wav)) {
    
    plot(time, wav[[i]], type = "l", las = 1, ylab = NA, xlab = NA, main = names[i])
    
  }
  
  if(print) { dev.off() }
  
}

########################################################################################
##### TEST #############################################################################
########################################################################################

### US ###

x <- ret[,1]
time <- as.Date(row.names(ret))

wav <- modwt(x = x, n.levels = 7, wf = "la8", boundary = "periodic")
plot.wav(wav, x, time)

### SHANNON ENTROPY ###

unlist(lapply(wav, FUN = shannon))

### INVERSE ###

y <- imodwt(wav)
dev.off()
plot(x, type = "l")
points(y, type = "l", col = "red")

y <- wav$s7+wav$d7+wav$d6+wav$d5+wav$d4+wav$d3+wav$d2+wav$d1
cor(x,y)

y <- do.call(cbind, wav)
y <- apply(y,1,sum)

test <- cbind(x,y)

plot(x, type = "l")
points(y, type = "l", col = "red")