########################################################################################
##### REAL ESTATE PROJECT
##### Jonathan Siverskog
##### 2015-07-03
########################################################################################

wd <- "C:\\Users\\Jonathan\\Dropbox\\Projekt Gazi\\06_Housing_Wavelet"
setwd(wd)
load("realestate.RData")

#install.packages("waveslim")
library(waveslim)

########################################################################################
##### MAXIMUM-OVERLAP DISCRETE WAVELET TRANSFORM #######################################
########################################################################################

########################################################################################
##### RETURN ###########################################################################

### SETUP ###

tmp <- ret
J <- 8
wav <- list(list(), list(), list(), list())

### LOOP OVER SUB-SAMPLES ###

for(j in 1:length(tmp)) {
  
  x <- tmp[[j]]
  
  ### WAVELET TRANSFORM ###
  
  for(i in 1:ncol(x)) {
    
    wav[[j]][[i]] <- modwt(x = x[,i], n.levels = J, wf = "la8", boundary = "periodic")
    
  }
  
}

wav_ret <- wav
rm(i, j, x, tmp)

########################################################################################
##### VOLATILITY #######################################################################

### SETUP ###

tmp <- vol
J <- 8
wav <- list(list(), list(), list(), list())

### LOOP OVER SUB-SAMPLES ###

for(j in 1:length(tmp)) {
  
  x <- tmp[[j]]
  
  ### WAVELET TRANSFORM ###
  
  for(i in 1:ncol(x)) {
    
    wav[[j]][[i]] <- modwt(x = x[,i], n.levels = J, wf = "la8", boundary = "periodic")
    
  }
  
}

wav_vol <- wav
rm(i, j, x, tmp, wav)

########################################################################################
##### WAVELET CROSS-CORRELATION WITH U.S REAL ESTATE ###################################
########################################################################################

########################################################################################
##### RETURN ###########################################################################

### SETUP ###

J <- 8
wav <- wav_ret
tmp <- ret
index <- 10
var <- setdiff(1:length(wav[[1]]), index)
cross.cor <- matrix(NA, ncol = length(wav)*length(var), nrow = J+2, dimnames = list(c("raw", paste(rep("d", J), 1:J, sep = ""), paste("a", J, sep = "")), rep(colnames(ret$pt)[-index], length(wav))))
count <- 1

### LOOP OVER SUB-SAMPLES, INDICES, AND LEVELS ###

for(i in 1:length(wav)) {
  for(j in 1:length(var)) {
    
    cross.cor[1,count] <- spin.correlation(tmp[[i]][[index]], tmp[[i]][[var[j]]], 0)
    
    for(k in 1:(J+1)) {
      
      print(c(i,j,k))
      
      cross.cor[1+k,count] <- spin.correlation(wav[[i]][[index]][[k]], wav[[i]][[var[j]]][[k]], 0)
      
    }
    
    count <- count+1
    
  }
}

### MAKE TABLE ###

cross.cor <- cross.cor[,order(colnames(cross.cor))]
colnames(cross.cor) <- rep(c("PT", "P1", "P2", "P3"), length(var))

tab6a <- cross.cor[,1:20]
tab6b <- cross.cor[,21:36]

tab6a <- xtable(x = tab6a)
align(tab6a) <- rep("l", ncol(tab6a)+1)
digits(tab6a) <- rep(2, ncol(tab6a)+1)

tab6b <- xtable(x = tab6b)
align(tab6b) <- rep("l", ncol(tab6b)+1)
digits(tab6b) <- rep(2, ncol(tab6b)+1)

print.xtable(tab6a, file = "table06a_crosscor_ret.tex", floating = FALSE, booktabs = TRUE)
print.xtable(tab6b, file = "table06b_crosscor_ret.tex", floating = FALSE, booktabs = TRUE)

########################################################################################
##### VOLATILITY ###########################################################################

### SETUP ###

J <- 8
wav <- wav_vol
tmp <- vol
index <- 10
var <- setdiff(1:length(wav[[1]]), index)
cross.cor <- matrix(NA, ncol = length(wav)*length(var), nrow = J+2, dimnames = list(c("raw", paste(rep("d", J), 1:J, sep = ""), paste("a", J, sep = "")), rep(colnames(ret$pt)[-index], length(wav))))
count <- 1

### LOOP OVER SUB-SAMPLES, INDICES, AND LEVELS ###

for(i in 1:length(wav)) {
  for(j in 1:length(var)) {
    
    cross.cor[1,count] <- spin.correlation(tmp[[i]][[index]], tmp[[i]][[var[j]]], 0)
    
    for(k in 1:(J+1)) {
      
      print(c(i,j,k))
      
      cross.cor[1+k,count] <- spin.correlation(wav[[i]][[index]][[k]], wav[[i]][[var[j]]][[k]], 0)
      
    }
    
    count <- count+1
    
  }
}

### MAKE TABLE ###

cross.cor <- cross.cor[,order(colnames(cross.cor))]
colnames(cross.cor) <- rep(c("PT", "P1", "P2", "P3"), length(var))

tab7a <- cross.cor[,1:20]
tab7b <- cross.cor[,21:36]

tab7a <- xtable(x = tab7a)
align(tab7a) <- rep("l", ncol(tab7a)+1)
digits(tab7a) <- rep(2, ncol(tab7a)+1)

tab7b <- xtable(x = tab7b)
align(tab7b) <- rep("l", ncol(tab7b)+1)
digits(tab7b) <- rep(2, ncol(tab7b)+1)

print.xtable(tab7a, file = "table07a_crosscor_vol.tex", floating = FALSE, booktabs = TRUE)
print.xtable(tab7b, file = "table07b_crosscor_vol.tex", floating = FALSE, booktabs = TRUE)

########################################################################################
##### CAUSALITY WITH U.S REAL ESTATE ###################################################
########################################################################################


########################################################################################
##### SAVE DATA ########################################################################
########################################################################################


