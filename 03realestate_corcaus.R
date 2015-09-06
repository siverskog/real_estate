########################################################################################
##### REAL ESTATE PROJECT
##### By Jonathan Siverskog
##### 2015-08-24
#####
##### PART 03: CROSS-CORRELATION
#####
########################################################################################

load("realestate_wav.RData")
load("realestate.RData")

data_wav <- wav
rm(wav)

#install.packages("waveslim")
library(waveslim)

###############################################################################
##### WAVELET CROSS-CORRELATION WITH U.S REAL ESTATE ###################################
########################################################################################

########################################################################################
##### RETURN ###########################################################################

### SETUP ###

J <- 10
wav <- data_wav[[1]]
tmp <- data$ret$pt
colnames(tmp)[9:10] <- c("U.K.", "U.S.")
index <- 10
var <- setdiff(1:length(wav), index)
cross.cor <- matrix(NA, ncol = length(var), nrow = J+2, dimnames = list(c("raw", paste(rep("d", J), 1:J, sep = ""), paste("a", J, sep = "")), colnames(tmp)[-index]))

### LOOP OVER INDICES AND LEVELS ###

for(j in 1:length(var)) {
    
    cross.cor[1,j] <- spin.correlation(tmp[,index], tmp[,var[j]], 0)
    
    for(k in 1:(J+1)) {
      
      print(c(j,k))
      
      error1 <- is.null(tryCatch(wav[[index]][[k]], error = function(x) NULL))
      error2 <- is.null(tryCatch(wav[[var[j]]][[k]], error = function(x) NULL))
      
      if(error1 | error2) {
        cross.cor[1+k,j] <- NA
      } else {
        cross.cor[1+k,j] <- spin.correlation(wav[[index]][[k]], wav[[var[j]]][[k]], 0)
      }
      
    }
    
}

### MAKE TABLE ###

tab <- list()
tab$a <- format(round(cross.cor, 3), nsmall = 3)

########################################################################################
##### VOLATILITY ###########################################################################

### SETUP ###

J <- 10
wav <- data_wav[[2]]
tmp <- data$vol$pt
colnames(tmp)[9:10] <- c("U.K.", "U.S.")
index <- 10
var <- setdiff(1:length(wav), index)
cross.cor <- matrix(NA, ncol = length(var), nrow = J+2, dimnames = list(c("raw", paste(rep("d", J), 1:J, sep = ""), paste("a", J, sep = "")), colnames(tmp)[-index]))

### LOOP OVER INDICES AND LEVELS ###

for(j in 1:length(var)) {
  
  cross.cor[1,j] <- spin.correlation(tmp[,index], tmp[,var[j]], 0)
  
  for(k in 1:(J+1)) {
    
    print(c(j,k))
    
    error1 <- is.null(tryCatch(wav[[index]][[k]], error = function(x) NULL))
    error2 <- is.null(tryCatch(wav[[var[j]]][[k]], error = function(x) NULL))
    
    if(error1 | error2) {
      cross.cor[1+k,j] <- NA
    } else {
      cross.cor[1+k,j] <- spin.correlation(wav[[index]][[k]], wav[[var[j]]][[k]], 0)
    }
    
  }
  
}

### MAKE TABLE ###

tab$b <- format(round(cross.cor, 3), nsmall = 3)

########################################################################################
##### LATEX TABLE ######################################################################
########################################################################################

source("https://raw.githubusercontent.com/siverskog/latex_table/master/latex_table.R")

names <- colnames(data$ret$pt)[-index]
#names[9] <- "U.K."

h <- list()
h$a <- header(list("RETURN", names), cmid = FALSE)
h$b <- header(list("VOLATILITY", names), cmid = FALSE)

cross_cor <- latex.table(h, tab, 10)
write(cross_cor, file = "table04_cross_cor.tex")

########################################################################################
##### CAUSALITY WITH U.S REAL ESTATE ###################################################
########################################################################################


########################################################################################
##### SAVE DATA ########################################################################
########################################################################################


