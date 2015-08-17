########################################################################################
##### REAL ESTATE PROJECT
##### By Jonathan Siverskog
##### 2015-08-16
#####
##### PART 99: FUNCTIONS
#####
########################################################################################

########################################################################################
##### PLOT FUNCTION ####################################################################
########################################################################################

.plot <- function(x, file = "figure.png", res = 300, print = TRUE, col = "deepskyblue3", brk = TRUE) {
  
  if(print) {png(file = file, units = c("mm"), width = 247, height = 140, res = res)}
  
  par(mar = c(2.5,3,2,1), mfrow = c(4,3))
  
  for(i in 1:ncol(x$pt)) {
    
    plot(as.Date(rownames(x$pt)), x$pt[,i], type = "l", col = col, las = 1, ylab = NA, xlab = NA, xaxt = "n", main = colnames(x$pt)[i])
    if(brk) {abline(v = as.Date("2008-09-15"), lty = 2)}
    axis.Date(side = 1, at = seq(as.Date("2004-01-01"), as.Date("2016-01-01"), by = "2 years"))
    axis.Date(side = 1, at = seq(as.Date("2004-01-01"), as.Date("2016-01-01"), by = "years"), labels = FALSE)
    axis.Date(side = 1, at = seq(as.Date("2004-01-01"), as.Date("2016-06-01"), by = "quarters"), labels = FALSE, tck = -0.03)
    
  }
  
  if(print) {dev.off()}
  
}

########################################################################################
##### DESCRIPTIVE STATISTICS ###########################################################
########################################################################################

desc.stat <- function(df, dec = 2, dlog = TRUE, obsperyear = 260, only.stars = TRUE) {
  
  result <- as.data.frame(matrix(NA, nrow = ncol(df), ncol = 9))
  colnames(result) <- c("Obs", "Mean", "StdDev", "Skewness", "Kurtosis", "JB", "Q(20)", "Q-sq(20)", "ARCH(20)")
  
  for(i in 1:ncol(df)) {
    
    x <- as.numeric(na.exclude(df[,i]))
    
    if(length(x)>50) {
      result[i,"Obs"] <- format(round(length(x), 0), nsmall = 0)
      result[i,"Skewness"] <- format(round(.skew(x), dec), nsmall = dec)
      result[i,"Kurtosis"] <- format(round(.kurt(x), dec), nsmall = dec)
      
      result[i,"JB"] <- sign(jb.test(x), digits = dec, only.stars = only.stars)
      result[i,"Q(20)"] <- sign(q.test(x, lag = 20), digits = dec, only.stars = only.stars)
      result[i,"$Q^2$(20)"] <- sign(q.test(x, lag = 20, sq = TRUE), digits = dec, only.stars = only.stars)
      result[i,"ARCH(20)"] <- sign(arch.test(x, lag = 20), digits = dec, only.stars = only.stars)
    } else {
      result[i,] <- rep(NA, ncol(result))
      result[i,"Obs"] <- 0
    }
    
    if(dlog) {
      result[i,"Mean"] <- format(round(mean(x)*obsperyear*100, dec), nsmall = dec)
      result[i,"StdDev"] <- format(round(sd(x)*sqrt(obsperyear)*100, dec), nsmall = dec)
    } else{
      result[i,"Mean"] <- format(round(mean(x), dec), nsmall = dec)
      result[i,"StdDev"] <- format(round(sd(x), dec), nsmall = dec)
    }
    
  }
  
  
  
  if(dlog) {rownames(df)[2:3] <- c("Mean (%)", "StdDev (%)")}
  
  rownames(result) <- colnames(df)
  
  return(result)
  
}

########################################################################################
##### UNIT ROOT TEST ###################################################################
########################################################################################

unit.root <- function(df, dec = 2, only.stars = FALSE) {
  
  result <- as.data.frame(matrix(NA, nrow = ncol(df), ncol = 2))
  colnames(result) <- c("ADF(ct)", "ADF(c)")
  
  for(i in 1:ncol(df)) {
  
    x <- as.numeric(na.exclude(df[,i]))
    
    result[i,"ADF(ct)"] <- sign(adf.test(x, type = "ct"), digits = dec, only.stars = only.stars)
    result[i,"ADF(c)"] <- sign(adf.test(x, type = "c"), digits = dec, only.stars = only.stars)
    
  }
  
  return(result)
  
}

########################################################################################
##### DESCRIPTIVE STATISTICS ###########################################################
########################################################################################

#desc.stat <- function(data, dec = 2, dlog = TRUE, obsperyear = 260, only.stars = TRUE) {
#  
#  z <- .nested.list(levels = c(length(data), length(data[[1]])))
#  
#  for(i in 1:length(data)) {
#    for(j in 1:length(data[[1]])) {
#      for(k in 1:ncol(data[[1]][[1]])) {
#       z[[i]][[j]][[k]] <- .desc.stat(data[[i]][[j]][,k], dec, dlog, obsperyear, only.stars)
#      }
#    }
#  }
#  
#  return(z)
#  
#}

