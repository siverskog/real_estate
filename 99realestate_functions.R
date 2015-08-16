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

.plot <- function(x, file = "figure.png", res = 300) {
  
  png(file = file, units = c("mm"), width = 247, height = 140, res = res)
  
  par(mar = c(2.5,3,2,1), mfrow = c(4,3))
  
  for(i in 1:ncol(x$pt)) {
    
    plot(as.Date(rownames(x$pt)), x$pt[,i], type = "l", col = "deepskyblue3", las = 1, ylab = NA, xlab = NA, xaxt = "n", main = colnames(x$pt)[i])
    abline(v = as.Date("2008-09-15"), lty = 2)
    axis.Date(side = 1, at = seq(as.Date("2004-01-01"), as.Date("2016-01-01"), by = "2 years"))
    axis.Date(side = 1, at = seq(as.Date("2004-01-01"), as.Date("2016-01-01"), by = "years"), labels = FALSE)
    axis.Date(side = 1, at = seq(as.Date("2004-01-01"), as.Date("2016-06-01"), by = "quarters"), labels = FALSE, tck = -0.03)
    
  }
  
  dev.off()
  
}

########################################################################################
##### DESCRIPTIVE STATISTICS ###########################################################
########################################################################################

desc.stat <- function(x, dec, dlog, obsperyear, only.stars) {
  
  z <- c("Obs" = format(round(length(x), 0), nsmall = 0),
         "Mean" = format(round(mean(x), dec), nsmall = dec),
         "StdDev" = format(round(sd(x), dec), nsmall = dec),
         "Skewness" = format(round(.skew(x), dec), nsmall = dec),
         "Kurtosis" = format(round(.kurt(x), dec), nsmall = dec),
         "JB" = sign(jb.test(x), digits = dec, only.stars = only.stars),
         "Q(20)" = sign(q.test(x, lag = 20), digits = dec, only.stars = only.stars),
         "$Q^2$(20)" = sign(q.test(x, lag = 20, sq = TRUE), digits = dec, only.stars = only.stars),
         "ARCH(20)" = sign(arch.test(x, lag = 20), digits = dec, only.stars = only.stars))
  
  if(dlog) {
    z["Mean"] <- format(round(mean(x)*obsperyear*100, dec), nsmall = dec)
    z["StdDev"] <- format(round(sd(x)*sqrt(obsperyear)*100, dec), nsmall = dec)
    names(z[c("Mean", "StdDev")]) <-  c("Mean (%)", "StdDev (%)")
  }
  
  return(z)
}

desc.stat()

test <- desc.stat(data$ret$pt[,1], dec = 2, dlog = TRUE, obsperyear = 260, only.stars = TRUE)

test[c("Mean", "StdDev")]

result <- list

desc.stat <- function(df, dec = 2, dlog = FALSE, obsperyear = 260, only.stars = TRUE) {
  
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
      result[i,"Q-sq(20)"] <- sign(q.test(x, lag = 20, sq = TRUE), digits = dec, only.stars = only.stars)
      result[i,"ARCH(20)"] <- sign(arch.test(x, lag = 20), digits = dec, only.stars = only.stars)
    } else {
      result[i,] <- rep(NA, ncol(result))
      result[i,"Obs"] <- 0
    }
    
    if(dlog) {
      result[i,"Mean"] <- format(round(mean(x)*obsperyear*100, dec), nsmall = dec)
      result[i,"StdDev"] <- format(round(sd(x)*sqrt(obsperyear)*100, dec), nsmall = dec)
    } else {
      result[i,"Mean"] <- format(round(mean(x), dec), nsmall = dec)
      result[i,"StdDev"] <- format(round(sd(x), dec), nsmall = dec)
    }
    
  }
  
  if(dlog) {
    colnames(result)[colnames(result)=="Mean"] <- "Mean (%)"
    colnames(result)[colnames(result)=="StdDev"] <- "StdDev (%)"
  }
  
  rownames(result) <- colnames(df)
  
  return(result)
  
}