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
##### NESTED LIST ######################################################################
########################################################################################

.nested.list <- function(levels = c(2,3)) {
  
  list1 <- list()
  length(list1) <- levels[1]
  
  list2 <- list()
  length(list2) <- levels[2]
  
  for(i in 1:levels[1]) {
    for(j in 1:levels[2]) {
      list2[[j]] <- list()
    }
    list1[[i]] <- list2
  }
  
  return(list1)
  
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
      result[i,"Q-sq(20)"] <- sign(q.test(x, lag = 20, sq = TRUE), digits = dec, only.stars = only.stars)
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
##### DESCRIPTIVE STATISTICS ###########################################################
########################################################################################

.desc.stat <- function(x, dec, dlog, obsperyear, only.stars) {
  
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
    names(z)[2:3] <-  c("Mean (%)", "StdDev (%)")
  }
  
  return(z)
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

########################################################################################
##### LATEX TABLE ######################################################################
########################################################################################

.table <- function(x) {
  
  z <- NULL
  
  for(i in 1:nrow(x)) {
    
    z <- c(z, str_c(c(str_c(c(rownames(x)[i], x[i,]), collapse = " & "), "\\\\"), collapse = " "))
    
  }
  
  z <- str_replace_all(z, "%", "\\\\%")
  
  return(z)
  
}

########################################################################################
##### LATEX TABLE ######################################################################
########################################################################################

.begin.table <- function(ncol, type = "tabularx", align = c("l", rep("X", ncol))) {
  begin <- paste("\\begin{", type, "}{\\textwidth}{", paste(align, collapse = ""), "}", sep = "")
  return(begin)
}

.end.table <- function(type = "tabularx") {
  end <- paste("\\end{", type, "}", sep = "")
  return(end)
}

.multicol <- function(size, names) {
  
  if(length(names)>1) {
    multicol <- str_c(c(str_c(c("", paste("\\multicolumn{", 3, "}", "{c}{", names, "}", sep = "")), collapse = " & "), "\\\\"), collapse = " ")
  } else {
    multicol <- paste("\\multicolumn{", size, "}{c}{", names, "} \\\\")
  }
  
   return(multicol)
}

.cmidrule <- function(size, length) {
  cmidrule <- paste("\\cmidrule(lr){", paste(seq(2,length+1-size,size), seq(1+size,length,size), sep = "-"), "}", sep = "")
  return(cmidrule)
}

.header <- function(names) {
  header <- str_c(c(str_c(c("", names), collapse = " & "), "\\\\"), collapse = " ")
  return(header)
}

header <- function(h) {
  
  n <- length(h[[length(h)]])
  
  if(length(h)>2) {
    
    a <- .multicol(n/length(h[[1]]) + 1, h[[1]])
    b <- .multicol(n/length(h[[2]]), h[[2]])
    c <- .cmidrule(n/length(h[[2]]), n + 1)
    d <- .header(h[[3]])
    
    header <- c("\\toprule", a, "\\toprule", b, c, d, "\\midrule")
    
  } else if(length(h)==2) {
    
    a <- .multicol(n/length(h[[1]]), h[[1]])
    b <- .cmidrule(n/length(h[[1]]), n + 1)
    c <- .header(h[[2]])
    
    header <- c("\\toprule", a, b, c, "\\midrule")
    
  } else {
    
    a <- .header(h[[1]])
    header <- c("\\toprule", a, "\\midrule")
    
  }
  
  return(header)
}

latex.table <- function(h, t, type, size) {
  
  begin <- .begin.table(size, type)
  end <- .end.table(type)
  
  tab <- NULL
  
  for(i in 1:length(h)) {
    tab <- c(tab, h[[i]], t[[i]])
  }
  
  tab <- c(begin, tab, "\\bottomrule", end)
  return(tab)
  
}