########################################################################################
##### REAL ESTATE PROJECT
##### By Jonathan Siverskog
##### 2015-08-16
#####
##### PART 01: DESCRIPTIVE STATISTICS
#####
########################################################################################

library(stringr)
source("https://raw.githubusercontent.com/siverskog/r_functions/master/diagnostics.R")
source("99realestate_functions.R")
load("realestate.RData")

########################################################################################
##### DESCRIPTIVE STATISTICS ###########################################################
########################################################################################

tab_ret <- NULL
tab_vol <- NULL

for(i in 1:length(data$ret)) {
  
  tmp <- desc.stat(data[[1]][[i]])
  tab_ret <- cbind(tab_ret, t(tmp))
  tmp <- desc.stat(data[[2]][[i]])
  tab_vol <- cbind(tab_vol, t(tmp))
  
}

tab_ret <- tab_ret[,order(colnames(tab_ret))]
tab_vol <- tab_vol[,order(colnames(tab_vol))]

tab <- list()

tab$a <- .table(tab_ret[,1:15])
tab$b <- .table(tab_ret[,16:30])
tab$c <- .table(tab_vol[,1:15])
tab$d <- .table(tab_vol[,16:30])

rm(tmp, tab_ret, tab_vol, i, j)

########################################################################################
##### LATEX TABLE ######################################################################
########################################################################################

names <- colnames(data$ret$pt)

#begin <- .begin.table(15, "tabularx")
#end <- .end.table("tabularx")
#h <- list()
# <- list()
#h$a <- .multicol(16, "RETURN")
#h$b <- .multicol(16, "VOLATILITY")
#m$a <- .multicol(3, names[1:5])
#m$b <- .multicol(3, names[6:10])
#c <- .cmidrule(3, 16)
#d <- .header(rep(c("$P_T$", "$P_1$", "$P_2$"),5))

h <- list()
h$a <- header(list("RETURN", names[1:5], rep(c("$P_T$", "$P_1$", "$P_2$"),5)))
h$b <- header(list(names[6:10], rep(c("$P_T$", "$P_1$", "$P_2$"),5)))
h$c <- header(list("VOLATILITY", names[1:5], rep(c("$P_T$", "$P_1$", "$P_2$"),5)))
h$d <- header(list(names[6:10], rep(c("$P_T$", "$P_1$", "$P_2$"),5)))

desc <- latex.table(h, tab, "tabularx", 15)
write(desc, file = "table01_desc.tex")

########################################################################################
##### CORRELATION ######################################################################
########################################################################################

########################################################################################
##### RETURN ###########################################################################

tmp <- ret
tab2a <- NULL

for(i in 1:length(tmp)) {
  tab3a <- cbind(tab3a, cor(tmp[[i]])[10,])
}

colnames(tab3a) <- c("PT", "P1", "P2", "P3")


########################################################################################
##### VOLATILITY #######################################################################

tmp <- vol
tab3b <- NULL

for(i in 1:length(tmp)) {
  tab3b <- cbind(tab3b, cor(tmp[[i]])[10,])
}

colnames(tab3b) <- c("PT", "P1", "P2", "P3")

tab3 <- cbind(tab3a, tab3b)
tab3 <- tab3[-nrow(tab3),]

tab3 <- xtable(x = tab3)
align(tab3) <- rep("l", ncol(tab3)+1)
digits(tab3) <- rep(2, ncol(tab3)+1)

print.xtable(tab3, file = "table03_cor.tex", floating = FALSE, booktabs = TRUE)

########################################################################################
##### SAVE DATA ########################################################################
########################################################################################

save(ret, vol, file = "realestate.RData")
rm(list = ls())