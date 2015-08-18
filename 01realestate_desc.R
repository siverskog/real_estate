########################################################################################
##### REAL ESTATE PROJECT
##### By Jonathan Siverskog
##### 2015-08-16
#####
##### PART 01: DESCRIPTIVE STATISTICS
#####
########################################################################################

<<<<<<< HEAD
=======
library(stringr)
>>>>>>> 905c11cb412b01f348ca20a9fff401a53dd3dec4
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

<<<<<<< HEAD
tab$a <- tab_ret[,1:15]
tab$b <- tab_ret[,16:30]
tab$c <- tab_vol[,1:15]
tab$d <- tab_vol[,16:30]

rm(tmp, tab_ret, tab_vol, i)

########################################################################################
##### LATEX TABLE ######################################################################
########################################################################################

source("https://raw.githubusercontent.com/siverskog/latex_table/master/latex_table.R")

names <- colnames(data$ret$pt)
h <- list()
h$a <- header(list("RETURN", names[1:5], rep(c("$P_T$", "$P_1$", "$P_2$"),5)))
h$b <- header(list(names[6:10], rep(c("$P_T$", "$P_1$", "$P_2$"),5)))
h$c <- header(list("VOLATILITY", names[1:5], rep(c("$P_T$", "$P_1$", "$P_2$"),5)))
h$d <- header(list(names[6:10], rep(c("$P_T$", "$P_1$", "$P_2$"),5)))

desc <- latex.table(h, tab, 15)
write(desc, file = "table01_desc.tex")

########################################################################################
##### UNIT ROOT ########################################################################
########################################################################################

#
#
=======
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
>>>>>>> 905c11cb412b01f348ca20a9fff401a53dd3dec4

########################################################################################
##### CORRELATION ######################################################################
########################################################################################

#
#

########################################################################################
##### ... ##############################################################################
########################################################################################

rm(list = ls())
