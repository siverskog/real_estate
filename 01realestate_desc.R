########################################################################################
##### REAL ESTATE PROJECT
##### By Jonathan Siverskog
##### 2015-08-16
#####
##### PART 01: DESCRIPTIVE STATISTICS
#####
########################################################################################

########################################################################################
##### DESCRIPTIVE STATISTICS ###########################################################
########################################################################################

library(stringr)
source("https://raw.githubusercontent.com/siverskog/r_functions/master/diagnostics.R")
source("99realestate_functions.R")
load("realestate.RData")

tab_ret <- NULL
tab_vol <- NULL

for(i in 1:length(temp)) {
  
  tmp <- desc.stat(temp[[i]], dlog = TRUE, obsperyear = 260)
  tab_ret <- cbind(tab_ret, t(tmp))
  
  tmp <- desc.stat(temp[[i]], dlog = TRUE, obsperyear = 260)
  tab_vol <- cbind(tab_vol, t(tmp))
  
}

tab_ret <- tab_ret[,order(colnames(tab_ret))]
colnames(tab_ret) <- rep(c("$P_T$", "$P_1$", "$P_2$"),10)

tab_vol <- tab_vol[,order(colnames(tab_vol))]
colnames(tab_vol) <- rep(c("$P_T$", "$P_1$", "$P_2$"),10)

########################################################################################
##### LATEX TABLE ######################################################################
########################################################################################

.table <- function(x) {
  
  tmp <- NULL
  
  for(i in 1:nrow(x)) {
    tmp <- c(tmp, str_c(c(str_c(c(rownames(x)[i], x[i,]), collapse = " & "), "\\\\"), collapse = " "))
  }
  
  tmp <- str_replace_all(tmp, "%", "\\\\%")
  
  return(tmp)
  
}

########################################################################################
##### LATEX TABLE ######################################################################
########################################################################################

begin <- paste("\\begin{tabular}{", paste(rep("l", 16), collapse = ""), "}", sep = "")
end <- "\\end{tabular}"

tab_a <- .table(tab_ret[,1:15])
tab_b <- .table(tab_ret[,16:30])
tab_c <- .table(tab_vol[,1:15])
tab_d <- .table(tab_vol[,16:30])


h1 <- paste("\\multicolumn{", 16, "}{c}{", "RETURN", "} \\\\")
h2 <- paste("\\multicolumn{", 16, "}{c}{", "VOLATILITY", "} \\\\")
h3 <- str_c(c(str_c(c("", rep(c("$P_T$", "$P_1$", "$P_2$"),5)), collapse = " & "), "\\\\"), collapse = " ")
cmid <- paste("\\cmidrule(lr){", paste(seq(2,14,3), seq(4,16,3), sep = "-"), "}", sep = "")
multicol1 <- str_c(c(str_c(c("", paste("\\multicolumn{", 3, "}", "{c}{", names[1:5], "}", sep = "")), collapse = " & "), "\\\\"), collapse = " ")
multicol2 <- str_c(c(str_c(c("", paste("\\multicolumn{", 3, "}", "{c}{", names[6:10], "}", sep = "")), collapse = " & "), "\\\\"), collapse = " ")
header1 <- c("\\toprule", multicol1, cmid, h3, "\\midrule")
header2 <- c("\\toprule", multicol2, cmid, h3, "\\midrule")

tab <- c(begin, h1, header1, tab_a, header2, tab_b, "\\bottomrule", h2, header1, tab_c, header2, tab_d, "\\bottomrule", end)
write(tab, file = "table.tex")


table <- c(begin, "\\toprule", header, "\\midrule", tmp, "\\bottomrule", end)
table <- str_replace_all(table, "%", "\\\\%")
write(table, file)


latex.table <- function(x, file = "table.tex") {
  
  begin <- paste("\\begin{tabular}{", paste(rep("l", ncol(x)+1), collapse = ""), "}", sep = "")
  end <- "\\end{tabular}"
  header <- str_c(c(str_c(c("", colnames(x)), collapse = " & "), "\\\\"), collapse = " ")
  
  tmp <- NULL
  
  for(i in 1:nrow(x)) {
    tmp <- c(tmp, str_c(c(str_c(c(rownames(x)[i], x[i,]), collapse = " & "), "\\\\"), collapse = " "))
  }
  
  table <- c(begin, "\\toprule", header, "\\midrule", tmp, "\\bottomrule", end)
  table <- str_replace_all(table, "%", "\\\\%")
  write(table, file)
  
}

latex.table(tab)


rm(data, tmp, i, names)

########################################################################################
##### RETURN ###########################################################################

temp <- ret
tab <- NULL

for(i in 1:length(temp)) {
  
  tmp <- desc.stat(temp[[i]], dlog = TRUE, obsperyear = 260)
  tmp[,2:5] <- format(round(tmp[2:5], 2), nsmall = 2)
  tab <- cbind(tab, t(tmp))
  
}

tab <- tab[,order(colnames(tab))]
colnames(tab) <- rep(c("PT", "P1", "P2"),10)

tab1a <- tab[,1:15]
tab1b <- tab[,16:30]

tab1a <- xtable(x = tab1a)
align(tab1a) <- rep("l", ncol(tab1a)+1)
digits(tab1a) <- rep(0, ncol(tab1a)+1)

tab1b <- xtable(x = tab1b)
align(tab1b) <- rep("l", ncol(tab1b)+1)
digits(tab1b) <- rep(0, ncol(tab1b)+1)

#tab <- rbind(tab1a, tab1b)

#print.xtable(tab1a, file = "table01a_desc_ret.tex", floating = FALSE, booktabs = TRUE)
#print.xtable(tab1b, file = "table01b_desc_ret.tex", floating = FALSE, booktabs = TRUE)

########################################################################################
##### VOLATILITY #######################################################################

temp <- vol
tab <- NULL

for(i in 1:length(temp)) {
  
  tmp <- desc.stat(temp[[i]], dlog = TRUE, obsperyear = 260)
  tmp[,2:5] <- format(round(tmp[2:5], 2), nsmall = 2)
  tab <- cbind(tab, t(tmp))
  
}

tab <- tab[,order(colnames(tab))]
colnames(tab) <- rep(c("PT", "P1", "P2"),10)

tab1c <- tab[,1:15]
tab1d<- tab[,16:30]

tab1c <- xtable(x = tab1c)
align(tab1c) <- rep("l", ncol(tab1c)+1)
digits(tab1c) <- rep(0, ncol(tab1c)+1)

tab1d <- xtable(x = tab1d)
align(tab1d) <- rep("l", ncol(tab1d)+1)
digits(tab1d) <- rep(0, ncol(tab1d)+1)

tab <- rbind(tab1a, tab1b, tab1c, tab1d)

#print.xtable(tab2a, file = "table02a_desc_vol.tex", floating = FALSE, booktabs = TRUE)
#print.xtable(tab2b, file = "table02b_desc_vol.tex", floating = FALSE, booktabs = TRUE)

print.xtable(tab, file = "table01_desc.tex", floating = FALSE, booktabs = TRUE)

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