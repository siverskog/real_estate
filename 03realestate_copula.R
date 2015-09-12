########################################################################################
##### FLEXIBLE COPULA ESTIMATION FOR REAL ESTATE DATA ##################################
##### By Jonathan Siverskog                           ##################################
##### 2015-09-12                                      ##################################
########################################################################################

source("https://raw.githubusercontent.com/siverskog/r_functions/master/flexible_copula.R")

########################################################################################
##### LOAD DATA ########################################################################
########################################################################################

load("realestate.RData")
data <- data$pri$pt
colnames(data) <- c("AU", "AE", "HK", "JP", "MY", "PH", "SG", "TH", "GB", "US")

##### CONVERT TO WEEKLY DATA #####

tmp <- data[weekdays(as.Date(rownames(data)))==weekdays(as.Date("2004-01-02")),]
as.Date(rownames(tmp))[-1]-as.Date(rownames(tmp))[-nrow(tmp)]
data <- tmp
rm(tmp)
data <- as.data.frame(diff(as.matrix(data)))

########################################################################################
##### APPLY AR-GARCH-FILTER TO DATA ####################################################
########################################################################################

best <- garch.spec(data) # FIND BEST GARCH-SPECIFICATION
res <- garch.filter(x = data, type = "gjrGARCH", error.dist = "std", garch = c(1,1), arma = c(4,0), package = "rugarch")

##### DESCRIPTIVE STATISTICS FOR GARCH-FILTERED RETURNS #####

#source("https://raw.githubusercontent.com/siverskog/latex_table/master/latex_table.R")

desc.stat(res, dec = 2, dlog = TRUE, obsperyear = 52, only.stars = FALSE)
apply(res, 2, function(x) q.test(x = x, lag = 10, sq = FALSE))

##### PLOT #####

par(mar = c(2,3,2,1), mfrow = c(5,2))
for(i in 1:ncol(res)) {
  plot(as.Date(rownames(res)), res[,i], type = "l", main = colnames(res)[i], las = 1)
}

########################################################################################
##### SETUP FOR COPULA #################################################################
########################################################################################

##### SET PAIRS OF VARIABLES TO SELECT FROM DATA #####

x <- rep(10, 9)
y <- 1:9

##### SET GRID FOR ESTIMTION #####

grid <- seq(-2,2,0.1)

########################################################################################
##### COPULA ###########################################################################
########################################################################################

bw <- compute.bandwidth(res, x = x, y = y, nmulti = 5) # COMPUTE BANDWIDTH FOR CDF
c <- nonparametric.copula(res, x, y, bw, as.vector = FALSE) # NON-PARAMETRIC COPULA
bc <- boot.copula(data, x = x, y = y, grid = grid, rep = 500, bw = bw, type = "gjrGARCH", error.dist = "std", garch = c(1,1), arma = c(4,0), package = "rugarch") # BOOTSTRAP
ec <- empirical.copula(res, x = x, y = y, grid = grid) # EMPIRICAL COPULA

##### PLOT CONDITIONAL PROBABILITIES #####

plot.copula(c = c, bc = bc, ec = ec, mfrow = c(3,3), print = TRUE, file = "realestate_copula.png", w = 250, h = 250)