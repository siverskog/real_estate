########################################################################################
##### REAL ESTATE PROJECT
##### Jonathan Siverskog
##### 2015-06-15
########################################################################################

wd <- "C:\\Users\\Jonathan\\Dropbox\\Projekt Gazi\\06_Housing_Wavelet"
setwd(wd)
load("realestate.RData")

#install.packages("forecast")
library(forecast)
#install.packages("waveslim")
library(waveslim)
#install.packages("nnet")
library(nnet)

########################################################################################
##### TEST #############################################################################
########################################################################################

x <- ret$pt$AUSTRALIA # UNIVARIATE TIME SERIES TO TEST CODE
H <- 5 # FORECAST HORIZON
J <- 8 # LEVELS OF WAVELET TRANSFORM
test.samp <- 10 # HOW MANY OBSERVATIONS WILL BE USED TO EVALUATE THE FORECASTS?
fe_raw <- NULL
fe_wav <- NULL

for(i in 1:(test.samp-H+1)) {
  
  ### RAW ###
  
  training <- x[i:(length(x)-test.samp+i-1)] # TRAINING SAMPLE
  test <- x[(length(x)-test.samp+i):(length(x)-test.samp+i+H-1)] # TEST SAMPLE
  
  print(i)
  
  fit_raw <- auto.arima(training) # FIT MODEL TO RAW DATA
  f_raw <- forecast(fit_raw, h = H) # FORECAST
  f_raw <- as.numeric(f_raw$mean)
  fe_raw <- rbind(fe_raw, test-f_raw) # FORECAST ERRORS FROM 1 TO H STEPS AHEAD
  
  ### WAVELET ###
  
  wav <- modwt(x = training, wf = "la8", n.levels = 8, boundary = "periodic") # MOD WAVELET TRANSFORM
  
  for(j in 1:(J+1)) {
    
    print(j)
    
    fit_wav <- auto.arima(wav[[j]]) # FIT MODEL TO WAVELET COMPONENTS D_j - D_J AND A_J
    tmp <- forecast(fit_wav, h = H) # FORECAST
    wav[[j]][length(wav[[j]])+1:H] <- as.numeric(tmp$mean) # ADD PREDICTED VALUES TO WAVELET DETAILS/APPROXIMATION
    
  }
  
  f_wav <- imodwt(wav)[(length(training)+1):(length(training)+H)] # GENERATE FORECAST THROUGH INVERSION OF THE MODWT
  fe_wav <- rbind(fe_wav, test-f_wav) # FORECAST ERRORS
  
}


cbind("MSFE_raw" = apply(fe_raw^2, 2, mean), # MEAN SQUARE FORECAST ERROR
      "MSFE_wav" = apply(fe_wav^2, 2, mean),
      "MAFE_raw" = apply(abs(fe_raw), 2, mean), # MEAN ABSOLUTE FORECAST ERROR
      "MAFE_wav" = apply(abs(fe_wav), 2, mean))

dm.test(fe_raw[,H], fe_wav[,H], h = H) # DM TEST FOR HORIZON = 5 (SHOULD ALSO BE RUN FOR OTHER HORIZONS)

########################################################################################
##### ... ##############################################################################
########################################################################################


y <- cbind(y1 = x, y2 = c(NA, x[-length(x)])) 
formula <- y1 ~ y2

ffann <- nnet(formula = formula, data = y, size = 5, na.action = "na.omit")

