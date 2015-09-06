########################################################################################
##### REAL ESTATE PROJECT
##### By Jonathan Siverskog
##### 2015-08-24
#####
##### PART 00: FORECAST
#####
########################################################################################

load("realestate_wav.RData")
load("realestate.RData")

#install.packages("forecast")
library(forecast)
#install.packages("waveslim")
library(waveslim)
#install.packages("nnet")
library(nnet)
source("C:\\Users\\Jonathan\\Dropbox\\Projekt Gazi\\waveANN.R")

########################################################################################
##### TEST #############################################################################
########################################################################################

### SLECT ONE DECOMPOSED SERIES FOR TESTING ###

x <- wav[[1]][[1]]

### PLOT ORIGINAL SERIES ###

par(mar = c(2,3,2,1))
plot(imodwt(x), type = "l", main = "Original Series", las = 1)

### PLOT WAVELET TRANSFORM ###

par(mar = c(2,3,2,1), mfrow = c(6,2))
for(i in 1:length(x)) {
  plot(x[[i]], type = "l", las = 1)
}

### ESTIMATE ANN FOR EACH LAYER ###

fits <- list()
for(i in 1: length(x)) {
  print(i)
  fits[[i]] <- nnetar(x[[i]], p = 2, size = 5, repeats = 10)
}

### FORECAST 5 STEPS AHEAD

H <- 5
f5 <- x

for(i in 1:length(f5)) {
  
  print(i)
  tmp <- forecast(fits[[i]], h = H)
  f5[[i]][length(f5[[i]])+1:H] <- as.numeric(tmp$mean)
  
}

### PLOT INVERSE OF EXTENDED SERIES TOGETHER WITH ORIGINAL SERIERS ###

par(mar = c(2,3,2,1), mfrow = c(1,1))
z <- imodwt(f5) 
w <- imodwt(x)

plot(z[(length(z)-149-H):(length(z))], type = "l", col = "red")
points(w[(length(w)-149):(length(w))], type = "l")

### FORECAST 5 STEPS AHEAD

H <- 64
f64 <- x

for(i in 1:length(f64)) {
  
  print(i)
  tmp <- forecast(fits[[i]], h = H)
  f64[[i]][length(f64[[i]])+1:H] <- as.numeric(tmp$mean)
  
}

### PLOT INVERSE OF EXTENDED SERIES TOGETHER WITH ORIGINAL SERIERS ###

par(mar = c(2,3,2,1), mfrow = c(1,1))
z <- imodwt(f64) 
w <- imodwt(x)

plot(z[(length(z)-149-H):(length(z))], type = "l", col = "red")
points(w[(length(w)-149):(length(w))], type = "l")

plot(z, type = "l")
points(w, type = "l", col = "red")

########################################################################################
##### TEST #############################################################################
########################################################################################

x <- data$ret$pt$AUSTRALIA # UNIVARIATE TIME SERIES TO TEST CODE
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


nnetar()

y <- cbind(y1 = x, y2 = c(NA, x[-length(x)])) 
formula <- y1 ~ y2

ffann <- nnet(formula = formula, data = y, size = 5, na.action = "na.omit")

waveANN()

