##########################################################
## Calibrating the Maize Model ##
## Author: Shana McDowell
##########################################################

# Load packages
library(tidyverse)
library(readr)
### User-defined function: Maze Model Function---- 
maize_model <- function(weather,Tb,TTM,TTL,K,RUE,a,LAIm,TT0,LAI0,B0,start,end,step){
  # Define parameters ----
  # Baseline temperature for growth [deg. C]
  Tb <- Tb
  # Temperature sum for crop maturity [deg.C/day]
  TTM <- TTM
  # Temperature sum at the end of leaf area increase [deg.C/day]
  TTL <- TTL
  # Extinction coefficient [--]
  K <- K
  # Radiation use efficiency [g/MJ]
  RUE <- RUE
  # The relative rate of LAI increase for small values of LAI [deg.C/day]
  a <- a
  # Maximum LAI [m3/m3]
  LAIm <- LAIm
  
  # Initialize state variables ----
  # Vector: time vector
  t <- seq(start,end,step)
  # Vector: Thermal time age of the crop on day t [deg.C/day]
  TT <- rep(NA, 365)
  # Vector: Leaf area index (area of leaves per unit ground area) [m3/m3]
  LAI <- rep(NA, 365)
  # Vector: Biomass of crop per square meter of ground area [g/m2]
  B <- rep(NA, 365)
  # Assign initial values
  TT[start] <- TT0
  LAI[start] <- LAI0
  B[start] <- B0
  
  # Run the simulation ----
  for (day in t[1:(length(t)-1)]) {
    # Calculate rates of change
    ## dTT:
    dTT <- max(((weather$Tmin[day] + weather$Tmax[day])/2) - Tb,0)
    ## dB:
    if(TT[day] <= TTM) {dB <- RUE*(1-exp(-K*LAI[day]))*weather$I[day]}
    else {dB <- 0}
    ## dLAI:
    if (TT[day] <= TTL) {dLAI <- a*dTT*LAI[day]*max(LAIm-LAI[day],0)}
    else {dLAI <- 0}
    
    # Update state variables
    TT[day + 1] <- TT[day] + dTT
    LAI[day + 1] <-LAI[day] + dLAI
    B[day + 1] <- B[day] + dB
  }
  outputs <- data.frame(day = weather$day[t], 
                        TT = TT[t], 
                        LAI = LAI[t],
                        Bsim = B[t])

  
  return(outputs)
}

### User-defined function: Error minimization ----
biomass_error <- function(parameters, observed_responses,weather) {
  #### Assign initial values to parameters
  ## Parameters that you are calibrating should be assigned values from the parameters vector (input to the minimize_error fcn)
  ## Parameters that you are NOT calibrating should be assigned a value (e.g. n_storms)
  # Baseline temperature for growth [deg. C]
  Tb <- parameters[1]
  # Temperature sum for crop maturity [deg.C/day]
  TTM <- parameters[2]
  # Temperature sum at the end of leaf area increase [deg.C/day]
  TTL <- parameters[3]
  # Extinction coefficient [--]
  K = 0.7
  # Radiation use efficiency [g/MJ]
  RUE = 1.85
  # The relative rate of LAI increase for small values of LAI [deg.C/day]
  a = 0.00243
  # Maximum LAI [m3/m3]
  LAIm = 7
  # Initial values for state variables
  TT0 = 0
  LAI0 = 0.1
  B0 = 1
  # Time vector information
  start <- 1
  end <- nrow(weather)
  step <- 1
  
  # Number of storms to simulate
  #n_days <- nrow(weather)
  #### Run model
  maize_simulation <- maize_model(weather,Tb,TTM,TTL,
                                  K,RUE,a,LAIm,TT0,LAI0,B0,start,end,step)
  maize_simulation <- maize_simulation %>% filter(day %in% c(140,160,180,200,220,240))
  #### Create a dataframe that includes the observed rainfall
  # Note that simulated_storms is longer than our observed data, which is why we're trimming it with [1:nrow()]
  sim_obs <-  observed_responses %>% 
    mutate(simulated = maize_simulation$Bsim) %>% 
    rename(observed = Bobs) # rename the "Rainfall_in" column to "Observed"
    
  #### Calculate Error: Difference between total rainfall over period of record
  #er <- abs(sum(sim_obs$observed)-sum(sim_obs$simulated)) # This needs to be positive! = abs()
  er <- abs(sum((sim_obs$simulated-sim_obs$observed)^2,na.rm = TRUE))
  
  #### Return error
  return(er)
}
#Initial values vector
initial_values <- c(7, 1200, 700)


p <- read_csv("calibration_data/state_variable_observations.csv")
q <- read.csv("calibration_data/weather_observations.csv")
### Calibrate
results <- 
  optim(par = initial_values,
        fn = biomass_error,
        observed_responses = p ,
        gr = NULL,
        weather = q, 
        method = "Nelder-Mead")

##baseline simulation
baseline_simulation <- maize_model(q, 
                          Tb = 7, 
                          TTM = 1200, 
                          TTL = 700, 
                          K = 0.7, 
                          RUE = 1.85, 
                          a = 0.00243, 
                          LAIm = 7,
                          TT0 = 0,
                          LAI0 = 0.1,
                          B0 = 1,
                          start = 1,
                          end = nrow(q))

# Calculate model efficiency: Uncalibrated
baseline_simulation <- baseline_simulation %>% filter(day %in% c(140,160,180,200,220,240))
sim_obs <- left_join(p, baseline_simulation)
EF_uncalibrated <- 1 - (sum((sim_obs$Bobs - sim_obs$Bsim)^2)) / (sum((sim_obs$Bobs - mean(sim_obs$Bobs))^2))
EF_uncalibrated

###calibrated simulation
# Run the calibrated model
calibrated_simulation <- maize_model(q, 
                                     Tb = results$par[1], 
                                     TTM = results$par[2], 
                                     TTL = results$par[3], 
                                     K = 0.7, 
                                     RUE = 1.85, 
                                     a = 0.00243, 
                                     LAIm = 7,
                                     TT0 = 0,
                                     LAI0 = 0.1,
                                     B0 = 1,
                                     start = 1,
                                     end = nrow(q))

# Calculate model efficiency: Calibrated
sim_obs_cal <- left_join(p, calibrated_simulation)
EF_calibrated <- 1 - (sum((sim_obs_cal$Bobs - sim_obs_cal$Bsim)^2)) / (sum((sim_obs_cal$Bobs - mean(sim_obs_cal$Bobs))^2))
EF_calibrated

# Plot observations and simulated data from the calibrated and baseline models
ggplot() + 
  geom_line(data = calibrated_simulation, aes(x = day, y = Bsim)) +
  geom_line(data = baseline_simulation, aes(x = day, y = Bsim), 
            color = 'red', linetype = 'dotted', size = 1) +
  geom_point(data = p, aes(x = day, y = Bobs)) +
  theme_bw() +
  labs(x = "Time (day)",
       y = expression("Biomass (g "*m^{-2}*")"))

###Pause,Think,Discuss
#1.Goodness-of-fit - Does the increase in predictive performance justify calibration in this case? Why or why not? Yes, .53 to .98 is a huge jump in model EF. We can also see that the calibrated model is a much better to the six points than the uncalibrated model.

#2.Interpreting the calibrated parameters - How did the calibrated parameters differ from the literature-defined values (Tb = 7, TTM = 1200, and TTL = 700)? What do you make of these differences? 