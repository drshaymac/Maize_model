##########################################################
## Calibrating the Maize Model ##
## Author: Shana McDowell
##########################################################

# Load packages
library(tidyverse)

# User-defined function: Maze Model Function----
maize_model <- function(weather,Tb_param,
                        TTM_param,TTL_param,
                        K_param,RUE_param,a_param,
                        LAIm_param,TT0,LAI0,B0,
                        start,end,step){
  # Define parameters ----
  # Baseline temperature for growth [deg. C]
  Tb <- Tb_param
  # Temperature sum for crop maturity [deg.C/day]
  TTM <- TTM_param
  # Temperature sum at the end of leaf area increase [deg.C/day]
  TTL <- TTL_param
  # Extinction coefficient [--]
  K <- K_param
  # Radiation use efficiency [g/MJ]
  RUE <- RUE_param
  # The relative rate of LAI increase for small values of LAI [deg.C/day]
  a <- a_param
  # Maximum LAI [m3/m3]
  LAIm <- LAIm_param
  
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
                        B = B[t])
  
  return(outputs)
}

### User-defined function: Error minimization ----
biomass_error <- function(parameters, observed_responses) {
  #### Assign initial values to parameters
  ## Parameters that you are calibrating should be assigned values from the parameters vector (input to the minimize_error fcn)
  ## Parameters that you are NOT calibrating should be assigned a value (e.g. n_storms)
  # Baseline temperature for growth [deg. C]
  Tb <- parameters[1]
  # Temperature sum for crop maturity [deg.C/day]
  TTM <- parameters[2]
  # Temperature sum at the end of leaf area increase [deg.C/day]
  TTL <- parameters[3]
  # Number of storms to simulate
  #n_days <- nrow(weather)
  #### Run model
  maize_simulation <- maize_model(weather,Tb_param,
                                  TTM_param,TTL_param,
                                  K_param,RUE_param,a_param,
                                  LAIm_param,TT0,LAI0,B0,
                                  start,end,step)
  #### Create a dataframe that includes the observed rainfall
  # Note that simulated_storms is longer than our observed data, which is why we're trimming it with [1:nrow()]
  sim_obs <- 
    observed_responses %>% 
    mutate(simulated = maize_simulation[1:nrow(observed_responses)]) %>% 
    rename(observed = Rainfall_in) # rename the "Rainfall_in" column to "Observed"
  #### Calculate Error: Difference between total rainfall over period of record
  er <- abs(sum(sim_obs$observed)-sum(sim_obs$simulated)) # This needs to be positive! = abs()
  #### Return error
  return(er)
}