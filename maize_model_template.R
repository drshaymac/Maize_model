########################################################################################
# Summary: Predicting maize yields using a simple model
# Author:Shana McDowell
# Date: 2/17/2023
########################################################################################

# Load packages and explanatory variable observations ----
library(tidyverse)
library(lubridate)
library(janitor)

weather <- read_csv("data/weather_observations.csv")
head(weather)

# Define parameters ----
# Baseline temperature for growth [deg. C]
Tb <- 7
# Temperature sum for crop maturity [deg.C/day]
TTM <- 1200
# Temperature sum at the end of leaf area increase [deg.C/day]
TTL <- 700
# Extinction coefficient [--]
K <- 0.7
# Radiation use efficiency [g/MJ]
RUE <- 1.85
# The relative rate of LAI increase for small values of LAI [deg.C/day]
a <- 0.00243
# Maximum LAI [m3/m3]
LAIm <- 7

# Initialize state variables ----
# Vector: Number of simulation days
ndays <- nrow(weather)
# Vector: Thermal time age of the crop on day t [deg.C/day]
TT <- rep(NA, ndays)
# Vector: Leaf area index (area of leaves per unit ground area) [m3/m3]
LAI <- rep(NA, ndays)
# Vector: Biomass of crop per square meter of ground area [g/m2]
B <- rep(NA, ndays)
# Assign initial values
TT[1] <- 0
LAI[1] <- 0.1
B[1] <- 1

# Run the simulation ----
for (day in 1:(ndays-1)) { 
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

# Inspect your model outputs ----
# Create a dataframe with all state variables/outputs
outputs <- data.frame(day = weather$day, TT, LAI, B)
# Read state variable observations
observations <- read_csv("data/state_variable_observations.csv")
# Create a plot showing simulated and observed maize biomass values
ggplot() + 
  geom_point(data = outputs, 
             aes(x = day, y = B),
             color = "red") +
  geom_point(data = observations, 
             aes(x = day, y = Bobs)) +
  theme_bw() +
  labs(x = "Time (day)",
       y = expression("Biomass (g "*m^{-2}*")"))

