########################################################################################
# Summary: Creating a model function, quantifying goodness-of-fit, and applying the model to Robeson County,NC
# Author:Shana McDowell
# Date: 2/17/2023
########################################################################################
## 1. SETUP WORKSPACE
# Load packages and explanatory variable observations ----
library(tidyverse)
library(lubridate)
library(janitor)

#load data
weather <- read_csv("data/weather_observations.csv")

## 2. CREATE USER-DEFINED FUNCTION:MAIZE_MODEL()
#Initialize maize_model_function
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
  # Vector: Number of simulation days
  #ndays <- nrow(weather)
  #ndays <- length(seq(start,end,step))
  # Vector: Thermal time age of the crop on day t [deg.C/day]
  TT <- rep(NA, ndays)
  # Vector: Leaf area index (area of leaves per unit ground area) [m3/m3]
  LAI <- rep(NA, ndays)
  # Vector: Biomass of crop per square meter of ground area [g/m2]
  B <- rep(NA, ndays)
  # Assign initial values
  TT[1] <- TT0
  LAI[1] <- LAI0
  B[1] <- B0
  
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
  outputs <- data.frame(day = weather$day, TT, LAI, B)
  #sim_yield <- B[134]
  return( outputs)
}

# Inspect your function outputs --
function_outputs <- maize_model(weather, Tb = 7, TTM = 1200, TTL = 700, K = 0.7, RUE = 1.85, a = 0.001, LAIm = 7, TT0 = 0, LAI0 = 0.1, B0 = 1, start = 1, end = 365, step = 1)

# Inspect new output
head(function_outputs)

# Read state variable observations
observations <- read_csv("data/state_variable_observations.csv")

# Create a plot showing simulated and observed maize biomass values
ggplot() + 
  geom_point(data = function_outputs, 
             aes(x = day, y = B),
             color = "red") +
  geom_point(data = observations, 
             aes(x = day, y = Bobs)) +
  theme_bw() +
  labs(x = "Time (day)",
       y = expression("Biomass (g "*m^{-2}*")"))


## EVALUATE GOODNESS-OF-FIT
maize_data<-left_join(observations,function_outputs)
# Create a plot showing simulated and observed maize biomass values
ggplot() + 
  geom_point(data = maize_data, 
             aes(x = B, y = Bobs),
             color = "purple") +
  geom_abline() +
  theme_bw() +
  labs(x = expression("Simulated Biomass (g "*m^{-2}*")"),
       y = expression("Observed Biomass (g "*m^{-2}*")"))
#Pause,think,discuss
# I thought that the model captured the overall signal of the observed data so I assumed that the model efficiency would be good. I was surprised and thought that the model would under predict the data. The model appears to over predict in the beginning and the under predict towards the end but the values fall close to the one-to-one line.

## 4.Apply the maize model to estimate average county-scale corn yields in Robeson County,NC.
daily_weather <- read_csv("data/weather_lumberton_2016_2018.csv")
corn_yeild <- read_csv("data/usda_nass_corn_yields.csv")
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
  # Vector: Number of simulation days
  #ndays <- nrow(weather)
  ndays <- length(seq(start,end,step))
  # Vector: Thermal time age of the crop on day t [deg.C/day]
  TT <- rep(NA, ndays)
  # Vector: Leaf area index (area of leaves per unit ground area) [m3/m3]
  LAI <- rep(NA, ndays)
  # Vector: Biomass of crop per square meter of ground area [g/m2]
  B <- rep(NA, ndays)
  # Assign initial values
  TT[1] <- TT0
  LAI[1] <- LAI0
  B[1] <- B0
  
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
  #outputs <- data.frame(day = weather$day, TT, LAI, B)
  #sim_yield <- B[134]
  sim_yield <- B[134]
  return(sim_yield)
}


#clean corn yield data
tidy_corn_yeild <- clean_names(corn_yeild)#change column names
robco_corn_yeild <- tidy_corn_yeild %>% filter(county == "ROBESON",data_item == "CORN, GRAIN - YIELD, MEASURED IN BU / ACRE", year > "2015") %>% select(year,county,data_item,value) # filter data to only include the columns and rows of interest
robco_corn_yeild$value <- as.numeric(robco_corn_yeild$value) # convert value to numeric

#clean weather data
tidy_daily_weather <- clean_names(daily_weather)#change column names
tidy_daily_weather <- tidy_daily_weather %>% rename(day = date_time_est,
                                                    Tmax = daily_max_of_12_23m_temperature_f,
                                                    Tmin = daily_min_of_12_23m_temperature_f,
                                                    I = daily_avg_of_15_24m_solar_radiation_hourly_average_wm2) %>% mutate_at(c('Tmax', 'Tmin', 'I'), as.numeric) # rename columns and change data types

tidy_daily_weather$day <- mdy(tidy_daily_weather$day) # change data type
tidy_daily_weather <- tidy_daily_weather %>% mutate(year = year(day)) # add a year column
tidy_daily_weather[2:3] <-lapply(tidy_daily_weather[2:3],function(x) {(x-32)*(5/9)}) # convert units
tidy_daily_weather[4] <-lapply(tidy_daily_weather[4],function(x) {(x*0.0036)*(24)}) # convert units

tidy_daily_weather %>% fill(Tmin, Tmax, I, .direction = "down") -> tidy_daily_weather # Replace NAs

#subset weather data by year
wdata_2016<-tidy_daily_weather %>% filter(year == "2016")%>% slice(125:258)
wdata_2017<-tidy_daily_weather %>% filter(year == "2017")%>% slice(125:258)
wdata_2018<-tidy_daily_weather %>% filter(year == "2018")%>% slice(125:258)

outputs_robeson <- data.frame(year = c(2016, 2017, 2018),
                              simulated = rep(NA, 3),
                              observed = robco_corn_yeild$value[3:1])


outputs_robeson$simulated[1]<-maize_model(wdata_2016, Tb = 12.8, TTM = 1400, TTL = 700, K = 1, RUE = 1.5, a = 0.001, LAIm = 7, TT0 = 0, LAI0 = 0.1, B0 = 0.1, start = 125, end = 258, step = 1)

outputs_robeson$simulated[2]<-maize_model(wdata_2017, Tb = 12.8, TTM = 1400, TTL = 700, K = 1, RUE = 1.5, a = 0.001, LAIm = 7, TT0 = 0, LAI0 = 0.1, B0 = 0.1, start = 125, end = 258, step = 1)

outputs_robeson$simulated[3]<-maize_model(wdata_2018, Tb = 12.8, TTM = 1400, TTL = 700, K = 1, RUE = 1.5, a = 0.001, LAIm = 7, TT0 = 0, LAI0 = 0.1, B0 = 0.1, start = 125, end = 258, step = 1)

outputs_robeson[2] <-lapply(outputs_robeson[2],function(x) {x*(10^-6)*39.37*4046.86}) # convert units


#PAUSE,THINK,DISCUSS
# Looking at the values there is some over predicting and under predicting between the observed and simulated values. The 3 points fall a good  distance away from the one-to-one line but I feel that the model performed well, given that the 3 points fall along the on-to-on line. 
# Create a plot showing simulated and observed maize biomass values
ggplot() + 
  
  geom_point(data = outputs_robeson, 
             aes(x = simulated, y = observed),
             color = "purple") +
  geom_abline() +
  theme(aspect.ratio = 1)+
  theme_bw() +
  xlim(c(min(unlist(outputs_robeson)),
         max(unlist(outputs_robeson)))) +
  ylim(c(min(unlist(outputs_robeson)),
         max(unlist(outputs_robeson)))) +
  labs(x = expression("Simulated Biomass (g "*m^{-2}*")"),
       y = expression("Observed Biomass (g "*m^{-2}*")"))

