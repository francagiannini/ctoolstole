library(tidyverse)

# I. Inputs ----

## Temperature ----
temperatures <- read.table("InputFiles/temp55years.txt")
colnames(temperatures) <- c("temp_m")
head(temperatures)

temp_minmax <- read.table("InputFiles/temp_ranges_foulum.txt", header = T) |> 
  mutate(temp_amp =max-min) 

## Soil ----
soil <- as.data.frame(readLines("InputFiles/input.txt"))
colnames(soil) <- c("vect")

paramtype <- c(rep("soilInit",9),
               rep("crop",10),
               rep("manure",11),
               rep("C14_crop",11),
               rep("C14_manure",12),
               rep("fom", 6))

soil_df <- soil |> 
  separate(vect, sep = "\t", into = c("soilparam","value")) |> 
  dplyr::mutate(soilparam = paste(soilparam, paramtype, sep="_")) |> 
  pivot_wider(names_from = "soilparam", 
              values_from = "value") |>   
  dplyr::select(!starts_with("[") & where(~!any(is.na(.)))) |> 
  dplyr::select(where(~!any(is.na(.)))) |> 
  mutate_if(is.character, as.numeric)


## C inputs ----

cinp <- read.table("InputFiles/data.txt", sep = "\t", header= TRUE)

colnames(cinp) <-c("year",
                   "Cinp_plant_top","Cinp_plant_sub",
                   "Cinp_Cmanure", "C14plant","C14manure")

# 2. functions ----

# temperature dependence
# modifying decomposition as function of monthly temperatures 

#https://open.library.okstate.edu/rainorshine/chapter/13-4-sub-surface-soil-temperatures/


temperature <- #function(depth, temp_m, temp_amp, month) {
  function(j) {
    #browser()
    # depth in meters#
    z = as.numeric(j["depth"]) / 2 * 0.01 
    
    #temporal position in daily bases setted as the last day of each month
    julianDay = as.numeric(j["month"]) * 30.4166
    
    t = (julianDay + soil_df$offset_soilInit)
    
    #soil thermal difussivity 
    #see https://doi.org/10.1016/j.biosystemseng.2017.06.011
    #dependent on soil moisture
    # this is m2/day 0.35e-6 if m2/s per second*3600*24
    
    Th_diff <-
      as.numeric(j["th_diff"]) 
    
    #angular frequency
    # here the cycle is daily for secondly cycles (365 * 24 * 3600) 
    rho <-
      3.1415926 * 2 /365
    
    # Damping depth here in m
    D <- sqrt(2 * Th_diff / rho)
    
    # here monthly daily average temperature 
    T_ave <- as.numeric(j["temp_m"])
    
    # here monthly daily range of temperature
    A_0 <- as.numeric(j["temp_amp"])
    
    # Soil temperature at t days and z depth in m Montein and Unsworth 
    T_zt <-
      T_ave + A_0 * exp(-z / D) * sin(rho * t - z / D)
    
    # temp coefficient uniti at 10 C¤
    tempCoefficient <-
      7.24 * exp(-3.432 + 0.168 * T_zt * (1 - 0.5 * T_zt / 36.9))
    
    
    data.frame("tempCoefficient" = tempCoefficient,
               "soil_temp" = T_zt
)
  }


temperature_bis <- #function(depth, temp_m, temp_amp, month) {
  function(j) {
    #browser()
    
    # depth in meters#
    D = as.numeric(j["depth"]) / 2 * 0.01 
    
    #temporal position in secondly bases setted as the last day of each month
    julianDay = as.numeric(j["month"]) * 30.4166
    
    t = (julianDay + soil_df$offset_soilInit)*24*3600
    
    #soil thermal conductivity different from diffusivity

    # this is m2/s per second*3600*24 by defect 8e-7
   
    a <- 8e-7
    #as.numeric(j["th_cond"]) 
    
    #osilation time in seconds 
    # here is secondly cycles (365 * 24 * 3600) 
    t_0 <-3.15e-7
    
    # here yearly average temperature 
    T_mean <- as.numeric(j["temp_m"])
    
    # here monthly daily range of temperature
    T_xf <- t-D*sqrt(t_0/4*a*3.1415926)
    
    # Soil temperature at t days and z depth in m Montein and Unsworth 
    
    #
    T_zt_bis <-
      T_mean + (T_xf-T_mean) * exp(-D* sqrt(3.1415926*a*t_0))
    
    # temp coefficient uniti at 10 C¤
    
    tempCoefficient_bis <-
      7.24 * exp(-3.432 + 0.168 * T_zt_bis * (1 - 0.5 * T_zt_bis / 36.9))
    
    
    data.frame("tempCoefficient_bis" = tempCoefficient_bis,
               "soil_temp_bis"=T_zt_bis)
  }

# tcoeff_df <- rbind(
#   data.frame(
#     temperatures,
#     "temp_amp" = as.numeric(rep(temp_minmax$temp_amp, 55)),
#     "month" = as.numeric(rep(seq(1, 12, 1), 55)),
#     "depth" = 25,
#     "amplitude"="variable monthly"
#   ),
#   data.frame(
#     temperatures,
#     "temp_amp" = as.numeric(rep(temp_minmax$temp_amp, 55)),
#     "month" = as.numeric(rep(seq(1, 12, 1), 55)),
#     "depth" = 100,
#     "amplitude"="variable monthly"
#   ),
#   data.frame(
#     temperatures,
#     "temp_amp" = as.numeric(max(temperatures)-min(temperatures)),
#     "month" = as.numeric(rep(seq(1, 12, 1), 55)),
#     "depth" = 25,
#     "amplitude"="complete period"
#   ),
#   data.frame(
#     temperatures,
#     "temp_amp" = as.numeric(max(temperatures)-min(temperatures)),
#     "month" = as.numeric(rep(seq(1, 12, 1), 55)),
#     "depth" = 100,
#     "amplitude"="complete period"
#   )
# ) 

#thermal diffusivity m2/d

th_diff <- data.frame(
  "soil" = c(paste("JB", seq(1, 9, 1)), "defect"),
  "th_diff" = c(
    0.073,0.061452, 0.073,0.056484, 0.054,0.051516,
    0.049032, 0.046548,0.044064,0.035
  ))
             
             
df_temp <- expand_grid(
  
  data.frame(
    "temp_m"=rep(temperatures$temp_m,2),#[1:120, ],[1:12,]
    "month" = as.numeric(rep(seq(1, 12, 1), 55)),
    "temp_amp" = c(as.numeric(rep((temp_minmax$temp_amp),55)),
                 rep(as.numeric(max(temperatures)-min(temperatures)),12*55)
                 ),
  "amplitude" = c(rep("monthly",55*12), rep("complete period",55*12))),
  th_diff,
  "depth" = c(50, 30,100, 200))

tcoeff_example <- cbind(df_temp,
                        do.call("rbind", apply(df_temp, 1, temperature)))

tcoeff_example2 <- cbind(df_temp,
                        do.call("rbind", apply(df_temp, 1, temperature_bis)))

tcoeff_example |>
#filter(soil=="defect") |> 
ggplot(aes(y=soil_temp, #soil_temp,
           x=month, 
           col=(depth),
           )) +
  geom_point(aes(col=depth, alpha=0.4, size=1))+
  geom_smooth(aes(group=(depth)), se=FALSE) + 
  #geom_abline(slope = 1, intercept = c(0,0))+
  facet_grid(soil~amplitude)+
  facet_grid(.~amplitude)+
  theme_classic()

cor(tcoeff_example$soil_temp, tcoeff_example$temp_m)

#hist(tcoeff_example$tempCoefficient)

# turnover
# decay rate modified by temperature
# C decay in each pool  

decay <- function(amount, k, tempCoefficient ){
  amount = amount_t * (-k * tempCoefficient)
  amount
}

# humification coeficcient as function of clay content
# k_hum

humification <- function(clayfrac) {
  R= 1.67 * (1.85 + 1.6 * exp(-7.86 * clayfrac))
  h= 1/(R+1)
  
  h
}

clayfrac=seq(0.01,1,0.01)

data.frame("h" = do.call(rbind, lapply(clayfrac, humification)),
           "clayfrac" = clayfrac) |>
  ggplot(aes(y=h, x=clayfrac)) + geom_line() + theme_classic()
 

data.frame("h" = do.call(rbind, lapply(clayfrac, humification)),
           "clayfrac" = clayfrac) |>
  ggplot(aes(y=1.358-1-h, x=clayfrac)) + 
    geom_line() + 
    ylab("fHUM(manuere)")+
    theme_classic()



# inmovilization proportion as a function of C/N relation

CN <- function(cn){
  CNfraction=min(56.2*cn^(-1.69),1)
  
  CNfraction
}

cn=seq(8,30,0.5)

data.frame("cnfrac" = do.call(rbind, lapply(cn, inmovilization)),
           "cn" = cn) |>
  ggplot(aes(y=cnfrac, x=cn)) + geom_line() + theme_classic()

# Runge-Kutta function

Rk4decay <- function(t0, u0, dt, k, tempCoefficient) {
  
  tempCoefficient=tempdependence(temp_m)
  
  f1 <- funcRk4step(t0, u0, k, tempCoefficient)
  f2 <- funcRk4step(t0 + dt / 2, u0 + dt * f1 / 2, k, tempCoefficient)
  f3 <- funcRk4step(t0 + dt / 2, u0 + dt * f2 / 2, k, tempCoefficient)
  f4 <- funcRk4step(t0 + dt, u0 + dt * f3, k, tempCoefficient)
  
  u1 <- u0 + dt * (f1 + 2 * f2 + 2 * f3 + f4) / 6
  return(u1)
}

funcRk4step <- function(time, amount, k, tempCoefficient) {
  value <- amount * -k * tempCoefficient
  return(value)
}

# decomposition of FOM

DecompositionFom <-
  function(fomc,
           FOMdecompositionrate,
           tF,
           humification,
           humc,
           WITH_TRANSPORT,
           C14,
           DecayRate,
           transportFile) {
    
    fomcLeft <- rep(0, length(fomc))
    output <- rep(0, 4)
    CO2output <- rep(0, length(fomc))
    
    for (j in 1:length(fomc)) {
      depthInLayer <- DepthInLayer(j)
      
      substract <- 0
      if (C14 == TRUE)
        substract <- DecayRate * fomc[j]
      tempCofficent <-
        TemperatureCoefficent(Temperature(depthInLayer))
      
      FomAfterDecom <-
        Rk4decay(julianDay * t, fomc[j], t, FOMdecompositionrate, tempCofficent) - substract
      
      fomcLeft[j] <- fomcLeft[j] + FomAfterDecom
      fomc[j] <- fomc[j] - FomAfterDecom
      
      toLowerLayer <- 0
      inCorrentLayer <- 0
      
      if (WITH_TRANSPORT == TRUE) {
        toLowerLayer <- fomc[j] * tF
        inCorrentLayer <- fomc[j] * (1 - tF)
      }
      
      if (j != length(fomc))
        fomcLeft[j + 1] <- fomcLeft[j + 1] + toLowerLayer
      else
        fomcLeft[j] <- fomcLeft[j] + toLowerLayer
      
      fomc[j] <- inCorrentLayer
      
      CO2 <- fomc[j] * (1 - humification)
      humificationAmount <- fomc[j] * humification
      fomc[j] <- fomc[j] - CO2 - humificationAmount
      humc[j] <- humc[j] + humificationAmount
      CO2output[j] <- CO2
      
      if (j == 1)
        transportFile <- toLowerLayer
    }
    
    fomc <- fomcLeft
    
    return(CO2output)
  }



