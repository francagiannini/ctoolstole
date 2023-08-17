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
  
  depth_h = as.numeric(j["depth"])/2
  
  julianDay = as.numeric(j["month"]) * 30.4166
  
  t = (julianDay + soil_df$offset_soilInit)

  #soil thermal difussivity see https://doi.org/10.1016/j.biosystemseng.2017.06.011
  #dependent on soil moisture
  
  Th_diff <- 0.35E-6

  #angular frequency
  
  rho <- 3.1415926 * 2 / (365 * 24 * 3600)
    
  dampDepth <- sqrt(2 * Th_diff / rho)
    
  Td <-
    as.numeric(j["temp_m"]) + as.numeric(j["temp_amp"])*exp(- depth_h / dampDepth) * sin(rho * t * 24 * 3600- depth_h / dampDepth)
    #  
    
tempCoefficient <- 
  7.24 * exp(-3.432 + 0.168 * Td * (1 - 0.5 * Td / 36.9))
  
  data.frame("tempCoefficient" = tempCoefficient,
    "soil_temp"= Td) 
  }

tcoeff_df <- rbind(
  data.frame(
    temperatures,
    "temp_amp" = as.numeric(rep(temp_minmax$temp_amp, 55)),
    "month" = as.numeric(rep(seq(1, 12, 1), 55)),
    "depth" = 25,
    "amplitude"="variable monthly"
  ),
  data.frame(
    temperatures,
    "temp_amp" = as.numeric(rep(temp_minmax$temp_amp, 55)),
    "month" = as.numeric(rep(seq(1, 12, 1), 55)),
    "depth" = 100,
    "amplitude"="variable monthly"
  ),
  data.frame(
    temperatures,
    "temp_amp" = as.numeric(max(temperatures)-min(temperatures)),
    "month" = as.numeric(rep(seq(1, 12, 1), 55)),
    "depth" = 25,
    "amplitude"="complete period"
  ),
  data.frame(
    temperatures,
    "temp_amp" = as.numeric(max(temperatures)-min(temperatures)),
    "month" = as.numeric(rep(seq(1, 12, 1), 55)),
    "depth" = 100,
    "amplitude"="complete period"
  )
)

tcoeff_example <- cbind(tcoeff_df,
                        do.call("rbind", apply(tcoeff_df, 1, temperature)))


tcoeff_example |>
ggplot(aes(y=tempCoefficient, x=temp_amp, 
           col=as.character(depth),
           )) +
  geom_point(aes(shape=as.character(depth), alpha=0.4, size=1))+
  geom_smooth(aes(group=as.character(depth)), se=FALSE) + 
  geom_abline(slope = 1, intercept = c(0,0))+
  #facet_grid(~amplitude)+
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

clayfrac=seq(0.01,0.6,0.01)

data.frame("h" = do.call(rbind, lapply(clayfrac, humification)),
           "clayfrac" = clayfrac) |>
  ggplot(aes(y=h, x=clayfrac)) + geom_line() + theme_classic()


# inmovilization proportion as a function of C/N relation

inmovilization <- function(cn){
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



# 3. starting soil conditions and rates constants in time ----

extraCarbon <-  soil_df$`Amended C_soilInit` / 12
#CNfraction <- soil_df$`C/N_soilInit`

Cproptop <- 0.47

startCAmount_top <- soil_df$`Initial C(t/ha)_soilInit` * Cproptop
startCAmount_sub <- soil_df$`Initial C(t/ha)_soilInit` * 1-Cproptop


humcPlant_top <-
  startCAmount_top * soil_df$PupperLayer_soilInit * inmovilization(soil_df$`C/N_soilInit`)
romcPlant_top <- startCAmount_top - humcPlant_top
humcPlant_sub <-
  startCAmount_sub * soil_df$PLoweLayer_soilInit * inmovilization(soil_df$`C/N_soilInit`)
romcPlant_sub <- startCAmount_sub - humcPlant_sub
humificationPlant <- humification(soil_df$clayfraction_crop)

# 4. general turnover flow ----
# plantinputs

fom_add_plant_top <- cinp$Cinp_plant_top[i]
# mnure 
fom_add_predecomp_top <- cinp$Cinp_Cmanure[i] - fHUM*cinp$cinp$Cinp_Cmanure[i]

hum_add <- fHUM*cinp$Cinp_plant_top[i]

#kFOM should be affected by temp 

tempCofficent <- tempdependence()

kFOM <- soil_df$FOMdecompositionrate_crop*tempCofficent

fom_decomp <- (fomprev+fom_add_plant_top)*kFOM

fom_toLowerLayer<- fom_afte_fomdecomp*soil_df$tF_crop




# # Assuming Data.Values is a data frame in R containing the necessary values
# 
# # Initialize data frames to store the results
# co2_results <- data.frame(matrix(NA, nrow = nrow(cinp), ncol = 6))
# total_amount <- data.frame(matrix(NA, nrow = nrow(cinp), ncol = 14))
# transport_results <- data.frame(matrix(NA, nrow = nrow(cinp), ncol = 3))
# 
# # Loop through each data row in Data.Values
# for (i in 1:nrow(cinp)) {
#   dataYearValues <- cinp[i, ]
#   julianDay <- 0
#   
#   # Loop through each month (1 to 12)
#     
#   # Implement the region-specific calculations for different months (if conditions)
#     # ...  
#   for (month in 1:12) {
#     julianDay <- month * 30.4166
# 
#     
#   # Calculate the values using the appropriate functions for DecompositionFom, DecompositionHum, and DecompositionRom
#    #co2FomPlant = DecompositionFom(ref fomcPlant, Variables.FOMdecompositionratePlant, Variables.TFPlant, humificationPlant, ref humcPlant, WITH_TRANSPORT, false, 0, ref transportFomPlant);
#     
#     # Store the results in the corresponding data frames
#     co2_results[i, ] <- c(co2FomPlant[1], co2FomPlant[2], co2HumPlant[1], co2HumPlant[2], co2RomPlant[1], co2RomPlant[2])
#     total_amount[i, ] <- c(fomcPlant[1], humcPlant[1], romcPlant[1], fomcManure[1], humcManure[1], romcManure[1], fomcPlantC14[1], humcPlantC14[1], romcPlantC14[1], fomcManureC14[1], humcManureC14[1], romcManureC14[1], (fomcPlantC14[1] + humcPlantC14[1] + romcPlantC14[1] + fomcManureC14[1] + humcManureC14[1] + romcManureC14[1]) / (fomcPlant[1] + humcPlant[1] + romcPlant[1] + fomcManure[1] + humcManure[1] + romcManure[1]) * 100, (fomcPlant[1] + humcPlant[1] + romcPlant[1] + fomcManure[1] + humcManure[1] + romcManure[1]))
#     transport_results[i, ] <- c(transportFomPlant + transportFomManure, transportHumPlant + transportHumManure, transportRomPlant + transportRomManure)
#     
#     # Increment temperatureValuePosition (if necessary)
#     # ...
#   }
# }
# 
# # If Mode.Value == 2, co2_results, total_amount, and transport_results will contain the final results for each month
# # If Mode.Value == 3, use co2_results, total_amount, and transport_results to create corresponding data frames or tables as needed
# 
# 
# 
# # transport vertical ----
# 
# inptop <- cinp[1,]
# 
# # topsoil




