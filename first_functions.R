library(tidyverse)

# Inputs ----

## Temperature ----
temperatures <- read.table("InputFiles/temp55years.txt")
colnames(temperatures) <- c("meanmonthlytemp")
head(temperatures)

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

# functions ----




# modifiying decomposition as afunction of monthly temperatures


tempdependence <- function(temp_m){
  tempCoefficient <- 
    7.24 * exp(-3.432 + 0.168 * temp_m * (1 - 0.5 * temp_m / 36.9))
  tempCoefficient
}

temp_m=temperatures$meanmonthlytemp

data.frame("tempCoefficient" = do.call(rbind, lapply(temp_m, tempdependence)),
           "temp_m" = temperatures$meanmonthlytemp,
           "month"=rep(seq(1,12,1),length(temp_m)/12)) |>
ggplot(aes(y=tempCoefficient, x=temp_m)) + geom_line() + theme_classic()
ggplot(aes(y=tempCoefficient, x=month)) + geom_smooth() + geom_point()+ theme_classic()

## turnover ----

# C decay in each pool  

decay <- function(amount, k, tempCoefficient ){
  amount = amount_t * (-k * tempCoefficient)
  amount
}



# humification coeficcient as function of clay content
# k_hum
humification <- function(clayfrac){
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

# Runge-Kutta function ----

Rk4decay <- function(t0, u0, dt, k, tempCoefficient) {
  
  tempCoefficient=tempdependence(temp_m)
  
  f1 <- func(t0, u0, k, tempCoefficient)
  f2 <- func(t0 + dt / 2, u0 + dt * f1 / 2, k, tempCoefficient)
  f3 <- func(t0 + dt / 2, u0 + dt * f2 / 2, k, tempCoefficient)
  f4 <- func(t0 + dt, u0 + dt * f3, k, tempCoefficient)
  
  u1 <- u0 + dt * (f1 + 2 * f2 + 2 * f3 + f4) / 6
  return(u1)
}

func <- function(time, amount, k, tempCoefficient) {
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



# starting soil conditions and rates constants in time ----

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

# Assuming Data.Values is a data frame in R containing the necessary values

# Initialize data frames to store the results
co2_results <- data.frame(matrix(NA, nrow = nrow(cinp), ncol = 6))
total_amount <- data.frame(matrix(NA, nrow = nrow(cinp), ncol = 14))
transport_results <- data.frame(matrix(NA, nrow = nrow(cinp), ncol = 3))

# Loop through each data row in Data.Values
for (i in 1:nrow(cinp)) {
  dataYearValues <- cinp[i, ]
  julianDay <- 0
  
  # Loop through each month (1 to 12)
    
  # Implement the region-specific calculations for different months (if conditions)
    # ...  
  for (month in 1:12) {
    julianDay <- month * 30.4166

    
  # Calculate the values using the appropriate functions for DecompositionFom, DecompositionHum, and DecompositionRom
   #co2FomPlant = DecompositionFom(ref fomcPlant, Variables.FOMdecompositionratePlant, Variables.TFPlant, humificationPlant, ref humcPlant, WITH_TRANSPORT, false, 0, ref transportFomPlant);
    
    # Store the results in the corresponding data frames
    co2_results[i, ] <- c(co2FomPlant[1], co2FomPlant[2], co2HumPlant[1], co2HumPlant[2], co2RomPlant[1], co2RomPlant[2])
    total_amount[i, ] <- c(fomcPlant[1], humcPlant[1], romcPlant[1], fomcManure[1], humcManure[1], romcManure[1], fomcPlantC14[1], humcPlantC14[1], romcPlantC14[1], fomcManureC14[1], humcManureC14[1], romcManureC14[1], (fomcPlantC14[1] + humcPlantC14[1] + romcPlantC14[1] + fomcManureC14[1] + humcManureC14[1] + romcManureC14[1]) / (fomcPlant[1] + humcPlant[1] + romcPlant[1] + fomcManure[1] + humcManure[1] + romcManure[1]) * 100, (fomcPlant[1] + humcPlant[1] + romcPlant[1] + fomcManure[1] + humcManure[1] + romcManure[1]))
    transport_results[i, ] <- c(transportFomPlant + transportFomManure, transportHumPlant + transportHumManure, transportRomPlant + transportRomManure)
    
    # Increment temperatureValuePosition (if necessary)
    # ...
  }
}

# If Mode.Value == 2, co2_results, total_amount, and transport_results will contain the final results for each month
# If Mode.Value == 3, use co2_results, total_amount, and transport_results to create corresponding data frames or tables as needed



# transport vertical ----

inptop <- cinp[1,]

# topsoil

fom_top <- inptop$Cinp_plant_top

