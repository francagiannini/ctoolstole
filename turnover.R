

# Starting soil conditions and rates constants in time ----

extraCarbon <-  soil_df$`Amended C_soilInit` / 12
#CNfraction <- soil_df$`C/N_soilInit`

Cproptop <- 0.47

startCAmount_top <- soil_df$`Initial C(t/ha)_soilInit` * Cproptop
startCAmount_sub <- soil_df$`Initial C(t/ha)_soilInit` * (1-Cproptop)


init_pool_top <-pool_cn(cn=soil_df$`C/N_soilInit`,
                         HUM_frac = soil_df$PupperLayer_soilInit,
                         C_0=startCAmount_top)

init_pool_sub <-pool_cn(cn=soil_df$`C/N_soilInit`,
                         HUM_frac = soil_df$PLoweLayer_soilInit,
                         C_0=startCAmount_sub)

# General turnover flow ----
# plantinputs

Cinp_plant_top <- cinp$Cinp_plant_top[i]
# mnure 
fom_add_predecomp_top <- cinp$Cinp_Cmanure[i] - fHUM*cinp$Cinp_Cmanure[i]

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




