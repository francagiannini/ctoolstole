library(rCTOOL)

# Starting soil conditions and rates constants in time ----

#extraCarbon <-  soil_df$`Amended C_soilInit` / 12

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

y=1 #year, 
m=4 #month 

month_prop=c(0,0,0,8,12,16,64,0,0,0,0,0)/100
#month_prop=c(1,1,2,7,12,15,17,16,14,9,5,1)/100 # perennial from Henri
#month_prop=c(5,10,10,0,0,0,0,10,20,20,15,10) # cc other 

month_man=c(0,0,100,0,0,0,0,0,0,0,0,0)/100
#month_man=c(0,10,25,50,10,5,0,0,0,0,0,0)/100 #bare soil
#month_man=c(0,0,25,0,0,50,0,0,0,25,0,0)/100 #cc #perennial

Cinp_plant_top <- cinp$Cinp_plant_top[y]*month_prop[m]

Cinp_plant_sub <- cinp$Cinp_plant_sub[y]*month_prop[m]

# manure 
fHUM_man_rate=soil_df$HumFraction_manure #0.192  feaces # 0.63 #digested feaces #0.39 #digested feed

hum_man_inp <- fHUM_man*cinp$Cinp_Cmanure[y]*month_man[m]

fom_man_inp <- cinp$Cinp_Cmanure[y]*month_man[m] - hum_man_inp

t_coef <- temp_coef(T_zt = soil_temp(depth= 0.25/2,
                                        month=j,
                                        T_ave= temperatures[y-1+m,],
                                        A_0= 25.09995,
                                        th_diff=0.035))


#
FOM_0=0

FOM=0+#ifelse(y=1 %&% m=1,init_pool_top["FOM"], FOM_0)+
  Cinp_plant_top-decay(amount_t=Cinp_plant_top,
                               k=soil_df$FOMdecompositionrate_crop,
                               tempCoefficient = t_coef
                               )+
  fom_man_inp

FOM=init_pool_top["HUM"]+Cinp_plant_top+hum_man_inp


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




