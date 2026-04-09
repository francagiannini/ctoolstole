<<<<<<< HEAD
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

pool_top <-ifelse(y=="1" & m=="1", init_pool_top, pool_top)

pool_sub <-ifelse(y=="1" & m=="1", init_pool_sub, pool_sub)


Cinp_plant_top <- cinp$Cinp_plant_top[y]*month_prop[m]

Cinp_plant_sub <- cinp$Cinp_plant_sub[y]*month_prop[m]

# manure 
fHUM_man_rate=soil_df$HumFraction_manure #0.192  feaces # 0.63 #digested feaces #0.39 #digested feed

hum_man_inp <- fHUM_man_rate*cinp$Cinp_Cmanure[y]*month_man[m]

fom_man_inp <- cinp$Cinp_Cmanure[y]*month_man[m] - hum_man_inp

#
FOM_0 = c(pool_top["FOM"]+fom_man_inp+Cinp_plant_top,
          pool_sub["FOM"]+fom_man_inp+Cinp_plant_sub)

t_coef <- temp_coef(
      T_zt = soil_temp(
        depth = c(0.25 / 2, 0.25+(1-0.25)/2),
        month = m,
        T_ave = temperatures[y - 1 + m,],
        A_0 = 25.09995,
        th_diff = 0.035
      ))
  
FOM_after_decomp = FOM_0+decay(
    amount_t = FOM_0,
    k = soil_df$FOMdecompositionrate_crop,
    tempCoefficient = t_coef)

tr_FOM= c(FOM_after_decomp[1]*soil_df$tF_crop,0)

C02_FOM=(FOM_after_decomp-tr_FOM)*hum_coef(clayfrac = c(soil_df$clayfraction_crop, soil_df$clayfraction_crop))

FOM_next=FOM_after_decomp-tr_FOM-C02_FOM

FOMtoHUM=FOM_after_decomp*hum_coef(clayfrac = c(soil_df$clayfraction_crop, soil_df$clayfraction_crop))

HUM_0=pool_top["HUM"]+c(hum_man_inp+FOMtoHUM

HUM_after_decomp = HUM_0+decay(
  amount_t = HUM_0,
  k = soil_df$HUMdecompositionrate_crop,
  tempCoefficient = t_coef)

tr_HUM= c(HUM_after_decomp[1]*soil_df$tF_crop,0)

C02_HUM=(HUM_after_decomp-tr_HUM)*0.628


#   
#   FOM_plant=FOM_now





tr_HUM= (FOM_0-FOMtoHUM)*soil_df$tF_crop
  

FOM_plant = FOM_0 -  +
  fom_man_inp-co2_FOM-tr_FOM

# Decomposition functions 

# bce_FOMpool <- function(Cinit,
#                         decomp_rate,
#                         tr_rate,
#                         resp_frac,
#                         y,
#                         m) {
#   
#   Cinp_plant <- c(
#     cinp$Cinp_plant_top[y] * month_prop[m],
#     cinp$Cinp_plant_sub[y] * month_prop[m]
#   )
#   
#   t_coef <- temp_coef(
#     T_zt = soil_temp(
#       depth = c(0.25 / 2, 0.25+(1-0.25)/2),
#       month = m,
#       T_ave = temperatures[y - 1 + m,],
#       A_0 = 25.09995,
#       th_diff = 0.035
#     ))
#   
#   FOM_after_decomp <- Cinp_plant - decay(
#     amount_t = Cinp_plant,
#     k = soil_df$FOMdecompositionrate_crop,
#     tempCoefficient = t_coef
#   )
#   
#   FOM_now=FOM_before+FOM_after_decomp
#   
#   tr_FOM= FOM_now[1]*soil_df$tF_crop
#   
#   FOM_plant=FOM_now
# }
# FOM_plant = c(init_pool_top["FOM"]+fom_man_inp+Cinp_plant_top,
#               init_pool_sub["FOM"]+Cinp_plant_sub)
# 
# HUM_Plant = c(init_pool_top["HUM"]+hum_man_inp,
#               init_pool_sub["HUM"])
# 
# # ROM_Plant = c(init_pool_top["ROM"],
# #               init_pool_sub["ROM"])
# 
# 
# tr_HUM=
# tr_ROM=
# 
# co2_FOM=FOM_0*hum_coef(clayfrac = soil_df$clayfraction_crop)
# co2_HUM=
# co2_ROM=



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


=======
# Turnover core function -----
>>>>>>> 882f7baec404068625a49dcfdb2300026777f0e7

# i refers to the time step 

turnover <- function(i) {
  
  result_pools <- result_pools[i - 1, ]
  result_pools <- as.data.frame(t(result_pools))
  
  #browser()
  
  m = ifelse(as.numeric(result_pools[, "mth"]) < 12, 
             as.numeric(result_pools[, "mth"]) + 1,
             1)
  y = ifelse(as.numeric(result_pools[, "mth"]) < 12,
             as.numeric(result_pools[, "yr"]),
             as.numeric(result_pools[, "yr"]) + 1)
  
  # FOM topsoil ----
  
  FOM_top <-
    result_pools[, "FOM_top"] +
    C_input_top[y] * month_prop[m] +
    C_input_man[y] * (1 - fman) * month_man[m]
  
  FOM_after_decomp_top <- FOM_top +
    decay(
      C0_t = FOM_top,
      k = kFOM,
      tempCoefficient = temp_coef(
        T_zt =
          soil_temp(
            depth = 25,
            month = m,
            T_ave = T_ave[y - 1 + m],
            A_0 = T_range[y - 1 + m],
            th_diff = phi
          )
      )
    )
  
  substrate_FOM_decomp_top <- FOM_top - FOM_after_decomp_top
  
  FOM_humified_top <-
    substrate_FOM_decomp_top * hum_coef(clayfrac = clay_top)
  
  CO2_FOM_top <-
    substrate_FOM_decomp_top * (1 - hum_coef(clayfrac = clay_top))
  
  FOM_tr <- FOM_after_decomp_top * ftr
  
  FOM_top <- FOM_top - FOM_humified_top - CO2_FOM_top - FOM_tr
  
  # FOM subsoil ----
  
  FOM_sub <-
    result_pools[, "FOM_sub"] +
    FOM_tr +
    C_input_sub[y] * month_prop[m]
  
  FOM_after_decomp_sub <-
    FOM_sub +
    decay(
      C0_t = FOM_sub,
      k = kFOM,
      tempCoefficient =  temp_coef(
        T_zt = soil_temp(
          depth = 100,
          month = m,
          T_ave = T_ave[y - 1 + m],
          A_0 = T_range[y - 1 + m],
          th_diff = phi
        )
      )
    )
  
  substrate_FOM_decomp_sub <- FOM_sub - FOM_after_decomp_sub
  
  FOM_humified_sub <-
    substrate_FOM_decomp_sub * hum_coef(clayfrac = clay_sub)
  
  CO2_FOM_sub <-
    substrate_FOM_decomp_sub * (1 - hum_coef(clayfrac = clay_sub))
  
  FOM_sub <- FOM_sub - FOM_humified_sub - CO2_FOM_sub
  
  # HUM topsoil ----
  
  HUM_top <-
    result_pools[, "HUM_top"] +
    C_input_man[y] * fman * month_man[m] +
    FOM_humified_top
  
  HUM_after_decomp_top <-
    HUM_top +
    decay(
      C0_t = HUM_top,
      k = kHUM,
      tempCoefficient = temp_coef(
        T_zt = soil_temp(
          depth = 25,
          month = m,
          T_ave = T_ave[y - 1 + m],
          A_0 = T_range[y - 1 + m],
          th_diff = phi
        )
      )
    )
  
  substrate_HUM_decomp_top <- HUM_top - HUM_after_decomp_top
  
  HUM_romified_top <- substrate_HUM_decomp_top * (fromi+fco2)
  
  CO2_HUM_top <- substrate_HUM_decomp_top * (1 - fromi-fco2) #
  
  HUM_tr <- HUM_after_decomp_top * ftr
  
  HUM_top <- HUM_top - HUM_romified_top - CO2_HUM_top - HUM_tr
  
  # HUM subsoil ----
  
  HUM_sub <-
    result_pools[, "HUM_sub"] +
    HUM_tr +
    FOM_humified_sub
  
  HUM_after_decomp_sub <-
    HUM_sub +
    decay(
      C0_t = HUM_sub,
      k = kHUM,
      tempCoefficient = temp_coef(
        T_zt = soil_temp(
          depth = 100,
          month = m,
          T_ave = T_ave[y - 1 + m],
          A_0 = T_range[y - 1 + m],
          th_diff = phi
        )
      )
    )
  
  substrate_HUM_decomp_sub <- HUM_sub - HUM_after_decomp_sub
  
  HUM_romified_sub <- substrate_HUM_decomp_sub * (fromi+fco2)
  
  CO2_HUM_sub <-
    substrate_HUM_decomp_top * (1 - (fromi+fco2)) #
  
  HUM_sub <- HUM_sub - HUM_romified_sub - CO2_HUM_sub
  
  # ROM topsoil ----
  
  ROM_top <-
    result_pools[, "ROM_top"] +
    HUM_romified_top
  
  ROM_after_decomp_top <-
    ROM_top +
    decay(
      C0_t = ROM_top,
      k = kROM,
      tempCoefficient =  temp_coef(
        T_zt = soil_temp(
          depth = 25,
          month = m,
          T_ave = T_ave[y - 1 + m],
          A_0 = T_range[y - 1 + m],
          th_diff = phi
        )
      )
    )
  
  substrate_ROM_decomp_top <- ROM_top - ROM_after_decomp_top
  
  CO2_ROM_top <-
    substrate_ROM_decomp_top * fco2
  
  # ROM_not_respirated <-
  #   substrate_ROM_decomp_top*(1-fco2)
  
  ROM_tr <-
    ROM_after_decomp_top * ftr
  
  ROM_top <- ROM_top - ROM_tr - CO2_ROM_top
  
  # ROM subsoil ----
  
  ROM_sub <-
    result_pools[, "ROM_sub"] +
    ROM_tr +
    HUM_romified_sub
  
  ROM_after_decomp_sub <-
    ROM_sub +
    decay(
      C0_t = ROM_sub,
      k = kHUM,
      tempCoefficient =  temp_coef(
        T_zt = soil_temp(
          depth = 100,
          month = m,
          T_ave = T_ave[y - 1 + m],
          A_0 = T_range[y - 1 + m],
          th_diff = phi
        )
      )
    )
  
  substrate_ROM_decomp_sub <- ROM_sub - ROM_after_decomp_sub
  
  CO2_ROM_sub <-
    substrate_ROM_decomp_top * fco2
  
  ROM_sub <- ROM_sub - CO2_ROM_sub
  
  result_pools <-
    cbind(
      
      "step" = result_pools[, "step"] + 1,
      "yr" = y,
      "mth" = m,
      
      "FOM_top" = FOM_top,
      "HUM_top" = HUM_top,
      "ROM_top" = ROM_top,
      
      "FOM_sub" = FOM_sub,
      "HUM_sub" = HUM_sub,
      "ROM_sub" = ROM_sub,
      
      "C_topsoil" = FOM_top + HUM_top + ROM_top,
      "C_subsoil" = FOM_sub + HUM_sub + ROM_sub,
      
      "FOM_tr" = FOM_tr,
      "HUM_tr" = HUM_tr,
      "ROM_tr" = ROM_tr,
      
      "C_tr" = FOM_tr + HUM_tr + ROM_tr,
      
      "CO2_FOM_top" = CO2_FOM_top,
      "CO2_HUM_top" = CO2_HUM_top,
      "CO2_ROM_top" = CO2_ROM_top,
      
      "CO2_FOM_sub" = CO2_FOM_sub,
      "CO2_HUM_sub" = CO2_HUM_sub,
      "CO2_ROM_sub" = CO2_ROM_sub,
      
      "C_CO2_top" = CO2_FOM_top + CO2_HUM_top + CO2_ROM_top,
      "C_CO2_sub" = CO2_FOM_sub + CO2_HUM_sub + CO2_ROM_sub
    )
  
  return(result_pools)
}
