library(rCTOOL)

library(tidyverse)

# Dummy scenario 5 years----

# Temp historical Fou

temperatures <- read.table("InputFiles/temp55years.txt")
colnames(temperatures) <- c("temp_m")
head(temperatures)

temp_minmax <- read.table("InputFiles/temp_ranges_foulum.txt", header = T) |> 
  mutate(temp_amp =max-min)

temperatures <- temperatures |> mutate(month=rep(seq(1,12,1),55)) |> group_by(month) |> summarize(monthly_mean=mean(temp_m))

T_ave <- rep(temperatures$monthly_mean,5)

T_range <- rep(temp_minmax$temp_amp, 5)

C_input_top <- rep(1,5)

C_input_sub <- rep(0.1,5)

C_input_man <- rep(0.2,5)

fman <- 0.192

phi <- 0.035

fco2 <- 0.628

month_prop <- c(0,0,0,8,12,16,64,0,0,0,0,0)/100

month_man <- c(0,0,100,0,0,0,0,0,0,0,0,0)/100

Cproptop <- 0.47

clay_top <- 0.1

clay_sub <- 0.15

Cinit <- 100

kFOM <- 0.12

kHUM <- 0.028

kROM <- 3.858e-05

ft <- 0.003

fHUM_top <- 0.4803

fROM_top <- 0.4881 

fHUM_sub <- 0.3123

fROM_sub <- 0.6847 

CN <- 10

# time 0

startCAmount_top <- Cinit * Cproptop
startCAmount_sub <- Cinit * (1-Cproptop)

init_pool_top <-pool_cn(cn=CN,
                        HUM_frac = fHUM_top,
                        ROM_frac = fROM_top,
                        C_0=startCAmount_top)

init_pool_sub <-pool_cn(cn=CN,
                        HUM_frac = fHUM_sub,
                        ROM_frac = fROM_sub,
                        C_0=startCAmount_sub)

# time 1
y=1 
m=1

# FOM topsoil ----

FOM_top <-
  ifelse(y == 1 & m == 1, init_pool_top["FOM"], FOM_after_t[m-1]) +
  C_input_top[y] * month_prop[m] +
  C_input_man[y] * (1 - fman) * month_man[m]

FOM_after_decomp_top <- FOM_top +
  decay(
    amount_t = FOM_top,
    k = kFOM,
    tempCoefficient = soil_temp(
      depth = 25,
      month = m,
      T_ave = T_ave[y - 1 + m],
      A_0 = T_range[y - 1 + m],
      th_diff = phi
    )
  )

FOM_after_hum_top <-
  FOM_after_decomp_top * (1 - hum_coef(clayfrac = clay_top))

CO2_FOM_top <- FOM_top - FOM_after_hum_top

FOM_after_t <- FOM_after_hum_top * (1 - ft)

# FOM subsoil ----

FOM_sub <-
  ifelse(y == 1 & m == 1, init_pool_sub["FOM"], FOM_after_hum_sub[m-1]) +
  FOM_after_hum_top * ft +
  C_input_sub[y] * month_prop[m]

FOM_after_decomp_sub <-
  FOM_sub +
  decay(
    amount_t = FOM_sub,
    k = kFOM,
    tempCoefficient = soil_temp(
      depth = 100,
      month = m,
      T_ave = T_ave[y - 1 + m],
      A_0 = T_range[y - 1 + m],
      th_diff = phi
    )
  )

FOM_after_hum_sub <-
  FOM_after_decomp_sub * (1 - hum_coef(clayfrac = clay_sub))

CO2_FOM_sub <- FOM_sub - FOM_after_hum_sub

# HUM topsoil ----

HUM_top <-
  ifelse(y == 1 & m == 1, init_pool_top["HUM"], HUM_after_t[m-1]) +
  C_input_man[y] * fman * month_man[m]+
  FOM_after_decomp_top*hum_coef(clayfrac = clay_top)

HUM_after_decomp_top <-
  HUM_top +
  decay(
    amount_t = HUM_top,
    k = kHUM,
    tempCoefficient = soil_temp(
      depth = 25,
      month = m,
      T_ave = T_ave[y - 1 + m],
      A_0 = T_range[y - 1 + m],
      th_diff = phi
    )
  )

HUM_after_rom_top <-
  HUM_after_decomp_top*(1-fco2)

CO2_HUM_top <- 
  HUM_top - HUM_after_rom_top

HUM_after_t <- 
  HUM_after_rom_top * (1-ft)

# HUM subsoil ----

HUM_sub <-
  ifelse(y == 1 & m == 1, init_pool_sub["HUM"], HUM_after_rom_sub[m-1]) +
  HUM_after_rom_top * ft+
  FOM_after_decomp_sub*hum_coef(clayfrac = clay_sub)

HUM_after_decomp_sub <-
  HUM_sub +
  decay(
    amount_t = HUM_sub,
    k = kHUM,
    tempCoefficient = soil_temp(
      depth = 100,
      month = m,
      T_ave = T_ave[y - 1 + m],
      A_0 = T_range[y - 1 + m],
      th_diff = phi
    )
  )

HUM_after_rom_sub <-
  HUM_after_decomp_sub*(1-fco2)

CO2_HUM_sub <- 
  HUM_sub - HUM_after_rom_sub 


# ROM topsoil ----

ROM_top <-
  ifelse(y == 1 & m == 1, init_pool_top["ROM"], ROM_after_t[m-1])+
  HUM_after_decomp_top*(1-fco2)

ROM_after_decomp_top <-
  ROM_top +
  decay(
    amount_t = ROM_top,
    k = kROM,
    tempCoefficient = soil_temp(
      depth = 25,
      month = m,
      T_ave = T_ave[y - 1 + m],
      A_0 = T_range[y - 1 + m],
      th_diff = phi
    )
  )

ROM_after_final_top <-
  ROM_after_decomp_top*(1-fco2)

CO2_ROM_top <- 
  ROM_top - ROM_after_final_top

ROM_after_t <- 
  ROM_after_final_top * (1-ft)

# ROM subsoil ----

ROM_sub <-
  ifelse(y == 1 & m == 1, init_pool_sub["HUM"], ROM_after_final_sub[m-1]) +
  ROM_after_final_top * ft+
  HUM_after_decomp_sub*(1-fco2)
  
ROM_after_decomp_sub <-
  ROM_sub +
  decay(
    amount_t = ROM_sub,
    k = kHUM,
    tempCoefficient = soil_temp(
      depth = 100,
      month = m,
      T_ave = T_ave[y - 1 + m],
      A_0 = T_range[y - 1 + m],
      th_diff = phi
    )
  )

ROM_after_final_sub <-
  ROM_after_decomp_sub * (1 - fco2)

CO2_ROM_sub <- 
  ROM_sub - ROM_after_final_sub 

