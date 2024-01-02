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

fromi <- 0.012

month_prop <- c(0,0,0,8,12,16,64,0,0,0,0,0)/100

month_man <- c(0,0,100,0,0,0,0,0,0,0,0,0)/100

Cproptop <- 0.47

clay_top <- 0.1

clay_sub <- 0.15

Cinit <- 100

kFOM <- 0.12

kHUM <- 0.0028

kROM <- 3.858e-05

ftr <- 0.003

fHUM_top <- 0.4803

fROM_top <- 0.4881 

fHUM_sub <- 0.3123

fROM_sub <- 0.6847 

CN <- 10

# time period
y=seq(1,5,1) 
m=seq(1,12,1)

# time 0
# initial_values

startCAmount_top <- Cinit * Cproptop
startCAmount_sub <- Cinit * (1-Cproptop)

init_pool_top <-pool_cn(cn=CN,
                        HUM_frac = fHUM_top,
                        ROM_frac = fROM_top,
                        C_0=startCAmount_top)|> t()

colnames(init_pool_top) <- paste(colnames(init_pool_top), "top", sep = "_")

init_pool_sub <-pool_cn(cn=CN,
                        HUM_frac = fHUM_sub,
                        ROM_frac = fROM_sub,
                        C_0=startCAmount_sub) |> t()

colnames(init_pool_sub)<-paste(colnames(init_pool_sub),"sub",sep="_")

initial_value <-
  cbind(
    "step" = 1,
    "yr" = 1,
    "mth" = 1,
    init_pool_top,
    init_pool_sub
  )

turnover <- 
  function(i, initial_value) {
    
    result_pools <- ifelse(i==1, initial_value, result_pools[i - 1,])
    
    browser()
    
    m = as.numeric(result_pools["mth"]) + 1
    
    y = as.numeric(result_pools["yr"])
    
    # Pools ----- 
    ## FOM topsoil ----
    
    FOM_top <-
      result_pools['FOM_top'] +
      #ifelse(y == 1 & m == 1, init_pool_top["FOM"], FOM_top[i-1]) +
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
    
    substrate_FOM_decomp_top <-
      FOM_top - FOM_after_decomp_top
    
    FOM_humified_top <-
      substrate_FOM_decomp_top * hum_coef(clayfrac = clay_top)
    
    CO2_FOM_top <-
      substrate_FOM_decomp_top * (1 - hum_coef(clayfrac = clay_top))
    
    FOM_tr <-
      FOM_after_decomp_top * ftr
    
    FOM_top <-
      FOM_top - FOM_humified_top - CO2_FOM_top - FOM_tr
    
    ## FOM subsoil ----
    
    FOM_sub <-
      result_pools['FOM_sub'] +
      #ifelse(y == 1 & m == 1, init_pool_sub["FOM"], FOM_sub) +
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
    
    substrate_FOM_decomp_sub <-
      FOM_sub - FOM_after_decomp_sub
    
    FOM_humified_sub <-
      substrate_FOM_decomp_sub * hum_coef(clayfrac = clay_sub)
    
    CO2_FOM_sub <-
      substrate_FOM_decomp_sub * (1 - hum_coef(clayfrac = clay_sub))
    
    FOM_sub <-
      FOM_sub - FOM_humified_sub - CO2_FOM_sub
    
    ## HUM topsoil ----
    
    HUM_top <-
      result_pools['HUM_top'] +
      #ifelse(y == 1 & m == 1, init_pool_top["HUM"], HUM_top) +
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
    
    substrate_HUM_decomp_top <-
      HUM_top - HUM_after_decomp_top
    
    HUM_romified_top <-
      substrate_HUM_decomp_top * fromi
    
    CO2_HUM_top <-
      substrate_HUM_decomp_top * (1 - fromi) #fco2
    
    HUM_tr <-
      HUM_after_decomp_top * ftr
    
    HUM_top <-
      HUM_top - HUM_romified_top - CO2_HUM_top - HUM_tr
    
    ## HUM subsoil ----
    
    HUM_sub <-
      result_pools['HUM_sub'] +
      #ifelse(y == 1 & m == 1, init_pool_sub["HUM"], HUM_sub) +
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
    
    substrate_HUM_decomp_sub <-
      HUM_sub - HUM_after_decomp_sub
    
    HUM_romified_sub <-
      substrate_HUM_decomp_sub * fromi
    
    CO2_HUM_sub <-
      substrate_HUM_decomp_top * (1 - fromi) #fco2
    
    HUM_sub <-
      HUM_sub - HUM_romified_sub - CO2_HUM_sub
    
    ## ROM topsoil ----
    
    ROM_top <-
      result_pools['ROM_top'] +
      #ifelse(y == 1 & m == 1, init_pool_top["ROM"], ROM_top)+
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
    
    substrate_ROM_decomp_top <-
      ROM_top - ROM_after_decomp_top
    
    CO2_ROM_top <-
      substrate_ROM_decomp_top * fco2
    
    # ROM_not_respirated <-
    #   substrate_ROM_decomp_top*(1-fco2)
    
    ROM_tr <-
      ROM_after_decomp_top * ftr
    
    ROM_top <-
      ROM_top - ROM_tr - CO2_ROM_top
    
    ## ROM subsoil ----
    
    ROM_sub <-
      result_pools['ROM_sub'] +
      #ifelse(y == 1 & m == 1, init_pool_sub["HUM"], ROM_sub) +
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
    
    substrate_ROM_decomp_sub <-
      ROM_sub - ROM_after_decomp_sub
    
    CO2_ROM_sub <-
      substrate_ROM_decomp_top * fco2
    
    ROM_sub <- ROM_sub - CO2_ROM_sub
    
    # Results -----
    
    result_pools <-
      cbind(
        'step' = result_pools['step'] + 1,
        'yr' = ifelse(m == 1, y + 1, y),
        'mth' = ifelse(m + 1 > 12, 1, m),
        
        'FOM_top' = FOM_top,
        'HUM_top' = HUM_top,
        'ROM_top' = ROM_top,
        
        'FOM_sub' = FOM_sub,
        'HUM_sub' = HUM_sub,
        'ROM_sub' = ROM_sub
      )
  }


iterateFunction <- function(initial_value, num_steps) {
  # Create an empty arrange to store results
  result_pools <- data.frame(matrix(ncol = ncol(initial_value), nrow = 0))
  colnames(result_pools) <- colnames(initial_value)
  
  # Store initial value in the first position
  result_pools[1,] <- initial_value

  result_pools <- rbind(result_pools[1,],
                        do.call(rbind,
                                lapply(2:num_steps,
                                       turnover
                                       )
                                )
                        )
  
  return(result_pools)

# output <-
#   cbind(
#     'yr' = y,
#     'mth' = m,
#     
#     'FOM_top' = FOM_top,
#     'HUM_top' = HUM_top,
#     'ROM_top' = ROM_top,
#     
#     'FOM_sub' = FOM_sub,
#     'HUM_sub' = HUM_sub,
#     'ROM_sub' = ROM_sub,
#     
#     'C_topsoil' = FOM_top + HUM_top + ROM_top,
#     'C_subsoil' = FOM_sub + HUM_sub + ROM_sub,
#     
#     "FOM_tr" = FOM_tr,
#     "HUM_tr" = HUM_tr,
#     "ROM_tr" = ROM_tr,
#     
#     'C_tr' = FOM_tr + HUM_tr + ROM_tr,
#     
#     "CO2_FOM_top" = CO2_FOM_top,
#     "CO2_HUM_top" = CO2_HUM_top,
#     "CO2_ROM_top" = CO2_ROM_top,
#     
#     "CO2_FOM_sub" = CO2_FOM_sub,
#     "CO2_HUM_sub" = CO2_HUM_sub,
#     "CO2_ROM_sub" = CO2_ROM_sub,
#     
#     'C_CO2_top' = CO2_FOM_top + CO2_HUM_top + CO2_ROM_top,
#     'C_CO2_sub' = CO2_FOM_sub + CO2_HUM_sub + CO2_ROM_sub
#     
#   )
# 
# return(output)

                  }


# result_pools[1,] <- initial_value
# 
# res <- for(i in 2:6) turnover(result_pool[i - 1])
#   
# rbind(initial_value)
# 
# ##
# iterateFunction <- function(initial_value, num_steps) {
#   
# result_pools <- data.frame(matrix(ncol = ncol(initial_value), nrow = 0))
# colnames(result_pools) <- colnames(initial_value)
# 
#   # Store initial value in the first position
# result_pools[1,] <- initial_value
#   
#   # Use sapply to calculate and store results without a for loop
#   result_pool <-
#     rbind(initial_value, apply(2:num_steps,1 ,turnover(i = result_pool[i - 1])))
#   
#   return(result_pools)
#}

tosk <- iterateFunction(initial_value, num_steps)


