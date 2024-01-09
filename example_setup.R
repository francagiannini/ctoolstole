# Call packages 

library(rCTOOL)

library(tidyverse)

# Example scenario 55 years coming from Klimagræs----

## Input setup ----

### Temperature ----- 
# We set an scenario for the average condition of historical Foulum registers 

temperatures <- read.table("InputFiles/temp55years.txt")
colnames(temperatures) <- c("temp_m")

head(temperatures)

temp_minmax <- read.table("InputFiles/temp_ranges_foulum.txt", header = T) |> 
  mutate(temp_amp =max-min) 

T_ave <- temperatures$temp_m

T_range <- rep(temp_minmax$temp_amp, 55)

### Carbon inputs ----
# Simple yearly C inputs
cinp <- read.table("InputFiles/data.txt", sep = "\t", header= TRUE)

colnames(cinp) <-c("year",
                   "Cinp_plant_top","Cinp_plant_sub",
                   "Cinp_Cmanure", "C14plant","C14manure")

C_input_top <- cinp$Cinp_plant_top

C_input_sub <- cinp$Cinp_plant_sub

C_input_man <- cinp$Cinp_Cmanure

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

# Monthly distribution of C inputs taken from previous version 
# assuming that C inputs comes from grain crops 
# and the Organic fertilization is all made in march  

month_prop <- c(0,0,0,8,12,16,64,0,0,0,0,0)/100

month_man <- c(0,0,100,0,0,0,0,0,0,0,0,0)/100

# Fraction of manure that we consider is already Humidified

fman <- soil_df$HumFraction_manure

### Soil ----
#Parameters referring to site-specific soil conditions 

# Initial C stock at 1m depth 
Cinit <- soil_df$`Initial C(t/ha)_soilInit`

# Proportion of the total C allocated in topsoil

Cproptop <- 0.47

clay_top <- soil_df$clayfraction_crop

clay_sub <- soil_df$clayfraction_crop

# Diffusion index 
phi <- 0.035

# respiration fraction
fco2 <- 0.628

# romification fraction
fromi <- 0.012

# decomposition rates 
kFOM <- soil_df$FOMdecompositionrate_crop

kHUM <- soil_df$HUMdecompositionrate_crop

kROM <- soil_df$ROMdecompositionrate_crop

# transport rate
ftr <- soil_df$tF_crop

# initial pool distribution
fHUM_top <- soil_df$PupperLayer_soilInit

fROM_top <- 1-soil_df$FOMfractionPlantTopLayer_fom-fHUM_top

fHUM_sub <- soil_df$PLoweLayer_soilInit

fROM_sub <- 1-soil_df$FOMfractionPlantLowerLayer_fom-fHUM_sub 

# CN relation
CN <- soil_df$`C/N_soilInit`

## Pre-Processing for time 0 ----
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

# time period 
y=seq(1,length(C_input_top),1) 
m=seq(1,12,1)
# 
initial_value <-
  cbind(
    "step" = 1,
    "yr" = y[1],
    "mth" = 1,
    init_pool_top,
    init_pool_sub,
    "C_topsoil" = NA,
    "C_subsoil" = NA,
    
    "FOM_tr" = NA,
    "HUM_tr" = NA,
    "ROM_tr" = NA,
    
    "C_tr" = NA,
    
    "CO2_FOM_top" = NA,
    "CO2_HUM_top" = NA,
    "CO2_ROM_top" = NA,
    
    "CO2_FOM_sub" = NA,
    "CO2_HUM_sub" = NA,
    "CO2_ROM_sub" = NA,
    
    "C_CO2_top" = NA,
    "C_CO2_sub" = NA
  )

# Defining number of steps in the complete run 

nsteps <- as.list(seq(1, length(y) * length(m), 1))

# Turnover core function -----

## faster ----

#iterateFunc <- function(nsteps, initial_value) {

result_pools <-
  matrix(ncol = length(initial_value), nrow = length(nsteps))

colnames(result_pools) <- colnames(initial_value)

result_pools[1, ] <- initial_value

system.time(
  for (i in 2:length(nsteps)) {
    result_pools[i, ] <- turnover(i)
  }
  
  
)

# return(result_pools)
# }
#tired <- iterateFunc(nsteps, initial_value)

input <- 
  sum(C_input_top)+sum(C_input_sub)+sum(C_input_man)

SOC_stock <- result_pools[length(nsteps),"C_topsoil"] +
  result_pools[length(nsteps),"C_subsoil"]

emited <-sum(result_pools[,"C_CO2_top"],na.rm = TRUE) +
  sum(result_pools[,"C_CO2_sub"],na.rm = TRUE)

transp <- sum(result_pools[,"C_tr"],na.rm = TRUE)

as.numeric((Cinit+input))-as.numeric((SOC_stock+emited))

theme_set(theme_bw())
theme_update(panel.grid = element_blank())

result_pools |> 
  as.data.frame() |> 
  pivot_longer(
    cols = c(C_subsoil,C_topsoil), 
    names_to = "depth",
    values_to = "SOC" )|>
  ggplot(aes(x=step,y=SOC,fill=depth))+
  geom_col(position = "stack")

pool_cols=values = c(FOM_top="#E7298A", FOM_sub="#7570B3",
                     HUM_top="#66A61E", HUM_sub="#E6AB02",
                     ROM_top="#1B9E77", ROM_sub="#D95F02")

result_pools |> 
  as.data.frame() |> 
  pivot_longer(
    cols = c("FOM_top","HUM_top","ROM_top",
             "FOM_sub","HUM_sub","ROM_sub"), 
    names_to = "pool",
    values_to = "SOC" )|>  
  mutate(pool = fct_relevel(pool, 
                            "FOM_top", "FOM_sub", 
                            "HUM_top", "HUM_sub",
                            "ROM_top", "ROM_sub")) |> 
  ggplot(aes(x=step,y=SOC,fill=pool))+
  geom_col(position = "stack")+
  scale_fill_manual(values = pool_cols)+
  ylim(0,150)

out_mauro_sa <- 
  read.table("C:/Users/au710823/OneDrive - Aarhus universitet/ctool1st2022/ctool_mauro_2023/outputFiles/totalAmount.txt", header = T)|> 
  #select(c("total.1.1.","total.2.1.")) |> 
  mutate(id=seq(1,55*12,1)) |> 
  rename(CTop="total.1.1." ,
         CSub="total.2.1.")

out_mauro_sa |> 
  pivot_longer(
  cols = c(CSub, CTop),
  names_to = "depth",
  values_to = "SOC"
) |>
  ggplot(aes(x = id, y = SOC, fill = depth)) +
  geom_col(position = "stack")

out_mauro_sa |>
  pivot_longer(
    cols = c(
      "fomcPlant.1.1.",
      "humcPlant.1.1.",
      "romcPlant.1.1.",
      "fomcManure.1.1.",
      "humcManure.1.1.",
      "romcManure.1.1.",
      "fomcPlant.2.1.",
      "humcPlant.2.1.",
      "romcPlant.2.1.",
      "fomcManure.2.1.",
      "humcManure.2.1.",
      "romcManure.2.1."
    ),
    names_to = "pool",
    values_to = "SOC"
  ) |> mutate(
    pool =
      recode_factor(
        pool,
        "fomcPlant.1.1." = 'FOM_top',
        "humcPlant.1.1." = 'HUM_top',
        "romcPlant.1.1." = 'ROM_top' ,
        "fomcManure.1.1." = 'FOM_top',
        "humcManure.1.1." = 'HUM_top',
        "romcManure.1.1."= 'ROM_top',
        "fomcPlant.2.1." = 'FOM_sub',
        "humcPlant.2.1."= 'HUM_sub',
        "romcPlant.2.1." = 'ROM_sub',
        "fomcManure.2.1." = 'FOM_sub',
        "humcManure.2.1." = 'HUM_sub',
        "romcManure.2.1." = 'ROM_sub'
      ) ) |>  
  mutate(pool = fct_relevel(pool, 
                     "FOM_top", "FOM_sub", 
                     "HUM_top", "HUM_sub",
                     "ROM_top", "ROM_sub")) |> 
  ggplot(aes(x = id, y = SOC, fill = pool)) +
      geom_col(position = "stack") +
        scale_fill_manual(values = pool_cols) +
      ylim(0, 150)

plot(out_mauro_sa$CTop,result_pools[,"C_topsoil"])+
  abline(0,1, col="#D95F02")

plot(out_mauro_sa$CSub,result_pools[,"C_subsoil"])+
  abline(0,1, col="#D95F02")
