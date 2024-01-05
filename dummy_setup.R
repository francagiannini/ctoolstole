# Call packages 

library(rCTOOL)

library(tidyverse)

# Dummy scenario 5 years----

## Input setup ----

### Temperature ----- 
# We set an scenario for the average condition of historical Foulum registers 
temperatures <- read.table("InputFiles/temp55years.txt")
colnames(temperatures) <- c("temp_m")
head(temperatures)

temp_minmax <- read.table("InputFiles/temp_ranges_foulum.txt", header = T) |> 
  mutate(temp_amp =max-min)

temperatures <- temperatures |> mutate(month=rep(seq(1,12,1),55)) |> 
  group_by(month) |> 
  summarize(monthly_mean=mean(temp_m))

T_ave <- rep(temperatures$monthly_mean,5)

T_range <- rep(temp_minmax$temp_amp, 5)

### Carbon inputs ----
# Simple set up of a yearly C inputs

C_input_top <- rep(1,5)

C_input_sub <- rep(0.1,5)

C_input_man <- rep(0.2,5)

# Monthly distribution of C inputs taken from previous version 
# assuming that C inputs comes from grain crops 
# and the Organic fertilization is all made in march  

month_prop <- c(0,0,0,8,12,16,64,0,0,0,0,0)/100

month_man <- c(0,0,100,0,0,0,0,0,0,0,0,0)/100

# Fraction of manure that we consider is already Humidified

fman <- 0.192

### Soil ----
#Parameters referring to site-specific soil conditions 

# Initial C stock at 1m depth 
Cinit <- 100

# Proportion of the total C allocated in topsoil

Cproptop <- 0.47

clay_top <- 0.1

clay_sub <- 0.15

# Diffusion index 
phi <- 0.035

# respiration fraction
fco2 <- 0.628

# romification fraction
fromi <- 0.012

# decomposition rates 
kFOM <- 0.12

kHUM <- 0.0028

kROM <- 3.858e-05

# transport rate
ftr <- 0#0.003

# initial pool distribution
fHUM_top <- 0.4803

fROM_top <- 0.4881 

fHUM_sub <- 0.3123

fROM_sub <- 0.6847 

# CN relation
CN <- 10

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

y=seq(1,5,1) 
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

# Run Turnover core function -----
## faster run----

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
                

as.numeric(Cinit+input) == as.numeric(SOC_stock+emited)
as.numeric(Cinit+input) - as.numeric(SOC_stock+emited)

theme_set(theme_bw())
theme_update(panel.grid = element_blank())

result_pools |> 
  as.data.frame() |> 
  pivot_longer(cols = c(C_subsoil,C_topsoil), 
               names_to = "depth",values_to = "SOC" )|>
  ggplot(aes(x=step,y=SOC,fill=depth))+
  geom_col()

