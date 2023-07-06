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
  dplyr::select(where(~!any(is.na(.))))


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

# temp_m=temperatures$meanmonthlytemp
# 
# data.frame("tempCoefficient" = do.call(rbind, lapply(temp_m, humification)),
#            "temp_m" = temperatures$meanmonthlytemp,
#            "month"=rep(seq(1,12,1),length(temp_m)/12)) |>
  #ggplot(aes(y=tempCoefficient, x=temp_m)) + geom_line() + theme_classic()  
  #ggplot(aes(y=tempCoefficient, x=month)) + geom_smooth() + geom_point()+ theme_classic() 

## turnover ----

# C decay in each pool  

decay <- function(amount, k, tempCoefficient ){
  amount = amount_t * (-k * tempCoefficient)
  amount
}



# humification coeficcient as function of clay content

humification <- function(clayfrac){
  R= 1.67 * (1.85 + 1.6 * exp(-7.86 * clayfrac))
  h= 1/(R+1)
  
  h
}

# clayfrac=seq(0.01,0.6,0.01)
# 
# data.frame("h" = do.call(rbind, lapply(clayfrac, humification)),
#            "clayfrac" = clayfrac) |>
#   ggplot(aes(y=h, x=clayfrac)) + geom_line() + theme_classic()


# inmovilization proportion as a function of C/N relation

inmovilization <- function(cn){
  cnfrac=min(56.2*cn^(-1.69),1)
  
  cnfrac
}


# cn=seq(8,30,0.5)
# 
# data.frame("cnfrac" = do.call(rbind, lapply(cn, inmovilization)),
#            "cn" = cn) |>
#   ggplot(aes(y=cnfrac, x=cn)) + geom_line() + theme_classic()

# transport vertical ----

