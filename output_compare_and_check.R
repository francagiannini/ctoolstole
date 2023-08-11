library(tidyverse)
library(viridis)
## check outputs 


# Comparison ----

out_mauro_sa <- 
  read.table("C:/Users/au710823/OneDrive - Aarhus universitet/ctool1st2022/ctool_mauro_2023/outputFiles/totalAmount.txt", header = T)|> 
  #select(c("total.1.1.","total.2.1.")) |> 
  mutate(id=seq(1,55*12,1)) |> 
  rename(CTop="total.1.1." ,
         CSub="total.2.1.") |> 
  filter(id %in% seq(12,55*12,12))


out_exe_2.3 <- read.table(
  "O:/Tech_AGRO/Jornaer/Franca/CTOOL_sh/example_CTOOL/totalAmount.xls", 
  header = T) |> 
  #select(c("total.1.1.","total.2.1.")) |>
  rename(CTop="total.1.1." ,
                    CSub="total.2.1.")

bind_cols(out_mauro_sa, out_exe_2.3) |> 
  ggplot(aes(y=CTop...1,x=CTop...4))+
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_line()+
  labs(y="C# ",x=" c2.3 ")+
  theme_bw()

bind_cols(out_mauro_sa, out_exe_2.3) |> 
  ggplot(aes(CSub...2,CSub...5))+
  geom_point()+
  geom_line()

bind_cols(out_mauro_sa, out_exe_2.3) |> #cor()
  mutate(dif_top=CTop...1-CTop...4 ,
         dif_sub=CSub...2-CSub...5) |> 
  pivot_longer(cols = c(dif_top,dif_sub), 
               names_to = "output",values_to = "difference" ) |> 
  ggplot(aes(y=difference,x=id, col=output))+
  geom_point()+
  geom_line()+
  ylab("difference C# vs. c2.3 ")+
  theme_bw()

# pool dinamic ----

out_exe_2.3|> 
  mutate(year= seq(1,55,1)) |> 
  pivot_longer(cols=c("fomcPlant.1.1.","humcPlant.1.1.","romcPlant.1.1.",
                      "fomcManure.1.1.","humcManure.1.1.","romcManure.1.1.",
                      "fomcPlant.2.1.","humcPlant.2.1.","romcPlant.2.1.",
                      "fomcManure.2.1.","humcManure.2.1.","romcManure.2.1."
                      ),
               values_to = "Cstock", names_to = "pool") |> 
  ggplot(aes(x=year, y=Cstock, fill=pool))+
  geom_area(alpha=0.6 , colour="white")+
  scale_y_continuous(breaks = seq(0,120,10))+
  theme_bw()

# bind_cols(out_mauro_sa, out_exe_2.3) |> #cor()
#   mutate(diff_top=CTop...1-CTop...4 ,
#          diff_sub=CSub...2-CSub...5) |> 
#   mutate(dif_rel_top=dif_top/mean(CTop...1)*100) |> 
#   ggplot(aes(y=dif_rel_top,x=id))+
#   geom_point()+
#   geom_line()+
#   ylab("c#-c2.3 difference")+
#   theme_bw()

out_exe_2.3_C02 <- read.table(
  "O:/Tech_AGRO/Jornaer/Franca/CTOOL_sh/example_CTOOL/C02.xls", header = T)

out_exe_2.3_C02 |> 
  mutate(year= seq(1,55,1)) |> 
  pivot_longer(cols=c("Foml1","Foml2",
                      "Huml1","Huml2",
                      "Roml1","Roml2"),
               values_to = "C02", names_to = "pool") |> 
  ggplot(aes(x=year, y=C02, fill=pool)) +
  geom_area(alpha=0.6 , colour="white") +
  scale_fill_viridis(discrete = T)+
  theme_bw()

out_mauro_emited <- 
  read.table(
    "C:/Users/au710823/OneDrive - Aarhus universitet/ctool1st2022/ctool_mauro_2023/outputFiles/CO2.txt", 
    header = T)#|> 
#mutate(id=seq(1,55*12,1))

out_mauro_emited |> 
  mutate(month= seq(1,55*12,1)) |> 
  pivot_longer(cols=c("Foml1","Foml2",
                      "Huml1","Huml2",
                      "Roml1","Roml2"),
               values_to = "C02", names_to = "pool") |> 
  ggplot(aes(x=month, y=C02, fill=pool)) +
  geom_area(alpha=0.6 , colour="white") +
  scale_fill_viridis(discrete = T)+
  theme_bw()


out_exe_2.3_tr <- read.table(
  "O:/Tech_AGRO/Jornaer/Franca/CTOOL_sh/example_CTOOL/transport.xls", 
  header = T)

out_exe_2.3_tr |> 
  mutate(year= seq(1,55,1)) |> 
  pivot_longer(cols=c("Fom","Hum","Rom"),
               values_to = "Ctrans", names_to = "pool") |>
  ggplot(aes(x=year, y=Ctrans, fill=pool))+
  geom_area(alpha=0.6 , colour="white")+
  #scale_fill_brewer()+
  theme_bw()


out_mauro_tr <- 
  read.table(
    "C:/Users/au710823/OneDrive - Aarhus universitet/ctool1st2022/ctool_mauro_2023/outputFiles/transport.txt", 
    header = T)#|> 
#
out_mauro_tr |> 
  mutate(year= seq(1,55*12,1)) |> 
  pivot_longer(cols=c("Fom","Hum","Rom"),
               values_to = "Ctrans", names_to = "pool") |>
  ggplot(aes(x=year, y=Ctrans, fill=pool))+
  geom_area(alpha=0.6 , colour="white")+
  #scale_fill_brewer()+
  theme_bw()

# inputs ----

## Soil ----
soil <- as.data.frame(
  readLines("O:/Tech_AGRO/Jornaer/Franca/CTOOL_sh/example_CTOOL/input.txt"))
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

cinp <- read.table(
  "O:/Tech_AGRO/Jornaer/Franca/CTOOL_sh/example_CTOOL/data.txt", 
  sep = "\t", header= TRUE)

colnames(cinp) <-c("year",
                   "Cinp_plant_top","Cinp_plant_sub",
                   "Cinp_Cmanure", "C14plant","C14manure")

# Balance ----

initial <- 
  soil_df$`Initial C(t/ha)_soilInit`

input <- 
  sum(cinp$Cinp_plant_top)+sum(cinp$Cinp_plant_sub)+sum(cinp$Cinp_Cmanure)

stock_exe <- out_exe_2.3$CSub[55]+out_exe_2.3$CTop[55]

stock_mau <- out_mauro_sa$CSub[55]+out_mauro_sa$CTop[55]

emited <-sum(out_mauro_emited) 

(initial+input)-(stock_mau+emited)
