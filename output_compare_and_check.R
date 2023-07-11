library(tidyverse)

## check outputs 

out_mauro_sa <- 
  read.table("C:/Users/au710823/OneDrive - Aarhus universitet/ctool1st2022/ctool_mauro_2023/outputFiles/totalAmount.txt", header = T)|> 
  select(c("total.1.1.","total.2.1.")) |> 
  mutate(id=seq(1,55*12,1)) |> 
  rename(CTop="total.1.1." ,
         CSub="total.2.1.") |> 
  filter(id %in% seq(12,55*12,12))


out_exe_2.3 <- read.table(
  "O:/Tech_AGRO/Jornaer/Franca/CTOOL_sh/example_CTOOL/totalAmount.xls", 
  header = T) |> 
  select(c("total.1.1.","total.2.1.")) |>
  rename(CTop="total.1.1." ,
                    CSub="total.2.1.")

bind_cols(out_mauro_sa, out_exe_2.3) |> 
  ggplot(aes(CTop...1,CTop...4))+
  geom_point()+
  geom_line()



