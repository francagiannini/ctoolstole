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

bind_cols(out_mauro_sa, out_exe_2.3) |> #cor()
  mutate(diff_top=CTop...1-CTop...4 ,
         diff_sub=CSub...2-CSub...5) |> 
  mutate(dif_rel_top=dif_top/mean(CTop...1)*100) |> 
  ggplot(aes(y=dif_rel_top,x=id))+
  geom_point()+
  geom_line()+
  ylab("c#-c2.3 difference")+
  theme_bw()
