# Foulum measurements January 2014 to January 2023 ----
library(tidyverse)
library(plotly)
## Temperature ----
theme_set(theme_bw())
theme_update(panel.grid = element_blank())

# temp_measures_hourly <- read.csv("FoulumHourly01012014to01012023.csv", dec = ".")
# 
# temp_hr <- temp_measures_hourly|> 
#   mutate_all(as.numeric) |> 
#   mutate(date = dmy(temp_measures_hourly$date)) |> 
#   mutate(date_month= floor_date(date, "month"),
#          month=month(date),
#          year=year(date))
# 
# temp_hr |> ggplot(aes(x=month,y=mesotp30))+
#   geom_smooth(aes(group=year))+
#   geom_point()#+
#   #geom_point(aes(y = mesotp30, color = "red")) #+
#  # geom_smooth()
# 
# temp_hr |> ggplot(aes(y=mesotp30,x=metp))+
#   #geom_smooth()+
#   geom_point()+
#   #geom_point(aes(y = mesotp10, color = "red")) +
#   geom_smooth()

temp_measures_daily <- read.csv("FoulumDaily01012014to01012023.csv", 
                                dec = ".") |> 
  mutate(date = dmy(date)) |> 
  mutate(date_month= floor_date(date, "month"),
         month=month(date),
         year=year(date),
         julianday=yday(date),
         temp_amp=(maxte-minte))

# Monthly ----

temp_monthly <- 
  temp_measures_daily |> 
  mutate(daily_amp = maxte-minte) |> 
  group_by(date_month) |> 
  summarize(temp_m = mean(temp, na.rm = T),
            soil_t_ave_10 = mean(soilt10 , na.rm = T),
            soil_t_ave_30 = mean(soilt30 , na.rm = T),
            temp_amp=mean(daily_amp, na.rm = T),
            prec=sum(prec),
            month=month(date),
            year=year(date)
            ) |> unique()

temp_monthly |> ggplot(aes(x=date_month,y=temp_m))+
  geom_line(#aes(group=year)
              )+
  geom_point()+
  geom_point(aes(y = soil_t_ave_10, color = "red"))+
  geom_line(aes(y = soil_t_ave_10, color = "red"))+
  geom_point(aes(y = soil_t_ave_30, color = "blue"))+
  geom_line(aes(y = soil_t_ave_30, color = "blue"))+
  geom_abline(slope = 0, linetype="dashed")


th_diff <- data.frame(
  "soil" = c(paste("JB", seq(1, 9, 1)), "defect"),
  "th_diff" = c(
    0.073,0.061452, 0.073,0.056484, 0.054,0.051516,
    0.049032, 0.046548,0.044064,0.035))

df_temp <- expand_grid(
  temp_monthly,
  th_diff,
  "depth" = c(10, 50, 30, 100, 200))

# functions ----

# temperature dependence
# modifying decomposition as function of monthly temperatures 

#https://open.library.okstate.edu/rainorshine/chapter/13-4-sub-surface-soil-temperatures/


temperature_MandU_monthly <- #function(depth, temp_m, temp_amp, month) {
  function(j) {
    #browser()
    # depth in meters#
    z = as.numeric(j["depth"]) / 2 * 0.01 
    
    #temporal position in daily bases setted as the last day of each month
    
    #julianDay = as.numeric(j["month"]) * 30.4166 #+ 119
    
    t = as.numeric(j["month"]) #(julianDay + soil_df$offset_soilInit)
    
    #soil thermal difussivity 
    #see https://doi.org/10.1016/j.biosystemseng.2017.06.011
    #dependent on soil moisture
    # this is m2/day 0.35e-6 if m2/s per second*3600*24
    
    Th_diff <-
      as.numeric(j["th_diff"])/30 
    
    #angular frequency
    # here the cycle is daily, for secondly cycles (365 * 24 * 3600) 
    rho <-
      3.1415926 * 2/ 30 #/365
    
    # Damping depth here in m
    D <- sqrt(2 * Th_diff / rho)
    
    # here monthly daily average temperature 
    T_ave <- as.numeric(j["temp_m"])
    
    # here monthly daily range of temperature
    A_0 <- as.numeric(j["temp_amp"])
    
    # Soil temperature at t days and z depth in m Montein and Unsworth 
    T_zt <-
      T_ave + A_0 * exp(-z / D) * sin(rho * t - z / D)
    
    # temp coefficient uniti at 10 C¤
    tempCoefficient <-
      7.24 * exp(-3.432 + 0.168 * T_zt * (1 - 0.5 * T_zt / 36.9))
    
    
    data.frame("tempCoefficient" = tempCoefficient,
               "soil_temp" = T_zt
    )
  }


tmp_example_MandU <- cbind(df_temp,
                        do.call("rbind", 
                                apply(df_temp, 1, temperature_MandU_monthly)))

scatter_MandU <- 
  tmp_example_MandU |>
  filter(depth %in% c("30","10")& soil %in% c("defect","JB 4")) |> 
  ggplot(aes(y=soil_temp, #soil_temp,
             x=month, 
             col=as.factor(depth)#,
  )) +
  #geom_point(aes(col=depth))+
  geom_point(aes(y=soil_t_ave_10), color="#F8766D", shape="0")+
  geom_point(aes(y=soil_t_ave_30), color="#00BFC4")+
  geom_point(aes(y=temp_m), color="black", show.legend = TRUE,
             shape="3")+
  geom_smooth(aes(group=interaction(depth,soil), linetype=soil), alpha=0.2) + 
  #geom_abline(slope = 1, intercept = c(0,0))+
  #facet_grid(soil~amplitude)+
  #facet_grid(.~amplitude)+
  scale_x_continuous(breaks = seq(1,12,1))

#intervative_scatter <- 
  ggplotly(scatter_MandU)
#   library(orca)
#   
#   # Export the Plotly plot to an HTML file
# orca(scatter_MandU, "interactive_plot_temp_fou.html")

  tmp_example_MandU |>
  filter(depth==c(30) & soil =="defect") |> 
  ggplot(aes(y=soil_t_ave_30, #soil_temp,
             x=soil_temp, 
             col=as.character(month)#,
  )) +
  geom_point(size=2#aes(col=depth)
             )+
  #geom_smooth(aes(group=soil, linetype=soil), se=FALSE) + 
  geom_abline(slope = 1, intercept = c(0,0))+
  labs(x="observed", y="predicted")
  #facet_grid(soil~amplitude)+
  #facet_grid(.~amplitude)+
  
  # Daily ----

  temp_measures_daily|> ggplot(aes(x=date,y=temp))+
    geom_line(#aes(group=year)
      )+
    geom_point()+
    geom_point(aes(y = soilt10, color = "red"))+
    geom_line(aes(y = soilt10, color = "red"))+
    geom_point(aes(y = soilt30, color = "blue"))+
    geom_line(aes(y = soilt30, color = "blue"))+
    geom_abline(slope = 0, linetype="dashed")
  
  
  th_diff <- data.frame(
    "soil" = c(paste("JB", seq(1, 9, 1)), "defect"),
    "th_diff" = c(
      0.073,0.061452, 0.073,0.056484, 0.054,0.051516,
      0.049032, 0.046548,0.044064,0.035))
  
  df_temp_day <- expand_grid(
    temp_measures_daily,
    th_diff,
    "depth" = c(10, 30, 100, 200)
    )
  
  # functions ----
  
  # temperature dependence
  # modifying decomposition as function of monthly temperatures 
  
  #https://open.library.okstate.edu/rainorshine/chapter/13-4-sub-surface-soil-temperatures/
  
  
  temperature_MandU_daily <- #function(depth, temp_m, temp_amp, month) {
    function(j) {
      #browser()
      # depth in meters#
      z = as.numeric(j["depth"]) * 0.01 
      
      #temporal position in daily bases setted as the last day of each month
      
      #julianDay = as.numeric(j["month"]) * 30.4166 #+ 119
      
      t = as.numeric(j["julianday"]) #(julianDay + soil_df$offset_soilInit)
      
      #soil thermal difussivity 
      #see https://doi.org/10.1016/j.biosystemseng.2017.06.011
      #dependent on soil moisture
      # this is m2/day 0.35e-6 if m2/s per second*3600*24
      
      Th_diff <-
        as.numeric(j["th_diff"])#
      
      #angular frequency
      # here the cycle is daily for secondly cycles (365 * 24 * 3600) 
      rho <-
        3.1415926 * 2/365
      
      # Damping depth here in m
      D <- sqrt(2 * Th_diff / rho)
      
      # here monthly daily average temperature 
      T_ave <- as.numeric(j["temp"])
      
      # here monthly daily range of temperature
      A_0 <- as.numeric(j["temp_amp"])
      
      # Soil temperature at t days and z depth in m Montein and Unsworth 
      T_zt <-
        T_ave + A_0 * exp(-z / D) * sin(rho * t - z / D)
      
      # temp coefficient uniti at 10 C¤
      tempCoefficient <-
        7.24 * exp(-3.432 + 0.168 * T_zt * (1 - 0.5 * T_zt / 36.9))
      
      
      data.frame("tempCoefficient" = tempCoefficient,
                 "soil_temp" = T_zt
      )
    }
  
  
  tmp_example_MandU_day <- cbind(df_temp_day,
                                 do.call("rbind",
                                         apply(df_temp_day, 1, 
                                               temperature_MandU_daily)))
  
  scatter_MandU_day <- 
    tmp_example_MandU_day |>
    filter(depth %in% c("30","10")& soil %in% c("defect","JB 4")) |>
    #filter(soil=="defect") |> 
    ggplot(aes(y=soil_temp, #soil_temp,
               x=julianday, 
               col=as.factor(depth)#,
    )) +
    #geom_point(aes(col=depth))+
    geom_point(aes(y=soilt10), color="#F8766D", shape="0")+
    geom_point(aes(y=soilt30), color="#00BFC4")+
    geom_point(aes(y=temp), col="black", shape=3)+
    geom_smooth(aes(group=interaction(depth,soil), 
                    linetype=soil), se=FALSE)
  
  #intervative_scatter <- 
  ggplotly(scatter_MandU_day)
  
  tmp_example_MandU_day |>
    filter(depth==c(30) & soil =="defect") |> 
    ggplot(aes(y=soilt30, #soil_temp,
               x=soil_temp, 
               col=reorder(as.character(month), month)#,
    )) +
    geom_point(#aes(col=depth)
    )+
    #geom_smooth(aes(group=soil, linetype=soil), se=FALSE) + 
    geom_abline(slope = 1, intercept = c(0,0))+
    #facet_grid(soil~amplitude)+
    #facet_grid(.~amplitude)+
    theme_classic()
  

temperature_bis <- #function(depth, temp_m, temp_amp, month) {
  function(j) {
    #browser()
    
    # depth in meters#
    D = as.numeric(j["depth"]) * 0.01 #as.numeric(j["depth"]) / 2 * 0.01 
    
    #temporal position in secondly bases setted as the last day of each month
    julianDay = as.numeric(j["month"]) * 30.4166
    
    t = (julianDay + soil_df$offset_soilInit)*24*3600
    
    #soil thermal conductivity different from diffusivity
    
    # this is m2/s per second*3600*24 by defect 8e-7
    
    a <- 8e-7
    #as.numeric(j["th_cond"]) 
    
    #osilation time in seconds 
    # here is secondly cycles (365 * 24 * 3600) 
    t_0 <-3.15e-7
    
    # here yearly average temperature 
    T_mean <- as.numeric(j["temp_m"])
    
    # here monthly daily range of temperature
    T_xf <- t-D*sqrt(t_0/4*a*3.1415926)
    
    # Soil temperature at t days and z depth in m Montein and Unsworth 
    
    T_zt_bis <-
      T_mean + (T_xf-T_mean) * exp(-D* sqrt(3.1415926*a*t_0))
    
    # temp coefficient uniti at 10 C¤
    
    tempCoefficient_bis <-
      7.24 * exp(-3.432 + 0.168 * T_zt_bis * (1 - 0.5 * T_zt_bis / 36.9))
    
    
    data.frame("tempCoefficient_bis" = tempCoefficient_bis,
               "soil_temp_bis"=T_zt_bis)
  }

tcoeff_example2 <- cbind(df_temp,
                         do.call("rbind", apply(df_temp, 1, temperature_bis)))


plot(tcoeff_example2$soil_temp_bis, tcoeff_example2$temp_m)

#hist(tcoeff_example$tempCoefficient)

