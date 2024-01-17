# static simulation ----
## set simulation parameters ----
period = define_timeperiod(yr_start = 2006, yr_end = 2010)
management = management_config(manure_monthly_allocation = c(0,0,.5,.5,0,0,0,0,0,0,0,0)) # set to default
soil = soil_config(clay_top = 0.25, clay_sub = 0.35, ftr = 0.003)
cin = define_Cinputs(Cin_top = c(2,1.5,2,0.5,1), Cin_sub = rep(0.1,5), Cin_man = c(2,1,0.5,1.5,1), time_config = period)
temp = prepare_temperature('c:/Users/João Serra/OneDrive/Modelling/ctool/Temp_foulum.csv','Tavg')
soil_pools = initialize_soil_pools(soil_config = soil)

## run simulation ----
ctool = run_ctool(time_config = period, cin_config = cin, m_config = management, t_config = temp, s_config = soil, soil_pools = soil_pools)
ctool_yr = aggregate(.~yrs, FUN='sum', data = ctool[-1])

require('ggplot2')
ctool$id = 1:nrow(ctool)
ggplot(ctool, aes(x=id,y=SOC_stock)) + geom_line()

ggplot(ctool_yr, aes(x=yrs,y=em_CO2_total)) + geom_line()


# dynamic simulation ----
## set simulation parameters ----
period = define_timeperiod(yr_start = 2006, yr_end = 2010)
write_Cinput_management(yr_start = 2006, yr_end = 2010, filepath = './user_cin.csv')
soil = soil_config(clay_top = 0.25, clay_sub = 0.35, ftr = 0.003)
cin = define_Cinputs(cin_filepath = './user_cin.csv', time_config = period)
management = management_config(manag_filepath = './user_cin.csv', time_config = period)
temp = prepare_temperature('c:/Users/João Serra/OneDrive/Modelling/ctool/Temp_foulum.csv','Tavg')
soil_pools = initialize_soil_pools(soil_config = soil)




