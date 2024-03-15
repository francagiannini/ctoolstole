temp_minmax = read.table("c:/Users/João Serra/Downloads/temp_ranges_foulum.txt", header = T) |> 
  mutate(temp_amp =max-min) 

# set simulation parameters ----
period = define_timeperiod(yr_start = 2006, yr_end = 2010)
management = management_config(manure_monthly_allocation = c(0,0,1,0,0,0,0,0,0,0,0,0), plant_monthly_allocation = c(0,0,0,8,12,16,64,0,0,0,0,0)/100) # set to default
soil = soil_config(clay_top = 0.25, clay_sub = 0.35)
cin = define_Cinputs(Cin_top = rep(1,5), Cin_sub = rep(0.2,5), Cin_man = rep(0.1,5), time_config = period)
temp = prepare_temperature('c:/Users/João Serra/OneDrive/Modelling/ctool/Temp_foulum.csv',Trange_col = rep(temp_minmax$temp_amp, 5))
soil_pools = initialize_soil_pools(soil_config = soil)

# run simulation ----
ctool = run_ctool(time_config = period, cin_config = cin, m_config = management, t_config = temp, s_config = soil, soil_pools = soil_pools)
initial <- soil$Csoil_init
int = sum(cin$Cin_top) + sum(cin$Cin_sub) + sum(cin$Cin_man)
st = ctool$C_topsoil[60] + ctool$C_subsoil[60]
em = sum(ctool$em_CO2_top) + sum(ctool$em_CO2_sub)
(initial + int) - (st + em)


require('ggplot2')
ctool$id = 1:nrow(ctool)
ggplot(ctool, aes(x=id,y=SOC_stock)) + geom_line()

ggplot(ctool_yr, aes(x=yrs,y=em_CO2_total)) + geom_line()