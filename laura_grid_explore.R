library(sf)
library(tidyverse)
library(tmap)
library(mapview)

theme_set(theme_bw())
theme_update(panel.grid = element_blank())

lauraC <-
  read.csv(
  "O:/Tech_AGRO/Jornaer/Franca/Laura/Data/20230524_KVNdata_til_database_med_tekstur_Kun_ORGC_LSH.csv", 
  header = T,
  #sep = ",",
  dec = ".",
  na.strings = "NA")

summary(lauraC)

JB <- lauraC |> 
  group_by(Point) |> 
  summarize(JB=mean(as.numeric(JB), na.rm=T))
 # table(JB, Point)
summary(JB$JB)

lauraC_sf <- 
  lauraC |> drop_na(c(Lat,Long)) |> 
  st_as_sf(coords = c( "Long","Lat"),
                      crs="25832") |> 
  group_by(Point) |> 
  mutate(JB=mean(as.numeric(JB), na.rm=T)) |> 
  ungroup()
  
tmap_mode("view")

tm_shape(lauraC_sf)+
  tm_dots()

lauraC_sf |> filter(!is.na(Clay)) |> 
  ggplot(aes(x=Clay, y=reorder(Depth,-Clay)
             ))+
  geom_point()+
  geom_boxplot()+
  stat_summary(fun=mean, 
               geom="point", 
               shape=20, 
               size=2, 
               color="red", fill="red",
               show.legend = T) +
  facet_grid(Year~JB)
  geom_line()

lauraC_sf |> filter(!is.na(OrgC)) |> 
  ggplot(aes(x=OrgC, y=reorder(Depth, -Lower)
  ))+
  geom_point()+
  geom_boxplot()+
  stat_summary(fun=mean, 
               geom="point", 
               shape=20, 
               size=2, 
               color="red", fill="red",
               show.legend = T) +
  facet_grid(Year~JB)+
  geom_line()

sebaC <-
    readxl::read_excel(
      "C:/Users/au710823/OneDrive - Aarhus universitet/ctool1st2022/CTOOL_stole/DataJoinedGrid.xlsx"#, 
      #header = T,
      #sep = ",",
      #dec = "."#,
      #na.strings = "NA"
    )
sebaC[sebaC == 'NA'] <- NA

JB <- sebaC |> 
  group_by(PointID) |> 
  summarise(JB=mean(as.numeric(JB), na.rm=T)) 
# table(JB, Point)
summary(JB$JB)  


sebaC <- sebaC |> 
  group_by(PointID) |> 
  mutate(JB=mean(as.numeric(JB), na.rm=T)) |> 
  ungroup()

sebaC |> filter(!is.na(OrgC)) |> 
  mutate(Depth_cat=as.factor(Depth)) |> 
  ggplot(aes(x=as.numeric(OrgC), y=reorder(Depth_cat, -Depth)
  ))+
  geom_point()+
  geom_boxplot()+
  stat_summary(fun=mean, 
               geom="point", 
               shape=20, 
               size=2, 
               color="red", fill="red",
               show.legend = T) +
  facet_grid(Year~JB)+
  geom_line()

### Lucas DDGD