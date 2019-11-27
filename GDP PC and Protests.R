#GDP Per Capita and Mass Incidents Per Capita

library("tidyverse")
library("rgdal")
library("tigris")
library("leaflet")
library("RColorBrewer")
library("spdep")
library("lmtest")

############################
#Preparing the spatial data
china_protest <- readxl::read_xlsx("/Users/phillipcecil/Documents/Masters Econ/thesis/china/docs for the final draft/spreadsheets/FINAL TABLE.xlsx")
gadm36_CHN_2 <- readOGR(dsn = "/Users/phillipcecil/Documents/Masters Econ/thesis/china/china spatial/gadm/gadm36_CHN_shp/prefectures", layer = "gadm36_CHN_2")
protest_geo_merge <- geo_join(
  spatial_data = gadm36_CHN_2, 
  data_frame = china_protest, 
  by_sp = "GID_2", 
  by_df = "GID_2",how="inner")

# sccaterplot, in ggplot 
ggplot(protest_geo_merge@data, aes(x=GDPPC2015, y=events_PC_wick)) + geom_point() + geom_smooth(method="lm")  +
  labs(title="GDP Per Capita and Protests (logged)", y="(log) Protests Per Capita (logged)", x="(log) GDP Per Capita" ) +
  theme(plot.title = element_text(hjust = 0.5))

