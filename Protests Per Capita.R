#Quantile map of protests per capita

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

############################
#Mapping the data

#Map of Protests Per Capita
events_PC_wick.qpal <- colorQuantile("YlOrBr", protest_geo_merge$events_PC_wick, n=7)  
events_PC_wick.map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = protest_geo_merge,  
    color=~events_PC_wick.qpal(events_PC_wick), fillOpacity = 0.7,  weight = 1, smoothFactor = 0.2, stroke = TRUE  ,     opacity = 1) %>%    
  addLegend(    position = "bottomright",   
                pal = events_PC_wick.qpal,  
                values = protest_geo_merge$events_PC_wick,
                title = "Protest Per Capita by City,<br>2013 (quantiles)" ,labFormat = labelFormat(suffix = ""),     opacity = 1)
