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


#Map of social media use per capita
weibo2013PC.qpal <- colorQuantile("YlOrBr", protest_geo_merge$weibo2013PC, n=7)  
weibo2013PC.map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = protest_geo_merge,  
    color=~weibo2013PC.qpal(weibo2013PC), fillOpacity = 0.7,  weight = 1, smoothFactor = 0.2, stroke = TRUE  ,     opacity = 1) %>%    
  addLegend(    position = "bottomright",   
                pal = weibo2013PC.qpal,  
                values = protest_geo_merge$weibo2013PC,
                title = "Social Media Use Per Capita<br>by City, 2013 (quantiles)" ,labFormat = labelFormat(suffix = ""),     opacity = 1)
