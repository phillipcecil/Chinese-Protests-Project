#LISA Cluster Map

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

#Creating a logged variable for mass incidents per capita
protest_geo_merge@data$Levents_PC_wick <- log(protest_geo_merge@data$events_PC_wick)

#Adding spatial lags to the data
nb_nowest <- poly2nb(protest_geo_merge, queen=TRUE)
lw_nowest <- nb2listw(nb_nowest, style="W", zero.policy = T)

#Local Moran test
lmoran <- localmoran(Levents_PC_wick, lw_nowest, zero.policy = T)
summary(lmoran)

# scale the variable and save it to a new column
protest_geo_merge@data$s_events_PC_wick <- scale(protest_geo_merge@data$Levents_PC_wick)  %>% as.vector()

# create a spatially lagged variable and save it to a new column
protest_geo_merge@data$lag_s_events_PC_wick <- lag.listw(lw_nowest, protest_geo_merge@data$s_events_PC_wick, zero.policy = T)

# create a new variable identifying the moran plot quadrant for each observation, dismissing the non-significant ones
protest_geo_merge@data$quad_sig <- NA

# high-high quadrant
protest_geo_merge@data[which(protest_geo_merge@data$s_events_PC_wick >= 0 & protest_geo_merge@data$lag_s_events_PC_wick >= 0 & lmoran[, 5] <= 0.05) ,"quad_sig" ] <- "high-high"
# low-low quadrant
protest_geo_merge@data[which(protest_geo_merge@data$s_events_PC_wick <= 0 & protest_geo_merge@data$lag_s_events_PC_wick <= 0 & lmoran[, 5] <= 0.05) ,"quad_sig" ] <- "low-low"
# high-low quadrant
protest_geo_merge@data[which(protest_geo_merge@data$s_events_PC_wick >= 0 & protest_geo_merge@data$lag_s_events_PC_wick <= 0 & lmoran[, 5] <= 0.05) ,"quad_sig" ] <- "high-low"
# low-high quadrant
protest_geo_merge@data[which(protest_geo_merge@data$s_events_PC_wick <= 0 & protest_geo_merge@data$lag_s_events_PC_wick >= 0 & lmoran[, 5] <= 0.05) ,"quad_sig" ] <- "low-high"
# non-significant observations
protest_geo_merge@data[which(lmoran[, 5] > 0.05), "quad_sig"] <- "not signif."  

# plotting the map
protest_geo_merge$quad_sig <- as.factor(protest_geo_merge$quad_sig)
protest_geo_merge@data$idB <- rownames(protest_geo_merge@data)
df_protest_geo_merge <- fortify(protest_geo_merge, region="idB")
df_protest_geo_merge <- left_join(df_protest_geo_merge, protest_geo_merge@data)
df_protest_geo_merge %>% 
  ggplot(aes(long, lat, group = group, fill = quad_sig)) + 
  geom_polygon(color = "white", size = .05)  + coord_equal() + 
  theme_void() + scale_fill_brewer(palette = "Set1")