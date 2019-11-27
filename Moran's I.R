#Moran's I


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

#Performing the Moran's I test
attach(protest_geo_merge@data)
MC<- moran.mc(Levents_PC_wick, lw_nowest, nsim=599, zero.policy=TRUE)
plot(MC, main="", las=1)
plot(MCB, main="", las=1, xlab = "", ylab = "")

#plotting Moran's I
Lwickedpop.Lag <- lag.listw(lw_nowest,protest_geo_merge@data$Levents_PC_wick ,zero.policy=TRUE )
which(protest_geo_merge@data$Levents_PC_wick<=0)
lm_lag_Lwickedpop <- lm(Lwickedpop.Lag ~protest_geo_merge@data$Levents_PC_wick )
plot(Lwickedpop.Lag ~ protest_geo_merge@data$Levents_PC_wick,xlab = "Log of Mass Events Per Capita", ylab = "Lagged Value")
abline(lm(Lwickedpop.Lag ~ protest_geo_merge@data$Levents_PC_wick), col="red")
title("Moran's I plot for mass incidents per capita")

# scale the variable and save it to a new column
protest_geo_merge@data$s_events_PC_wick <- scale(protest_geo_merge@data$Levents_PC_wick)  %>% as.vector()
# create a spatially lagged variable and save it to a new column
protest_geo_merge@data$lag_s_events_PC_wick <- lag.listw(lw_nowest, protest_geo_merge@data$s_events_PC_wick, zero.policy = T)

# moran scatterplot
x_nonlagged <- protest_geo_merge@data$s_events_PC_wick
y_lagged <- protest_geo_merge@data$lag_s_events_PC_wick %>% as.vector()
xydataframe <- data.frame(x_nonlagged, y_lagged)

# moran sccaterplot, in ggplot 
ggplot(xydataframe, aes(x_nonlagged, y_lagged)) + geom_point() + geom_smooth(method = 'lm', se = F) + 
  geom_hline(yintercept = 0, linetype = 'dashed') + geom_vline(xintercept = 0, linetype = 'dashed')  +
  labs(x = "Protests Per Capita", y = "Lagged Value")
