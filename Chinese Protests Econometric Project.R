
#R Code for Spatial Analysis of Protests in China
#Phillip Cecil
#2019


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

#Density plot of protests per capita
events_PC_wick_density_plot <- ggplot(protest_geo_merge@data, aes(x=events_PC_wick)) + 
  geom_density()  +
  labs(title="Frequency Distribution of Protests (per 10,000)", y="Density", x="Protests (per 10,000)" ) + 
  geom_vline(aes(xintercept=mean(events_PC_wick)),
             color="blue", linetype="dashed", size=1) +
  theme(plot.title = element_text(hjust = 0.5)  )

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

#Map of GDP per capita
GDPPC2015.qpal <- colorQuantile("YlOrBr", protest_geo_merge$GDPPC2015, n=7)  
GDPPC2015.map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = protest_geo_merge,  
    color=~GDPPC2015.qpal(GDPPC2015), fillOpacity = 0.7,  weight = 1, smoothFactor = 0.2, stroke = TRUE  ,     opacity = 1) %>%    
  addLegend(    position = "bottomright",   
                pal = GDPPC2015.qpal,  
                values = protest_geo_merge$GDPPC2015,
                title = "GDP Per Capita by City,<br>2013 (quantiles)" ,labFormat = labelFormat(suffix = ""),     opacity = 1)

############################
#LISA Cluster Map 

#Add spatial lags to the data
nb_nowest <- poly2nb(protest_geo_merge, queen=TRUE)
lw_nowest <- nb2listw(nb_nowest, style="W", zero.policy = T)
attach(protest_geo_merge@data)
MC<- moran.mc(events_PC_wick, lw_nowest, nsim=599, zero.policy=TRUE)
lmoran <- localmoran(events_PC_wick, lw_nowest, zero.policy = T)
summary(lmoran)

# scale the variable and save it to a new column
protest_geo_merge@data$s_events_PC_wick <- scale(protest_geo_merge@data$events_PC_wick)  %>% as.vector()
# create a spatially lagged variable and save it to a new column
protest_geo_merge@data$lag_s_events_PC_wick <- lag.listw(lw_nowest, protest_geo_merge@data$s_events_PC_wick, zero.policy = T)

# moran scatterplot, in basic graphics (with identification of influential observations)
x_nonlagged <- protest_geo_merge@data$s_events_PC_wick
y_lagged <- protest_geo_merge@data$lag_s_events_PC_wick %>% as.vector()
xydataframe <- data.frame(x_nonlagged, y_lagged)

# moran sccaterplot, in ggplot 
ggplot(xydataframe, aes(x_nonlagged, y_lagged)) + geom_point() + geom_smooth(method = 'lm', se = F) + 
  geom_hline(yintercept = 0, linetype = 'dashed') + geom_vline(xintercept = 0, linetype = 'dashed')  +
  labs(x = "Protests Per Capita", y = "Lagged Value")

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

############################
#Looking for correlations among variables
ggplot(china_protest, aes(log(GDPPC2015), log(events_PC_wick))) + geom_point() + geom_smooth(method = 'lm', se = F)  + 
  labs(title="GDP Per Capita and Protests (logged)",x = "(log) GDP Per Capita", y = "(log) Protests Per Capita") +
  theme(plot.title = element_text(hjust = 0.5)  ) 
ggplot(china_protest, aes(log(weibo2013PC), log(events_PC_wick))) + geom_point() + geom_smooth(method = 'lm', se = F)  + 
  labs(title="Social Media Use and Protests (logged)",x = "(log) Weibo Use Per Capita", y = "(log) Protests Per Capita") +
  theme(plot.title = element_text(hjust = 0.5) )

#########################################################################################################
#Spatial Regression
#########################################################################################################
model1 <- log(events_PC_wick) ~ log(weibo2013PC)+log(immigration) + log(urbanization_rate)  + log(savings_PC)  +log(natgrwthrate15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif) + henan + shaanxi
model2 <- log(events_PC_wick) ~ log(weibo2013PC)+log(immigration) + log(urbanization_rate)  + log(savings_PC)  +log(natgrwthrate15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif) + log_unemp+henan + shaanxi+guangdong +fujian+heilongjiang+jiangsu
model3 <- log(events_PC_wick) ~ log(weibo2013PC)+log(immigration) + log(urbanization_rate)  + log(savings_PC)  +log(natgrwthrate15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif) + log_unemp+henan + shaanxi + log(constructionPC)
model4 <- log(events_PC_wick) ~ log(weibo2013PC)+log(immigration) + log(urbanization_rate)  + log(savings_PC)  +log(natgrwthrate15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif) + log_unemp+henan + shaanxi + log(housing_investPC)
model5 <- log(events_PC_wick) ~ log(weibo2013PC)+log(immigration) + log(urbanization_rate)  + log(savings_PC)  +log(natgrwthrate15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif) + log_unemp+henan + shaanxi + log(housing_investPC) + log(constructionPC)

#OLS
lm1 <- lm(model1, data = protest_geo_merge)
lm2 <- lm(model2, data = protest_geo_merge)
lm3 <- lm(model3, data = protest_geo_merge)
lm4 <- lm(model4, data = protest_geo_merge)
lm5 <- lm(model5, data = protest_geo_merge)
# Spatial Durbin Model (SDM)
SDM1=lagsarlm(model1, data=protest_geo_merge,lw_nowest, type="mixed")
SDM1_summ <- summary(impacts(SDM1,listw=lw_nowest,R=100),zstats=TRUE) 
SDM2=lagsarlm(model2, data=protest_geo_merge,lw_nowest, type="mixed")
SDM2_summ <- summary(impacts(SDM2,listw=lw_nowest,R=100),zstats=TRUE) 
SDM3=lagsarlm(model3, data=protest_geo_merge,lw_nowest, type="mixed")
SDM3_summ <- summary(impacts(SDM3,listw=lw_nowest,R=100),zstats=TRUE)
SDM4=lagsarlm(model4, data=protest_geo_merge,lw_nowest, type="mixed")
SDM4_summ <- summary(impacts(SDM4,listw=lw_nowest,R=100),zstats=TRUE) 
SDM5=lagsarlm(model5, data=protest_geo_merge,lw_nowest, type="mixed")
SDM5_summ <- summary(impacts(SDM5,listw=lw_nowest,R=100),zstats=TRUE) 
#Spatial Durbin Error Model (SDEM)
SDEM1=errorsarlm(model1, data=protest_geo_merge, lw_nowest, etype="emixed")
SDEM2=errorsarlm(model2, data=protest_geo_merge, lw_nowest, etype="emixed")
SDEM3=errorsarlm(model3, data=protest_geo_merge, lw_nowest, etype="emixed")
SDEM4=errorsarlm(model4, data=protest_geo_merge, lw_nowest, etype="emixed")
SDEM5=errorsarlm(model5, data=protest_geo_merge, lw_nowest, etype="emixed")

#########################################################################################################
#Testing the Validity of the Models
#########################################################################################################

#BP test
#########################################################################################################
bptest(lm1,studentize=TRUE)
bptest(lm2,studentize=TRUE)
bptest(lm3,studentize=TRUE)
bptest(lm4,studentize=TRUE)
bptest(lm5,studentize=TRUE)
bptest.sarlm(SDM1,studentize=TRUE)
bptest.sarlm(SDM2,studentize=TRUE)
bptest.sarlm(SDM3,studentize=TRUE)
bptest.sarlm(SDM4,studentize=TRUE)
bptest.sarlm(SDM5,studentize=TRUE)
bptest.sarlm(SDEM1,studentize=TRUE)
bptest.sarlm(SDEM2,studentize=TRUE)
bptest.sarlm(SDEM3,studentize=TRUE)
bptest.sarlm(SDEM4,studentize=TRUE)
bptest.sarlm(SDEM5,studentize=TRUE)

#RMSE
#########################################################################################################
RMSE(lm1$fitted.values,log(protest_geo_merge$events_PC_wick)) 
RMSE(lm2$fitted.values,log(protest_geo_merge$events_PC_wick)) 
RMSE(lm3$fitted.values,log(protest_geo_merge$events_PC_wick)) 
RMSE(lm4$fitted.values,log(protest_geo_merge$events_PC_wick)) 
RMSE(lm5$fitted.values,log(protest_geo_merge$events_PC_wick)) 

#########################################################################################################
#Monte Carlo simulation of Moran I on residuals -- with 600 simulations
moran.mc(log(protest_geo_merge$events_PC_wick), lw_nowest, nsim=599, zero.policy=TRUE)
####################################################################################
#multicollinearity
vif(lm1)
vif(lm2)
vif(lm3)
vif(lm4)
vif(lm5)
