

#Econometric Analysis of Mass Incidents in China


#packages
library("tidyverse")
library(dplyr)
#install.packages("dplyr", dependencies = TRUE)
library(ggplot2)
library("leaflet")
library("htmlwidgets")
library("rgdal")
library("tigris")#error
#install.packages("tigris", dependencies = TRUE)
library(sf)#error
#install.packages("sf", dependencies = TRUE)
library(scales)
library(gridExtra)
library(maptools)
#install.packages("ggspatial",dependencies = TRUE)
library(ggspatial) #error
#install.packages("spdep", dependencies = TRUE)
#install.packages("RColorBrewer", dependencies = TRUE)
library(spdep) #error
library(RColorBrewer)
#install.packages("gdal")
#install.packages("ggmap")
library(ggmap)
library(dummies) 
library(plyr)
library(foreign) #read.dbf
library(knitr)
library(RWordPress)#not available
library(rgeos)# from te mapping china part
library(gpclib) #from the mapping china part
library(data.table)
library(maptools)
library(plm)
library(lmtest)
library(AER)
library(tmap)
library(spatialreg)

#import the files
july23Bcsv <- read.csv("ALL15_merge_sub@dataB.csv", encoding = "utf-8")
gadm36_CHN_2 <- readOGR(dsn = "/Users/phillipcecil/Documents/Masters Econ/thesis/china/china spatial/gadm/gadm36_CHN_shp/prefectures", layer = "gadm36_CHN_2")

wicked_merge_no_missingB_urb_sub <- subset(wicked_merge_no_missingB, !is.na(wicked_merge_no_missingB$urb_rur_wage_dif)==T)
wicked_merge_no_missingB_urb_sub_house_sub <- subset(wicked_merge_no_missingB_urb_sub, !is.na(wicked_merge_no_missingB_urb_sub$住宅投资15PC)==T)
nrow(wicked_merge_no_missingB_urb_sub)
nbBB <- poly2nb(wicked_merge_no_missingB_urb_sub, queen=TRUE)
lw_wickBB <- nb2listw(nbBB, style="W")
nbBC <- poly2nb(wicked_merge_no_missingB_urb_sub_house_sub, queen=TRUE)
lw_wickBC <- nb2listw(nbBC, style="W")


regwick7B <- log(wickedpop) ~ log(weibo2013PC)+log(newhukou_no_out) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital + L年末城镇失业15_all_PCnew*shaanxi + L年末城镇失业15_all_PCnew*henan
regwick7BB <- log(wickedpop) ~ log(weibo2013PC)+log(newhukou_no_out) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  + I(urb_rur_wage_dif^(1/3))
regwick7BB <- log(wickedpop) ~ log(weibo2013PC)+log(newhukou_no_out) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  + scale(urb_rur_wage_dif)
regwick7BC <- log(wickedpop) ~ log(weibo2013PC)+log(newhukou_no_out) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif) + henan + shaanxi
regwick7BD <- log(wickedpop) ~ log(weibo2013PC)+log(newhukou_no_out) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif) + henan + shaanxi+guangdong +fujian+heilongjiang+jiangsu
regwick7BE <- log(wickedpop) ~ log(weibo2013PC)+log(newhukou_no_out) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif) + L年末城镇失业15_all_PCnew+henan + shaanxi+guangdong +fujian+heilongjiang+jiangsu
regwick7BF <- log(wickedpop) ~ log(weibo2013PC)+log(newhukou_no_out) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif) + L年末城镇失业15_all_PCnew+henan + shaanxi
regwick7BG <- log(wickedpop) ~ log(weibo2013PC)+log(newhukou_no_out) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif) + L年末城镇失业15_all_PCnew+henan + shaanxi + log(jianzhuPCnew_pop)
regwick7BH <- log(wickedpop) ~ log(weibo2013PC)+log(newhukou_no_out) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif) + L年末城镇失业15_all_PCnew+henan + shaanxi + log(zhizaoPCnew_pop)
regwick7BI <- log(wickedpop) ~ log(weibo2013PC)+log(newhukou_no_out) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif) + L年末城镇失业15_all_PCnew+henan + shaanxi + log(住宅投资15PC)
regwick7BJ <- log(wickedpop) ~ log(weibo2013PC)+log(newhukou_no_out) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif) + L年末城镇失业15_all_PCnew+henan + shaanxi + log(numb_doctorPC)
regwick7BK <- log(wickedpop) ~ log(weibo2013PC)+log(newhukou_no_out) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif) + L年末城镇失业15_all_PCnew+henan + shaanxi + log(第三产业GDP_perc)
regwick7BL <- log(wickedpop) ~ log(weibo2013PC)+log(newhukou_no_out) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif) + L年末城镇失业15_all_PCnew+henan + shaanxi + log(住宅投资15PC) + log(jianzhuPCnew_pop)
regwick7BM <- log(wickedpop) ~ log(weibo2013PC)+log(newhukou_no_out) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif_no_out) + L年末城镇失业15_all_PCnew+henan + shaanxi + log(住宅投资15PC) + log(jianzhuPCnew_pop)
regwick7BN <- log(wickedpop) ~ log(weibo2013PC)+log(newhukou_no_out) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  +urb_rur_wage_dif_no_out + L年末城镇失业15_all_PCnew+henan + shaanxi + log(住宅投资15PC) + log(jianzhuPCnew_pop)
regwick7BO <- log(wickedpop) ~ log(weibo2013PC)+log(reverse_hukou) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  +urb_rur_wage_dif_no_out + L年末城镇失业15_all_PCnew+henan + shaanxi + log(住宅投资15PC) + log(jianzhuPCnew_pop)
regwick7BP <- log(wickedpop) ~ log(weibo2013PC)+log(reverse_hukou) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif_no_out) + L年末城镇失业15_all_PCnew+henan + shaanxi + log(住宅投资15PC) 
regwick7BQ <- log(wickedpop) ~ log(weibo2013PC)+log(reverse_hukou):notethnic + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif_no_out) + L年末城镇失业15_all_PCnew+henan + shaanxi + log(住宅投资15PC) 
regwick7BR <- log(wickedpop) ~ log(weibo2013PC)+log(reverse_hukou):shanxi +log(reverse_hukou):heilongjiang+log(reverse_hukou):yunnan+ log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital  +scale(urb_rur_wage_dif_no_out) + L年末城镇失业15_all_PCnew+henan + shaanxi + log(住宅投资15PC) 
regwick7BS <- log(wickedpop) ~ log(weibo2013PC)+ log(urbanization_rate) + log(reverse_hukou):yunnan+ log(城乡居民储蓄年末余额15PC)  +log(natgrwthrate_all_15 + 6.44) + 
  prov_capital
regwick7BS <- log(wickedpop) ~ log(weibo2013PC)+ log(urbanization_rate) + log(reverse_hukou):prov_capital
regwick7BT <- log(wickedpop) ~ log(weibo2013PC)+ log(reverse_hukou)
regwick7BU <- log(wickedpop) ~ log(weibo2013PC)+log(newhukou_no_out) + log(urbanization_rate)  + log(城乡居民储蓄年末余额15PC)  +log(GDPPC2015) + 
  prov_capital  +scale(urb_rur_wage_dif) + L年末城镇失业15_all_PCnew+henan + shaanxi + log(住宅投资15PC) + log(jianzhuPCnew_pop)


which(scale(wicked_merge_no_missingB_urb_sub$urb_rur_wage_dif)> 1)
wicked_merge_no_missingB_urb_sub$urb_rur_wage_dif_no_out <- wicked_merge_no_missingB_urb_sub$urb_rur_wage_dif
wicked_merge_no_missingB_urb_sub$urb_rur_wage_dif[which(scale(wicked_merge_no_missingB_urb_sub$urb_rur_wage_dif)> 1)]
wicked_merge_no_missingB_urb_sub$urb_rur_wage_dif_no_out[which(scale(wicked_merge_no_missingB_urb_sub$urb_rur_wage_dif)> 4)] <- 58168


wicked_merge_no_missingB_urb_sub_house_sub$urb_rur_wage_dif_no_out <- wicked_merge_no_missingB_urb_sub_house_sub$urb_rur_wage_dif
wicked_merge_no_missingB_urb_sub_house_sub$urb_rur_wage_dif_no_out[which(scale(wicked_merge_no_missingB_urb_sub_house_sub$urb_rur_wage_dif)> 4)] <- 58168

wicked_merge_no_missingB$urb_rur_wage_dif_no_out<- wicked_merge_no_missingB$urb_rur_wage_dif
wicked_merge_no_missingB$urb_rur_wage_dif_no_out[which(scale(wicked_merge_no_missingB$urb_rur_wage_dif)> 4)] <- 58168

wicked_merge_no_missing$urb_rur_wage_dif_no_out<- wicked_merge_no_missing$urb_rur_wage_dif
wicked_merge_no_missing$urb_rur_wage_dif_no_out[which(scale(wicked_merge_no_missing$urb_rur_wage_dif)> 4)] <- 58168


wicked_merge_no_missingB_urb_sub$urb_rur_wage_dif_no_out[which(scale(wicked_merge_no_missingB_urb_sub$urb_rur_wage_dif)> 4)] <- 58168

plot(density(wicked_merge_no_missingB_urb_sub$urb_rur_wage_dif,na.rm=T))
#foreign_invPC + numb_doctorPC + numb_hospPC +第一产业GDP_perc + 第二产业GDP_perc+第三产业GDP_perc +totalwater_amount_shixiaqu_15_PC + 住宅投资15PC
wicked_merge_no_missingB_urb_sub$newhukou_no_out[1:10]
wicked_merge_no_missingB_urb_sub$reverse_hukou <- wicked_merge_no_missingB_urb_sub$imputed_CZRK15 / wicked_merge_no_missingB_urb_sub$HJRK2015
wicked_merge_no_missingB_urb_sub$reverse_hukou[1:10]
boxplot(wicked_merge_no_missingB_urb_sub$reverse_hukou)
max(wicked_merge_no_missingB_urb_sub$reverse_hukou)
min(wicked_merge_no_missingB_urb_sub$reverse_hukou)
wicked_merge_no_missingB_urb_sub$reverse_hukou[which(scale(wicked_merge_no_missingB_urb_sub$reverse_hukou) > 7)] <- 3.078319
wicked_merge_no_missingB_urb_sub$reverse_hukou[which(scale(wicked_merge_no_missingB_urb_sub$reverse_hukou) < -2)]

wicked_merge_no_missingB_urb_sub_house_sub$reverse_hukou <- wicked_merge_no_missingB_urb_sub_house_sub$imputed_CZRK15 / wicked_merge_no_missingB_urb_sub_house_sub$HJRK2015
wicked_merge_no_missingB_urb_sub_house_sub$reverse_hukou[which(scale(wicked_merge_no_missingB_urb_sub_house_sub$reverse_hukou) > 7)] <- 3.078319

plot(wicked_merge_no_missingB_urb_sub_house_sub$Lwickedpop~log(wicked_merge_no_missingB_urb_sub_house_sub$reverse_hukou))
wicked_merge_no_missing$reverse_hukou <- wicked_merge_no_missing$imputed_CZRK15 / wicked_merge_no_missing$HJRK2015
wicked_merge_no_missing$reverse_hukou[which(scale(wicked_merge_no_missing$reverse_hukou) > 4)] <- 2.067839
wicked_merge_no_missingB$reverse_hukou <- wicked_merge_no_missingB$imputed_CZRK15 / wicked_merge_no_missingB$HJRK2015
wicked_merge_no_missingB$reverse_hukou[which(scale(wicked_merge_no_missingB$reverse_hukou) > 4)] <- 2.067839

wicked_merge_no_missingB_urb_sub_house_sub$notethnic <- 0
wicked_merge_no_missingB_urb_sub_house_sub$notethnic[which(wicked_merge_no_missingB_urb_sub_house_sub$province.x.y != "云南省" & wicked_merge_no_missingB_urb_sub_house_sub$province.x.y != "内蒙古自治区" &
                                                             wicked_merge_no_missingB_urb_sub_house_sub$province.x.y != "黑龙江" & wicked_merge_no_missingB_urb_sub_house_sub$province.x.y != "甘肃省" )] <- 1
wicked_merge_no_missingB_urb_sub_house_sub$yunnan <- 0
wicked_merge_no_missingB_urb_sub_house_sub$yunnan[which(wicked_merge_no_missingB_urb_sub_house_sub$province.x.y == "云南省")] <- 1
unique(wicked_merge_no_missingB$province.x.y)
wicked_merge_no_missingB_urb_sub_house_sub_noyun <- subset(wicked_merge_no_missingB_urb_sub_house_sub,wicked_merge_no_missingB_urb_sub_house_sub$province.x.y != "云南省" )
wicked_merge_no_missingB_urb_sub_house_sub_noyun_nosichuan_noheil_nojilin <- subset(wicked_merge_no_missingB_urb_sub_house_sub, wicked_merge_no_missingB_urb_sub_house_sub$province.x.y != "云南省" &
                                                                                      wicked_merge_no_missingB_urb_sub_house_sub$province.x.y != "黑龙江" & wicked_merge_no_missingB_urb_sub_house_sub$province.x.y != "四川省" &
                                                                                      wicked_merge_no_missingB_urb_sub_house_sub$province.x.y != "吉林省")
write.csv(ALL15_merge@data,file = "ALL15_merge_aug4.csv",row.names = FALSE)
ALL15_merge$reverse_hukou
#######################
#OLS
lm_regwick7BB <- lm(regwick7BB, data = wicked_merge_no_missingB_urb_sub)
lm_regwick7BC <- lm(regwick7BC, data = wicked_merge_no_missingB_urb_sub)
lm_regwick7BD <- lm(regwick7BD, data = wicked_merge_no_missingB_urb_sub)#weibo vif 6
lm_regwick7BE <- lm(regwick7BE, data = wicked_merge_no_missingB_urb_sub)#weibo vif 6
lm_regwick7BF <- lm(regwick7BF, data = wicked_merge_no_missingB_urb_sub)
lm_regwick7BG <- lm(regwick7BG, data = wicked_merge_no_missingB_urb_sub)
lm_regwick7BH <- lm(regwick7BH, data = wicked_merge_no_missingB_urb_sub)#urbanization vif 5
lm_regwick7BI <- lm(regwick7BI, data = wicked_merge_no_missingB_urb_sub_house_sub)
lm_regwick7BJ <- lm(regwick7BJ, data = wicked_merge_no_missingB_urb_sub)
lm_regwick7BK <- lm(regwick7BK, data = wicked_merge_no_missingB_urb_sub)
lm_regwick7BL <- lm(regwick7BL, data = wicked_merge_no_missingB_urb_sub_house_sub)
lm_regwick7BM <- lm(regwick7BM, data = wicked_merge_no_missingB_urb_sub_house_sub)
lm_regwick7BN <- lm(regwick7BN, data = wicked_merge_no_missingB_urb_sub_house_sub)
lm_regwick7BO <- lm(regwick7BO, data = wicked_merge_no_missingB_urb_sub_house_sub)
lm_regwick7BP <- lm(regwick7BP, data = wicked_merge_no_missingB_urb_sub_house_sub_hukousub)
lm_regwick7BQ <- lm(regwick7BQ, data = wicked_merge_no_missingB_urb_sub_house_sub)
lm_regwick7BR <- lm(regwick7BR, data = wicked_merge_no_missingB_urb_sub_house_sub)
lm_regwick7BS <- lm(regwick7BS, data = wicked_merge_no_missingB_urb_sub_house_sub_noyun_nosichuan_noheil_nojilin)
lm_regwick7BT <- lm(regwick7BT, data = wicked_merge_no_missingB_urb_sub_house_sub_noyun_nosichuan_noheil_nojilin)
lm_regwick7BU <- lm(regwick7BU, data = wicked_merge_no_missingB_urb_sub_house_sub_noyun_nosichuan_noheil_nojilin)
summary(lm_regwick7BL)

0.57166588+ 2*0.19422884#0.9601236
0.57166588- 2*0.19422884#0.1832082

library(coefplot)
coef_lm_regwick7BL <- coefplot(lm_regwick7BL)

wicked_merge_no_missingB_urb_sub_house_sub_hukousub <- subset(wicked_merge_no_missingB_urb_sub_house_sub, wicked_merge_no_missingB_urb_sub_house_sub$notethnic==1 &wicked_merge_no_missingB_urb_sub_house_sub$province.x.y!= "吉林省" )

nrow(wicked_merge_no_missingB_urb_sub_house_sub_hukousub)
lm_regwick7BP$coefficients
summary(lm_regwick7BU)
vif(lm_regwick7BM)
which(is.na(wicked_merge_no_missingB_urb_sub$住宅投资15PC))
wicked_merge_no_missingB_urb_sub
wicked_merge_no_missingB_urb_sub_house_sub
wicked_merge_no_missingB
wicked_merge_no_missing
mean(wicked_merge_no_missingB_urb_sub_house_sub$Freq)
sum(wicked_merge_no_missingB_urb_sub_house_sub$Freq)
wicked_merge_no_missingB_urb_sub_house_sub$city[which()]
################
#SDM
SDM_wickB7BB=lagsarlm(regwick7BB, data=wicked_merge_no_missingB_urb_sub,lw_wickBB, type="mixed")#AIC 300.51 Rho 0.38801
SDM_wickB7BC=lagsarlm(regwick7BC, data=wicked_merge_no_missingB_urb_sub,lw_wickBB, type="mixed")#AIC 277.31 Rho 0.25389  ----
SDM_wickB7BD=lagsarlm(regwick7BD, data=wicked_merge_no_missingB_urb_sub,lw_wickBB, type="mixed")#AIC 286.65 Rho 0.23
SDM_wickB7BE=lagsarlm(regwick7BE, data=wicked_merge_no_missingB_urb_sub,lw_wickBB, type="mixed")#AIC 287.69 Rho 0.21303
SDM_wickB7BF=lagsarlm(regwick7BF, data=wicked_merge_no_missingB_urb_sub,lw_wickBB, type="mixed")#AIC 278.38 Rho 0.24004
SDM_wickB7BG=lagsarlm(regwick7BG, data=wicked_merge_no_missingB_urb_sub,lw_wickBB, type="mixed")#AIC 276.39 Rho 0.24739 ----
SDM_wickB7BH=lagsarlm(regwick7BH, data=wicked_merge_no_missingB_urb_sub,lw_wickBB, type="mixed")#AIC 281.64 Rho 0.2372
SDM_wickB7BI=lagsarlm(regwick7BI, data=wicked_merge_no_missingB_urb_sub_house_sub,lw_wickBC, type="mixed")#AIC 273.42  Rho 0.25994 ----
SDM_wickB7BJ=lagsarlm(regwick7BJ, data=wicked_merge_no_missingB_urb_sub,lw_wickBB, type="mixed")#AIC 280.03 Rho 0.24025
SDM_wickB7BK=lagsarlm(regwick7BK, data=wicked_merge_no_missingB_urb_sub,lw_wickBB, type="mixed")#AIC 280.65 Rho 0.23978
SDM_wickB7BL=lagsarlm(regwick7BL, data=wicked_merge_no_missingB_urb_sub_house_sub,lw_wickBC, type="mixed")#AIC 268.83  Rho 0.26501  ----
SDM_wickB7BM=lagsarlm(regwick7BM, data=wicked_merge_no_missingB_urb_sub_house_sub,lw_wickBC, type="mixed")#AIC: 270.66, (AIC for lm: 281.43)#Rho: 0.27853, LR test value: 12.762, p-value: 0.00035376
SDM_wickB7BN=lagsarlm(regwick7BN, data=wicked_merge_no_missingB_urb_sub_house_sub,lw_wickBC, type="mixed")#AIC: 270.66, (AIC for lm: 281.43)#Rho: 0.27853, LR test value: 12.762, p-value: 0.00035376
SDM_wickB7BP=lagsarlm(regwick7BP, data=wicked_merge_no_missingB_urb_sub_house_sub,lw_wickBC, type="mixed")#AIC: 270.66, (AIC for lm: 281.43)#Rho: 0.27853, LR test value: 12.762, p-value: 0.00035376
summary(SDM_wickB7BP)


#C G  I L

summary(SDM_wickB7BM)
which(is.na(wicked_merge_no_missingB_urb_sub$caijuePCnew))
SDM_wickB7BB_summ <- summary(impacts(SDM_wickB7BB,listw=lw_wickBB,R=100),zstats=TRUE) 
SDM_wickB7BC_summ <- summary(impacts(SDM_wickB7BC,listw=lw_wickBB,R=100),zstats=TRUE) 
SDM_wickB7BD_summ <- summary(impacts(SDM_wickB7BD,listw=lw_wickBB,R=100),zstats=TRUE) 
SDM_wickB7BE_summ <- summary(impacts(SDM_wickB7BE,listw=lw_wickBB,R=100),zstats=TRUE) 
SDM_wickB7BF_summ <- summary(impacts(SDM_wickB7BF,listw=lw_wickBB,R=100),zstats=TRUE) 
SDM_wickB7BG_summ <- summary(impacts(SDM_wickB7BG,listw=lw_wickBB,R=100),zstats=TRUE) #constr. sig
SDM_wickB7BH_summ <- summary(impacts(SDM_wickB7BH,listw=lw_wickBB,R=100),zstats=TRUE) #manuf not sig
SDM_wickB7BI_summ <- summary(impacts(SDM_wickB7BI,listw=lw_wickBC,R=100),zstats=TRUE)#housing inves sig 
SDM_wickB7BJ_summ <- summary(impacts(SDM_wickB7BJ,listw=lw_wickBB,R=100),zstats=TRUE) #doctor not sig
SDM_wickB7BK_summ <- summary(impacts(SDM_wickB7BK,listw=lw_wickBB,R=100),zstats=TRUE) #service industry not sig
SDM_wickB7BL_summ <- summary(impacts(SDM_wickB7BL,listw=lw_wickBC,R=100),zstats=TRUE) #constr. and housing sig
SDM_wickB7BM_summ <- summary(impacts(SDM_wickB7BM,listw=lw_wickBC,R=100),zstats=TRUE) #
SDM_wickB7BN_summ <- summary(impacts(SDM_wickB7BN,listw=lw_wickBC,R=100),zstats=TRUE) #
SDM_wickB7BP_summ <- summary(impacts(SDM_wickB7BP,listw=lw_wickBC,R=100),zstats=TRUE) #

SDM_wickB7SDM_wickB7BI_summ <- summary(impacts(SDM_wickB7BI,listw=lw_wickBB,R=100),zstats=TRUE) 
BI_summ <- summary(impacts(SDM_wickB7BI,listw=lw_wickBB,R=100),zstats=TRUE) 

0.28945143*2 + 0.8646201099 #1.443523
0.28945143*2 - 0.8646201099#-0.2857172

0.8646201099 + 0.28945143*2 #1.443523
0.8646201099 - 0.28945143*2 #0.2857172


#0.28945143 - SE
#0.8646201099 - estimate
##################
#SDEM
SDEM_wick7BB=errorsarlm(regwick7BB, data=wicked_merge_no_missingB_urb_sub, lw_wickBB, etype="emixed")#302.84
SDEM_wick7BC=errorsarlm(regwick7BC, data=wicked_merge_no_missingB_urb_sub, lw_wickBB, etype="emixed")#279.17
SDEM_wick7BD=errorsarlm(regwick7BD, data=wicked_merge_no_missingB_urb_sub, lw_wickBB, etype="emixed")#289.2
SDEM_wick7BE=errorsarlm(regwick7BE, data=wicked_merge_no_missingB_urb_sub, lw_wickBB, etype="emixed")#290.32
SDEM_wick7BF=errorsarlm(regwick7BF, data=wicked_merge_no_missingB_urb_sub, lw_wickBB, etype="emixed")#280.31
SDEM_wick7BG=errorsarlm(regwick7BG, data=wicked_merge_no_missingB_urb_sub, lw_wickBB, etype="emixed")#278.19
SDEM_wick7BH=errorsarlm(regwick7BH, data=wicked_merge_no_missingB_urb_sub, lw_wickBB, etype="emixed")#283.43
SDEM_wick7BI=errorsarlm(regwick7BI, data=wicked_merge_no_missingB_urb_sub, lw_wickBB, etype="emixed")#275.58
SDEM_wick7BJ=errorsarlm(regwick7BJ, data=wicked_merge_no_missingB_urb_sub, lw_wickBB, etype="emixed")#281.99
SDEM_wick7BK=errorsarlm(regwick7BK, data=wicked_merge_no_missingB_urb_sub, lw_wickBB, etype="emixed")#282.42
SDEM_wick7BL=errorsarlm(regwick7BL, data=wicked_merge_no_missingB_urb_sub_house_sub, lw_wickBC, etype="emixed")#270.78
SDEM_wick7BM=errorsarlm(regwick7BM, data=wicked_merge_no_missingB_urb_sub_house_sub, lw_wickBC, etype="emixed")#272.48
summary(SDEM_wick7BM)
SDEM_wick7BL_summ <- summary(impacts(SDEM_wick7BL,listw=lw_wickBC,R=100),zstats=TRUE) #AIC: 270.78, (AIC for lm: 278.26)
SDEM_wick7BL_summary <- summary(SDEM_wick7BL)

coefplot(lkjftdresrateghfgc$Coef)
#SDEM_wick7BL_summ$pzmat
#SDEM_wick7BL_summ$mat


SDEM_wick7BL_summary <- summary(SDEM_wick7BL)
SDEM_wick7BL_summary
#BP test
bptest.sarlm(SDM_wickB7BB,studentize=TRUE)#0.05623
bptest.sarlm(SDM_wickB7BC,studentize=TRUE)#0.09084
bptest.sarlm(SDM_wickB7BD,studentize=TRUE)#0.3161
bptest.sarlm(SDM_wickB7BE,studentize=TRUE)#0.4095
bptest.sarlm(SDM_wickB7BF,studentize=TRUE)#0.1561
bptest.sarlm(SDM_wickB7BG,studentize=TRUE)#0.2886
bptest.sarlm(SDM_wickB7BH,studentize=TRUE)#0.1585
bptest.sarlm(SDM_wickB7BI,studentize=TRUE)#0.1659
bptest.sarlm(SDM_wickB7BJ,studentize=TRUE)#0.2135
bptest.sarlm(SDM_wickB7BK,studentize=TRUE)#0.1333
bptest.sarlm(SDM_wickB7BL,studentize=TRUE)#0.3212
bptest.sarlm(SDEM_wick7BL,studentize=TRUE)#0.3364
bptest(lm_regwick7BL)
#####################
#RMSE
#OLS
RMSE(lm_regwick7BL$fitted.values,log(wicked_merge_no_missingB_urb_sub_house_sub$wickedpop)) #0.3919919 ---
summary(lm_regwick7BL)

#SDM
RMSE(SDM_wickB7BB$fitted.values,log(wicked_merge_no_missingB_urb_sub$wickedpop)) #0.3912448
RMSE(SDM_wickB7BC$fitted.values,log(wicked_merge_no_missingB_urb_sub$wickedpop)) #0.3731202
RMSE(SDM_wickB7BD$fitted.values,log(wicked_merge_no_missingB_urb_sub$wickedpop)) #0.3690101
RMSE(SDM_wickB7BE$fitted.values,log(wicked_merge_no_missingB_urb_sub$wickedpop)) #0.3673077  ----
RMSE(SDM_wickB7BF$fitted.values,log(wicked_merge_no_missingB_urb_sub$wickedpop)) #0.3713903
RMSE(SDM_wickB7BG$fitted.values,log(wicked_merge_no_missingB_urb_sub$wickedpop)) #0.3670825  ----
RMSE(SDM_wickB7BH$fitted.values,log(wicked_merge_no_missingB_urb_sub$wickedpop)) #0.3709371
RMSE(SDM_wickB7BI$fitted.values,log(wicked_merge_no_missingB_urb_sub_house_sub$wickedpop)) #0.365326 ---
RMSE(SDM_wickB7BJ$fitted.values,log(wicked_merge_no_missingB_urb_sub$wickedpop)) #0.3697513
RMSE(SDM_wickB7BK$fitted.values,log(wicked_merge_no_missingB_urb_sub$wickedpop)) #0.3701926
RMSE(SDM_wickB7BL$fitted.values,log(wicked_merge_no_missingB_urb_sub_house_sub$wickedpop)) #0.3593559 ---
#E G I L 
#SDEM
RMSE(SDEM_wick7BC$fitted.values,log(wicked_merge_no_missingB_urb_sub$wickedpop))#0.3746469
RMSE(SDEM_wick7BE$fitted.values,log(wicked_merge_no_missingB_urb_sub$wickedpop))#0.369578
RMSE(SDEM_wick7BG$fitted.values,log(wicked_merge_no_missingB_urb_sub$wickedpop))#0.3684577
RMSE(SDEM_wick7BI$fitted.values,log(wicked_merge_no_missingB_urb_sub_house_sub$wickedpop))#0.3670825
RMSE(SDEM_wick7BL$fitted.values,log(wicked_merge_no_missingB_urb_sub_house_sub$wickedpop)) #0.360846 ---
###########################
#AIC

###############################
#Log Likelihood
summary(SDM_wickB7BB)#-133.2537
summary(SDM_wickB7BC)#-117.655
summary(SDM_wickB7BD)#-114.3229
summary(SDM_wickB7BE)#-112.8471 ---
summary(SDM_wickB7BF)#-116.1919
summary(SDM_wickB7BG)#-113.1931  ----
summary(SDM_wickB7BH)#-115.8221
summary(SDM_wickB7BI)#-111.7094 ---
summary(SDM_wickB7BJ)#-115.0143
summary(SDM_wickB7BK)#-115.3254
summary(SDM_wickB7BL)#-107.4139 ----
summary(SDEM_wick7BL)#-108.3898
logLik(lm_regwick7BL)#-128.3249
#E G I L
summary(SDEM_wick7BC)#Log likelihood: -118.585 for error model
summary(SDEM_wick7BE)#Log likelihood: -114.1608 for error model
summary(SDEM_wick7BG)#Log likelihood: -114.097 for error model
summary(SDEM_wick7BI)#Log likelihood: -112.7906 for error model
summary(SDEM_wick7BL)#Log likelihood: -108.3898 for error model


#Monte Carlo simulation of Moran I on residuals -- with 600 simulations
wicked_merge_no_missingB_urb_sub
wicked_merge_no_missingB_urb_sub_house_sub
wicked_merge_no_missingB
wicked_merge_no_missing
moran.mc(wicked_merge_no_missingB_urb_sub$Lwickedpop, lw_wickBB, nsim=599, zero.policy=TRUE)#statistic = 0.27962, observed rank = 600, p-value = 0.001667
moran.mc(wicked_merge_no_missingB_urb_sub_house_sub$Lwickedpop, lw_wickBC, nsim=599, zero.policy=TRUE)#statistic = 0.2859, observed rank = 600, p-value = 0.001667
moran.mc(wicked_merge_no_missingB$Lwickedpop, lw_wickB, nsim=599, zero.policy=TRUE)#statistic = 0.28196, observed rank = 600, p-value = 0.001667
moran.mc(wicked_merge_no_missing$Lwickedpop, lw_wick, nsim=599, zero.policy=TRUE)#statistic = 0.27122, observed rank = 600, p-value = 0.001667


moran.mc(lm_regwick7BL$residuals, lw_wickBC, nsim=599, zero.policy=TRUE)#statistic = 0.17758, observed rank = 600, p-value = 0.001667

moran.mc(SDM_wickB7BC$residuals, lw_wickBB, nsim=599, zero.policy=TRUE)#statistic = -0.023466, observed rank = 204, p-value = 0.66
moran.mc(SDM_wickB7BE$residuals, lw_wickBB, nsim=599, zero.policy=TRUE)#statistic = -0.029038, observed rank = 154, p-value = 0.7433
moran.mc(SDM_wickB7BG$residuals, lw_wickBB, nsim=599, zero.policy=TRUE)#statistic = -0.020616, observed rank = 217, p-value = 0.6383
moran.mc(SDM_wickB7BI$residuals, lw_wickBC, nsim=599, zero.policy=TRUE)#statistic = -0.024644, observed rank = 193, p-value = 0.6783
moran.mc(SDM_wickB7BL$residuals, lw_wickBC, nsim=599, zero.policy=TRUE)#statistic = -0.02133, observed rank = 215, p-value = 0.6417

moran.mc(SDEM_wick7BC$residuals, lw_wickBB, nsim=599, zero.policy=TRUE)#statistic = -0.0081556, observed rank = 276, p-value = 0.54
moran.mc(SDEM_wick7BE$residuals, lw_wickBB, nsim=599, zero.policy=TRUE)#statistic = -0.0030302, observed rank = 310, p-value = 0.4833
moran.mc(SDEM_wick7BG$residuals, lw_wickBB, nsim=599, zero.policy=TRUE)#statistic = -0.0055069, observed rank = 291, p-value = 0.515
moran.mc(SDEM_wick7BI$residuals, lw_wickBC, nsim=599, zero.policy=TRUE)#statistic = -0.0073738, observed rank = 285, p-value = 0.525
moran.mc(SDEM_wick7BL$residuals, lw_wickBC, nsim=599, zero.policy=TRUE)#statistic = -0.0062974, observed rank = 279, p-value = 0.535

#LR test -- only works for nested models

LR.sarlm(SDM_wickB7BL, lm_regwick7BL)#Likelihood ratio = 41.822, df = 13, p-value = 7.003e-05
summary(SDM_wickB7BL)

LR.sarlm(SDM_wickB9, spat_wicklm9)#Likelihood ratio = 36.333, df = 11, p-value = 0.0001489
LR.sarlm(SDEM_wick5, spat_wicklm5)#Likelihood ratio = 38.17, df = 13, p-value = 0.000271
LR.sarlm(SDEM_wick6C, spat_wicklm6C)#Likelihood ratio = 37.272, df = 12, p-value = 0.0002018
LR.sarlm(SDEM_wick9, spat_wicklm9)#Likelihood ratio = 26.421, df = 11, p-value = 0.005616

lm.LMtests(lm_regwick7BL,lw_wickBC,test="all") 
reg1LMtest <- lm.LMtests(reg1,lwB,zero.policy = T,test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))

#multicollinearity
vif(lm_regwick7BB)#weibo 3.175056 urban 4.047317 saving 3.663299
vif(lm_regwick7BC)#weibo 3.250539 urban 4.053333 saving 3.685842
vif(lm_regwick7BD)#weibo 6.014332 urban 4.356921 saving 4.057049
vif(lm_regwick7BE)#weibo 6.061459 urban 4.784430 saving 4.068196
vif(lm_regwick7BF)#weibo 3.250564 urban 4.337833 saving 3.686515
vif(lm_regwick7BG)#weibo 3.252180 urban 4.551925 saving 3.815487
vif(lm_regwick7BH)#weibo 3.541386 urban 5.308820 saving 3.688013
vif(lm_regwick7BI)#weibo 3.306793 urban 4.554918 saving 4.106919
vif(lm_regwick7BJ)#weibo 3.282563 urban 4.418104 saving 4.415006
vif(lm_regwick7BK)#weibo 3.251520 urban 4.479515 saving 4.010999
vif(lm_regwick7BL)#weibo 3.307463 urban 4.681237 saving 4.144520
lm_regwick7BL
##########################
#coef plot
coefplot(lm_regwick7BB)
coefplot(lm_regwick7BC)
coefplot(lm_regwick7BD)
coefplot(lm_regwick7BE)
coefplot(lm_regwick7BF)
coefplot(lm_regwick7BG)
coefplot(lm_regwick7BH)
coefplot(lm_regwick7BI)
coefplot(lm_regwick7BJ)
coefplot(lm_regwick7BK)
coefplot(lm_regwick7BL)
asdfsafs <- summary(lm_regwick7BL)

########################
#confidence intervals
asdfsafs$coefficients
confint(lm_regwick7BL)
0.03058551+ 2*0.04488034 #0.1203462
0.03058551- 2*0.04488034 #-0.05917517

newdata <- data.frame(wicked_merge_no_missingB_urb_sub_house_sub$weibo2013PC  )
newdata$newdata <- mean(log(wicked_merge_no_missingB_urb_sub_house_sub$weibo2013PC))
new.dat <- data.frame(weibo = mean( log(wicked_merge_no_missingB_urb_sub_house_sub$weibo2013PC)))
data(wicked_merge_no_missingB_urb_sub_house_sub)
new.dat <- data.frame(weibo = mean( log(wicked_merge_no_missingB_urb_sub_house_sub$weibo2013PC)))
predict(lm_regwick7BL, newdata = new.dat, interval = 'confidence')
length(lm_regwick7BL$fitted.values)
predict(lm_regwick7BL, newdata$newdata, interval="confidence") 
data(cars)
cars.lm <- lm(dist ~ speed, data = cars)
new.dat <- data.frame(speed=30)
predict(cars.lm, newdata = new.dat, interval = 'confidence')

2*0.04488034 + mean(log(wicked_merge_no_missingB_urb_sub_house_sub$weibo2013PC))#-0.430754
2*0.04488034 - mean(log(wicked_merge_no_missingB_urb_sub_house_sub$weibo2013PC))#0.6102754
2*0.04488034 + 0.03058551#0.1203462
2*0.04488034 - 0.03058551#0.05917517

coefplot(summary(m1)$coefficients[,1], summary(m1)$coefficients[,2], vertical=FALSE, varnames=variableNames, ylim=c(-5, 25), main="")
new.dat <- data.frame(speed=30)
#A 95% confidence interval (CI) is twice the standard error (also called margin of error) plus or minus the mean
#2*0.19422884 + 0.01485636 = 0.403314
#2*0.19422884 - 0.0148563 = 0.3736014
mean(log(wicked_merge_no_missingB_urb_sub$newhukou_no_out), na.rm=T)#0.01485636
SDM_wickB7BL
SDM_wickB7BL_summ$semat[1,1]
mean(log(wicked_merge_no_missingB_urb_sub_house_sub$weibo2013PC), na.rm=T)#-0.5205147
mean(log(wicked_merge_no_missingB_urb_sub_house_sub$newhukou_no_out), na.rm=T)#0.01483108
mean(log(wicked_merge_no_missingB_urb_sub_house_sub$urbanization_rate), na.rm=T)#-0.6322683
mean(log(wicked_merge_no_missingB_urb_sub_house_sub$城乡居民储蓄年末余额15PC), na.rm=T)#1.16304
mean(log(wicked_merge_no_missingB_urb_sub_house_sub$natgrwthrate_all_15 + 6.44), na.rm=T)#2.464652
mean(scale(wicked_merge_no_missingB_urb_sub_house_sub$urb_rur_wage_dif),na.rm=T)
#weibo
#2*SDM_wickB7BL_summ$semat[1,1] + -0.5205147 = -0.4009542
#2*SDM_wickB7BL_summ$semat[1,1] - -0.5205147 = 0.6400752
#hukou
#2*SDM_wickB7BL_summ$semat[2,1] + -0.01483108 = 0.4052833
#2*SDM_wickB7BL_summ$semat[2,1] - -0.01483108 = 0.4349455
#urb_rur_wage_dif
2*SDM_wickB7BL_summ$semat[7,1] + 0 = 0.04376688
2*SDM_wickB7BL_summ$semat[7,1] - 0 = 0.04376688
SDM_wickB7BL_summ_estimates <- data.frame(SDM_wickB7BL_summ$res[1])
SDM_wickB7BL_summ_estimates[1,1]
#weibo
W_U <- 2*SDM_wickB7BL_summ$semat[1,1] + mean(log(wicked_merge_no_missingB_urb_sub_house_sub$weibo2013PC), na.rm=T)
W_L <- 2*SDM_wickB7BL_summ$semat[1,1] - mean(log(wicked_merge_no_missingB_urb_sub_house_sub$weibo2013PC), na.rm=T)
W_U <- 2*SDM_wickB7BL_summ$semat[1,1] + SDM_wickB7BL_summ_estimates[1,1]
W_L <- 2*SDM_wickB7BL_summ$semat[1,1] - SDM_wickB7BL_summ_estimates[1,1]
#hukou
H_U <- 2*SDM_wickB7BL_summ$semat[2,1] + SDM_wickB7BL_summ_estimates[2,1]
H_L <- 2*SDM_wickB7BL_summ$semat[2,1] - SDM_wickB7BL_summ_estimates[2,1]
#urbanization_rate
Urb_U <- 2*SDM_wickB7BL_summ$semat[3,1] + SDM_wickB7BL_summ_estimates[3,1]
Urb_L <- 2*SDM_wickB7BL_summ$semat[3,1] - SDM_wickB7BL_summ_estimates[3,1]
#城乡居民储蓄年末余额15PC
S_U <- 2*SDM_wickB7BL_summ$semat[4,1] + SDM_wickB7BL_summ_estimates[4,1]
S_L <- 2*SDM_wickB7BL_summ$semat[4,1] - SDM_wickB7BL_summ_estimates[4,1]
#natgrwthrate_all_15 + 6.44
G_U <- 2*SDM_wickB7BL_summ$semat[5,1] + SDM_wickB7BL_summ_estimates[5,1]
G_L <- 2*SDM_wickB7BL_summ$semat[5,1] - SDM_wickB7BL_summ_estimates[5,1]
#prov_capital
C_U <- 2*SDM_wickB7BL_summ$semat[6,1] + SDM_wickB7BL_summ_estimates[6,1]
C_L <- 2*SDM_wickB7BL_summ$semat[6,1] - SDM_wickB7BL_summ_estimates[6,1]
#urb_rur_wage_dif
UR_U <- 2*SDM_wickB7BL_summ$semat[7,1] + SDM_wickB7BL_summ_estimates[7,1]
UR_L <- 2*SDM_wickB7BL_summ$semat[7,1] - SDM_wickB7BL_summ_estimates[7,1]
#L年末城镇失业15_all_PCnew
Un_U <- 2*SDM_wickB7BL_summ$semat[8,1] + SDM_wickB7BL_summ_estimates[8,1]
Un_L <- 2*SDM_wickB7BL_summ$semat[8,1] - SDM_wickB7BL_summ_estimates[8,1]
#henan
Hen_U <- 2*SDM_wickB7BL_summ$semat[9,1] + SDM_wickB7BL_summ_estimates[9,1]
Hen_L <- 2*SDM_wickB7BL_summ$semat[9,1] - SDM_wickB7BL_summ_estimates[9,1]
#shaanxi
Sha_U <- 2*SDM_wickB7BL_summ$semat[10,1] + SDM_wickB7BL_summ_estimates[10,1]
Sha_L <- 2*SDM_wickB7BL_summ$semat[10,1] - SDM_wickB7BL_summ_estimates[10,1]
#住宅投资15PC
RE_U <- 2*SDM_wickB7BL_summ$semat[11,1] + SDM_wickB7BL_summ_estimates[11,1]
RE_L <- 2*SDM_wickB7BL_summ$semat[11,1] - SDM_wickB7BL_summ_estimates[11,1]
#jianzhuPCnew_pop
Con_U <- 2*SDM_wickB7BL_summ$semat[12,1] + SDM_wickB7BL_summ_estimates[12,1]
Con_L <- 2*SDM_wickB7BL_summ$semat[12,1] - SDM_wickB7BL_summ_estimates[12,1]
SDM_wickB7BL_summ_estimates <- data.frame(SDM_wickB7BL_summ$res[1])
SDM_wickB7BL_summ_SE <- data.frame(SDM_wickB7BL_summ$semat[,1])
SDM_wickB7BL_summ_SE$SDM_wickB7BL_summ.semat...1.
SDM_wickB7BL_upper <- cbind(W_U,H_U,Urb_U,S_U,G_U,C_U,UR_U,Un_U,Hen_U,Sha_U,RE_U,Con_U)
SDM_wickB7BL_lower <- cbind(W_L,H_L,Urb_L,S_L,G_L,C_L,UR_L,Un_L,Hen_L,Sha_L,RE_L,Con_L)
SDM_wickB7BL_confint <- rbind(SDM_wickB7BL_upper,SDM_wickB7BL_lower)
coefplot(SDM_wickB7BL_summ_estimates,SDM_wickB7BL_summ_SE$SDM_wickB7BL_summ.semat...1., vertical=FALSE,  ylim=c(-5, 25), main="")
t.test(SDM_wickB7BL_summ)
onetoten <- 1:12

ggplot(SDM_wickB7BL_summ_estimates, aes(x = onetoten, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = SDM_wickB7BL_upper, ymin = SDM_wickB7BL_lower))
#
#export tables
library(stargazer)
stargazer(SDM_wickB7BL_summ$pzmat,type="html", out="SDM_wickB7BL_summ_pvals.html" )
stargazer(SDM_wickB7BL_summ$res,type="html", out="SDM_wickB7BL_summ$res.html" )
stargazer(SDEM_wick7BL_summary,type="html", out="SDEM_wick7BL_summary.html" )
stargazer(lm_regwick7BL,type="html", out="lm_regwick7BL_summary.html" )
lm_regwick7BL_summary <- summary(lm_regwick7BL)
SDEM_wick7BL_summary$Coef
write.table(SDEM_wick7BL_summary$Coef, file = "SDEM_wick7BL_summary$Coef.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
write.table(lm_regwick7BL_summary$coefficients, file = "lm_regwick7BL_summary$coefficients.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
write.table(SDEM_wick7BL_summ$pzmat, file = "SDEM_wick7BL_summ$pzmat.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
write.table(SDEM_wick7BL_summ$mat, file = "SDEM_wick7BL_summ$mat.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
SDM_wickB7BL_summ$
  SDEM_wick7BL_summ$pzmat
SDEM_wick7BL_summ$mat
stargazer(summary(SDM_wickB5),summary(SDM_wickB6C),summary(SDM_wickB9),type="html", out="SDM_summary.html" )

#lots of SDM outputs - C E  G I L 
stargazer(SDM_wickB7BC_summ$pzmat,SDM_wickB7BE_summ$pzmat,SDM_wickB7BG_summ$pzmat,SDM_wickB7BI_summ$pzmat,SDM_wickB7BL_summ$pzmat,type="html", out="SDM lots of models pvals.html" )
stargazer(SDM_wickB7BC_summ$res,SDM_wickB7BE_summ$res,SDM_wickB7BG_summ$res,SDM_wickB7BI_summ$res,SDM_wickB7BL_summ$res,type="html", out="SDM lots of models estimates.html" )
########################
#exploratory data

attach(wicked_merge_no_missingB@data )
#population
mean(imputed_CZRK15, na.rm=T)#454.08
min(imputed_CZRK15, na.rm=T)#24.39
max(imputed_CZRK15, na.rm=T)#2991.4
sd(imputed_CZRK15, na.rm=T)#341.0956
mean(log(imputed_CZRK15), na.rm=T)#5.902781
min(log(imputed_CZRK15), na.rm=T)#3.194173
max(log(imputed_CZRK15), na.rm=T)#8.003497
sd(log(imputed_CZRK15), na.rm=T)#0.6675791

#wickedpop -- mass incidents per ten thousand people
mean(wickedpop, na.rm=T)#0.1999058
min(wickedpop, na.rm=T)#0.02326934
max(wickedpop, na.rm=T)#0.9189487
sd(wickedpop, na.rm=T)#0.1179941

#protest events recorded by the Wickedonna blog per 10000
mean(Lwickedpop, na.rm=T)# -1.77572
min(Lwickedpop, na.rm=T)# -3.760619
max(Lwickedpop, na.rm=T)# -0.08452496
sd(Lwickedpop, na.rm=T)# 0.6008797

#number of weibo posts from a random sample ( divided by 10000 people)
mean(weibo2013PC, na.rm=T)#1.069877
min(weibo2013PC, na.rm=T)#0.06478782
max(weibo2013PC, na.rm=T)#12.44936
sd(weibo2013PC, na.rm=T)#1.609234
mean(log(weibo2013PC), na.rm=T)#-0.5226362
min(log(weibo2013PC), na.rm=T)#-2.736638
max(log(weibo2013PC), na.rm=T)#2.521669
sd(log(weibo2013PC), na.rm=T)#0.9958613


#registered population divided by long term resident population
mean(newhukou_no_out, na.rm=T)#1.030959
min(newhukou_no_out, na.rm=T)#0.4835965
max(newhukou_no_out, na.rm=T)#1.439629
sd(newhukou_no_out, na.rm=T)#0.1663201
mean(log(newhukou_no_out), na.rm=T)#0.01519403
min(log(newhukou_no_out), na.rm=T)#-0.7265044
max(log(newhukou_no_out), na.rm=T)#0.3643855
sd(log(newhukou_no_out), na.rm=T)#0.1839643

#Reverse Hukou
reverse_hukou
mean(reverse_hukou, na.rm=T)#1.030959
min(reverse_hukou, na.rm=T)#0.6946233
max(reverse_hukou, na.rm=T)#2.067839
sd(reverse_hukou, na.rm=T)#0.2209483
plot(density(reverse_hukou))
mean(log(reverse_hukou), na.rm=T)#-0.01519403
min(log(reverse_hukou), na.rm=T)#-0.3643855
max(log(reverse_hukou), na.rm=T)#0.7265043
sd(log(reverse_hukou), na.rm=T)#0.1839643


#number of internet subscribing households per capita  - china city stastical yearbook
mean(newinternetpop, na.rm=T)#0.1790132
min(newinternetpop, na.rm=T)#0.00625
max(newinternetpop, na.rm=T)#0.8815331
sd(newinternetpop, na.rm=T)#0.1013821
mean(log(newinternetpop), na.rm=T)#-1.838515
min(log(newinternetpop), na.rm=T)#-5.075174
max(log(newinternetpop), na.rm=T)#-0.1260927
sd(log(newinternetpop), na.rm=T)#0.4880708


# Outstanding Amount of Savings Deposit of Urban and Rural Residents at Year-end(100 million yuan) per 10000 people
mean(城乡居民储蓄年末余额15PC, na.rm=T)#3.553081
min(城乡居民储蓄年末余额15PC, na.rm=T)#1.029245
max(城乡居民储蓄年末余额15PC, na.rm=T)#11.01772
sd(城乡居民储蓄年末余额15PC, na.rm=T)#1.719489
mean(log(城乡居民储蓄年末余额15PC), na.rm=T)#1.166654
min(log(城乡居民储蓄年末余额15PC), na.rm=T)#0.02882546
max(log(城乡居民储蓄年末余额15PC), na.rm=T)#2.399505
sd(log(城乡居民储蓄年末余额15PC), na.rm=T)#0.4422387


# Number of Urban Registered Unemployees at Year-end per capita
mean(年末城镇失业15_all_PC, na.rm=T)#0.01167735
min(年末城镇失业15_all_PC, na.rm=T)#0.001377656
max(年末城镇失业15_all_PC, na.rm=T)#0.1554662
sd(年末城镇失业15_all_PC, na.rm=T)#0.0118329
mean(log(年末城镇失业15_all_PC), na.rm=T)#-4.64121
min(log(年末城镇失业15_all_PC), na.rm=T)#-6.587372
max(log(年末城镇失业15_all_PC), na.rm=T)#-1.861327
sd(log(年末城镇失业15_all_PC), na.rm=T)#0.5643309


#GDP - dollars per person
mean(GDPPC2015, na.rm=T)#8107.953
min(GDPPC2015, na.rm=T)#1764
max(GDPPC2015, na.rm=T)#33261
sd(GDPPC2015, na.rm=T)#4721.02
mean(log(GDPPC2015), na.rm=T)#8.857805
min(log(GDPPC2015), na.rm=T)#7.475339
max(log(GDPPC2015), na.rm=T)#10.41214
sd(log(GDPPC2015), na.rm=T)#0.5269067

nrow(wicked15join)

#urbanization_rate
mean(urbanization_rate, na.rm=T)#0.5465381
min(urbanization_rate, na.rm=T)#0.2816
max(urbanization_rate, na.rm=T)#.99
sd(urbanization_rate, na.rm=T)#0.1345237
city[which(urbanization_rate > .99)] #深圳市 
city[which(urbanization_rate < .282)]#陇南市
province.x.y[which(urbanization_rate < .282)]#甘肃省
mean(log(urbanization_rate), na.rm=T)#-0.6327878
min(log(urbanization_rate), na.rm=T)#-1.267268
max(log(urbanization_rate), na.rm=T)#-0.01005034
sd(log(urbanization_rate), na.rm=T)#0.2379687
log(.99)

#GDP growth rate,2014 to 2015 natgrwthrate_all_15
mean(natgrwthrate_all_15, na.rm=T)#6.721355
min(natgrwthrate_all_15, na.rm=T)#-5.44
max(natgrwthrate_all_15, na.rm=T)#27.75
sd(natgrwthrate_all_15, na.rm=T)#5.786869
city[max(natgrwthrate_all_15, na.rm=T)] #漳州市 福建省
city[min(natgrwthrate_all_15, na.rm=T)] #伊春市 黑龙江省
mean(log(natgrwthrate_all_15+6.44), na.rm=T)#2.457549
min(log(natgrwthrate_all_15+6.44), na.rm=T)#0
max(log(natgrwthrate_all_15+6.44), na.rm=T)#3.531933
sd(log(natgrwthrate_all_15+6.44), na.rm=T)#0.5394931

#wicked_merge_no_missingB$urb_rur_wage_dif_no_out
plot(density(wicked_merge_no_missingB$urb_rur_wage_dif_no_out,na.rm=T))
plot(density(scale(wicked_merge_no_missingB$urb_rur_wage_dif_no_out),,na.rm=T))
mean(wicked_merge_no_missingB$urb_rur_wage_dif_no_out, na.rm=T) #2989.322
min(wicked_merge_no_missingB$urb_rur_wage_dif_no_out, na.rm=T) #-41905
wicked_merge_no_missingB$city[which(wicked_merge_no_missingB$urb_rur_wage_dif_no_out < 0)]
max(wicked_merge_no_missingB$urb_rur_wage_dif_no_out, na.rm=T) #60743
sd(wicked_merge_no_missingB$urb_rur_wage_dif_no_out, na.rm=T) #7828.992
mean(scale(wicked_merge_no_missingB$urb_rur_wage_dif_no_out), na.rm=T) #0
min(scale(wicked_merge_no_missingB$urb_rur_wage_dif_no_out), na.rm=T) #-5.734368
max(scale(wicked_merge_no_missingB$urb_rur_wage_dif_no_out), na.rm=T) #7.376898
sd(scale(wicked_merge_no_missingB$urb_rur_wage_dif_no_out), na.rm=T) #1

#住宅投资15PC
mean(wicked_merge_no_missingB$住宅投资15PC, na.rm=T) #0.3934683
min(wicked_merge_no_missingB$住宅投资15PC, na.rm=T) #0.02111542
max(wicked_merge_no_missingB$住宅投资15PC, na.rm=T) #2.358301
sd(wicked_merge_no_missingB$住宅投资15PC, na.rm=T)#0.3470947
mean(log(wicked_merge_no_missingB$住宅投资15PC), na.rm=T) #-1.265913
min(log(wicked_merge_no_missingB$住宅投资15PC), na.rm=T) #-3.857751
max(log(wicked_merge_no_missingB$住宅投资15PC), na.rm=T) #0.8579415
sd(log(wicked_merge_no_missingB$住宅投资15PC), na.rm=T) #0.8360234
d
#jianzhuPCnew_pop
mean(wicked_merge_no_missingB$jianzhuPCnew_pop, na.rm=T) #0.01915033
min(wicked_merge_no_missingB$jianzhuPCnew_pop, na.rm=T) #0.000888494
max(wicked_merge_no_missingB$jianzhuPCnew_pop, na.rm=T) #0.1692055
sd(wicked_merge_no_missingB$jianzhuPCnew_pop, na.rm=T)#0.02166119
mean(log(wicked_merge_no_missingB$jianzhuPCnew_pop), na.rm=T) #-4.351606
min(log(wicked_merge_no_missingB$jianzhuPCnew_pop), na.rm=T) #-7.025983
max(log(wicked_merge_no_missingB$jianzhuPCnew_pop), na.rm=T) #-1.776641
sd(log(wicked_merge_no_missingB$jianzhuPCnew_pop), na.rm=T) #0.871384

#zhizaoPCnew_pop
mean(wicked_merge_no_missingB$zhizaoPCnew_pop, na.rm=T) #0.03494126
min(wicked_merge_no_missingB$zhizaoPCnew_pop, na.rm=T) #0.001485394
max(wicked_merge_no_missingB$zhizaoPCnew_pop, na.rm=T) #0.2415397
sd(wicked_merge_no_missingB$zhizaoPCnew_pop, na.rm=T)#0.03572324
mean(log(wicked_merge_no_missingB$zhizaoPCnew_pop), na.rm=T) #-3.734601
min(log(wicked_merge_no_missingB$zhizaoPCnew_pop), na.rm=T) #-6.512075
max(log(wicked_merge_no_missingB$zhizaoPCnew_pop), na.rm=T) #-1.420721
sd(log(wicked_merge_no_missingB$zhizaoPCnew_pop), na.rm=T) #0.8865527

#numb_doctorPC
mean(wicked_merge_no_missingB$numb_doctorPC, na.rm=T) #0.002158139
min(wicked_merge_no_missingB$numb_doctorPC, na.rm=T) #0.000926319
max(wicked_merge_no_missingB$numb_doctorPC, na.rm=T) #0.007351881
sd(wicked_merge_no_missingB$numb_doctorPC, na.rm=T)#0.0007565578
mean(log(wicked_merge_no_missingB$numb_doctorPC), na.rm=T) #-6.188789
min(log(wicked_merge_no_missingB$numb_doctorPC), na.rm=T) #-6.984292
max(log(wicked_merge_no_missingB$numb_doctorPC), na.rm=T) #-4.912799
sd(log(wicked_merge_no_missingB$numb_doctorPC), na.rm=T) #0.3104531

#log(住宅投资15PC) + log(jianzhuPCnew_pop) zhizaoPCnew_pop numb_doctorPC

