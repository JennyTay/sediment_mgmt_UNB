#Projection of LFRR and BSS habitat suitabilit with climate change and with sediment management


library(raster)
library(tidyverse)
library(lubridate)
library(readxl)
library(sf)
library(caret)
library(sp)
library(rgeos)

#### Model one: model predicting marsh vegetation (type and height) from inundation (hours and depths) ####
# 2012 data is used because that is when thorne et al did their elevation survey

# Calculate Inundation from raster elevation and LA Water level data ##
# Elevation is NAVD88 Based on report Need to confirm with Karen (unit is meters)
elv_m_2012<- raster("WARMER/Newport_DEM_m_2012.TIF")
ncell(elv_m_2012)
plot(elv_m_2012)

#water level data NAVD88, Unit is feet so will need to conver to meters
lev_2012<-read.csv("LAWaterLevel/CO-OPS_9410660_wl_2012.csv") %>% 
  mutate(lev_m = Verified..ft.*0.3048)%>% #convert to m
  dplyr::select(1,2,6) %>% 
  unite(date, 1,2, sep = " ") #make single date column
lev_2012$date<-ymd_hm(lev_2012$date) #convert to date object
lev_2012$season<-ifelse(month(lev_2012$date)%in% c(1,2,3,10,11,12), "W", "S") #add column for season
names(lev_2012)
head(lev_2012)

#inundation rasterStack functin
#subtract water level from each pixel value (ele - lev)  for each waterlevel value (1:nrow(lev))

inundation<-function(elv, lev){
  
  inund<-elv #initiate rasterStack using the elevation file
  
  for (i in 1:nrow(lev)) {
    
    temp<-elv-lev[i,2]
    inund<-stack(inund,temp)
    cat(i, '\t')
  }
  
  inund<-dropLayer(inund,1)
  
}

#apply function to year, summer, winter
summ <- inundation(elv = elv_m_2012, lev = lev_2012[lev_2012$season=="S",])
wint <- inundation(elv = elv_m_2012, lev = lev_2012[lev_2012$season=="W",])

#sum the values  less than 0 (whole year, winter, summer) - this is the hours that pixel was inundated
#average pixel values that are less than 0 - the avg dept of water above the marsh surface pixel
#average pixel values that are greater than 0 - the avg height of  marsh above the water


avg_height_above_water_level <- function(x){mean(x[x>0])} #function for average exposure height
avg_depth_below_water_level <- function(x){mean(x[x<0])} #function for avg submergence depth
avg_height <- function(x){mean(x)} #function for mean inundation
max_height_above_water_level <- function(x){max(x)} #function for max distance above water level
max_depth_below_water_level <- function(x){min(x)} #function for max depth below water level
pct_inundated <- function(x){ (sum(x<0))/8784 } #function for percent of annaul hours inundated

summ_avg_ht_above_m <- calc(summ, fun = avg_height_above_water_level) #caluclute for each pixel in raster stack (hourly timestep)
summ_avg_ht_below_m <- calc(summ, fun = avg_depth_below_water_level, progress = "text")
summ_avg_ht <- calc(summ, fun = avg_height, progress = "text")
summ_max_ht_above_wl <- calc(summ, fun = max_height_above_water_level, progress = "text")
summ_max_depth_below_wl <- calc(summ, fun = max_depth_below_water_level, progress = "text")
summ_inu_pct <- calc(summ, fun = pct_inundated, progress = "text")

wint_avg_ht_above_m <- calc(wint, fun = avg_height_above_water_level, progress = "text") #caluclute for each pixel in raster stack (hourly timestep)
wint_avg_ht_below_m <- calc(wint, fun = avg_depth_below_water_level, progress = "text")
wint_avg_ht <- calc(wint, fun = avg_height, progress = "text")
wint_max_ht_above_wl <- calc(wint, fun = max_height_above_water_level, progress = "text")
wint_max_depth_below_wl <- calc(wint, fun = max_depth_below_water_level, progress = "text")
wint_inu_pct <- calc(wint, fun = pct_inundated, progress = "text")


#stack the raster objects used in the veg model - names need to be exact
inun_metrics <- stack(summ_avg_ht_above_m, summ_avg_ht_below_m, summ_avg_ht, summ_max_ht_above_wl,
                      summ_max_depth_below_wl,summ_inu_pct, 
                      wint_avg_ht_above_m, wint_avg_ht_below_m, wint_avg_ht, wint_max_ht_above_wl,
                      wint_max_depth_below_wl, wint_inu_pct,
                      elv_m_2012)
names(inun_metrics) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", "summ_max_ht_above_wl",
                         "summ_max_depth_below_wl", "summ_inu_pct",
                         "wint_avg_ht_above_m", "wint_avg_ht_below_m","wint_avg_ht", "wint_max_ht_above_wl", 
                         "wint_max_depth_below_wl", "wint_inu_pct",
                         "elv_m") 

writeRaster(inun_metrics, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/2012_inun_metrics.grd", format = "raster", overwrite = TRUE)

inun_metrics_12 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/2012_inun_metrics.grd")

plot(x = inun_metrics_12, y = 6, main = '', axes = F) # not sure why main isnt working
title(main = "Summer Inundation 2012 (%)")
plot(x = inun_metrics_12, y = 13, main = '', axes = F) # not sure why main isnt working
title(main = "Elevation 2012 (m)")


### extract inundatin metrics to veg file ###

#read in 2012 Thorne vegetation survey 
veg<- read_excel("Newport_Vegetation_FINAL_K_Thorne.xlsx")
names(veg)[1] <- "Point_ID"
ID<-veg$Point_ID

#keep just % cover, max, avg height
veg<- veg[,!grepl(pattern = "MIN$", colnames(veg))]
veg<- veg[,!grepl(pattern = "^UNK", colnames(veg))]
veg<-veg %>% dplyr::select(-"QA/QC Notes", -"DODDER")

#make NAs in the veg df 0
veg[is.na(veg)]<-0

# read in Thorne 2012 elevation survey
UNB_elv<- st_read("Newport RTK GPS/6 Newport/Newport Elevation Points.shp")
UNB_elv$Point_ID<- as.character(UNB_elv$Point_ID)

# join veg to elevation so veg gets spatial information
veg<- left_join(UNB_elv, veg, by = "Point_ID") %>% st_zm()

# filter for only the pts where veg data was take
veg<- veg %>%
  filter(Point_ID %in% ID)

#overlay vegetation points on top of inunation rasters to extract inundation metric at each point

veg<-st_transform(veg, crs = crs(elv_m_2012)) # project veg to correct crs

veg<-as_Spatial(veg) #convert to spatial data type

#extract inundation and elevation metrics to veg file - take mean value from a 10 m buffer (incase veg point happened to be
#in a pot hole or something)

veg<-cbind(veg, raster::extract(x = inun_metrics_12, y = veg, layer = 1, nl = 13))

#convert to sf
veg <- st_as_sf(veg)



library(GGally)
ggpairs(data = veg, columns = 61:73)

ggplot(data = veg, mapping = aes(x = summ_inu_pct, y = SPSP_MAX))+
  geom_point(alpha = 1/3)+
  labs(x = "Summer Inundation (%)", y = "Height (cm)", title = "Maximum S. foliosa height")+
  geom_smooth(method = "glm",formula = y~x+I(x^2))+
  xlim(c(0,0.5))


library(pscl)
library(boot)



# Model Vegetation from inundatin and elevation (leave one out calibration)
# zero inflated negative bionmial
# if height is 0, that means the spp is absent
# if highgt > 0, want to model the value

ggplot(data = veg, mapping = aes(x= SAPA_MAX))+
  geom_histogram(binwidth = 5, color = "grey70")+
  labs(y= "Count", x = "Maximum height (cm)")+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        title = element_text(size = 30), 
        panel.grid.major=element_line(colour="grey80"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
var(veg$SPSP_MAX) #much much larger - probabaly want to do a zero inflated negative bionmial
mean(veg$SPSP_MAX)

veg$summ_inu_pct_sq<-(veg$summ_inu_pct)^2

#SPSP_max model
mdl_sp_max <- zeroinfl(SPSP_MAX ~ summ_inu_pct + summ_inu_pct_sq | summ_inu_pct, 
                       dist = "negbin",
                       data = veg,
                       EM = TRUE)
summary(mdl_sp_max)
save(mdl_sp_max, file = "SPSP_MAX_model.rda")

#test prediction on the data
prd <- data.frame("summ_inu_pct" = veg$summ_inu_pct, "summ_inu_pct_sq" = veg$summ_inu_pct_sq)
predict_ct <- predict(mdl_sp_max, newdata = prd, type = "response")
predict_zr <- predict(mdl_sp_max, newdata = prd, type = "zero")
compare <- data.frame("SPSP_MAX"=veg$SPSP_MAX, "ct" = predict_ct, "zr" = predict_zr)
compare$zr_binary <- as.factor(ifelse(compare$zr <=.5, 1, 0))
ggplot(data = compare, mapping = aes(x = SPSP_MAX, y = ct, col = zr_binary))+
  geom_point(alpha = 1/2)+
  xlim(c(0,150))+
  ylim(c(0,150))
#how many false postives
false_pos <- compare %>% 
  filter(SPSP_MAX == 0 & zr_binary == 1) # 14 false positives out of 240 (5.8%)
#how many true negatives
true_neg <- compare %>% 
  filter(SPSP_MAX!=0 & zr_binary ==0) # 15 true negatives (6.2%)

#does model do better than null model
# mdl_0 <- update(mdl_sp_max, . ~ 1)#null model
# pchisq(2*(logLik(mdl_sp_max) - logLik(mdl_0)), df = 3, lower.tail = FALSE) #'log Lik.' 1.919463e-41 (df=6) yes - stat sig
# is model better than standar negative binomiol regression?
# standard neg binom
# library(MASS)
# summary(mdl1 <- glm.nb(SPSP_MAX ~ summ_inu_pct , data = veg, init.theta = 0.2552931119,
#                        link = log))
# vuong(mdl_sp_max, mdl1)
#yes and yes, remove the testing models
# rm(mdl_0, mdl1)

#get confidence intervals for parametres and exponentiated parameters using bootstrapping
#for neg binom, parameters are incident risk ratios; for zero infl model they are odds ratios
dput(round(coef(mdl_sp_max, "count"), 4)) # coef for count model
dput(round(coef(mdl_sp_max, "zero"), 4)) # coef for zero infl model
f <- function(data, i){
  require(pscl)
  m <- zeroinfl(SPSP_MAX ~ summ_inu_pct | summ_inu_pct,
                data = data[i, ], dist = "negbin",
                start = list(count = c(4.0887, 3.4899),
                             zero =c(-21.2225, 13.7736)))
  as.vector(t(do.call(rbind, coef(summary(m)))[,1:2]))
}

set.seed(10)
(res <- boot(veg, f, R = 1200, parallel = "snow", ncpus = 4))




# predict vegetation in 2011-2019 ( 2012 is calidation year when thorne did the survye)
# need to add in a raster that is summ12_inu_pct_sq

load("SPSP_MAX_model.rda")

#2012 inundation raster stack - adding in summ12_inu_pct_cubed
summ_inu_pct_sq <- raster(inun_metrics_12, layer = 6)
summ_inu_pct_sq <- summ_inu_pct_sq^2
inun_metrics_12 <- addLayer(inun_metrics_12, summ_inu_pct_sq)
names(inun_metrics_12) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", "summ_max_ht_above_wl",
                            "summ_max_depth_below_wl", "summ_inu_pct",
                            "wint_avg_ht_above_m", "wint_avg_ht_below_m","wint_avg_height", "wint_max_ht_above_wl", 
                            "wint_max_depth_below_wl", "wint_inu_pct",
                            "elv_m", "summ_inu_pct_sq") 


#predictions

#SPSP_MAX

#2012
sp_max_ct <- raster::predict(inun_metrics_12, mdl_sp_max, type = "response", progress = "text")
#predict prob that its a zero
sp_max_zr <- raster::predict(inun_metrics_12, mdl_sp_max, type = "zero", progress = "text")
#replace values with 0 or 1
values(sp_max_zr)[values(sp_max_zr)>0.5] = 0
values(sp_max_zr)[values(sp_max_zr)<=0.5 & values(sp_max_zr)>0] = 1
#final raster: if zero>0.5 then 0, else count
sp_max_2012 <- sp_max_zr * sp_max_ct
plot(sp_max_2012, main = "2012")



########################################################################################################
########################################################################################################
########################################################################################################

#water level
lev_2011<-read.csv("LAWaterLevel/CO-OPS_9410660_wl_2011.csv")
lev_2013<-read.csv("LAWaterLevel/CO-OPS_9410660_wl_2013.csv") 
lev_2014<-read.csv("LAWaterLevel/CO-OPS_9410660_wl_2014.csv")  
lev_2015<-read.csv("LAWaterLevel/CO-OPS_9410660_wl_2015.csv")
lev_2016<-read.csv("LAWaterLevel/CO-OPS_9410660_wl_2016.csv")
lev_2017<-read.csv("LAWaterLevel/CO-OPS_9410660_wl_2017.csv")
lev_2018<-read.csv("LAWaterLevel/CO-OPS_9410660_wl_2018.csv")
#water level in MSL to be used with Matts Delft3D raster. 2011 is a normal year and 2015 is an el nino year
lev_2011_MSL <- read.csv("LAWaterLevel/CO-OPS_9410660_wl_2011_MSL.csv")
lev_2015_MSL <- read.csv("LAWaterLevel/CO-OPS_9410660_wl_2015_MSL.csv")

tidy_water_level <- function(timeseries){
  
  timeseries <-  timeseries %>% 
    mutate(lev_m = Verified..ft.*0.3048)%>% #convert to m
    dplyr::select(1,2,6) %>% 
    unite(date, 1,2, sep = " ") #make single date column
  
  timeseries$date<-ymd_hm(timeseries$date) #convert to date object
  
  timeseries$season<-ifelse(month(timeseries$date)%in% c(1,2,3,10,11,12), "W", "S") #add column for season
  
  return(timeseries)
}
#tid water level function for the MSL csvs which for some reason are getting read in differently
tidy_water_level2 <- function(timeseries){
  
  timeseries <-  timeseries %>% 
    mutate(lev_m = Verified..ft.*0.3048)%>% #convert to m
    dplyr::select(1,2,6) %>% 
    unite(date, 1,2, sep = " ") #make single date column
  
  timeseries$date<-mdy_hm(timeseries$date) #convert to date object
  
  timeseries$season<-ifelse(month(timeseries$date)%in% c(1,2,3,10,11,12), "W", "S") #add column for season
  
  return(timeseries)
}
#these levels are all NAV*88
lev_2011 <- tidy_water_level(timeseries = lev_2011)
lev_2013 <- tidy_water_level(timeseries = lev_2013)
lev_2014 <- tidy_water_level(timeseries = lev_2014)
lev_2015 <- tidy_water_level(timeseries = lev_2015)
lev_2016 <- tidy_water_level(timeseries = lev_2016)
lev_2017 <- tidy_water_level(timeseries = lev_2017)
lev_2018 <- tidy_water_level(timeseries = lev_2018)


#water level for prediction, 2100

# RCP 4.5 , 2100 -> 0.3962m
# RCP 8.5 , 2100 -> 1.09728m

#RCP 8.5 , 2100, normal year, MSL
lev_2011_MSL <- tidy_water_level2(timeseries = lev_2011_MSL)
lev_2100_normal_RCP8 <- lev_2011_MSL %>% 
  mutate(lev_m_2100 = lev_m+1.09728) %>% 
  dplyr::select(1,4,3)

#RCP 4.5 , 2100, normal year, MSL
lev_2100_normal_RCP4 <- lev_2011_MSL %>% 
  mutate(lev_m_2100 = lev_m+0.3962) %>% 
  dplyr::select(1,4,3)

#RCP 8.5 , 2100, el nino year, MSL
lev_2015_MSL <- tidy_water_level2(timeseries = lev_2015_MSL)
lev_2100_elnino_RCP8 <- lev_2015_MSL %>% 
  mutate(lev_m_2100 = lev_m+1.09728) %>% 
  dplyr::select(1,4,3)

#RCP 4.5 , 2100, el nino year, MSL
lev_2100_elnino_RCP4 <- lev_2015_MSL %>% 
  mutate(lev_m_2100 = lev_m+0.39624) %>% 
  dplyr::select(1,4,3)


#water level for prediction, 2050

# RCP 4.5 , 2050 -> 0.21336m
# RCP 8.5 , 2050 -> 0.36576m

#RCP 8.5 , 2050, normal year, MSL
lev_2050_normal_RCP8 <- lev_2011_MSL %>% 
  mutate(lev_m_2100 = lev_m+0.36576) %>% 
  dplyr::select(1,4,3)

#RCP 4.5 , 2050, normal year, MSL
lev_2050_normal_RCP4 <- lev_2011_MSL %>% 
  mutate(lev_m_2100 = lev_m+0.21336) %>% 
  dplyr::select(1,4,3)

#RCP 8.5 , 2050, el nino year, MSL
lev_2050_elnino_RCP8 <- lev_2015_MSL %>% 
  mutate(lev_m_2100 = lev_m+0.36576) %>% 
  dplyr::select(1,4,3)

#RCP 4.5 , 2050, el nino year, MSL
lev_2050_elnino_RCP4 <- lev_2015_MSL %>% 
  mutate(lev_m_2100 = lev_m+0.21336) %>% 
  dplyr::select(1,4,3)



level_data_compare <- rbind(lev_2011, lev_2012, lev_2013, lev_2014, lev_2015, lev_2016, lev_2017, lev_2018)
level_data_compare$year <- year(level_data_compare$date)
ggplot(data = level_data_compare, mapping = aes(x = as.factor(year), y = lev_m))+
  geom_boxplot(aes(col = season))+
  labs(x = "year", y = "water level (m)")
year_means <- level_data_compare %>% 
  group_by(year) %>% 
  summarise(avg = round(mean(lev_m),2), sd = round(sd(lev_m),2))

#Elevation #these elevations from K thorne are all in NAVD88
elv_m_2011<- raster("WARMER/Newport_DEM_m_2011.TIF")
elv_m_2013<- raster("WARMER/Newport_DEM_m_2013.TIF")
elv_m_2014<- raster("WARMER/Newport_DEM_m_2014.TIF")
elv_m_2015<- raster("WARMER/Newport_DEM_m_2015.TIF")
elv_m_2016<- raster("WARMER/Newport_DEM_m_2016.TIF")
elv_m_2017<- raster("WARMER/Newport_DEM_m_2017.TIF")
elv_m_2018<- raster("WARMER/Newport_DEM_m_2018.TIF")
elv_m_2019<- raster("WARMER/Newport_DEM_m_2019.TIF")

#for prediction: baseline elevation from delft for the whole marsh, elevation is in MSL
elv_m_2017_baseline <- raster("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/Delft3D/elevation_post_Mar_9_2017.TIF")
extent <- extent(c(1847000, 1850050, 661700, 666000))
elv_m_2017_baseline <- crop(elv_m_2017_baseline, extent) #crop to marsh extent

#for prediction: baseline elevation from delft for the whole marsh, elevation is in MSL
elv_m_2015_baseline <- raster("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/Delft3D/elevation_post_Jan_11_2015.TIF")
extent <- extent(c(1847000, 1850050, 661700, 666000))
elv_m_2015_baseline <- crop(elv_m_2015_baseline, extent) #crop to marsh extent


#Future Predictions: 2050

#dredging,  0.21336m SLR, elevation is in MSL
elv_m_2050_dre_RCP4 <- crop(raster("RCP4_dredging_2050/Bed_Level_2050.TIF"), extent)

#dredging, 0.36576m  SLR, elevation is in MSL
elv_m_2050_dre_RCP8 <- crop(raster("RCP8_2050_dredging/RCP8_2050_dredging.TIF"), extent)

#No dredging,  0.21336m SLR, elevation is in MSL
elv_m_2050_ND_RCP4 <- crop(raster("RCP4_no_dredging_2050/RCP4_no_dredging_2050.TIF"),extent)

#No dredging, 0.36576m  SLR, elevation is in MSL
elv_m_2050_ND_RCP8 <- crop(raster("RCP8_2050_no_dredging/RCP8_2050_no_dredging.TIF"), extent)

#No dredging,  0.21336m SLR, 66th quantile streamflow 
elv_m_2050_ND_RCP4_Q66 <- crop(raster("RCP4_Q66_no_dredging_2050/RCP4_Q66_no_dredging_2050/RCP4_Q66_no_dredging_2050.TIF"), extent)

#dredging, 0.36576m SLR, 33th quantile streamflow 
elv_m_2050_dre_RCP8_Q33 <- crop(raster("RCP8_Q33_dredging_2050/RCP8_Q33_dredging_2050/RCP8_Q33_dredging_2050.TIF"), extent)



#Future Predictions: 2100

#dredging, 0.39624m SLR, elevation is in MSL
elv_m_2100_dre_RCP4 <- crop(raster("RCP4_dredging_2100/RCP4_dredging_2100.TIF"), extent)

#dredging, 1.09728m  SLR, elevation is in MSL
elv_m_2100_dre_RCP8 <- crop(raster("RCP8_2100_dredging/RCP8_2100_dredging.TIF"), extent)

#No dredging, 0.39624m SLR, elevation is in MSL
elv_m_2100_ND_RCP4 <- crop(raster("RCP4_no_dredging_2100/RCP4_2100_no_dredging.TIF"), extent)

#No dredging, 1.09728m  SLR, elevation is in MSL
elv_m_2100_ND_RCP8 <- crop(raster("RCP8_2100_no_dredging/BL_2100.TIF"), extent)

#No dredging, 0.39624m SLR, 66th quantile streamflow 
elv_m_2100_ND_RCP4_Q66 <- crop(raster("RCP4_Q66_no_dredging_2100/RCP4_Q66_no_dredging_2100/RCP4_Q66_no_dredging_2100.TIF"), extent)

#dredging, 1.09728m SLR, 33th quantile streamflow 
elv_m_2100_dre_RCP8_Q33 <- crop(raster("RCP8_Q33_dredging_2100/RCP8_Q33_dredging_2100/RCP8_Q33_dredging_2100.TIF"), extent)

#the resolutions of the CP files are slightly different than the delft3d files, so need to resample to make them comprable#
#Coupled model, dredging, 0.39624m SLR, elevation is in MSL
CP_elv_m_2100_dre_RCP4 <- crop(resample(raster("Delft3D_WARMER_couple/WARMER_RCP4_D_2100.TIF"), elv_m_2100_dre_RCP8_Q33), extent)

#Coupled model, dredging, 1.09728m  SLR, elevation is in MSL
CP_elv_m_2100_dre_RCP8 <- crop(resample(raster("Delft3D_WARMER_couple/WARMER_RCP8_D_2100.TIF"), elv_m_2100_dre_RCP8_Q33), extent)

#Coupled model, dredging, 1.09728m SLR, 33th quantile streamflow
CP_elv_m_2100_dre_RCP8_Q33 <- crop(resample(raster("Delft3D_WARMER_couple/WARMER_RCP8_D_Q33_2100.TIF"), elv_m_2100_dre_RCP8_Q33), extent)

#Coupled model, No dredging, 0.39624m SLR, elevation is in MSL
CP_elv_m_2100_ND_RCP4 <- crop(resample(raster("Delft3D_WARMER_couple/WARMER_RCP4_ND_2100.TIF"), elv_m_2100_dre_RCP8_Q33), extent)

#Coupled model, No dredging, 1.09728m  SLR, elevation is in MSL
CP_elv_m_2100_ND_RCP8 <- crop(resample(raster("Delft3D_WARMER_couple/WARMER_RCP8_ND_2100.TIF"), elv_m_2100_dre_RCP8_Q33), extent)

#Coupled model, No dredging, 0.39624m SLR, 66th quantile streamflow
CP_elv_m_2100_ND_RCP4_Q66 <- crop(resample(raster("Delft3D_WARMER_couple/WARMER_RCP4_ND_Q66_2100.TIF"), elv_m_2100_dre_RCP8_Q33), extent)









#inundation rasterStack function
#subtract water level from each pixel value (ele - lev)  for each waterlevel value (1:nrow(lev))

inundation<-function(elv, lev){
  
  inund<-elv #initiate rasterStack using the elevation file
  
  for (i in 1:nrow(lev)) {
    
    temp<-elv-lev[i,2]
    inund<-stack(inund,temp)
    cat(i, '\t')
  }
  
  inund<-dropLayer(inund,1)
  
}

# functions to calucate water level metrics for summer and winter
avg_height_above_water_level <- function(x){mean(x[x>0])} #function for average exposure height
avg_depth_below_water_level <- function(x){mean(x[x<0])} #function for avg submergence depth
avg_height <- function(x){mean(x)} #function for mean inundation
max_height_above_water_level <- function(x){max(x)} #function for max distance above water level
max_depth_below_water_level <- function(x){min(x)} #function for max depth below water level
pct_inundated <- function(x){ (sum(x<0))/8784 } #function for percent of annaul hours inundated


#function to create raster layers of inundation metrics, stack, and save

inundation_metrics <- function(summ, wint, elv, year){
  
  summ_avg_ht_above_m <- calc(summ, fun = avg_height_above_water_level) #caluclute for each pixel in raster stack (hourly timestep)
  summ_avg_ht_below_m <- calc(summ, fun = avg_depth_below_water_level)
  summ_avg_ht <- calc(summ, fun = avg_height)
  summ_max_ht_above_wl <- calc(summ, fun = max_height_above_water_level)
  summ_max_depth_below_wl <- calc(summ, fun = max_depth_below_water_level)
  summ_inu_pct <- calc(summ, fun = pct_inundated)
  
  wint_avg_ht_above_m <- calc(wint, fun = avg_height_above_water_level) #caluclute for each pixel in raster stack (hourly timestep)
  wint_avg_ht_below_m <- calc(wint, fun = avg_depth_below_water_level)
  wint_avg_ht <- calc(wint, fun = avg_height)
  wint_max_ht_above_wl <- calc(wint, fun = max_height_above_water_level)
  wint_max_depth_below_wl <- calc(wint, fun = max_depth_below_water_level)
  wint_inu_pct <- calc(wint, fun = pct_inundated)
  
  #stack the raster objects used in the veg model - names need to be exact
  inun_metrics <- stack(summ_avg_ht_above_m, summ_avg_ht_below_m, summ_avg_ht, summ_max_ht_above_wl,
                        summ_max_depth_below_wl,summ_inu_pct, 
                        wint_avg_ht_above_m, wint_avg_ht_below_m, wint_avg_ht, wint_max_ht_above_wl,
                        wint_max_depth_below_wl, wint_inu_pct, elv)
  
  #add the summer inundation squared raster layer
  summ_inu_pct_sq <- raster(inun_metrics, layer = 6)
  summ_inu_pct_sq <- summ_inu_pct_sq^2
  inun_metrics <- addLayer(inun_metrics, summ_inu_pct_sq)
  
  names(inun_metrics) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", "summ_max_ht_above_wl",
                           "summ_max_depth_below_wl", "summ_inu_pct",
                           "wint_avg_ht_above_m", "wint_avg_ht_below_m","wint_avg_ht", "wint_max_ht_above_wl", 
                           "wint_max_depth_below_wl", "wint_inu_pct",
                           "elv_m", "summ_inu_pct_sq") 
  
  writeRaster(inun_metrics, paste("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/", year, "inun_metrics", ".grd", sep = "_"), format = "raster")
  
  return(inun_metrics)
  
}



# 2011
summ_2011 <- inundation(elv = elv_m_2011, lev = lev_2011[lev_2011$season=="S",])
wint_2011 <- inundation(elv = elv_m_2011, lev = lev_2011[lev_2011$season=="W",])
inun_metrics_11 <- inundation_metrics(summ = summ_2011, wint = wint_2011, elv = elv_m_2011, year = "2011")
# read stack back in for use in veg prediction
inun_metrics_11 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2011_inun_metrics_.grd")

# 2013
summ_13 <- inundation(elv = elv_m_2013, lev = lev_2013[lev_2013$season=="S",])
wint_13 <- inundation(elv = elv_m_2013, lev = lev_2013[lev_2013$season=="W",])
inun_metrics_13 <- inundation_metrics(summ = summ_13, wint = wint_13, elv = elv_m_2013, year = "2013")
rm(elv_m_2013, inun_metrics_13, lev_2013, summ_13, wint_13)
inun_metrics_13 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2013_inun_metrics_.grd")

# 2014
summ_14 <- inundation(elv = elv_m_2014, lev = lev_2014[lev_2014$season=="S",])
wint_14 <- inundation(elv = elv_m_2014, lev = lev_2014[lev_2014$season=="W",])
inun_metrics_14 <- inundation_metrics(summ = summ_14, wint = wint_14, elv = elv_m_2014, year = "2014")
rm(elv_m_2014, inun_metrics_14, lev_2014, summ_14, wint_14)
inun_metrics_14 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2014_inun_metrics_.grd")

# 2015
summ_2015 <- inundation(elv = elv_m_2015, lev = lev_2015[lev_2015$season=="S",])
wint_2015 <- inundation(elv = elv_m_2015, lev = lev_2015[lev_2015$season=="W",])
inun_metrics_15 <- inundation_metrics(summ = summ_2015, wint = wint_2015, elv = elv_m_2015, year = "2015")
rm(elv_m_2015, inun_metrics_15, lev_2015, summ_2015, wint_2015)
inun_metrics_15 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2015_inun_metrics_.grd")

# 2016
summ_2016 <- inundation(elv = elv_m_2016, lev = lev_2016[lev_2016$season=="S",])
wint_2016 <- inundation(elv = elv_m_2016, lev = lev_2016[lev_2016$season=="W",])
inun_metrics_16 <- inundation_metrics(summ = summ_2016, wint = wint_2016, elv = elv_m_2016, year = "2016")
rm(elv_m_2016, inun_metrics_16, lev_2016, summ_2016, wint_2016)
inun_metrics_16 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2016_inun_metrics_.grd")

# 2017
summ_2017 <- inundation(elv = elv_m_2017, lev = lev_2017[lev_2017$season=="S",])
wint_2017 <- inundation(elv = elv_m_2017, lev = lev_2017[lev_2017$season=="W",])
inun_metrics_17 <- inundation_metrics(summ = summ_2017, wint = wint_2017, elv = elv_m_2017, year = "2017")
rm(elv_m_2017, inun_metrics_17, lev_2017, summ_2017, wint_2017)
inun_metrics_17 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2017_inun_metrics_.grd")

# 2018
summ_2018 <- inundation(elv = elv_m_2018, lev = lev_2018[lev_2018$season=="S",])
wint_2018 <- inundation(elv = elv_m_2018, lev = lev_2018[lev_2018$season=="W",])
inun_metrics_18 <- inundation_metrics(summ = summ_2018, wint = wint_2018, elv = elv_m_2018, year = "2018")

inun_metrics_18 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2018_inun_metrics_.grd")

#future inundation metrics for 2050 and 2100 , RCP 4.5 and 8.5, and el nino/normal year
#need a new function because its too much memory to do the same function for the entire marsh

inundation<-function(elv, lev){
  
  temp <- elv #initate elv file
  values(temp) <- 0 #replace all values with 0 for a blank raster
  temp2 <- temp
  temp3 <- temp
  temp4 <- temp
  temp5 <- temp
  temp6 <- temp
  
  for (i in 1:nrow(lev)) {
    
    ind <- elv-lev[i,2]
    temp <- (temp + (ind > 0) * ind) #mean of avg exposure height
    temp2 <- (temp2 + (ind < 0) * ind) #mean of avg exposure height
    temp3 <- temp3 + ind #mean inundation
    temp4 <- stack(ind, temp4)
    temp4 <- calc(temp4, fun=max) #max distance above water level
    temp5 <- stack(ind, temp5)
    temp5 <- calc(temp5, fun=min) #max depth below water level
    temp6 <- temp6 + (ind < 0) #percent of annaul hours inundated
    
    cat(i, '\t')
  }
  
  avg_ht_above_m <- temp / nrow(lev)
  avg_ht_below_m <- temp2 / nrow(lev)
  avg_ht <- temp3 / nrow(lev)
  max_ht_above_wl <- temp4
  max_depth_below_wl <- temp5
  inu_pct <- temp6/8784
  inu_pct_sq <- inu_pct^2
  
  inundation_metrics <- stack(avg_ht_above_m, avg_ht_below_m, avg_ht, max_ht_above_wl, max_depth_below_wl, inu_pct, inu_pct_sq)
  
  return(inundation_metrics)
  
}


#2015_delft3D_validation , MSL
summ_2015_delft3D <- inundation(elv = elv_m_2015_baseline, lev = lev_2015_MSL[lev_2015_MSL$season=="S",])
names(summ_2015_delft3D) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                              "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2015_delft3D  <- inundation(elv = elv_m_2015_baseline, lev = lev_2015_MSL[lev_2015_MSL$season=="W",])
names(wint_2015_delft3D ) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                               "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2015_delft3D <- stack(summ_2015_delft3D, wint_2015_delft3D , elv_m_2015_baseline)
names(inun_metrics_2015_delft3D[[15]]) <- "elv_m"
inun_metrics_2015_delft3D <- writeRaster(inun_metrics_2015_delft3D, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/__2015_delft3D_inun_metrics_.grd", overwrite=TRUE)




# 2100 elvations



#Dredge, RCP8.5, 2100, El Nino , MSL
summ_2100_dredge_RCP8_elnino <- inundation(elv = elv_m_2100_dre_RCP8, lev = lev_2100_elnino_RCP8[lev_2100_elnino_RCP8$season=="S",])
names(summ_2100_dredge_RCP8_elnino) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                       "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2100_dredge_RCP8_elnino <- inundation(elv = elv_m_2100_dre_RCP8, lev = lev_2100_elnino_RCP8[lev_2100_elnino_RCP8$season=="W",])
names(wint_2100_dredge_RCP8_elnino) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                       "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2100_dredge_RCP8_elnino <- stack(summ_2100_dredge_RCP8_elnino, wint_2100_dredge_RCP8_elnino, elv_m_2100_dre_RCP8)
names(inun_metrics_2100_dredge_RCP8_elnino[[15]]) <- "elv_m"
inun_metrics_2100_dredge_RCP8_elnino <- writeRaster(inun_metrics_2100_dredge_RCP8_elnino, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_dredge_RCP8_elnino_inun_metrics_.grd", overwrite=TRUE)

#Dredge, RCP8.5, 2100, Normal, MSL
summ_2100_dredge_RCP8_normal <- inundation(elv = elv_m_2100_dre_RCP8, lev = lev_2100_normal_RCP8[lev_2100_normal_RCP8$season=="S",])
names(summ_2100_dredge_RCP8_normal) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                       "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2100_dredge_RCP8_normal <- inundation(elv = elv_m_2100_dre_RCP8, lev = lev_2100_normal_RCP8[lev_2100_normal_RCP8$season=="W",])
names(wint_2100_dredge_RCP8_normal) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                       "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2100_dredge_RCP8_normal <- stack(summ_2100_dredge_RCP8_normal, wint_2100_dredge_RCP8_normal, elv_m_2100_dre_RCP8)
names(inun_metrics_2100_dredge_RCP8_normal[[15]]) <- "elv_m"
inun_metrics_2100_dredge_RCP8_normal <- writeRaster(inun_metrics_2100_dredge_RCP8_normal, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_dredge_RCP8_normal_inun_metrics_.grd", overwrite=TRUE)

#Dredge, RCP4.5, 2100, El Nino , MSL
summ_2100_dredge_RCP4_elnino <- inundation(elv = elv_m_2100_dre_RCP4 , lev = lev_2100_elnino_RCP4[lev_2100_elnino_RCP4$season=="S",])
names(summ_2100_dredge_RCP4_elnino) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                         "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2100_dredge_RCP4_elnino <- inundation(elv = elv_m_2100_dre_RCP4 , lev = lev_2100_elnino_RCP4[lev_2100_elnino_RCP4$season=="W",])
names(wint_2100_dredge_RCP4_elnino) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                         "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2100_dredge_RCP4_elnino <- stack(summ_2100_dredge_RCP4_elnino, wint_2100_dredge_RCP4_elnino, elv_m_2100_dre_RCP4 )
names(inun_metrics_2100_dredge_RCP4_elnino[[15]]) <- "elv_m"
inun_metrics_2100_dredge_RCP4_elnino <- writeRaster(inun_metrics_2100_dredge_RCP4_elnino, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_dredge_RCP4_elnino_inun_metrics_.grd", overwrite=TRUE)

#Dredge, RCP4.5, 2100, Normal , MSL
summ_2100_dredge_RCP4_normal <- inundation(elv = elv_m_2100_dre_RCP4 , lev = lev_2100_normal_RCP4[lev_2100_normal_RCP4$season=="S",])
names(summ_2100_dredge_RCP4_normal) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                         "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2100_dredge_RCP4_normal <- inundation(elv = elv_m_2100_dre_RCP4 , lev = lev_2100_normal_RCP4[lev_2100_normal_RCP4$season=="W",])
names(wint_2100_dredge_RCP4_normal) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                         "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2100_dredge_RCP4_normal <- stack(summ_2100_dredge_RCP4_normal, wint_2100_dredge_RCP4_normal, elv_m_2100_dre_RCP4 )
names(inun_metrics_2100_dredge_RCP4_normal[[15]]) <- "elv_m"
inun_metrics_2100_dredge_RCP4_normal <- writeRaster(inun_metrics_2100_dredge_RCP4_normal, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_dredge_RCP4_normal_inun_metrics_.grd", overwrite=TRUE)

#Dredge, RCP8.5, 2100, Normal , Q33 MSL
summ_2100_dredge_RCP8_normal_Q33 <- inundation(elv = elv_m_2100_dre_RCP8_Q33  , lev = lev_2100_normal_RCP8[lev_2100_normal_RCP8$season=="S",])
names(summ_2100_dredge_RCP8_normal_Q33) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                         "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2100_dredge_RCP8_normal_Q33 <- inundation(elv = elv_m_2100_dre_RCP8_Q33  , lev = lev_2100_normal_RCP8[lev_2100_normal_RCP8$season=="W",])
names(wint_2100_dredge_RCP8_normal_Q33) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                         "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2100_dredge_RCP8_normal_Q33 <- stack(summ_2100_dredge_RCP8_normal_Q33, wint_2100_dredge_RCP8_normal_Q33, elv_m_2100_dre_RCP8_Q33  )
names(inun_metrics_2100_dredge_RCP8_normal_Q33[[15]]) <- "elv_m"
inun_metrics_2100_dredge_RCP8_normal_Q33 <- writeRaster(inun_metrics_2100_dredge_RCP8_normal_Q33, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_dredge_RCP8_normal_Q33_inun_metrics_.grd", overwrite=TRUE)

#Dredge, RCP8.5, 2100, El Nino , Q33 MSL
summ_2100_dredge_RCP8_elnino_Q33 <- inundation(elv = elv_m_2100_dre_RCP8_Q33  , lev = lev_2100_elnino_RCP8[lev_2100_elnino_RCP8$season=="S",])
names(summ_2100_dredge_RCP8_elnino_Q33) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                             "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2100_dredge_RCP8_elnino_Q33 <- inundation(elv = elv_m_2100_dre_RCP8_Q33  , lev = lev_2100_elnino_RCP8[lev_2100_elnino_RCP8$season=="W",])
names(wint_2100_dredge_RCP8_elnino_Q33) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                             "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2100_dredge_RCP8_elnino_Q33 <- stack(summ_2100_dredge_RCP8_elnino_Q33, wint_2100_dredge_RCP8_elnino_Q33, elv_m_2100_dre_RCP8_Q33  )
names(inun_metrics_2100_dredge_RCP8_elnino_Q33[[15]]) <- "elv_m"
inun_metrics_2100_dredge_RCP8_elnino_Q33 <- writeRaster(inun_metrics_2100_dredge_RCP8_elnino_Q33, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_dredge_RCP8_elnino_Q33_inun_metrics_.grd", overwrite=TRUE)

#NoDredge, RCP8.5, 2100, El Nino , MSL
summ_2100_nodredge_RCP8_elnino <- inundation(elv = elv_m_2100_ND_RCP8, lev = lev_2100_elnino_RCP8[lev_2100_elnino_RCP8$season=="S",])
names(summ_2100_nodredge_RCP8_elnino) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                         "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2100_nodredge_RCP8_elnino <- inundation(elv = elv_m_2100_ND_RCP8, lev = lev_2100_elnino_RCP8[lev_2100_elnino_RCP8$season=="W",])
names(wint_2100_nodredge_RCP8_elnino) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                         "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2100_nodredge_RCP8_elnino <- stack(summ_2100_nodredge_RCP8_elnino, wint_2100_nodredge_RCP8_elnino, elv_m_2100_ND_RCP8)
names(inun_metrics_2100_nodredge_RCP8_elnino[[15]]) <- "elv_m"
inun_metrics_2100_nodredge_RCP8_elnino <- writeRaster(inun_metrics_2100_nodredge_RCP8_elnino, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_ND_RCP8_elnino_inun_metrics_.grd", overwrite=TRUE)

#NoDredge, RCP8.5, 2100, Normal , MSL
summ_2100_nodredge_RCP8_normal <- inundation(elv = elv_m_2100_ND_RCP8, lev = lev_2100_normal_RCP8[lev_2100_normal_RCP8$season=="S",])
names(summ_2100_nodredge_RCP8_normal) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                         "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2100_nodredge_RCP8_normal <- inundation(elv = elv_m_2100_ND_RCP8, lev = lev_2100_normal_RCP8[lev_2100_normal_RCP8$season=="W",])
names(wint_2100_nodredge_RCP8_normal) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                         "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2100_nodredge_RCP8_normal <- stack(summ_2100_nodredge_RCP8_normal, wint_2100_nodredge_RCP8_normal, elv_m_2100_ND_RCP8)
names(inun_metrics_2100_nodredge_RCP8_normal[[15]]) <- "elv_m"
inun_metrics_2100_nodredge_RCP8_normal <- writeRaster(inun_metrics_2100_nodredge_RCP8_normal, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_ND_RCP8_normal_inun_metrics_.grd", overwrite=TRUE)

#NoDredge, RCP4.5, 2100, El Nino , MSL
summ_2100_nodredge_RCP4_elnino <- inundation(elv = elv_m_2100_ND_RCP4, lev = lev_2100_elnino_RCP4[lev_2100_elnino_RCP4$season=="S",])
names(summ_2100_nodredge_RCP4_elnino) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                           "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2100_nodredge_RCP4_elnino <- inundation(elv = elv_m_2100_ND_RCP4, lev = lev_2100_elnino_RCP4[lev_2100_elnino_RCP4$season=="W",])
names(wint_2100_nodredge_RCP4_elnino) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                           "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2100_nodredge_RCP4_elnino <- stack(summ_2100_nodredge_RCP4_elnino, wint_2100_nodredge_RCP4_elnino, elv_m_2100_ND_RCP4)
names(inun_metrics_2100_nodredge_RCP4_elnino[[15]]) <- "elv_m"
inun_metrics_2100_nodredge_RCP4_elnino <- writeRaster(inun_metrics_2100_nodredge_RCP4_elnino, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_ND_RCP4_elnino_inun_metrics_.grd", overwrite=TRUE)

#NoDredge, RCP4.5, 2100, Normal  , MSL
summ_2100_nodredge_RCP4_normal <- inundation(elv = elv_m_2100_ND_RCP4, lev = lev_2100_normal_RCP4[lev_2100_normal_RCP4$season=="S",])
names(summ_2100_nodredge_RCP4_normal) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                           "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2100_nodredge_RCP4_normal <- inundation(elv = elv_m_2100_ND_RCP4, lev = lev_2100_normal_RCP4[lev_2100_normal_RCP4$season=="W",])
names(wint_2100_nodredge_RCP4_normal) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                           "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2100_nodredge_RCP4_normal <- stack(summ_2100_nodredge_RCP4_normal, wint_2100_nodredge_RCP4_normal, elv_m_2100_ND_RCP4)
names(inun_metrics_2100_nodredge_RCP4_normal[[15]]) <- "elv_m"
inun_metrics_2100_nodredge_RCP4_normal <- writeRaster(inun_metrics_2100_nodredge_RCP4_normal, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_ND_RCP4_normal_inun_metrics_.grd", overwrite=TRUE)

#NoDredge, RCP4.5, 2100, Q66, Normal  , MSL
summ_2100_nodredge_RCP4_normal_Q66 <- inundation(elv = elv_m_2100_ND_RCP4_Q66, lev = lev_2100_normal_RCP4[lev_2100_normal_RCP4$season=="S",])
names(summ_2100_nodredge_RCP4_normal_Q66) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                           "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2100_nodredge_RCP4_normal_Q66 <- inundation(elv = elv_m_2100_ND_RCP4_Q66, lev = lev_2100_normal_RCP4[lev_2100_normal_RCP4$season=="W",])
names(wint_2100_nodredge_RCP4_normal_Q66) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                           "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2100_nodredge_RCP4_normal_Q66 <- stack(summ_2100_nodredge_RCP4_normal_Q66, wint_2100_nodredge_RCP4_normal_Q66, elv_m_2100_ND_RCP4_Q66)
names(inun_metrics_2100_nodredge_RCP4_normal_Q66[[15]]) <- "elv_m"
inun_metrics_2100_nodredge_RCP4_normal_Q66 <- writeRaster(inun_metrics_2100_nodredge_RCP4_normal_Q66, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_ND_RCP4_normal_Q66_inun_metrics_.grd", overwrite=TRUE)

#NoDredge, RCP4.5, 2100, Q66, El Nino  , MSL
summ_2100_nodredge_RCP4_elnino_Q66 <- inundation(elv = elv_m_2100_ND_RCP4_Q66, lev = lev_2100_elnino_RCP4[lev_2100_elnino_RCP4$season=="S",])
names(summ_2100_nodredge_RCP4_elnino_Q66) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                               "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2100_nodredge_RCP4_elnino_Q66 <- inundation(elv = elv_m_2100_ND_RCP4_Q66, lev = lev_2100_elnino_RCP4[lev_2100_elnino_RCP4$season=="W",])
names(wint_2100_nodredge_RCP4_elnino_Q66) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                               "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2100_nodredge_RCP4_elnino_Q66 <- stack(summ_2100_nodredge_RCP4_elnino_Q66, wint_2100_nodredge_RCP4_elnino_Q66, elv_m_2100_ND_RCP4_Q66)
names(inun_metrics_2100_nodredge_RCP4_elnino_Q66[[15]]) <- "elv_m"
inun_metrics_2100_nodredge_RCP4_elnino_Q66 <- writeRaster(inun_metrics_2100_nodredge_RCP4_elnino_Q66, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_ND_RCP4_elnino_Q66_inun_metrics_.grd", overwrite=TRUE)



#Coupled model, dredging, 0.39624m SLR, 2100, normal, MSL
CP_summ_2100_dredge_RCP4_normal <- inundation(elv = CP_elv_m_2100_dre_RCP4 , lev = lev_2100_normal_RCP4[lev_2100_normal_RCP4$season=="S",])
names(CP_summ_2100_dredge_RCP4_normal) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                               "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
CP_wint_2100_dredge_RCP4_normal <- inundation(elv = CP_elv_m_2100_dre_RCP4 , lev = lev_2100_normal_RCP4[lev_2100_normal_RCP4$season=="W",])
names(CP_wint_2100_dredge_RCP4_normal) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                               "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
CP_inun_metrics_2100_dredge_RCP4_normal <- stack(CP_summ_2100_dredge_RCP4_normal, CP_wint_2100_dredge_RCP4_normal, CP_elv_m_2100_dre_RCP4 )
names(CP_inun_metrics_2100_dredge_RCP4_normal[[15]]) <- "elv_m"
CP_inun_metrics_2100_dredge_RCP4_normal <- writeRaster(CP_inun_metrics_2100_dredge_RCP4_normal, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_dredge_RCP4_normal_inun_metrics_.grd", overwrite=TRUE)

#Coupled model, dredging, 0.39624m SLR, 2100, El Nino, MSL
CP_summ_2100_dredge_RCP4_elnino <- inundation(elv = CP_elv_m_2100_dre_RCP4 , lev = lev_2100_elnino_RCP4[lev_2100_elnino_RCP4$season=="S",])
names(CP_summ_2100_dredge_RCP4_elnino) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                            "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
CP_wint_2100_dredge_RCP4_elnino <- inundation(elv = CP_elv_m_2100_dre_RCP4 , lev = lev_2100_elnino_RCP4[lev_2100_elnino_RCP4$season=="W",])
names(CP_wint_2100_dredge_RCP4_elnino) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                            "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
CP_inun_metrics_2100_dredge_RCP4_elnino <- stack(CP_summ_2100_dredge_RCP4_elnino, CP_wint_2100_dredge_RCP4_elnino, CP_elv_m_2100_dre_RCP4 )
names(CP_inun_metrics_2100_dredge_RCP4_elnino[[15]]) <- "elv_m"
CP_inun_metrics_2100_dredge_RCP4_elnino <- writeRaster(CP_inun_metrics_2100_dredge_RCP4_elnino, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_dredge_RCP4_elnino_inun_metrics_.grd", overwrite=TRUE)


#Coupled model, dredging, 1.09728m  SLR, 2100, normal, MSL
CP_summ_2100_dredge_RCP8_normal <- inundation(elv = CP_elv_m_2100_dre_RCP8 , lev = lev_2100_normal_RCP8[lev_2100_normal_RCP8$season=="S",])
names(CP_summ_2100_dredge_RCP8_normal) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                            "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
CP_wint_2100_dredge_RCP8_normal <- inundation(elv = CP_elv_m_2100_dre_RCP8 , lev = lev_2100_normal_RCP8[lev_2100_normal_RCP8$season=="W",])
names(CP_wint_2100_dredge_RCP8_normal) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                            "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
CP_inun_metrics_2100_dredge_RCP8_normal <- stack(CP_summ_2100_dredge_RCP8_normal, CP_wint_2100_dredge_RCP8_normal, CP_elv_m_2100_dre_RCP8 )
names(CP_inun_metrics_2100_dredge_RCP8_normal[[15]]) <- "elv_m"
CP_inun_metrics_2100_dredge_RCP8_normal <- writeRaster(CP_inun_metrics_2100_dredge_RCP8_normal, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_dredge_RCP8_normal_inun_metrics_.grd", overwrite=TRUE)

#Coupled model, dredging, 1.09728m  SLR, 2100, El Nino, MSL
CP_summ_2100_dredge_RCP8_elnino <- inundation(elv = CP_elv_m_2100_dre_RCP8 , lev = lev_2100_elnino_RCP8[lev_2100_elnino_RCP8$season=="S",])
names(CP_summ_2100_dredge_RCP8_elnino) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                            "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
CP_wint_2100_dredge_RCP8_elnino <- inundation(elv = CP_elv_m_2100_dre_RCP8 , lev = lev_2100_elnino_RCP8[lev_2100_elnino_RCP8$season=="W",])
names(CP_wint_2100_dredge_RCP8_elnino) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                            "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
CP_inun_metrics_2100_dredge_RCP8_elnino <- stack(CP_summ_2100_dredge_RCP8_elnino, CP_wint_2100_dredge_RCP8_elnino, CP_elv_m_2100_dre_RCP8 )
names(CP_inun_metrics_2100_dredge_RCP8_elnino[[15]]) <- "elv_m"
CP_inun_metrics_2100_dredge_RCP8_elnino <- writeRaster(CP_inun_metrics_2100_dredge_RCP8_elnino, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_dredge_RCP8_elnino_inun_metrics_.grd", overwrite=TRUE)


#Coupled model, dredging, 1.09728m SLR, 2100,  normal, MSL, Q33
CP_summ_2100_dredge_RCP8_normal_Q33 <- inundation(elv = CP_elv_m_2100_dre_RCP8_Q33  , lev = lev_2100_normal_RCP8[lev_2100_normal_RCP8$season=="S",])
names(CP_summ_2100_dredge_RCP8_normal_Q33) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                            "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
CP_wint_2100_dredge_RCP8_normal_Q33 <- inundation(elv = CP_elv_m_2100_dre_RCP8_Q33  , lev = lev_2100_normal_RCP8[lev_2100_normal_RCP8$season=="W",])
names(CP_wint_2100_dredge_RCP8_normal_Q33) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                            "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
CP_inun_metrics_2100_dredge_RCP8_normal_Q33 <- stack(CP_summ_2100_dredge_RCP8_normal_Q33, CP_wint_2100_dredge_RCP8_normal_Q33, CP_elv_m_2100_dre_RCP8_Q33  )
names(CP_inun_metrics_2100_dredge_RCP8_normal_Q33[[15]]) <- "elv_m"
CP_inun_metrics_2100_dredge_RCP8_normal_Q33 <- writeRaster(CP_inun_metrics_2100_dredge_RCP8_normal_Q33, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_dredge_RCP8_normal_Q33_inun_metrics_.grd", overwrite=TRUE)

#Coupled model, dredging, 1.09728m SLR, 2100,  El Nino, MSL, Q33
CP_summ_2100_dredge_RCP8_elnino_Q33 <- inundation(elv = CP_elv_m_2100_dre_RCP8_Q33  , lev = lev_2100_elnino_RCP8[lev_2100_elnino_RCP8$season=="S",])
names(CP_summ_2100_dredge_RCP8_elnino_Q33) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                                "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
CP_wint_2100_dredge_RCP8_elnino_Q33 <- inundation(elv = CP_elv_m_2100_dre_RCP8_Q33  , lev = lev_2100_elnino_RCP8[lev_2100_elnino_RCP8$season=="W",])
names(CP_wint_2100_dredge_RCP8_elnino_Q33) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                                "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
CP_inun_metrics_2100_dredge_RCP8_elnino_Q33 <- stack(CP_summ_2100_dredge_RCP8_elnino_Q33, CP_wint_2100_dredge_RCP8_elnino_Q33, CP_elv_m_2100_dre_RCP8_Q33  )
names(CP_inun_metrics_2100_dredge_RCP8_elnino_Q33[[15]]) <- "elv_m"
CP_inun_metrics_2100_dredge_RCP8_elnino_Q33 <- writeRaster(CP_inun_metrics_2100_dredge_RCP8_elnino_Q33, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_dredge_RCP8_elnino_Q33_inun_metrics_.grd", overwrite=TRUE)


#Coupled model, No dredging, 0.39624m SLR, 2100, normal, MSL
CP_summ_2100_nodredge_RCP4_normal <- inundation(elv = CP_elv_m_2100_ND_RCP4 , lev = lev_2100_normal_RCP4[lev_2100_normal_RCP4$season=="S",])
names(CP_summ_2100_nodredge_RCP4_normal) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                            "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
CP_wint_2100_nodredge_RCP4_normal <- inundation(elv = CP_elv_m_2100_ND_RCP4 , lev = lev_2100_normal_RCP4[lev_2100_normal_RCP4$season=="W",])
names(CP_wint_2100_nodredge_RCP4_normal) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                            "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
CP_inun_metrics_2100_nodredge_RCP4_normal <- stack(CP_summ_2100_nodredge_RCP4_normal, CP_wint_2100_nodredge_RCP4_normal, CP_elv_m_2100_ND_RCP4 )
names(CP_inun_metrics_2100_nodredge_RCP4_normal[[15]]) <- "elv_m"
CP_inun_metrics_2100_nodredge_RCP4_normal <- writeRaster(CP_inun_metrics_2100_nodredge_RCP4_normal, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_ND_RCP4_normal_inun_metrics_.grd", overwrite=TRUE)

#Coupled model, No dredging, 0.39624m SLR, 2100, El Nino, MSL
CP_summ_2100_nodredge_RCP4_elnino <- inundation(elv = CP_elv_m_2100_ND_RCP4 , lev = lev_2100_elnino_RCP4[lev_2100_elnino_RCP4$season=="S",])
names(CP_summ_2100_nodredge_RCP4_elnino) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                              "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
CP_wint_2100_nodredge_RCP4_elnino <- inundation(elv = CP_elv_m_2100_ND_RCP4 , lev = lev_2100_elnino_RCP4[lev_2100_elnino_RCP4$season=="W",])
names(CP_wint_2100_nodredge_RCP4_elnino) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                              "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
CP_inun_metrics_2100_nodredge_RCP4_elnino <- stack(CP_summ_2100_nodredge_RCP4_elnino, CP_wint_2100_nodredge_RCP4_elnino, CP_elv_m_2100_ND_RCP4 )
names(CP_inun_metrics_2100_nodredge_RCP4_elnino[[15]]) <- "elv_m"
CP_inun_metrics_2100_nodredge_RCP4_elnino <- writeRaster(CP_inun_metrics_2100_nodredge_RCP4_elnino, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_ND_RCP4_elnino_inun_metrics_.grd", overwrite=TRUE)


#Coupled model, No dredging, 1.09728m  SLR, 2100, normal, MSL
CP_summ_2100_nodredge_RCP8_normal <- inundation(elv = CP_elv_m_2100_ND_RCP8  , lev = lev_2100_normal_RCP8[lev_2100_normal_RCP8$season=="S",])
names(CP_summ_2100_nodredge_RCP8_normal) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                              "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
CP_wint_2100_nodredge_RCP8_normal <- inundation(elv = CP_elv_m_2100_ND_RCP8  , lev = lev_2100_normal_RCP8[lev_2100_normal_RCP8$season=="W",])
names(CP_wint_2100_nodredge_RCP8_normal) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                              "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
CP_inun_metrics_2100_nodredge_RCP8_normal <- stack(CP_summ_2100_nodredge_RCP8_normal, CP_wint_2100_nodredge_RCP8_normal, CP_elv_m_2100_ND_RCP8  )
names(CP_inun_metrics_2100_nodredge_RCP8_normal[[15]]) <- "elv_m"
CP_inun_metrics_2100_nodredge_RCP8_normal <- writeRaster(CP_inun_metrics_2100_nodredge_RCP8_normal, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_ND_RCP8_normal_inun_metrics_.grd", overwrite=TRUE)

#Coupled model, No dredging, 1.09728m  SLR, 2100, El Nino, MSL
CP_summ_2100_nodredge_RCP8_elnino <- inundation(elv = CP_elv_m_2100_ND_RCP8  , lev = lev_2100_elnino_RCP8[lev_2100_elnino_RCP8$season=="S",])
names(CP_summ_2100_nodredge_RCP8_elnino) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                              "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
CP_wint_2100_nodredge_RCP8_elnino <- inundation(elv = CP_elv_m_2100_ND_RCP8  , lev = lev_2100_elnino_RCP8[lev_2100_elnino_RCP8$season=="W",])
names(CP_wint_2100_nodredge_RCP8_elnino) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                              "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
CP_inun_metrics_2100_nodredge_RCP8_elnino <- stack(CP_summ_2100_nodredge_RCP8_elnino, CP_wint_2100_nodredge_RCP8_elnino, CP_elv_m_2100_ND_RCP8  )
names(CP_inun_metrics_2100_nodredge_RCP8_elnino[[15]]) <- "elv_m"
CP_inun_metrics_2100_nodredge_RCP8_elnino <- writeRaster(CP_inun_metrics_2100_nodredge_RCP8_elnino, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_ND_RCP8_elnino_inun_metrics_.grd", overwrite=TRUE)


#Coupled model, No dredging, 0.39624m SLR, 2100,  normal, MSL, Q66
CP_summ_2100_nodredge_RCP4_normal_Q66 <- inundation(elv = CP_elv_m_2100_ND_RCP4_Q66 , lev = lev_2100_normal_RCP4[lev_2100_normal_RCP4$season=="S",])
names(CP_summ_2100_nodredge_RCP4_normal_Q66) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                              "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
CP_wint_2100_nodredge_RCP4_normal_Q66 <- inundation(elv = CP_elv_m_2100_ND_RCP4_Q66 , lev = lev_2100_normal_RCP4[lev_2100_normal_RCP4$season=="W",])
names(CP_wint_2100_nodredge_RCP4_normal_Q66) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                              "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
CP_inun_metrics_2100_nodredge_RCP4_normal_Q66 <- stack(CP_summ_2100_nodredge_RCP4_normal_Q66, CP_wint_2100_nodredge_RCP4_normal_Q66, CP_elv_m_2100_ND_RCP4_Q66 )
names(CP_inun_metrics_2100_nodredge_RCP4_normal_Q66[[15]]) <- "elv_m"
CP_inun_metrics_2100_nodredge_RCP4_normal_Q66 <- writeRaster(CP_inun_metrics_2100_nodredge_RCP4_normal_Q66, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_ND_RCP4_normal_Q66_inun_metrics_.grd", overwrite=TRUE)

#Coupled model, No dredging, 0.39624m SLR, 2100,  El Nino, MSL, Q66
CP_summ_2100_nodredge_RCP4_elnino_Q66 <- inundation(elv = CP_elv_m_2100_ND_RCP4_Q66 , lev = lev_2100_elnino_RCP4[lev_2100_elnino_RCP4$season=="S",])
names(CP_summ_2100_nodredge_RCP4_elnino_Q66) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                                  "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
CP_wint_2100_nodredge_RCP4_elnino_Q66 <- inundation(elv = CP_elv_m_2100_ND_RCP4_Q66 , lev = lev_2100_elnino_RCP4[lev_2100_elnino_RCP4$season=="W",])
names(CP_wint_2100_nodredge_RCP4_elnino_Q66) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                                  "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
CP_inun_metrics_2100_nodredge_RCP4_elnino_Q66 <- stack(CP_summ_2100_nodredge_RCP4_elnino_Q66, CP_wint_2100_nodredge_RCP4_elnino_Q66, CP_elv_m_2100_ND_RCP4_Q66 )
names(CP_inun_metrics_2100_nodredge_RCP4_elnino_Q66[[15]]) <- "elv_m"
CP_inun_metrics_2100_nodredge_RCP4_elnino_Q66 <- writeRaster(CP_inun_metrics_2100_nodredge_RCP4_elnino_Q66, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_ND_RCP4_elnino_Q66_inun_metrics_.grd", overwrite=TRUE)






# 2050 elvations




#Dredge, RCP8.5, 2050, El Nino , MSL
summ_2050_dredge_RCP8_elnino <- inundation(elv = elv_m_2050_dre_RCP8, lev = lev_2050_elnino_RCP8[lev_2050_elnino_RCP8$season=="S",])
names(summ_2050_dredge_RCP8_elnino) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                       "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2050_dredge_RCP8_elnino <- inundation(elv = elv_m_2050_dre_RCP8, lev = lev_2050_elnino_RCP8[lev_2050_elnino_RCP8$season=="W",])
names(wint_2050_dredge_RCP8_elnino) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                       "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2050_dredge_RCP8_elnino <- stack(summ_2050_dredge_RCP8_elnino, wint_2050_dredge_RCP8_elnino, elv_m_2050_dre_RCP8)
names(inun_metrics_2050_dredge_RCP8_elnino[[15]]) <- "elv_m"
inun_metrics_2050_dredge_RCP8_elnino <- writeRaster(inun_metrics_2050_dredge_RCP8_elnino, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_dredge_RCP8_elnino_inun_metrics_.grd", overwrite=TRUE)

#Dredge, RCP8.5, 2050, Normal, MSL
summ_2050_dredge_RCP8_normal <- inundation(elv = elv_m_2050_dre_RCP8, lev = lev_2050_normal_RCP8[lev_2050_normal_RCP8$season=="S",])
names(summ_2050_dredge_RCP8_normal) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                       "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2050_dredge_RCP8_normal <- inundation(elv = elv_m_2050_dre_RCP8, lev = lev_2050_normal_RCP8[lev_2050_normal_RCP8$season=="W",])
names(wint_2050_dredge_RCP8_normal) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                       "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2050_dredge_RCP8_normal <- stack(summ_2050_dredge_RCP8_normal, wint_2050_dredge_RCP8_normal, elv_m_2050_dre_RCP8)
names(inun_metrics_2050_dredge_RCP8_normal[[15]]) <- "elv_m"
inun_metrics_2050_dredge_RCP8_normal <- writeRaster(inun_metrics_2050_dredge_RCP8_normal, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_dredge_RCP8_normal_inun_metrics_.grd", overwrite=TRUE)


#Dredge, RCP4.5, 2050, El Nino , MSL
summ_2050_dredge_RCP4_elnino <- inundation(elv = elv_m_2050_dre_RCP4 , lev = lev_2050_elnino_RCP4[lev_2050_elnino_RCP4$season=="S",])
names(summ_2050_dredge_RCP4_elnino) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                         "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2050_dredge_RCP4_elnino <- inundation(elv = elv_m_2050_dre_RCP4 , lev = lev_2050_elnino_RCP4[lev_2050_elnino_RCP4$season=="W",])
names(wint_2050_dredge_RCP4_elnino) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                         "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2050_dredge_RCP4_elnino <- stack(summ_2050_dredge_RCP4_elnino, wint_2050_dredge_RCP4_elnino, elv_m_2050_dre_RCP4 )
names(inun_metrics_2050_dredge_RCP4_elnino[[15]]) <- "elv_m"
inun_metrics_2050_dredge_RCP4_elnino <- writeRaster(inun_metrics_2050_dredge_RCP4_elnino, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_dredge_RCP4_elnino_inun_metrics_.grd", overwrite=TRUE)

#Dredge, RCP4.5, 2050, Normal , MSL
summ_2050_dredge_RCP4_normal <- inundation(elv = elv_m_2050_dre_RCP4 , lev = lev_2050_normal_RCP4[lev_2050_normal_RCP4$season=="S",])
names(summ_2050_dredge_RCP4_normal) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                         "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2050_dredge_RCP4_normal <- inundation(elv = elv_m_2050_dre_RCP4 , lev = lev_2050_normal_RCP4[lev_2050_normal_RCP4$season=="W",])
names(wint_2050_dredge_RCP4_normal) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                         "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2050_dredge_RCP4_normal <- stack(summ_2050_dredge_RCP4_normal, wint_2050_dredge_RCP4_normal, elv_m_2050_dre_RCP4 )
names(inun_metrics_2050_dredge_RCP4_normal[[15]]) <- "elv_m"
inun_metrics_2050_dredge_RCP4_normal <- writeRaster(inun_metrics_2050_dredge_RCP4_normal, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_dredge_RCP4_normal_inun_metrics_.grd", overwrite=TRUE)


#Dredge, RCP8.5, 2050, Normal, Q33, MSL
summ_2050_dredge_RCP8_normal_Q33 <- inundation(elv = elv_m_2050_dre_RCP8_Q33, lev = lev_2050_normal_RCP8[lev_2050_normal_RCP8$season=="S",])
names(summ_2050_dredge_RCP8_normal_Q33) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                         "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2050_dredge_RCP8_normal_Q33 <- inundation(elv = elv_m_2050_dre_RCP8_Q33, lev = lev_2050_normal_RCP8[lev_2050_normal_RCP8$season=="W",])
names(wint_2050_dredge_RCP8_normal_Q33) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                         "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2050_dredge_RCP8_normal_Q33 <- stack(summ_2050_dredge_RCP8_normal_Q33, wint_2050_dredge_RCP8_normal_Q33, elv_m_2050_dre_RCP8_Q33)
names(inun_metrics_2050_dredge_RCP8_normal_Q33[[15]]) <- "elv_m"
inun_metrics_2050_dredge_RCP8_normal_Q33 <- writeRaster(inun_metrics_2050_dredge_RCP8_normal_Q33, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_dredge_RCP8_normal_Q33_inun_metrics_.grd", overwrite=TRUE)

#Dredge, RCP8.5, 2050, El Nino, Q33, MSL
summ_2050_dredge_RCP8_elnino_Q33 <- inundation(elv = elv_m_2050_dre_RCP8_Q33, lev = lev_2050_elnino_RCP8[lev_2050_elnino_RCP8$season=="S",])
names(summ_2050_dredge_RCP8_elnino_Q33) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                             "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2050_dredge_RCP8_elnino_Q33 <- inundation(elv = elv_m_2050_dre_RCP8_Q33, lev = lev_2050_elnino_RCP8[lev_2050_elnino_RCP8$season=="W",])
names(wint_2050_dredge_RCP8_elnino_Q33) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                             "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2050_dredge_RCP8_elnino_Q33 <- stack(summ_2050_dredge_RCP8_elnino_Q33, wint_2050_dredge_RCP8_elnino_Q33, elv_m_2050_dre_RCP8_Q33)
names(inun_metrics_2050_dredge_RCP8_elnino_Q33[[15]]) <- "elv_m"
inun_metrics_2050_dredge_RCP8_elnino_Q33 <- writeRaster(inun_metrics_2050_dredge_RCP8_elnino_Q33, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_dredge_RCP8_elnino_Q33_inun_metrics_.grd", overwrite=TRUE)

#NoDredge, RCP8.5, 2050, El Nino , MSL
summ_2050_nodredge_RCP8_elnino <- inundation(elv = elv_m_2050_ND_RCP8, lev = lev_2050_elnino_RCP8[lev_2050_elnino_RCP8$season=="S",])
names(summ_2050_nodredge_RCP8_elnino) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                           "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2050_nodredge_RCP8_elnino <- inundation(elv = elv_m_2050_ND_RCP8, lev = lev_2050_elnino_RCP8[lev_2050_elnino_RCP8$season=="W",])
names(wint_2050_nodredge_RCP8_elnino) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                           "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2050_nodredge_RCP8_elnino <- stack(summ_2050_nodredge_RCP8_elnino, wint_2050_nodredge_RCP8_elnino, elv_m_2050_ND_RCP8)
names(inun_metrics_2050_nodredge_RCP8_elnino[[15]]) <- "elv_m"
inun_metrics_2050_nodredge_RCP8_elnino <- writeRaster(inun_metrics_2050_nodredge_RCP8_elnino, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_ND_RCP8_elnino_inun_metrics_.grd", overwrite=TRUE)

#NoDredge, RCP8.5, 2050, Normal , MSL
summ_2050_nodredge_RCP8_normal <- inundation(elv = elv_m_2050_ND_RCP8, lev = lev_2050_normal_RCP8[lev_2050_normal_RCP8$season=="S",])
names(summ_2050_nodredge_RCP8_normal) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                           "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2050_nodredge_RCP8_normal <- inundation(elv = elv_m_2050_ND_RCP8, lev = lev_2050_normal_RCP8[lev_2050_normal_RCP8$season=="W",])
names(wint_2050_nodredge_RCP8_normal) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                           "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2050_nodredge_RCP8_normal <- stack(summ_2050_nodredge_RCP8_normal, wint_2050_nodredge_RCP8_normal, elv_m_2050_ND_RCP8)
names(inun_metrics_2050_nodredge_RCP8_normal[[15]]) <- "elv_m"
inun_metrics_2050_nodredge_RCP8_normal <- writeRaster(inun_metrics_2050_nodredge_RCP8_normal, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_ND_RCP8_normal_inun_metrics_.grd", overwrite=TRUE)

#NoDredge, RCP4.5, 2050, El Nino , MSL
summ_2050_nodredge_RCP4_elnino <- inundation(elv = elv_m_2050_ND_RCP4, lev = lev_2050_elnino_RCP4[lev_2050_elnino_RCP4$season=="S",])
names(summ_2050_nodredge_RCP4_elnino) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                           "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2050_nodredge_RCP4_elnino <- inundation(elv = elv_m_2050_ND_RCP4, lev = lev_2050_elnino_RCP4[lev_2050_elnino_RCP4$season=="W",])
names(wint_2050_nodredge_RCP4_elnino) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                           "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2050_nodredge_RCP4_elnino <- stack(summ_2050_nodredge_RCP4_elnino, wint_2050_nodredge_RCP4_elnino, elv_m_2050_ND_RCP4)
names(inun_metrics_2050_nodredge_RCP4_elnino[[15]]) <- "elv_m"
inun_metrics_2050_nodredge_RCP4_elnino <- writeRaster(inun_metrics_2050_nodredge_RCP4_elnino, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_ND_RCP4_elnino_inun_metrics_.grd", overwrite=TRUE)

#NoDredge, RCP4.5, 2050, Normal  , MSL
summ_2050_nodredge_RCP4_normal <- inundation(elv = elv_m_2050_ND_RCP4, lev = lev_2050_normal_RCP4[lev_2050_normal_RCP4$season=="S",])
names(summ_2050_nodredge_RCP4_normal) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                           "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2050_nodredge_RCP4_normal <- inundation(elv = elv_m_2050_ND_RCP4, lev = lev_2050_normal_RCP4[lev_2050_normal_RCP4$season=="W",])
names(wint_2050_nodredge_RCP4_normal) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                           "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2050_nodredge_RCP4_normal <- stack(summ_2050_nodredge_RCP4_normal, wint_2050_nodredge_RCP4_normal, elv_m_2050_ND_RCP4)
names(inun_metrics_2050_nodredge_RCP4_normal[[15]]) <- "elv_m"
inun_metrics_2050_nodredge_RCP4_normal <- writeRaster(inun_metrics_2050_nodredge_RCP4_normal, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_ND_RCP4_normal_inun_metrics_.grd", overwrite=TRUE)

#NoDredge, RCP4.5, 2050, Normal, Q66  , MSL
summ_2050_nodredge_RCP4_normal_Q66 <- inundation(elv = elv_m_2050_ND_RCP4_Q66, lev = lev_2050_normal_RCP4[lev_2050_normal_RCP4$season=="S",])
names(summ_2050_nodredge_RCP4_normal_Q66) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                           "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2050_nodredge_RCP4_normal_Q66 <- inundation(elv = elv_m_2050_ND_RCP4_Q66, lev = lev_2050_normal_RCP4[lev_2050_normal_RCP4$season=="W",])
names(wint_2050_nodredge_RCP4_normal_Q66) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                           "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2050_nodredge_RCP4_normal_Q66 <- stack(summ_2050_nodredge_RCP4_normal_Q66, wint_2050_nodredge_RCP4_normal_Q66, elv_m_2050_ND_RCP4_Q66)
names(inun_metrics_2050_nodredge_RCP4_normal_Q66[[15]]) <- "elv_m"
inun_metrics_2050_nodredge_RCP4_normal_Q66 <- writeRaster(inun_metrics_2050_nodredge_RCP4_normal_Q66, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_ND_RCP4_normal_Q66_inun_metrics_.grd", overwrite=TRUE)

#NoDredge, RCP4.5, 2050, El nino, Q66  , MSL
summ_2050_nodredge_RCP4_elnino_Q66 <- inundation(elv = elv_m_2050_ND_RCP4_Q66, lev = lev_2050_elnino_RCP4[lev_2050_elnino_RCP4$season=="S",])
names(summ_2050_nodredge_RCP4_elnino_Q66) <- c("summ_avg_ht_above_m", "summ_avg_ht_below_m", "summ_avg_ht", 
                                               "summ_max_ht_above_wl", "summ_max_depth_below_wl", "summ_inu_pct", "summ_inu_pct_sq")
wint_2050_nodredge_RCP4_elnino_Q66 <- inundation(elv = elv_m_2050_ND_RCP4_Q66, lev = lev_2050_elnino_RCP4[lev_2050_elnino_RCP4$season=="W",])
names(wint_2050_nodredge_RCP4_elnino_Q66) <- c("wint_avg_ht_above_m", "wint_avg_ht_below_m", "wint_avg_ht", 
                                               "wint_max_ht_above_wl", "wint_max_depth_below_wl", "wint_inu_pct", "wint_inu_pct_sq")
inun_metrics_2050_nodredge_RCP4_elnino_Q66 <- stack(summ_2050_nodredge_RCP4_elnino_Q66, wint_2050_nodredge_RCP4_elnino_Q66, elv_m_2050_ND_RCP4_Q66)
names(inun_metrics_2050_nodredge_RCP4_elnino_Q66[[15]]) <- "elv_m"
inun_metrics_2050_nodredge_RCP4_elnino_Q66 <- writeRaster(inun_metrics_2050_nodredge_RCP4_elnino_Q66, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_ND_RCP4_elnino_Q66_inun_metrics_.grd", overwrite=TRUE)




# read in models for the veg prediction
#SPSP_MAX
load(file = "SPSP_MAX_model.rda")

#function to predict SPSP_MAX based on model for each years inundatin metrics
spsp_predict <- function(rasterstack){
  
  sp_max_ct <- raster::predict(rasterstack, mdl_sp_max, type = "response", progress = "text")
  #predict prob that its a zero
  sp_max_zr <- raster::predict(rasterstack, mdl_sp_max, type = "zero", progress = "text")
  #replace values with 0 or 1
  values(sp_max_zr)[values(sp_max_zr)>0.5] = 0
  values(sp_max_zr)[values(sp_max_zr)<=0.5 & values(sp_max_zr)>0] = 1
  #final raster: if zero>0.5 then 0, else count
  sp_max <- sp_max_zr * sp_max_ct
  plot(sp_max)
  return(sp_max)
  
}

# predict veg for all years and stack for each year - want for each year a stack of veg metrics to use in the LRFF and BSS species occupancy model

sp_max_2012 <- spsp_predict(rasterstack = inun_metrics_12)

inun_metrics_11 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2011_inun_metrics_.grd")
sp_max_2011 <- spsp_predict(rasterstack = inun_metrics_11)

inun_metrics_13 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2013_inun_metrics_.grd")
sp_max_2013 <- spsp_predict(rasterstack = inun_metrics_13)

inun_metrics_14 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2014_inun_metrics_.grd")
sp_max_2014 <- spsp_predict(rasterstack = inun_metrics_14)

inun_metrics_15 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2015_inun_metrics_.grd")
sp_max_2015 <- spsp_predict(rasterstack = inun_metrics_15)

inun_metrics_16 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2016_inun_metrics_.grd")
sp_max_2016 <- spsp_predict(rasterstack = inun_metrics_16)

inun_metrics_17 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2017_inun_metrics_.grd")
sp_max_2017 <- spsp_predict(rasterstack = inun_metrics_17)

inun_metrics_18 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2018_inun_metrics_.grd")
sp_max_2018 <- spsp_predict(rasterstack = inun_metrics_18)

#stack rasters so there is a stack of the max height of Spartina each year - this will be used in the model to predict the probability of LFRR occurrence
sp_max_stack <- stack(sp_max_2011, sp_max_2012, sp_max_2013, sp_max_2014, sp_max_2015,sp_max_2016, sp_max_2017, sp_max_2018)

names(sp_max_stack) <- c("sp_max_2011", "sp_max_2012", "sp_max_2013", "sp_max_2014", "sp_max_2015", "sp_max_2016", "sp_max_2017", "sp_max_2018") 

writeRaster(sp_max_stack, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/sp_max_stack.grd", format = "raster")
sp_max_stack <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/sp_max_stack.grd")

plot(sp_max_stack)
sp_max_stack_stats <- data.frame(cellStats(sp_max_stack, quantile),
                                 "median" = cellStats(sp_max_stack, median),
                                 "stdev" = cellStats(sp_max_stack, sd))
boxplot(sp_max_stack, notch = T)



#2015 baseline year for validation with Delft3d
inun_metrics_2015_delft3D  <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/__2015_delft3D_inun_metrics_.grd")
sp_max_2015_delft3D <- spsp_predict(rasterstack = inun_metrics_2015_delft3D)
plot(sp_max_2015_delft3D)
writeRaster(sp_max_2015_delft3D, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/sp_max_2015_delft3D.grd")


#Future year, 2100
inun_metrics_2100_dre_RCP8_elnino  <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_dredge_RCP8_elnino_inun_metrics_.grd")
sp_max_2100_dre_RCP8_elnino <- spsp_predict(rasterstack = inun_metrics_2100_dre_RCP8_elnino)

inun_metrics_2100_dre_RCP8_normal <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_dredge_RCP8_normal_inun_metrics_.grd")
sp_max_2100_dre_RCP8_normal <- spsp_predict(rasterstack = inun_metrics_2100_dre_RCP8_normal)

inun_metrics_2100_dre_RCP4_elnino <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_dredge_RCP4_elnino_inun_metrics_.grd")
sp_max_2100_dre_RCP4_elnino <- spsp_predict(rasterstack = inun_metrics_2100_dre_RCP4_elnino)

inun_metrics_2100_dre_RCP4_normal <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_dredge_RCP4_normal_inun_metrics_.grd")
sp_max_2100_dre_RCP4_normal <- spsp_predict(rasterstack = inun_metrics_2100_dre_RCP4_normal)

inun_metrics_2100_ND_RCP8_elnino <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_ND_RCP8_elnino_inun_metrics_.grd")
sp_max_2100_ND_RCP8_elnino <- spsp_predict(rasterstack = inun_metrics_2100_ND_RCP8_elnino)

inun_metrics_2100_ND_RCP8_normal <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_ND_RCP8_normal_inun_metrics_.grd")
sp_max_2100_ND_RCP8_normal <- spsp_predict(rasterstack = inun_metrics_2100_ND_RCP8_normal)

inun_metrics_2100_ND_RCP4_elnino <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_ND_RCP4_elnino_inun_metrics_.grd")
sp_max_2100_ND_RCP4_elnino <- spsp_predict(rasterstack = inun_metrics_2100_ND_RCP4_elnino)

inun_metrics_2100_ND_RCP4_normal <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_ND_RCP4_normal_inun_metrics_.grd")
sp_max_2100_ND_RCP4_normal <- spsp_predict(rasterstack = inun_metrics_2100_ND_RCP4_normal)

inun_metrics_2100_ND_RCP4_elnino_Q66 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_ND_RCP4_elnino_Q66_inun_metrics_.grd")
sp_max_2100_ND_RCP4_elnino_Q66 <- spsp_predict(rasterstack = inun_metrics_2100_ND_RCP4_elnino_Q66)

inun_metrics_2100_ND_RCP4_normal_Q66 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_ND_RCP4_normal_Q66_inun_metrics_.grd")
sp_max_2100_ND_RCP4_normal_Q66 <- spsp_predict(rasterstack = inun_metrics_2100_ND_RCP4_normal_Q66)

inun_metrics_2100_dre_RCP8_elnino_Q33 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_dredge_RCP8_elnino_Q33_inun_metrics_.grd")
sp_max_2100_dre_RCP8_elnino_Q33 <- spsp_predict(rasterstack = inun_metrics_2100_dre_RCP8_elnino_Q33)

inun_metrics_2100_dre_RCP8_normal_Q33 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_dredge_RCP8_normal_Q33_inun_metrics_.grd")
sp_max_2100_dre_RCP8_normal_Q33 <- spsp_predict(rasterstack = inun_metrics_2100_dre_RCP8_normal_Q33)

CP_inun_metrics_2100_dre_RCP4_normal <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_dredge_RCP4_normal_inun_metrics_.grd")
sp_max_2100_dre_RCP4_normal_CP <- spsp_predict(rasterstack = CP_inun_metrics_2100_dre_RCP4_normal)

CP_inun_metrics_2100_dre_RCP4_elnino <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_dredge_RCP4_elnino_inun_metrics_.grd")
sp_max_2100_dre_RCP4_elnino_CP <- spsp_predict(rasterstack = CP_inun_metrics_2100_dre_RCP4_elnino)

CP_inun_metrics_2100_dre_RCP8_normal <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_dredge_RCP8_normal_inun_metrics_.grd")
sp_max_2100_dre_RCP8_normal_CP <- spsp_predict(rasterstack = CP_inun_metrics_2100_dre_RCP8_normal)

CP_inun_metrics_2100_dre_RCP8_elnino <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_dredge_RCP8_elnino_inun_metrics_.grd")
sp_max_2100_dre_RCP8_elnino_CP <- spsp_predict(rasterstack = CP_inun_metrics_2100_dre_RCP8_elnino)

CP_inun_metrics_2100_dre_RCP8_normal_Q33 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_dredge_RCP8_normal_Q33_inun_metrics_.grd")
sp_max_2100_dre_RCP8_normal_CP_Q33 <- spsp_predict(rasterstack = CP_inun_metrics_2100_dre_RCP8_normal_Q33)

CP_inun_metrics_2100_dre_RCP8_elnino_Q33 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_dredge_RCP8_elnino_Q33_inun_metrics_.grd")
sp_max_2100_dre_RCP8_elnino_CP_Q33 <- spsp_predict(rasterstack = CP_inun_metrics_2100_dre_RCP8_elnino_Q33)

CP_inun_metrics_2100_ND_RCP4_normal <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_ND_RCP4_normal_inun_metrics_.grd")
sp_max_2100_ND_RCP4_normal_CP <- spsp_predict(rasterstack = CP_inun_metrics_2100_ND_RCP4_normal)

CP_inun_metrics_2100_ND_RCP4_elnino <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_ND_RCP4_elnino_inun_metrics_.grd")
sp_max_2100_ND_RCP4_elnino_CP <- spsp_predict(rasterstack = CP_inun_metrics_2100_ND_RCP4_elnino)

CP_inun_metrics_2100_ND_RCP8_normal <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_ND_RCP8_normal_inun_metrics_.grd")
sp_max_2100_ND_RCP8_normal_CP <- spsp_predict(rasterstack = CP_inun_metrics_2100_ND_RCP8_normal)

CP_inun_metrics_2100_ND_RCP8_elnino <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_ND_RCP8_elnino_inun_metrics_.grd")
sp_max_2100_ND_RCP8_elnino_CP <- spsp_predict(rasterstack = CP_inun_metrics_2100_ND_RCP8_elnino)

CP_inun_metrics_2100_ND_RCP4_normal_Q66 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_ND_RCP4_normal_Q66_inun_metrics_.grd")
sp_max_2100_ND_RCP4_normal_CP_Q66 <- spsp_predict(rasterstack = CP_inun_metrics_2100_ND_RCP4_normal_Q66)

CP_inun_metrics_2100_ND_RCP4_elnino_Q66 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/CP_2100_ND_RCP4_elnino_Q66_inun_metrics_.grd")
sp_max_2100_ND_RCP4_elnino_CP_Q66 <- spsp_predict(rasterstack = CP_inun_metrics_2100_ND_RCP4_elnino_Q66)





#Future year, 2050
inun_metrics_2050_dre_RCP8_elnino  <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_dredge_RCP8_elnino_inun_metrics_.grd")
sp_max_2050_dre_RCP8_elnino <- spsp_predict(rasterstack = inun_metrics_2050_dre_RCP8_elnino)

inun_metrics_2050_dre_RCP8_normal <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_dredge_RCP8_normal_inun_metrics_.grd")
sp_max_2050_dre_RCP8_normal <- spsp_predict(rasterstack = inun_metrics_2050_dre_RCP8_normal)

inun_metrics_2050_dre_RCP4_elnino <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_dredge_RCP4_elnino_inun_metrics_.grd")
sp_max_2050_dre_RCP4_elnino <- spsp_predict(rasterstack = inun_metrics_2050_dre_RCP4_elnino)

inun_metrics_2050_dre_RCP4_normal <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_dredge_RCP4_normal_inun_metrics_.grd")
sp_max_2050_dre_RCP4_normal <- spsp_predict(rasterstack = inun_metrics_2050_dre_RCP4_normal)

inun_metrics_2050_ND_RCP8_elnino <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_ND_RCP8_elnino_inun_metrics_.grd")
sp_max_2050_ND_RCP8_elnino <- spsp_predict(rasterstack = inun_metrics_2050_ND_RCP8_elnino)

inun_metrics_2050_ND_RCP8_normal <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_ND_RCP8_normal_inun_metrics_.grd")
sp_max_2050_ND_RCP8_normal <- spsp_predict(rasterstack = inun_metrics_2050_ND_RCP8_normal)

inun_metrics_2050_ND_RCP4_elnino <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_ND_RCP4_elnino_inun_metrics_.grd")
sp_max_2050_ND_RCP4_elnino <- spsp_predict(rasterstack = inun_metrics_2050_ND_RCP4_elnino)

inun_metrics_2050_ND_RCP4_normal <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_ND_RCP4_normal_inun_metrics_.grd")
sp_max_2050_ND_RCP4_normal <- spsp_predict(rasterstack = inun_metrics_2050_ND_RCP4_normal)

inun_metrics_2050_ND_RCP4_elnino_Q66 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_ND_RCP4_elnino_Q66_inun_metrics_.grd")
sp_max_2050_ND_RCP4_elnino_Q66 <- spsp_predict(rasterstack = inun_metrics_2050_ND_RCP4_elnino_Q66)

inun_metrics_2050_ND_RCP4_normal_Q66 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_ND_RCP4_normal_Q66_inun_metrics_.grd")
sp_max_2050_ND_RCP4_normal_Q66 <- spsp_predict(rasterstack = inun_metrics_2050_ND_RCP4_normal_Q66)

inun_metrics_2050_dre_RCP8_elnino_Q33 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_dredge_RCP8_elnino_Q33_inun_metrics_.grd")
sp_max_2050_dre_RCP8_elnino_Q33 <- spsp_predict(rasterstack = inun_metrics_2050_dre_RCP8_elnino_Q33)

inun_metrics_2050_dre_RCP8_normal_Q33 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2050_dredge_RCP8_normal_Q33_inun_metrics_.grd")
sp_max_2050_dre_RCP8_normal_Q33 <- spsp_predict(rasterstack = inun_metrics_2050_dre_RCP8_normal_Q33)


#the extents of the CP_files are slightly different than the delft3d files.. need to fix
#make stack of the %summer inundation
perc_summ_inund_fut <- stack(inun_metrics_2050_ND_RCP4_normal[[6]], inun_metrics_2050_ND_RCP4_elnino[[6]],
                             inun_metrics_2050_ND_RCP8_normal[[6]], inun_metrics_2050_ND_RCP8_elnino[[6]],
                             inun_metrics_2050_dre_RCP4_normal[[6]], inun_metrics_2050_dre_RCP4_elnino[[6]],
                             inun_metrics_2050_dre_RCP8_normal[[6]], inun_metrics_2050_dre_RCP8_elnino[[6]],
                             inun_metrics_2050_ND_RCP4_normal_Q66[[6]], inun_metrics_2050_ND_RCP4_elnino_Q66[[6]],
                             inun_metrics_2050_dre_RCP8_normal_Q33[[6]], inun_metrics_2050_dre_RCP8_elnino_Q33[[6]],
                             inun_metrics_2100_ND_RCP4_normal[[6]], inun_metrics_2100_ND_RCP4_elnino[[6]],
                             inun_metrics_2100_ND_RCP8_normal[[6]], inun_metrics_2100_ND_RCP8_elnino[[6]],
                             inun_metrics_2100_dre_RCP4_normal[[6]], inun_metrics_2100_dre_RCP4_elnino[[6]],
                             inun_metrics_2100_dre_RCP8_normal[[6]], inun_metrics_2100_dre_RCP8_elnino[[6]],
                             inun_metrics_2100_ND_RCP4_normal_Q66[[6]], inun_metrics_2100_ND_RCP4_elnino_Q66[[6]],
                             inun_metrics_2100_dre_RCP8_normal_Q33[[6]], inun_metrics_2100_dre_RCP8_elnino_Q33[[6]],
                             CP_inun_metrics_2100_dre_RCP4_normal[[6]], CP_inun_metrics_2100_dre_RCP4_elnino[[6]],
                             CP_inun_metrics_2100_dre_RCP8_normal[[6]], CP_inun_metrics_2100_dre_RCP8_elnino[[6]],
                             CP_inun_metrics_2100_dre_RCP8_normal_Q33[[6]], CP_inun_metrics_2100_dre_RCP8_elnino_Q33[[6]],
                             CP_inun_metrics_2100_ND_RCP4_normal[[6]], CP_inun_metrics_2100_ND_RCP4_elnino[[6]],
                             CP_inun_metrics_2100_ND_RCP8_normal[[6]], CP_inun_metrics_2100_ND_RCP8_elnino[[6]],
                             CP_inun_metrics_2100_ND_RCP4_normal_Q66[[6]], CP_inun_metrics_2100_ND_RCP4_elnino_Q66[[6]] 
                             )
#DL = delft3D, CP = coupled delft+warmer.
names(perc_summ_inund_fut) <- c("DL_perc_summ_inun_2050_ND_RCP4_normal_Q50", "DL_perc_summ_inun_2050_ND_RCP4_elnino_Q50",
                                "DL_perc_summ_inun_2050_ND_RCP8_normal_Q50", "DL_perc_summ_inun_2050_ND_RCP8_elnino_Q50",
                                "DL_perc_summ_inun_2050_dre_RCP4_normal_Q50", "DL_perc_summ_inun_2050_dre_RCP4_elnino_Q50",
                                "DL_perc_summ_inun_2050_dre_RCP8_normal_Q50", "DL_perc_summ_inun_2050_dre_RCP8_elnino_Q50",
                                "DL_perc_summ_inun_2050_ND_RCP4_normal_Q66", "DL_perc_summ_inun_2050_ND_RCP4_elnino_Q66",
                                "DL_perc_summ_inun_2050_dre_RCP8_normal_Q33", "DL_perc_summ_inuns_2050_dre_RCP8_elnino_Q33",
                                "DL_perc_summ_inun_2100_ND_RCP4_normal_Q50", "DL_perc_summ_inun_2100_ND_RCP4_elnino_Q50",
                                "DL_perc_summ_inun_2100_ND_RCP8_normal_Q50", "DL_perc_summ_inun_2100_ND_RCP8_elnino_Q50",
                                "DL_perc_summ_inun_2100_dre_RCP4_normal_Q50", "DL_perc_summ_inun_2100_dre_RCP4_elnino_Q50",
                                "DL_perc_summ_inun_2100_dre_RCP8_normal_Q50", "DL_perc_summ_inun_2100_dre_RCP8_elnino_Q50",
                                "DL_perc_summ_inun_2100_ND_RCP4_normal_Q66", "DL_perc_summ_inun_2100_ND_RCP4_elnino_Q66",
                                "DL_perc_summ_inun_2100_dre_RCP8_normal_Q33", "DL_perc_summ_inun_2100_dre_RCP8_elnino_Q33",
                                "CP_perc_summ_inun_2100_dre_RCP4_normal_Q50", "CP_perc_summ_inun_2100_dre_RCP4_elnino_Q50",
                                "CP_perc_summ_inun_2100_dre_RCP8_normal_Q50", "CP_perc_summ_inun_2100_dre_RCP8_elnino_Q50",
                                "CP_perc_summ_inun_2100_dre_RCP8_normal_Q33", "CP_perc_summ_inun_2100_dre_RCP8_elnino_Q33",
                                "CP_perc_summ_inun_2100_ND_RCP4_normal_Q50", "CP_perc_summ_inun_2100_ND_RCP4_elnino_Q50",
                                "CP_perc_summ_inun_2100_ND_RCP8_normal_Q50", "CP_perc_summ_inun_2100_ND_RCP8_elnino_Q50",
                                "CP_perc_summ_inun_2100_ND_RCP4_normal_Q66", "CP_perc_summ_inun_2100_ND_RCP4_elnino_Q66")

writeRaster(perc_summ_inund_fut, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/perc_summ_inund_fut.grd", format = "raster", overwrite = TRUE)
perc_summ_inund_fut <- raster::stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/perc_summ_inund_fut.grd")




#make stack of spartina height
sp_max_stack_fut <- stack(sp_max_2100_dre_RCP8_elnino, sp_max_2100_dre_RCP8_normal, 
                          sp_max_2100_dre_RCP4_elnino, sp_max_2100_dre_RCP4_normal, 
                          sp_max_2100_ND_RCP8_elnino,sp_max_2100_ND_RCP8_normal,
                          sp_max_2100_ND_RCP4_elnino, sp_max_2100_ND_RCP4_normal,
                          sp_max_2100_dre_RCP8_elnino_Q33, sp_max_2100_dre_RCP8_normal_Q33,
                          sp_max_2100_ND_RCP4_elnino_Q66, sp_max_2100_ND_RCP4_normal_Q66,
                          sp_max_2050_dre_RCP8_elnino, sp_max_2050_dre_RCP8_normal,
                          sp_max_2050_dre_RCP4_elnino, sp_max_2050_dre_RCP4_normal,
                          sp_max_2050_ND_RCP8_elnino, sp_max_2050_ND_RCP8_normal,
                          sp_max_2050_ND_RCP4_elnino, sp_max_2050_ND_RCP4_normal,
                          sp_max_2050_dre_RCP8_elnino_Q33, sp_max_2050_dre_RCP8_normal_Q33,
                          sp_max_2050_ND_RCP4_elnino_Q66, sp_max_2050_ND_RCP4_normal_Q66,
                          sp_max_2100_dre_RCP4_normal_CP, sp_max_2100_dre_RCP4_elnino_CP,
                          sp_max_2100_dre_RCP8_normal_CP, sp_max_2100_dre_RCP8_elnino_CP,
                          sp_max_2100_dre_RCP8_normal_CP_Q33, sp_max_2100_dre_RCP8_elnino_CP_Q33,
                          sp_max_2100_ND_RCP4_normal_CP, sp_max_2100_ND_RCP4_elnino_CP,
                          sp_max_2100_ND_RCP8_normal_CP, sp_max_2100_ND_RCP8_elnino_CP,
                          sp_max_2100_ND_RCP4_normal_CP_Q66, sp_max_2100_ND_RCP4_elnino_CP_Q66)

names(sp_max_stack_fut) <- c("sp_max_2100_dre_RCP8_elnino_Q50_DL", "sp_max_2100_dre_RCP8_normal_Q50_DL", 
                             "sp_max_2100_dre_RCP4_elnino_Q50_DL", "sp_max_2100_dre_RCP4_normal_Q50_DL", 
                             "sp_max_2100_ND_RCP8_elnino_Q50_DL", "sp_max_2100_ND_RCP8_normal_Q50_DL",
                             "sp_max_2100_ND_RCP4_elnino_Q50_DL", "sp_max_2100_ND_RCP4_normal_Q50_DL",
                             "sp_max_2100_dre_RCP8_elnino_Q33_DL", "sp_max_2100_dre_RCP8_normal_Q33_DL",
                             "sp_max_2100_ND_RCP4_elnino_Q66_DL", "sp_max_2100_ND_RCP4_normal_Q66_DL",
                             "sp_max_2050_dre_RCP8_elnino_Q50_DL", "sp_max_2050_dre_RCP8_normal_Q50_DL", 
                             "sp_max_2050_dre_RCP4_elnino_Q50_DL", "sp_max_2050_dre_RCP4_normal_Q50_DL", 
                             "sp_max_2050_ND_RCP8_elnino_Q50_DL", "sp_max_2050_ND_RCP8_normal_Q50_DL",
                             "sp_max_2050_ND_RCP4_elnino_Q50_DL", "sp_max_2050_ND_RCP4_normal_Q50_DL",
                             "sp_max_2050_dre_RCP8_elnino_Q33_DL", "sp_max_2050_dre_RCP8_normal_Q33_DL",
                             "sp_max_2050_ND_RCP4_elnino_Q66_DL", "sp_max_2050_ND_RCP4_normal_Q66_DL",
                             "sp_max_2100_dre_RCP4_normal_Q50_CP", "sp_max_2100_dre_RCP4_elnino_Q50_CP",
                             "sp_max_2100_dre_RCP8_normal_Q50_CP", "sp_max_2100_dre_RCP8_elnino_Q50_CP",
                             "sp_max_2100_dre_RCP8_normal_Q33_CP", "sp_max_2100_dre_RCP8_elnino_Q33_CP",
                             "sp_max_2100_ND_RCP4_normal_Q50_CP", "sp_max_2100_ND_RCP4_elnino_Q50_CP",
                             "sp_max_2100_ND_RCP8_normal_Q50_CP", "sp_max_2100_ND_RCP8_elnino_Q50_CP",
                             "sp_max_2100_ND_RCP4_normal_Q66_CP", "sp_max_2100_ND_RCP4_elnino_Q66_CP") 

writeRaster(sp_max_stack_fut, "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/sp_max_stack_fut.grd", format = "raster", overwrite = TRUE)
sp_max_stack_fut <- raster::stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/sp_max_stack_fut.grd")

t <- data.frame(cellStats(sp_max_stack_fut, quantile))





###################################################################################################
###################################################################################################

###################### model 2: Predicting site occupancy in Marsh using vegetation ###############

###################################################################################################
###################################################################################################





#Predict the percentage of years a LFRR will occupy an acre using the SPSP max metrics calculated per acre plot

#read in data showing the max height of spartina
sp_max_stack <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/sp_max_stack.grd")


#  presence and absence acres  for each year for LFRR and BSS
load("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/FinalData/rail_beld_elv_veg.RData") 
bird_dat <- dat %>% 
  dplyr::select(UID:spar_num_pairs)
rm(dat)
#read in UNB file to make the bird data spatial
UNB <- st_read("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/FinalData/UNB_Grid.shp")
bird_dat <- left_join(UNB, bird_dat, by = "UID")
plot(bird_dat["yrs_rail_pres"], main = "LFRR Occupancy (Years)", key.pos = 1, 
     key.width = lcm(1.3), key.length = 0.5)




# extract from vegetation rasters  5, 25, 50, 75, and 99th percentiles as a way to describe the dist of that metric
# within the acre plot for each year - these metrics will get paired up with the bird data from the following year!

#crop the bird data to be just where veg data is
veg_boundary <- st_read("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/UNB Vegetation/Veg_boundary.shp")
veg_boundary <- as(veg_boundary, "Spatial") #veg boundary convert to spatial object
bird_dat_sp <- as(bird_dat, "Spatial")#convert bird object to spatial for cropping
veg_boundary <- spTransform(veg_boundary, CRSobj = crs(bird_dat_sp))
bird_dat_sp <- raster::intersect(veg_boundary, bird_dat_sp ) #crop to extent
plot(bird_dat_sp)
bird_dat <- st_as_sf(bird_dat_sp)
plot(bird_dat["yrs_rail_pres"])

# function to take the min, 5, 25, 50, 75, and 99th percentiles, max, and min of each acre plot 
#need to work on this section, want to ignore NAs in the functions


#med

#function for 50th percentile
medin <- function(x,...){
  d <- quantile(x, probs = 0.50, na.rm = TRUE)
  return(d)
}

bird_dat_inundation<-cbind(bird_dat, raster::extract(x = sp_max_stack, y = bird_dat, fun = medin, layer = 1, nl = 8))
bird_dat_inundation$SPSP_MAX_50 <- ifelse(bird_dat_inundation$year==2011,bird_dat_inundation$sp_max_2011, NA)
bird_dat_inundation$SPSP_MAX_50 <- ifelse(bird_dat_inundation$year==2012,bird_dat_inundation$sp_max_2012, bird_dat_inundation$SPSP_MAX_50)
bird_dat_inundation$SPSP_MAX_50 <- ifelse(bird_dat_inundation$year==2013,bird_dat_inundation$sp_max_2013, bird_dat_inundation$SPSP_MAX_50)
bird_dat_inundation$SPSP_MAX_50 <- ifelse(bird_dat_inundation$year==2014,bird_dat_inundation$sp_max_2014, bird_dat_inundation$SPSP_MAX_50)
bird_dat_inundation$SPSP_MAX_50 <- ifelse(bird_dat_inundation$year==2015,bird_dat_inundation$sp_max_2015, bird_dat_inundation$SPSP_MAX_50)
bird_dat_inundation$SPSP_MAX_50 <- ifelse(bird_dat_inundation$year==2016,bird_dat_inundation$sp_max_2016, bird_dat_inundation$SPSP_MAX_50)
bird_dat_inundation$SPSP_MAX_50 <- ifelse(bird_dat_inundation$year==2017,bird_dat_inundation$sp_max_2017, bird_dat_inundation$SPSP_MAX_50)
bird_dat_inundation$SPSP_MAX_50 <- ifelse(bird_dat_inundation$year==2018,bird_dat_inundation$sp_max_2018, bird_dat_inundation$SPSP_MAX_50)
bird_dat_inundation <- bird_dat_inundation %>% 
  dplyr::select(-(sp_max_2011:sp_max_2018))

#75% percentile

#function for 75th percentile
seventyfifthpercentile <- function(x,...){
  d <- quantile(x, probs = 0.75, na.rm = TRUE)
  return(d)
}

bird_dat_inundation<-cbind(bird_dat_inundation, raster::extract(x = sp_max_stack, y = bird_dat, fun = seventyfifthpercentile, layer = 1, nl = 8))
bird_dat_inundation$SPSP_MAX_75 <- ifelse(bird_dat_inundation$year==2011,bird_dat_inundation$sp_max_2011, NA)
bird_dat_inundation$SPSP_MAX_75 <- ifelse(bird_dat_inundation$year==2012,bird_dat_inundation$sp_max_2012, bird_dat_inundation$SPSP_MAX_75)
bird_dat_inundation$SPSP_MAX_75 <- ifelse(bird_dat_inundation$year==2013,bird_dat_inundation$sp_max_2013, bird_dat_inundation$SPSP_MAX_75)
bird_dat_inundation$SPSP_MAX_75 <- ifelse(bird_dat_inundation$year==2014,bird_dat_inundation$sp_max_2014, bird_dat_inundation$SPSP_MAX_75)
bird_dat_inundation$SPSP_MAX_75 <- ifelse(bird_dat_inundation$year==2015,bird_dat_inundation$sp_max_2015, bird_dat_inundation$SPSP_MAX_75)
bird_dat_inundation$SPSP_MAX_75 <- ifelse(bird_dat_inundation$year==2016,bird_dat_inundation$sp_max_2016, bird_dat_inundation$SPSP_MAX_75)
bird_dat_inundation$SPSP_MAX_75 <- ifelse(bird_dat_inundation$year==2017,bird_dat_inundation$sp_max_2017, bird_dat_inundation$SPSP_MAX_75)
bird_dat_inundation$SPSP_MAX_75 <- ifelse(bird_dat_inundation$year==2018,bird_dat_inundation$sp_max_2018, bird_dat_inundation$SPSP_MAX_75)
bird_dat_inundation <- bird_dat_inundation %>% 
  dplyr::select(-(sp_max_2011:sp_max_2018))

#95% percentile

#function for 95th percentile
nintyfifthpercentile <- function(x,...){
  d <- quantile(x, probs = 0.95, na.rm = TRUE)
  return(d)
}

bird_dat_inundation<-cbind(bird_dat_inundation, raster::extract(x = sp_max_stack, y = bird_dat, fun = nintyfifthpercentile, layer = 1, nl = 8))
bird_dat_inundation$SPSP_MAX_95 <- ifelse(bird_dat_inundation$year==2011,bird_dat_inundation$sp_max_2011, NA)
bird_dat_inundation$SPSP_MAX_95 <- ifelse(bird_dat_inundation$year==2012,bird_dat_inundation$sp_max_2012, bird_dat_inundation$SPSP_MAX_95)
bird_dat_inundation$SPSP_MAX_95 <- ifelse(bird_dat_inundation$year==2013,bird_dat_inundation$sp_max_2013, bird_dat_inundation$SPSP_MAX_95)
bird_dat_inundation$SPSP_MAX_95 <- ifelse(bird_dat_inundation$year==2014,bird_dat_inundation$sp_max_2014, bird_dat_inundation$SPSP_MAX_95)
bird_dat_inundation$SPSP_MAX_95 <- ifelse(bird_dat_inundation$year==2015,bird_dat_inundation$sp_max_2015, bird_dat_inundation$SPSP_MAX_95)
bird_dat_inundation$SPSP_MAX_95 <- ifelse(bird_dat_inundation$year==2016,bird_dat_inundation$sp_max_2016, bird_dat_inundation$SPSP_MAX_95)
bird_dat_inundation$SPSP_MAX_95 <- ifelse(bird_dat_inundation$year==2017,bird_dat_inundation$sp_max_2017, bird_dat_inundation$SPSP_MAX_95)
bird_dat_inundation$SPSP_MAX_95 <- ifelse(bird_dat_inundation$year==2018,bird_dat_inundation$sp_max_2018, bird_dat_inundation$SPSP_MAX_95)
bird_dat_inundation <- bird_dat_inundation %>% 
  dplyr::select(-(sp_max_2011:sp_max_2018))

#function for 5th percentile
fifthpercentile <- function(x,...){
  d <- quantile(x, probs = 0.05, na.rm = TRUE)
  return(d)
}

bird_dat_inundation<-cbind(bird_dat_inundation, raster::extract(x = sp_max_stack, y = bird_dat, fun = fifthpercentile, layer = 1, nl = 8))
bird_dat_inundation$SPSP_MAX_05 <- ifelse(bird_dat_inundation$year==2011,bird_dat_inundation$sp_max_2011, NA)
bird_dat_inundation$SPSP_MAX_05 <- ifelse(bird_dat_inundation$year==2012,bird_dat_inundation$sp_max_2012, bird_dat_inundation$SPSP_MAX_05)
bird_dat_inundation$SPSP_MAX_05 <- ifelse(bird_dat_inundation$year==2013,bird_dat_inundation$sp_max_2013, bird_dat_inundation$SPSP_MAX_05)
bird_dat_inundation$SPSP_MAX_05 <- ifelse(bird_dat_inundation$year==2014,bird_dat_inundation$sp_max_2014, bird_dat_inundation$SPSP_MAX_05)
bird_dat_inundation$SPSP_MAX_05 <- ifelse(bird_dat_inundation$year==2015,bird_dat_inundation$sp_max_2015, bird_dat_inundation$SPSP_MAX_05)
bird_dat_inundation$SPSP_MAX_05 <- ifelse(bird_dat_inundation$year==2016,bird_dat_inundation$sp_max_2016, bird_dat_inundation$SPSP_MAX_05)
bird_dat_inundation$SPSP_MAX_05 <- ifelse(bird_dat_inundation$year==2017,bird_dat_inundation$sp_max_2017, bird_dat_inundation$SPSP_MAX_05)
bird_dat_inundation$SPSP_MAX_05 <- ifelse(bird_dat_inundation$year==2018,bird_dat_inundation$sp_max_2018, bird_dat_inundation$SPSP_MAX_05)
bird_dat_inundation <- bird_dat_inundation %>% 
  dplyr::select(-(sp_max_2011:sp_max_2018))

#function for 25th percentile
twentyfifthpercentile <- function(x,...){
  d <- quantile(x, probs = 0.25, na.rm = TRUE)
  return(d)
}

bird_dat_inundation<-cbind(bird_dat_inundation, raster::extract(x = sp_max_stack, y = bird_dat, fun = twentyfifthpercentile, layer = 1, nl = 8))
bird_dat_inundation$SPSP_MAX_25 <- ifelse(bird_dat_inundation$year==2011,bird_dat_inundation$sp_max_2011, NA)
bird_dat_inundation$SPSP_MAX_25 <- ifelse(bird_dat_inundation$year==2012,bird_dat_inundation$sp_max_2012, bird_dat_inundation$SPSP_MAX_25)
bird_dat_inundation$SPSP_MAX_25 <- ifelse(bird_dat_inundation$year==2013,bird_dat_inundation$sp_max_2013, bird_dat_inundation$SPSP_MAX_25)
bird_dat_inundation$SPSP_MAX_25 <- ifelse(bird_dat_inundation$year==2014,bird_dat_inundation$sp_max_2014, bird_dat_inundation$SPSP_MAX_25)
bird_dat_inundation$SPSP_MAX_25 <- ifelse(bird_dat_inundation$year==2015,bird_dat_inundation$sp_max_2015, bird_dat_inundation$SPSP_MAX_25)
bird_dat_inundation$SPSP_MAX_25 <- ifelse(bird_dat_inundation$year==2016,bird_dat_inundation$sp_max_2016, bird_dat_inundation$SPSP_MAX_25)
bird_dat_inundation$SPSP_MAX_25 <- ifelse(bird_dat_inundation$year==2017,bird_dat_inundation$sp_max_2017, bird_dat_inundation$SPSP_MAX_25)
bird_dat_inundation$SPSP_MAX_25 <- ifelse(bird_dat_inundation$year==2018,bird_dat_inundation$sp_max_2018, bird_dat_inundation$SPSP_MAX_25)
bird_dat_inundation <- bird_dat_inundation %>% 
  dplyr::select(-(sp_max_2011:sp_max_2018))

#function for percent greater than 60
perc_grt_60 <- function(x,...){ 
  d <- x[!is.na(x)]
  e <- (sum((d>60)/length(d)))
  return(e)
}

bird_dat_inundation<-cbind(bird_dat_inundation, raster::extract(x = sp_max_stack, y = bird_dat, fun = perc_grt_60, layer = 1, nl = 8))
bird_dat_inundation$SPSP_MAX_grt60 <- ifelse(bird_dat_inundation$year==2011,bird_dat_inundation$sp_max_2011, NA)
bird_dat_inundation$SPSP_MAX_grt60 <- ifelse(bird_dat_inundation$year==2012,bird_dat_inundation$sp_max_2012, bird_dat_inundation$SPSP_MAX_grt60)
bird_dat_inundation$SPSP_MAX_grt60 <- ifelse(bird_dat_inundation$year==2013,bird_dat_inundation$sp_max_2013, bird_dat_inundation$SPSP_MAX_grt60)
bird_dat_inundation$SPSP_MAX_grt60 <- ifelse(bird_dat_inundation$year==2014,bird_dat_inundation$sp_max_2014, bird_dat_inundation$SPSP_MAX_grt60)
bird_dat_inundation$SPSP_MAX_grt60 <- ifelse(bird_dat_inundation$year==2015,bird_dat_inundation$sp_max_2015, bird_dat_inundation$SPSP_MAX_grt60)
bird_dat_inundation$SPSP_MAX_grt60 <- ifelse(bird_dat_inundation$year==2016,bird_dat_inundation$sp_max_2016, bird_dat_inundation$SPSP_MAX_grt60)
bird_dat_inundation$SPSP_MAX_grt60 <- ifelse(bird_dat_inundation$year==2017,bird_dat_inundation$sp_max_2017, bird_dat_inundation$SPSP_MAX_grt60)
bird_dat_inundation$SPSP_MAX_grt60 <- ifelse(bird_dat_inundation$year==2018,bird_dat_inundation$sp_max_2018, bird_dat_inundation$SPSP_MAX_grt60)
bird_dat_inundation <- bird_dat_inundation %>% 
  dplyr::select(-(sp_max_2011:sp_max_2018))

#function for percent greater than 90
perc_grt_90 <- function(x,...){ 
  d <- x[!is.na(x)]
  e <- (sum((d>90)/length(d)))
  return(e)
}

bird_dat_inundation<-cbind(bird_dat_inundation, raster::extract(x = sp_max_stack, y = bird_dat, fun = perc_grt_90, layer = 1, nl = 8))
bird_dat_inundation$SPSP_MAX_grt90 <- ifelse(bird_dat_inundation$year==2011,bird_dat_inundation$sp_max_2011, NA)
bird_dat_inundation$SPSP_MAX_grt90 <- ifelse(bird_dat_inundation$year==2012,bird_dat_inundation$sp_max_2012, bird_dat_inundation$SPSP_MAX_grt90)
bird_dat_inundation$SPSP_MAX_grt90 <- ifelse(bird_dat_inundation$year==2013,bird_dat_inundation$sp_max_2013, bird_dat_inundation$SPSP_MAX_grt90)
bird_dat_inundation$SPSP_MAX_grt90 <- ifelse(bird_dat_inundation$year==2014,bird_dat_inundation$sp_max_2014, bird_dat_inundation$SPSP_MAX_grt90)
bird_dat_inundation$SPSP_MAX_grt90 <- ifelse(bird_dat_inundation$year==2015,bird_dat_inundation$sp_max_2015, bird_dat_inundation$SPSP_MAX_grt90)
bird_dat_inundation$SPSP_MAX_grt90 <- ifelse(bird_dat_inundation$year==2016,bird_dat_inundation$sp_max_2016, bird_dat_inundation$SPSP_MAX_grt90)
bird_dat_inundation$SPSP_MAX_grt90 <- ifelse(bird_dat_inundation$year==2017,bird_dat_inundation$sp_max_2017, bird_dat_inundation$SPSP_MAX_grt90)
bird_dat_inundation$SPSP_MAX_grt90 <- ifelse(bird_dat_inundation$year==2018,bird_dat_inundation$sp_max_2018, bird_dat_inundation$SPSP_MAX_grt90)
bird_dat_inundation <- bird_dat_inundation %>% 
  dplyr::select(-(sp_max_2011:sp_max_2018))

save(bird_dat_inundation, file = "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/bird_dat_inundation_baseline.RData")

#filter out year 2012 bc LRFF data not complete

#model percent of bird occupancy vs spartina metrics
load("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/bird_dat_inundation_baseline.RData")

ggplot(data = bird_dat_inundation, aes(x = SPSP_MAX_95, y = pct_rail_pres))+
  geom_point(alpha = 1/6)+
  geom_smooth(method = "lm")

#Logistic reg model with 8 weights to predict percent rail occurrence
summary(mdl_pct_rail_pres <- glm(pct_rail_pres ~ SPSP_MAX_grt60 + SPSP_MAX_95, 
                                 family = "binomial", 
                                 weights = rep(8, length (pct_rail_pres)), data = bird_dat_inundation))

save(mdl_pct_rail_pres, file = "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/mdl_pct_rail_pres.rda")

#make dataframe with odds ratio, CI, pvalue and coef name
library(broom)
RR_mdl<-tidy(mdl_pct_rail_pres)
RR_mdl<-RR_mdl %>% 
  mutate("odds_ratio"= exp(estimate),
         "lwr_lim"=estimate-1.96*std.error,
         "upr_lim"=estimate+1.96*std.error,
         "OR_lwr_lim"=exp(lwr_lim),
         "OR_upr_lim"=exp(upr_lim)) %>% 
  filter(term !="(Intercept)")

write.csv(RR_mdl, file = "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/LFRR_model_coef.csv")

bird_dat_inundation$predict <- predict(mdl_pct_rail_pres, newdata = bird_dat_inundation, type= "response")
ggplot(data = bird_dat_inundation, aes(x = pct_rail_pres, y = predict))+
  geom_point(alpha = 1/6)
plot(bird_dat_inundation[bird_dat_inundation$year==2011,]["predict"])

#predict on Matts baseline 2015

sp_max_2015_delft3D <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/sp_max_2015_delft3D.grd")
UNB <- st_read("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/FinalData/UNB_Grid.shp") %>% st_zm()
UNB <- as_Spatial(UNB)
UNB <- spTransform(UNB, CRSobj = crs(elv_m_2017_baseline))
UNB <- st_as_sf(UNB)

#extract the approriate metrics (SPSP_MAX_grt60 + SPSP_MAX_95 + SPSP_MAX_grt90) and tidy up data
#grt_90 (proportion of acre with spartina greater than 90cm)
bird_dat_inundation_2015_Delft3D<-cbind(UNB, raster::extract(x = sp_max_2015_delft3D, y = UNB, fun = perc_grt_90))
names(bird_dat_inundation_2015_Delft3D)[2] <- "SPSP_MAX_grt90"

#MAX_95 (95% percentile value of the max spartina height)
bird_dat_inundation_2015_Delft3D_2<-cbind(UNB, raster::extract(x = sp_max_2015_delft3D, y = UNB, fun = nintyfifthpercentile))
names(bird_dat_inundation_2015_Delft3D_2)[2] <- "SPSP_MAX_95"

#MAX_grt60 (proportion of acre with spartina greater than 60cm)
bird_dat_inundation_2015_Delft3D_3<-cbind(UNB, raster::extract(x = sp_max_2015_delft3D, y = UNB, fun = perc_grt_60))
names(bird_dat_inundation_2015_Delft3D_3)[2] <- "SPSP_MAX_grt60"

bird_dat_inundation_2015_Delft3D_4 <- left_join(data.frame(bird_dat_inundation_2015_Delft3D),
                                                data.frame(bird_dat_inundation_2015_Delft3D_2),
                                                by = "UID")
bird_dat_inundation_2015_Delft3D_4 <- left_join(bird_dat_inundation_2015_Delft3D_4,
                                                data.frame(bird_dat_inundation_2015_Delft3D_3),
                                                by = "UID") %>% 
  dplyr::select( - c("geometry.x", "geometry.y", "geometry"))


load("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/mdl_pct_rail_pres.rda")
UNB <- st_read("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/FinalData/UNB_Grid.shp") %>% st_zm()
load("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/FinalData/rail_beld_elv_veg.RData")
dat <- dat %>% 
  dplyr::select(UID, pct_rail_pres)

#predict prob of bird occurrence using rows with the same attributes
validation_2015_Delft3D <- bird_dat_inundation_2015_Delft3D_4
bird_dat_inundation_2015_Delft3D_4$predict <- predict(mdl_pct_rail_pres, newdata = bird_dat_inundation_2015_Delft3D_4, type= "response")
bird_dat_inundation_2015_Delft3D_4 <- left_join(UNB, bird_dat_inundation_2015_Delft3D_4, by = "UID")
bird_dat_inundation_2015_Delft3D_4 <- left_join(bird_dat_inundation_2015_Delft3D_4, dat, by = "UID")
plot(bird_dat_inundation_2015_Delft3D_4["predict"])
cor(bird_dat_inundation_2015_Delft3D_4$pct_rail_pres, bird_dat_inundation_2015_Delft3D_4$predict, use = "complete.obs") #0.6584

save(bird_dat_inundation_2015_Delft3D_4, file = "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/FinalData/bird_dat_inundation_2015_Delft3D_4.RData")



#predict on the future scenarios

sp_max_stack_fut <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/sp_max_stack_fut.grd")

#predict on the future scenarios (make sure the quantile functions from above are active)
#read in UNB shape file with grid

UNB <- st_read("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/FinalData/UNB_Grid.shp") %>% st_zm()
UNB <- as_Spatial(UNB)
UNB <- spTransform(UNB, CRSobj = crs(elv_m_2017_baseline))
UNB <- st_as_sf(UNB)

#extract the approriate metrics (SPSP_MAX_grt60 + SPSP_MAX_95 + SPSP_MAX_grt90) and tidy up data

#grt_90 (proportion of acre with spartina greater than 90cm)
bird_dat_inundation_fut<-cbind(UNB, raster::extract(x = sp_max_stack_fut, y = UNB, fun = perc_grt_90, layer = 1, nl = 36))
names(bird_dat_inundation_fut)[2:37] <- names(sp_max_stack_fut)
bird_dat_inundation_fut_tidy <- bird_dat_inundation_fut %>% 
  gather(sp_max_2100_dre_RCP8_elnino_Q50_DL:sp_max_2100_ND_RCP4_elnino_Q66_CP, key = "key", value = "SPSP_MAX_grt90", na.rm = T) %>% 
  separate(key, c("spp", "max", "year", "sediment", "RCP", "yrtype", "flow", "model"), sep = "_") %>% 
  dplyr::select(-"spp", -"max")

#MAX_95 (95% percentile value of the max spartina height)
bird_dat_inundation_fut_2<-cbind(UNB, raster::extract(x = sp_max_stack_fut, y = UNB, fun = nintyfifthpercentile, layer = 1, nl = 36))
names(bird_dat_inundation_fut_2)[2:37] <- names(sp_max_stack_fut)
bird_dat_inundation_fut_tidy_2 <- bird_dat_inundation_fut_2 %>% 
  gather(sp_max_2100_dre_RCP8_elnino_Q50_DL:sp_max_2100_ND_RCP4_elnino_Q66_CP, key = "key", value = "SPSP_MAX_95", na.rm = T) %>% 
  separate(key, c("spp", "max", "year", "sediment", "RCP", "yrtype", "flow", "model"), sep = "_") %>% 
  dplyr::select(-"spp", -"max")

#MAX_grt60 (proportion of acre with spartina greater than 60cm)
bird_dat_inundation_fut_3<-cbind(UNB, raster::extract(x = sp_max_stack_fut, y = UNB, fun = perc_grt_60, layer = 1, nl = 36))
names(bird_dat_inundation_fut_3)[2:37] <- names(sp_max_stack_fut)
bird_dat_inundation_fut_tidy_3 <- bird_dat_inundation_fut_3 %>% 
  gather(sp_max_2100_dre_RCP8_elnino_Q50_DL:sp_max_2100_ND_RCP4_elnino_Q66_CP, key = "key", value = "SPSP_MAX_grt60", na.rm = T) %>% 
  separate(key, c("spp", "max", "year", "sediment", "RCP", "yrtype", "flow", "model"), sep = "_") %>% 
  dplyr::select(-"spp", -"max")

#join future sp metrics
bird_dat_inundation_fut_4 <- left_join(data.frame(bird_dat_inundation_fut_tidy), 
                                       data.frame(bird_dat_inundation_fut_tidy_2),
                                       by = c("UID", "year", "sediment", "RCP", "yrtype", "flow", "model"))
bird_dat_inundation_fut_4 <- left_join(bird_dat_inundation_fut_4, 
                                       data.frame(bird_dat_inundation_fut_tidy_3),
                                       by = c("UID", "year", "sediment", "RCP", "yrtype", "flow", "model"))
bird_dat_inundation_fut_4 <- bird_dat_inundation_fut_4 %>% 
  select(-c("geometry.x", "geometry.y", "geometry"))

save(bird_dat_inundation_fut_4, file = "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/FinalData/future_spartina_UNB.RData")

load("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/FinalData/future_spartina_UNB.RData")
load("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/mdl_pct_rail_pres.rda")




#predict prob of bird occurrence 



bird_dat_inundation_fut_4$predict <- predict(mdl_pct_rail_pres, newdata = bird_dat_inundation_fut_4, type= "response")
railpred <- left_join(UNB, bird_dat_inundation_fut_4, by = "UID")
railpred <- railpred %>% 
  filter(!is.na(year))
save(railpred, file = "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/FinalData/railpred.RData")




###################################################################################################
###################################################################################################

######### model 3: Predicting annual LFRR breeding population using P(occurrence) and #############
######### other predictors like precip, streamflow, predation, etc ################################

###################################################################################################
###################################################################################################






###################################################################################################
###################################################################################################

######### model 1a: Predicting Belding Savannah sparrow site occupancy using inundation ############

###################################################################################################
###################################################################################################
library(tidyverse)
library(raster)
library(sf)
library(lubridate)
library(rgeos)

#Bss location data
load("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/FinalData/rail_beld_elv_veg.RData") 
bss_dat <- dat %>% 
  dplyr::select(UID:spar_num_pairs) %>% 
  filter(year == 2015)
rm(dat)
#read in UNB file to make the bird data spatial
UNB <- st_read("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/FinalData/UNB_Grid.shp")
bss_dat <- left_join(UNB, bss_dat, by = "UID")
plot(bss_dat["spar_num_pairs"], main = "BSS Occupancy (2015)", key.pos = 1, 
     key.width = lcm(1.3), key.length = 0.5)

# #crop the bird data to be just where veg data is
# veg_boundary <- st_read("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/UNB Vegetation/Veg_boundary.shp")
# veg_boundary <- as(veg_boundary, "Spatial") #veg boundary convert to spatial object
# bird_dat_sp <- as(bss_dat, "Spatial")#convert bird object to spatial for cropping
# veg_boundary <- spTransform(veg_boundary, CRSobj = crs(bird_dat_sp))
# bird_dat_sp <- raster::intersect(veg_boundary, bird_dat_sp ) #crop to extent
# plot(bird_dat_sp)
# bss_dat <- st_as_sf(bird_dat_sp)
# plot(bss_dat["spar_num_pairs"])

#in the plots with zero BSS, make a point with zero

zero_pts <- SpatialPointsDataFrame(gCentroid(as(bss_dat, "Spatial"), byid = TRUE), data = data.frame(bss_dat)) #use the bss_dat before cropping
plot(zero_pts[zero_pts$spar_num_pairs==0,])
zero_pts <- st_as_sf(zero_pts) %>% 
  filter(spar_num_pairs == 0) %>% 
  dplyr::select(marsh, year, spar_num_pairs)
zero_pts[,"Species"] <- "Belding's Savannah Sparrow"
plot(zero_pts['spar_num_pairs'])

bss_pt <- st_read("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/RailBeldData_pt.shp") %>% 
  dplyr::select(year, Species, marsh) %>% 
  filter(year == 2015, marsh == "Upper Newport Bay", Species == "Belding's Savannah Sparrow")
bss_pt <- st_transform(bss_pt, crs = crs(zero_pts))
bss_pt[,"spar_num_pairs"] <- 1

bss_pt <- rbind(bss_pt, zero_pts)

plot(bss_pt["spar_num_pairs"])
bss_pt_fullmarsh <- bss_pt
save(bss_pt_fullmarsh, file = "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/bss_pt_fullmarsh.RData")

#crop the bird data to be just where veg data is
veg_boundary <- st_read("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/UNB Vegetation/Veg_boundary.shp")
veg_boundary <- as(veg_boundary, "Spatial") #veg boundary convert to spatial object
bird_dat_sp <- as(bss_pt, "Spatial")#convert bird object to spatial for cropping
veg_boundary <- spTransform(veg_boundary, CRSobj = crs(bird_dat_sp))
bird_dat_sp <- raster::crop(bird_dat_sp,veg_boundary ) #crop to extent
plot(bird_dat_sp)
bss_pt <- st_as_sf(bird_dat_sp)
plot(bss_pt["spar_num_pairs"]) #this is presence and absence BSS file for model training!!!

save(bss_pt, file = "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/bss_pt.RData")

#start here to load the BSS presence and absence file for model training

load("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/bss_pt.RData")


#inundation metrics for baseline (year 2015, last year, and the year before)

inun_metrics_13 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2013_inun_metrics_.grd")
inun_metrics_14 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2014_inun_metrics_.grd")
inun_metrics_15 <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2015_inun_metrics_.grd")

#make a moving window for the summ_inu_pct in 2015.  make it a circle with radius 50m. then we will make a new raster that has the average value
#and the 95th percentile value. Want to capture the habitat utilitzaiton as a function of how common that habitat it.
#we also extract the summ_inu_pct value directly where the sparrow point is. 

summ_inu_pct_15 <- inun_metrics_15$summ_inu_pct
#make a weight matrix with the focalWeight function.
buffer_radius <- 5
fw_5m <- focalWeight(summ_inu_pct_15, buffer_radius, type = "circle")
#recale weight matrix to 1/0 for calculations
fw_5m <- ifelse(fw_5m>0,1,0)
#mean value for percent of summer inundated
mean_summ_inu_pct_15 <- focal(summ_inu_pct_15, w = fw_5m, fun = mean, na.rm = TRUE)

#95th percentile
#function for 95th percentile
nintyfifthpercentile <- function(x,...){
  d <- quantile(x, probs = 0.95, na.rm = TRUE)
  return(d)
}
ninyfifthperc_summ_inu__pct_15 <- focal(summ_inu_pct_15, w = fw_5m, fun = nintyfifthpercentile, na.rm = TRUE)


dat <- cbind(bss_pt, raster::extract(x = mean_summ_inu_pct_15, y = bss_pt))
dat <- cbind(dat, raster::extract(x = ninyfifthperc_summ_inu__pct_15, y = bss_pt))
dat <- cbind(dat, raster::extract(x = summ_inu_pct_15, y = bss_pt))
names(dat)[5:7] <-c("mean_summ_inu_pct", "ntyfth_summ_inu_pct", "summ_inu_pct")


ggplot(data = dat, mapping = aes(x = ntyfth_summ_inu_pct, fill = as.factor(spar_num_pairs)))+
  geom_histogram(position = "stack")

save(dat, file = "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/bss_training_data.RData")

#start here with the BSS training data loaded with the inundation metrics for 2013-2015
load("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/bss_training_data.RData")



num<-5:7
bss_dat <- data.frame(dat)

bss_pval<-NULL

for (i in num){
  
  mod <- glm(spar_num_pairs ~ bss_dat[,i] , family= "binomial", data= bss_dat)
  
  bss_pval <- rbind(bss_pval, data.frame("pval"= coef(summary(mod))[2,4], "variable"= names(bss_dat[i])))
  
}
bss_pval <- bss_pval[order(bss_pval$pval),]
#95th percentile was the least significnat and well correlated with the others so we won't use it further


#model for bss using the mean percentage of time inundated in the same year of the survey


summary(bss_mdl <- glm(data = dat, family = "binomial", spar_num_pairs ~ mean_summ_inu_pct))
save(bss_mdl, file = "C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/bss_mdl.rda")

ggplot(data = dat, mapping = aes(x = mean_summ_inu_pct, y = spar_num_pairs))+
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"))+
  geom_point(alpha = 0.2)


load("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/bss_mdl.rda")



ggplot(data = dat, mapping = aes(x = as.factor(spar_num_pairs), y = mean_summ_inu_pct))+
  geom_violin()+
  geom_boxplot(width = .2)
count(dat,  spar_num_pairs)



#inundation metrics for validation and for future scenario prediction 
#this model was trained on karens data that had a max summer inundation percetn of 0.292122. This is because they did
#not collect data at lower marsh elevations or in the channel which is mudflat or always submerged. We will remove the values
#greater than this in the Delft3D output so that the moving window analysis does not average in very low values which 
#were not there in the training data.

#moving window analysis function to get mean summ % inundation that will be applied to all validation and future years. 

mean_summ_inun_perc_function <- 
  
  function(rasterstack){
    
    summ_inu_pct <- rasterstack$summ_inu_pct
    #make a weight matrix with the focalWeight function.
    buffer_radius <- 5
    fw_5m <- focalWeight(summ_inu_pct, buffer_radius, type = "circle")
    #recale weight matrix to 1/0 for calculations
    fw_5m <- ifelse(fw_5m>0,1,0)
    #mean value for percent of summer inundated
    mean_summ_inu_pct <- focal(summ_inu_pct, w = fw_5m, fun = mean, na.rm = TRUE)
    names(mean_summ_inu_pct) <- "mean_summ_inu_pct"
    
    
    return(mean_summ_inu_pct)
  }

load("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/bss_mdl.rda")



#2015 baseline year for validation with Delft3d
inun_metrics_2015_delft3D  <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/__2015_delft3D_inun_metrics_.grd")

values(inun_metrics_2015_delft3D$summ_inu_pct) <- ifelse(values(inun_metrics_2015_delft3D$summ_inu_pct>0.292122), NA, values(inun_metrics_2015_delft3D$summ_inu_pct))

inun_metrics_2015_delft3D <- mean_summ_inun_perc_function(inun_metrics_2015_delft3D)

bss_predict_2015_delft3D <- raster::predict(object = inun_metrics_2015_delft3D, bss_mdl, type = "response")
plot(bss_predict_2015_delft3D)

load("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/bss_pt_fullmarsh.RData")
bss_pt_fullmarsh <- st_transform(bss_pt_fullmarsh, crs = crs(bss_predict_2015_delft3D))
crs(bss_pt_fullmarsh)
bss_pt_fullmarsh <- cbind(bss_pt_fullmarsh, raster::extract(x = bss_predict_2015_delft3D, y = bss_pt_fullmarsh))
names(bss_pt_fullmarsh)[5] <- "Predicted_Prob"
ggplot(data = bss_pt_fullmarsh, mapping = aes(x = as.factor(spar_num_pairs), y = Predicted_Prob))+
  geom_boxplot()

#Future years

 
inun_metrics_2100_dre_RCP8_elnino  <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_dredge_RCP8_elnino_inun_metrics_.grd")
bss_predict_2100_dre_RCP8_elnino <- raster::predict(object = inun_metrics_2100_dre_RCP8_elnino, bss_mdl, type = "response")
plot(bss_predict_2100_dre_RCP8_elnino)

inun_metrics_2100_dre_RCP8_normal <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_dredge_RCP8_normal_inun_metrics_.grd")
bss_predict_2100_dre_RCP8_normal <- raster::predict(object = inun_metrics_2100_dre_RCP8_normal, bss_mdl, type = "response")
plot(bss_predict_2100_dre_RCP8_normal)

inun_metrics_2100_dre_RCP4_elnino <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_dredge_RCP4_elnino_inun_metrics_.grd")
bss_predict_2100_dre_RCP4_elnino <- raster::predict(object = inun_metrics_2100_dre_RCP4_elnino, bss_mdl, type = "response")
plot(bss_predict_2100_dre_RCP4_elnino)

inun_metrics_2100_dre_RCP4_normal <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_dredge_RCP4_normal_inun_metrics_.grd")
bss_predict_2100_dre_RCP4_normal <- raster::predict(object = inun_metrics_2100_dre_RCP4_normal, bss_mdl, type = "response")
plot(bss_predict_2100_dre_RCP4_normal)

inun_metrics_2100_ND_RCP8_elnino <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_ND_RCP8_elnino_inun_metrics_.grd")
values(inun_metrics_2100_ND_RCP8_elnino$summ_inu_pct) <- ifelse(values(inun_metrics_2100_ND_RCP8_elnino$summ_inu_pct>0.292122), NA, values(inun_metrics_2100_ND_RCP8_elnino$summ_inu_pct))
inun_metrics_2100_ND_RCP8_elnino <- mean_summ_inun_perc_function(inun_metrics_2100_ND_RCP8_elnino)
bss_predict_2100_ND_RCP8_elnino <- raster::predict(object = inun_metrics_2100_ND_RCP8_elnino, bss_mdl, type = "response")
plot(bss_predict_2100_ND_RCP8_elnino)

inun_metrics_2100_ND_RCP8_normal <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_ND_RCP8_normal_inun_metrics_.grd")
bss_predict_2100_ND_RCP8_normal <- raster::predict(object = inun_metrics_2100_ND_RCP8_normal, bss_mdl, type = "response")
plot(bss_predict_2100_ND_RCP8_normal)

inun_metrics_2100_ND_RCP4_elnino <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_ND_RCP4_elnino_inun_metrics_.grd")
bss_predict_2100_ND_RCP4_elnino <- raster::predict(object = inun_metrics_2100_ND_RCP4_elnino, bss_mdl, type = "response")
plot(bss_predict_2100_ND_RCP4_elnino)

inun_metrics_2100_ND_RCP4_normal <- stack("C:/Users/Jenny/Documents/JennySCCWRP/LitReview/UCI/working data/_2100_ND_RCP4_normal_inun_metrics_.grd")
bss_predict_2100_ND_RCP4_normal <- raster::predict(object = inun_metrics_2100_ND_RCP4_normal, bss_mdl, type = "response")
plot(bss_predict_2100_ND_RCP4_normal)