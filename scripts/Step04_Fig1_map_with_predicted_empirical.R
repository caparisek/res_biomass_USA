## ---------------------------
## Author: Christine A. Parisek, caparisek@ucdavis.edu
## Figure 1 - Map  
## ---------------------------


library(tidyverse)   # ggplot() & dplyr()
library(sp)          # SpatialPoints()
library(ggspatial)   # annotation_scale()
library(patchwork) # Stitch final plots together
library (fBasics)    # Optional; to explore basic summary stats
options(scipen=999)  # Optional; to remove sci-notation





# Data ------------------------------------------------------------
## The output from Script Step 03's Schema 5 - 
ROYL <- read_csv("data output/03_USA_GAM5_fit.csv")

colnames(ROYL)
fBasics::basicStats(ROYL$Biomass_lbsPERacre) 
fBasics::basicStats(ROYL$gam_calc5_L2ecoXarea) 
fBasics::basicStats(ROYL$Year.y) 


# Spatial prep -------------------------------------------------
crsnad83<-'+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0'
crsaea <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'

nad83spatial<-with(ROYL, SpatialPoints(coords=cbind(ROYL$LONGITUDE, ROYL$LATITUDE), proj4string = CRS(crsnad83)))

sptransformnad83<-spTransform(nad83spatial, CRSobj=CRS(crsaea)) #reprojected to albers equal area projection

generic_nad83 <- as.data.frame(sptransformnad83@coords) #with just lat/long, back to df 

wgs1984.proj <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

names(generic_nad83)<-c("Longitude_albers", "Latitude_albers")
POINT<-cbind(ROYL, generic_nad83)

mapbase<-map_data('state')
##  view(mapbase)                            #explore
##  ca<-subset(states, region=="california") #explore; CA lat/long data

CAP_map_coords <- SpatialPoints(coords = with(mapbase, data.frame(x = long, y = lat)), proj4string = CRS(wgs1984.proj)) #here are the coordinates in that frame, and this is the CRS they're in 

res_albers <- spTransform(CAP_map_coords, CRSobj = CRS(crsaea)) #put latlong to this CRS

mapbase$long <- res_albers@coords[,1]
mapbase$lat <- res_albers@coords[,2]

res_map<-ggplot(mapbase, aes(x=long,y=lat))

# 6 lat-longs remain (0,0) bc they were only able to be assigned general ecoregion; remove these for map.
POINT2<-POINT %>% 
  dplyr::filter(!LATITUDE==0) 




# Panel A – blank plot --------------------------------------------------------

blankPlot <- ggplot()+
  geom_blank(aes(1,1))+
  theme(
    plot.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank())


# Panel B – Map of USA ------------------------------------------------------------

basemap<-res_map+
  geom_path(color="black", size=0.2,aes(group=group))+
  geom_point(data=POINT2,aes(x=Longitude_albers,y=Latitude_albers), size= 0.5, color= "slategray",  alpha= 0.5, shape=16)+
  theme_bw()+
  theme(axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        panel.border = element_rect(colour = "white", linewidth=0.4),
        plot.margin = margin(t = 0, r = 0, b = 1.1, l = 0,  "cm"))+
  annotation_scale(location = "bl",
                   style = "ticks",
                   line_width = 0.5,
                   height = unit(0.1, "cm"),
                   tick_height = 0.4, 
                   text_cex = 0.8)+
  annotation_north_arrow(width = unit(.30,"in"),
                         height = unit (.49, "in"),
                         #pad_y = unit(.3, "in"),
                         location = "tl", 
                         which_north = "true",
                         style = north_arrow_minimal)



# Bin Lat-Long ------------------------------------------------------------
# For figure to top and right of map figure. 
# Perform binning with specific number of bins per lat and long

fBasics::basicStats(POINT2$LONGITUDE)#range: -124-67
(124-67)/80 
fBasics::basicStats(POINT2$LATITUDE) #range: 25-49
(49-25)/40

BINNED_LATLONG<-POINT2 %>% 
  mutate(bin_long = cut(LONGITUDE, breaks=80)) %>% 
  mutate(bin_lat =  cut(LATITUDE, breaks=40)) 

#check
x<-BINNED_LATLONG %>% group_by(bin_long) %>% tally(sort=TRUE) %>% ungroup()#check
x<-BINNED_LATLONG %>% group_by(bin_lat) %>% tally(sort = TRUE) %>% ungroup()#check


#get metrics per bin

#define quantiles of interest, then run quantile line. https://www.statology.org/r-quantile-by-group/ 
q = c(.25, .5, .75)

LONG_metrics<- BINNED_LATLONG %>% 
  group_by(bin_long) %>% 
  mutate(Long_LOG_gam5_ecoXarea = log     (gam_calc5_L2ecoXarea)) %>% 
  mutate(Long_SS_Median         = median  (Long_LOG_gam5_ecoXarea)) %>% 
  mutate(LONG_quant25 = quantile(Long_LOG_gam5_ecoXarea, probs = q[1]), 
         LONG_quant50 = quantile(Long_LOG_gam5_ecoXarea, probs = q[2]),
         LONG_quant75 = quantile(Long_LOG_gam5_ecoXarea, probs = q[3])) %>% 
  ungroup()
  

LAT_metrics<- LONG_metrics %>% 
  group_by(bin_lat) %>% 
  mutate(Lat_LOG_gam5_ecoXarea = log   (gam_calc5_L2ecoXarea)) %>% 
  mutate(Lat_SS_Median         = median(Lat_LOG_gam5_ecoXarea)) %>% 
  mutate(LAT_quant25 = quantile(Lat_LOG_gam5_ecoXarea, probs = q[1]), 
         LAT_quant50 = quantile(Lat_LOG_gam5_ecoXarea, probs = q[2]),
         LAT_quant75 = quantile(Lat_LOG_gam5_ecoXarea, probs = q[3])) %>% 
  ungroup()


BIN_ALL<-LAT_metrics


#remove dup medians (or plot line won't draw)
# e.g., say bin A has 100 observations each with the same median Y. ggplot is trying to draw a line in those 100 within the same bin and it doesnt work. need 1 unique median per bin. 

Long_Unique_Median<-BIN_ALL %>% 
  dplyr::group_by(bin_long) %>% 
  dplyr::filter(row_number(Long_SS_Median)==1) %>%  #resulting rows = same # of bin breaks (good)
  ungroup()

Lat_Unique_Median<- BIN_ALL %>%
  dplyr::group_by(bin_lat) %>% 
  dplyr::filter(row_number(Lat_SS_Median)==1) %>% #resulting rows = same # of bin breaks (good)
  ungroup()


#Quantiles... 
Long_Unique_Quantile<-BIN_ALL %>% 
  dplyr::group_by(bin_long) %>% 
  dplyr::filter(row_number(LONG_quant25)==1) %>%  #using any quantile will work for the rest
  ungroup()

Lat_Unique_Quantile<- BIN_ALL %>%
  dplyr::group_by(bin_lat) %>% 
  dplyr::filter(row_number(LAT_quant25)==1) %>% #using any quantile will work for the rest
  ungroup()


# Panel C – pulse, top figure -------------------------------------------------

TOP_MEDIAN<-BIN_ALL %>%  
  ggplot()+
  geom_point(data=BIN_ALL,              aes(x=bin_long,y=Long_LOG_gam5_ecoXarea), color="royalblue4", size=3,alpha=0.1)+
  geom_line( data=Long_Unique_Median,   aes(x=bin_long,y=Long_SS_Median),group=1, color="red",        size=1,alpha=0.9)+
  geom_line( data=Long_Unique_Quantile, aes(x=bin_long,y=LONG_quant75),  group=1, color="slategray1", linetype="solid")+
  #geom_line( data=Long_Unique_Quantile, aes(x=bin_long,y=LONG_quant50), group=1, color="blue", linetype="dashed")+
  geom_line( data=Long_Unique_Quantile, aes(x=bin_long,y=LONG_quant25),  group=1, color="slategray4", linetype="solid")+
  theme_bw()+
  labs(y= "Total Standing Stock (log, kg)")+
  scale_x_discrete(expand=c(0.08,0.0))+
  scale_y_continuous(labels = scales::comma)+
  #scale_y_continuous(labels = scales::comma, breaks = seq(0, 150000000,20000000))+
  theme(axis.ticks.x = element_blank(), 
        axis.text.x  = element_blank(), 
        axis.title.x = element_blank(),
        #axis.title.y = element_text(size=12), 
        axis.ticks.y = element_line(color="gray10"),
        axis.text.y  = element_text(color="gray10"),
        panel.border = element_rect(colour = "white", size=0.4),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0.1, "cm"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())


# Panel D – pulse, right figure -------------------------------------------------

RIGHT_MEDIAN<-BIN_ALL %>% 
  ggplot()+
  geom_point(data=BIN_ALL,            aes(y=bin_lat,x=Lat_LOG_gam5_ecoXarea), color="royalblue4",size=3,alpha=0.1)+
  geom_line( data=Lat_Unique_Median,  aes(y=bin_lat,x=Lat_SS_Median),group=1, color="red",       size=1,alpha=0.9)+
  geom_line( data=Lat_Unique_Quantile,aes(y=bin_lat,x=LAT_quant75),  group=1, color="slategray1", linetype="solid")+
  #geom_line(data=Lat_Unique_Quantile,aes(y=bin_lat,x=LAT_quant50),  group=1, color="blue", linetype="dashed")+
  geom_line( data=Lat_Unique_Quantile,aes(y=bin_lat,x=LAT_quant25),  group=1, color="slategray4", linetype="solid")+
  theme_bw()+
  labs(x= "Total Standing Stock (log, kg)")+
  scale_y_discrete(expand=c(0.1,0.01))+
  scale_x_continuous(labels = scales::comma)+
  #scale_x_continuous(labels = scales::comma, breaks = seq(0, 150000000,20000000))+
  theme(
    axis.ticks.y = element_blank(), 
    axis.text.y  = element_blank(), 
    axis.title.y = element_blank(), 
    #axis.title.x = element_text(size=12),
    axis.ticks.x = element_line(color="gray10"),
    axis.text.x  = element_text(color="gray10"),
    panel.grid.major=element_blank(), 
    panel.grid.minor=element_blank(),
    panel.border = element_rect(colour = "white", size=0.4),
    plot.margin = margin(t = 0, r = 0.1, b = 0, l = 0, "cm"))



# Figure 1 Map  --------------------------------------------------------

final <- TOP_MEDIAN + blankPlot + basemap+ RIGHT_MEDIAN + patchwork::plot_layout(ncol = 2, 
                                                                               widths=c(3.5, 1.3), 
                                                                               heights=c(1.5, 3.5))

ggsave(plot=final,"figures/Fig_1_Map_pulse_Predicted_with_Quantile.tiff", height=18, width=22, units="cm", dpi=900)



