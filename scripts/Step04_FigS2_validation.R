## ---------------------------
## Author: Christine A. Parisek, caparisek@ucdavis.edu
## Figure S2 - Validation   
## ---------------------------

library(tidyverse) # General use
library(lmerTest)  # Model 
library(smatr)     # Slope test
library(MuMIn)     # MuMIn::r.squaredGLMM	

#install.packages("devtools") 
#devtools::install_github("johannesbjork/LaCroixColoR") 
library(LaCroixColoR) # Color

library(fBasics) # Optional; explore summary stats
options(scipen=999) # Optional; removes sci-notation




# Data - This study -------------------------------------------------------
## Read in outputs from Script Step 03 which are for each Schema.

a_Gam1<-read_csv("data output/03_USA_GAM1_fit.csv",
                 col_types = cols(STATEID = col_character(),
                                  URL_ADDRESS = col_character(),
                                  Ecosystem = col_character(),
                                  LONGITUDE_wasLat = col_double(),
                                  LATITUDE_wasLon = col_double(),
                                  Year.x = col_double(),
                                  YearDamCompleted = col_double(),
                                  Area_acres = col_double(),
                                  NCoves = col_double(),
                                  Species = col_character(),
                                  Biomass_lbsPERacre = col_double(),
                                  Notes = col_character(),
                                  Biomass_corrected_lbsPERacre = col_double(),
                                  Biomass_kgha = col_double(),
                                  minAge = col_double(),
                                  ResAge = col_double(),
                                  Year.y = col_double(),
                                  fit = col_double(),
                                  se.fit = col_double()))
a_Gam2<-read_csv("data output/03_USA_GAM2_fit.csv",
                 col_types = cols(STATEID = col_character(),
                                  URL_ADDRESS = col_character(),
                                  Ecosystem = col_character(),
                                  LONGITUDE_wasLat = col_double(),
                                  LATITUDE_wasLon = col_double(),
                                  Year.x = col_double(),
                                  YearDamCompleted = col_double(),
                                  Area_acres = col_double(),
                                  NCoves = col_double(),
                                  Species = col_character(),
                                  Biomass_lbsPERacre = col_double(),
                                  Notes = col_character(),
                                  Biomass_corrected_lbsPERacre = col_double(),
                                  Biomass_kgha = col_double(),
                                  minAge = col_double(),
                                  ResAge = col_double(),
                                  Year.y = col_double(),
                                  fit = col_double(),
                                  se.fit = col_double()))
a_Gam3<-read_csv("data output/03_USA_GAM3_fit.csv",
                 col_types = cols(STATEID = col_character(),
                                  URL_ADDRESS = col_character(),
                                  Ecosystem = col_character(),
                                  LONGITUDE_wasLat = col_double(),
                                  LATITUDE_wasLon = col_double(),
                                  Year.x = col_double(),
                                  YearDamCompleted = col_double(),
                                  Area_acres = col_double(),
                                  NCoves = col_double(),
                                  Species = col_character(),
                                  Biomass_lbsPERacre = col_double(),
                                  Notes = col_character(),
                                  Biomass_corrected_lbsPERacre = col_double(),
                                  Biomass_kgha = col_double(),
                                  minAge = col_double(),
                                  ResAge = col_double(),
                                  Year.y = col_double(),
                                  fit = col_double(),
                                  se.fit = col_double()))
a_Gam4<-read_csv("data output/03_USA_GAM4_fit.csv",
                 col_types = cols(STATEID = col_character(),
                                  URL_ADDRESS = col_character(),
                                  Ecosystem = col_character(),
                                  LONGITUDE_wasLat = col_double(),
                                  LATITUDE_wasLon = col_double(),
                                  Year.x = col_double(),
                                  YearDamCompleted = col_double(),
                                  Area_acres = col_double(),
                                  NCoves = col_double(),
                                  Species = col_character(),
                                  Biomass_lbsPERacre = col_double(),
                                  Notes = col_character(),
                                  Biomass_corrected_lbsPERacre = col_double(),
                                  Biomass_kgha = col_double(),
                                  minAge = col_double(),
                                  ResAge = col_double(),
                                  Year.y = col_double(),
                                  fit = col_double(),
                                  se.fit = col_double()))
a_Gam5<-read_csv("data output/03_USA_GAM5_fit.csv",
                 col_types = cols(STATEID = col_character(),
                                  URL_ADDRESS = col_character(),
                                  Ecosystem = col_character(),
                                  LONGITUDE_wasLat = col_double(),
                                  LATITUDE_wasLon = col_double(),
                                  Year.x = col_double(),
                                  YearDamCompleted = col_double(),
                                  Area_acres = col_double(),
                                  NCoves = col_double(),
                                  Species = col_character(),
                                  Biomass_lbsPERacre = col_double(),
                                  Notes = col_character(),
                                  Biomass_corrected_lbsPERacre = col_double(),
                                  Biomass_kgha = col_double(),
                                  minAge = col_double(),
                                  ResAge = col_double(),
                                  Year.y = col_double(),
                                  fit = col_double(),
                                  se.fit = col_double()))

colnames(a_Gam1) #each calculation column has unique description for each gam/method. 
colnames(a_Gam2)
colnames(a_Gam3)
colnames(a_Gam4)
colnames(a_Gam5)

#Note desired columns and units: 
#GAM1 predictions - (1993 fit = kg/ha) & (gam_calc1_L2ecoXarea = kg)
#GAM2 predictions - (1993 fit = kg/ha) & (gam_calc2_L2ecoXarea = kg)
#GAM3 predictions - (1993 fit = kg/ha) & (gam_calc3_L2ecoXarea = kg)
#GAM4 predictions - (1993 fit = kg/ha) & (gam_calc4_L2ecoXarea = kg)
#GAM5 predictions - (1993 fit = kg/ha) & (gam_calc5_L2ecoXarea = kg)


aa_Gam1<-a_Gam1 %>% 
  dplyr::select(c(NIDID,gam_cacl1_simpleavgXarea))
aa_Gam2<-a_Gam2 %>% 
  dplyr::select(c(NIDID,gam_calc2_medianXarea))
aa_Gam3<-a_Gam3 %>% 
  dplyr::select(c(NIDID,gam_calc3_methXarea))
aa_Gam4<-a_Gam4 %>% 
  dplyr::select(c(NIDID,gam_calc4_L2ecoXarea))

a_G5_G1<-left_join(a_Gam5,aa_Gam1)
a_G5_G1_G2<-left_join(a_G5_G1,aa_Gam2)
a_G5_G1_G2_G3<-left_join(a_G5_G1_G2,aa_Gam3)
a_G5_G1_G2_G3_G4<-left_join(a_G5_G1_G2_G3,aa_Gam4)

colnames(a_G5_G1_G2_G3_G4) #use schema1-5 columns in validation. 

a_fish_estimates<-a_G5_G1_G2_G3_G4 # a single dataset with schema 1-5's data collated.



# Data - Validation dataset -----------------------------------------------
b_francine<-read_csv("data/validation_metaanalysis_biomass_litvalues.csv")

colnames(b_francine)
unique(b_francine$F_Ecosystem) # Names are with commma;
unique(b_francine$NIDID)
sum(is.na(b_francine$F_Biomass_lbsPERacre))

c_validation_join <-left_join(a_fish_estimates, b_francine, by="NIDID") #join C&F dfs

d_valid_datapresent<-c_validation_join %>% 
  dplyr::filter(!is.na(F_Biomass_lbsPERacre)) #keep only rows found in lit search

sum(is.na(d_valid_datapresent$surface_area_ha_HAS_NA)) #no NA; can use "surface_area_ha" bc if real values existed they were retained. 

x<-d_valid_datapresent %>% 
  dplyr::select(NIDID,surface_area_ha_HAS_NA,surface_area_ha) #proof for choice of SA column use. 

#make conversion & corrections, parallel methods have already been done to ROYL.
e_valid_totalkg <- d_valid_datapresent %>% 
  mutate(F_Biomass_kgha = (F_Biomass_lbsPERacre*0.453592)/(0.404686)) %>%  #Convert 42row DF's ROYL BIOMASS lb/acre to kg/ha
  mutate(F_biomass_kgha_corrected = (F_Biomass_kgha)*(1.773056)) %>%  #apply adjustment factor from Davies book
  mutate(F_biomass_kg_corrected = (F_biomass_kgha_corrected)*(surface_area_ha)) #kg/ha to KG only




# Prep --------------------------------------------------------------------
# Prep for mixed-effect regression model w/ random effects

colnames(e_valid_totalkg)
unique(e_valid_totalkg$Year.y) #FYI, this is the year of predicted data used for calc. 

e_keep_some_col<-e_valid_totalkg %>% #keep only useful columns so it's easier to explore outputs. 
  dplyr::select(c("NIDID",
                  "DAM_NAME",
                  "YEAR_COMPLETED",
                  "OTHER_DAM_NAME",
                  "DAM_FORMER_NAME",
                  "COUNTY",
                  "RIVER",
                  "CITY",
                  "LONGITUDE",
                  "LATITUDE",
                  "geometry",
                  "Ecosystem",
                  "YearDamCompleted",
                  "ResAge",
                  "Year.y",
                  "gam_calc5_L2ecoXarea",
                  "gam_cacl1_simpleavgXarea",
                  "gam_calc2_medianXarea",
                  "gam_calc3_methXarea",
                  "gam_calc4_L2ecoXarea",
                  "F_Ecosystem",
                  "F_Biomass_lbsPERacre",
                  "f_Notes",
                  "F_Biomass_kgha",
                  "F_biomass_kgha_corrected",
                  "F_biomass_kg_corrected"))
  

f_GatheredDF<-e_keep_some_col %>%
  gather(key="ClassificationMethod", value="PredictedValues_kg_adjusted", gam_calc5_L2ecoXarea:gam_calc4_L2ecoXarea) %>% 
  # KEY is where original COL names go. VALUE is where values go. Gather columns into rows from col meth1 TO calc5. 
  dplyr::filter(ClassificationMethod %in% c("gam_calc5_L2ecoXarea", 
                                            "gam_cacl1_simpleavgXarea",
                                            "gam_calc2_medianXarea",
                                            "gam_calc3_methXarea",
                                            "gam_calc4_L2ecoXarea")) %>% #keep only those in the COL w these names
  mutate(ObsFishMass_kg_log_adjusted = log10(F_biomass_kg_corrected)) %>% #log them here to keep model cleaner looking
  mutate(PredFishMass_kg_log_adjusted = log10(PredictedValues_kg_adjusted))

5*42 #df n=210

# Model -------------------------------------------------------------------

mmm<-lmerTest::lmer(ObsFishMass_kg_log_adjusted ~ PredFishMass_kg_log_adjusted + (1 + PredFishMass_kg_log_adjusted | ClassificationMethod), data=f_GatheredDF)

summary(mmm) # y = 0.983337 x + 0.481981 
# intercept/b = 0.481981
# slope    /m = 0.983337


MuMIn::r.squaredGLMM(mmm) # R2M: marginal / R2C: conditional

#slope test; don't want it to be significant.
smatr::sma(f_GatheredDF$ObsFishMass_kg_log_adjusted~f_GatheredDF$PredFishMass_kg_log_adjusted, slope.test=1)
slope.test(f_GatheredDF$ObsFishMass_kg_log_adjusted,f_GatheredDF$PredFishMass_kg_log_adjusted,test.value = 1, method = 'SMA')

basicStats(f_GatheredDF$PredFishMass_kg_log_adjusted)
basicStats(f_GatheredDF$ObsFishMass_kg_log_adjusted)


#Create Column with the new names; "levels" orders them in the legend. 
f_GatheredDF$CalcType <- factor(f_GatheredDF$ClassificationMethod, 
                              levels = c("gam_cacl1_simpleavgXarea",
                                         "gam_calc2_medianXarea",
                                         "gam_calc3_methXarea",
                                         "gam_calc4_L2ecoXarea",
                                         "gam_calc5_L2ecoXarea"),
                              labels = c("Schema 1",
                                         "Schema 2",
                                         "Schema 3",
                                         "Schema 4",
                                         "Schema 5"))


fBasics::basicStats(f_GatheredDF$PredFishMass_kg_log_adjusted) #2.43-7.8
fBasics::basicStats(f_GatheredDF$ObsFishMass_kg_log_adjusted)  #3.0-8.0

#plotting all calc lines on top of each other
ggplot(f_GatheredDF, aes(x=PredFishMass_kg_log_adjusted, y=ObsFishMass_kg_log_adjusted, color=CalcType)) +
  geom_abline(aes(intercept=0.481981,slope=0.983337),    size=0.5, alpha=0.6, linetype="solid")+
  annotate(geom="text", x=1.500, y=8.600, color="black", size=2.5, label= "Mixed Effect Model" )+
  annotate(geom="text", x=1.240, y=8.290, color="black", size=2.3, label= "y = 0.98x + 0.48" )+
  annotate(geom="text", x=0.900, y=8.000, color="black", size=2.3, label= "p < 0.0001")+
  annotate(geom="text", x=0.950, y=7.640, color="black", size=2.3, label= deparse(bquote(~R[c]^2 ==~0.9847 )),parse = T)+
  geom_point(size=3.5, alpha=0.4) +
  geom_line(stat="smooth",method='lm',formula=y~x, se=F, size=0.5, alpha=0.5, linetype="solid")+
  #scale_x_continuous(breaks = seq(1, 10, 1), limits=c(2.49,8.2))+
  #scale_y_continuous(breaks = seq(1, 10, 1), limits=c(2.49,8.2))+
  scale_x_continuous(breaks = seq(0, 8, 1), limits=c(0,9), expand=c(0,0))+
  scale_y_continuous(breaks = seq(0, 8, 1), limits=c(0,9), expand=c(0,0))+
  scale_color_manual(values=(lacroix_palette("PeachPear",type = "continuous", n=5)))+ #rev() to reverse color order
  theme_bw()+
  theme(axis.title.x=element_text(size=8.5), 
        axis.title.y=element_text(size=8.5), 
        axis.text.x =element_text(size=6),
        axis.text.y =element_text(size=6),
        legend.title = element_text(colour="black", size=9),
        legend.text = element_text(colour="black",  size=8),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  labs(color='Classification Methods')+
  xlab(label = "Predicted Total Standing Stock (log, kg)")+ 
  ylab(label = "Observed Total Standing Stock (log, kg)")

ggsave("figures/Fig_S2_validation_to_litvalues.tiff", height=8, width=12.5, units="cm", dpi=900)



