## ---------------------------
## Author: Christine A. Parisek, caparisek@ucdavis.edu
## This script: Creates Dataset S3
## ---------------------------

library(tidyverse)
library(fBasics)    # Optional; descriptive summary statistics
options(scipen=999) # Optional; removes sci-notation

# Data - This study -------------------------------------------------------
## Read in outputs from Script Step 03 which are for each Schema. Then join relevant columns into one DF. 

a_classificationed<-read_csv("data output/02_L1L2L3_Clustered.csv") # The output DF from Script 02.

# And the output DFs from Script 03 (all 5)
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

#Note desired columns and units: 
#GAM1 predictions - (1993 fit = kg/ha) & (gam_calc1_L2ecoXarea = kg)
#GAM2 predictions - (1993 fit = kg/ha) & (gam_calc2_L2ecoXarea = kg)
#GAM3 predictions - (1993 fit = kg/ha) & (gam_calc3_L2ecoXarea = kg)
#GAM4 predictions - (1993 fit = kg/ha) & (gam_calc4_L2ecoXarea = kg)
#GAM5 predictions - (1993 fit = kg/ha) & (gam_calc5_L2ecoXarea = kg)


colnames(a_Gam1) #each calculation column has unique description for each gam/method. 
colnames(a_Gam2)
colnames(a_Gam3)
colnames(a_Gam4)
colnames(a_Gam5)
colnames(a_classificationed)



# Check the DFs can be combined -------------------------------------------

fBasics::basicStats(a_classificationed$RECORDID)
fBasics::basicStats(a_Gam5$RECORDID)

fBasics::basicStats(a_classificationed$SURFACE_AREA)
fBasics::basicStats(a_Gam5$SURFACE_AREA)

fBasics::basicStats(a_classificationed$surface_area_ha)
fBasics::basicStats(a_Gam5$surface_area_ha)

fBasics::basicStats(a_Gam5$Year.y) #Delete columns that were created for each GAM; they are artifacts that no longer mean anything useful once the dataset is now in this state. 
85470-85206 # 264 have "1993" in the year column bc this is how many Ecosystem-Year rows were used in this Calc.

sum(is.na(a_Gam5$gam_meth5_L2ecosize))
sum(is.na(a_Gam5$gam_calc5_L2ecoXarea))


# Drop columns that are artifacts -----------------------------------------
# Keep the 2 new columns from the schema's (biomass (kg/ha) and total standing stock (kg)). 
# And keep this connected to the NID. 

colnames(a_Gam1)
aa_Gam1<-a_Gam1 %>% 
  dplyr::select(c(NIDID, gam_meth1_simpleaverage, gam_cacl1_simpleavgXarea))

colnames(a_Gam2)
aa_Gam2<-a_Gam2 %>% 
  dplyr::select(c(NIDID, gam_meth2_median, gam_calc2_medianXarea))

colnames(a_Gam3)
aa_Gam3<-a_Gam3 %>% 
  dplyr::select(c(NIDID, gam_meth3_clustervol, gam_calc3_methXarea))

colnames(a_Gam4)
aa_Gam4<-a_Gam4 %>% 
  dplyr::select(c(NIDID, gam_meth4_L2ecoregion, gam_calc4_L2ecoXarea))

colnames(a_Gam5) #In this case, we'll remove the artifacts and keep NID + the 2 schema5 columns.
aa_Gam5<-a_Gam5 %>% 
  dplyr::select(-c(Ecosystem,
                   LONGITUDE_wasLat,
                   LATITUDE_wasLon,
                   Year.x,
                   YearDamCompleted,
                   Area_acres,
                   NCoves,
                   Species,
                   Biomass_lbsPERacre,
                   Notes,
                   Biomass_corrected_lbsPERacre,
                   Biomass_kgha,ResAge,
                   fit,
                   Year.y,
                   se.fit,
                   minAge))



# Join the 5 schema outputs into one DF -----------------------------------

a_G5_G1<-left_join(aa_Gam5,aa_Gam1)
a_G5_G1_G2<-left_join(a_G5_G1,aa_Gam2)
a_G5_G1_G2_G3<-left_join(a_G5_G1_G2,aa_Gam3)
a_G5_G1_G2_G3_G4<-left_join(a_G5_G1_G2_G3,aa_Gam4)

colnames(a_G5_G1_G2_G3_G4) #use schema1-5 columns in validation. 

a_fish_estimates<-a_G5_G1_G2_G3_G4 # a single dataset with schema 1-5's data collated.



# Further drop or rename confusing columns --------------------------------

colnames(a_fish_estimates)

# Drop these: "clusterL2" & "ClusterEco_L2"; they were replaced more accurately by "L2_ClustReassign" and "ClassTypeL2" 
# Rename 10 columns (2 per schema) for interpretability. +2 others from kmeans script that could do with better names. 

b_tidied_dataset<-a_fish_estimates %>% 
  dplyr::select(-c(clusterL2,ClusterEco_L2)) %>% 
  rename(ClassVolumeNameL2              = Class4Name,
         gam3_k4_on_clustvol            = meth_clustvol_k4,
         
         Schema5_Biomass_kgha           =gam_meth5_L2ecosize,
         Schema5_TotalStandingStock_kg  =gam_calc5_L2ecoXarea,
         
         Schema1_Biomass_kgha           =gam_meth1_simpleaverage,
         Schema1_TotalStandingStock_kg  =gam_cacl1_simpleavgXarea,
         
         Schema2_Biomass_kgha           =gam_meth2_median,
         Schema2_TotalStandingStock_kg  =gam_calc2_medianXarea,
         
         Schema3_Biomass_kgha           =gam_meth3_clustervol,
         Schema3_TotalStandingStock_kg  =gam_calc3_methXarea,
         
         Schema4_Biomass_kgha           =gam_meth4_L2ecoregion,
         Schema4_TotalStandingStock_kg  =gam_calc4_L2ecoXarea)


colnames(b_tidied_dataset)

sum(b_tidied_dataset$Schema5_TotalStandingStock_kg) # 3,855,651,138
sum(b_tidied_dataset$Schema4_TotalStandingStock_kg) # 3,689,453,361
sum(b_tidied_dataset$Schema3_TotalStandingStock_kg) # 2,976,459,235
sum(b_tidied_dataset$Schema2_TotalStandingStock_kg) # 3,587,800,617
sum(b_tidied_dataset$Schema1_TotalStandingStock_kg) # 3,031,693,257



write_csv(b_tidied_dataset,"data output/Dataset S3 - USA Reservoir Biomass, Total Standing Stock, Clusters, and NIDIDs.csv")



