## ---------------------------
## Author: Christine A. Parisek, caparisek@ucdavis.edu
## This script: Checks effect of reservoir age on the biomass trend. 
## Use classification as random effect and ecosystem name as a random effect nested in that. 
## ---------------------------

#PACKAGES FOR ANALYSIS
library(tidyverse)
library(data.table)
library(mgcv)       # For GAMMs
library(scales)     # For aesthetics in scale_xy_continuous in ggplot
library(sciplot)    # For calculating Standard Error in Quick Metrics section
library(fBasics)    # Optional; descriptive summary statistics
options(scipen=999) # Optional; removes sci-notation

set.seed(1)




# Read in DF: ROYL -------------------------------------------------

a_FishTB <- read_csv("data/ROYL QAQC TSC/ROYL_TSC_DATA/ROYL_TSC_QAQC_v3_2023_Final_Dataset_2023-06-05.csv")

# Explore
fBasics::basicStats(a_FishTB$Year)
unique(a_FishTB$Ecosystem) #301 unique ecosystems (aka reservoirs)
unique(a_FishTB$NIDID)     #299 unique NIDID

# ~ Why 299 vs 301? >>> 2 dams moved; had new Ecosystem Name but not NIDID. See SI methods.
# Explore with:
#  ROYLuniqueNIDID <- BIOMASS[!duplicated(BIOMASS$NIDID),]   #299 unique NIDID
#  ROYLuniqueECO <- BIOMASS[!duplicated(BIOMASS$Ecosystem),] #301 unique Ecosystem

# Fix 2 affected waterbodies in NIDID in NRRP
## Assign NIDs with "a" and "b" so they are treated as unique reservoirs. 
## Similar procedure was performed to these 2 NIDs in Script Step 01 with NID dataset.

#View the reservoirs in question
x<-a_FishTB %>% 
  dplyr::filter(Ecosystem %in% c('HalesBar_TN', 'Nickajack_TN','Century_TX', 'SulphurSprings_TX')) 

#Assign adjusted NIDID
a_FishTB_HalesBar <- within(a_FishTB, NIDID[Ecosystem == "HalesBar_TN"]       <- 'TN11502a')
a_FishTB_Nickajack <- within(a_FishTB_HalesBar, NIDID[Ecosystem == "Nickajack_TN"]      <- 'TN11502b')
a_FishTB_Century <- within(a_FishTB_Nickajack, NIDID[Ecosystem == "Century_TX"]        <- 'TX04356a')
a_FishTB_SulphurSprings <- within(a_FishTB_Century, NIDID[Ecosystem == "SulphurSprings_TX"] <- 'TX04356b')
a_FishTotalBiomass<-a_FishTB_SulphurSprings

# Check assignments 
x<-a_FishTotalBiomass %>% 
  dplyr::filter((Ecosystem %in% c('HalesBar_TN', 'Nickajack_TN','Century_TX', 'SulphurSprings_TX')))

unique(a_FishTotalBiomass$Ecosystem) # 301 uniques 
unique(a_FishTotalBiomass$NIDID)     # 301 uniques 


write_csv(a_FishTotalBiomass, "data output/Dataset S1 - NRRP ROYL 2023-06-23.csv") # Dataset S1



# APPLY ADJUSTMENT FACTOR: Source: Davies, W.D., and W.L. Shelton. 1983. Sampling with toxicants. pp. 199-213 in L.A. Nielson and D.L. Johnson editors. Fisheries Techniques, 1st Edition. American Fisheries Society, Bethesda MD USA.

a_AdjustmentsTable<-read_csv("data/Table 10.1 Adjustments to Cove Rotenone Data.csv")
colnames((a_AdjustmentsTable))

mean(a_AdjustmentsTable$Adjustments_for_NonRecovery_1,na.rm=TRUE) # 1.773056

b_Biomass_CorrectedConverted<-a_FishTotalBiomass %>% 
  mutate(Biomass_corrected_lbsPERacre = (Biomass_lbsPERacre*1.773056)) %>%  # Multiply biomass by adj factor 
  mutate(Biomass_kgha = (Biomass_corrected_lbsPERacre*0.453592)/0.404686)   # Convert lb/acre to kg/ha

fBasics::basicStats(b_Biomass_CorrectedConverted$Biomass_kgha) # Explore



# Read in DF: Classification  --------
a_classificationed<-read_csv("data output/02_L1L2L3_Clustered.csv") # The output DF from Script 02.
colnames(a_classificationed)

# Create columns for GAM 1-5 groupings -------------------------------------
### GAM will use "by=" to smooth on for 4 of the 5 models. Create those columns in the large DF now, otherwise later you'd have to do it twice (once for the ROYL df that has predictions, and once to the larger df once getting into the calculations step. that would be messy.) 
# GAM 1 - for calc "Simple Average" - requires no new column to be made. 
# GAM 2 - for calc "Large&Small (Median)" - requires a column labeling res as "Large" or "Small" based on median value (CREATE THIS)
# GAM 3 - for calc "Cluster Volume" - requires a column clustering based on size-discharge (volume) (CREATED AT END OF KMEANS SCRIPT, SO LABELING METH_CLUSTVOL_K4 EACH TIME IS NOT NECESSARY)
# GAM 4 - for calc "Ecoregion" - requires column of each res paired with ecoregion (THIS EXISTS FROM PREVIOUS CODE; FINE AS IS)
# GAM 5 - for calc "Eco-Flow Cluster" - requires column based on unique ecoregion-clustervol CLASS (THIS EXISTS FROM PREVIOUS CODE; JUST RENAME)


# [GAM1] - no columns to create. 

# [GAM2] for median (large-small) calculation#
## Median uses "surface_area_ha_HAS_NA" with original NID values, not "surface_area_ha" which has NAs filled in using rules.
median(a_classificationed$surface_area_ha_HAS_NA,na.rm=TRUE) # Col. w original values (NAs present): median = 4.856232

a_ResLargeSmall<-a_classificationed %>% 
  mutate(gam2_BY_LargeSmall = case_when(surface_area_ha_HAS_NA <= 4.856232 ~ "Small", surface_area_ha_HAS_NA > 4.856232 ~ "Large"))

a_ResLargeSmall %>% # Explore. NA will be filled using rules (see Table S2)
  ggplot()+
  geom_point(aes(x=nidstorage_cm_logscale, y=maxdischarge_cms_logscale, color=gam2_BY_LargeSmall),
             alpha=0.4,size=5)

# [GAM3] - no columns to create. View: 
a_classificationed %>%
  ggplot()+
  geom_point(aes(x=nidstorage_cm_logscale, y=maxdischarge_cms_logscale, color=gam3_BY_ClusterVol),
             alpha=0.5, size=5)


# [GAM4] - no columns to create; Omernik Level 2 Ecoregions.
# [GAM5] - no columns to create; Created in Script 02 Kmeans. 


# Join - NID2018 to ReservoirFishBiomass  ---------------------------------

c_join_NIDbiomass <-left_join(a_ResLargeSmall, b_Biomass_CorrectedConverted, by="NIDID") #df larger now bc multiple year obs. per NIDID
86295-85467

unique(c_join_NIDbiomass$Ecosystem) # Explore; 301 + NA = 302 unique

fBasics::basicStats(c_join_NIDbiomass$Biomass_kgha)            # Explore
fBasics::basicStats(b_Biomass_CorrectedConverted$Biomass_kgha) # Explore

x<-c_join_NIDbiomass %>% group_by(NA_L2NAME)           %>% tally(sort=TRUE) %>% print(n=Inf) %>% ungroup() # Explore
x<-c_join_NIDbiomass %>% group_by(NA_L2NAME_lowercase) %>% tally(sort=TRUE) %>% print(n=Inf) %>% ungroup() # Explore





# Convert to Char to Fac --------------------------------------------------
# GAM requires character columns be converted to factor. 

class(c_join_NIDbiomass$Ecosystem)           #for all GAMs. all character -> should be a factor if used in GAM. 
class(c_join_NIDbiomass$gam2_BY_LargeSmall)  #for gam 2
class(c_join_NIDbiomass$gam3_BY_ClusterVol)  #for gam 3
class(c_join_NIDbiomass$NA_L1NAME)           #for gam 4
class(c_join_NIDbiomass$NA_L2NAME_lowercase) #for gam 4
class(c_join_NIDbiomass$NA_L3NAME)           #for gam 4
class(c_join_NIDbiomass$ClassTypeL2)         #for gam 5

c_join_NIDbiomass$Ecosystem           <- as.factor(c_join_NIDbiomass$Ecosystem)
c_join_NIDbiomass$gam2_BY_LargeSmall  <- as.factor(c_join_NIDbiomass$gam2_BY_LargeSmall)
c_join_NIDbiomass$gam3_BY_ClusterVol  <- as.factor(c_join_NIDbiomass$gam3_BY_ClusterVol)
c_join_NIDbiomass$NA_L1NAME           <- as.factor(c_join_NIDbiomass$NA_L1NAME)
c_join_NIDbiomass$NA_L2NAME_lowercase <- as.factor(c_join_NIDbiomass$NA_L2NAME_lowercase)
c_join_NIDbiomass$NA_L3NAME           <- as.factor(c_join_NIDbiomass$NA_L3NAME)
c_join_NIDbiomass$ClassTypeL2         <- as.factor(c_join_NIDbiomass$ClassTypeL2)

is.factor(c_join_NIDbiomass$Ecosystem)  
is.factor(c_join_NIDbiomass$gam2_BY_LargeSmall) 
is.factor(c_join_NIDbiomass$gam3_BY_ClusterVol) 
is.factor(c_join_NIDbiomass$NA_L1NAME) 
is.factor(c_join_NIDbiomass$NA_L2NAME_lowercase) 
is.factor(c_join_NIDbiomass$NA_L3NAME) 
is.factor(c_join_NIDbiomass$ClassTypeL2) 

#  x_examine <- c_join_NIDbiomass %>% 
#    dplyr::select(DAM_NAME, Ecosystem, max_discharge_cms, nid_storage_cm,
#           LATITUDE, LATITUDE_wasLon,  
#           LONGITUDE, LONGITUDE_wasLat, 
#           Biomass_kgha,YearDamCompleted,Year, L2_ClustReassign, Class4Name, ClassTypeL2) %>% 
#    arrange(YearDamCompleted) %>% 
#    ungroup()
#  
#  x_examine<-c_join_NIDbiomass %>% group_by(Class4Name,ClassTypeL2) %>% tally() %>% print(n=Inf) %>% ungroup()


# Create Reservoir-Age Column ---------------------------------------------
# YearSampled minus YearDamCompleted = Age at time of sampling
c_ResFishData<-c_join_NIDbiomass %>% 
  mutate(ResAge = (Year - YearDamCompleted)) 


fBasics::basicStats(c_ResFishData$Biomass_kgha) # Basic stats for the Biomass column
fBasics::basicStats(c_ResFishData$ResAge)       # Basic stats for the ResAge column 

class(c_ResFishData$Year)             #Confirm correct class. 
class(c_ResFishData$YearDamCompleted) #Confirm correct class. 
class(c_ResFishData$ResAge)           #Confirm correct class. 

x<-c_ResFishData %>% group_by(Ecosystem)   %>% tally(sort=TRUE)  %>% print(n = Inf) %>% ungroup() # Explore
x<-c_ResFishData %>% group_by(ResAge)      %>% tally(sort=FALSE) %>% print(n = Inf) %>% ungroup() # Explore
x<-c_ResFishData %>% group_by(ClassTypeL2) %>% tally(sort=TRUE)  %>% print(n = Inf) %>% ungroup() # Explore

# ~~~~  ------------------------------------------------------------------

# GAM 5 - set up --------------------------------------------------------------------

# FOR CALC 5: ECO-SIZE-FLOW
## A level II ecoregion was identified for every reservoir. Within every ecoregion of the USA, a cluster analysis constrained to four clusters was performed using volume (i.e., storage capacity) and maximum discharge. Mean fish mass was calculated across all reservoirs for a given cluster. We use cluster mean fish mass values to assign a fish mass value to any reservoir missing empirical data.

m5_ResFish<-c_ResFishData %>%  # Refine dataset prior to GAM; GAM only done on those Ecosystems with biomass data & class data.
  dplyr::filter((!is.na(Class4Name))
                &!is.na(Ecosystem))%>%
  group_by(ClassTypeL2) %>%
  dplyr::filter(n()>5) %>%  # Filter out any CLASS with <x data points
  ungroup()

unique(m5_ResFish$ClassTypeL2) # 105 levels recognized; 79 visible. NOTE: Simon Wood {mgcv}: "by default unused levels are dropped from factors before fitting." (https://cran.r-project.org/web/packages/mgcv/mgcv.pdf) 


# GAM 5 - model -----------------------------------------------------------
### GAM based on needs of CALC5 (by = the ecoregion-flow-size cluster)

#check columns used in model are either "numeric" or "factor" (requirement of model)
class(m5_ResFish$Biomass_kgha)
class(m5_ResFish$ResAge)
class(m5_ResFish$Year)
class(m5_ResFish$ClassTypeL2)
class(m5_ResFish$Ecosystem)
sum(is.na(m5_ResFish$ClassTypeL2))

m5 <- mgcv::gam(Biomass_kgha ~ s(ResAge, bs="tp", by=ClassTypeL2) + 
                  s(Ecosystem, bs="re")+
                  s(Year, bs="tp"),
                family=Gamma(link=log), data=m5_ResFish, method="REML")  

summary.gam(m5)

gam.check(m5) 

plot.gam(m5, page=1, 
         all.terms=TRUE, 
         rug=TRUE,residuals=TRUE,pch = 1, cex = 1,
         shade=TRUE,shade.col = "lightblue",
         shift = coef(m5)[1]) # Need Rstudio "plots" pane to be large or will result in error.


# GAM 5 - predict ---------------------------------------------------------

#Create new DF for Predict()
m5_MinYearRange<-m5_ResFish %>% 
  group_by(Ecosystem) %>%               # Prediction by ECOSYSTEM (each ecosystem needs a prediction even though m5's smooth is by=class)
  summarize(minAge = min(ResAge) ) %>%  # Create column with the minimum age each reservoir had been when sampled 
  tidyr::expand_grid(AgePred = ((min(m5_ResFish$ResAge)):130)) %>% #add Age-To-Predict-On col by min(ResAge):105
  dplyr::filter(minAge <= AgePred ) %>%  # Summarize causes dup rows; filter so that each res gets a unique AgePrediction
  ungroup()

#View new DF
x<-m5_MinYearRange %>%   
  arrange(Ecosystem,AgePred)  

# Want new DF to also have attributes related to CLASS & YEARDAMCOMPLETED:
m5_TheRestOfPredDF<-m5_ResFish %>%
  ungroup() %>% 
  dplyr::select("Ecosystem","YearDamCompleted", "ClassTypeL2") %>%  
  distinct() 

x<-m5_TheRestOfPredDF %>% group_by(Ecosystem) %>% tally(sort=FALSE) %>% print(n= Inf ) %>% ungroup() # tally check

# Join the DF with Age Predictions to the Attributes we want still associated with them...
m5_Join_forPredDF<-left_join(m5_TheRestOfPredDF,m5_MinYearRange)

m5_Predictable_FishData<-m5_Join_forPredDF %>% 
  rename(ResAge = AgePred) %>%               # Predict()'s NewDF doesn't have actual-age, so it's ok to rename to the name used in GAM. 
  arrange(ClassTypeL2,ResAge) %>%            # Arrange Res within a ClassificationType by Age; a helpful visual 
  mutate(Year = (YearDamCompleted + ResAge)) # Predict's NewDF also doesn't have Year_Sampled (needed in GAM model too)

m5_predicted<-predict(m5, m5_Predictable_FishData, type="response", se.fit = TRUE)

m5_predbind<-do.call(cbind,m5_predicted) # Iterated for each ecoregion & pulled into one df 
m5_predtable<-data.table(m5_predbind)
m5_pred_final<-cbind(m5_Predictable_FishData,m5_predtable)



# GAM 5 - plot - fit~resage (Figure S1) -----------------------------------------------
### PredictBiomass~ResAge Plots ... with raw data overlayed 

# Link true biomass values to the rows they belong to. For all else in this column, NA 
# Doing this will let us see raw data overlayed on predictions (plot1 - Figure S1) 
#             & compare/validate true vs predicted biomass 1:1 (plot2 - Figure S4)


m5_small<-m5_ResFish %>% 
  ungroup() %>% 
  dplyr::select("Ecosystem","ResAge", "Biomass_kgha") #2 joining columns + the true empirical biomass column

m5_Actual_and_Pred_data<-left_join(m5_pred_final,m5_small, by = c("Ecosystem","ResAge")) #bind predictions to true biomass DF



# Get mean of fit & mean of se.fit
m5_meanfit<-m5_Actual_and_Pred_data %>% 
  group_by(ClassTypeL2,Year) %>% 
  mutate(mean_parent_fit     = mean(fit)) %>% 
  mutate(mean_parenet_se.fit = mean(se.fit)) %>% 
  arrange(ClassTypeL2,Year) %>% 
  ungroup()

m5_mean_uniques <- m5_meanfit[!duplicated(m5_meanfit$mean_parent_fit),] # Reduce dups. for geom_"mean" 
m5_mean_unique  <- m5_mean_uniques %>% dplyr::filter(Year<=1993)        # Cut off at YEAR  


# % Change per Ecoregion - Identify % change from 1st to last year. 
z_PChange<-m5_mean_unique %>% 
  select(-c(YearDamCompleted,minAge,ResAge,mean_parenet_se.fit,Biomass_kgha,se.fit, Ecosystem)) %>% 
  group_by(ClassTypeL2) %>% 
  arrange(Year) %>% 
  mutate(initial_biomass = first(mean_parent_fit),
         final_biomass = last(mean_parent_fit),
         pct_change = (((final_biomass - initial_biomass) / final_biomass) * 100)) %>% 
  ungroup()

z_PChangeUnique <- z_PChange[!duplicated(z_PChange$ClassTypeL2),] #Get 1 row per unique class with its %change

z_LabelsForFacets<-z_PChangeUnique %>% select(ClassTypeL2,pct_change) #These are the labels to use in the object: dat_text

# Labels to add % change to plot 
dat_text <- data.frame(
  ClassTypeL2   = c("Mississippi Alluvial and Southeast Coastal Plains, Large Volume & High Discharge",
                    "Mississippi Alluvial and Southeast Coastal Plains, Large Volume & Low Discharge",
                    "Ozark-Ouachita Appalachian Forests, Large Volume & High Discharge",
                    "Ozark-Ouachita Appalachian Forests, Large Volume & Low Discharge",
                    "South Central Semiarid Prairies, Large Volume & High Discharge",
                    "South Central Semiarid Prairies, Small Volume & Low Discharge",
                    "Southeastern Plains, Large Volume & High Discharge",
                    "Southeastern Plains, Large Volume & Low Discharge",
                    "Southeastern Plains, Small Volume & High Discharge",
                    "Temperate Prairies, Large Volume & High Discharge"),
  label =         c( "49.0%",
                    "-87.0%",
                    "-109.3%",
                     "78.9%",
                      "5.0%",  
                     "18.0%",
                     "78.2%",
                     "83.8%",
                     "77.0%",
                     "6.2%"))


m5_Actual_and_Pred_data %>%
  dplyr::filter(Year<=1993) %>% 
  ggplot()+
  geom_point(aes(x=Year, y=fit),            color="gray50",         size=2,alpha=0.3,shape=16)+#predicted
  geom_point(aes(x=Year, y=Biomass_kgha),   color="cornflowerblue", size=3,alpha=0.3,shape=16)+#observed
  geom_ribbon(data=m5_mean_unique, aes(x=Year, 
                  ymax=((mean_parent_fit)+(mean_parenet_se.fit)), 
                  ymin=((mean_parent_fit)-(mean_parenet_se.fit))), fill="brown1", alpha=0.3)+  #mean_predict_SE
  geom_point(data=m5_mean_unique, aes(x=Year, y=mean_parent_fit), color="grey0",          size=1,alpha=0.7,shape=16)+#mean_predict
  scale_x_continuous(breaks = seq(1948, 1993,5), limits=c(1948, 1993))+
  scale_y_continuous(breaks = seq(0, 3100, 1000), limits=c(-300, 3200))+ #parent_se.fit goes neg.
  facet_wrap(.~ClassTypeL2, ncol=2,labeller=label_wrap_gen(width=30,multi_line = TRUE))+
  geom_text(
    data    = dat_text,
    mapping = aes(x = Inf, y = Inf, label = label),
    hjust   = 1.08,
    vjust   = 1.5)+
  theme_bw()+
  theme(axis.title.x=element_text(size=12.8), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=11.5,angle=50,vjust=0.6),
        axis.text.y =element_text(size=11.5),
        #legend.position = "none",
        strip.text = element_text(size=11),
        strip.background = element_rect(fill=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  labs( x = "Year", y = expression(paste("Fish Biomass"~('kg'~ha^-1))))

ggsave("figures/Fig_S2_GAM5_Biomass_Real_and_Predicted_by_Year.tiff", height=29, width=15, units="cm", dpi=900)

#dev.off()


# GAM 5 - plot - validation (Figure S4) -----------------------------------------------
# VALIDATE 

m5_truebio_and_predbio <-m5_Actual_and_Pred_data %>% 
  dplyr::filter(!is.na(Biomass_kgha))

m5_truebio_and_predbio %>%
  ggplot(aes(x=fit, y=Biomass_kgha)) +
  geom_point(size=3.0, alpha=0.5, color="cornflowerblue")+
  geom_abline(aes(intercept=0.0,slope=1.0), size=1.0, alpha=0.8, linetype="solid")+
  scale_x_continuous(breaks = seq(0, 3090, 1000),limits=c(-5, 3300), expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0, 3090, 1000),limits=c(-5, 3300), expand = c(0, 0))+
  facet_wrap(.~ClassTypeL2, ncol=2,labeller=label_wrap_gen(width=30,multi_line = TRUE))+
  theme_bw()+
  theme(axis.title.x=element_text(size=12.8), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=11.5,angle=50,vjust=0.6),
        axis.text.y =element_text(size=11.5),
        legend.position = "none",
        strip.text = element_text(size=11),
        strip.background = element_rect(fill=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  labs(x = expression(paste("Predicted Fish Biomass"~('kg'~ha^-1))), y = expression(paste("Observed Fish Biomass"~('kg'~ha^-1))))

ggsave("figures/Fig_S4_GAM5_42Validation_Observed_vs_Predicted.tiff", height=29, width=15, units="cm", dpi=900) 





# Year Cut-Off - mean SE fit  --------------------------------------------------------
# Used 3 methods to determine what year to cut off fit predictions.
# Here, which YEAR is last to have fit predictions that are < 2x the mean SE of fit 

m5_mean_full_SE<-m5_Actual_and_Pred_data %>% 
  group_by(Year) %>% # Mean just by YEAR, not YEAR & CLASS like before. 
  mutate(mean_year_se.fit = mean(se.fit)) %>% 
  ungroup()

m5_SE_year <- m5_mean_full_SE[!duplicated(m5_mean_full_SE$Year),] # Make 1 row per x
colnames(m5_SE_year)

x<-m5_SE_year %>% 
  dplyr::select(Year,mean_year_se.fit) # Explore

m5_SE_year %>%
  ggplot()+
  geom_point(aes(x=Year, y=mean_year_se.fit), color="brown3", size=2,alpha=0.9,shape=16)+
  scale_x_continuous(breaks = seq(1920, 2020, 5), limits=c(1920, 2020))+
  scale_y_continuous(breaks = seq(0, 1000, 100), limits=c(0,1000))+
  #facet_wrap(.~ClassTypeL2, ncol=4,labeller=label_wrap_gen(width=31,multi_line = TRUE))+
  theme_bw()+
  theme(axis.title.x=element_text(size=15), 
        axis.title.y=element_text(size=15), 
        axis.text.x =element_text(size=15,angle=90),
        axis.text.y =element_text(size=15),
        legend.position = "none",
        #strip.text = element_text(size=12),
        #strip.text = element_text(face="bold", size=9,lineheight=5.0
        #strip.background = element_rect(fill="lightblue", colour="black", size=1),
        #panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) + 
  labs( x = "Year", y = expression(paste("Mean SE of Fish Biomass Predictions"~('kg'~ha^-1))))

#no ggsave; not a paper figure. 



# Year Cut-Off - st.dev of fit ----------------------------------------------------------
# Used 3 methods to determine what year to cut off fit predictions.
# Here, which YEAR is last to have fit predictions that are < 2x the mean STDEV of fit 

years_N_in_mean_fit<-m5_Actual_and_Pred_data %>% #tally (N) of years used in the mean(fit) calc. 
  group_by(Year) %>% 
  tally() %>% 
  ungroup()

Years_n<-left_join(m5_SE_year,years_N_in_mean_fit)

stdev_on_mean_year<-Years_n %>% 
  mutate(stdev = (mean_year_se.fit* (sqrt(n)))) 

# Determine predict-year cutoff (see se fit and stddev fit side by side.)
## 1st - Sort by year. In the year 1978 (last year w empirical data), take value in SE or STDEV and multiply it by 2. 
## 2nd - Scroll down to find the future year whose value is not above this (1993). 
x_YearCutoffViewer <-stdev_on_mean_year %>% 
  dplyr::select(Year,mean_year_se.fit,stdev) 

stdev_on_mean_year %>%
  dplyr::filter(Year<=2020) %>% 
  ggplot()+
  geom_vline(xintercept = 1993, linetype="solid", color = "black", size=0.5)+
  geom_vline(xintercept = 1978, linetype="solid", color = "black", size=0.5)+
  geom_vline(xintercept = 1948, linetype="solid", color = "black", size=0.5)+
  geom_point(aes(x=Year, y=stdev), color="blue", size=2,alpha=0.5,shape=16)+
  geom_point(aes(x=Year, y=mean_year_se.fit), color="brown3", size=2,alpha=0.5,shape=16)+
  scale_x_continuous(breaks = seq(1920, 2020, 5), limits=c(1920, 2020))+
  theme_bw()+
  theme(axis.title.x=element_text(size=15), 
        axis.title.y=element_text(size=15), 
        axis.text.x =element_text(size=15,angle=90),
        axis.text.y =element_text(size=15),
        panel.grid.major=element_blank())+
  labs( x = "Year", 
        y = "mean_year_se.fit (red)  -OR-  stdev (blue)")


# Year Cut-Off - broken stick ------------------------------------------------------------
# Used 3 methods to determine what year to cut off fit predictions.
# Here, used broken stick method to ID break point. 

library(segmented)

df<-m5_SE_year %>% 
  dplyr::filter(Year<2025)

fit <- lm(mean_year_se.fit ~ Year, df)
summary(fit)
seg <- segmented(fit, seg.Z = ~Year)
plot(mean_year_se.fit ~ Year, df, xaxp = c(1925, 2025, 100)) # plot points
plot(seg, add = TRUE)                                        # plot broken line
abline(v = seg$psi[2])                                       # plot vertical line 



# ~~~ 


# GAM 5 - plot - histogram -------------------------------------------------------
# of then-vs-now biomass; did not use this figure in paper

m5_histogram_1993mean<-m5_Actual_and_Pred_data %>% 
  group_by(ClassTypeL2) %>% 
  dplyr::filter(Year==1993) %>% 
  mutate(MaxYr_mean = mean(fit)) %>% 
  dplyr::select("Ecosystem","ClassTypeL2","MaxYr_mean") %>% 
  ungroup()

m5_histogram_maxyr<-left_join(m5_Actual_and_Pred_data,m5_histogram_1993mean, by=c("Ecosystem","ClassTypeL2"))

m5_histogram_justempirical<-m5_histogram_maxyr %>% 
  dplyr::filter(!is.na(Biomass_kgha))

m5_histogram_minmax_yr<-m5_histogram_justempirical %>% 
  group_by(Ecosystem) %>% 
  dplyr::filter(Year==min(Year)) %>% 
  mutate(MinYr = Biomass_kgha) %>% 
  ungroup()

m5_histogram_difference<-m5_histogram_minmax_yr %>% 
  group_by(ClassTypeL2) %>% 
  mutate(difference_biomass =  MaxYr_mean - MinYr) %>% 
  arrange(Ecosystem,fit) %>% 
  ungroup()

fBasics::basicStats(m5_histogram_difference$difference_biomass)

x<-m5_histogram_difference %>% 
  group_by(ClassTypeL2) %>% 
  summarize(mean = mean(difference_biomass),
            Min  =min(difference_biomass),
            Max  =max(difference_biomass), 
            SD   =sd (difference_biomass)) %>% ungroup()

m5_histogram_difference %>% 
  ggplot(aes(x=difference_biomass))+
  geom_histogram(aes(y=..count../sum(..count..)),bins=30,fill="cornflowerblue",color="slategray1") +
  scale_x_continuous(breaks = seq(-2500, 1000, 500), limits=c(-2500,1000))+
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1))+
  geom_abline(aes(intercept=0,slope=1), color="gray60",size=1, alpha=0.9, linetype="dashed")+
  theme_bw()+
  theme(axis.title.x=element_text(size=15), 
        axis.title.y=element_text(size=15), 
        axis.text.x =element_text(size=14),
        axis.text.y =element_text(size=14),
        legend.position = "none",
        strip.text = element_text(size=12),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
  #annotate(geom="text", x=-2120, y=.20, color="black", size=5, label="Mean = 104.17 ")+
  #annotate(geom="text", x=-1850, y=.19, color="black", size=5, label="Range = -2326.39 - 859.44")+
  #annotate(geom="text", x=-2090, y=.18, color="black", size=5, label="SE Mean = 23.72")+
  #annotate(geom="text", x=-2216, y=.17, color="black", size=5, label="SD = 388.41")+
  #labs( x = expression(paste("Raw Difference in Biomass"~('kg'~ha^-1))))



# GAM 5 - USA calculation -------------------------------------------------------
# See Table S2 for steps. 

colnames(m5_Actual_and_Pred_data)

# Create DF with only the columns needed (otherwise it'll ".y" to column names in next step)
m5_1993_predicted_biomass<-m5_Actual_and_Pred_data %>% 
  dplyr::filter(Year==1993) %>% 
  dplyr::select("Ecosystem","ResAge", "Year", "fit", "se.fit", "minAge")

m5_1993pred_join        <- left_join(c_join_NIDbiomass,m5_1993_predicted_biomass, by="Ecosystem")
m5_unique_res           <- m5_1993pred_join[!duplicated(m5_1993pred_join$NIDID),] #make 1 row per res again. 85767 rows looks good
m5_1993pred_and_therest <- m5_unique_res


# Calculation 5 - L2 EcoSizeCluster

## Split into 2 DFs - one with NA for fit, one without. To the NA one, follow steps in Table S2.
m5_V_ecosize_NA<- m5_1993pred_and_therest %>% 
  group_by(ClusterEco_L2) %>% 
  dplyr::filter(all(is.na(fit)) | is.na(ClusterEco_L2)) %>% # Any ecoregion not in NRRP, or NA ecoregion, filters out. 
  ungroup()

m5_V_ecosize<-m5_1993pred_and_therest[(!m5_1993pred_and_therest$NIDID %in% m5_V_ecosize_NA$NIDID),] 

m5_V_meanECOSIZE <- m5_V_ecosize %>% # There's no fit NA, so this copies the fit column into a new one.
  group_by(ClusterEco_L2) %>% 
  mutate(gam_meth5_L2ecosize = case_when(is.na(fit) ~ mean(fit, na.rm=TRUE),
                                     TRUE ~ as.numeric(fit))) %>%
  ungroup()

m5_V_meanECOSIZE_NA <- m5_V_ecosize_NA %>% # This case_when does the thing. 
  group_by(ClusterEco_L2) %>% 
  mutate(gam_meth5_L2ecosize = case_when(is.na(fit) ~ mean(m5_1993pred_and_therest$fit, na.rm=TRUE),
                                     TRUE ~ as.numeric(fit))) %>% 
  ungroup()

m5_V_meanECOSIZE   <-data.table(m5_V_meanECOSIZE) 
m5_V_meanECOSIZE_NA<-data.table(m5_V_meanECOSIZE_NA)
m5_V_bound<-rbind(m5_V_meanECOSIZE,m5_V_meanECOSIZE_NA) # Bind 2 split DFs back together

sum(is.na(m5_V_bound$gam_meth5_L2ecosize))

x<-m5_V_bound %>% #Explore
  group_by(ClassTypeL2) %>% 
  summarise(average = mean(gam_meth5_L2ecosize,na.rm=TRUE)) %>% 
  arrange(desc(average)) %>% 
  print(n=Inf) %>% 
  ungroup()


# Multiply by AREA to get the CALC. (See SI Methods)

m5_V_final <- m5_V_bound %>% 
  mutate(gam_calc5_L2ecoXarea = ((m5_V_bound$gam_meth5_L2ecosize)*(m5_V_bound$surface_area_ha))) 

sum(m5_V_final$gam_calc5_L2ecoXarea, na.rm=TRUE)  #Schema5 USA - Total Standing Stock (KG)
              
write_csv(m5_V_final,"data output/03_USA_GAM5_fit.csv") 



# GAM 5 - South calculation -----------------------------------------------------------

m5s_NA_biomass_states<-m5_V_final %>% # Remove states not in NRRP data set.
  group_by(STATE) %>% 
  dplyr::filter(all(is.na(Biomass_kgha)) | is.na(STATE)) %>% 
  ungroup()

m5S_SE_biomass<-m5_V_final[(!m5_V_final$RECORDID %in% m5s_NA_biomass_states$RECORDID),] #Keep states that are in the original ROYL dataset. 

x<-m5s_NA_biomass_states %>% group_by(STATE) %>% tally() %>% print(n=Inf) %>% ungroup() #View
x<-m5S_SE_biomass        %>% group_by(STATE) %>% tally() %>% print(n=Inf) %>% ungroup() #View

sum(m5S_SE_biomass$gam_calc5_L2ecoXarea, na.rm=TRUE) #Schema5 SOUTH - Total Standing Stock (KG)



# ~~~~ --------------------------------------------------------------------

# GAM 1 - set up --------------------------------------------------------------------
# FOR CALC 1: SIMPLE AVERAGE. A mean fish mass was calculated across all USA reservoirs. Reservoirs with no empirical biomass value were given this mean.

# Didn't make new columns for G1, so nothing new to convert from character to factor for GAM. 
###  class()                             
###  as.factor() 
###  is.factor() 
###  m1_freshDF <- c_ResFishData # Nothing to filter out either. GAM1 is simplest in prep & model structure.

### Started back with the original DF used at the top of GAM 5 ("c_ResFishData")
m1_freshDF <- c_ResFishData %>% 
  dplyr::filter(!is.na(Ecosystem))



# GAM 1 - model -----------------------------------------------------------
### GAM based on needs of CALC1 (by= NOTHING)

class(m1_freshDF$Biomass_kgha)
class(m1_freshDF$ResAge)
class(m1_freshDF$Ecosystem)
class(m1_freshDF$Year)

m1 <- mgcv::gam(Biomass_kgha ~ s(ResAge, bs="tp") + 
                  s(Ecosystem, bs="re")+
                  s(Year, bs="tp"),
                family=Gamma(link=log), data=m1_freshDF, method="REML")  

summary.gam(m1)

gam.check(m1)

plot.gam(m1, page=1, 
         all.terms=TRUE, 
         rug=TRUE,residuals=TRUE,pch = 1, cex = 1,
         shade=TRUE,shade.col = "lightblue",
         shift = coef(m1)[1])



# GAM 1 - predict ---------------------------------------------------------

#Create new DF for Predict()
m1_MinYearRange<-m1_freshDF %>% 
  group_by(Ecosystem) %>%                # Prediction by ECOSYSTEM (each ecosystem needs a prediction even though m1's smooth is by=NOTHING)
  summarize(minAge = min(ResAge) ) %>%   # Create column with the minimum age each reservoir had been when sampled 
  tidyr::expand_grid(AgePred = ((min(m1_freshDF$ResAge)):130))%>% # Add Age-To-Predict-On col by min(ResAge):105
  dplyr::filter(minAge <= AgePred ) %>%  # Summarize causes dup rows; filter so that each res gets a unique AgePrediction
  ungroup()

m1_MinYearRange %>% # Check new DF
  arrange(Ecosystem,AgePred) %>%  
  print(n= Inf )

# Want the new DF to also have attributes related to select(xxx):
m1_TheRestOfPredDF<-m1_freshDF %>%
  ungroup() %>% 
  dplyr::select("Ecosystem","YearDamCompleted") %>% 
  distinct() 

# Join the DF with Age Predictions to the Attributes we want still associated with them...
m1_Join_forPredDF<-left_join(m1_TheRestOfPredDF,m1_MinYearRange)

m1_Predictable_FishData<-m1_Join_forPredDF %>% 
  rename(ResAge = AgePred) %>%               # Predict()'s NewDF doesn't have actual-age, so it's ok to rename to the name used in GAM. 
  arrange(ResAge) %>%                        # Arrange Res within a ClassificationType by Age; a helpful visual 
  mutate(Year = (YearDamCompleted + ResAge)) # Predict's NewDF also doesn't have Year_Sampled (found in GAM model too)

m1_predicted<-predict(m1, m1_Predictable_FishData, type="response", se.fit = TRUE)

m1_predbind<-do.call(cbind,m1_predicted) # Iterated for each ecoregion & pulled into one df 
m1_predtable<-data.table(m1_predbind)
m1_pred_final<-cbind(m1_Predictable_FishData,m1_predtable)





# GAM 1 - plot - fit~resage -----------------------------------------------
### PredictBiomass~ResAge Plots ... with raw data overlayed 
# Link true biomass values to the rows they belong to. for all else in this column, NA 
# doing this will let us see raw data overlayed on predictions (plot1) 
#             & compare/validate true vs predicted biomass 1:1 (plot2)

m1_small<-m1_freshDF %>% 
  ungroup() %>% 
  dplyr::select("Ecosystem","ResAge", "Biomass_kgha") #2 joining columns + the true empirical biomass column

m1_Actual_and_Pred_data<-left_join(m1_pred_final,m1_small, by = c("Ecosystem","ResAge")) #bind predictions to true biomass DF

m1_Actual_and_Pred_data %>% 
  dplyr::filter(Year<=1993) %>% 
  ggplot()+
  geom_point(aes(x=Year, y=fit),            color="gray50",       size=2, alpha=0.3, shape=16) + 
  geom_point(aes(x=Year, y=Biomass_kgha), color="cornflowerblue", size=3, alpha=0.4, shape=16) +
  scale_x_continuous(breaks = seq(1948, 1993, 5), limits=c(1948, 1993))+
  scale_y_continuous(breaks = seq(0, 3500, 500), limits=c(0, 3500))+
  #facet_wrap(.~Ecosystem.y, ncol=4,labeller=label_wrap_gen(width=31,multi_line = TRUE))+
  #scale_color_manual(values = colorRampPalette(glasbey(27))(colourCount)) +
  theme_bw()+
  theme(axis.title.x=element_text(size=12.8), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=11.5),
        axis.text.y =element_text(size=11.5),
        legend.position = "none",
        strip.text = element_text(size=11),
        #strip.text = element_text(face="bold", size=9,lineheight=5.0
        #strip.background = element_rect(fill="lightblue", colour="black", size=1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  labs( x = "Year", y = expression(paste("Fish Biomass"~('kg'~ha^-1))))

# GAM 1 - plot - validation -----------------------------------------------
# VALIDATE

m1_truebio_and_predbio <-m1_Actual_and_Pred_data %>% 
  dplyr::filter(!is.na(Biomass_kgha))

m1_truebio_and_predbio %>% 
  ggplot(aes(x=fit, y=Biomass_kgha)) +
  geom_point(size=3.0, alpha=0.5, color="cornflowerblue")+
  geom_abline(aes(intercept=0.0,slope=1.0), size=1.0, alpha=0.8, linetype="solid")+
  scale_x_continuous(breaks = seq(0, 3100, 500),limits=c(0, 3200), expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0, 3100, 500),limits=c(0, 3200), expand = c(0, 0))+
  theme_bw()+
  theme(axis.title.x=element_text(size=12.8), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=11.5),
        axis.text.y =element_text(size=11.5),
        legend.position = "none",
        strip.text = element_text(size=11),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+ 
  labs(x = expression(paste("Predicted Fish Biomass"~('kg'~ha^-1))), 
       y = expression(paste("Observed Fish Biomass"~('kg'~ha^-1))))


# GAM 1 - plot - histogram -------------------------------------------------------
# of then-vs-now biomass

m1_histogram_1993mean<-m1_Actual_and_Pred_data %>% # group by = NOTHING
  dplyr::filter(Year==1993) %>% 
  mutate(MaxYr_mean = mean(fit)) %>% 
  dplyr::select("Ecosystem","MaxYr_mean") %>% 
  ungroup()

m1_histogram_maxyr<-left_join(m1_Actual_and_Pred_data,m1_histogram_1993mean, by=c("Ecosystem"))

m1_histogram_justempirical<-m1_histogram_maxyr %>% 
  dplyr::filter(!is.na(Biomass_kgha))

m1_histogram_minmax_yr<-m1_histogram_justempirical %>% 
  group_by(Ecosystem) %>% 
  dplyr::filter(Year==min(Year)) %>% 
  mutate(MinYr = Biomass_kgha) %>% 
  ungroup()

m1_histogram_difference<-m1_histogram_minmax_yr %>% # group by = NOTHING
  mutate(difference_biomass =  MaxYr_mean - MinYr) %>% 
  arrange(Ecosystem,fit) %>% 
  ungroup()

fBasics::basicStats(m1_histogram_difference$difference_biomass)

m1_histogram_difference %>% 
  ggplot(aes(x=difference_biomass))+
  geom_histogram(aes(y=..count../sum(..count..)),bins=30,fill="cornflowerblue",color="slategray1") +
  scale_x_continuous(breaks = seq(-2500, 1000, 500), limits=c(-2500,1000))+
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1))+
  geom_abline(aes(intercept=0,slope=1), color="gray60",size=1, alpha=0.9, linetype="dashed")+
  theme_bw()+
  theme(axis.title.x=element_text(size=12.8), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=11.5),
        axis.text.y =element_text(size=11.5),
        legend.position = "none",
        strip.text = element_text(size=11),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())


# GAM 1 - USA calculation -------------------------------------------------------

colnames(m1_Actual_and_Pred_data)

# Select necessary columns only pre-join or you end up with .y in column names.
m1_1993_predicted_biomass<-m1_Actual_and_Pred_data %>% 
  dplyr::filter(Year==1993) %>% 
  dplyr::select("Ecosystem","minAge","ResAge", "Year", "fit", "se.fit") 

m1_a_1993pred_join<-left_join(c_join_NIDbiomass, m1_1993_predicted_biomass, by="Ecosystem") 
m1_unique_res <- m1_a_1993pred_join[!duplicated(m1_a_1993pred_join$NIDID),] #make 1 row per res again.
m1_a_1993pred_and_therest<-m1_unique_res

sum(!is.na(m1_a_1993pred_and_therest$fit))

colnames(m1_a_1993pred_and_therest)
x<-m1_a_1993pred_and_therest %>% 
  dplyr::filter(!is.na(fit))


#Calculation 1 - Simple Average
fBasics::basicStats(m1_a_1993pred_and_therest$Biomass_kgha)
mean(m1_a_1993pred_and_therest$Biomass_kgha,na.rm=TRUE) #mean biomass of all res = 472.253 (prev. mean of TSC)
mean(m1_a_1993pred_and_therest$fit,na.rm=TRUE) 

# new column = (when there is an NA biomass value) ~ then (put the mean of all res we have data for), but (if the row has empirical data already the retain that value)
m1_b__methavg1<-m1_a_1993pred_and_therest %>% 
  mutate(gam_meth1_simpleaverage = case_when(is.na(fit) ~ mean(fit, na.rm=TRUE),
                                         TRUE ~ as.numeric(fit)))

m1_c_calc1<- m1_b__methavg1 %>% 
  mutate(gam_cacl1_simpleavgXarea = ((gam_meth1_simpleaverage)*(m1_b__methavg1$surface_area_ha))) # kg/ha * ha = kg

sum(is.na(m1_c_calc1$gam_cacl1_simpleavgXarea)) 
sum(m1_c_calc1$gam_meth1_simpleaverage) 
sum(m1_c_calc1$gam_cacl1_simpleavgXarea,na.rm=TRUE) # Schema1 USA Total Standing Stock (KG)

write_csv(m1_c_calc1,"data output/03_USA_GAM1_fit.csv") 


# GAM 1 - South -----------------------------------------------------------

m1s_NA_biomass_states<-m1_c_calc1 %>% #Remove states not in ROYL data set.
  group_by(STATE) %>% 
  dplyr::filter(all(is.na(Biomass_kgha)) | is.na(STATE)) %>%   #genius; very clean. 
  ungroup()

m1S_SE_biomass<-m1_c_calc1[(!m1_c_calc1$RECORDID %in% m1s_NA_biomass_states$RECORDID),] #Keep states that are in the original ROYL dataset. 

x<-m1s_NA_biomass_states %>% group_by(STATE) %>% tally() %>% print(n=Inf) %>% ungroup() #View
x<-m1S_SE_biomass %>% group_by(STATE) %>% tally() %>% print(n=Inf) %>% ungroup()        #View

sum(m1S_SE_biomass$gam_cacl1_simpleavgXarea,na.rm=TRUE) # Schema1 SOUTH Total Standing Stock (KG)


# ~~~~ --------------------------------------------------------------------




# GAM 2 - set up --------------------------------------------------------------------
# FOR CALC 2: Large & Small. Reservoirs were grouped into two classes, large and small. Median reservoir surface area for southern impoundments was 4.047 ha, and 4.856 ha for all USA impoundments; thus reservoirs below these thresholds were considered “small” while ones above were “large”. A mean fish mass was calculated for both large and small reservoirs and these values used for reservoirs where no biomass values were available.⍏


m2_freshDF <- c_ResFishData 
median(a_classificationed$surface_area_ha_HAS_NA,na.rm=TRUE) # median = 4.856232
# Already created a column to group res into LARGE or SMALL (Column: "gam2_BY_LargeSmall")

# FILTER 
x<-m2_freshDF %>% group_by(gam2_BY_LargeSmall) %>% tally() %>% print(n=Inf) %>% ungroup()#18166 NA sized reservoirs

m2_gam_ready<-m2_freshDF %>% 
  dplyr::filter(!is.na(Biomass_kgha)) # Filter out the NA sized reservoirs for now
  


# GAM 2 - model -----------------------------------------------------------
### GAM based on needs of CALC2 (by= Median_Grouping_Large-vs-Small)

class(m2_gam_ready$Biomass_kgha)
class(m2_gam_ready$ResAge)
class(m2_gam_ready$gam2_BY_LargeSmall)
class(m2_gam_ready$Ecosystem)
class(m2_gam_ready$Year)

m2 <- mgcv::gam(Biomass_kgha ~ s(ResAge, bs="tp", by=gam2_BY_LargeSmall) + 
                  s(Ecosystem, bs="re")+
                  s(Year, bs="tp"),
                family=Gamma(link=log), data=m2_gam_ready, method="REML")  

summary.gam(m2)

gam.check(m2)

plot.gam(m2, page=1, 
         all.terms=TRUE, 
         rug=TRUE,residuals=TRUE,pch = 1, cex = 1,
         shade=TRUE,shade.col = "lightblue",
         shift = coef(m2)[1])


# GAM 2 - predict ---------------------------------------------------------

#Create NewDF for Predict()
m2_MinYearRange<-m2_gam_ready %>% 
  group_by(Ecosystem) %>%                # Prediction by ECOSYSTEM (each ecosystem needs a prediction even though m2's smooth is by=NOTHING)
  summarize(minAge = min(ResAge) ) %>%   # Create column with the minimum age each reservoir had been when sampled 
  tidyr::expand_grid(AgePred = ((min(m2_gam_ready$ResAge)):130))%>% # Add Age-To-Predict-On col by min(ResAge):105
  dplyr::filter(minAge <= AgePred ) %>%  # Summarize causes dup rows; filter so that each res gets a unique AgePrediction
  ungroup()

m2_MinYearRange %>%   #Check new DF
  arrange(Ecosystem,AgePred) %>%
  print(n= Inf )

# Want the new DF to also have attributes related to select(xxx):
m2_TheRestOfPredDF<-m2_gam_ready %>%
  ungroup() %>% 
  dplyr::select("Ecosystem","YearDamCompleted", "gam2_BY_LargeSmall") %>% 
  distinct() 

# Join the DF with Age Predictions to the Attributes we want still associated with them...
m2_Join_forPredDF<-left_join(m2_TheRestOfPredDF,m2_MinYearRange)

m2_Predictable_FishData<-m2_Join_forPredDF %>% 
  rename(ResAge = AgePred) %>%               # Predict()'s NewDF doesn't have actual-age, so it's ok to rename to the name used in GAM. 
  arrange(ResAge) %>%                        # Arrange Res within a ClassificationType by Age; a helpful visual 
  mutate(Year = (YearDamCompleted + ResAge)) # Predict's NewDF also doesn't have Year_Sampled (found in GAM model too)

m2_predicted<-predict(m2, m2_Predictable_FishData, type="response", se.fit = TRUE)

m2_predbind<-do.call(cbind,m2_predicted) # Iterated for each ecoregion & pulled into one df 
m2_predtable<-data.table(m2_predbind)
m2_pred_final<-cbind(m2_Predictable_FishData,m2_predtable)





# GAM 2 - plot - fit~resage -----------------------------------------------
### PredictBiomass~ResAge Plots ... with raw data overlayed 
# Link true biomass values to the rows they belong to. for all else in this column, NA 
# doing this will let us see raw data overlayed on predictions (plot1) 
#             & compare/validate true vs predicted biomass 1:1 (plot2)

m2_small<-m2_gam_ready %>% 
  ungroup() %>% 
  dplyr::select("Ecosystem","ResAge", "Biomass_kgha") #2 joining columns + the true empirical biomass column

m2_Actual_and_Pred_data<-left_join(m2_pred_final,m2_small, by = c("Ecosystem","ResAge")) #bind predictions to true biomass DF

m2_Actual_and_Pred_data %>% 
  dplyr::filter(!is.na(gam2_BY_LargeSmall)) %>% 
  dplyr::filter(Year<=1993) %>% 
  ggplot()+
  geom_point(aes(x=Year, y=fit),            color="gray50",         size=2, alpha=0.3, shape=16) + 
  geom_point(aes(x=Year, y=Biomass_kgha),   color="cornflowerblue", size=3, alpha=0.4, shape=16) + 
  scale_x_continuous(breaks = seq(1948, 1993, 10), limits=c(1948, 1993))+
  scale_y_continuous(breaks = seq(0, 3100, 500), limits=c(0, 3100))+
  facet_wrap(.~gam2_BY_LargeSmall, ncol=4,labeller=label_wrap_gen(width=31,multi_line = TRUE))+
  #scale_color_manual(values = colorRampPalette(glasbey(27))(colourCount)) +
  theme_bw()+
  theme(axis.title.x=element_text(size=12.8), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=11.5),
        axis.text.y =element_text(size=11.5),
        legend.position = "none",
        strip.text = element_text(size=11),
        strip.background = element_rect(fill=NA),
        #strip.text = element_text(face="bold", size=9,lineheight=5.0
        #strip.background = element_rect(fill="lightblue", colour="black", size=1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  labs( x = "Year", y = expression(paste("Fish Biomass"~('kg'~ha^-1))))


# GAM 2 - plot - validation -----------------------------------------------
# VALIDATE 
m2_truebio_and_predbio <-m2_Actual_and_Pred_data %>% 
  dplyr::filter(!is.na(Biomass_kgha))
basicStats(m2_truebio_and_predbio$Biomass_kgha)

m2_truebio_and_predbio %>% 
  dplyr::filter(!is.na(gam2_BY_LargeSmall)) %>%
  ggplot(aes(x=fit, y=Biomass_kgha)) +
  geom_point(size=3.0, alpha=0.5, color="cornflowerblue")+
  geom_abline(aes(intercept=0.0,slope=1.0), size=1.0, alpha=0.8, linetype="solid")+
  scale_x_continuous(breaks = seq(0, 3100, 500),limits=c(0, 3200), expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0, 3100, 500),limits=c(0, 3200), expand = c(0, 0))+
  facet_wrap(.~gam2_BY_LargeSmall, ncol=4,labeller=label_wrap_gen(width=31,multi_line = TRUE))+
  #scale_color_manual(values = colorRampPalette(glasbey(27))(colourCount)) +
  theme_bw()+
  theme(axis.title.x=element_text(size=12.8), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=11.5),
        axis.text.y =element_text(size=11.5),
        legend.position = "none",
        strip.text = element_text(size=11),
        strip.background = element_rect(fill=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  labs(x = expression(paste("Predicted Fish Biomass"~('kg'~ha^-1))), 
       y = expression(paste("Observed Fish Biomass"~('kg'~ha^-1))))


# GAM 2 - plot - histogram -------------------------------------------------------
# of then-vs-now biomass

m2_histogram_1993mean<-m2_Actual_and_Pred_data %>% 
  group_by(gam2_BY_LargeSmall) %>% 
  dplyr::filter(Year==1993) %>% 
  mutate(MaxYr_mean = mean(fit)) %>% 
  dplyr::select("Ecosystem","gam2_BY_LargeSmall","MaxYr_mean") %>% 
  ungroup()

m2_histogram_maxyr<-left_join(m2_Actual_and_Pred_data,m2_histogram_1993mean, by=c("Ecosystem","gam2_BY_LargeSmall"))

m2_histogram_justempirical<-m2_histogram_maxyr %>% 
  dplyr::filter(!is.na(Biomass_kgha))

m2_histogram_minmax_yr<-m2_histogram_justempirical %>% 
  group_by(Ecosystem) %>% 
  dplyr::filter(Year==min(Year)) %>% 
  mutate(MinYr = Biomass_kgha) %>% 
  ungroup()

m2_histogram_difference<-m2_histogram_minmax_yr %>% 
  group_by(gam2_BY_LargeSmall) %>% 
  mutate(difference_biomass =  MaxYr_mean - MinYr) %>% 
  arrange(Ecosystem,fit) %>% 
  ungroup()

fBasics::basicStats(m2_histogram_difference$difference_biomass)

m2_histogram_difference %>% 
  ggplot(aes(x=difference_biomass))+
  geom_histogram(aes(y=..count../sum(..count..)),bins=30,fill="cornflowerblue",color="slategray1") +
  scale_x_continuous(breaks = seq(-2500, 1000, 500), limits=c(-2500,1000))+
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1))+
  geom_abline(aes(intercept=0,slope=1), color="gray60",size=1, alpha=0.9, linetype="dashed")+
  theme_bw()+
  theme(axis.title.x=element_text(size=12.8), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=11.5),
        axis.text.y =element_text(size=11.5),
        legend.position = "none",
        strip.text = element_text(size=11),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
  #annotate(geom="text", x=-2120, y=.20, color="black", size=5, label="Mean = 104.17 ")+
  #annotate(geom="text", x=-1850, y=.19, color="black", size=5, label="Range = -2326.39 - 859.44")+
  #annotate(geom="text", x=-2090, y=.18, color="black", size=5, label="SE Mean = 23.72")+
  #annotate(geom="text", x=-2216, y=.17, color="black", size=5, label="SD = 388.41")+
  #labs( x = expression(paste("Raw Difference in Biomass"~('kg'~ha^-1))))



# GAM 2 - USA calculation -------------------------------------------------------

colnames(m2_Actual_and_Pred_data)

# Select only the columns needed
m2_1993_predicted_biomass<-m2_Actual_and_Pred_data %>% 
  dplyr::filter(Year==1993) %>% 
  dplyr::select("Ecosystem","minAge","ResAge", "Year", "fit", "se.fit") 

m2_a_1993pred_join<-left_join(c_join_NIDbiomass,m2_1993_predicted_biomass, by="Ecosystem")
m2_unique_res <- m2_a_1993pred_join[!duplicated(m2_a_1993pred_join$NIDID),] #make 1 row per res again.
m2_a_1993pred_and_therest<-m2_unique_res 

# Calculation 2 - Median (Large and Small)

sum(is.na(m2_a_1993pred_and_therest$surface_area_ha))
median(m2_a_1993pred_and_therest$surface_area_ha_HAS_NA,na.rm=TRUE) # median = 4.856232

# Create smallres.df and bigres.df 
unique(m2_a_1993pred_and_therest$gam2_BY_LargeSmall) #Explore
x<-m2_a_1993pred_and_therest %>% 
  dplyr::select(NIDID,Ecosystem,surface_area_ha_HAS_NA,surface_area_ha,gam2_BY_LargeSmall) #Explore

m2_d_smallres<- m2_a_1993pred_and_therest %>% #CREATE SMALL RES DF
  #dplyr::filter(surface_area_ha_HAS_NA <= 4.856232) %>% 
  dplyr::filter(gam2_BY_LargeSmall=="Small")
range(m2_d_smallres$surface_area_ha_HAS_NA) # 0.000404686-4.856232

m2_d_bigres<- m2_a_1993pred_and_therest %>% #CREATE BIG RES DF
  #dplyr::filter(surface_area_ha_HAS_NA > 4.856232) %>% 
  dplyr::filter(gam2_BY_LargeSmall=="Large")
range(m2_d_bigres$surface_area_ha_HAS_NA) #4.860279-8196914.9

m2_d_temp_bind_to_count<-rbind(m2_d_smallres,m2_d_bigres)
m2_d_NAsizes<-m2_a_1993pred_and_therest[(!m2_a_1993pred_and_therest$RECORDID %in% m2_d_temp_bind_to_count$RECORDID),] #has NA res 

# tie these three back (d_small res),(m2_d_bigres),(m2_d_NAsizes). Ignore m2_d_temp_bind.

m2_e_SMALL_methmed2<-m2_d_smallres %>% 
  mutate(gam_meth2_median = case_when(is.na(fit) ~ mean(fit, na.rm=TRUE),
                                  TRUE ~ as.numeric(fit))) 
m2_e_BIG_methmed2<-m2_d_bigres %>% 
  mutate(gam_meth2_median = case_when(is.na(fit) ~ mean(fit, na.rm=TRUE),
                                  TRUE ~ as.numeric(fit)))
m2_e_NA_methmed2<-m2_d_NAsizes %>% 
  mutate(gam_meth2_median = case_when(is.na(fit) ~ mean(m2_a_1993pred_and_therest$fit, na.rm=TRUE),
                                  TRUE ~ as.numeric(fit)))

sum(!is.na(m2_a_1993pred_and_therest$fit))
mean(m2_e_SMALL_methmed2$fit,na.rm=TRUE)
mean(m2_e_BIG_methmed2$fit,na.rm=TRUE)
mean(m2_a_1993pred_and_therest$fit,na.rm=TRUE)

m2_f_median<-rbind(m2_e_SMALL_methmed2,m2_e_BIG_methmed2,m2_e_NA_methmed2)

sum(m2_f_median$gam_meth2_median) 

m2_g_medcalc2 <- m2_f_median %>% 
  mutate(gam_calc2_medianXarea = ((m2_f_median$gam_meth2_median)*(m2_f_median$surface_area_ha))) 

sum(is.na(m2_g_medcalc2$gam_calc2_medianXarea)) 

sum(m2_g_medcalc2$gam_calc2_medianXarea, na.rm=TRUE) # Schema2 USA Total Standing Stock (KG)

write_csv(m2_g_medcalc2,"data output/03_USA_GAM2_fit.csv")


# South GAM 2 - set up --------------------------------------------------------------------
# FOR CALC 2: Large & Small. Reservoirs were grouped into two classes, large and small. Median reservoir surface area for southern impoundments was 4.047 ha, and 4.856 ha for all USA impoundments; thus reservoirs below these thresholds were considered “small” while ones above were “large”. A mean fish mass was calculated for both large and small reservoirs and these values used for reservoirs where no biomass values were available.⍏


# Keep only SE states
m2S_NA_biomass_states<-c_ResFishData %>% 
  group_by(STATE) %>% 
  dplyr::filter(all(is.na(Biomass_kgha)) | is.na(STATE)) %>%
  ungroup()

# Keep states that are in the original ROYL dataset. 
m2S_SE_biomass<-c_ResFishData[(!c_ResFishData$RECORDID %in% m2S_NA_biomass_states$RECORDID),]

x<-m2S_NA_biomass_states %>% group_by(STATE) %>% tally() %>% print(n=Inf) %>% ungroup() #check
x<-m2S_SE_biomass %>% group_by(STATE) %>% tally() %>% print(n=Inf) %>% ungroup()        #check

# South median:
m2S_uniques_to_calc_median_from <- m2S_SE_biomass[!duplicated(m2S_SE_biomass$NIDID),] # make 1row/res for median() function. 
median(m2S_uniques_to_calc_median_from$surface_area_ha_HAS_NA,na.rm=TRUE)             # SOUTH median = 4.04686

colnames(m2S_SE_biomass)

# Create median-column for to use in south-gam & south-calc
m2S_south_medians_assigned<-m2S_SE_biomass %>% 
  mutate(SOUTHgam2_BY_LargeSmall = case_when(surface_area_ha_HAS_NA <= 4.04686 ~ "Small", surface_area_ha_HAS_NA > 4.04686 ~ "Large"))
unique(m2S_south_medians_assigned$SOUTHgam2_BY_LargeSmall)

# Convert column used in model to "factor"
class(m2S_south_medians_assigned$SOUTHgam2_BY_LargeSmall) #for gam2
m2S_south_medians_assigned$SOUTHgam2_BY_LargeSmall<- as.factor(m2S_south_medians_assigned$SOUTHgam2_BY_LargeSmall)
is.factor(m2S_south_medians_assigned$SOUTHgam2_BY_LargeSmall) 

colnames(m2S_south_medians_assigned)

#ResAge column already exists (YearSampled minus YearDamCompleted = Age at time of sampling)

####

#FILTER 
x<-m2S_south_medians_assigned %>% group_by(SOUTHgam2_BY_LargeSmall) %>% tally() %>% print(n=Inf) %>% ungroup() #Explore

m2S_GamReady<-m2S_south_medians_assigned %>% 
  dplyr::filter(!is.na(Biomass_kgha)) # Use only empirical data for GAM



# South GAM 2 - model -----------------------------------------------------------
### GAM based on needs of CALC2 (by= Median_Grouping_Large-vs-Small)

class(m2S_GamReady$Biomass_kgha)
class(m2S_GamReady$ResAge)
class(m2S_GamReady$SOUTHgam2_BY_LargeSmall)
class(m2S_GamReady$Ecosystem)
class(m2S_GamReady$Year)


m2S <- mgcv::gam(Biomass_kgha ~ s(ResAge, bs="tp", by=SOUTHgam2_BY_LargeSmall) + 
                  s(Ecosystem, bs="re")+
                  s(Year, bs="tp"),
                family=Gamma(link=log), data=m2S_GamReady, method="REML")  

summary.gam(m2S)

gam.check(m2S)

plot.gam(m2S, page=1, 
         all.terms=TRUE, 
         rug=TRUE,residuals=TRUE,pch = 1, cex = 1,
         shade=TRUE,shade.col = "lightblue",
         shift = coef(m2S)[1])


# South GAM 2 - predict ---------------------------------------------------------

#Create new DF for Predict()
m2S_MinYearRange<-m2S_GamReady %>% 
  group_by(Ecosystem) %>%                # Prediction by ECOSYSTEM (each ecosystem needs a prediction even though m2S's smooth is by=NOTHING)
  summarize(minAge = min(ResAge) ) %>%   # Create column with the minimum age each reservoir had been when sampled 
  tidyr::expand_grid(AgePred = ((min(m2S_GamReady$ResAge)):130))%>% # Add Age-To-Predict-On col by min(ResAge):105
  dplyr::filter(minAge <= AgePred ) %>%  # Summarize causes dup rows; filter so that each res gets a unique AgePrediction
  ungroup()

m2S_MinYearRange %>%   #check new DF
  arrange(Ecosystem,AgePred) %>%  
  print(n= Inf )

# Want the new DF to also have attributes related to select(xxx):
m2S_TheRestOfPredDF<-m2S_GamReady %>%
  ungroup() %>% 
  dplyr::select("Ecosystem","YearDamCompleted", "SOUTHgam2_BY_LargeSmall") %>% 
  distinct() 

# Join the DF with Age Predictions to the Attributes we want still associated with them...
m2S_Join_forPredDF<-left_join(m2S_TheRestOfPredDF,m2S_MinYearRange)

m2S_Predictable_FishData<-m2S_Join_forPredDF %>% 
  rename(ResAge = AgePred) %>%               # Predict()'s NewDF doesn't have actual-age, so it's ok to rename to the name used in GAM. 
  arrange(ResAge) %>%                        # Arrange Res within a ClassificationType by Age; a helpful visual 
  mutate(Year = (YearDamCompleted + ResAge)) # Predict's NewDF also doesn't have Year_Sampled (found in GAM model too)

m2S_predicted<-predict(m2S, m2S_Predictable_FishData, type="response", se.fit = TRUE)

m2S_predbind<-do.call(cbind,m2S_predicted) # Iterated for each ecoregion & pulled into one df 
m2S_predtable<-data.table(m2S_predbind)
m2S_pred_final<-cbind(m2S_Predictable_FishData,m2S_predtable)


# South GAM 2 - plot - fit~resage -----------------------------------------------
### PredictBiomass~ResAge Plots ... with raw data overlayed 
# Link true biomass values to the rows they belong to. for all else in this column, NA 
# doing this will let us see raw data overlayed on predictions (plot1) 
#             & compare/validate true vs predicted biomass 1:1 (plot2)

m2S_small<-m2S_GamReady %>% 
  ungroup() %>% 
  dplyr::select("Ecosystem","ResAge", "Biomass_kgha") #2 joining columns + the true empirical biomass column

m2S_Actual_and_Pred_data<-left_join(m2S_pred_final,m2S_small, by = c("Ecosystem","ResAge")) #bind predictions to true biomass DF

m2S_Actual_and_Pred_data %>% 
  dplyr::filter(!is.na(SOUTHgam2_BY_LargeSmall)) %>% 
  dplyr::filter(Year<=1993) %>% 
  ggplot()+
  geom_point(aes(x=Year, y=fit),            color="gray50",         size=2, alpha=0.3, shape=16) + 
  geom_point(aes(x=Year, y=Biomass_kgha),   color="cornflowerblue", size=3, alpha=0.4, shape=16) + 
  scale_x_continuous(breaks = seq(1948, 1993, 10), limits=c(1948, 1993))+
  scale_y_continuous(breaks = seq(0, 3100, 500), limits=c(0, 3100))+
  facet_wrap(.~SOUTHgam2_BY_LargeSmall, ncol=4,labeller=label_wrap_gen(width=31,multi_line = TRUE))+
  #scale_color_manual(values = colorRampPalette(glasbey(27))(colourCount)) +
  theme_bw()+
  theme(axis.title.x=element_text(size=12.8), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=11.5),
        axis.text.y =element_text(size=11.5),
        legend.position = "none",
        strip.text = element_text(size=11),
        strip.background = element_rect(fill=NA),
        #strip.text = element_text(face="bold", size=9,lineheight=5.0
        #strip.background = element_rect(fill="lightblue", colour="black", size=1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  labs( x = "Year", y = expression(paste("Fish Biomass"~('kg'~ha^-1))))


# South GAM 2 - plot - validation -----------------------------------------------
# VALIDATE 
m2S_truebio_and_predbio <-m2S_Actual_and_Pred_data %>% 
  dplyr::filter(!is.na(Biomass_kgha))
basicStats(m2S_truebio_and_predbio$Biomass_kgha)

m2S_truebio_and_predbio %>% 
  dplyr::filter(!is.na(SOUTHgam2_BY_LargeSmall)) %>% 
  ggplot(aes(x=fit, y=Biomass_kgha)) +
  geom_point(size=3.0, alpha=0.5, color="cornflowerblue")+
  geom_abline(aes(intercept=0.0,slope=1.0), size=1.0, alpha=0.8, linetype="solid")+
  scale_x_continuous(breaks = seq(0, 3100, 500),limits=c(0, 3200), expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0, 3100, 500),limits=c(0, 3200), expand = c(0, 0))+
  facet_wrap(.~SOUTHgam2_BY_LargeSmall, ncol=4,labeller=label_wrap_gen(width=31,multi_line = TRUE))+
  #scale_color_manual(values = colorRampPalette(glasbey(27))(colourCount)) +
  theme_bw()+
  theme(axis.title.x=element_text(size=12.8), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=11.5),
        axis.text.y =element_text(size=11.5),
        legend.position = "none",
        strip.text = element_text(size=11),
        strip.background = element_rect(fill=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  labs(x = expression(paste("Predicted Fish Biomass"~('kg'~ha^-1))), 
       y = expression(paste("Observed Fish Biomass"~('kg'~ha^-1))))

# GAM 2 - South calculation -------------------------------------------------------

colnames(m2S_Actual_and_Pred_data)

# Select only columns that are needed
m2s_1993_predicted_biomass<-m2S_Actual_and_Pred_data %>% 
  dplyr::filter(Year==1993) %>% 
  dplyr::select("Ecosystem","minAge","ResAge", "Year", "fit", "se.fit") 

m2S_1993_pred_join<-left_join(m2S_south_medians_assigned,m2s_1993_predicted_biomass, by="Ecosystem") 
m2S_1993_uniqueres <- m2S_1993_pred_join[!duplicated(m2S_1993_pred_join$NIDID),] #make 1 row per res again.

x<-m2S_1993_uniqueres %>% group_by(STATE) %>% tally() %>% print(n=Inf) %>% ungroup()  #Explore: 22 states

# Calculation 2 - Median (Large and Small)

median(m2S_1993_uniqueres$surface_area_ha_HAS_NA,na.rm=TRUE) # SOUTH median = 4.04686 # it's already 1row/res for median() function.
mean(m2S_1993_uniqueres$Biomass_kgha,na.rm=TRUE) 

# create smallres.df and bigres.df 
m2s_smallres<- m2S_1993_uniqueres %>% #CREATE SMALL RES DF
  #dplyr::filter(surface_area_ha_HAS_NA <= 4.04686)
  dplyr::filter(SOUTHgam2_BY_LargeSmall=="Small")
range(m2s_smallres$surface_area_ha_HAS_NA) 

m2s_bigres<- m2S_1993_uniqueres %>% #CREATE BIG RES DF
  #dplyr::filter(surface_area_ha_HAS_NA > 4.04686) 
  dplyr::filter(SOUTHgam2_BY_LargeSmall=="Large")
range(m2s_bigres$surface_area_ha_HAS_NA) 

m2s_BindTempToCount<-rbind(m2s_smallres,m2s_bigres)
m2s_NAsizes<-m2S_1993_uniqueres[(!m2S_1993_uniqueres$RECORDID %in% m2s_BindTempToCount$RECORDID),] #has NA res 

# tie these three back (d_small res),(m2s_bigres),(m2s_NAsizes). Ignore d_tempbind. 

m2s_SMALL_methmed2<-m2s_smallres %>% 
  mutate(SouthCalc_SouthGam_meth2_median = case_when(is.na(fit) ~ mean(fit, na.rm=TRUE),
                                      TRUE ~ as.numeric(fit))) 
m2s_BIG_methmed2<-m2s_bigres %>% 
  mutate(SouthCalc_SouthGam_meth2_median = case_when(is.na(fit) ~ mean(fit, na.rm=TRUE),
                                      TRUE ~ as.numeric(fit)))
m2s_NA_methmed2<-m2s_NAsizes %>% 
  mutate(SouthCalc_SouthGam_meth2_median = case_when(is.na(fit) ~ mean(m2S_1993_uniqueres$fit, na.rm=TRUE),
                                      TRUE ~ as.numeric(fit)))

mean(m2s_SMALL_methmed2$fit,na.rm=TRUE)
mean(m2s_BIG_methmed2$fit,na.rm=TRUE)
mean(m2S_1993_uniqueres$fit,na.rm=TRUE)

# Bind back together
m2s_medians<-rbind(m2s_SMALL_methmed2,m2s_BIG_methmed2,m2s_NA_methmed2)

m2s_medscalc2 <- m2s_medians %>% 
  mutate(SouthCalc_SouthGam_calc2_medianXarea = ((SouthCalc_SouthGam_meth2_median)*(surface_area_ha))) 

sum(is.na(m2s_medscalc2$SouthCalc_SouthGam_calc2_medianXarea)) 
x<-m2s_medscalc2 %>% group_by(STATE) %>% tally() %>% print(n=Inf) %>% ungroup()

sum(m2s_medscalc2$SouthCalc_SouthGam_calc2_medianXarea, na.rm=TRUE) #Schema2 South - Total Standing Stock (KG)

# ~~~~ --------------------------------------------------------------------






# GAM 3 - set up --------------------------------------------------------------------
# FOR CALC 3 - "Size-Flow" - Across all USA reservoirs, a cluster analysis constrained to four clusters was performed using volume (i.e., storage capacity) and maximum discharge. Mean fish mass was calculated across all reservoirs for a given cluster. Cluster mean fish mass values were used to assign a fish mass value to any reservoir missing empirical data.⍏

m3_freshDF <- c_ResFishData #Start fresh. 
colnames(c_ResFishData)

# Grouping column "gam3_BY_ClusterVol" already exists. 

#FILTER 
x<-m3_freshDF %>% group_by(gam3_BY_ClusterVol) %>% tally() %>% print(n=Inf) %>% ungroup()

m3_gam_ready<-m3_freshDF %>% 
  dplyr::filter( !is.na(gam3_BY_ClusterVol) #filter out NA clusters
                &!is.na(Ecosystem)) %>%     #filter out res not used in GAM (non-empirical)
  group_by(gam3_BY_ClusterVol) %>%
  #dplyr::filter(n()>5) %>%  # filter out any CLASS with <x data points
  ungroup()


# GAM 3 - model -----------------------------------------------------------
### GAM based on needs of CALC2 (by= Median_Grouping_Large-vs-Small)

class(m3_gam_ready$Biomass_kgha)
class(m3_gam_ready$ResAge)
class(m3_gam_ready$gam3_BY_ClusterVol)
class(m3_gam_ready$Ecosystem)
class(m3_gam_ready$Year)


m3 <- mgcv::gam(Biomass_kgha ~ s(ResAge, bs="tp", by=gam3_BY_ClusterVol) + 
                  s(Ecosystem, bs="re")+
                  s(Year, bs="tp"),
                family=Gamma(link=log), data=m3_gam_ready, method="REML")  

summary.gam(m3)

gam.check(m3) 

plot.gam(m3, page=1, 
         all.terms=TRUE, 
         rug=TRUE,residuals=TRUE,pch = 1, cex = 1,
         shade=TRUE,shade.col = "lightblue",
         shift = coef(m3)[1])


# GAM 3 - predict ---------------------------------------------------------

#Create NewDF for Predict()
m3_MinYearRange<-m3_gam_ready %>% 
  group_by(Ecosystem) %>%                # Prediction by ECOSYSTEM (each ecosystem needs a prediction even though m3's smooth is by=NOTHING)
  summarize(minAge = min(ResAge) ) %>%   # Create column with the minimum age each reservoir had been when sampled 
  tidyr::expand_grid(AgePred = ((min(m3_gam_ready$ResAge)):130))%>% #add Age-To-Predict-On col by min(ResAge):105
  dplyr::filter(minAge <= AgePred ) %>%  # Summarize causes dup rows; filter so that each res gets a unique AgePrediction
  ungroup()

# OTHER WAY TO DO IT 
#m3_resage_predictable_FishData<-m3_gam_ready %>% 
#  dplyr::select("ClassTypeL2", "ClassAbrv","Ecosystem.y","YearDamCompleted.y") %>% # just grab the name and grouping columns
#  distinct() %>% # filter out the unique rows
#  expand_grid(ResAge = 0:105) # expand so that each unique row occurs combined with each age

m3_MinYearRange %>%   #check new DF
  arrange(Ecosystem,AgePred) %>%  
  print(n= Inf )

# Want the new DF to also have attributes related to select(xxx):
m3_TheRestOfPredDF<-m3_gam_ready %>%
  ungroup() %>% 
  dplyr::select("Ecosystem","YearDamCompleted", "gam3_BY_ClusterVol") %>% 
  distinct() 

# Join the DF with Age Predictions to the Attributes we want still associated with them...
m3_Join_forPredDF<-left_join(m3_TheRestOfPredDF,m3_MinYearRange)

m3_Predictable_FishData<-m3_Join_forPredDF %>% 
  rename(ResAge = AgePred) %>%               # Predict()'s NewDF doesn't have actual-age, so it's ok to rename to the name used in GAM. 
  arrange(ResAge) %>%                        # Arrange Res within a ClassificationType by Age; a helpful visual 
  mutate(Year = (YearDamCompleted + ResAge)) # Predict's NewDF also doesn't have Year_Sampled (found in GAM model too)

m3_predicted<-predict(m3, m3_Predictable_FishData, type="response", se.fit = TRUE)

m3_predbind<-do.call(cbind,m3_predicted) # iterated for each ecoregion & pulled into one df 
m3_predtable<-data.table(m3_predbind)
m3_pred_final<-cbind(m3_Predictable_FishData,m3_predtable)





# GAM 3 - plot - fit~resage -----------------------------------------------
### PredictBiomass~ResAge Plots ... with raw data overlayed 
# Link true biomass values to the rows they belong to. for all else in this column, NA 
# doing this will let us see raw data overlayed on predictions (plot1) 
#             & compare/validate true vs predicted biomass 1:1 (plot2)

m3_small<-m3_gam_ready %>% 
  ungroup() %>% 
  dplyr::select("Ecosystem","ResAge", "Biomass_kgha") #2 joining columns + the true empirical biomass column

m3_Actual_and_Pred_data<-left_join(m3_pred_final,m3_small, by = c("Ecosystem","ResAge")) #bind predictions to true biomass DF


#reorder facets in plot:
m3_Actual_and_Pred_data$gam3_BY_ClusterVol_ordered = factor(m3_Actual_and_Pred_data$gam3_BY_ClusterVol, 
                                                            levels = c("Large Volume - Low Discharge",
                                                                       "Large Volume - High Discharge",
                                                                       "Small Volume - Low Discharge", 
                                                                       "Small Volume - High Discharge"))

m3_Actual_and_Pred_data %>% 
  dplyr::filter(Year<=1993) %>% 
  ggplot()+
  geom_point(aes(x=Year, y=fit),            color="gray50",         size=2, alpha=0.4, shape=16) + 
  geom_point(aes(x=Year, y=Biomass_kgha),   color="cornflowerblue", size=3, alpha=0.4, shape=16) + 
  scale_x_continuous(breaks = seq(1948, 1993, 10), limits=c(1948, 1993))+
  scale_y_continuous(breaks = seq(0, 3100, 500), limits=c(0, 3100))+
  facet_wrap(.~gam3_BY_ClusterVol_ordered, ncol=2,labeller=label_wrap_gen(width=31,multi_line = TRUE))+
  theme_bw()+
  theme(axis.title.x=element_text(size=12.8), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=11.5),
        axis.text.y =element_text(size=11.5),
        legend.position = "none",
        strip.text = element_text(size=11),
        strip.background = element_rect(fill=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  labs( x = "Year", 
        y = expression(paste("Fish Biomass"~('kg'~ha^-1))))


# GAM 3 - plot - validation -----------------------------------------------
# VALIDATE 
m3_truebio_and_predbio <-m3_Actual_and_Pred_data %>% 
  dplyr::filter(!is.na(Biomass_kgha))
basicStats(m3_truebio_and_predbio$Biomass_kgha)

m3_truebio_and_predbio %>% 
  ggplot(aes(x=fit, y=Biomass_kgha)) +
  geom_point(size=3.0, alpha=0.5, color="cornflowerblue")+
  geom_abline(aes(intercept=0.0,slope=1.0), size=1.0, alpha=0.8, linetype="solid")+
  scale_x_continuous(breaks = seq(0, 3300, 500),limits=c(0, 3300), expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0, 3300, 500),limits=c(0, 3300), expand = c(0, 0))+
  facet_wrap(.~gam3_BY_ClusterVol_ordered, ncol=2,labeller=label_wrap_gen(width=31,multi_line = TRUE))+
  #scale_color_manual(values = colorRampPalette(glasbey(27))(colourCount)) +
  theme_bw()+
  theme(axis.title.x=element_text(size=12.8), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=11.5),
        axis.text.y =element_text(size=11.5),
        legend.position = "none",
        strip.text = element_text(size=11),
        strip.background = element_rect(fill=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  labs(x = expression(paste("Predicted Fish Biomass"~('kg'~ha^-1))), 
       y = expression(paste("Observed Fish Biomass"~('kg'~ha^-1))))


# GAM 3 - plot - histogram -------------------------------------------------------
# of then-vs-now biomass

m3_histogram_1993mean<-m3_Actual_and_Pred_data %>% 
  group_by(gam3_BY_ClusterVol_ordered) %>% 
  dplyr::filter(Year==1993) %>% 
  mutate(MaxYr_mean = mean(fit)) %>% 
  dplyr::select("Ecosystem","gam3_BY_ClusterVol_ordered","MaxYr_mean") %>% 
  ungroup()

m3_histogram_maxyr<-left_join(m3_Actual_and_Pred_data,m3_histogram_1993mean, by=c("Ecosystem","gam3_BY_ClusterVol_ordered"))

m3_histogram_justempirical<-m3_histogram_maxyr %>% 
  dplyr::filter(!is.na(Biomass_kgha))

m3_histogram_minmax_yr<-m3_histogram_justempirical %>% 
  group_by(Ecosystem) %>% 
  dplyr::filter(Year==min(Year)) %>% 
  mutate(MinYr = Biomass_kgha) %>% 
  ungroup()

m3_histogram_difference<-m3_histogram_minmax_yr %>% 
  group_by(gam3_BY_ClusterVol_ordered) %>% 
  mutate(difference_biomass =  MaxYr_mean - MinYr) %>% 
  arrange(Ecosystem,fit) %>% 
  ungroup()

fBasics::basicStats(m3_histogram_difference$difference_biomass)

m3_histogram_difference %>% 
  ggplot(aes(x=difference_biomass))+
  geom_histogram(aes(y=..count../sum(..count..)),bins=30,fill="cornflowerblue",color="slategray1") +
  #scale_x_continuous(breaks = seq(-2500, 1000, 500), limits=c(-2500,1000))+
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1))+
  geom_abline(aes(intercept=0,slope=1), color="gray60",size=1, alpha=0.9, linetype="dashed")+
  #facet_wrap(~gam3_BY_ClusterVol_ordered, ncol=3,labeller=label_wrap_gen(width=31,multi_line = TRUE)) +
  theme_bw()+
  theme(axis.title.x=element_text(size=12.8), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=11.5),
        axis.text.y =element_text(size=11.5),
        legend.position = "none",
        strip.text = element_text(size=11),
        #strip.text = element_text(face="bold", size=9,lineheight=5.0
        #strip.background = element_rect(fill="lightblue", colour="black", size=1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
  #annotate(geom="text", x=-2120, y=.20, color="black", size=5, label="Mean = 104.17 ")+
  #annotate(geom="text", x=-1850, y=.19, color="black", size=5, label="Range = -2326.39 - 859.44")+
  #annotate(geom="text", x=-2090, y=.18, color="black", size=5, label="SE Mean = 23.72")+
  #annotate(geom="text", x=-2216, y=.17, color="black", size=5, label="SD = 388.41")+
  #labs( x = expression(paste("Raw Difference in Biomass"~('kg'~ha^-1))))





# GAM 3 - USA calculation -------------------------------------------------------

colnames(m3_Actual_and_Pred_data)

# Select only the columns you need for the year cut off  
m3_1993_predicted_biomass<-m3_Actual_and_Pred_data %>% 
  dplyr::filter(Year==1993) %>% 
  dplyr::select("Ecosystem","minAge","ResAge", "Year", "fit", "se.fit") 


m3_a_1993pred_join        <- left_join(c_join_NIDbiomass,m3_1993_predicted_biomass, by="Ecosystem") 
m3_unique_res             <- m3_a_1993pred_join[!duplicated(m3_a_1993pred_join$NIDID),] # 1 row per res again. 
m3_a_1993pred_and_therest <- m3_unique_res


# Calculation 3 - ClusterVolume 

# COLUMN OF CLUSTER WAS PREVIOUSLY CREATED (gam3_BY_ClusterVol) 

colnames(m3_a_1993pred_and_therest) 

m3_h_VOL_group <- m3_a_1993pred_and_therest %>% 
  dplyr::filter(!is.na(gam3_BY_ClusterVol)) #has no clustering info

m3_h_volNA_group <- m3_a_1993pred_and_therest %>% 
  dplyr::filter(is.na(gam3_BY_ClusterVol))  #has clustering info 
  
m3_j_meth3 <- m3_h_VOL_group %>% 
  group_by(gam3_BY_ClusterVol) %>% #group by cluster; mean by cluster
  mutate(gam_meth3_clustervol = case_when(is.na(fit) ~ mean(fit, na.rm=TRUE),
                                      TRUE ~ as.numeric(fit))) %>% 
  ungroup()

m3_j_NA_meth3 <- m3_h_volNA_group %>% 
  mutate(gam_meth3_clustervol = case_when(is.na(fit) ~ mean(m3_a_1993pred_and_therest$fit, na.rm=TRUE),
                                      TRUE ~ as.numeric(fit)))


class(m3_j_meth3)
class(m3_j_NA_meth3)

m3_j_meth3<-data.table(m3_j_meth3) #rbind insists the dfs be "data tables"
m3_j_NA_meth3<-data.table(m3_j_NA_meth3)

m3_k_meth3_bound <- rbind(m3_j_NA_meth3,m3_j_meth3) #rbind req "data.table" class format. if you get a large matrix with double the rows, that's the issue. 

m3_L_meth3calc <- m3_k_meth3_bound %>% 
  mutate(gam_calc3_methXarea = ((m3_k_meth3_bound$gam_meth3_clustervol)*(m3_k_meth3_bound$surface_area_ha))) 

sum(m3_L_meth3calc$gam_calc3_methXarea, na.rm=TRUE) #Schema3 USA - Total Standing Stock (KG)

write_csv(m3_L_meth3calc,"data output/03_USA_GAM3_fit.csv") 



# South GAM 3 - set up --------------------------------------------------------------------
# FOR CALC 3 - "Size-Flow" - Across all USA reservoirs, a cluster analysis constrained to four clusters was performed using volume (i.e., storage capacity) and maximum discharge. Mean fish mass was calculated across all reservoirs for a given cluster. Cluster mean fish mass values were used to assign a fish mass value to any reservoir missing empirical data.⍏


# Keep only SE states
m3S_NA_biomass_states<-c_ResFishData %>% 
  group_by(STATE) %>% 
  dplyr::filter(all(is.na(Biomass_kgha)) | is.na(STATE)) %>%
  ungroup()

# Keep states that are in the original ROYL dataset. 
m3S_SE_biomass<-c_ResFishData[(!c_ResFishData$RECORDID %in% m3S_NA_biomass_states$RECORDID),]

x<-m3S_NA_biomass_states %>% group_by(STATE) %>% tally() %>% print(n=Inf) %>% ungroup()    #View
x<-m3S_SE_biomass %>% group_by(STATE,Ecosystem) %>% tally() %>% print(n=Inf) %>% ungroup() #View

colnames(m3S_SE_biomass)

# create cluster-volume column for to use in south-gam & south-calc
# "Cluster Volume" - requires a column clustering based on size-discharge (volume)
###' 4 clusters based on size-discharge (volume)
###' take mean for each cluster factor (1,2,3,4)
###' ones w/o size-discharge --> get "grand avg for all res"

m3S_volNA_group<- m3S_SE_biomass %>% 
  dplyr::filter(is.na(maxdischarge_cms_logscale) 
                | !MAX_DISCHARGE>0 
                | !NID_STORAGE>0) #group that is not clusterable by volume. 

m3S_vols_group<- m3S_SE_biomass %>% 
  dplyr::filter(! (is.na(maxdischarge_cms_logscale))
                & (MAX_DISCHARGE>0)
                & (NID_STORAGE>0)) # group that is clusterable by volume

set.seed(2) # Set seed for reproducibility in kmeans analysis.

m3S_k4 <- m3S_vols_group %>% 
  dplyr::select(maxdischarge_cms_logscale,nidstorage_cm_logscale) %>%  #kmeans based on these columns
  kmeans(centers = 4,iter.max = 100) #4 clusters LIST

m3S_vols_k4addedcol<-m3S_vols_group %>% 
  mutate(South_meth_clustvolk4 = m3S_k4$cluster) # Make cluster-list as a column in the df

m3S_volNA_group$South_meth_clustvolk4<-NA # Doing this bc NAvol df needs same number of columns 

class(m3S_vols_k4addedcol)
class(m3S_volNA_group)

m3S_vols_k4addedcol<-data.table(m3S_vols_k4addedcol) #rbind insists the dfs be "data tables"
m3S_volNA_group<-data.table(m3S_volNA_group)

m3S_methclustvol <- rbind(m3S_volNA_group,m3S_vols_k4addedcol) #rbind req "data.table" class format. 

m3S_ClustVolGroups<- m3S_methclustvol %>% 
  mutate(SouthCalc_SouthGam3_BY_ClusterVol = case_when(
    South_meth_clustvolk4 == "1" ~ "Small Volume - Low Discharge",
    South_meth_clustvolk4 == "2" ~ "Large Volume - Low Discharge",
    South_meth_clustvolk4 == "4" ~ "Small Volume - High Discharge",
    South_meth_clustvolk4 == "3" ~ "Large Volume - High Discharge"))

m3S_ClustVolGroups %>% #View
  ggplot()+
  geom_point(aes(x=nidstorage_cm_logscale, 
                 y=maxdischarge_cms_logscale, 
                 color=SouthCalc_SouthGam3_BY_ClusterVol))

# Clustering done 

# Convert column used in model to "factor"
class(m3S_ClustVolGroups$SouthCalc_SouthGam3_BY_ClusterVol) #for gam3
m3S_ClustVolGroups$SouthCalc_SouthGam3_BY_ClusterVol<- as.factor(m3S_ClustVolGroups$SouthCalc_SouthGam3_BY_ClusterVol)
is.factor(m3S_ClustVolGroups$SouthCalc_SouthGam3_BY_ClusterVol) 

#ResAge column already exists (YearSampled minus YearDamCompleted = Age at time of sampling)

#FILTER 
x<-m3S_ClustVolGroups %>% group_by(SouthCalc_SouthGam3_BY_ClusterVol) %>% tally() %>% print(n=Inf) %>% ungroup() #Explore
x<-m3S_ClustVolGroups %>% group_by(STATE,Ecosystem,Year,Biomass_kgha) %>% tally() %>% print(n=Inf) %>% ungroup() #Explore
x<-m3S_ClustVolGroups %>% group_by(STATE) %>% tally() %>% print(n=Inf) %>% ungroup() #22 states w expanded data. Use this in calc-join.

m3S_GamReady<-m3S_ClustVolGroups %>% 
  dplyr::filter( !is.na(SouthCalc_SouthGam3_BY_ClusterVol) # Filter out NA clusters
                 &!is.na(Ecosystem)) %>%                   # Filter out res not used in GAM (non-empirical)
  group_by(SouthCalc_SouthGam3_BY_ClusterVol) %>%
  #dplyr::filter(n()>5) %>%  # filter out any CLASS with <x data points
  ungroup()


# South GAM 3 - model -----------------------------------------------------------
### GAM based on needs of CALC2 (by= Median_Grouping_Large-vs-Small)

class(m3S_GamReady$Biomass_kgha)
class(m3S_GamReady$ResAge)
class(m3S_GamReady$SouthCalc_SouthGam3_BY_ClusterVol)
class(m3S_GamReady$Ecosystem)
class(m3S_GamReady$Year)

m3S <- mgcv::gam(Biomass_kgha ~ s(ResAge, bs="tp", by=SouthCalc_SouthGam3_BY_ClusterVol) + 
                  s(Ecosystem, bs="re")+
                  s(Year, bs="tp"),
                family=Gamma(link=log), data=m3S_GamReady, method="REML")  

summary.gam(m3S)

gam.check(m3S) 

plot.gam(m3S, page=1, 
         all.terms=TRUE, 
         rug=TRUE,residuals=TRUE,pch = 1, cex = 1,
         shade=TRUE,shade.col = "lightblue",
         shift = coef(m3S)[1])


# South GAM 3 - predict ---------------------------------------------------------

#Create NewDF for Predict()
m3S_MinYearRange<-m3S_GamReady %>% 
  group_by(Ecosystem) %>%                # Prediction by ECOSYSTEM (each ecosystem needs a prediction even though m3's smooth is by=NOTHING)
  summarize(minAge = min(ResAge) ) %>%   # Create column with the minimum age each reservoir had been when sampled 
  tidyr::expand_grid(AgePred = ((min(m3S_GamReady$ResAge)):130))%>% # Add Age-To-Predict-On col by min(ResAge):105
  dplyr::filter(minAge <= AgePred ) %>%  # Summarize causes dup rows; filter so that each res gets a unique AgePrediction
  ungroup()

m3S_MinYearRange %>% # check new DF
  arrange(Ecosystem,AgePred) %>%  
  print(n= Inf )

# Want the NewDF to also have attributes related to select(xxx):
m3S_TheRestOfPredDF<-m3S_GamReady %>%
  ungroup() %>% 
  dplyr::select("Ecosystem","YearDamCompleted", "SouthCalc_SouthGam3_BY_ClusterVol") %>% 
  distinct() 

# Join the DF with Age Predictions to the Attributes we want still associated with them...
m3S_Join_forPredDF<-left_join(m3S_TheRestOfPredDF,m3S_MinYearRange)

m3S_Predictable_FishData<-m3S_Join_forPredDF %>% 
  rename(ResAge = AgePred) %>%               # Predict()'s NewDF doesn't have actual-age, so it's ok to rename to the name used in GAM. 
  arrange(ResAge) %>%                        # Arrange Res within a ClassificationType by Age; a helpful visual 
  mutate(Year = (YearDamCompleted + ResAge)) # Predict's NewDF also doesn't have Year_Sampled (found in GAM model too)

m3S_predicted<-predict(m3S, m3S_Predictable_FishData, type="response", se.fit = TRUE)

m3S_predbind<-do.call(cbind,m3S_predicted) # iterated for each ecoregion & pulled into one df 
m3S_predtable<-data.table(m3S_predbind)
m3S_pred_final<-cbind(m3S_Predictable_FishData,m3S_predtable)



m3S_small<-m3S_GamReady %>% 
  ungroup() %>% 
  dplyr::select("Ecosystem","ResAge", "Biomass_kgha") #2 joining columns + the true empirical biomass column

m3S_Actual_and_Pred_data<-left_join(m3S_pred_final,m3S_small, by = c("Ecosystem","ResAge")) #bind predictions to true biomass DF

#reorder facets in plot:
m3S_Actual_and_Pred_data$SouthCalc_SouthGam3_BY_ClusterVol_ordered = 
  factor(m3S_Actual_and_Pred_data$SouthCalc_SouthGam3_BY_ClusterVol, 
         levels = c("Large Volume - Low Discharge",
                    "Large Volume - High Discharge",
                    "Small Volume - Low Discharge", 
                    "Small Volume - High Discharge"))

# GAM 3 - South Calculation -------------------------------------------------------

colnames(m3S_Actual_and_Pred_data)

m3S_1993_predicted_biomass<-m3S_Actual_and_Pred_data %>% # take only columns that help, otherwise ".y" happens later
  dplyr::filter(Year==1993) %>% 
  dplyr::select("Ecosystem","minAge","ResAge", "Year", "fit", "se.fit") 

# m3S_ClustVolGroups instead of cJoinNID
# "c_join_NIDbiomass" - benefit was it had all res expanded (and multiyr info) / downside is it doesn't have cluster history
# "m3S_ClustVolGroups" - benefit is that it is just south states (good) with multiyr data (good) with cluster info (good!)
m3S_1993pred_join <- left_join(m3S_ClustVolGroups,m3S_1993_predicted_biomass, by="Ecosystem") #keep, may9,2022 
m3S_1993pred_uniques <-  m3S_1993pred_join[!duplicated(m3S_1993pred_join$NIDID),] # 1 row per res again. 85767 rows looks good


colnames(m3S_ClustVolGroups)
colnames(m3S_1993_predicted_biomass)
colnames(m3S_1993pred_join)
colnames(m3S_1993pred_uniques)

x<- m3S_1993pred_uniques %>% dplyr::select(SouthCalc_SouthGam3_BY_ClusterVol,Ecosystem)

x<-m3S_1993pred_uniques %>% 
  dplyr::filter(is.na(fit)) # i think it's ok we lost res bc they weren't in G3. May9,2022. 

# Calculation 3 - ClusterVolume 

sum(!is.na(m3S_1993pred_uniques$Biomass_kgha))#299
sum(!is.na(m3S_1993pred_uniques$fit))         #273

colnames(m3S_1993pred_uniques) 

x<-m3S_1993pred_uniques %>% #explore
  dplyr::select(NIDID, Ecosystem, South_meth_clustvolk4, SouthCalc_SouthGam3_BY_ClusterVol, Year.x, Year.y, Biomass_kgha, fit ) %>% 
  dplyr::filter(is.na(SouthCalc_SouthGam3_BY_ClusterVol)) %>% 
  ungroup()


m3S_VOLgroup <- m3S_1993pred_uniques %>% 
  dplyr::filter(!is.na(SouthCalc_SouthGam3_BY_ClusterVol)) #has no clustering info

m3S_volNAgroup <- m3S_1993pred_uniques %>% 
  dplyr::filter(is.na(SouthCalc_SouthGam3_BY_ClusterVol)) #has clustering info 


m3S_meth3 <- m3S_VOLgroup %>% 
  group_by(SouthCalc_SouthGam3_BY_ClusterVol) %>% #group by cluster; mean by cluster
  mutate(SouthCalc_SouthGam_meth3_clustervol = case_when(is.na(fit) ~ mean(fit, na.rm=TRUE),
                                          TRUE ~ as.numeric(fit)))%>% 
  ungroup()

m3S_NAmeth3 <- m3S_volNAgroup %>% 
  group_by(SouthCalc_SouthGam3_BY_ClusterVol) %>% #group by cluster; mean by cluster
  mutate(SouthCalc_SouthGam_meth3_clustervol = case_when(is.na(fit) ~ mean(m3S_1993pred_uniques$fit, na.rm=TRUE),
                                          TRUE ~ as.numeric(fit))) %>% 
  ungroup()


class(m3S_meth3)
class(m3S_NAmeth3)
m3S_meth3<-data.table(m3S_meth3) #rbind insists the dfs be "data tables"
m3S_NAmeth3<-data.table(m3S_NAmeth3)

m3S_meth3bound <- rbind(m3S_NAmeth3,m3S_meth3) #rbind req "data.table" class format.

m3S_meth3calc <- m3S_meth3bound %>% 
  mutate(SouthCalc_SouthGam_calc3_methXarea = ((SouthCalc_SouthGam_meth3_clustervol)*(surface_area_ha))) 

x<-m3S_meth3calc %>% group_by(STATE) %>% tally() %>% print(n=Inf) %>% ungroup() #View 

sum(m3S_meth3calc$SouthCalc_SouthGam_calc3_methXarea, na.rm=TRUE) #Schema 3 - South - Total Standing Stok (KG)



# ~~~~ --------------------------------------------------------------------






# GAM 4 - set up --------------------------------------------------------------------
# FOR CALC 4 - "ecoregion" - Level II ecoregion was identified for every reservoir. Mean fish mass was calculated across all reservoirs for an ecoregion. Ecoregion mean fish mass values were used for any reservoir with unknown fish biomass values.⍏

m4_freshDF <- c_ResFishData # Start fresh.
colnames(c_ResFishData)

#grouping column "NA_L2NAME" already exists

#FILTER - 
x<- m4_freshDF %>% group_by(NA_L2NAME)           %>% tally(sort=TRUE) %>% print(n=Inf) %>% ungroup() #Explore
x<- m4_freshDF %>% group_by(NA_L2NAME_lowercase) %>% tally(sort=TRUE) %>% print(n=Inf) %>% ungroup() #Explore

m4_gam_ready<-m4_freshDF %>% 
  dplyr::filter(!is.na(NA_L2NAME_lowercase) 
                & !is.na(Ecosystem)) %>% 
  group_by(NA_L2NAME_lowercase) %>%
  dplyr::filter(n()>5) %>% 
  ungroup()

x<-m4_gam_ready %>% group_by(NA_L2NAME) %>% tally() %>% print(n=Inf) %>% ungroup() #Explore


# GAM 4 - model -----------------------------------------------------------
### GAM based on needs of CALC2 (by= Median_Grouping_Large-vs-Small)

class(m4_gam_ready$Biomass_kgha)
class(m4_gam_ready$ResAge)
class(m4_gam_ready$NA_L2NAME_lowercase) #factor
class(m4_gam_ready$Ecosystem)
class(m4_gam_ready$Year)

m4 <- mgcv::gam(Biomass_kgha ~ s(ResAge, bs="tp", by=NA_L2NAME_lowercase) + 
                  s(Ecosystem, bs="re")+
                  s(Year, bs="tp"),
                family=Gamma(link=log), data=m4_gam_ready, method="REML")  

summary.gam(m4)

gam.check(m4)

plot.gam(m4, page=1, 
         all.terms=TRUE, 
         rug=TRUE,residuals=TRUE,pch = 1, cex = 1,
         shade=TRUE,shade.col = "lightblue",
         shift = coef(m4)[1])


# GAM 4 - predict ---------------------------------------------------------

#Create NewDF for Predict()
m4_MinYearRange<-m4_gam_ready %>% 
  group_by(Ecosystem) %>%                # Prediction by ECOSYSTEM (each ecosystem needs a prediction even though m4's smooth is by=NOTHING)
  summarize(minAge = min(ResAge) ) %>%   # Create column with the minimum age each reservoir had been when sampled 
  tidyr::expand_grid(AgePred = ((min(m4_gam_ready$ResAge)):130))%>% # Add Age-To-Predict-On col by min(ResAge):105
  dplyr::filter(minAge <= AgePred ) %>%  # Summarize causes dup rows; filter so that each res gets a unique AgePrediction
  ungroup()

# OTHER WAY TO DO IT 
#m4_resage_predictable_FishData<-m4_gam_ready %>% 
#  dplyr::select("ClassTypeL2", "ClassAbrv","Ecosystem.y","YearDamCompleted.y") %>% # just grab the name and grouping columns
#  distinct() %>% # filter out the unique rows
#  expand_grid(ResAge = 0:105) # expand so that each unique row occurs combined with each age

m4_MinYearRange %>%   #check new DF
  arrange(Ecosystem,AgePred) %>%  
  print(n= Inf )

# Want the NewDF to also have attributes related to select(xxx):
m4_TheRestOfPredDF<-m4_gam_ready %>%
  ungroup() %>% 
  dplyr::select("Ecosystem","YearDamCompleted", "NA_L2NAME_lowercase") %>% 
  distinct() 


# Join the DF with Age Predictions to the Attributes we want still associated with them...
m4_Join_forPredDF<-left_join(m4_TheRestOfPredDF,m4_MinYearRange)

m4_Predictable_FishData<-m4_Join_forPredDF %>% 
  rename(ResAge = AgePred) %>%               # Predict()'s NewDF doesn't have actual-age, so it's ok to rename to the name used in GAM. 
  arrange(ResAge) %>%                        # Arrange Res within a ClassificationType by Age; a helpful visual 
  mutate(Year = (YearDamCompleted + ResAge)) # Predict's NewDF also doesn't have Year_Sampled (found in GAM model too)

m4_predicted<-predict(m4, m4_Predictable_FishData, type="response", se.fit = TRUE)

m4_predbind<-do.call(cbind,m4_predicted) # iterated for each ecoregion & pulled into one df 
m4_predtable<-data.table(m4_predbind)
m4_pred_final<-cbind(m4_Predictable_FishData,m4_predtable)





# GAM 4 - plot - fit~resage -----------------------------------------------
### PredictBiomass~ResAge Plots ... with raw data overlayed 
# Link true biomass values to the rows they belong to. for all else in this column, NA 
# doing this will let us see raw data overlayed on predictions (plot1) 
#             & compare/validate true vs predicted biomass 1:1 (plot2)

m4_small<-m4_gam_ready %>% 
  ungroup() %>% 
  dplyr::select("Ecosystem","ResAge", "Biomass_kgha") #2 joining columns + the true empirical biomass column

m4_Actual_and_Pred_data<-left_join(m4_pred_final,m4_small, by = c("Ecosystem","ResAge")) #bind predictions to true biomass DF


m4_Actual_and_Pred_data %>%  
  dplyr::filter(Year<=1993) %>% 
  ggplot()+
  geom_point(aes(x=Year, y=fit),            color="gray50",         size=2, alpha=0.3, shape=16) + 
  geom_point(aes(x=Year, y=Biomass_kgha),   color="cornflowerblue", size=3, alpha=0.4, shape=16) + 
  scale_x_continuous(breaks = seq(1948, 1993, 10), limits=c(1948, 1993))+
  scale_y_continuous(breaks = seq(0, 3100, 500), limits=c(0, 3100))+
  facet_wrap(.~NA_L2NAME_lowercase, ncol=3,labeller=label_wrap_gen(width=31,multi_line = TRUE))+
  theme_bw()+
  theme(axis.title.x=element_text(size=12.8), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=11.5),
        axis.text.y =element_text(size=11.5),
        legend.position = "none",
        strip.text = element_text(size=11),
        strip.background = element_rect(fill=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  labs(x = "Year", 
       y = expression(paste("Fish Biomass"~('kg'~ha^-1))))


# GAM 4 - plot - validation -----------------------------------------------
# VALIDATE 
m4_truebio_and_predbio <-m4_Actual_and_Pred_data %>% 
  dplyr::filter(!is.na(Biomass_kgha))
basicStats(m4_truebio_and_predbio$Biomass_kgha)

m4_truebio_and_predbio %>% 
  ggplot(aes(x=fit, y=Biomass_kgha)) +
  geom_point(size=3.0, alpha=0.5, color="cornflowerblue")+
  geom_abline(aes(intercept=0.0,slope=1.0), size=1.0, alpha=0.8, linetype="solid")+
  scale_x_continuous(breaks = seq(0, 3100, 1000),limits=c(0, 3200), expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0, 3100, 1000),limits=c(0, 3200), expand = c(0, 0))+
  facet_wrap(.~NA_L2NAME_lowercase, ncol=3,labeller=label_wrap_gen(width=31,multi_line = TRUE))+
  #scale_color_manual(values = colorRampPalette(glasbey(27))(colourCount)) +
  theme_bw()+
  theme(axis.title.x=element_text(size=12.8), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=11.5),
        axis.text.y =element_text(size=11.5),
        legend.position = "none",
        strip.text = element_text(size=11),
        strip.background = element_rect(fill=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  labs(x = expression(paste("Predicted Fish Biomass"~('kg'~ha^-1))), 
       y = expression(paste("Observed Fish Biomass"~('kg'~ha^-1))))




# GAM 4 - plot - histogram -------------------------------------------------------
# of then-vs-now biomass

m4_histogram_1993mean<-m4_Actual_and_Pred_data %>% 
  group_by(NA_L2NAME_lowercase) %>% 
  dplyr::filter(Year==1993) %>% 
  mutate(MaxYr_mean = mean(fit)) %>% 
  dplyr::select("Ecosystem","NA_L2NAME_lowercase","MaxYr_mean") %>% 
  ungroup()

m4_histogram_maxyr<-left_join(m4_Actual_and_Pred_data,m4_histogram_1993mean, by=c("Ecosystem","NA_L2NAME_lowercase"))

m4_histogram_justempirical<-m4_histogram_maxyr %>% 
  dplyr::filter(!is.na(Biomass_kgha))

m4_histogram_minmax_yr<-m4_histogram_justempirical %>% 
  group_by(Ecosystem) %>% 
  dplyr::filter(Year==min(Year)) %>% 
  mutate(MinYr = Biomass_kgha) %>% 
  ungroup()

m4_histogram_difference<-m4_histogram_minmax_yr %>% 
  group_by(NA_L2NAME_lowercase) %>% 
  mutate(difference_biomass =  MaxYr_mean - MinYr) %>% 
  arrange(Ecosystem,fit) %>% 
  ungroup()

fBasics::basicStats(m4_histogram_difference$difference_biomass)

m4_histogram_difference %>% 
  ggplot(aes(x=difference_biomass))+
  geom_histogram(aes(y=..count../sum(..count..)),bins=30,fill="cornflowerblue",color="slategray1") +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1))+
  geom_abline(aes(intercept=0,slope=1), color="gray60",size=1, alpha=0.9, linetype="dashed")+
  theme_bw()+
  theme(axis.title.x=element_text(size=12.8), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=11.5),
        axis.text.y =element_text(size=11.5),
        legend.position = "none",
        strip.text = element_text(size=11),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
  #annotate(geom="text", x=-2120, y=.20, color="black", size=5, label="Mean = 104.17 ")+
  #annotate(geom="text", x=-1850, y=.19, color="black", size=5, label="Range = -2326.39 - 859.44")+
  #annotate(geom="text", x=-2090, y=.18, color="black", size=5, label="SE Mean = 23.72")+
  #annotate(geom="text", x=-2216, y=.17, color="black", size=5, label="SD = 388.41")+
  #labs( x = expression(paste("Raw Difference in Biomass"~('kg'~ha^-1))))




# GAM 4 - USA calculation -------------------------------------------------------

colnames(m4_Actual_and_Pred_data)

#Select only needed columns
m4_1993_predicted_biomass<-m4_Actual_and_Pred_data %>%  
  dplyr::filter(Year==1993) %>% 
  dplyr::select("Ecosystem","minAge","ResAge", "Year", "fit", "se.fit") 

m4_a_1993pred_join<-left_join(c_join_NIDbiomass,m4_1993_predicted_biomass, by="Ecosystem") 
m4_unique_res <- m4_a_1993pred_join[!duplicated(m4_a_1993pred_join$NIDID),] #make 1 row per res again. 
m4_a_1993pred_and_therest<-m4_unique_res


# Calculation 4 - L2 Ecoregion 

m4_O__L2_ECO_NA<- m4_a_1993pred_and_therest %>% # df with NA L2-Eco
  group_by(NA_L2NAME_lowercase) %>% 
  dplyr::filter(all(is.na(fit)) | is.na(NA_L2NAME_lowercase)) %>% 
  ungroup()

m4_O__L2_ECO<-m4_a_1993pred_and_therest[(!m4_a_1993pred_and_therest$RECORDID %in% m4_O__L2_ECO_NA$RECORDID),] #df with L2-Eco

m4_P__meanECO_NA <- m4_O__L2_ECO_NA %>% 
  mutate(gam_meth4_L2ecoregion = case_when(is.na(fit) ~ mean(m4_a_1993pred_and_therest$fit, na.rm=TRUE),
                                       TRUE ~ as.numeric(fit))) 

m4_P__meanECO <- m4_O__L2_ECO %>% 
  group_by(NA_L2NAME_lowercase) %>% 
  mutate(gam_meth4_L2ecoregion = case_when(is.na(fit) ~ mean(fit, na.rm=TRUE),
                                           TRUE ~ as.numeric(fit))) %>% 
  ungroup()

x <- m4_P__meanECO %>% group_by(NA_L2NAME_lowercase,Biomass_kgha, fit,gam_meth4_L2ecoregion) %>% tally() %>% ungroup() #View 

m4_P__meanECO    <- data.table(m4_P__meanECO) 
m4_P__meanECO_NA <- data.table(m4_P__meanECO_NA)
m4_P_bound<-rbind(m4_P__meanECO,m4_P__meanECO_NA)

# multiply by AREA to get the CALC

m4_P_finalL2 <-m4_P_bound %>% 
  mutate(gam_calc4_L2ecoXarea = ((m4_P_bound$gam_meth4_L2ecoregion)*(m4_P_bound$surface_area_ha))) 

sum(m4_P_finalL2$gam_calc4_L2ecoXarea, na.rm=TRUE) #Schema 4 USA - Total Standing Stock (KG)

write_csv(m4_P_finalL2,"data output/03_USA_GAM4_fit.csv") 




# GAM 4 - South -----------------------------------------------------------

sum(is.na(m4_P_finalL2$STATE)) #0

m4s_NA_biomass_states<-m4_P_finalL2 %>% #Remove states not in ROYL data set.
  group_by(STATE) %>% 
  dplyr::filter(all(is.na(Biomass_kgha)) | is.na(STATE)) %>%    
  ungroup()

m4S_SE_biomass<-m4_P_finalL2[(!m4_P_finalL2$RECORDID %in% m4s_NA_biomass_states$RECORDID),] #Keep states that are in the original ROYL dataset. 

x<-m4s_NA_biomass_states %>% group_by(STATE) %>% tally() %>% print(n=Inf) %>% ungroup() #view
x<-m4S_SE_biomass %>% group_by(STATE) %>% tally() %>% print(n=Inf) %>% ungroup()        #view

sum(m4S_SE_biomass$gam_calc4_L2ecoXarea, na.rm=TRUE) #Schema 4 South - Total Standing Stock (KG)

# ~~~~ --------------------------------------------------------------------


# AIC ---------------------------------------------------------------------

AIC(m1,m2,m3,m4,m5)
#             df       AIC
# m1    243.1628  14646.54
# m2    236.5298  14274.33
# m3    227.5092  13052.05
# m4    247.5736  14485.05
# m5    233.5400  12786.79 






# Production --------------------------------------------------------------
## Create DF from final Schema calcs for "Total Standing Stock (KG)" (can use document outline to navigate) 
## Calculate Production 

# For All-USA
df_USA_biomass_means <- data.frame(Calculation_Type = 
                                         c("USA_1_simpleaverage",
                                           "USA_2_median",
                                           "USA_3_cluster4volume",
                                           "USA_4_L2eco",
                                           "USA_5_L2ecovol"),
                                       Biomass_kg = 
                                         c(3031693257,
                                           3587800617,
                                           2976459235,
                                           3689453361,
                                           3855651138)) #25June2023 ALL USA 

df_USA_production<-df_USA_biomass_means %>% 
  mutate(Production = (Biomass_kg*1.3)) #multiply by P:B ratio

print(df_USA_production) # Manuscript Table 1

mean(df_USA_production$Biomass_kg) # USA mean biomass
mean(df_USA_production$Production) # USA mean production

sciplot::se(df_USA_production$Biomass_kg) # +/- SE
sciplot::se(df_USA_production$Production) # +/- SE

# For just the 22 Southeastern states
df_SOUTH_Biomass_means <- data.frame(Calculation_Type = 
                                             c("south_1_simpleaverage",
                                               "south_2_median",
                                               "south_3_cluster4volume",
                                               "south_4_L2eco",
                                               "south_5_L2ecovol"),
                                           Biomass_kg = 
                                             c(1693346335,
                                               2001994842,
                                               1713033098,
                                               2055598037,
                                               2137464393)) #25June2023 SOUTH 

df_SOUTH_Production<-df_SOUTH_Biomass_means %>% 
  mutate(Production = (Biomass_kg*1.3)) #multiply by P:B ratio

print(df_SOUTH_Production) # Manuscript Table 1

mean(df_SOUTH_Production$Biomass_kg) # SOUTH mean biomass
mean(df_SOUTH_Production$Production) # SOUTH mean production

sciplot::se(df_SOUTH_Production$Biomass_kg)
sciplot::se(df_SOUTH_Production$Production)


# Quick Metrics ------------------------------------------------------------
# You can use Object "m5_V_final", it is the final object for the Schema5 script section. 
# or if the above script isn't run, use its .csv:  XXX <- read_csv("data output/03_USA_GAM5_fit.csv") 

USA_GAM5_CSV <- read_csv("data output/03_USA_GAM5_fit.csv") # same as running "USA_GAM5_CSV <- m5_V_final"

colnames(USA_GAM5_CSV) 

fBasics::basicStats(USA_GAM5_CSV$Biomass_kgha) 
fBasics::basicStats(USA_GAM5_CSV$gam_calc5_L2ecoXarea)

#Create Production column
USA_GAM5_w_Production <- USA_GAM5_CSV %>% 
  mutate(Production = (gam_calc5_L2ecoXarea*1.3)) #( production = total standing stock (kg) * P:B ratio )

sum(is.na(USA_GAM5_w_Production$gam_calc5_L2ecoXarea)) #no NA 
sum(is.na(USA_GAM5_w_Production$Production))           #no NA


# QM: Sampled Reservoirs --------------------------------------------------
# find range, mean, SE in total standing stock (kg) for reservoirs w empirical data (n=301)

sampled_res<-USA_GAM5_w_Production %>% 
  dplyr::filter(!is.na(Ecosystem)) 

fBasics::basicStats(sampled_res$gam_calc5_L2ecoXarea) 

QuickMetric_SampledRes_Biomass <- sampled_res %>% 
  summarize(min        (gam_calc5_L2ecoXarea),
            max        (gam_calc5_L2ecoXarea),
            mean       (gam_calc5_L2ecoXarea),
            stdev      (gam_calc5_L2ecoXarea),
            sciplot::se(gam_calc5_L2ecoXarea),
            count = n())

QuickMetric_SampledRes_Production <- sampled_res %>% 
  summarize(min        (Production),
            max        (Production), 
            mean       (Production),
            stdev      (Production),
            sciplot::se(Production),
            count = n())


# QM: Southeast Reservoirs -------------------------------------------------------

ROYL_g5_metrics_SE_states<-USA_GAM5_w_Production %>% #Remove states not in NRRP dataset.
  group_by(STATE) %>% 
  dplyr::filter(!all(is.na(Biomass_kgha)) | is.na(STATE)) %>%   
  ungroup()

x<-ROYL_g5_metrics_SE_states %>% group_by(STATE) %>% tally() %>% print(n=Inf) %>% ungroup()#check

fBasics::basicStats(ROYL_g5_metrics_SE_states$gam_calc5_L2ecoXarea) 
fBasics::basicStats(ROYL_g5_metrics_SE_states$Production)

QuickMetric_SOUTH_N <- ROYL_g5_metrics_SE_states %>% 
  summarize(count = n())




# QM: Ecoregion quick metrics ---------------------------------------------

# Using: "USA_GAM5_CSV" 

# Take sum of GAM5-1993's Total Standing Stock (kg), by ECOREGION. 
# "X area" in "gam_calc5_L2ecoXarea" means it was already multiplied by surface area (ha); units are (kg). 

SumByEcoregion<-USA_GAM5_CSV %>% 
  dplyr::group_by(NA_L2NAME_lowercase) %>%
  mutate(sumECOREGION = sum(gam_calc5_L2ecoXarea,na.rm=TRUE)) %>% 
  ungroup()

# Get one row per L2 ECOREGION
ECOREGION <-SumByEcoregion[!duplicated(SumByEcoregion$NA_L2NAME_lowercase), ] 

x<-ECOREGION %>% dplyr::select(NA_L2NAME_lowercase,sumECOREGION) #explore
basicStats(ECOREGION$sumECOREGION) 

ECOREGION_B <- ECOREGION %>% 
  mutate(sumECOREGION_millons = sumECOREGION/1000000) #div by million

colnames(ECOREGION_B)

QM_Ecoregion<-ECOREGION_B %>% dplyr::select(NA_L2NAME_lowercase,sumECOREGION,sumECOREGION_millons) #explore again; comparing the 2 new columns. 

write_csv(QM_Ecoregion,"data output/TableS3 - Quick Metrics for Ecoregion.csv")

sum(ECOREGION_B$sumECOREGION) #this sums up to Table1's total standing stock (kg) for USA Schema5.



# QM: Ecoregion-Size-Flow quick merics ----------------------------------------

# Using: "USA_GAM5_CSV" 

SumByEcoSizeFlow<-USA_GAM5_CSV %>% 
  dplyr::group_by(ClassTypeL2) %>%
  mutate(sumEcoSizeFlow = sum(gam_calc5_L2ecoXarea,na.rm=TRUE)) %>% 
  ungroup()

# Get one row per L2 ECOREGION
EcoSizeFlow <-SumByEcoSizeFlow[!duplicated(SumByEcoSizeFlow$ClassTypeL2), ] 
x<-EcoSizeFlow %>% dplyr::select(ClassTypeL2,sumEcoSizeFlow) #explore
basicStats(EcoSizeFlow$sumEcoSizeFlow)

EcoSizeFlow_B <- EcoSizeFlow %>% 
  mutate(sumEcoSizeFlow_million = sumEcoSizeFlow/1000000) #div by million

QM_EcoSizeFlow<-EcoSizeFlow_B %>% dplyr::select(ClassTypeL2,sumEcoSizeFlow,sumEcoSizeFlow_million) #explore again; comparing the 2 new columns. 

write_csv(QM_EcoSizeFlow,"data output/TableS3 - Quick Metrics for EcoSizeFlow.csv")

sum(EcoSizeFlow_B$sumEcoSizeFlow) #this sums up to Table1's total standing stock (kg) for USA Schema5.



