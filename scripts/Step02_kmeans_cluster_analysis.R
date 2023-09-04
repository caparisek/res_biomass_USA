## ---------------------------
## Author: Christine A. Parisek, caparisek@ucdavis.edu
##
## This script:
##    Preps 2 clustering columns by LOGGING & SCALING
##    Performs K-MEANS cluster analysis (4 clusters), after grouping by either:
##        Omernik Level 1 ecoregions
##        Omernik Level 2 ecoregions (Figure 2)
##        Omernik Level 3 ecoregions
##    Creates output summary tables for reservoir stats based on OL2 (Dataset S2)
## ---------------------------


#install.packages("devtools") 
#devtools::install_github("johannesbjork/LaCroixColoR") 
library(LaCroixColoR) # Color palette for Figure 2
library(tidyverse)
library(data.table)
library (fBasics)   # Optional; for summary stats
options(scipen=999) # Optional; removes sci-notation


# Data - Reservoirs w Omernik ecoregion assignments -----------------------
## Reading in the output from end of script01
NID2018_omernik<-read_csv("data output/01_NID_Omernik_unitsconverted.csv") 

# Explore
class(NID2018_omernik)
colnames(NID2018_omernik)
unique(NID2018_omernik$NA_L1NAME) # View unique ecoregion names (L1)
unique(NID2018_omernik$NA_L2NAME) # View unique ecoregion names (L2)
unique(NID2018_omernik$NA_L3NAME) # View unique ecoregion names (L3)


# LOG & SCALE ---------------------------------------
## K-means clustering requires logged and scaled variables. 

NID_units_logscale<-NID2018_omernik %>% # log1p(x) computes log(1+x) accurately also for |x| << 1.
  mutate(maxdischarge_cms_log = log1p(max_discharge_cms))%>% 
  mutate(nidstorage_cm_log    = log1p(nid_storage_cm))

NID_units_logscale$maxdischarge_cms_logscale <- scale(NID_units_logscale$maxdischarge_cms_log) 
NID_units_logscale$nidstorage_cm_logscale    <- scale(NID_units_logscale$nidstorage_cm_log) 

colnames(NID_units_logscale) 
class(NID_units_logscale$nidstorage_cm_logscale)    # Matrix; array
class(NID_units_logscale$maxdischarge_cms_logscale) # Matrix; array

NID2018<-NID_units_logscale

# ~ ~ ~ -------------------------------------------------------------------
# L1 - cluster analysis ----------------------------------------------------------

x<-NID2018 %>% group_by(NA_L1NAME) %>% tally(sort=TRUE) %>% print(n=Inf) %>% ungroup() # # of res in each ecoregion. 

NID_Eco13 <- NID2018 %>% 
  dplyr::filter((!is.na(NA_L1NAME))
                & MAX_DISCHARGE>0  
                & NID_STORAGE>0) # Remove 0 prior to clustering (acting NA)

x=0
all_cluster<-list()
set.seed(123) # Run this prior to running below code (each time) for reproducibility
for(ecotype in unique(NID_Eco13$NA_L1NAME)){  
  print(ecotype)
  if(ecotype=="WATER"){next}
  x=x+1
  subnid<-NID_Eco13 %>% 
    dplyr::filter(NA_L1NAME==ecotype)
  subnid2<-subnid %>% 
    select(nidstorage_cm_logscale,maxdischarge_cms_logscale)
  k4<-kmeans(subnid2, centers=4,iter.max = 50)
  subnid$clusterL1<-k4$cluster
  all_cluster[[x]]<-subnid
}

NID_Eco13_Cluster<-do.call(rbind,all_cluster) # all_cluster iterated for each ecoregion is pulled into one df 

NID_Eco13_Cluster$ClusterEco_L1<-paste(NID_Eco13_Cluster$NA_L1NAME,NID_Eco13_Cluster$clusterL1,sep="_") # Create col that joins col "NA_L1NAME" with col "cluster" with a hypen separator. 

unique(NID_Eco13_Cluster$ClusterEco_L1)

# L1 - data visualization -------------------------------------------------

NID_Eco13_Cluster %>%
  ggplot(aes(nidstorage_cm_logscale, maxdischarge_cms_logscale, color = factor(clusterL1))) +
  facet_wrap(~NA_L1NAME)+
  geom_point(alpha=0.5)


# L1 - create final DF: bind clustered & non-clustered reservoirs -------

# Bind rows that were removed pre-L1-Cluster to post-L1-Cluster DF, so final DF has all reservoirs again. 
NID2018j_blankcols<-NID2018      # Name the last full-size DF something new; work with this object. 
NID2018j_blankcols$clusterL1<-NA # Populate with those columns names it's now "missing", which "NID-eco13cluster" had added.
NID2018j_blankcols$ClusterEco_L1<-NA

NID2018j_blankcols<-data.table(NID2018j_blankcols) # The following step requires DFs be "data tables"
NID_Eco13_Cluster<-data.table(NID_Eco13_Cluster)   # The following step requires DFs be "data tables"

NID_small<-NID2018j_blankcols[(!NID2018j_blankcols$RECORDID %in% NID_Eco13_Cluster$RECORDID),] 
# Explanation for above: %in% searches for one vector in another vector. This searches for NID2018's RECORDID inside Eco13, and removes ("!") them when creating NID_small. NID_small now has all reservoirs not used in the L1-Cluster analysis. 

L1_finaldf <-rbind(NID_Eco13_Cluster,NID_small) # Bind L1-Cluster DF to DF of reservoirs removed pre-L1-Clustering.

str(L1_finaldf) 
colnames(L1_finaldf)

fBasics::basicStats(L1_finaldf$clusterL1)
sum(!is.na(L1_finaldf$clusterL1)) #notNA 
sum(!is.na(L1_finaldf$NA_L1NAME)) #notNA 
sum(!is.na(L1_finaldf$NIDID))     #notNA 


# ~ ~ ~ -------------------------------------------------------------------
# L2 - cluster analysis ---------------------------------------
## Repeat steps above, but for Omernik Level 2 ecoregions. 

L2_df<-L1_finaldf # Rename

NID_Eco21<- L2_df %>% 
  dplyr::filter((!is.na(NA_L2NAME))       
                &(!is.na(MAX_DISCHARGE))  # New dataframe that has NAs removed from key columns 
                & (MAX_DISCHARGE>0)       
                & NID_STORAGE>0) 

x_explore<-L1_finaldf %>% group_by(NA_L2NAME) %>% tally(sort=TRUE) %>% print(n=Inf) %>% ungroup()
x_explore<-NID_Eco21  %>% group_by(NA_L2NAME) %>% tally(sort=TRUE) %>% print(n=Inf) %>% ungroup() 

x=0
all_cluster<-list()
set.seed(234) # Run this prior to running below code (each time) for reproducibility
for(ecotype in unique(NID_Eco21$NA_L2NAME)){  
  print(ecotype)
  if(ecotype=="ALASKA TUNDRA"){next}
  if(ecotype=="ALASKA BOREAL INTERIOR"){next}
  if(ecotype=="BOREAL CORDILLERA"){next}
  if(ecotype=="WATER"){next}
  x=x+1
  subnid<-NID_Eco21 %>% 
    dplyr::filter(NA_L2NAME==ecotype)
  subnid2<-subnid %>% 
    select(nidstorage_cm_logscale,maxdischarge_cms_logscale)
  k4<-kmeans(subnid2, centers=4,iter.max = 50)
  subnid$clusterL2<-k4$cluster
  all_cluster[[x]]<-subnid
}

NID_Eco21_Cluster<-do.call(rbind,all_cluster) # all_cluster iterated for each ecoregion is pulled into one df 
NID_Eco21_Cluster$ClusterEco_L2<-paste(NID_Eco21_Cluster$NA_L2NAME,NID_Eco21_Cluster$clusterL2,sep="_") # Create col that joins col "NA_L1NAME" with col "cluster" with a hypen separator. 

unique(NID_Eco21_Cluster$ClusterEco_L2) # View the new col w/ unique cluster types. (arid1,arid2,arid3,arid4)
x_explore<-NID_Eco21_Cluster %>% group_by(ClusterEco_L2) %>% tally(sort=TRUE) %>% print(n=Inf) %>% ungroup()

# L2 - data visualization ---------------------------------------------------------

NID_Eco21_Cluster %>%
  ggplot(aes(nidstorage_cm_logscale, maxdischarge_cms_logscale, color = factor(clusterL2))) +
  facet_wrap(~NA_L2NAME)+
  geom_point(alpha=1/2) 


# L2 - create final DF: bind clustered & non-clustered reservoirs ---------
L2_tobind<-L2_df            # Take the last full-sized df and make a new object to work with.
L2_tobind$clusterL2<-NA     # Need to add the column names that the full-sized df doesn't have 
L2_tobind$ClusterEco_L2<-NA # Need to add the column names that the full-sized df doesn't have 
L2_tobind          <-data.table(L2_tobind)           # The following step requires dfs be "data tables"
NID_Eco21_Cluster  <-data.table(NID_Eco21_Cluster)   # The following step requires dfs be "data tables"

NID_dropped<-L2_tobind[(!L2_tobind$RECORDID %in% NID_Eco21_Cluster$RECORDID),] 
# %in% searches for one vector in another vector. This searches for NID2018's RECORDID inside Eco21, and removes ("!") them to create NID_dropped. NID_dropped has all the reservoirs that were not included in Cluster Analaysis #2 (Omernik Level 2). 

L2_Needs_Renaming <-rbind(NID_Eco21_Cluster,NID_dropped) # Bind non-clustered + clustered dfs into one complete df.


# L2 - fix cluster labels to correspond w a definition ----------------------
## Note clustering was based on rules, but the assignment of "1,2,3,4" within each group (ecoregion) is random. (e.g., Everglades_4 might mean low discharge high flow, WarmDesert_4 could mean high discharge low flow). This is fixed below so that each assignment corresponds to the same discharge/flow definition across ecoregions. As long as you ran the setseed() prior to clustering, the output 1-4 assignments are reproducible and the below code to make definitions relevant will also be reproducible.

#Create column that changes case & shortens characters to better fit as facet-labels in Figure 2.
L2_EcoregionsRenamed_ColorsNot<-L2_Needs_Renaming %>% 
  mutate(NA_L2NAME_lowercase= case_when(
    NA_L2NAME== "WESTERN CORDILLERA" ~ "Western Cordillera", 
    NA_L2NAME== "MIXED WOOD PLAINS" ~ "Mixed Wood Plains", 
    NA_L2NAME== "SOUTHEASTERN USA PLAINS" ~ "Southeastern Plains", 
    NA_L2NAME== "CENTRAL USA PLAINS" ~ "Central Plains", 
    NA_L2NAME== "OZARK/OUACHITA-APPALACHIAN FORESTS" ~ "Ozark-Ouachita Appalachian Forests", 
    NA_L2NAME== "MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS" ~"Mississippi Alluvial and Southeast Coastal Plains",
    NA_L2NAME== "MEDITERRANEAN CALIFORNIA" ~ "Mediterranean California", 
    NA_L2NAME== "WEST-CENTRAL SEMIARID PRAIRIES" ~ "West-Central Semiarid Prairies", 
    NA_L2NAME== "SOUTH CENTRAL SEMIARID PRAIRIES" ~ "South Central Semiarid Prairies", 
    NA_L2NAME== "TEMPERATE PRAIRIES" ~ "Temperate Prairies", 
    NA_L2NAME== "TEXAS-LOUISIANA COASTAL PLAIN" ~ "Texas-Louisiana Coastal Plain", 
    NA_L2NAME== "TAMAULIPAS-TEXAS SEMIARID PLAIN" ~ "Tamaulipas-Texas Semiarid Plain", 
    NA_L2NAME== "ATLANTIC HIGHLANDS" ~ "Atlantic Highlands", 
    NA_L2NAME== "MIXED WOOD SHIELD" ~ "Mixed Wood Shield", 
    NA_L2NAME== "MARINE WEST COAST FOREST" ~ "Marine West Coast Forest", 
    NA_L2NAME== "COLD DESERTS" ~ "Cold Deserts", 
    NA_L2NAME== "WARM DESERTS" ~ "Warm Deserts", 
    NA_L2NAME== "UPPER GILA MOUNTAINS" ~ "Upper Gila Mountains", 
    NA_L2NAME== "WESTERN SIERRA MADRE PIEDMONT" ~ "Western Sierra Madre Piedmont", 
    NA_L2NAME== "EVERGLADES" ~ "Everglades", 
    NA_L2NAME== "BOREAL CORDILLERA" ~ "Boreal Cordillera", 
    NA_L2NAME== "ALASKA TUNDRA" ~ "Alaska Tundra", 
    NA_L2NAME== "ALASKA BOREAL INTERIOR" ~ "Alaska Boreal Interior", 
    NA_L2NAME== "WATER" ~ "Water"))
    
x<-L2_EcoregionsRenamed_ColorsNot %>% group_by(NA_L2NAME)           %>% tally(sort=TRUE) %>% print(n=Inf) %>% ungroup()#check
x<-L2_EcoregionsRenamed_ColorsNot %>% group_by(NA_L2NAME_lowercase) %>% tally(sort=TRUE) %>% print(n=Inf) %>% ungroup()#check 

sum(is.na(L2_EcoregionsRenamed_ColorsNot$clusterL2)) # Empty facets didn't have ecoregion assignment.
sum(is.na(L2_EcoregionsRenamed_ColorsNot$maxdischarge_cms_logscale))


#Visualize the problem (e.g., 1's don't mean the same thing in every facet.)
L2_EcoregionsRenamed_ColorsNot %>% 
  ggplot(aes(nidstorage_cm_logscale, maxdischarge_cms_logscale, color = factor(clusterL2))) +
  facet_wrap(~L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase, labeller=label_wrap_gen(width=32,multi_line = TRUE))+
  geom_point(alpha=0.8,size=1)+ 
  scale_color_manual(values=lacroix_palette("Pure",type = "continuous", n=4)) +
  theme_bw()+
  theme(#legend.position="none",
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        #axis.text.x= element_blank(), 
        #axis.text.y=element_blank(), 
        axis.title.x=element_text(size=20,face="bold"), 
        axis.title.y=element_text(size=20,face="bold"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        strip.text = element_text(size=12,face="bold"))+
  xlab(label = "Volume (cubic meters)")+ 
  ylab(label = "Maximum Discharge (cms)")

# Work through every Omernik L2 Ecoregion to:
## 1) Subset the ecoregion. 
## 2) Create # reassignments
## 3) Link #-reassignment to interpretable text. e.g., clusters relabeled as #1 always = "small volume, low discarge"
## 4) Check #2-3 was done correctly by plotting: 
###
###     xxx_2 %>%   # Use this to test assignment was done correctly. (below: xxx_2) 
###       ggplot(aes(nidstorage_cm_logscale, maxdischarge_cms_logscale, color = factor(L2_ClustReassign))) +
###       geom_point(alpha=0.8,size=5)+ 
###       scale_color_manual(values=lacroix_palette("Pure",type = "continuous", n=4)) +
###       theme_bw() # Tip to reassign right the first time: "what's shown as color X should be color Y"
###




WarmDesert<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Warm Deserts")) 
WarmDesert_2<-WarmDesert %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 4, 
                            clusterL2==2 ~ 1,
                            clusterL2==3 ~ 3,
                            clusterL2==4 ~ 2)) %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #med. blue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy
WarmDesert_2 %>% # Use this to test assignment was done correctly.
  ggplot(aes(nidstorage_cm_logscale, maxdischarge_cms_logscale, color = factor(L2_ClustReassign))) +
  geom_point(alpha=0.8,size=5)+ 
  scale_color_manual(values=lacroix_palette("Pure",type = "continuous", n=4)) +
  theme_bw() 

ColdDesert<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Cold Deserts")) 
ColdDesert_2<-ColdDesert %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 3, 
                            clusterL2==2 ~ 2,
                            clusterL2==3 ~ 1,
                            clusterL2==4 ~ 4)) %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy

WC_SemiaridPrairies<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("West-Central Semiarid Prairies")) 
WC_SemiaridPrairies_2<-WC_SemiaridPrairies %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 3, 
                            clusterL2==2 ~ 2,
                            clusterL2==3 ~ 4,
                            clusterL2==4 ~ 1))  %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy

SC_SemiaridPrairies<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("South Central Semiarid Prairies")) 
SC_SemiaridPrairies_2<-SC_SemiaridPrairies %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 3, 
                            clusterL2==2 ~ 1,
                            clusterL2==3 ~ 4,
                            clusterL2==4 ~ 2)) %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy

TT_SemiaridPlain<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Tamaulipas-Texas Semiarid Plain")) 
TT_SemiaridPlain_2<-TT_SemiaridPlain %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 3, 
                            clusterL2==2 ~ 4,
                            clusterL2==3 ~ 1,
                            clusterL2==4 ~ 2)) %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy

TemperatePrairies<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Temperate Prairies")) 
TemperatePrairies_2<-TemperatePrairies %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 3, 
                            clusterL2==2 ~ 1,
                            clusterL2==3 ~ 2,
                            clusterL2==4 ~ 4))  %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy

TexasLouisiana_CoastalPlain<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Texas-Louisiana Coastal Plain")) 
TexasLouisiana_CoastalPlain_2<-TexasLouisiana_CoastalPlain %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 4, 
                            clusterL2==2 ~ 2,
                            clusterL2==3 ~ 3,
                            clusterL2==4 ~ 1)) %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy

Everglades<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Everglades")) 
Everglades_2<-Everglades %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 2, 
                            clusterL2==2 ~ 1,
                            clusterL2==3 ~ 4,
                            clusterL2==4 ~ 3)) %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy

SoutheasternPlains<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Southeastern Plains")) 
SoutheasternPlains_2<-SoutheasternPlains %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 4, 
                            clusterL2==2 ~ 1,
                            clusterL2==3 ~ 3,
                            clusterL2==4 ~ 2))  %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy

OzarkAppalachian<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Ozark-Ouachita Appalachian Forests")) 
OzarkAppalachian_2<-OzarkAppalachian %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 4, 
                            clusterL2==2 ~ 2,
                            clusterL2==3 ~ 3,
                            clusterL2==4 ~ 1)) %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy

MississippiAlluvial_SoutheastCoastalPlains<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Mississippi Alluvial and Southeast Coastal Plains")) 
MississippiAlluvial_SoutheastCoastalPlains_2<-MississippiAlluvial_SoutheastCoastalPlains %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 4, 
                            clusterL2==2 ~ 3,
                            clusterL2==3 ~ 2,
                            clusterL2==4 ~ 1)) %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy

CentralPlains<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Central Plains")) 
CentralPlains_2<-CentralPlains %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 1, 
                            clusterL2==2 ~ 3,
                            clusterL2==3 ~ 2,
                            clusterL2==4 ~ 4)) %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy

MixedWoodPlains<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Mixed Wood Plains")) 
MixedWoodPlains_2<-MixedWoodPlains %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 1, 
                            clusterL2==2 ~ 2,
                            clusterL2==3 ~ 4,
                            clusterL2==4 ~ 3)) %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy

WesternCordillera<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Western Cordillera")) 
WesternCordillera_2<-WesternCordillera %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 2, 
                            clusterL2==2 ~ 3,
                            clusterL2==3 ~ 4,
                            clusterL2==4 ~ 1)) %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy

MediterraneanCalifornia<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Mediterranean California")) 
MediterraneanCalifornia_2<-MediterraneanCalifornia %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 2, 
                            clusterL2==2 ~ 1,
                            clusterL2==3 ~ 3,
                            clusterL2==4 ~ 4)) %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy

UpperGilaMountains<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Upper Gila Mountains")) 
UpperGilaMountains_2<-UpperGilaMountains %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 4, 
                            clusterL2==2 ~ 1,
                            clusterL2==3 ~ 3,
                            clusterL2==4 ~ 2)) %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy

MixedWoodShield<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Mixed Wood Shield")) 
MixedWoodShield_2<-MixedWoodShield %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 4, 
                            clusterL2==2 ~ 1,
                            clusterL2==3 ~ 3,
                            clusterL2==4 ~ 2)) %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy 

AtlanticHighlands<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Atlantic Highlands")) 
AtlanticHighlands_2<-AtlanticHighlands %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 3, 
                            clusterL2==2 ~ 1,
                            clusterL2==3 ~ 4,
                            clusterL2==4 ~ 2))%>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy 

MarineWestCoastForest<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Marine West Coast Forest")) 
MarineWestCoastForest_2<-MarineWestCoastForest %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 1, 
                            clusterL2==2 ~ 3,
                            clusterL2==3 ~ 4,
                            clusterL2==4 ~ 2)) %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy 

WesternSierraMadrePiedmont<- L2_EcoregionsRenamed_ColorsNot %>% 
  subset(L2_EcoregionsRenamed_ColorsNot$NA_L2NAME_lowercase %in% c("Western Sierra Madre Piedmont")) 
WesternSierraMadrePiedmont_2<-WesternSierraMadrePiedmont %>% 
  mutate(L2_ClustReassign= case_when(clusterL2==1 ~ 4, 
                            clusterL2==2 ~ 3,
                            clusterL2==3 ~ 1,
                            clusterL2==4 ~ 2)) %>% 
  mutate(Class4Name = case_when(L2_ClustReassign==1 ~ "Small Volume & Low Discharge",   #lightblue
                                L2_ClustReassign==2 ~ "Small Volume & High Discharge",  #teal
                                L2_ClustReassign==3 ~ "Large Volume & Low Discharge",   #medblue
                                L2_ClustReassign==4 ~ "Large Volume & High Discharge")) #navy 


# Now that each region is able to be labeled & colored by accurate dischage-volume, put them back into one DF. 
recoded<-rbind(WarmDesert_2,
               ColdDesert_2,
               WC_SemiaridPrairies_2,
               SC_SemiaridPrairies_2,
               TT_SemiaridPlain_2,
               TemperatePrairies_2,
               TexasLouisiana_CoastalPlain_2,
               Everglades_2,
               SoutheasternPlains_2,
               OzarkAppalachian_2,
               MississippiAlluvial_SoutheastCoastalPlains_2,
               CentralPlains_2,
               MixedWoodPlains_2,
               WesternCordillera_2,
               MediterraneanCalifornia_2,
               UpperGilaMountains_2,
               MixedWoodShield_2,
               AtlanticHighlands_2,
               MarineWestCoastForest_2,
               WesternSierraMadrePiedmont_2)

# Now add to that DF: those reservoirs that didn't have an ecoregion-cluster assignment in the first place. 
UnColored_Ecoregions_dropped<-L2_EcoregionsRenamed_ColorsNot[(!L2_EcoregionsRenamed_ColorsNot$RECORDID %in% recoded$RECORDID),] 

unique(UnColored_Ecoregions_dropped$NA_L2NAME_lowercase) # These ecoregions were dropped from the relabel (xxx_2) rbind bc they were not included in the L2 for loop (due too small sample size or not having one of the clustering variables). 

UnColored_Ecoregions_dropped$L2_ClustReassign<-NA # Create the columns that are missing, fill with NA
UnColored_Ecoregions_dropped$Class4Name<-NA       # Create the columns that are missing, fill with NA

L2_finalDF <- rbind(UnColored_Ecoregions_dropped,recoded) # Bind the L2-Clustered reservoirs to the non-clustered reservoirs. 

colnames(L2_finalDF)

# Create column linking "Ecoregion, x Volume, y Discharge" into one full character name. 
L2_finalDF$ClassTypeL2<- paste(L2_finalDF$NA_L2NAME_lowercase, 
                                                  L2_finalDF$Class4Name, sep=", ")

unique(L2_finalDF$ClassTypeL2) # View new names in the new column.  


# L2 - Figure 2 -----------------------------------------------------------

x<-recoded %>% group_by(L2_ClustReassign) %>% tally() %>% print(n=Inf) %>% ungroup() # NAs were excluded from L2-Clustering due to missing variable

L2Plot<-recoded %>% # Prep for plot
  dplyr::filter(!is.na(L2_ClustReassign))

x<-L2Plot %>% group_by(L2_ClustReassign) %>% tally() %>% print(n=Inf) %>% ungroup() 

L2Plot %>%
  ggplot(aes(nidstorage_cm_logscale, maxdischarge_cms_logscale, color = factor(L2_ClustReassign))) +
  facet_wrap(~L2Plot$NA_L2NAME_lowercase, labeller=label_wrap_gen(width=21, multi_line = TRUE))+
  geom_point(alpha=0.4,size=3)+ 
  scale_color_manual(values=LaCroixColoR::lacroix_palette("Pure",type = "continuous", n=4))+
  theme_bw()+
  #xlab(label = "Volume (cubic meters)")+ 
  #ylab(label = "Maximum Discharge (cms)")+
  xlab(expression("Storage Volume"~('log,'~m^3))) + 
  ylab(expression("Maximum Discharge"~('log,'~m^3~s^-1))) + 
  labs(col="Cluster")+
  theme(panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x= element_blank(), 
        axis.text.y=element_blank(), 
        axis.title.x=element_text(size=13), 
        axis.title.y=element_text(size=13), 
        #strip.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=11),
        strip.background = element_rect(fill=NA),
        legend.position="none")

ggsave("figures/Fig_2_Cluster.tiff", height=18, width=22, units="cm", dpi=900)


#  ~ ~ ~  -----------------------------------------------------------------

# L3 - cluster analysis ---------------------------------------
## Exploratory; L3 not used in final analysis. 
## Repeat steps for L1 & L2, but now for Omernik Level 3 (L3) ecoregions. 
## See L2 cluster analysis for detailed code annotation and explanation. 

L3_DF <-L2_finalDF

NID_Eco_L3<- L3_DF %>% 
  dplyr::filter(  (!is.na(NA_L3NAME))     #removed NA from L3 ecoregion
                & (!is.na(MAX_DISCHARGE)) #new dataframe that has NAs removed from CP_max_discharge
                & (MAX_DISCHARGE>0)       #keep old col name bc scaled one has negatives 
                & (NID_STORAGE>0))

x_view<-NID_Eco_L3 %>% group_by(NA_L3NAME) %>% tally() %>% print (n=Inf) %>% ungroup() # # rows associated with each ecoregion. 

x=0
all_cluster<-list()
set.seed(345) # Run this prior to running below code (each time) for reproducibility
for(ecotype in unique(NID_Eco_L3$NA_L3NAME)){  
  print(ecotype)
  if(ecotype=="Alaska Range"){next}
  if(ecotype=="Interior Bottomlands"){next}
  if(ecotype=="Interior Highlands and Klondike Plateau"){next}
  if(ecotype=="Arctic Coastal Plain"){next}
  if(ecotype=="Arctic Foothills"){next}
  if(ecotype=="Interior Forested Lowlands and Uplands"){next}
  if(ecotype=="Water"){next}
  x=x+1
  subnid<-NID_Eco_L3 %>% 
    dplyr::filter(NA_L3NAME==ecotype)
  subnid2<-subnid %>% 
    select(nidstorage_cm_logscale,maxdischarge_cms_logscale)
  k4<-kmeans(subnid2, centers=4)
  subnid$clusterL3<-k4$cluster
  all_cluster[[x]]<-subnid
}

NID_Eco_L3_Cluster<-do.call(rbind,all_cluster) #all_cluster iterated for each ecoregion is pulled into one df 

NID_Eco_L3_Cluster$ClusterEco_L3<-paste(NID_Eco_L3_Cluster$NA_L3NAME,NID_Eco_L3_Cluster$clusterL3,sep="_")
#Create col that joins col "NA_L1NAME" with col "cluster" with a hypen separator. 

table(NID_Eco_L3_Cluster$ClusterEco_L3) # view the new col w/ ~57 unique cluster types. (arid1,arid2,arid3,arid4)
unique(NID_Eco_L3_Cluster$ClusterEco_L3)
x_view<-NID_Eco_L3_Cluster %>% group_by(ClusterEco_L3) %>% tally() %>% print(n=Inf) %>% ungroup()



# L3 - create final DF: bind clustered & non-clustered reservoirs ---------
L3_tobind<-L3_DF
L3_tobind$clusterL3<-NA     # Create the columns the new DF is missing prior to binding; populate with NA. 
L3_tobind$ClusterEco_L3<-NA # Create the columns the new DF is missing prior to binding; populate with NA. 
L3_tobind<-data.table(L3_tobind)                  # The following step requires DF be "data table"
NID_Eco_L3_Cluster<-data.table(NID_Eco_L3_Cluster)# The following step requires DF be "data table"

# Create DF of reservoirs exclused from L3-Cluster-Analysis. 
NID_L3small<-L3_tobind[(!L3_tobind$RECORDID %in% NID_Eco_L3_Cluster$RECORDID),] 

unique(NID_Eco_L3_Cluster$clusterL3) # Check
unique(NID_L3small$clusterL3)        # Check

L3_rebound <-rbind(NID_Eco_L3_Cluster,NID_L3small) # Final DF with both L3-Clustered and Non-L3-Clustered reservoirs. 

str(L3_rebound)

# L3 - data visualization ---------------------------------------------------------

L3_rebound %>%
  as_tibble() %>%
  ggplot(aes(nidstorage_cm_logscale, maxdischarge_cms_logscale, color = factor(clusterL3))) +
  facet_wrap(~L3_rebound$NA_L3NAME, labeller=label_wrap_gen(width=32,multi_line = TRUE))+
  geom_point(alpha=0.8,size=1)+ 
  scale_color_manual(values=lacroix_palette("Pure",type = "continuous", n=4)) +
  theme_bw()+
  theme(legend.position="none",
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x= element_blank(), 
        axis.text.y=element_blank(), 
        axis.title.x=element_text(size=10,face="bold"), 
        axis.title.y=element_text(size=20,face="bold"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        strip.text = element_text(size=12,face="bold"))+
  xlab(label = "Volume (cubic meters)")+ 
  ylab(label = "Maximum Discharge (cms)")


# ~ ~ ~ -------------------------------------------------------------------
# Dataset S2 --------------------------------------------------------------
# Create a new object for the final DF from the Omernik Level 2 based cluster analysis. 
a_SUPPLEMENT<-L2_finalDF

unique(a_SUPPLEMENT$NA_L2NAME_lowercase) # Explore
unique(a_SUPPLEMENT$L2_ClustReassign)    # Explore
unique(a_SUPPLEMENT$Class4Name)          # Explore
unique(a_SUPPLEMENT$ClassTypeL2)         # Explore

# Summary stats of reservoirs included in each L2-Ecoregion-Cluster
c_SUPP_TABLE<-a_SUPPLEMENT %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(Class4Name)) %>% 
  group_by(NA_L2NAME_lowercase,L2_ClustReassign,Class4Name) %>% 
  summarize(max_discharge_cms_median = median(max_discharge_cms,na.rm=TRUE),
            max_discharge_cms_mean   = mean(max_discharge_cms,  na.rm=TRUE),
            max_discharge_cms_min    = min(max_discharge_cms,   na.rm=TRUE),
            max_discharge_cms_max    = max(max_discharge_cms,   na.rm=TRUE),
            nid_storage_cm_median = median(nid_storage_cm,      na.rm=TRUE),
            nid_storage_cm_mean   = mean(nid_storage_cm,        na.rm=TRUE),
            nid_storage_cm_min    = min(nid_storage_cm,         na.rm=TRUE),
            nid_storage_cm_max    = max(nid_storage_cm,         na.rm=TRUE), .groups = "keep")


unique(c_SUPP_TABLE$max_discharge_cms_median) #Explore
unique(c_SUPP_TABLE$max_discharge_cms_mean  ) #Explore 
unique(c_SUPP_TABLE$max_discharge_cms_min   ) #Explore 
unique(c_SUPP_TABLE$max_discharge_cms_max   ) #Explore 
unique(c_SUPP_TABLE$nid_storage_cm_median   ) #Explore
unique(c_SUPP_TABLE$nid_storage_cm_mean     ) #Explore
unique(c_SUPP_TABLE$nid_storage_cm_min      ) #Explore
unique(c_SUPP_TABLE$nid_storage_cm_max      ) #Explore

write_csv(c_SUPP_TABLE,"data output/Dataset S2 - basic stats on clusters.csv") # Dataset S2

# ~ ~ ~ -------------------------------------------------------------------

# Calc.Meth - ClusterVolume column ----------------------------------------
## Create column based on splitting reservoirs into 4 size-discharge clusters (regardless of ecoregion).
## This will be used later in a Schema calculation. 


sum(is.na(L3_rebound$maxdischarge_cms_logscale))        # Explore
sum(is.na(L3_rebound$nidstorage_cm_logscale))           # Explore
sum(L3_rebound$maxdischarge_cms_logscale==0,na.rm=TRUE) # Explore
sum(L3_rebound$nidstorage_cm_logscale==0, na.rm=TRUE)   # Explore


# Split DF into 2 groups: (1) Reservoirs that are clusterable. (2) Reservoirs that are not clusterable. 
aa_vols_group<- L3_rebound %>% # This group IS clusterable by volume. 
  dplyr::filter(! (is.na(maxdischarge_cms_logscale))
                & (MAX_DISCHARGE>0)
                & (NID_STORAGE>0)) 

aa_volNA_group<- L3_rebound %>% # This group IS NOT clusterable by volume.
  dplyr::filter(is.na(maxdischarge_cms_logscale) 
                | !MAX_DISCHARGE>0 
                | !NID_STORAGE>0)  

49130+36340 # = size of original DF 


# K-Means Cluster Analysis
set.seed(688) # Run this prior to running below code (each time) for reproducibility
aa_k4 <- aa_vols_group %>% 
  select(maxdischarge_cms_logscale,nidstorage_cm_logscale) %>%
  kmeans(centers = 4,iter.max = 200) #4 clusters in a list

# Add the new clustering column to both the Clusterable and Non-Clusterable DFs. 
aa_vols_k4_addedcol<-aa_vols_group %>% 
  mutate(meth_clustvol_k4 = aa_k4$cluster) # Attach the cluster list as a new column 

aa_volNA_group$meth_clustvol_k4<-NA # Create column by same name; populate with NAs since it has no clustering data.  


class(aa_vols_k4_addedcol)                           # Check class of the DF
class(aa_volNA_group)                                # Check class of the DF
aa_vols_k4_addedcol<-data.table(aa_vols_k4_addedcol) # rbind() requires the DF class be "data table"
aa_volNA_group<-data.table(aa_volNA_group)           # rbind() requires the DF class be "data table"

# Join clustered and non-clustered DFs back into one full DF
aa_meth_clustvol <- rbind(aa_volNA_group,aa_vols_k4_addedcol)


# Reassign cluster assignments to correspond with the same class-groups as previously. 
L123_and_ClustVol<- aa_meth_clustvol %>% 
  mutate(gam3_BY_ClusterVol = case_when(meth_clustvol_k4 == "3" ~ "Small Volume - Low Discharge",
                                        meth_clustvol_k4 == "1" ~ "Small Volume - High Discharge",
                                        meth_clustvol_k4 == "4" ~ "Large Volume - Low Discharge",
                                        meth_clustvol_k4 == "2" ~ "Large Volume - High Discharge"))


L123_and_ClustVol %>% # Check colors match the more interpretable name 
  ggplot()+
  geom_point(aes(x=nidstorage_cm_logscale, 
                 y=maxdischarge_cms_logscale, 
                 color=gam3_BY_ClusterVol),
             alpha=0.2,
             size=4)

colnames(L123_and_ClustVol)
class(L123_and_ClustVol$nidstorage_cm_logscale) #numeric

write_csv(L123_and_ClustVol, "data output/02_L1L2L3_Clustered.csv") # Use this to begin Script 03. 

 



