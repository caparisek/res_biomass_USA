## ---------------------------
## Author: Christine A. Parisek, caparisek@ucdavis.edu
##
## This script:  
##    1) Takes NID-dams and condenses to a NID-reservoirs database using rules.
##    2) Spatial-Joins NID to Omernik Ecoregions.
##    3) Tidies NID reservoirs database, filling missing info using rules.
##    4) Creates new Surface Area column where NA filled in using rules; see SI Methods for its use.
##    5) Outputs a final reservoirs dataset that is tidied for analysis. 
##    See SI Methods for detailed description.
##
##    - In any script, objects beginning with "x_" are checkpoints non-essential to code flow.
##      Script titles with same prefix (e.g., Step04_) can be performed in any order after Script 03.
##      See Rstudio script "document outline" for easy navigation. 
## ---------------------------


library(tidyverse)
library(tigris)
library(sf)
sf::sf_use_s2() 
options(scipen=999) #optional; removes sci-notation
library(fBasics)    #optional; to get summary statistics fBasics::basicStats()
library(mapview)    #optional; to viz res falling outside of ecoregion delineations


# Data – National Inventory of Dams (NID) ---------------------------------
NID2018 <-read_csv("data/NID/NID2018_U-1_original-zeroblank_latlongchangedto00_MaxDischarge-NIDStorage-SurfaceArea_zerosremainbutblanksgetNA.csv") 
## NID.csv required tidying before reading in:
##    (NA,0) latlongs got (0,0) for spatial join ease.
##    MAX_DISCHARGE: blanks got NA. Note: many zeros exist, can't help this. 
##    NID_STORAGE:   had no blanks. Note: some zeros exist, can't help this. 
##    SURFACE_AREA:  blanks got NA. Note: some zeros exist + will be addressed later in this script.
##    Other columns have many "" blanks; filled these (below) to avoid parsing failures.

NID2018[NID2018==""]  <- NA                         # Fill blanks with NA to avoid parsing failure
NID2018$DAM_NAME <- sub(" ", "_", NID2018$DAM_NAME) # Fill spaces in DAM_NAME column with "_"


# Surface Area - convert zeros to N/A -----------------------------------------------
x_check_A<-NID2018 %>% group_by(SURFACE_AREA) %>% tally(sort=TRUE)%>% ungroup()     #pre-check
NID2018$SURFACE_AREA <- replace(NID2018$SURFACE_AREA, NID2018$SURFACE_AREA == 0, NA)#convert
x_check_B<-NID2018 %>% group_by(SURFACE_AREA) %>% tally(sort=TRUE)%>% ungroup()     #post-check

# Remove duplicate NIDID rows -----------------------------------------------
NIDIDunique <- NID2018[!duplicated(NID2018$NIDID),]

# Explore columns 
sum(is.na(NIDIDunique$STATE))      #0 NA
sum(is.na(NIDIDunique$COUNTY))     #1390 NA
sum(is.na(NIDIDunique$RIVER))      #7603 NA 
sum(is.na(NIDIDunique$CITY))       #22795 NA
sum(is.na(NIDIDunique$LATITUDE))   #0 NA
sum(is.na(NIDIDunique$LONGITUDE))  #0 NA
x_st <- NIDIDunique %>% group_by(STATE) %>% tally(sort=TRUE) %>% print(n=Inf) %>% ungroup() # res per state
x_lt <- NIDIDunique %>% group_by(LATITUDE) %>% tally(sort=TRUE)  %>% ungroup() #244 Lats. w value "zero"
x_lg <- NIDIDunique %>% group_by(LONGITUDE) %>% tally(sort=TRUE) %>% ungroup() #244 Longs. w value "zero"

# Within criteria... if X is identical, filter out dups:
USA_reservoirs_nonrep <- NIDIDunique %>% 
  group_by(STATE, COUNTY, DAM_NAME) %>% 
  dplyr::filter(  !duplicated(SURFACE_AREA, na.rm=TRUE) 
                & !duplicated(MAX_DISCHARGE,na.rm=TRUE)
                & !duplicated(NID_STORAGE,na.rm=TRUE))%>% 
  ungroup()

# Remove largest natural waterbodies --------------------------------------
## Remove largest natural waterbodies that are simply dammed; e.g., Lake Superior, Lake Tahoe. 
## Method: Surface Area was sorted largest-to-smallest and largest 100 waterbodies were manually searched.

USA_reservoirs_artificial<-USA_reservoirs_nonrep %>% 
  dplyr::filter(!NIDID %in% c("MI00650",
                              "MN00573",
                              "CA10162",
                              "ID00319",
                              "ME00091",
                              "NY00410",
                              "MN00653",
                              "NH00216",
                              "CA00911",
                              "NY00709",
                              "NY00416",
                              "WA00004",
                              "ID06126",
                              "FL36001",
                              "FL00312",
                              "FL00678",
                              "FL00291",
                              "FL74004",
                              "FL74003",
                              "FL74002",
                              "WI00814",
                              "WI00647",
                              "MN00585",
                              "MN01645",
                              "WA00456"))  

x_view<-USA_reservoirs_nonrep %>% 
  ungroup() %>% 
  dplyr::select(NIDID,DAM_NAME,OTHER_DAM_NAME,STATE, RIVER,NID_STORAGE) %>% 
  dplyr::filter(NIDID %in% c("MI00650",
                              "MN00573",
                              "CA10162",
                              "ID00319",
                              "ME00091",
                              "NY00410",
                              "MN00653",
                              "NH00216",
                              "CA00911",
                              "NY00709",
                              "NY00416",
                              "WA00004",
                              "ID06126",
                              "FL36001",
                              "FL00312",
                              "FL00678",
                              "FL00291",
                              "FL74004",
                              "FL74003",
                              "FL74002",
                              "WI00814",
                              "WI00647",
                              "MN00585",
                              "MN01645",
                              "WA00456")) 




# Allow 2 NRRP NIDIDs to remain unique waterbodies when joining to --------
## See SI Methods for context. 

# Create new DF with the two Dam NIDIDs
NRRP_Ecosystems <- USA_reservoirs_artificial %>% 
  dplyr::filter((NIDID %in% c('TN11502','TX04356')))

# Give 2 dam-NIDIDs unique IDs that correspond with the unique reservoir Ecosystem
NRRP_Ecosystems_Nickajack      <- within(NRRP_Ecosystems,           NIDID[DAM_NAME == "NICKAJACK"]                <- 'TN11502a')
NRRP_Ecosystems_SulphurSprings <- within(NRRP_Ecosystems_Nickajack, NIDID[DAM_NAME == "LAKE_SULPHUR_SPRINGS_DAM"] <- 'TX04356a')

# Add these two rows back to NID
NID_NRRP<- rbind(x = USA_reservoirs_artificial, y = NRRP_Ecosystems_SulphurSprings)

# Give last 2 dam-NIDIDs unique IDs that correspond with the unique reservoir Ecosystem
NID_NRRP_a <- within(NID_NRRP,   NIDID[NIDID == "TN11502"] <- 'TN11502b')
NID_NRRP_b <- within(NID_NRRP_a, NIDID[NIDID == "TX04356"] <- 'TX04356b')

# Check
x_view<-NID_NRRP_b %>% 
  dplyr::filter((DAM_NAME %in% c('NICKAJACK','LAKE_SULPHUR_SPRINGS_DAM')))



# Keep only contiguous USA reservoirs -------------------------------------
## Remove NID reservoirs not in the contiguous USA
USA_reservoirs_contig<-NID_NRRP_b %>%
  dplyr::filter(!(STATE %in% c('AK','HI','PR','VI','GU')))


# Fill (0,0) lat-longs with centroid of the COUNTY ------------------------
## Most (0,0) res have COUNTY-STATE info; for those, fill lat-long with centroid of COUNTY so that the res gets assigned an approx. ecoregion rather than being removed from the analysis entirely. 

# Replace lat-long zeros with NA for the case_when() to work. 
USA_reservoirs_contig$LATITUDE <- replace(USA_reservoirs_contig$LATITUDE, USA_reservoirs_contig$LATITUDE == 0, NA)#convert
USA_reservoirs_contig$LONGITUDE <- replace(USA_reservoirs_contig$LONGITUDE, USA_reservoirs_contig$LONGITUDE == 0, NA)#convert

sum(is.na(USA_reservoirs_contig$LATITUDE)) #242 NA
sum(is.na(USA_reservoirs_contig$LONGITUDE))#242 NA

# Create subset to explore
x_explore<-USA_reservoirs_contig %>%  
  ungroup() %>% 
  dplyr::select(NIDID, DAM_NAME, OTHER_DAM_NAME, STATE, COUNTY, CITY, RIVER, LATITUDE,LONGITUDE) %>%
  dplyr::filter(is.na(LATITUDE)) 

sum(is.na(x_explore$COUNTY)) # How many will the centroid-fill method not work for? 22. Will fill these manually in later step of script01


NoLatLong<-USA_reservoirs_contig %>% # Create DF of the reservoirs that need centroid filling
  ungroup() %>% 
  dplyr::filter(is.na(LATITUDE))

ctys <- tigris::counties(cb=TRUE) # Load the counties
ctys <- cbind(ctys, sf::st_coordinates(sf::st_centroid(ctys)))# Attach centroid locations to county data.frame

class(ctys)
st_crs(ctys)

# Make CTYS an sf_object & assign Coordinate Reference System (CRS) to match Omernik data
ctys_sf <- st_as_sf(ctys, coords = c("X", "Y"), crs=4326,remove = FALSE) # (X,Y) is (LONG,LAT)

st_crs(ctys_sf)         #check
colnames(ctys)          #explore
unique(ctys$STUSPS)     #explore
unique(ctys$NAME)       #explore
unique(NoLatLong$COUNTY)#explore

ctys$cty_upper <- toupper(ctys$NAME)                          #make font-case consistent bw two dfs 
ctys$cty_upper <- sub(" ", "_", ctys$cty_upper)               #fill "spaces" column with "_"
ctys_rename_state<-rename(ctys,STATE=STUSPS)                  #rename column for join
ctys_rename_cty  <-rename(ctys_rename_state,COUNTY=cty_upper) #rename column for join
ctys_rename_XLNG<-rename(ctys_rename_cty,X_LNG_centroid=X)    #rename column for join
ctys_rename_YLAT<-rename(ctys_rename_XLNG,Y_LAT_centroid=Y)   #rename column for join
colnames(ctys_rename_YLAT)                                    #check

x_check<-ctys_rename_YLAT %>% group_by(STATE,COUNTY) %>% tally() %>% ungroup() #shows n=1 for each state-county =  merge wont create dups. 

ctys_RenameToMatchNID<-ctys_rename_YLAT

# Rename counties that still don't match between {TIGRIS} and NID.
## The NID-name is the name listed to the right.
ctys_RenameToMatchNID <- within(ctys_RenameToMatchNID, COUNTY[COUNTY == "DE_SOTO"] <- 'DESOTO')                   
ctys_RenameToMatchNID <- within(ctys_RenameToMatchNID, COUNTY[COUNTY == "ST._CLAIR"] <- 'ST.CLAIR')                
ctys_RenameToMatchNID <- within(ctys_RenameToMatchNID, COUNTY[COUNTY == "EAST_BATON ROUGE"] <- 'EAST_BATON_ROUGE') 
ctys_RenameToMatchNID <- within(ctys_RenameToMatchNID, COUNTY[COUNTY == "SUFFOLK"] <- 'CITY_OF_SUFFOLK')          

colnames(ctys_RenameToMatchNID)

# Selecting only the columns needed
ctys_only_needed_columns<-ctys_RenameToMatchNID %>% 
  dplyr::select(X_LNG_centroid,Y_LAT_centroid,STATE,COUNTY) 

class(ctys_only_needed_columns)
class(NoLatLong)

# Find the rows of the counties data.frame that match the NoLatLong data.frame
merge_to_centroids <- left_join(NoLatLong, ctys_only_needed_columns, by=c("STATE", "COUNTY"))
class(merge_to_centroids)
x<-merge_to_centroids %>% group_by(STATE,COUNTY) %>% tally() %>% ungroup() #explore
x<-merge_to_centroids %>% group_by(NIDID) %>% tally() %>% ungroup()        #explore

## A check on centroid assignments:
##
## x<-merge_to_centroids %>% 
##   dplyr::select(STATE,COUNTY,NIDID,LATITUDE, LONGITUDE,Y_LAT_centroid,X_LNG_centroid,geometry) %>%
##   dplyr::filter(!is.na(Y_LAT_centroid))
## class(x) # .csv's dataclass is not an sf_object yet.
## x_sf <- st_as_sf(x, coords = c("X_LNG_centroid", "Y_LAT_centroid"), crs=4326,remove = FALSE)
## class(x_sf) #check it's an sf_object 
## mapview(x_sf,col.regions="darkslategray3") #view; click a map point for details
##

colnames(merge_to_centroids)
sum(is.na(merge_to_centroids$X_LNG_centroid))#22
sum(is.na(merge_to_centroids$Y_LAT_centroid))#22
sum(is.na(merge_to_centroids$COUNTY))        #22
sum(is.na(merge_to_centroids$LATITUDE))      #242
sum(is.na(merge_to_centroids$LONGITUDE))     #242

class(merge_to_centroids)
class(USA_reservoirs_contig)
merged_centroid_uniques<-left_join(USA_reservoirs_contig,merge_to_centroids) #joins by many columns

x_explore<-merged_centroid_uniques %>% #explore
  group_by(NIDID) %>% 
  dplyr::select(NIDID,DAM_NAME,STATE, COUNTY, CITY, LATITUDE,Y_LAT_centroid, LONGITUDE,X_LNG_centroid) %>% 
  ungroup()

# Where there is NA lat or long, fill with the centroid (Y or X); if there's already a #, leave as-is
USA_reservoirs_lat_long_filled <- merged_centroid_uniques %>% 
  mutate(LATITUDE = case_when(is.na(LATITUDE)  ~ (Y_LAT_centroid), TRUE ~ as.numeric(LATITUDE))) %>% 
  mutate(LONGITUDE = case_when(is.na(LONGITUDE) ~ (X_LNG_centroid), TRUE ~ as.numeric(LONGITUDE)))

x<-USA_reservoirs_lat_long_filled %>% #check 
  arrange(Y_LAT_centroid, STATE, COUNTY) %>% 
  dplyr::select(NIDID, DAM_NAME, STATE, COUNTY, CITY, 
                LATITUDE, Y_LAT_centroid,
                LONGITUDE, X_LNG_centroid) %>% 
  ungroup()





# Manually fill the last 22 that had no COUNTY data. 
sum(is.na(USA_reservoirs_lat_long_filled$LATITUDE)) #22 have no COUNTY data. 

x<-USA_reservoirs_lat_long_filled %>% #view them 
  dplyr::filter(is.na(LATITUDE)) %>% 
  dplyr::select(NIDID,DAM_NAME,STATE, COUNTY, CITY, RIVER,LATITUDE,Y_LAT_centroid, LONGITUDE,X_LNG_centroid)

USA_reservoir_manualfill<-USA_reservoirs_lat_long_filled
 
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LATITUDE[NIDID == "CA00945"] <- '41.076324')                    
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LONGITUDE[NIDID == "CA00945"] <- '-120.630380')
# 41.076324, -120.630380

USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LATITUDE[NIDID == "CA00881"] <- '41.398243')                    
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LONGITUDE[NIDID == "CA00881"] <- '-120.465491')
# 41.398243, -120.465491

USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LATITUDE[NIDID == "SC01232"] <- '34.145555')                    
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LONGITUDE[NIDID == "SC01232"] <- '-82.008130')
# 34.145555, -82.008130

USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LATITUDE[NIDID == "OR04075"] <- '45.852566')                
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LONGITUDE[NIDID == "OR04075"] <- '-119.671812') 
# 45.852566, -119.671812

USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LATITUDE[NIDID == "TN07719"] <- '35.629006')                    
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LONGITUDE[NIDID == "TN07719"] <- '-88.383422')
# 35.629006, -88.383422

USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LATITUDE[NIDID == "UT53236"] <- '37.871833')                    
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LONGITUDE[NIDID == "UT53236"] <- '-111.427125')
# 37.871833, -111.427125

USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LATITUDE[NIDID == "WV06145"] <- '39.608622')                    
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LONGITUDE[NIDID == "WV06145"] <- '-79.966405')
# 39.608622, -79.966405

USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LATITUDE[NIDID == "SD02128"] <- '43.560817')                    
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LONGITUDE[NIDID == "SD02128"] <- '-101.690754')
# 43.560817, -101.690754

USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LATITUDE[NIDID == "UT00040"] <- '41.505934')                    
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LONGITUDE[NIDID == "UT00040"] <- '-111.315895')
# 41.505934, -111.315895

USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LATITUDE[NIDID == "UT00041"] <- '41.505645')                    
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LONGITUDE[NIDID == "UT00041"] <- '-111.326623')
#41.505645, -111.326623

USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LATITUDE[NIDID == "TN12502"] <- '36.580824')                    
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LONGITUDE[NIDID == "TN12502"] <- '-87.482923')
# 36.580824, -87.482923

USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LATITUDE[NIDID == "UT00084"] <- '40.309941')                    
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LONGITUDE[NIDID == "UT00084"] <- '-113.948088')
# 40.309941, -113.948088

USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LATITUDE[NIDID == "WI00235"] <- '44.889685')                    
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LONGITUDE[NIDID == "WI00235"] <- '-88.621925')
# 44.889685, -88.621925

USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LATITUDE[NIDID == "WI00236"] <- '44.888560')  # WI,LegendLakes            
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LONGITUDE[NIDID == "WI00236"] <- '-88.626474')# WI,LegendLakes 
# 44.888560, -88.626474

USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LATITUDE[NIDID == "AZ10450"] <- '32.847525')                    
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LONGITUDE[NIDID == "AZ10450"] <- '-111.482195')
#32.847525, -111.482195

USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LATITUDE[NIDID == "NM00248"] <- '36.693113')                    
USA_reservoir_manualfill <- within(USA_reservoir_manualfill, LONGITUDE[NIDID == "NM00248"] <- '-108.485235')
# 36.693113, -108.485235

# NIDID UT53905 - Not enough info to fill lat-long. SANDWASH L2 Ecoregion determinable: COLD DESERTS
# NIDID UT53910 - Not enough info to fill lat-long. SANDWASH L2 Ecoregion determinable: COLD DESERTS
# NIDID UT53639 - Not enough info to fill lat-long. SANDWASH L2 Ecoregion likely: COLD DESERTS
# NIDID WV04527 - Not enough info to fill lat-long. WV L2 Ecoregion determinable: OZARK/OUACHITA-APPALACHIAN FORESTS 
# NIDID WV04528 - Not enough info to fill lat-long. WV L2 Ecoregion determinable: OZARK/OUACHITA-APPALACHIAN FORESTS 
# NIDID IL55182 - Not enough info to fill lat-long. IL PitchlerParkNatureCenter L2 Ecoregion: CENTRAL USA PLAINS

# These remaining 6 will get Ecoregion assignment further down in script:
x_explore<-USA_reservoir_manualfill %>% 
  dplyr::filter(is.na(LATITUDE)) %>% 
  dplyr::select(NIDID,DAM_NAME,STATE, COUNTY, CITY, RIVER,LATITUDE,Y_LAT_centroid, LONGITUDE,X_LNG_centroid)

sum(is.na(USA_reservoir_manualfill$LATITUDE))
22-16

USA_reservoir_LatLng_ALL<-USA_reservoir_manualfill



# Spatial Join - NID2018 & Omernik ----------------------------------------
## Prep the reservoir data for spatial analysis
## Then read & prep Omernik data.

x_explore<-USA_reservoir_LatLng_ALL %>% group_by(LATITUDE) %>% tally() %>% ungroup() #pre-check 

# Fill NA back to zero for {sf} to work. 
USA_reservoir_LatLng_ALL$LATITUDE <- replace(USA_reservoir_LatLng_ALL$LATITUDE, is.na(USA_reservoir_LatLng_ALL$LATITUDE), 0)#convert
USA_reservoir_LatLng_ALL$LONGITUDE <- replace(USA_reservoir_LatLng_ALL$LONGITUDE, is.na(USA_reservoir_LatLng_ALL$LONGITUDE), 0)#convert

x_explore<-USA_reservoir_LatLng_ALL %>% group_by(LATITUDE) %>% tally() %>% ungroup() #post-check; NA converted to 0. 

# Need to assign CoorRefSystm (CRS). 
class(USA_reservoir_LatLng_ALL) # .csv's dataclass is not an sf_object yet
NID2018_sf <- st_as_sf(USA_reservoir_LatLng_ALL, coords = c("LONGITUDE", "LATITUDE"), crs=4326,remove = FALSE) #CRS: WGS84[4326]
class(NID2018_sf)  # Check it's an sf_object 
st_crs(NID2018_sf) # Check CRS
#plot(NID2018_sf$geometry) 
#options(sf_max.plot=1) 




# Data – Omernik Ecoregions 
OmernickEcoregion <- sf::st_read("data/OmernikEcoregions/NA_CEC_Eco_Level3/NA_CEC_Eco_Level3.shp") 
class(OmernickEcoregion) #.shp's dataclass is an sf_object - good.
OmernickEcoregion_sf <- st_transform(OmernickEcoregion, crs=4326) # Make sure .shp uses same CRS
class(OmernickEcoregion_sf)  # Compare this to DF we're about to join to
st_crs(OmernickEcoregion_sf) # Compare this to DF we're about to join to
#mapview(OmernickEcoregion)  # Visualize; works but takes a while to load
#plot(OmernickEcoregion_shp$geometry) 



# Spatial join: from .shp we want NA_L1NAME & NA_L2NAME columns to join to .csv
sf::sf_use_s2() # Check  

NID2018_sf <- st_make_valid(NID2018_sf)                    # Make sf_objects valid()
OmernickEcoregion_sf <- st_make_valid(OmernickEcoregion_sf)# Make sf_objects valid()

NIDECOjoined <- st_join(NID2018_sf, left = TRUE, OmernickEcoregion_sf[c("NA_L1NAME", "NA_L2NAME", "NA_L3NAME")])
# lefttrue = keeps all. leftfalse = keeps where pt falls in polygon.
# NA_L1NAME = Level 1 Omernike Ecoregion. NA_L2NAME = Level 2 Omernik Ecoregion. NA_L3NAME = Level 3 Omernik Ecoregion. 
# (Above line): Took these 3 Omernik columns and added them to res database; if a res lat-long didn't fall into an ecoregion polygon, it was kept but its ecoregion assigned an NA. Explored all 3 and used Level2 for final analysis; see Methods for details. 


colnames(NIDECOjoined)
x_needs_eco_filling<-NIDECOjoined %>% 
  dplyr::filter(is.na(NA_L2NAME)) %>% 
  dplyr::select(NIDID, NA_L2NAME, DAM_NAME,OTHER_DAM_NAME,STATE,COUNTY,CITY,RIVER, LATITUDE, LONGITUDE) # Explore

sum(is.na(NIDECOjoined$NA_L2NAME)) #27 NA Ecoregions;
sum(is.na(NIDECOjoined$LATITUDE))

# Visualize the 27 reservoir locations that need Ecoregion assignment.
mapview(OmernickEcoregion,col.regions="darkslategray3")+x_needs_eco_filling
unique(x_needs_eco_filling$NIDID)

# Manually assign ecoregions
## Assign ecoregion for NIDIDs that had their lat-long fall on/out of a boundary - resulting in NA. 
eco_filled_sf<-NIDECOjoined
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "TX07533"] <- 'TEXAS-LOUISIANA COASTAL PLAIN')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "WA00406"] <- 'MARINE WEST COAST FOREST')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "TX03687"] <- 'TEXAS-LOUISIANA COASTAL PLAIN')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "FL28002"] <- 'MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "FL00616"] <- 'MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "FL00093"] <- 'MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "FL00037"] <- 'MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "VA131006"] <- 'MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "VA001004"] <- 'MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "WV03707"] <- 'MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "WV03702"] <- 'MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "MD00284"] <- 'MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "MD00019"] <- 'MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "WV00310"] <- 'MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "NY01509"] <- 'MIXED WOOD PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "NY01502"] <- 'MIXED WOOD PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "NY13632"] <- 'MIXED WOOD PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "NY16102"] <- 'MIXED WOOD PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "MA01092"] <- 'MIXED WOOD PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "MA01088"] <- 'MIXED WOOD PLAINS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "CA10261"] <- 'MEDITERRANEAN CALIFORNIA')
sum(is.na(eco_filled_sf$NA_L2NAME)) #These are NA Ecoregions because they are NA Lat-Long. 

# Assign ecoregion for NIDID with missing lat-long; too little info to ID coord., but can determine its L2 Ecoregion.
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "UT53905"] <- 'COLD DESERTS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "UT53910"] <- 'COLD DESERTS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "UT53639"] <- 'COLD DESERTS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "WV04527"] <- 'OZARK/OUACHITA-APPALACHIAN FORESTS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "WV04528"] <- 'OZARK/OUACHITA-APPALACHIAN FORESTS')
eco_filled_sf <- within(eco_filled_sf, NA_L2NAME[NIDID == "IL55182"] <- 'CENTRAL USA PLAINS')

sum(is.na(eco_filled_sf$NA_L2NAME)) # 0 NA Ecoregions 

class(eco_filled_sf)
eco_filled<-as_tibble(eco_filled_sf) # No longer an sf_object
class(eco_filled) 


# Convert Units -----------------------------------------------------------

# CASE_WHEN: if the LEFT condition is met/true (then ~) do RIGHT side thing
# Convert NID MAX_DISCHARGE cf/s to cm/s
# Convert NID NID_STORAGE a*f to m^3
# Convert NID SURFACE_AREA acres to ha -- note: NA still exist & are managed further below. 

NID_units_converted <- eco_filled %>% 
  mutate( max_discharge_cms      = case_when(!is.na(MAX_DISCHARGE) ~ (MAX_DISCHARGE*0.0283168))) %>%  
  mutate( nid_storage_cm         = case_when(!is.na(NID_STORAGE)   ~ (NID_STORAGE*1233.48))) %>%  
  mutate( surface_area_ha_HAS_NA = case_when(!is.na(SURFACE_AREA)  ~ (SURFACE_AREA*0.404686)))


# Surface Area N/As ------------------------------------------------------
## The Problem: ~20K reservoirs have N/A for surface area. This means that in the last step of the calculations (script 3), multiplying biomass (kg/ha) * SA (ha) for the (kg) loses ~20K rows of data. Not able to establish a trend b/w Surface Area and Drainage Area to fix this. The Solution: Fill in those N/A with a conservative S.A. estimate using mean(log()) ("bin mode"). See SI methods.


# ~ S.A. Histogram ---------------------------------------------------------------
## Histogram exploring surface area trends by ecoregion.
 
fBasics::basicStats(NID_units_converted$surface_area_ha_HAS_NA) # basic surface area (SA) stats. 
fBasics::basicStats(log10(NID_units_converted$surface_area_ha_HAS_NA))
mean(log10(NID_units_converted$surface_area_ha_HAS_NA),na.rm=TRUE) #mean(log(x)) surface area (for all regions)

NID_units_converted %>% 
  dplyr::filter(  !(is.na(NA_L2NAME))
                & !(surface_area_ha_HAS_NA == 0))%>% 
  ggplot(aes(x=log10(surface_area_ha_HAS_NA)))+
  #geom_histogram(aes(y=..count../sum(..count..)),bins=40,fill="palegreen",color="palegreen3") + #deprecated in ggplot2 3.4.0
  geom_histogram(aes(y=after_stat(count)/sum(after_stat(count))),bins=40,fill="palegreen",color="palegreen3") +
  facet_wrap(~NA_L2NAME, ncol=4,labeller=label_wrap_gen(width=29,multi_line = TRUE)) +
  scale_x_continuous(breaks = seq(-3.4, 7, 2))+
  #scale_y_continuous(breaks = seq(0, 0.2, 0.1), limits=c(0, 0.2))+
  theme_bw()+
  theme(axis.title.x=element_text(size=17), 
        axis.title.y=element_text(size=17), 
        axis.text.x =element_text(size=14,angle = 0),
        axis.text.y =element_text(size=14),
        legend.position = "none",
        strip.text = element_text(size=13),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

# Mean(Log(x))-Per-Ecoregion, table ----------------------------------
## Table of values to explore.
## 1. log(SA)
## 2. then ID median() and mean()
## 3. then backtransform. 

fBasics::basicStats(NID_units_converted$surface_area_ha_HAS_NA)
sum(is.na(NID_units_converted$NA_L2NAME))
class(NID_units_converted)

x_listed_SA_per_ecoregion<- NID_units_converted %>% 
  dplyr::filter( (!is.na(surface_area_ha_HAS_NA))
                 &(!surface_area_ha_HAS_NA == 0)) %>% 
  dplyr::group_by(NA_L2NAME) %>% 
  dplyr::summarise(n=n(),
                   median_logged = median(log10(surface_area_ha_HAS_NA)),
                   mean_logged   = mean(  log10(surface_area_ha_HAS_NA))) %>% 
  dplyr::mutate(median = (10^(median_logged)),
                mean   = (10^(mean_logged))) %>%
  ungroup()  


# S.A. fill NA with mean(log()) ------------------------------------------------
## 1) Give Ecoregions with NA surface area the mean(log(x)) of it's Ecoregion.
## 2) For NA-Ecoregion, do mean-of-all-regions combined. (There are none since we filled in manually.)

sum(is.na(NID_units_converted$NA_L2NAME)) # Zero reservoirs have no assigned ecoregion; were filled manually.
10^(mean(log10(NID_units_converted$surface_area_ha_HAS_NA),na.rm=TRUE)) # 6.462131 = mean surface area (for all regions) 
Ecoregion_NAs <- NID_units_converted %>% # Create separate DFs for NA-Ecoregions vs with-Ecoregions
  dplyr::filter(is.na(NA_L2NAME)) %>% 
  mutate(surface_area_ha = case_when(is.na(surface_area_ha_HAS_NA) ~ (6.462131), 
                                     TRUE ~ as.numeric(surface_area_ha_HAS_NA))) 
##Explore this DF
## colnames(Ecoregion_NAs)
## Map_NA_Ecos<-Ecoregion_NAs %>% 
##   select("NIDID", "LONGITUDE", "LATITUDE", "STATE", "COUNTY", 
##          "RIVER", "YEAR_COMPLETED", "DAM_NAME", "OTHER_DAM_NAME", "DAM_FORMER_NAME")
## mapview(OmernickEcoregion,col.regions="darkslategray3")+Map_NA_Ecos

Ecoregion_fill_blankSA_with_mean <- NID_units_converted %>% # Convert N/A SA's to its mean(log()) for respective ecoregion. 
  group_by(NA_L2NAME) %>%  
  dplyr::filter(!is.na(NA_L2NAME)) %>% 
  mutate(surface_area_ha = case_when(is.na(surface_area_ha_HAS_NA) ~ (10^(mean(log10(surface_area_ha_HAS_NA),na.rm=TRUE))),
                                     TRUE ~ as.numeric(surface_area_ha_HAS_NA))) %>% 
  ungroup() #log10 doesn't produce INF bc zeros were converted to NA earlier on

class(Ecoregion_NAs)
class(Ecoregion_fill_blankSA_with_mean)

Filled_Ecoregions_All <- rbind(Ecoregion_fill_blankSA_with_mean,Ecoregion_NAs)

x_check_SA_assignments<-Filled_Ecoregions_All %>% 
  select(NIDID, DAM_NAME, NA_L2NAME,surface_area_ha_HAS_NA,surface_area_ha) %>% 
  arrange(desc(surface_area_ha_HAS_NA),NA_L2NAME) %>% 
  ungroup()



write_csv(Filled_Ecoregions_All, "data output/01_NID_Omernik_unitsconverted.csv") #use in script 02




