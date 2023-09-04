## ---------------------------
## Author: Christine A. Parisek, caparisek@ucdavis.edu
## Figure 3 - State Rank Plot 
## ---------------------------

library(tidyverse)  # General Use 
library(cowplot)    # Stitch plots together
library(fBasics)    # Optional; basic summary stats
library(ggbreak)    # View PanelB error bars better since all bars are >500 
options(scipen=999) # Optional; removes sci-notation


# Read in .csv that is the output from Gam5 predicted 1993 calculation. 
# Note: TSC (kg), Biomass (kg/ha)

ROYL_NS <- read_csv("data output/03_USA_GAM5_fit.csv", col_types = cols(STATEID = col_character(),
                                                                     URL_ADDRESS = col_character(),
                                                                     Notes = col_character())) 

# Add column with full state name for label

unique(ROYL_NS$STATE) # List states
ROYL<-ROYL_NS %>%
  mutate(State_fullname= case_when(
    STATE == "NE" ~ "Nebraska",
    STATE == "OK" ~ "Oklahoma",
    STATE == "TX" ~ "Texas",
    STATE == "KS" ~ "Kansas",
    STATE == "WY" ~ "Wyoming",
    STATE == "CO" ~ "Colorado",
    STATE == "NM" ~ "New Mexico",
    STATE == "MO" ~ "Missouri",
    STATE == "IA" ~ "Iowa",
    STATE == "MN" ~ "Minnesota",
    STATE == "WI" ~ "Wisconsin",
    STATE == "SD" ~ "South Dakota",
    STATE == "ND" ~ "North Dakota",
    STATE == "IL" ~ "Illinois",
    STATE == "IN" ~ "Indiana",
    STATE == "KY" ~ "Kentucky", 
    STATE == "MD" ~ "Maryland", 
    STATE == "NJ" ~ "New Jersey", 
    STATE == "NY" ~ "New York", 
    STATE == "WV" ~ "West Virginia", 
    STATE == "DE" ~ "Delaware", 
    STATE == "VA" ~ "Virginia", 
    STATE == "PA" ~ "Pennsylvania", 
    STATE == "NC" ~ "North Carolina",
    STATE == "AL" ~ "Alabama",
    STATE == "GA" ~ "Georgia", 
    STATE == "SC" ~ "South Carolina", 
    STATE == "LA" ~ "Louisiana", 
    STATE == "FL" ~ "Florida", 
    STATE == "MS" ~ "Mississippi", 
    STATE == "TN" ~ "Tennessee", 
    STATE == "AR" ~ "Arkansas", 
    STATE == "OH" ~ "Ohio", 
    STATE == "MI" ~ "Michigan", 
    STATE == "CA" ~ "California", 
    STATE == "NV" ~ "Nevada",
    STATE == "AZ" ~ "Arizona",
    STATE == "UT" ~ "Utah", 
    STATE == "WA" ~ "Washington", 
    STATE == "OR" ~ "Oregon", 
    STATE == "ID" ~ "Idaho", 
    STATE == "MT" ~ "Montana", 
    STATE == "MA" ~ "Massachusetts", 
    STATE == "RI" ~ "Rhode Island", 
    STATE == "NH" ~ "New Hampshire", 
    STATE == "CT" ~ "Connecticut", 
    STATE == "ME" ~ "Maine", 
    STATE == "VT" ~ "Vermont"))


colnames(ROYL)                        #explore
sum(is.na(ROYL$STATE))                #explore
sum(is.na(ROYL$gam_calc5_L2ecoXarea)) #explore



# Fig 3A - summed kg/ha by state --------------------------------------

# Take sum of GAM5-1993's TSC (kg), by STATE. "X area" in "gam_calc5_L2ecoXarea" means it was already multiplied by surface area (ha); units are (kg). 

sumbystate<-ROYL %>% 
  dplyr::group_by(STATE) %>%
  mutate(sumSTATE = sum(gam_calc5_L2ecoXarea,na.rm=TRUE)) %>% 
  ungroup()

# Get one row per state
STATE <-sumbystate[!duplicated(sumbystate$STATE), ] 

x<-STATE %>% dplyr::select(STATE,sumSTATE) #explore
basicStats(STATE$sumSTATE)          #explore

394000000/1000000 

STATE1 <- STATE %>% 
  mutate(sumSTATEb = sumSTATE/1000000) #div by 1,000,000 (million) to make y-axis more readable

x<-STATE1 %>% dplyr::select(STATE,sumSTATE,sumSTATEb) #explore; comparing the 2 new columns. 

sum(STATE1$sumSTATE)         #explore; checking yes, this sums up to Table's TSC (kg) for USA Calc5.
basicStats(STATE1$sumSTATEb) #explore; this helps with ggplot scale_x/y limit setting 



# Fig 3, Panel A
#### --- (A) plot ----

a<-ggplot(STATE1, aes(x=reorder(State_fullname,-sumSTATEb,sum), y=(sumSTATEb))) + 
  geom_bar(stat="identity",width=0.70,fill="royalblue4") + 
  coord_flip()+
  scale_y_continuous(expand = expansion(add = c(0, 0)), breaks=seq(0,400,100), limits=c(0,420)) +
  theme_bw()+
  ylab(label = "Total Standing Stock (million kg)")+
  xlab(label = "State")+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,angle=45,hjust=1),
        axis.title.x = element_text(size=14,vjust=1.4,
                                    margin= unit(c(top=0.7, right=0, bottom=0, left=0),"cm")),
        axis.title.y = element_text(size=14,vjust=1.4),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.margin = unit(c(top=0.5, right=0, bottom=0.2, left=0.2),"cm"),
        panel.border = element_blank())




# Fig 3B - mean kg/ha by state, w error bars -----------------------------

# Take MEAN of GAM5-1993's biomass (kg/ha), by STATE. 
# get mean and its SEM error bars

State_Mean_Biomass <- ROYL %>%
  dplyr::mutate(LOG_gam_meth5_L2ecosize = log10(gam_meth5_L2ecosize)) %>% 
  dplyr::group_by(State_fullname) %>%
  dplyr::summarize(mean_by_state_biomass_kgha = mean(gam_meth5_L2ecosize,na.rm=TRUE),
                   se_of_mean_by_state_biomass_kgha  = sd(gam_meth5_L2ecosize)/sqrt(n()),na.rm=TRUE,
                   LOG_mean_by_state_biomass_kgha = mean(LOG_gam_meth5_L2ecosize,na.rm=TRUE),
                   LOG_se_of_mean_by_state_biomass_kgha  = sd(LOG_gam_meth5_L2ecosize)/sqrt(n()),na.rm=TRUE) %>%
  ungroup()

basicStats(State_Mean_Biomass$mean_by_state_biomass_kgha)     #explore
sum(State_Mean_Biomass$mean_by_state_biomass_kgha)            #explore

basicStats(State_Mean_Biomass$LOG_mean_by_state_biomass_kgha) #explore
sum(State_Mean_Biomass$LOG_mean_by_state_biomass_kgha)        #explore



# (B) plot  ---------------------------------------------------------------
# mean kg/ha by state, w error bars

b<-ggplot(State_Mean_Biomass, aes(y=reorder(State_fullname,-mean_by_state_biomass_kgha,sum), 
                               x=(mean_by_state_biomass_kgha))) + 
  geom_bar(stat="identity",width=0.70, fill="royalblue4") + 
  geom_errorbar(aes(xmin=mean_by_state_biomass_kgha - se_of_mean_by_state_biomass_kgha, 
                    xmax=mean_by_state_biomass_kgha + se_of_mean_by_state_biomass_kgha), 
                width=.08, color="black",alpha=0.9, position = position_dodge(0.5))+ 
  scale_x_continuous(expand = expansion(add = c(0,0)), breaks=seq(0,650,50),limits = c(0,630)) +
  ggbreak::scale_x_break(breaks=c(10, 500), 
                         scales = "fixed", 
                         space = 0.01)+
  annotate("segment", color="black",
              x = c(9.0, 493.5), 
           xend = c(10.5, 495.0), 
              y = c(0.2, 0.2), 
           yend = c(0.7, 0.7))+
  labs( y = "State", x = expression(paste("Biomass"~('kg'~ha^-1))))+
  #xlab(label = "Mean Biomass (kg/ha)")+
  #ylab(label = "State")+ 
  theme_bw()+
  theme(axis.ticks.y = element_blank(),
        axis.text.x = element_text(size=12,angle=45,hjust=1),
        axis.text.y = element_text(size=12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=14),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank())




# Stitch plots ------------------------------------------------
bprint<-print(b) #yep, need to do this
cowplot::plot_grid(a, bprint, labels = c('A', 'B'),
          label_size = 16,
          label_x = 0.935, label_y = 0.965,
          hjust = -0.5, vjust = -0.5,
          ncol=2)

ggsave("figures/Fig_3_state_rank.tiff", height=10, width=10, units="in", dpi=900)



