### script to run nlme model on all of the metabolism data (from 2021-2023)
## taken from 2122 script and updated to include all new data as well

## NOTE -- USING ONLY THE MAIN FLOW SEASON (cropped 22-23 data to match 21-22 data, going to the end of Jan, 1/22 - 1/27 ish )

## my data may not be normally distributed -- tried a various methods to normalize (log, sqrt, cubroot)
## glmm models can be used for non normally distributed over lme -- maybe use this instead?? Added the code 

# -- setup----
library(ggplot2)
library(nlme)
library(dplyr)
library(knitr)
library(lubridate)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##input and output location for files
drive<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/'
drive1<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/2122 Model Outputs/'
drive2<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/2223 Model Outputs/'
outdrive<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/Mixed effects modeling/'

####### DATA WAS ALREADY CREATED USING MODEL OUTPUTS IN UPDATED_METAB_DATA.R To get daily avgs from inst values and combine all 
###### streams together etc. 
##### Read in that updated data here 

## Read in 21-22 and 22-23 Daily avg data
data21<-read.csv(paste0(drive, 'All_2122Metab_Data.csv'))
data21$Season<-'2021-2022'
data22<-read.csv(paste0(drive, 'All_2223Metab_Data.csv'))
data22$Season<-'2022-2023'

Alldata<-full_join(data21, data22)

rm(data21)
rm(data22)

##### TRY THIS WITHOUT LOST SEAL #################
# Alldata<-Alldata%>%
#   dplyr::filter(., !Stream=='Lost Seal')

## read in the PAR for each season
## 21-22
PAR1<-read.csv(paste0(drive, '2021_2022_data_ATW/2021_2022_FryxPAR.csv'), stringsAsFactors = FALSE)
PAR1$DateTime<-paste(PAR1$DateTime, '00:00:00')
PAR1$DateTime<-substr(PAR1$DateTime, 1, 19)
PAR1$DateTime<-lubridate::ymd_hms(PAR1$DateTime)
PAR1$Season<-'2021-2022'
PAR1<-PAR1%>%dplyr::rename(., PAR=par)

library(padr)
##22-23 
PAR2<-read.csv(paste0(drive, '2022_2023_data/FRLM_pressure_PAR.csv'), stringsAsFactors=FALSE)
PAR2$DateTime<-lubridate::mdy_hm(PAR2$TIMESTAMP)
##pad the PAR data out
PAR2<-pad(PAR2, interval="15 min")
PAR2$PAR<-zoo::na.approx(PAR2$PAR, na.rm = TRUE) ## don't want to add data on either end, so keep leading and ending NAs
PAR2$Season<-'2022-2023'

PAR<-full_join(PAR1, PAR2)
PAR<-PAR%>%
  dplyr::group_by(date=as.Date(DateTime))%>%
  dplyr::summarise(., PAR_avg=mean(PAR, na.rm=TRUE), PAR_max=max(PAR, na.rm=TRUE), Season=Season)
PAR$PAR_max[PAR$PAR_max=='-Inf']<-NA
PAR$PAR_avg[PAR$PAR_avg=='NaN']<-NA

PARseas1<-PAR1%>%
  dplyr::summarise(., PAR_avg=mean(PAR, na.rm=TRUE), Season=Season)
PARseas2<-PAR2%>%
  dplyr::summarise(., PAR_avg=mean(PAR, na.rm=TRUE), Season=Season)
PARseas<-full_join(PARseas1, PARseas2)

rm(PARseas1)
rm(PARseas2)
rm(PAR1)
rm(PAR2)

##Combine PAR data with other data
Alldata$date<-as.Date(Alldata$date)
Alldata<-full_join(Alldata, PAR)
Alldata<-Alldata%>%dplyr::filter(., !is.na(GPP_daily_mean)&!is.na(ER_daily_mean))

rm(PAR)

Daydata<-gather(Alldata, parameter, value, "Q_avg", "Q_max", "Q_min", "Q_skewness", "Temp_avg", "Temp_max", "PAR_avg", "PAR_max", "EC_avg")

# Dayplot<-ggplot(Daydata, aes(x=value, y=GPP_daily_mean, color=parameter, group=Stream))+geom_point()+
#   geom_smooth(method = "lm")+
#   facet_wrap(parameter~., scales="free")+
#   theme(axis.text.x = element_text(angle = 90))+
#   theme(legend.position="none")+
#   theme(text = element_text(size = 30))
# # ggsave(Dayplot, filename="All_GPP_dailyAvg_regressions.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 15)

##Read in 21-22 data ## FULL SEASON DATA
### this is using data that covers the main flow season so it can be compared between streams
# Seas21<-read.csv(paste0(drive, 'Alldata2122_MainFlowSeason.csv'))
# Seas21$Season<-'2021-2022'
# Seas22<-read.csv(paste0(drive, 'Alldata2223_MainFlowSeason.csv'))
# Seas22$Season<-'2022-2023'
# 
# Season<-full_join(Seas21, Seas22)
# Season<-full_join(Season, PARseas)
# Season<-distinct(Season)
# 
# rm(PARseas)
# rm(Seas21)
# rm(Seas22)

###read in the 21-22 nutrient datasets from Kathy 
library(readxl)
library(stringr)
St_Nut<-read_excel(paste0('/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/MDV streams/Streams NUTS 2021-22.xlsx'), sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
St_Nut$date<-as.Date(St_Nut$Date)
St_Nut<-St_Nut%>%
  dplyr::rename(., `NO3 mg/L N`=`NO3 by difference`)
St_Nut$Season<-'2021-2022'

St_DOC<-read_excel(paste0('/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/MDV streams/2021-22 DOC streams.xlsx'), sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
St_DOC$date<-as.Date(St_DOC$DateTime)
St_DOC$Season<-'2021-2022'

##Calculate avgs as well to use for seasonal data below
Nuts_avg<-St_Nut%>%
  dplyr::group_by(Stream)%>%
  summarise(., NH4_avg=mean(`NH4 mg/L N`, na.rm=TRUE), NO3_avg=mean(`NO3 mg/L N`, na.rm=TRUE), SRP_avg=mean(`SRP mg/L`, na.rm=TRUE), NO2_avg=mean(`NO2 mg/L N`, na.rm=TRUE), Season=Season)
Nuts_avg$`N_mM`<-((Nuts_avg$NH4_avg + Nuts_avg$NO3_avg + Nuts_avg$NO2_avg) /14.01)
Nuts_avg$`SRP_mM`<-(Nuts_avg$SRP_avg /30.973)
Nuts_avg$NP_avg<-(Nuts_avg$N_mM / Nuts_avg$SRP_mM)

DOC_avg<-St_DOC%>%
  dplyr::group_by(Stream)%>%
  summarise(., DOC_avg=mean(`NPOC mg/L`, na.rm=TRUE), Season=Season)

Daydata$Stream[Daydata$Stream=='Vonguerard']<-'Von Guerard'

##Do the same thing for the 22-23 nutrient data 
St_Nut2<-read_excel(paste0(drive, '2022_2023_data/Fryxell stream nutrients 2022-23.xlsx'), sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
St_Nut2$date<-lubridate::mdy(St_Nut2$Date)
## conver from µg /L to mg/L
St_Nut2$`NO3 mg/L N`<-(St_Nut2$`Nitrate + nitrite - Results [µg N/liter]`/1000)
St_Nut2$`NH4 mg/L N`<-(St_Nut2$`NH4 µg N/L`/1000)
St_Nut2$`SRP mg/L`<-(St_Nut2$`SRP µg P/L`/1000)
St_Nut2$Season<-'2022-2023'

St_DOC2<-read_excel(paste0('/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/MDV streams/Stream NPOC 2022-23.xlsx'), sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
St_DOC2$date<-lubridate::ymd(St_DOC2$`Date Collected`)
St_DOC2$Season<-'2022-2023'

##Calculate avgs as well to use for seasonal data below
Nuts_avg2<-St_Nut2%>%
  dplyr::group_by(Stream)%>%
  summarise(., NH4_avg=mean(`NH4 mg/L N`, na.rm=TRUE), NO3_avg=mean(`NO3 mg/L N`, na.rm=TRUE), SRP_avg=mean(`SRP mg/L`, na.rm=TRUE), Season=Season)
Nuts_avg2$`N_mM`<-((Nuts_avg2$NH4_avg + Nuts_avg2$NO3_avg) /14.01)
Nuts_avg2$`SRP_mM`<-(Nuts_avg2$SRP_avg /30.973)
Nuts_avg2$NP_avg<-(Nuts_avg2$N_mM / Nuts_avg2$SRP_mM)

St_Nut<-St_Nut%>%
  dplyr::select(., `Stream`, `date`, `SRP mg/L`, `NH4 mg/L N`, `NO3 mg/L N`, `NO2 mg/L N`, `Season`)
## calculate the N:P ratio
St_Nut$`N_mM`<-(St_Nut$`NH4 mg/L N` + St_Nut$`NO3 mg/L N` + St_Nut$`NO2 mg/L N` /14.01)
St_Nut$`SRP_mM`<-(St_Nut$`SRP mg/L` /30.973)
St_Nut$NP<-(St_Nut$N_mM / St_Nut$SRP_mM)

St_Nut2<-St_Nut2%>%
  dplyr::select(., `Stream`, `date`, `SRP mg/L`, `NH4 mg/L N`, `NO3 mg/L N`, `Season`)
St_Nut2$`NO2 mg/L N`<-NA
St_Nut2$`N_mM`<-(St_Nut2$`NH4 mg/L N` + St_Nut2$`NO3 mg/L N`/14.01)
St_Nut2$`SRP_mM`<-(St_Nut2$`SRP mg/L` /30.973)
St_Nut2$NP<-(St_Nut2$N_mM / St_Nut2$SRP_mM)

St_DOC2<-St_DOC2%>%dplyr::rename(., `DOC mg/L`=`[DOC]mg/L (data pasted as values)`)
St_DOC<-St_DOC%>%dplyr::rename(., `DOC mg/L`=`NPOC mg/L`)

DOC_avg2<-St_DOC2%>%
  dplyr::group_by(Stream)%>%
  summarise(., DOC_avg=mean(`DOC mg/L`, na.rm=TRUE), Season=Season)

AllNut<-full_join(St_Nut, St_Nut2)
## Also calculate total avgs 
Nut_tot<-AllNut%>%
  dplyr::group_by(Stream)%>%
  summarise(., NH4_avg=mean(`NH4 mg/L N`, na.rm=TRUE), NO3_avg=mean(`NO3 mg/L N`, na.rm=TRUE), NO2_avg=mean(`NO2 mg/L N`, na.rm=TRUE), SRP_avg=mean(`SRP mg/L`, na.rm=TRUE))
AllDOC<-full_join(St_DOC, St_DOC2)
DOC_tot<-AllDOC%>%
  dplyr::group_by(Stream)%>%
  summarise(., DOC_avg=mean(`DOC mg/L`, na.rm=TRUE))

AllChem<-full_join(AllNut, AllDOC)
Alldata<-full_join(Alldata, AllChem)
Alldata<-Alldata%>%
  dplyr::select(., Stream, date, `SRP mg/L`, `NH4 mg/L N`, `NO3 mg/L N`, `NO2 mg/L N`, NP, `DOC mg/L`, `GPP_daily_mean`, `ER_daily_mean`, `Q_max`, `Q_avg`, `Q_skewness`, `Temp_avg`,
                `EC_avg`, `length`,`Season`, `PAR_avg`, `PAR_max`, `Season`)
##Take out Onyx rn
Alldata<-Alldata%>%
  dplyr::filter(., Stream=='Commonwealth'|Stream=="Green"|Stream=='Canada'|Stream=='Von Guerard'|
                  Stream=='Aiken'|Stream=='Delta'|Stream=='Harnish'|Stream=='Crescent'|Stream=='Huey'|Stream=='Lost Seal')

##calculate the P/R ratio for each day
Alldata$`P/R`<-(Alldata$GPP_daily_mean / abs(Alldata$ER_daily_mean))

Alldata<-Alldata%>%
  dplyr::filter(., !is.na(GPP_daily_mean&is.na(ER_daily_mean)))

Alldata<-distinct(Alldata)

rm(St_Nut)
rm(St_Nut2)
rm(St_DOC)
rm(St_DOC2)
rm(AllNut)

library(ggpmisc)
library(tidyr)
### Plot GPP against PAR
# PG<-ggplot(Alldata, aes(x=PAR_avg, y=GPP_daily_mean, color=Stream, group=Stream))+geom_point()+
#   stat_poly_line() +
#   stat_poly_eq(use_label(c("P")), size=10)+
#   theme(text = element_text(size=35))+
#   theme(legend.position="bottom")+
#   ylab('Daily average GPP (g O2/m2/day)')+
#   xlab('')
# ggsave(PG, filename="GPPPAR_each.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 15)

##plot the physical and bgc params for each one to make sure all looks okay 
DayPhys<-gather(Alldata, parameter, value, 'Q_avg', 'Q_max', 'Q_skewness', 'Temp_avg', 'PAR_avg', 'PAR_max')
DayBGC<-gather(Alldata, parameter, value, 'SRP mg/L', 'NH4 mg/L N', 'NO3 mg/L N', 'NP', 'DOC mg/L', 'EC_avg')

GPPPhys<-ggplot(DayPhys, aes(x=value, y=GPP_daily_mean, color=Stream, group=Stream))+geom_point()+
  facet_wrap(`parameter`~ ., scales = "free", labeller = label_wrap_gen(10))+
  stat_poly_line() +
  stat_poly_eq(use_label(c("P")), size=10)+
  theme(text = element_text(size=35))+
  theme(legend.position="bottom")+
  ylab('Daily average GPP (g O2/m2/day)')+
  xlab('')
ggsave(GPPPhys, filename="GPPphysTemporal.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 15)

GPPBGC<-ggplot(DayBGC, aes(x=value, y=GPP_daily_mean, color=Stream, group=Stream))+geom_point()+
  facet_wrap(`parameter`~ ., scales = "free", labeller = label_wrap_gen(10))+
  stat_poly_line() +
  stat_poly_eq(use_label(c("P")), size=10)+
  theme(text = element_text(size=35))+
  theme(legend.position="bottom")+
  ylab('Daily average GPP (g O2/m2/day)')+
  xlab('')
ggsave(GPPBGC, filename="GPPbgcTemporal.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 15)

DayBGC<-gather(Alldata, parameter, value, 'SRP mg/L', 'NH4 mg/L N', 'NO3 mg/L N', 'NP', 'DOC mg/L', 'GPP_daily_mean')

ERPhys<-ggplot(DayPhys, aes(x=value, y=ER_daily_mean, color=Stream, group=Stream))+geom_point()+
  facet_wrap(`parameter`~ ., scales = "free", labeller = label_wrap_gen(10))+
  stat_poly_line() +
  stat_poly_eq(use_label(c("P")), size=10)+
  theme(text = element_text(size=35))+
  theme(legend.position="bottom")+
  ylab('Daily average ER (g O2/m2/day)')+
  xlab('')
ggsave(ERPhys, filename="ERphysTemporal.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 15)

ERBGC<-ggplot(DayBGC, aes(x=value, y=ER_daily_mean,color=Stream, group=Stream))+geom_point()+
  facet_wrap(`parameter`~ ., scales = "free", labeller = label_wrap_gen(10))+
  stat_poly_line() +
  stat_poly_eq(use_label(c("P")), size=10)+
  theme(text = element_text(size=40))+
  theme(legend.position="bottom")+
  ylab('Daily average ER (g O2/m2/day)')+
  xlab('')
ggsave(ERBGC, filename="ERbgcTemporal.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 15)

### GPP ~ ER relationship 
ERGPP<-ggplot(Alldata, aes(x=GPP_daily_mean, y=ER_daily_mean, color=Season))+geom_point(size=2)+
  stat_poly_line() +
  stat_poly_eq(use_label(c("P")), size=15, label.x.npc = "right")+
  theme(text = element_text(size=40))+
  theme(legend.position="bottom")+
  ylab(expression(Average~ER~(g~O[2]/m^2/day)))+
  xlab(expression(Average~GPP~(g~O[2]/m^2/day)))
ggsave(ERGPP, filename="ERGPP_temp.jpeg", device="jpeg", path=paste0(outdrive), width = 12, height = 12)


rm(DayPhys)
rm(DayBGC)

##Calculate the seasonal average (for spatial analysis between different streams across the entire season)
# AllAvg<-DayAll%>%
#   group_by(Stream)%>%
#   summarise(GPP=mean(GPP_avg, na.rm=TRUE), ER=mean(ER_avg, na.rm=TRUE), discharge_avg=mean(discharge, na.rm=TRUE), temp_avg=mean(temp_avg, na.rm=TRUE), PAR_avg=mean(PAR_avg, na.rm=TRUE))
# Season$`P/R`<-(Season$GPP_avg / abs(Season$ER_avg))
##combine with stream nutrient and DOC averages from above
All_Nuts<-full_join(Nuts_avg, Nuts_avg2)
# Seas<-full_join(Season, All_Nuts)
# Seas<-Seas%>%
#   dplyr::select(., Stream, Season, GPP_avg, ER_avg, PR_daily_avg, PR_seas_avg, NH4_avg, NO3_avg, SRP_avg, NP_avg, PAR_avg, Temp_avg, Q_avg, length, EC_avg, 
#                   dist_coast, Avg_black_percent_coverage,  Avg_orange_percent_coverage, Avg_black_biomass_Kg,       
#                   Avg_orange_Biomass_kg, Avg_biomass_density_kgm2, Avg_total_biomass_kg)
All_DOC<-full_join(DOC_avg, DOC_avg2)
# Seas<-full_join(Seas, All_DOC)
# Seas<-distinct(Seas)
# Seas<-Seas%>%
#   dplyr::filter(., Stream=='Commonwealth'|Stream=="Green"|Stream=='Lost Seal'|Stream=='Canada'|Stream=='Von Guerard'|
#                   Stream=='Aiken'|Stream=='Delta'|Stream=='Harnish'|Stream=='Crescent'|Stream=='Huey')

# rm(Season)
# rm(DOC_avg)
# rm(Nuts_avg)
# rm(Nuts_avg2)

### want to calculate the average between both seasons and use that
# ### A few different ways to to do this, the first is to take the averaegs of each season and the average that (average of an average???)
# Total<-Seas%>%
#   dplyr::group_by(Stream)%>%
#   dplyr::summarise(., GPP_avg=mean(GPP_avg, na.rm=TRUE), ER_avg=mean(ER_avg, na.rm=TRUE),PR_daily_avg=mean(PR_daily_avg, na.rm=TRUE), PR_seas_avg=mean(PR_seas_avg, na.rm=TRUE),
#                    NH4_avg=mean(NH4_avg, na.rm=TRUE), NO3_avg=mean(NO3_avg, na.rm=TRUE), SRP_avg=mean(SRP_avg, na.rm=TRUE),
#                    DOC_avg=mean(DOC_avg, na.rm=TRUE), Temp_avg=mean(Temp_avg, na.rm=TRUE), NP_avg=mean(NP_avg, na.rm=TRUE),
#                    Q_avg=mean(Q_avg, na.rm=TRUE), EC_avg=mean(EC_avg, na.rm=TRUE), length=mean(length, na.rm=TRUE), dist_coast=mean(dist_coast, na.rm=TRUE),
#                    Avg_black_percent_coverage=mean(Avg_black_percent_coverage, na.rm=TRUE),  Avg_orange_percent_coverage=mean(Avg_orange_percent_coverage, na.rm=TRUE), Avg_black_biomass_Kg=mean(Avg_black_biomass_Kg, na.rm=TRUE),
#                    Avg_orange_Biomass_kg=mean(Avg_orange_Biomass_kg, na.rm=TRUE), Avg_biomass_density_kgm2=mean(Avg_biomass_density_kgm2, na.rm=TRUE), Avg_total_biomass_kg=mean(Avg_total_biomass_kg, na.rm=TRUE))
# Total<-distinct(Total)

# #### The second is to take all of the main flow data for each season, and then just overall averages from there
### WE WANT TO USE DAILY P/R VALUES 
### This total data is from Mark's estimates from unmixing model 
# Total<-read.csv(paste0(drive, 'Alldata_BothSeasons.csv'))
### or can do data just from the main flow season
# # Total<-read.csv(paste0(drive, 'Alldata_MainFlowSeason.csv'))
# Total<-full_join(Total, Nut_tot)
# Total<-full_join(Total, DOC_tot)
# Total$NP_avg<-((Total$NH4_avg+Total$NO3_avg+Total$NO2_avg) / Total$SRP_avg)
# Total<-Total%>%
#   dplyr::select(., Stream, GPP_avg, GPP_med, GPP_max, ER_avg, ER_med, ER_max, PR_seas, PR_daily_avg, PR_daily_med, PR_daily_max, PR_seas_avg, NH4_avg, NO3_avg, NO2_avg, SRP_avg, NP_avg, Temp_avg, Q_avg, DOC_avg, length, EC_avg, 
#                 dist_coast, Avg_black_percent_coverage,  Avg_orange_percent_coverage, Avg_black_biomass_Kg,       
#                 Avg_orange_Biomass_kg, Avg_biomass_density_kgm2, Avg_biomass_density_wMoss, Avg_total_biomass_kg, Avg_total_biomass_wMoss)

### TO INCLUDE THE ALGAE OPS DATA READ FROM HERE AND COMMENT OUT ABOVE
Total<-read.csv(paste0(drive, 'Alldata_BothSeasons_wAlgOps.csv'))
# Total<-read.csv(paste0(drive, 'Alldata_MainFlowSeason.csv'))
Total<-full_join(Total, Nut_tot)
DOC_tot<-distinct(DOC_tot)
Total<-full_join(Total, DOC_tot)
Total$NP_avg<-((Total$NH4_avg+Total$NO3_avg+Total$NO2_avg) / Total$SRP_avg)
Total<-Total%>%
  dplyr::select(., Stream, GPP_avg, GPP_med, GPP_max, ER_avg, ER_med, ER_max, PR_daily_avg, PR_daily_med, PR_daily_max, NH4_avg, NO3_avg, NO2_avg, SRP_avg, NP_avg, Temp_avg, Q_avg, Qskew_avg, Qskewness, DOC_avg, length, width, EC_avg, 
                EC_med,dist_coast, Avg_black_coverage,  Avg_orange_coverage, Avg_black_area_m2, Avg_orange_area_m2, Avg_black_bio_gm2,
                Avg_orange_bio_gm2, Est_black_biomass_g, Est_orange_biomass_g, Est_total_bio_g, low_flow, LowFlow_Prop)


Total<-distinct(Total)
Total<-Total%>%
  dplyr::filter(., Stream=='Commonwealth'|Stream=="Green"|Stream=='Canada'|Stream=='Von Guerard'|
                  Stream=='Aiken'|Stream=='Delta'|Stream=='Harnish'|Stream=='Crescent'|Stream=='Huey'|Stream=='Lost Seal')
Total<-distinct(Total)

##plot the physical and bgc params for each one to make sure all looks okay 
SeasPhys<-gather(Total, parameter, value, 'Q_avg', 'Temp_avg', 'length', 'width', 'EC_avg', 'EC_med', 'dist_coast', 'low_flow', 'LowFlow_Prop')
SeasBGC<-gather(Total, parameter, value, 'SRP_avg', 'NH4_avg', 'NO3_avg', 'NP_avg', 'DOC_avg')
Seasbio<-gather(Total, parameter, value, "Avg_black_coverage", "Avg_orange_coverage","Avg_black_area_m2", "Avg_orange_area_m2",        
                "Avg_black_bio_gm2", "Avg_orange_bio_gm2", "Est_black_biomass_g", "Est_orange_biomass_g", "Est_total_bio_g")

GPPseasPhys<-ggplot(SeasPhys, aes(x=value, y=GPP_avg))+geom_point()+
  facet_wrap(`parameter`~ ., scales = "free_x", labeller = label_wrap_gen(10), strip.position="bottom")+
  stat_poly_line() +
  stat_poly_eq(use_label(c("P", "R2")), size=8, digits = 2)+
  theme(text = element_text(size=35))+
  theme(legend.position="bottom")+
  ylab('Average GPP (g O2/m2/day)')+
  xlab('')
  # ylim(0, 1.5)
ggsave(GPPseasPhys, filename="GPPphysSpat_Total.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 17)

GPPseasBGC<-ggplot(SeasBGC, aes(x=value, y=GPP_avg))+geom_point()+
  facet_wrap(`parameter`~ ., scales = "free_x", labeller = label_wrap_gen(10))+
  stat_poly_line() +
  stat_poly_eq(use_label(c("P", "R2")), size=8)+
  theme(text = element_text(size=35))+
  theme(legend.position="bottom")+
  ylab('Average GPP (g O2/m2/day)')+
  xlab('')
ggsave(GPPseasBGC, filename="GPPbgcSpat_Total.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 15)

GPPseasBIO<-ggplot(Seasbio, aes(x=value, y=GPP_avg))+geom_point()+
  facet_wrap(`parameter`~ ., scales = "free_x", labeller = label_wrap_gen(10))+
  stat_poly_line() +
  stat_poly_eq(use_label(c("P", "R2")), size=8)+
  theme(text = element_text(size=30))+
  theme(legend.position="bottom")+
  ylab('Average GPP (g O2/m2/day)')+
  xlab('')
ggsave(GPPseasBIO, filename="GPPbioSpat_Total.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 15)


SeasBGC<-gather(Total, parameter, value, 'SRP_avg', 'NH4_avg', 'NO3_avg', 'NP_avg', 'DOC_avg', 'GPP_avg')

ERseasPhys<-ggplot(SeasPhys, aes(x=value, y=ER_avg))+geom_point()+
  facet_wrap(`parameter`~ ., scales = "free_x", labeller = label_wrap_gen(10))+
  stat_poly_line() +
  stat_poly_eq(use_label(c("P", "R2")), size=8)+
  theme(text = element_text(size=35))+
  theme(legend.position="bottom")+
  ylab('Average ER (g O2/m2/day)')+
  xlab('')
ggsave(ERseasPhys, filename="ERphysSpat_Total.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 15)


ERseasBGC<-ggplot(SeasBGC, aes(x=value, y=ER_avg))+geom_point()+
  facet_wrap(`parameter`~ ., scales = "free_x", labeller = label_wrap_gen(10), strip.position="bottom")+
  stat_poly_line() +
  stat_poly_eq(use_label(c("P", "R2")), size=8)+
  theme(text = element_text(size=35))+
  theme(legend.position="bottom")+
  ylab('Average ER (g O2/m2/day)')+
  xlab('')
ggsave(ERseasBGC, filename="ERbgcSpat_Total.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 12)

# ERseasBIO<-ggplot(Seasbio, aes(x=value, y=ER_avg))+geom_point()+
#   facet_wrap(`parameter`~ ., scales = "free", labeller = label_wrap_gen(10))+
#   stat_poly_line() +
#   stat_poly_eq(use_label(c("P", "R2")), size=8)+
#   theme(text = element_text(size=30))+
#   theme(legend.position="bottom")+
#   ylab('Average ER (g O2/m2/day)')+
#   xlab('')
# ggsave(ERseasBIO, filename="ERbioSpat_Total.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 15)
# 

### ER to GPP
ERsGPP<-ggplot(Total, aes(x=GPP_avg, y=ER_avg))+geom_point(size=2)+
  stat_poly_line() +
  stat_poly_eq(use_label(c("P", "R2")), size=8)+
  theme(text = element_text(size=40))+
  theme(legend.position="bottom")+
  ylab('Average season ER')+
  xlab('Average season GPP')
ggsave(ERsGPP, filename="ERGPP_spat.jpeg", device="jpeg", path=paste0(outdrive), width = 12, height = 12)

### P/R ratio seasonal 
### USE DAILY P/R AVERAGE 
PRseasPhys<-ggplot(SeasPhys, aes(x=value, y=PR_daily_med))+geom_point()+
  facet_wrap(`parameter`~ ., scales = "free_x", labeller = label_wrap_gen(10))+
  stat_poly_line() +
  stat_poly_eq(use_label(c("P", "R2")), size=8)+
  theme(text = element_text(size=35))+
  theme(legend.position="bottom")+
  ylab('Median GPP:ER')+
  xlab('')
ggsave(PRseasPhys, filename="PRseasPhys.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 15)

PRseasBGC<-ggplot(SeasBGC, aes(x=value, y=PR_daily_med))+geom_point()+
  facet_wrap(`parameter`~ ., scales = "free_x", labeller = label_wrap_gen(10))+
  stat_poly_line() +
  stat_poly_eq(use_label(c("P", "R2")), size=8)+
  theme(text = element_text(size=35))+
  theme(legend.position="bottom")+
  ylab('Median GPP:ER')+
  xlab('')
ggsave(PRseasBGC, filename="PRseasBGC.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 15)

PRseasBIO<-ggplot(Seasbio, aes(x=value, y=PR_daily_med))+geom_point()+geom_smooth(method=lm)+
  facet_wrap(`parameter`~ ., scales = "free_x", labeller = label_wrap_gen(7), strip.position="bottom")+
  stat_poly_line() +
  stat_poly_eq(use_label(c("P", "R2")), size=8)+
  theme(text = element_text(size=30))+
  theme(legend.position="bottom")+
  ylab('Median daily GPP:ER')+
  xlab('')
ggsave(PRseasBIO, filename="PRmedBio_Total_AlgOPs.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 15)

### look at biomass relationship with Q
QseasBIO<-ggplot(Seasbio, aes(x=value, y=Q_avg))+geom_point()+geom_smooth(method=lm)+
  facet_wrap(`parameter`~ ., scales = "free", labeller = label_wrap_gen(7))+
  stat_poly_line() +
  stat_poly_eq(use_label(c("P", "R2")), size=8)+
  theme(text = element_text(size=30))+
  theme(legend.position="bottom")+
  ylab('Average daily Q')+
  xlab('')
# ggsave(PRseasBIO, filename="PRmedBio_Total_AlgOPs.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 15)

###plot the signficant ones individually for the paper
GPP_Qplot<-ggplot(Total, aes(x=Q_avg, y=GPP_avg))+geom_point(size=5)+
  stat_poly_line(se=TRUE) +
  stat_poly_eq(use_label(c("P", "R2")), size=20)+
  theme(text = element_text(size=50))+
  theme(legend.position="bottom")+
  ylab('Average Daily GPP')+
  xlab('Average Daily Q')
  # ylim(0, 2)
ggsave(GPP_Qplot, filename="GPP_Q_plot.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 10)

GPP_Fplot<-ggplot(Total, aes(x=low_flow, y=GPP_avg))+geom_point(size=5)+
  stat_poly_line(se=TRUE) +
  stat_poly_eq(use_label(c("P", "R2")), size=20)+
  theme(text = element_text(size=50))+
  theme(legend.position="bottom")+
  ylab('Average Daily GPP')+
  xlab('# Low Flow Days')
# ylim(0, 2)
ggsave(GPP_Fplot, filename="GPP_LowFlow_plot.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 10)

GPP_ECplot<-ggplot(Total, aes(x=EC_avg, y=GPP_avg))+geom_point(size=5)+
  stat_poly_line(se=TRUE) +
  stat_poly_eq(use_label(c("P", "R2")), size=20)+
  theme(text = element_text(size=50))+
  theme(legend.position="bottom")+
  ylab('Average Daily GPP')+
  xlab('Average Daily EC (μS/cm)')
  # ylim(0, 2)
ggsave(GPP_ECplot, filename="GPP_EC_plot.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 10)

ER_ECplot<-ggplot(Total, aes(x=EC_avg, y=ER_avg))+geom_point(size=5)+
  stat_poly_line(se=TRUE) +
  stat_poly_eq(use_label(c("P", "R2")), size=20)+
  theme(text = element_text(size=50))+
  theme(legend.position="bottom")+
  ylab('Average Daily ER')+
  xlab('Average Daily EC (μS/cm)')
  # ylim(-1.5, 0)
ggsave(ER_ECplot, filename="ER_EC_plot.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 10)

ER_GPPplot<-ggplot(Total, aes(x=GPP_avg, y=ER_avg))+geom_point(size=5)+
  stat_poly_line(se=TRUE) +
  stat_poly_eq(use_label(c("P", "R2")), size=20)+
  theme(text = element_text(size=50))+
  theme(legend.position="bottom")+
  ylab('Average Daily ER')+
  xlab('Average Daily GPP')
  # ylim(-1.5, 0)
ggsave(ER_GPPplot, filename="ER_GPP_plot.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 10)

ER_Tplot<-ggplot(Total, aes(x=Temp_avg, y=ER_avg))+geom_point(size=5)+
  stat_poly_line(se=TRUE) +
  stat_poly_eq(use_label(c("P", "R2")), size=20)+
  theme(text = element_text(size=50))+
  theme(legend.position="bottom")+
  ylab('Average Daily ER')+
  xlab('Average Daily Water Temp')
# ylim(-1.5, 0)
ggsave(ER_Tplot, filename="ER_Temp_plot.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 10)

PR_plot<-ggplot(Total, aes(x=low_flow, y=PR_daily_med))+geom_point(size=5)+
  stat_poly_line(se=FALSE) +
  stat_poly_eq(use_label(c("P", "R2")), size=20)+
  theme(text = element_text(size=50))+
  theme(legend.position="bottom")+
  ylab('Average Daily P:R')+
  xlab('Number of Low Flow Days')
  # ylim(0, 2)
ggsave(PR_plot, filename="PR_lowflow_plot.jpeg", device="jpeg", path=paste0(outdrive), width = 15, height = 15)

PRall<-gather(Total, parameter, value, 'low_flow', "Avg_black_coverage","Avg_black_area_m2", "Est_total_bio_g")

PRseas<-ggplot(PRall, aes(x=value, y=PR_daily_med))+geom_point(size=3)+
  facet_wrap(`parameter`~ ., scales = "free_x", labeller = label_wrap_gen(7), strip.position="bottom", nrow=2)+
  stat_poly_line(se=TRUE) +
  stat_poly_eq(use_label(c("P", "R2")), size=8)+
  theme(text = element_text(size=35))+
  theme(legend.position="bottom")+
  ylab('Median daily P:R')+
  xlab('')
ggsave(PRseas, filename="PR_all.jpeg", device="jpeg", path=paste0(outdrive), width = 20, height = 15)

##Create correlation matrix
Phys<-Total%>%select(., GPP_avg, ER_avg, PR_daily_med, Q_avg, Temp_avg, length, width, EC_avg, dist_coast, low_flow)

BGC<-Total%>%select(., GPP_avg, ER_avg, PR_daily_med, SRP_avg, NH4_avg, NO3_avg, NP_avg, DOC_avg)

Bio<-Total%>%select(., GPP_avg, ER_avg, PR_daily_med, Avg_black_area_m2, Avg_black_coverage,Est_total_bio_g)
# Total2<-Total%>%select(., )
# numeric_Total <- Total[sapply(Total, is.numeric)]
Physcor_matrix<-cor(Phys, use="pairwise.complete.obs")
print(Physcor_matrix)
library(ggcorrplot)
PhysCorr<-ggcorrplot(Physcor_matrix, lab=TRUE, type="lower")
ggsave(PhysCorr, filename="PhysCorrelationMatrix.jpeg", device="jpeg", path=paste0(outdrive))

BGCcor_matrix<-cor(BGC, use="pairwise.complete.obs")
print(BGCcor_matrix)
BGCCorr<-ggcorrplot(BGCcor_matrix, lab=TRUE, type="lower")
ggsave(BGCCorr, filename="BGCCorrelationMatrix.jpeg", device="jpeg", path=paste0(outdrive))

Biocor_matrix<-cor(Bio, use="pairwise.complete.obs")
print(Biocor_matrix)
BioCorr<-ggcorrplot(Biocor_matrix, lab=TRUE, type="lower")
ggsave(BioCorr, filename="BioCorrelationMatrix.jpeg", device="jpeg", path=paste0(outdrive))

PhGPP<-lm(GPP_avg ~ Q_avg+Temp_avg+length+width+EC_avg+dist_coast+low_flow, data=Total)
summary(PhGPP)
PhER<-lm(ER_avg ~ GPP_avg*Q_avg, data=Total)
summary(PhER)

BGCGPP<-lm(GPP_avg ~ SRP_avg + NH4_avg + NO3_avg + NP_avg +DOC_avg, data=Total)
summary(BGCGPP)
BGCER<-lm(ER_avg ~ GPP_avg+SRP_avg + NH4_avg + NO3_avg + NP_avg +DOC_avg, data=Total)
summary(BGCER)

BioGPP<-lm(GPP_avg~Avg_black_coverage + Avg_orange_coverage +Avg_black_area_m2+Avg_orange_area_m2+       
          Avg_black_bio_gm2+Avg_orange_bio_gm2+Est_black_biomass_g, data=Total)
BioER<-lm(GPP_avg~Avg_black_coverage + Avg_orange_coverage +Avg_black_area_m2+Avg_orange_area_m2+       
            Avg_black_bio_gm2+Avg_orange_bio_gm2+Est_black_biomass_g, data=Total)
BioPR<-lm(PR_daily_med~Avg_black_coverage +Avg_black_area_m2+      
            Avg_black_bio_gm2+Est_black_biomass_g, data=Total)
summary(BioGPP)
summary(BioER)
summary(BioPR)

### REMOVE ALL LARGE DATASETS AT THIS POINT TO GET R TO RUN SMOOTHER
rm(SeasBGC)
rm(SeasPhys)

########################################################################################################################
### run lme model with ER and GPP as dependent, Q, temp, length, DOC, Nut conc and ratios, and PAR as independent
### lme(model, data, fixed, random, groups, start, correlation, weights, subset, method, na.action, naPattern, control, verbose)
## input Stream and date as random effects 
### Or can use lm for models without random effects - just looking at the effect of one variable on another

##check distribution of residuals with shapiro.test(residuals())
##or look a the hist (residuals(F9_lme)) and see if it is approximately normally distributed around 0 
Alldata$month<-lubridate::month(Alldata$date)
### Run for both temporal variation (over daily avgs) and spatial variation (differences between streams for seasonal avgs)

library(lme4)
library(lmerTest)
library(flexplot)
library(afex)
library(tidymodels)
library(tidyverse)
library(car)

##run all of the streams together, looking at avg GPP rate vs. avg Q rate, stream length, avg Nuts, N:P, DOC, avg temp
## Want to get one average value for each stream
##################################### TEMPORAL VARIATION ##############################################################
##Scale discharge so that it is comparable across all streams if grouping by stream
Alldata<-Alldata %>% 
  group_by(Stream) %>% 
  mutate(Q_z = scale(Q_avg), PAR_z=scale(PAR_avg), EC_z=scale(EC_avg), Temp_z=scale(Temp_avg))
# ## do other physical parameters together? 
## not by stream
GPPday_phys1<-lm(GPP_daily_mean ~PAR_z+Q_z+Temp_z, data=Alldata) ##log(Q)  to get on same scales
summary(GPPday_phys1)
## look at each individual stream relationship
# GPPday_phys2<-lm(GPP_daily_mean ~(Temp_avg+Q_z+PAR_avg+EC_avg)*Stream, data=Alldata) ##log(Q)  to get on same scales
# summary(GPPday_phys2)
#### each stream has individual relationship
GPPday_phys<-lmer(GPP_daily_mean ~PAR_z+Q_z+Temp_z+ (1+Q_z+PAR_z+Temp_z||Stream),data=Alldata) ##log(Q)  to get on same scales
# GPPday_phys1<-lmer(GPP_daily_mean ~PAR_avg+Q_z+Temp_avg+EC_avg+ (1|Stream),data=Alldata) ##log(Q)  to get on same scales
# GPPday_phys2<-lmer(GPP_daily_mean ~PAR_avg+Q_z+Temp_avg+EC_avg+ (Q_z|Stream),data=Alldata) ##log(Q)  to get on same scales

summary(GPPday_phys)
summary(GPPday_phys)$coefficients
ranef(GPPday_phys)$Stream + fixef(GPPday_phys)
anova(GPPday_phys, GPPday_phys2)

# Alldata$predicted_GPP <- predict(GPPday_phys, newdata = Alldata)
# 
# # Plot GPP vs. PAR_avg, grouped by Stream
# ggplot(Alldata, aes(x = PAR_avg, y = GPP_daily_mean, color = Stream)) +
#   geom_point() +  # Observed data points
#   geom_line(aes(y = predicted_GPP), size = 1) +  # Predicted GPP from the model
#   facet_wrap(~Stream) +  # Facet by stream
#   labs(title = "GPP vs PAR_avg by Stream", x = "PAR_avg", y = "GPP") +
#   theme_minimal()

### Plot each individual stream's relationship in one table
library(purrr)
GPPphysAll <- Alldata %>%
  group_by(Stream) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(GPP_daily_mean ~ Temp_avg + Q_z + EC_avg + PAR_avg, data = .x)),
    tidied = map(model, broom::tidy)
  ) %>%
  unnest(tidied)
library(flextable)
ft <- GPPphysAll %>%
  select(Stream, term, estimate, std.error, statistic, p.value) %>%
  flextable() %>%
  autofit()
# Create Word doc and add table
library(officer)
doc <- read_docx() %>%
  body_add_par("Stream-by-Stream Linear Model Results", style = "heading 1") %>%
  body_add_flextable(ft)
# Save to file
print(doc, target ='Mixed effects modeling/GPPday_phys_indiv.docx')


# ###biogeochemical parameters together
GPPday_bgc1<-lm(GPP_daily_mean ~(`DOC mg/L`+`NO3 mg/L N`+`NH4 mg/L N`+`SRP mg/L`+NP), data=Alldata) ##log(Q)  to get on same scales
summary(GPPday_bgc1)
## look at each individual stream relationship
# GPPday_bgc2<-lm(GPP_daily_mean ~(`DOC mg/L`+`NO3 mg/L N`+`NH4 mg/L N`+`SRP mg/L`+NP)*Stream, data=Alldata) ##log(Q)  to get on same scales
# summary(GPPday_bgc2)

GPPday_bgc<-lmer(GPP_daily_mean ~(`DOC mg/L`+`NO3 mg/L N`+`NH4 mg/L N`+`SRP mg/L`+NP)+ (`DOC mg/L`+`NO3 mg/L N`+`NH4 mg/L N`+`SRP mg/L`+NP||Stream), data=Alldata) ##log(Q)  to get on same scales
summary(GPPday_bgc)
emtrends(GPPday_bgc, ~ Stream, var = "DOC mg/L")

anova(GPPday_bgc, GPPday_bgc1)

library(purrr)
GPPbgcAll <- Alldata %>%
  group_by(Stream) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(GPP_daily_mean ~ (`DOC mg/L`+`NO3 mg/L N`+`NH4 mg/L N`+`SRP mg/L`+NP), data = .x)),
    tidied = map(model, broom::tidy)
  ) %>%
  unnest(tidied)
library(flextable)
ft <- GPPbgcAll %>%
  select(Stream, term, estimate, std.error, statistic, p.value) %>%
  flextable() %>%
  autofit()
# Create Word doc and add table
library(officer)
doc <- read_docx() %>%
  body_add_par("Stream-by-Stream Linear Model Results", style = "heading 1") %>%
  body_add_flextable(ft)
# Save to file
print(doc, target ='Mixed effects modeling/GPPday_bgc_indiv.docx')

# 
# ##plot them and put into tables
sjPlot::tab_model(GPPday_phys1, show.ci=FALSE, file='Mixed effects modeling/GPPday_phys_all.doc')
sjPlot:: tab_model(GPPday_phys, show.ci=FALSE, file='Mixed effects modeling/GPPday_phys_byStream.doc')
# sjPlot::tab_model(GPPday_phys2, show.ci=FALSE, file='Mixed effects modeling/GPPday_phys_individ.doc')
sjPlot:: tab_model(GPPday_bgc1, show.ci=FALSE, file='Mixed effects modeling/GPPday_bgc_all.doc')
sjPlot:: tab_model(GPPday_bgc, show.ci=FALSE, file='Mixed effects modeling/GPPday_bgc_byStream.doc')
# sjPlot:: tab_model(GPPday_bgc2, show.ci=FALSE, file='Mixed effects modeling/GPPday_bgc_indivd.doc')

###write the individual beta values out for each stream 
library(officer)
library(flextable)
# Combine fixed effects and random effects for each stream
stream_betas <- ranef(GPPday_phys)$Stream + fixef(GPPday_phys)
stream_betas_df <- as.data.frame(stream_betas)
stream_betas_df$Stream <- rownames(stream_betas)
stream_betas_df <- stream_betas_df[, c("Stream", setdiff(names(stream_betas_df), "Stream"))]
doc <- read_docx()
doc <- body_add_par(doc, "GPPP_StreamPhys_Betas", style = "heading 1")
ft <- flextable(stream_betas_df)
doc <- body_add_flextable(doc, ft)
print(doc, target = "streamGPPP_phys_betas.docx")

stream_betas <- ranef(GPPday_bgc)$Stream + fixef(GPPday_bgc)
stream_betas_df <- as.data.frame(stream_betas)
stream_betas_df$Stream <- rownames(stream_betas)
stream_betas_df <- stream_betas_df[, c("Stream", setdiff(names(stream_betas_df), "Stream"))]
doc <- read_docx()
doc <- body_add_par(doc, "GPPP_StreamBgc_Betas", style = "heading 1")
ft <- flextable(stream_betas_df)
doc <- body_add_flextable(doc, ft)
print(doc, target = "streamGPPP_bgc_betas.docx")

### same thing for ER
# ## do other physical parameters together? 
## not by stream
ERday_phys1<-lm(ER_daily_mean ~GPP_daily_mean+Temp_z+Q_z+PAR_z, data=Alldata) ##log(Q)  to get on same scales
summary(ERday_phys1)
## look at each individual stream relationship
# ERday_phys2<-lm(ER_daily_mean ~(GPP_daily_mean+Temp_avg+Q_z+PAR_avg+EC_avg)*Stream, data=Alldata) ##log(Q)  to get on same scales
# summary(ERday_phys2)
#### each stream has individual relationship
ERday_phys<-lmer(ER_daily_mean ~GPP_daily_mean+Temp_z+Q_z+PAR_z+ (1+GPP_daily_mean+Q_z+PAR_z+Temp_z||Stream),data=Alldata) ##log(Q)  to get on same scales
summary(ERday_phys)
anova(ERday_phys1, ERday_phys)

ERphysAll <- Alldata %>%
  group_by(Stream) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(ER_daily_mean ~ (GPP_daily_mean+Temp_avg+Q_z+PAR_avg+EC_avg), data = .x)),
    tidied = map(model, broom::tidy)
  ) %>%
  unnest(tidied)
library(flextable)
ft <- ERphysAll %>%
  select(Stream, term, estimate, std.error, statistic, p.value) %>%
  flextable() %>%
  autofit()
# Create Word doc and add table
library(officer)
doc <- read_docx() %>%
  body_add_par("Stream-by-Stream Linear Model Results", style = "heading 1") %>%
  body_add_flextable(ft)
# Save to file
print(doc, target ='Mixed effects modeling/ERday_phys_indiv.docx')


# ###biogeochemical parameters together
ERday_bgc1<-lm(ER_daily_mean ~(`DOC mg/L`+`NO3 mg/L N`+`NH4 mg/L N`+`SRP mg/L`+NP), data=Alldata) ##log(Q)  to get on same scales
summary(ERday_bgc1)
## look at each individual stream relationship
# ERday_bgc2<-lm(ER_daily_mean ~(`DOC mg/L`+`NO3 mg/L N`+`NH4 mg/L N`+`SRP mg/L`+NP)*Stream, data=Alldata) ##log(Q)  to get on same scales
# summary(ERday_bgc2)

ERday_bgc<-lmer(ER_daily_mean ~(`DOC mg/L`+`NO3 mg/L N`+`NH4 mg/L N`+`SRP mg/L`+NP)+ (`DOC mg/L`+`NO3 mg/L N`+`NH4 mg/L N`+`SRP mg/L`+NP||Stream), data=Alldata) ##log(Q)  to get on same scales
summary(ERday_bgc)
emtrends(ERday_bgc, ~ Stream, var = "DOC mg/L")

anova(ERday_bgc, ERday_bgc1)

ERbgcAll <- Alldata %>%
  group_by(Stream) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(ER_daily_mean ~ (`DOC mg/L`+`NO3 mg/L N`+`NH4 mg/L N`+`SRP mg/L`+NP), data = .x)),
    tidied = map(model, broom::tidy)
  ) %>%
  unnest(tidied)
library(flextable)
ft <- ERbgcAll %>%
  select(Stream, term, estimate, std.error, statistic, p.value) %>%
  flextable() %>%
  autofit()
# Create Word doc and add table
library(officer)
doc <- read_docx() %>%
  body_add_par("Stream-by-Stream Linear Model Results", style = "heading 1") %>%
  body_add_flextable(ft)
# Save to file
print(doc, target ='Mixed effects modeling/ERday_bgc_indiv.docx')


# 
# ##plot them and put into tables
sjPlot::tab_model(ERday_phys1, show.ci=FALSE, file='Mixed effects modeling/ERday_phys_all.doc')
sjPlot:: tab_model(ERday_phys, show.ci=FALSE, file='Mixed effects modeling/ERday_phys_byStream.doc')
# sjPlot::tab_model(ERday_phys2, show.ci=FALSE, file='Mixed effects modeling/ERday_phys_individ.doc')
sjPlot:: tab_model(ERday_bgc1, show.ci=FALSE, file='Mixed effects modeling/ERday_bgc_all.doc')
sjPlot:: tab_model(ERday_bgc, show.ci=FALSE, file='Mixed effects modeling/ERday_bgc_byStream.doc')
# sjPlot:: tab_model(ERday_bgc2, show.ci=FALSE, file='Mixed effects modeling/ERday_bgc_indivd.doc')

stream_betas <- ranef(ERday_bgc)$Stream + fixef(ERday_bgc)
stream_betas_df <- as.data.frame(stream_betas)
stream_betas_df$Stream <- rownames(stream_betas)
stream_betas_df <- stream_betas_df[, c("Stream", setdiff(names(stream_betas_df), "Stream"))]
doc <- read_docx()
doc <- body_add_par(doc, "ERP_StreamBgc_Betas", style = "heading 1")
ft <- flextable(stream_betas_df)
doc <- body_add_flextable(doc, ft)
print(doc, target = "streamERP_bgc_betas.docx")

stream_betas <- ranef(ERday_phys)$Stream + fixef(ERday_phys)
stream_betas_df <- as.data.frame(stream_betas)
stream_betas_df$Stream <- rownames(stream_betas)
stream_betas_df <- stream_betas_df[, c("Stream", setdiff(names(stream_betas_df), "Stream"))]
doc <- read_docx()
doc <- body_add_par(doc, "ERP_StreamPhys_Betas", style = "heading 1")
ft <- flextable(stream_betas_df)
doc <- body_add_flextable(doc, ft)
print(doc, target = "streamERP_phys_betas.docx")

#######################################################################################
##run them each individually
## to compare between different types of models! Figure out which type of model is best for our data 
# GPPbase<-lmer(GPP_daily_mean~1 + (1|Stream), data=Alldata) ## this is the baseline model, only looking at all GPP data
# icc(GPPbase) ## the icc shows you how much of the variabnce is due to clustering -- want this as small as possible, high icc = need to use lme
# visualize(GPPbase, plot="model")
# GPPfixslo<-lmer(GPP_daily_mean~Temp_avg + (1|Stream), data=Alldata) ## add in fixed slope, random intercept
# icc(GPPfixslo)
# GPPrandslo<-lmer(GPP_daily_mean~Temp_avg + (Temp_avg|Stream), data=Alldata) ### add in random slope and random intercept
# icc(GPPrandslo) ## this is the lowest icc yet -- use this one!
# ### compare between them
# compare.fits(GPP_daily_mean~Temp_avg|Stream, data=Alldata, GPPfixslo, GPPrandslo)
# model.comparison(GPPfixslo, GPPrandslo) ## randslo has much higher bayes.factor -- choose this one! 

## Do each random effect seperately since they are not related to each other -- random slope and random intercept, grouped by stream
GPP_P<-lm(GPP_daily_mean ~Q_avg, data=Alldata) ##log(Q)  to get on same scales

GPP_P<-lmer(GPP_daily_mean ~PAR_avg+EC_avg+ (1|Stream), data=Alldata) ##log(Q)  to get on same scales
summary(GPP_P)
library(emmeans)
result<-emtrends(GPP_P, ~Stream, var="PAR_avg", infer=TRUE)
summary(result)

Alldata$PAR_z <- scale(Alldata$PAR_avg)
GPP_P2<-lmer(GPP_daily_mean ~PAR_z + (PAR_z|Stream), data=Alldata) ##log(Q)  to get on same scales
summary(GPP_P)
anova(GPP_P, GPP_P2)
jpeg(file=paste0(outdrive, 'GPP_PAR_plot.jpeg'))
visualize(GPP_P, plot="model")
dev.off()
sjPlot:: tab_model(GPP_P, show.ci=FALSE, digits=4, file='Mixed effects modeling/GPPday_PAR.doc')
qqnorm(resid(GPP_P))
qqline(resid(GPP_P))
shapiro.test(resid(GPP_P))

Alldata$Temp_avg[Alldata$Temp_avg=='-Inf']<-NA
GPP_T<-lmer(GPP_daily_mean ~Temp_avg+ (1|Stream), data=Alldata) 
Alldata$Temp_z <- scale(Alldata$Temp_avg)
GPP_T2<-lmer(GPP_daily_mean ~Temp_z+ (Temp_z|Stream), data=Alldata) 
summary(GPP_T2)
anova(GPP_T, GPP_T2)
GPP_T<-lmer(GPP_daily_mean ~Temp_avg+Q_avg+PAR_avg+(1|Stream), data=Alldata)  ### add in Q to remove variance from Q 
summary(GPP_T2)
Anova(GPP_T)
# hist(residuals(GPP_T))
jpeg(file=paste0(outdrive, 'GPP_temp_plot.jpeg'))
visualize(GPP_T, plot="model", sample=10)
dev.off()
sjPlot:: tab_model(GPP_T, show.ci=FALSE, file='Mixed effects modeling/GPP_Temp_lme_wQPAR.doc')
qqnorm(resid(GPP_T))
qqline(resid(GPP_T))
shapiro.test(resid(GPP_T))

GPP_Q<-lmer(GPP_daily_mean ~Q_z+PAR_avg+Temp_avg+EC_avg+ (1|Stream), data=Alldata) 
summary(GPP_Q)
GPP_Q2<-lmer(GPP_daily_mean ~Q_z+ (1+Q_z|Stream), data=Alldata) 
summary(GPP_Q2)
anova(GPP_Q, GPP_Q2)
jpeg(file=paste0(outdrive, 'GPP_Q_plot.jpeg'))
visualize(GPP_Q, plot="model", sample=10)
dev.off()
sjPlot:: tab_model(GPP_Q, show.ci=FALSE, digits=4,file='Mixed effects modeling/GPP_Q_lme.doc')
qqnorm(resid(GPP_Q))
qqline(resid(GPP_Q))
shapiro.test(resid(GPP_Q))

GPP_Qskew<-lmer(GPP_daily_mean ~Q_skewness+ (1|Stream), data=Alldata) 
summary(GPP_Qskew)
jpeg(file=paste0(outdrive, 'GPP_Q_plot.jpeg'))
visualize(GPP_Qskew, plot="model", sample=10)
dev.off()
sjPlot:: tab_model(GPP_Qskew, show.ci=FALSE, digits=4,file='Mixed effects modeling/GPP_Q_lme.doc')
qqnorm(resid(GPP_Qskew))
qqline(resid(GPP_Qskew))
shapiro.test(resid(GPP_Qskew))

GPP_D<-lmer(GPP_daily_mean ~`DOC mg/L`+ (1|Stream), data=Alldata)  ##these resid are normally distributed! in log-log space
summary(GPP_D)
# jpeg(file=paste0(outdrive, 'GPP_DOC_plot.jpeg'))
# visualize(GPP_Q, plot="model", sample=10)
# dev.off()
sjPlot:: tab_model(GPP_D, show.ci=FALSE, file='Mixed effects modeling/GPP_DOC_lme.doc')
qqnorm(resid(GPP_D))
qqline(resid(GPP_D))
shapiro.test(resid(GPP_D))

GPP_NO3<-lmer(GPP_daily_mean ~`NO3 mg/L N` + (1|Stream), data=Alldata)  ##these resid are normally distributed! in log-log space
summary(GPP_NO3)
# jpeg(file=paste0(outdrive, 'GPP_NO3_plot.jpeg'))
# visualize(GPP_NO3, plot="model", sample=10)
# dev.off()
sjPlot:: tab_model(GPP_NO3, show.ci=FALSE, file='Mixed effects modeling/GPP_NO3_lme.doc')
qqnorm(resid(GPP_NO3))
qqline(resid(GPP_NO3))
shapiro.test(resid(GPP_NO3))

GPP_NH4<-lmer(GPP_daily_mean ~`NH4 mg/L N` + (1|Stream), data=Alldata)  ##these resid are normally distributed! in log-log space
summary(GPP_NH4)
# jpeg(file=paste0(outdrive, 'GPP_NH4_plot.jpeg'))
# visualize(GPP_NH4, plot="model", sample=10)
# dev.off()
sjPlot:: tab_model(GPP_NH4, show.ci=FALSE, file='Mixed effects modeling/GPP_NH4_lme.doc')
qqnorm(resid(GPP_NH4))
qqline(resid(GPP_NH4))
shapiro.test(resid(GPP_NH4))

GPP_SRP<-lmer(GPP_daily_mean ~`SRP mg/L` + (1|Stream), data=Alldata)  ##these resid are normally distributed! in log-log space
summary(GPP_SRP)
# jpeg(file=paste0(outdrive, 'GPP_NH4_plot.jpeg'))
# visualize(GPP_NH4, plot="model", sample=10)
# dev.off()
sjPlot:: tab_model(GPP_SRP, show.ci=FALSE, file='Mixed effects modeling/GPP_SRP_lme.doc')
qqnorm(resid(GPP_SRP))
qqline(resid(GPP_SRP))
shapiro.test(resid(GPP_SRP))

GPP_NP<-lmer(GPP_daily_mean ~NP + (1|Stream), data=Alldata)  ##these resid are normally distributed! in log-log space
GPP_NP<-lmer(GPP_daily_mean ~NP +Q_avg+ (1|Stream), data=Alldata)  ##add in Q to remove variance from Q
summary(GPP_NP)
Anova(GPP_NP)
jpeg(file=paste0(outdrive, 'GPP_NP_plot.jpeg'))
visualize(GPP_NP, plot="model", sample=10)
dev.off()
sjPlot:: tab_model(GPP_NP, show.ci=FALSE, digits=4,file='Mixed effects modeling/GPP_NP_lme_wQ.doc')
qqnorm(resid(GPP_NP))
qqline(resid(GPP_NP))
shapiro.test(resid(GPP_NP))

###relationships with Q 
Q<-lmer(Q_avg~NP+Temp_avg+GPP_daily_mean+(1|Stream), data=Alldata)
summary(Q)
Anova(Q)
dev.off()
sjPlot:: tab_model(GPP_NP, show.ci=FALSE, digits=4,file='Mixed effects modeling/Q_NP_GPP_Temp_PAR_lme.doc')

##Merge them all together into one table?
sjPlot::tab_model(GPP_Q, GPP_T, GPP_P, GPP_D, GPP_NH4, GPP_NO3, GPP_NP, GPP_SRP, show.ci=FALSE, digits=4,file='Mixed effects modeling/GPP_Combined.doc')


########################## SAME THING WITH ER ############################################################
# ER_P<-lm(ER_daily_mean ~PAR_avg, data=Alldata) ##log(Q)  to get on same scales
# summary(ER_P)
# 
# ERday_phys<-lmer(ER_daily_mean ~GPP_daily_mean+Temp_avg+Q_avg+length+ (1|Stream), data=Alldata) ##log(Q)  to get on same scales
# summary(ERday_phys)
# 
# ERday_bgc<-lmer(ER_daily_mean ~`DOC mg/L`+`NO3 mg/L N`+`NH4 mg/L N`+`SRP mg/L`+NP +EC_avg+ (1|Stream), data=Alldata) 
# summary(ERday_bgc)
# 
# ##plot them and put into tables
# sjPlot::plot_model(ERday_phys, type="slope")
# sjPlot:: tab_model(ERday_phys, show.ci=FALSE, file='Mixed effects modeling/ERday_phys.doc')
# sjPlot:: tab_model(ERday_bgc, show.ci=FALSE, file='Mixed effects modeling/ERday_bgc.doc') 
# sjPlot:: tab_model(ER_P, show.ci=FALSE, file='Mixed effects modeling/ERday_PAR.doc') 

##individually
ER_G<-lmer(ER_daily_mean ~GPP_daily_mean + (1|Stream), data=Alldata) 
ER_G<-lmer(ER_daily_mean ~GPP_daily_mean +Q_avg+ (1|Stream), data=Alldata) 
summary(ER_G)
Anova(ER_G)
jpeg(file=paste0(outdrive, 'ER_GPP_plot.jpeg'))
visualize(ER_G, plot="model", sample=10)
dev.off()
sjPlot:: tab_model(ER_G, show.ci=FALSE, file='Mixed effects modeling/ER_GPP_lme.doc')
qqnorm(resid(ER_G))
qqline(resid(ER_G))
shapiro.test(resid(ER_G))


ER_T<-lmer(ER_daily_mean ~Temp_avg +(1|Stream), data=Alldata) 
summary(ER_T)
ER_T<-lmer(ER_daily_mean ~Temp_avg +PAR_avg+ (1|Stream), data=Alldata) 
summary(ER_T)
jpeg(file=paste0(outdrive, 'ER_Temp_plot.jpeg'))
visualize(ER_T, plot="model", sample=10)
dev.off()
sjPlot:: tab_model(ER_T, show.ci=FALSE, file='Mixed effects modeling/ER_Temp_lme.doc')
qqnorm(resid(ER_T))
qqline(resid(ER_T))
shapiro.test(resid(ER_T))

ER_P<-lmer(ER_daily_mean ~PAR_avg+(1|Stream), data=Alldata) 
summary(ER_P)
ER_P<-lmer(ER_daily_mean ~PAR_avg+GPP_daily_mean+(1|Stream), data=Alldata) 
summary(ER_P)
# jpeg(file=paste0(outdrive, 'ER_PAR_plot.jpeg'))
# visualize(ER_P, plot="model", sample=10)
# dev.off()
sjPlot:: tab_model(ER_P, show.ci=FALSE, file='Mixed effects modeling/ER_PAR_lme.doc')
qqnorm(resid(ER_P))
qqline(resid(ER_P))
shapiro.test(resid(ER_P))

ER_Q<-lmer(ER_daily_mean ~Q_avg+(1|Stream), data=Alldata) 
summary(ER_Q)
ER_Q<-lmer(ER_daily_mean ~Q_avg+GPP_daily_mean+(1|Stream), data=Alldata) 
summary(ER_Q)
jpeg(file=paste0(outdrive, 'ER_Q_plot.jpeg'))
visualize(ER_Q, plot="model", sample=10)
dev.off()
sjPlot:: tab_model(ER_Q, show.ci=FALSE,digits=4, file='Mixed effects modeling/ER_Q_lme.doc')
qqnorm(resid(ER_Q))
qqline(resid(ER_Q))
shapiro.test(resid(ER_Q))

ER_Qskew<-lmer(ER_daily_mean ~Q_skewness+ (1|Stream), data=Alldata) 
summary(ER_Qskew)
jpeg(file=paste0(outdrive, 'ER_Qskew_plot.jpeg'))
visualize(ER_Qskew, plot="model", sample=10)
dev.off()
sjPlot:: tab_model(ER_Qskew, show.ci=FALSE,digits=4, file='Mixed effects modeling/ER_Q_lme.doc')
qqnorm(resid(ER_Qskew))
qqline(resid(ER_Qskew))
shapiro.test(resid(ER_Qskew))

ER_D<-lmer(ER_daily_mean ~`DOC mg/L`+ (1|Stream), data=Alldata)  ##these resid are normally distributed! in log-log space
summary(ER_D)
# jpeg(file=paste0(outdrive, 'ER_DOC_plot.jpeg'))
# visualize(ER_D, plot="model", sample=10)
# dev.off()
sjPlot:: tab_model(ER_D, show.ci=FALSE, file='Mixed effects modeling/ER_DOC_lme.doc')
qqnorm(resid(ER_D))
qqline(resid(ER_D))
shapiro.test(resid(ER_D))

ER_NO3<-lmer(ER_daily_mean ~`NO3 mg/L N`+ (1|Stream), data=Alldata) ##these resid are normally distributed! in log-log space
summary(ER_NO3)
# jpeg(file=paste0(outdrive, 'ER_NO3_plot.jpeg'))
# visualize(ER_NO3, plot="model", sample=10)
# dev.off()
sjPlot:: tab_model(ER_NO3, show.ci=FALSE, file='Mixed effects modeling/ER_NO3_lme.doc')
qqnorm(resid(ER_NO3))
qqline(resid(ER_NO3))
shapiro.test(resid(ER_NO3))

ER_NH4<-lmer(ER_daily_mean ~`NH4 mg/L N`+ (1|Stream), data=Alldata) ##these resid are normally distributed! 
summary(ER_NH4)
# jpeg(file=paste0(outdrive, 'ER_NH4_plot.jpeg'))
# visualize(ER_NH4, plot="model", sample=10)
dev.off()
sjPlot:: tab_model(ER_NH4, show.ci=FALSE, file='Mixed effects modeling/ER_NH4_lme.doc')
qqnorm(resid(ER_NH4))
qqline(resid(ER_NH4))
shapiro.test(resid(ER_NH4))

ER_SRP<-lmer(ER_daily_mean ~`SRP mg/L`+ (1|Stream), data=Alldata) ##these resid are normally distributed! 
summary(ER_SRP)
# jpeg(file=paste0(outdrive, 'ER_NO3_plot.jpeg'))
# visualize(ER_NO3, plot="model", sample=10)
# dev.off()
sjPlot:: tab_model(ER_SRP, show.ci=FALSE, file='Mixed effects modeling/ER_SRP_lme.doc')
qqnorm(resid(ER_SRP))
qqline(resid(ER_SRP))
shapiro.test(resid(ER_SRP))

ER_NP<-lmer(ER_daily_mean ~NP+ (1|Stream), data=Alldata) ##these resid are normally distributed! 
summary(ER_NP)
# jpeg(file=paste0(outdrive, 'ER_NP_plot.jpeg'))
# visualize(ER_NP, plot="model", sample=10)
# dev.off()
sjPlot:: tab_model(ER_NP, show.ci=FALSE, file='Mixed effects modeling/ER_NP_lme.doc')
qqnorm(resid(ER_NP))
qqline(resid(ER_NP))
shapiro.test(resid(ER_NP))

sjPlot::tab_model(ER_Q, ER_T, ER_P, ER_D, ER_NH4, ER_NO3, ER_NP, ER_SRP,ER_G, show.ci=FALSE, digits=4,file='Mixed effects modeling/ER_Combined.doc')


#### Tested all and Q is the only significant one with PR 
Alldata<-Alldata%>%rename(., PR=`P/R`)
PR_Q<-lmer(`PR` ~Q_avg + (1|Stream), data=Alldata) ##only one that was significant for PR.... 
summary(PR_Q)
jpeg(file=paste0(outdrive, 'PR_Q_plot.jpeg'))
visualize(PR_Q, plot="model", sample=10)
dev.off()
sjPlot:: tab_model(PR_Q, show.ci=FALSE, file='Mixed effects modeling/PR_Q_lme.doc')
qqnorm(resid(PR_Q))
qqline(resid(PR_Q))
shapiro.test(resid(PR_Q))
# 
# PR_T<-lmer(PR ~`DOC mg/L`+ (1|Stream), data=Alldata) ##log(Q)  to get on same scales
# summary(PR_T)
# 
# PRday_bgc<-lmer(`P/R` ~`DOC mg/L`+`NO3 mg/L N`+`NH4 mg/L N`+`SRP mg/L`+NP + EC_avg+ (1|Stream), data=Alldata) ##log(Q)  to get on same scales
# summary(PRday_bgc)

##plot them
# sjPlot::plot_model(PRday_lme, type="est")
# sjPlot:: tab_model(PRday_phys, file='Mixed effects modeling/PRday_phys.doc') 
# sjPlot:: tab_model(PRday_bgc, file='Mixed effects modeling/PRday_phys.doc') 
# sjPlot:: tab_model(PR_P, file='Mixed effects modeling/PRday_PAR.doc') 

############################### SPATIAL VARIATION LINEAR MODELS ###################################################################
########### THIS IS ON HOLD FOR NOW.... DOING NORMAL LINEAR REGRESSIONS WITH SEASON AS GROUP 
## But we are going to run an lm for spatial variation just to control for flow against other factors (temp, EC, NH4)
## found the significant relationships from the linear reg plotted in this script, then seeing if we remove the 
## variance from Q if those relationships still hold

### data = Total 
## sig relationships:
## GPP ~ NH4, EC_avg, Q_avg
## ER ~ GPP_avg, EC_avg, Q_avg, Temp_avg

GPP_EC<-lm(GPP_avg ~EC_avg+data=Total) ##log(Q)  to get on same scales
summary(GPP_EC)
Anova(GPP_EC)
## not sig with Q

GPP_NH4<-lm(GPP_avg ~NH4_avg + Q_avg, data=Total) ##log(Q)  to get on same scales
summary(GPP_NH4)
## less sig with Q

ER_GPP<-lm(ER_avg ~GPP_avg, data=Total) ##log(Q)  to get on same scales
summary(ER_GPP)
Anova(ER_GPP)
##not sig with Q

ER_Q<-lm(ER_avg ~Q_avg +GPP_avg, data=Total) ##log(Q)  to get on same scales
summary(ER_Q)
Anova(ER_Q)
##not sig with Q

ER_EC<-lm(ER_avg ~EC_avg+Q_avg, data=Total) ##log(Q)  to get on same scales
summary(ER_EC)
Anova(ER_EC)
##not sig with Q

ER_T<-lm(ER_avg ~Temp_avg+Q_avg+GPP_avg, data=Total) ##log(Q)  to get on same scales
summary(ER_T)
##not sig with Q or GPP

### DO the same thing for seasonal averages (spatial differences between groups)
# ## Season as random effects 
# # library(gls)
# Seas<-distinct(Seas)
# GPPseas_phys<-lmer(GPP_avg ~Temp_avg+Q_avg+length+dist_coast + (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(GPPseas_phys)
# 
# GPPseas_bgc<-lmer(GPP_avg ~`DOC_avg`+`NO3_avg`+`NH4_avg`+`SRP_avg`+NP_avg +EC_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(GPPseas_bgc)
# 
# GPPseas_bio<-lmer(GPP_avg ~ Avg_bio_black+Avg_bio_orange +Max_bio_black+Max_bio_orange+ (1|Season), data=Seas)
# summary(GPPseas_bio)
# 
# ##plot them and put into tables
# sjPlot::plot_model(GPPseas_phys, type="est")
# sjPlot:: tab_model(GPPseas_phys, show.ci=FALSE, file='Mixed effects modeling/GPPseas_phys.doc') 
# sjPlot:: tab_model(GPPseas_bgc, show.ci=FALSE, file='Mixed effects modeling/GPPseas_bgc.doc') 
# sjPlot:: tab_model(GPPseas_bio, show.ci=FALSE, file='Mixed effects modeling/GPPseas_bio.doc') 
# 
# ##run them each individually? See if that changes anything
# GPP_T<-lmer(GPP_avg ~Temp_avg + (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(GPP_T)
# GPP_Q<-lmer(GPP_avg ~Q_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(GPP_Q)
# GPP_D<-lmer(GPP_avg ~DOC_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(GPP_D)
# GPP_NO3<-lmer(GPP_avg ~NO3_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(GPP_NO3)
# GPP_NH4<-lmer(GPP_avg ~NH4_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(GPP_NH4)
# GPP_SRP<-lmer(GPP_avg ~SRP_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(GPP_SRP)
# GPP_NP<-lmer(GPP_avg ~NP_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(GPP_NP)
# 
# ERseas_phys<-lmer(ER_avg ~GPP_avg+Temp_avg+Q_avg+length+dist_coast+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(ERseas_phys)
# 
# ERseas_bgc<-lmer(ER_avg ~`DOC_avg`+`NO3_avg`+`NH4_avg`+`SRP_avg`+NP_avg +EC_avg + (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(ERseas_bgc)
# 
# ERseas_bio<-lmer(ER_avg ~ Avg_bio_black+Avg_bio_orange +Max_bio_black+Max_bio_orange+  (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(ERseas_bio)
# 
# ##plot them and put into table
# sjPlot::plot_model(ERseas_phys, type="est")
# sjPlot:: tab_model(ERseas_phys, show.ci=FALSE, file='Mixed effects modeling/ERseas_phys.doc') 
# sjPlot:: tab_model(ERseas_bgc, show.ci=FALSE, file='Mixed effects modeling/ERseas_bgc.doc') 
# sjPlot:: tab_model(ERseas_bio, show.ci=FALSE, file='Mixed effects modeling/ERseas_bio.doc') 
# 
# ##run them each individually? See if that changes anything
# ER_T<-lmer(ER_avg ~Temp_avg + (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(ER_T)
# ER_Q<-lmer(ER_avg ~Q_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(ER_Q)
# ER_D<-lmer(ER_avg ~DOC_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(ER_D)
# ER_NO3<-lmer(ER_avg ~NO3_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(ER_NO3)
# ER_NH4<-lmer(ER_avg ~NH4_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(ER_NH4)
# ER_SRP<-lmer(ER_avg ~SRP_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(ER_SRP)
# ER_NP<-lmer(ER_avg ~NP_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(ER_NP)
# 
# PRseas_phys<-lmer(`P/R` ~Temp_avg+Q_avg+length+dist_coast + (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(PRseas_phys)
# 
# PRseas_bgc<-lmer(`P/R` ~`DOC_avg`+`NO3_avg`+`NH4_avg`+`SRP_avg`+NP_avg +EC_avg+ (1|Season), data=Seas) 
# summary(PRseas_bgc)
# 
# PRseas_bio<-lmer(`P/R` ~Avg_bio_black+Avg_bio_orange +Max_bio_black+Max_bio_orange+  (1|Season), data=Seas)
# summary(PRseas_bio)
# 
# ##plot them
# sjPlot:: tab_model(PRseas_phys, file='Mixed effects modeling/PRseas_phys.doc') 
# sjPlot:: tab_model(PRseas_bgc, file='Mixed effects modeling/PRseas_bgc.doc') 
# sjPlot:: tab_model(PRseas_bio, file='Mixed effects modeling/PRseas_bio.doc') 
# 
# ##run them each individually? See if that changes anything
# PR_T<-lmer(`P/R` ~Temp_avg + (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(PR_T)
# PR_Q<-lmer(`P/R` ~Q_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(PR_Q)
# PR_D<-lmer(`P/R` ~DOC_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(PR_D)
# PR_NO3<-lmer(`P/R` ~NO3_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(PR_NO3)
# PR_NH4<-lmer(`P/R` ~NH4_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(PR_NH4)
# PR_SRP<-lmer(`P/R` ~SRP_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(PR_SRP)
# PR_NP<-lmer(`P/R` ~NP_avg+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(PR_NP)
# PR_black<-lmer(`P/R` ~Avg_bio_black+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(PR_black)
# PR_orange<-lmer(`P/R` ~Avg_bio_orange+ (1|Season), data=Seas) ##log(Q)  to get on same scales
# summary(PR_orange)
# 
# ERseas_gpp<-lm(ER ~GPP, data=Seas) ##log(Q)  to get on same scales
# summary(ERseas_gpp)

############# MULTIVARIATE MODELS######################################################
Total_long=Total%>%
  pivot_longer(cols=c(GPP_avg, ER_avg), names_to="GPP_ER", values_to="Metabolism")
m1<-lmer(Metabolism ~ GPP_ER * (Temp_avg + Q_avg + length + width + EC_avg + dist_coast + low_flow + LowFlow_Prop) +
           (1|Stream), data=Total_long)

## using the significant relationships from above to tease apart some of the actual effects
library(MMeM)
## this package can do 2 responses, one fixed effect, one random 

## For temporal (all days of data, stream as random effects) data = Alldata
## sig relationships from lme: 
## GPP ~ PAR, Q_avg, Temp_avg, NP 
## ER ~ Q_avg, GPP daily mean 

# T.start <- matrix(c(10,5,5,15),2,2)
# E.start <- matrix(c(10,1,1,3),2,2)

Alldata1<-Alldata%>%
  dplyr::select(., GPP_daily_mean, Temp_avg, Q_avg, Stream)%>%
  dplyr::filter(., !is.na(GPP_daily_mean))%>%
  dplyr::filter(., !is.na(Temp_avg))%>%
  dplyr::filter(., !is.na(Q_avg))

## look at relationship between GPP and Q, Temp, NP
# GPP_T_Q<-MMeM_henderson3(fml=c(GPP_daily_mean, Temp_avg) ~ Q_avg + (1|Stream), data=Alldata1, factor_X = FALSE)
# summary(GPP_T_Q)

GPP_T_Q<-lm(cbind(GPP_daily_mean, Q_avg) ~ Temp_avg + NP, data=Alldata)
summary(GPP_T_Q)
## From this, NP does have a relationship with GPP_daily_mean, but Temp has a relationship with Q (which in turn has relationship with GPP)

GPP<-lmer(GPP_daily_mean ~ Temp_avg +Q_avg+NP+ (1|Stream), data=Alldata)
summary(GPP)

############################ Spatial data -- avgs between streams  ##################################
### data = Total 
## sig relationships:
## GPP ~ NH4, EC_avg, Q_avg
## ER ~ GPP_avg, EC_avg, Q_avg, Temp_avg

GPPmulti<-lm(cbind(GPP_avg, Q_avg) ~ NH4_avg + EC_avg, data=Total)
summary(GPPmulti)

ERmulti<-lm(cbind(ER_avg, Q_avg) ~ EC_avg, data=Total)
summary(ERmulti)

ER<-lm(ER_avg~ GPP_avg, data=Total)



