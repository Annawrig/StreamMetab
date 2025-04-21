#### Script to combine all of the Metabolism model outputs into new files -- make further analysis easier
#### For 21-22 and 22-23 data
### using "main flow season"  -- cropped the 2223 data dates for comparison to the 2122 season

#### Script to make new datafiles from model outputs in StreamMetabolizer for 21-22 season
## Get daily estimates and instantaneous values for GPP and ER from model (other R script) -- put them together in new datasets here
## created by ATW 09/27/22

## Sept. 2024 add in a value for "skewness" of discharge to estimate stability of flows (Bernhardt et al., 2017)

# -- setup----
library(ggplot2)
library(nlme)
library(dplyr)
library(knitr)
library(lubridate)
library(moments)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##input and output location for files
drive<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/'
drive1<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/2122 Model Outputs/'
drive2<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/2223 Model Outputs/'

##Read in .csv files with GPP, ER estimates for each stream
##read in 2021-2022 data, add in "season"
C121<-read.csv(paste0(drive1, 'C1_2122_Metab_predic_K600bin.csv'), stringsAsFactors = FALSE)
C121$season<-'2021-2022'
C121$Stream<-'Commonwealth'
C121<-C121%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
C121$date<-as.Date(C121$date)
F121<-read.csv(paste0(drive1, 'F1_2122_Metab_predic_K600bin.csv'), stringsAsFactors = FALSE)
F121$season<-'2021-2022'
F121$Stream<-'Canada'
F121<-F121%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
F121$date<-as.Date(F121$date)
F321<-read.csv(paste0(drive1, 'F3_2122_Metab_predic_K600bin.csv'), stringsAsFactors = FALSE)
F321$season<-'2021-2022'
F321$Stream<-'Lost Seal'
F321<-F321%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
F321$date<-as.Date(F321$date)
F521<-read.csv(paste0(drive1, 'F5_2122_Metab_predic_K600bin.csv'), stringsAsFactors = FALSE)
F521$season<-'2021-2022'
F521$Stream<-'Aiken'
F521<-F521%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
F521$date<-as.Date(F521$date)
F621<-read.csv(paste0(drive1, 'F6_2122_Metab_predic_K600bin.csv'), stringsAsFactors = FALSE)
F621$season<-'2021-2022'
F621$Stream<-'Von Guerard'
F621<-F621%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
F621$date<-as.Date(F621$date)
F921<-read.csv(paste0(drive1, 'F9_2122_Metab_predic_K600bin.csv'), stringsAsFactors = FALSE)
F921$season<-'2021-2022'
F921$Stream<-'Green'
F921<-F921%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
F921$date<-as.Date(F921$date)
F1021<-read.csv(paste0(drive1, 'F10_2122_Metab_predic_K600bin.csv'), stringsAsFactors = FALSE)
F1021$season<-'2021-2022'
F1021$Stream<-'Delta'
F1021<-F1021%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
F1021$date<-as.Date(F1021$date)


##read in 2022-2023 data, add in "season"
C122<-read.csv(paste0(drive2, 'C1_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
C122$season<-'2022-2023'
C122$Stream<-'Commonwealth'
C122<-C122%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
C122$date<-as.Date(C122$date)
F122<-read.csv(paste0(drive2, 'F1_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
F122$season<-'2022-2023'
F122$Stream<-'Canada'
F122<-F122%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
F122$date<-as.Date(F122$date)
F222<-read.csv(paste0(drive2, 'F2_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
F222$season<-'2022-2023'
F222$Stream<-'Huey'
F222<-F222%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
F222$date<-as.Date(F222$date)
F322<-read.csv(paste0(drive2, 'F3_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
F322$season<-'2022-2023'
F322$Stream<-'Lost Seal'
F322<-F322%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
F322$date<-as.Date(F322$date)
F522<-read.csv(paste0(drive2, 'F5_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
F522$season<-'2022-2023'
F522$Stream<-'Aiken'
F522<-F522%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
F522$date<-as.Date(F522$date)
F622<-read.csv(paste0(drive2, 'F6_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
F622$season<-'2022-2023'
F622$Stream<-'Von Guerard'
F622<-F622%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
F622$date<-as.Date(F622$date)
F722<-read.csv(paste0(drive2, 'F7_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
F722$season<-'2022-2023'
F722$Stream<-'Harnish'
F722<-F722%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
F722$date<-as.Date(F722$date)
F822<-read.csv(paste0(drive2, 'F8_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
F822$season<-'2022-2023'
F822$Stream<-'Crescent'
F822<-F822%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
F822$date<-as.Date(F822$date)
F922<-read.csv(paste0(drive2, 'F9_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
F922$season<-'2022-2023'
F922$Stream<-'Green'
F922<-F922%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
F922$date<-as.Date(F922$date)
F1022<-read.csv(paste0(drive2, 'F10_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
F1022$season<-'2022-2023'
F1022$Stream<-'Delta'
F1022<-F1022%>%
  dplyr::filter(.,!is.na(GPP_daily_mean))
F1022$date<-as.Date(F1022$date)

# # ### FOR 22-23 data we want to crop to the dates of 21-22 season for comparison of "main flow season" -- just for the seasonal averages
# ## match the end date for 21-22 and use dec 15 at the beginning or the start of the 21-22 data if before 
# F122<-F122%>%
#   dplyr::filter(., date<'2023-01-22'&date>='2022-12-15')
# F222<-F222%>%
#   dplyr::filter(., date<'2023-01-22'&date>='2022-12-15')
# F322<-F322%>%
#   dplyr::filter(., date<'2023-01-22'&date>='2022-12-13')
# F522<-F522%>%
#   dplyr::filter(., date<'2023-01-22'&date>='2022-12-16')
# F622<-F622%>%
#   dplyr::filter(., date<'2023-01-26'&date>='2022-12-15')
# F722<-F722%>%
#   dplyr::filter(., date<'2023-01-22'&date>='2022-12-15')
# F822<-F822%>%
#   dplyr::filter(., date<'2023-01-22'&date>='2022-12-15')
# F922<-F922%>%
#   dplyr::filter(., date<'2023-01-17'&date>='2022-12-06')
# F1022<-F1022%>%
#   dplyr::filter(., date<'2023-01-25'&date>='2022-12-15')
# C122<-C122%>%
#   dplyr::filter(., date<'2023-01-23'&date>='2022-12-08')

## read in .csv files with drives - Q, stream temp, EC
## Need to get daily mean and max, set any temp < 0 to be 0 
F1Q21<-read.csv(paste0(drive, '2021_2022_data_ATW/Gage data/',  'F1_2122A_SUBM.csv'), na.strings="NaN")
F1Q21$Stream<-'Canada'
F1Q21$DateTime<-lubridate::mdy_hm(F1Q21$DATE_TIME)
F1Q21$date<-as.Date(F1Q21$DateTime)
F1Q21$WATER_TEMP[F1Q21$WATER_TEMP<0]<-0
##also add in a value for flow skewnewss -- how flashy or stable are the flows?
F1Q21<-F1Q21%>%
  group_by(date)%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE), Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE),EC_med=median(CONDUCTIVITY,na.rm=TRUE), Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

F3Q21<-read.csv(paste0(drive, '2021_2022_data_ATW/Gage data/',  'F3_2122A_SUBM.csv'), na.strings="NaN")
F3Q21$Stream<-'Lost Seal'
F3Q21$DateTime<-lubridate::mdy_hm(F3Q21$DATE_TIME)
F3Q21$WATER_TEMP[F3Q21$WATER_TEMP<0]<-0
F3Q21<-F3Q21%>%
  group_by(date=as.Date(DateTime))%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE),Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE),EC_med=median(CONDUCTIVITY,na.rm=TRUE),  Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

F5Q21<-read.csv(paste0(drive, '2021_2022_data_ATW/Gage data/',  'F5_2122A_SUBM.csv'), na.strings="NaN")
F5Q21$Stream<-'Aiken'
F5Q21$DateTime<-lubridate::mdy_hm(F5Q21$DATE_TIME)
F5Q21$WATER_TEMP[F5Q21$WATER_TEMP<0]<-0
F5Q21<-F5Q21%>%
  group_by(date=as.Date(DateTime))%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE),Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE),EC_med=median(CONDUCTIVITY,na.rm=TRUE),  Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

F6Q21<-read.csv(paste0(drive, '2021_2022_data_ATW/Gage data/',  'F6_2122A_SUBM.csv'), na.strings="NaN")
F6Q21$Stream<-'Von Guerard'
F6Q21$DateTime<-lubridate::mdy_hm(F6Q21$DATE_TIME)
F6Q21$WATER_TEMP[F6Q21$WATER_TEMP<0]<-0
F6Q21<-F6Q21%>%
  group_by(date=as.Date(DateTime))%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE),Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE),EC_med=median(CONDUCTIVITY,na.rm=TRUE),  Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

F9Q21<-read.csv(paste0(drive, '2021_2022_data_ATW/Gage data/',  'F9_2122A_SUBM.csv'), na.strings="NaN")
F9Q21$Stream<-'Green'
F9Q21$DateTime<-lubridate::mdy_hm(F9Q21$DATE_TIME)
F9Q21$WATER_TEMP[F9Q21$WATER_TEMP<0]<-0
F9Q21<-F9Q21%>%
  group_by(date=as.Date(DateTime))%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE),Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE), EC_med=median(CONDUCTIVITY,na.rm=TRUE), Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

F10Q21<-read.csv(paste0(drive, '2021_2022_data_ATW/Gage data/',  'F10_2122A_SUBM.csv'), na.strings="NaN")
F10Q21$Stream<-'Delta'
F10Q21$DateTime<-lubridate::mdy_hm(F10Q21$DATE_TIME)
F10Q21$WATER_TEMP[F10Q21$WATER_TEMP<0]<-0
F10Q21<-F10Q21%>%
  group_by(date=as.Date(DateTime))%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE),Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE),EC_med=median(CONDUCTIVITY,na.rm=TRUE),  Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

C1Q21<-read.csv(paste0(drive, '2021_2022_data_ATW/Gage data/',  'C1_2122A_SUBM.csv'), na.strings="NaN")
C1Q21$Stream<-'Commonwealth'
C1Q21$DateTime<-lubridate::mdy_hm(C1Q21$DATE_TIME)
C1Q21$WATER_TEMP[C1Q21$WATER_TEMP<0]<-0
C1Q21<-C1Q21%>%
  group_by(date=as.Date(DateTime))%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE),Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE),EC_med=median(CONDUCTIVITY,na.rm=TRUE),  Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

#### 2022-2023 data
#### for the "main flow season" only use A data, for the full season use A and B 
F1Q22<-read.csv(paste0(drive, '2022_2023_data/',  'F1_2223A_SUBM.csv'), na.strings="NaN")
F1Q22B<-read.csv(paste0(drive, '2022_2023_data/',  '2223B data/F1_2223B_SUBM.csv'), na.strings="NaN")
F1Q22<-full_join(F1Q22, F1Q22B)
F1Q22$Stream<-'Canada'
F1Q22$DateTime<-lubridate::mdy_hm(F1Q22$DATE_TIME)
F1Q22$WATER_TEMP[F1Q22$WATER_TEMP<0]<-0
F1Q22<-F1Q22%>%
  group_by(date=as.Date(DateTime))%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE),Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE),EC_med=median(CONDUCTIVITY,na.rm=TRUE),  Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

F2Q22<-read.csv(paste0(drive, '2022_2023_data/',  'F2_2223A_SUBM.csv'), na.strings="NaN")
F2Q22B<-read.csv(paste0(drive, '2022_2023_data/',  '2223B data/F2_2223B_SUBM.csv'), na.strings="NaN")
F2Q22<-full_join(F2Q22, F2Q22B)
F2Q22$Stream<-'Huey'
F2Q22$DateTime<-lubridate::mdy_hm(F2Q22$DATE_TIME)
F2Q22$WATER_TEMP[F2Q22$WATER_TEMP<0]<-0
F2Q22<-F2Q22%>%
  group_by(date=as.Date(DateTime))%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE),Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE),EC_med=median(CONDUCTIVITY,na.rm=TRUE),  Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

F3Q22<-read.csv(paste0(drive, '2022_2023_data/',  'F3_2223A_SUBM.csv'), na.strings="NaN")
F3Q22B<-read.csv(paste0(drive, '2022_2023_data/',  '2223B data/F3_2223B_SUBM.csv'), na.strings="NaN")
F3Q22<-full_join(F3Q22, F3Q22B)
F3Q22$Stream<-'Lost Seal'
F3Q22$DateTime<-lubridate::mdy_hm(F3Q22$DATE_TIME)
F3Q22$WATER_TEMP[F3Q22$WATER_TEMP<0]<-0
F3Q22<-F3Q22%>%
  group_by(date=as.Date(DateTime))%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE),Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE),EC_med=median(CONDUCTIVITY,na.rm=TRUE),  Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

F5Q22<-read.csv(paste0(drive, '2022_2023_data/',  'F5_2223A_SUBM.csv'), na.strings="NaN")
F5Q22B<-read.csv(paste0(drive, '2022_2023_data/',  '2223B data/F5_2223B_SUBM.csv'), na.strings="NaN")
F5Q22<-full_join(F5Q22, F5Q22B)
F5Q22$Stream<-'Aiken'
F5Q22$DateTime<-lubridate::mdy_hm(F5Q22$DATE_TIME)
F5Q22$WATER_TEMP[F5Q22$WATER_TEMP<0]<-0
F5Q22<-F5Q22%>%
  group_by(date=as.Date(DateTime))%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE),Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE),EC_med=median(CONDUCTIVITY,na.rm=TRUE),  Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

F6Q22<-read.csv(paste0(drive, '2022_2023_data/',  'F6_2223A_SUBM.csv'), na.strings="NaN")
F6Q22B<-read.csv(paste0(drive, '2022_2023_data/',  '2223B data/F6_2223B_SUBM.csv'), na.strings="NaN")
F6Q22<-full_join(F6Q22, F6Q22B)
F6Q22$Stream<-'Von Guerard'
F6Q22$DateTime<-lubridate::mdy_hm(F6Q22$DATE_TIME)
F6Q22$WATER_TEMP[F6Q22$WATER_TEMP<0]<-0
F6Q22<-F6Q22%>%
  group_by(date=as.Date(DateTime))%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE),Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE),EC_med=median(CONDUCTIVITY,na.rm=TRUE),  Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

F7Q22<-read.csv(paste0(drive, '2022_2023_data/',  'F7_2223A_SUBM.csv'), na.strings="NaN")
F7Q22B<-read.csv(paste0(drive, '2022_2023_data/',  '2223B data/F7_2223B_SUBM.csv'), na.strings="NaN")
F7Q22<-full_join(F7Q22, F7Q22B)
F7Q22$Stream<-'Harnish'
F7Q22$DateTime<-lubridate::mdy_hm(F7Q22$DATE_TIME)
F7Q22$WATER_TEMP[F7Q22$WATER_TEMP<0]<-0
F7Q22<-F7Q22%>%
  group_by(date=as.Date(DateTime))%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE),Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE),EC_med=median(CONDUCTIVITY,na.rm=TRUE),  Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

F8Q22<-read.csv(paste0(drive, '2022_2023_data/',  'F8_2223A_SUBM.csv'), na.strings="NaN")
F8Q22B<-read.csv(paste0(drive, '2022_2023_data/',  '2223B data/F8_2223B_SUBM.csv'), na.strings="NaN")
F8Q22<-full_join(F8Q22, F8Q22B)
F8Q22$Stream<-'Crescent'
F8Q22$DateTime<-lubridate::mdy_hm(F8Q22$DATE_TIME)
F8Q22$WATER_TEMP[F8Q22$WATER_TEMP<0]<-0
F8Q22<-F8Q22%>%
  group_by(date=as.Date(DateTime))%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE),Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE),EC_med=median(CONDUCTIVITY,na.rm=TRUE),  Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

F9Q22<-read.csv(paste0(drive, '2022_2023_data/',  'F9_2223A_SUBM.csv'), na.strings="NaN")
F9Q22B<-read.csv(paste0(drive, '2022_2023_data/',  '2223B data/F9_2223B_SUBM.csv'), na.strings="NaN")
F9Q22<-full_join(F9Q22, F9Q22B)
F9Q22$Stream<-'Green'
F9Q22$DateTime<-lubridate::mdy_hm(F9Q22$DATE_TIME)
F9Q22$WATER_TEMP[F9Q22$WATER_TEMP<0]<-0
F9Q22<-F9Q22%>%
  group_by(date=as.Date(DateTime))%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE),Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE),EC_med=median(CONDUCTIVITY,na.rm=TRUE),  Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

F10Q22<-read.csv(paste0(drive, '2022_2023_data/',  'F10_2223A_SUBM.csv'), na.strings="NaN")
F10Q22B<-read.csv(paste0(drive, '2022_2023_data/',  '2223B data/F10_2223B_SUBM.csv'), na.strings="NaN")
F10Q22<-full_join(F10Q22, F10Q22B)
F10Q22$Stream<-'Delta'
F10Q22$DateTime<-lubridate::mdy_hm(F10Q22$DATE_TIME)
F10Q22$WATER_TEMP[F10Q22$WATER_TEMP<0]<-0
F10Q22<-F10Q22%>%
  group_by(date=as.Date(DateTime))%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE),Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE),EC_med=median(CONDUCTIVITY,na.rm=TRUE),  Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

C1Q22<-read.csv(paste0(drive, '2022_2023_data/',  'C1_2223A_SUBM.csv'), na.strings="NaN")
C1Q22B<-read.csv(paste0(drive, '2022_2023_data/',  '2223B data/C1_2223B_SUBM.csv'), na.strings="NaN")
C1Q22<-full_join(C1Q22, C1Q22B)
C1Q22$Stream<-'Commonwealth'
C1Q22$DateTime<-lubridate::mdy_hm(C1Q22$DATE_TIME)
C1Q22$WATER_TEMP[C1Q22$WATER_TEMP<0]<-0
C1Q22<-C1Q22%>%
  group_by(date=as.Date(DateTime))%>%
  summarise(., Q_avg=mean(DISCHARGE_RATE, na.rm=TRUE), Q_max=max(DISCHARGE_RATE, na.rm=TRUE), Q_min=min(DISCHARGE_RATE, na.rm=TRUE),Temp_avg=mean(WATER_TEMP, na.rm=TRUE),
            Temp_max=max(WATER_TEMP,na.rm=TRUE), EC_avg=mean(CONDUCTIVITY,na.rm=TRUE),EC_med=median(CONDUCTIVITY,na.rm=TRUE),  Q_skewness=skewness(DISCHARGE_RATE,na.rm=TRUE), Stream=Stream)

## Combine together by season and add in stream length in km -- table 1 from Harmon et al. 2021
AllF621<-full_join(F621, F6Q21)
AllF621$length<-4.9
AllF921<-full_join(F921, F9Q21)
AllF921$length<-1.2
AllF121<-full_join(F121, F1Q21)
AllF121$length<-1.4
AllF321<-full_join(F321, F3Q21)
AllF321$length<-2.3
AllF521<-full_join(F521, F5Q21)
AllF521$length<-6.9
AllF1021<-full_join(F1021, F10Q21)
AllF1021$length<-6.5
AllC121<-full_join(C121, C1Q21)
AllC121$length<-5.1
# AllOX<-full_join(OXdat, OXQT)
# AllOX<-full_join(AllOX, OX_PAR)
# AllOX$length<-32

AllF122<-full_join(F122, F1Q22)
AllF122$length<-1.4
AllF222<-full_join(F222, F2Q22)
AllF222$length<-2.4 
AllF322<-full_join(F322, F3Q22)
AllF322$length<-2.3
AllF522<-full_join(F522, F5Q22)
AllF522$length<-6.9
AllF622<-full_join(F622, F6Q22)
AllF622$length<-4.9
AllF722<-full_join(F722, F7Q22)
AllF722$length<-6.4
AllF822<-full_join(F822, F8Q22)
AllF822$length<-8.6
AllF922<-full_join(F922, F9Q22)
AllF922$length<-1.2
AllF1022<-full_join(F1022, F10Q22)
AllF1022$length<-6.5
AllC122<-full_join(C122, C1Q22)
AllC122$length<-5.1

# ##Combine them ALL together by season
data21<-full_join(AllF121, AllF321)
data21<-full_join(data21, AllF521)
data21<-full_join(data21, AllF621)
data21<-full_join(data21, AllF921)
data21<-full_join(data21, AllF1021)
data21<-full_join(data21, AllC121)

data22<-full_join(AllF122, AllF222)
data22<-full_join(data22, AllF322)
data22<-full_join(data22, AllF522)
data22<-full_join(data22, AllF622)
data22<-full_join(data22, AllF722)
data22<-full_join(data22, AllF822)
data22<-full_join(data22, AllF922)
data22<-full_join(data22, AllF1022)
data22<-full_join(data22, AllC122)

##calculate the daily P/R ratio for each stream
data21$PR_daily=(data21$GPP_daily_mean/abs(data21$ER_daily_mean))
data22$PR_daily=(data22$GPP_daily_mean/abs(data22$ER_daily_mean))

##refine down to only the things we need
##For instanteous values
data21<-data21%>%
  dplyr::select(., date, GPP_daily_mean, ER_daily_mean, PR_daily, Q_avg, Q_max, Q_min, Q_skewness, Temp_avg,Temp_max,EC_avg, EC_med, Stream, length)
data21<-distinct(data21)
data21[sapply(data21, is.nan)] <- NA
data21$Q_max[data21$Q_max=='-Inf']<-NA
data21$Q_min[data21$Q_min=='Inf']<-NA
data21$Q_avg[data21$Q_avg=='NaN']<-NA
data21$Temp_max[data21$Temp_max=='-Inf']<-NA

##find # of days with avg flow <1 l/s
data21$low_flow[data21$Q_avg<1]<-1
data21$low_flow[data21$Q_avg>1]<-0
##add in a sperate column with just a 1 for each date to count the # of days wtih flow 
data21$flow_date<-1

data22<-data22%>%
  dplyr::select(., date, GPP_daily_mean, ER_daily_mean, PR_daily, Q_avg, Q_max, Q_min, Q_skewness, Temp_avg,Temp_max,EC_avg, EC_med, Stream, length)
data22<-distinct(data22)
data22[sapply(data22, is.nan)] <- NA
data22$Q_max[data22$Q_max=='-Inf']<-NA
data22$Q_min[data22$Q_min=='Inf']<-NA
data22$Q_avg[data22$Q_avg=='NaN']<-NA
data22$Temp_max[data22$Temp_max=='-Inf']<-NA

##find # of days with avg flow <1 l/s
data22$low_flow[data22$Q_avg<1]<-1
data22$low_flow[data22$Q_avg>1]<-0
data22$flow_date<-1

##write out the data so that you don't have to re-create it every time but can just read it in
write.csv(data21,paste0(drive, 'All_2122Metab_Data.csv'), row.names = FALSE)
## do 2223 with the main flow season, and one with the entire season
# write.csv(data22,paste0(drive, 'All_2223Metab_Data_MainFlowSeas.csv'), row.names = FALSE)
write.csv(data22,paste0(drive, 'All_2223Metab_Data.csv'), row.names = FALSE)


## FIND Seasonal AVERAGES 
## find average skewness and do another skewness for all daily avgs. 
All21<-data21%>%
  dplyr::group_by(Stream)%>%
  summarise(., GPP_avg=mean(GPP_daily_mean, na.rm=TRUE), ER_avg=mean(ER_daily_mean, na.rm=TRUE), PR_daily_avg=mean(PR_daily, na.rm=TRUE),Q_avg=mean(Q_avg, na.rm=TRUE),
            Temp_avg=mean(Temp_avg, na.rm=TRUE),EC_avg=mean(EC_avg, na.rm=TRUE),EC_med=median(EC_med, na.rm=TRUE), length=length, low_flow=sum(low_flow, na.rm=TRUE), flow_date=sum(flow_date, na.rm=TRUE), 
            Qskew_avg=mean(Q_skewness, na.rm=TRUE), Qskewness=skewness(Q_avg, na.rm=TRUE))
All21<-distinct(All21)
All21$PR_seas_avg<-(All21$GPP_avg/abs(All21$ER_avg))
##find proportion of flow season with <1 l/s
All21$LowFlow_Prop<-(All21$low_flow/All21$flow_date)

##add in biomass estimates for 
### use estimates created by Mark's unmixing model 
# bio<-read_excel(paste0(drive, 'Wright_BiomassEstimate_ChannelOnly_WV02&WV03.xlsx'), sheet=3)

### OR USE THE ESTIMATES THAT INCLUDE algae ops data (combine mark's spatial coverage with algae ops biomass) 
bio<-read_excel(paste0(drive, 'All_Biomass_estimates.xlsx'), sheet=1)

# bio2<-bio%>%dplyr::select(.,Stream, Avg_biomass_g_m2, `MAT.TYPE`)%>%spread(., `MAT.TYPE`, `Avg_biomass_g_m2`)
# bio2<-bio2%>%dplyr::rename(., Avg_bio_black=black, Avg_bio_orange=orange)%>%select(., Stream, Avg_bio_black, Avg_bio_orange)
# bio3<-bio%>%dplyr::select(.,Stream, Max_biomass_g_m, `MAT.TYPE`)%>%spread(., `MAT.TYPE`, `Max_biomass_g_m`)
# bio3<-bio3%>%dplyr::rename(., Max_bio_black=black, Max_bio_orange=orange)%>%select(., Stream, Max_bio_black, Max_bio_orange)

All21<-full_join(All21, bio)
All21<-All21%>%dplyr::filter(., !is.na(GPP_avg))

##also add distance from coast here
All21$dist_coast[All21$Stream=='Canada']<-12
All21$dist_coast[All21$Stream=='Lost Seal']<-6.8
All21$dist_coast[All21$Stream=='Aiken']<-4.4
All21$dist_coast[All21$Stream=='Von Guerard']<-7.3
All21$dist_coast[All21$Stream=='Delta']<-11.3
All21$dist_coast[All21$Stream=='Green']<-12
All21$dist_coast[All21$Stream=='Commonwealth']<-0
  
All22<-data22%>%
  dplyr::group_by(Stream)%>%
  summarise(., GPP_avg=mean(GPP_daily_mean, na.rm=TRUE), ER_avg=mean(ER_daily_mean, na.rm=TRUE), PR_daily_avg=mean(PR_daily, na.rm=TRUE),Q_avg=mean(Q_avg, na.rm=TRUE),
            Temp_avg=mean(Temp_avg, na.rm=TRUE),EC_avg=mean(EC_avg, na.rm=TRUE),length=length, EC_med=median(EC_med, na.rm=TRUE), low_flow=sum(low_flow, na.rm=TRUE), flow_date=sum(flow_date, na.rm=TRUE), 
            Qskew_avg=mean(Q_skewness, na.rm=TRUE), Qskewness=skewness(Q_avg, na.rm=TRUE))
All22<-distinct(All22)
All22$PR_seas_avg<-(All22$GPP_avg/abs(All22$ER_avg))
All22$LowFlow_Prop<-(All22$low_flow/All22$flow_date)

All22<-full_join(All22, bio)
All22<-All22%>%dplyr::filter(., !is.na(GPP_avg))

All22$dist_coast[All22$Stream=='Canada']<-12
All22$dist_coast[All22$Stream=='Lost Seal']<-6.8
All22$dist_coast[All22$Stream=='Aiken']<-4.4
All22$dist_coast[All22$Stream=='Von Guerard']<-7.3
All22$dist_coast[All22$Stream=='Delta']<-11.3
All22$dist_coast[All22$Stream=='Green']<-12
All22$dist_coast[All22$Stream=='Commonwealth']<-0
All22$dist_coast[All22$Stream=='Huey']<-10
All22$dist_coast[All22$Stream=='Crescent']<-9.5
All22$dist_coast[All22$Stream=='Harnish']<-8.1

# write.csv(All21, paste0(drive, 'Alldata2122_MainFlowSeason.csv'), row.names = FALSE)
# write.csv(All22, paste0(drive, 'Alldata2223_MainFlowSeason.csv'), row.names = FALSE)

## Find averages across all the data -- both seasons 
### DO NOT SURE THE MAIN FLOW SEASON -- USE ALL OF THE DATA (i.e. to do this make sure you block out the area above where you crop the dates)
Full1<-full_join(data21, data22)
Full<-Full1%>%
  dplyr::group_by(Stream)%>%
  summarise(., GPP_avg=mean(GPP_daily_mean, na.rm=TRUE), GPP_med=median(GPP_daily_mean, na.rm=TRUE), GPP_max=max(GPP_daily_mean, na.rm=TRUE), GPP_sum=sum(GPP_daily_mean, na.rm=TRUE), 
            ER_avg=mean(ER_daily_mean, na.rm=TRUE), ER_med=median(ER_daily_mean, na.rm=TRUE), ER_max=max(ER_daily_mean, na.rm=TRUE), ER_sum=sum(abs(ER_daily_mean), na.rm=TRUE),
            PR_daily_avg=mean(PR_daily, na.rm=TRUE), PR_daily_med=median(PR_daily, na.rm=TRUE), PR_daily_max=max(PR_daily, na.rm=TRUE), Q_avg=mean(Q_avg, na.rm=TRUE),
            Temp_avg=mean(Temp_avg, na.rm=TRUE),EC_avg=mean(EC_avg, na.rm=TRUE),length=length, EC_med=mean(EC_med, na.rm=TRUE), low_flow=sum(low_flow, na.rm=TRUE), max_flow_date=sum(flow_date, na.rm=TRUE), 
            Qskew_avg=mean(Q_skewness, na.rm=TRUE), Qskewness=skewness(Q_avg, na.rm=TRUE))
Full<-distinct(Full)
Full$LowFlow_Prop<-(Full$low_flow/Full$max_flow_date)
# Full$PR_seas_avg<-(Full$GPP_avg/abs(Full$ER_avg))
# Full$PR_seas_med<-(Full$GPP_med/abs(Full$ER_med))
# Full$PR_seas<-(Full$GPP_sum/Full$ER_sum)

Full$dist_coast[Full$Stream=='Canada']<-12
Full$dist_coast[Full$Stream=='Lost Seal']<-6.8
Full$dist_coast[Full$Stream=='Aiken']<-4.4
Full$dist_coast[Full$Stream=='Von Guerard']<-7.3
Full$dist_coast[Full$Stream=='Delta']<-11.3
Full$dist_coast[Full$Stream=='Green']<-12
Full$dist_coast[Full$Stream=='Commonwealth']<-0
Full$dist_coast[Full$Stream=='Huey']<-10
Full$dist_coast[Full$Stream=='Crescent']<-9.5
Full$dist_coast[Full$Stream=='Harnish']<-8.1

Full$width[Full$Stream=='Canada']<-3.87
Full$width[Full$Stream=='Lost Seal']<-2.35
Full$width[Full$Stream=='Aiken']<-5.32
Full$width[Full$Stream=='Von Guerard']<-1.32
Full$width[Full$Stream=='Delta']<-2.32
Full$width[Full$Stream=='Green']<-5.56
Full$width[Full$Stream=='Commonwealth']<-3.19
Full$width[Full$Stream=='Huey']<-0.95
Full$width[Full$Stream=='Crescent']<-1.83
Full$width[Full$Stream=='Harnish']<-2.82

# Full$max_width[Full$Stream=='Canada']<-5.55
# Full$max_width[Full$Stream=='Lost Seal']<-5.15
# Full$max_width[Full$Stream=='Aiken']<-10.86
# Full$max_width[Full$Stream=='Von Guerard']<-2.16
# Full$max_width[Full$Stream=='Delta']<-2.32
# Full$max_width[Full$Stream=='Green']<-8.69
# Full$max_width[Full$Stream=='Commonwealth']<-5.15
# Full$max_width[Full$Stream=='Huey']<-0.95
# Full$max_width[Full$Stream=='Crescent']<-2.41
# Full$max_width[Full$Stream=='Harnish']<-3.05

Full<-full_join(Full, bio)

# Full<-Full%>%dplyr::filter(., !is.na(GPP_avg))

### For data that is only the "main flow season"
# write.csv(Full, paste0(drive, 'Alldata_MainFlowSeason.csv'), row.names = FALSE)
## For full season data 
# write.csv(Full, paste0(drive, 'Alldata_BothSeasons.csv'), row.names = FALSE)
### if using the algae ops data name it differently here
write.csv(Full, paste0(drive, 'Alldata_BothSeasons_wAlgOps.csv'), row.names = FALSE)

  

