### Seperate script to combine and QAQC the input data for the streamMetabolizer model
### previously all on script, seperated out here to save RAM running the model 

###NOTES/TODO
## PAR data is not finalized to be published yet -- may need to update this in the future
## Need to update with correct 2223 B data

##helpful docs:
## https://github.com/USGS-R/streamMetabolizer/blob/main/vignettes/get_started.Rmd
## https://github.com/USGS-R/streamMetabolizer/blob/main/vignettes/model_structures.Rmd 
## http://usgs-r.github.io/streamMetabolizer/articles/data_prep.html
## http://usgs-r.github.io/streamMetabolizer/index.html 

# -- setup----
library (magrittr)
library (dplyr)
library (ggplot2)
library (tidyr)
library(lubridate)
library(parallel)
library(readxl)
library(padr)
library(zoo)

#to install streamMetabolizer
# http://usgs-r.github.io/streamMetabolizer/articles/installation.html
# library(remotes)
# remotes::install_github('appling/unitted', force=TRUE)
# remotes::install_github("DOI-USGS/streamMetabolizer", build_vignettes = TRUE)
# # In summer or fall 2023, this package will move from
# # https://github.com/USGS-R/streamMetabolizer to
# # https://github.com/DOI-USGS/streamMetabolizer.
# # Please update your links accordingly

library(streamMetabolizer)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##NEED to prep the data
##call metab_inputs() to see what the data should look like 

##input and output location for files
drive<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/2022_2023_data'
outdrive<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/2223 Model Outputs/'

##Read each streams data
##Read in DO, DOsat calc, temp (from gage) and Q data for each stream and combine by DateTime
F1dat<-read.table(paste0(drive, '/2223B data/F1_Canada (cr1000x)_F1_Canada.dat'), sep=",", skip=1, header=TRUE)
F1dat<-F1dat[-c(1, 2, 3), ]
F1dat$DateTime<-lubridate::ymd_hms(F1dat$`TIMESTAMP`)
F1dat<-F1dat%>%
  dplyr::filter(., DateTime<'2023-03-01')
F1dat$Stream<-'Canada'
F1dat$DO_conc<-as.numeric(F1dat$DO_conc)
F1sat<-read.csv(paste0(drive, '/', 'F1_DOsatcalc.csv'))
F1sat$DateTime<-paste(F1sat$DateTime, '00:00:00')
F1sat$DateTime<-substr(F1sat$DateTime, 1, 19)
F1sat<-F1sat%>%
  dplyr::select(., DateTime, Stream, DOsat)
F1sat$DateTime<-lubridate::ymd_hms(F1sat$DateTime)
F1dat<-full_join(F1sat, F1dat)
#IF YOU WANT TO INCUDE Q
F1Q1<-read.csv(paste0(drive, '/F1_2223A_SUBM.csv'))
F1Q1<-F1Q1[-c(1:16),]
F1Q1$DateTime<-lubridate::mdy_hm(F1Q1$DATE_TIME)
F1Q2<-read.csv(paste0(drive, '/2223B data/F1_2223B_SUBM.csv'))
F1Q2$DateTime<-lubridate::mdy_hm(F1Q2$DATE_TIME)
F1Q<-full_join(F1Q1,F1Q2)
F1Q$DISCHARGE_RATE[is.nan(F1Q$DISCHARGE_RATE)]<-NA
F1Q$WATER_TEMP[is.nan(F1Q$WATER_TEMP)]<-NA
F1_all<-full_join(F1dat, F1Q, by="DateTime")
F1_all$WATER_TEMP[F1_all$WATER_TEMP<0]<-NA
F1_all<-F1_all%>%
  dplyr::filter(., !is.na(DISCHARGE_RATE)&!is.na(WATER_TEMP)&!is.na(DOsat))
F1_all<-pad(F1_all, interval="15 min")
F1_all$DISCHARGE_RATE<-zoo::na.approx(F1_all$DISCHARGE_RATE, na.rm = FALSE)
F1_all$DOsat<-zoo::na.approx(F1_all$DOsat, na.rm=FALSE)
F1_all$WATER_TEMP<-zoo::na.approx(F1_all$WATER_TEMP, na.rm=FALSE) 
### remove dips that are out of the normal variation
F1_all$DO_conc[F1_all$DO_conc<10]<-NA
F1_all$DO_conc<-zoo::na.approx(F1_all$DO_conc, na.rm=FALSE) ## don't want to make up DO data on the end of the data, all others are fine?

##Plot it to see if the DO data looks good, needs any QAQC'ing
F1_all2<-gather(F1_all, Variable, Value, DO_conc, DOsat, WATER_TEMP, DISCHARGE_RATE, DO_conc)
#Plot each stream individually with Q and temp to see what is happening at individual days
F1Plot<-ggplot(F1_all2, aes(x=DateTime, y=Value))+geom_point()+
  facet_grid(`Variable`~ ., scales = "free_y", labeller = label_wrap_gen(10))+
  ggtitle('F1 22-23')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position="blank")+
  theme(text = element_text(size = 40))+
  xlab("Date")
### IF any DO data needs to be removed, do that here

F2dat<-read.table(paste0(drive, '/2223B data/F2_Huey (cr1000x)_F2_Huey.dat'), sep=",", skip=1, header=TRUE)
F2dat<-F2dat[-c(1, 2, 3), ]
F2dat$DateTime<-lubridate::ymd_hms(F2dat$`TIMESTAMP`)
F2dat<-F2dat%>%
  dplyr::filter(., DateTime<'2023-03-01')
F2dat$Stream<-'Huey'
F2dat$DO_conc<-as.numeric(F2dat$DO_conc)
F2sat<-read.csv(paste0(drive, '/', 'F2_DOsatcalc.csv'))
F2sat$DateTime<-paste(F2sat$DateTime, '00:00:00')
F2sat$DateTime<-substr(F2sat$DateTime, 1, 19)
F2sat<-F2sat%>%
  dplyr::select(., DateTime, Stream, DOsat)
F2sat$DateTime<-lubridate::ymd_hms(F2sat$DateTime)
F2dat<-full_join(F2sat, F2dat)
#IF YOU WANT TO INCUDE Q
F2Q1<-read.csv(paste0(drive, '/F2_2223A_SUBM.csv'))
F2Q1<-F2Q1[-c(1:16),]
F2Q1$DateTime<-lubridate::mdy_hm(F2Q1$DATE_TIME)
F2Q2<-read.csv(paste0(drive, '/2223B data/F2_2223B_SUBM.csv'))
F2Q2$DateTime<-lubridate::mdy_hm(F2Q2$DATE_TIME)
F2Q<-full_join(F2Q1,F2Q2)
F2Q$DISCHARGE_RATE[is.nan(F2Q$DISCHARGE_RATE)]<-NA
F2Q$WATER_TEMP[is.nan(F2Q$WATER_TEMP)]<-NA
F2_all<-full_join(F2dat, F2Q, by="DateTime")
F2_all$WATER_TEMP[F2_all$WATER_TEMP<0]<-NA
F2_all$DISCHARGE_RATE[F2_all$DISCHARGE_RATE>20]<-NA
F2_all<-F2_all%>%
  dplyr::filter(., !is.na(DISCHARGE_RATE)&!is.na(WATER_TEMP))
F2_all<-pad(F2_all, interval="15 min")
F2_all$DISCHARGE_RATE<-zoo::na.approx(F2_all$DISCHARGE_RATE, na.rm = FALSE)
F2_all$DOsat<-zoo::na.approx(F2_all$DOsat, na.rm=FALSE)
F2_all$DO_conc<-zoo::na.approx(F2_all$DO_conc, na.rm=FALSE) ## don't want to make up DO data on the end of the data, all others are fine? 
F2_all$WATER_TEMP<-zoo::na.approx(F2_all$WATER_TEMP, na.rm=FALSE) 

##Plot it to see if the DO data looks good, needs any QAQC'ing
F2_all2<-gather(F2_all, Variable, Value, DO_conc, WATER_TEMP, DISCHARGE_RATE, DO_conc)
##Plot each stream individually with Q and temp to see what is happening at individual days
F2Plot<-ggplot(F2_all2, aes(x=DateTime, y=Value))+geom_point()+
  facet_grid(`Variable`~ ., scales = "free_y", labeller = label_wrap_gen(10))+
  ggtitle('F2 22-23')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position="blank")+
  theme(text = element_text(size = 40))+
  xlab("Date")
# ### IF any DO data needs to be removed, do that here 

F3dat<-read.table(paste0(drive, '/2223B data/F3_Lost Seal (cr1000x)_F3_LostSeal.dat'), sep=",", skip=1, header=TRUE)
F3dat<-F3dat[-c(1, 2, 3), ]
F3dat$DateTime<-lubridate::ymd_hms(F3dat$`TIMESTAMP`)
F3dat<-F3dat%>%
  dplyr::filter(., DateTime<'2023-03-01')
F3dat$Stream<-'Lost Seal'
F3dat$DO_conc<-as.numeric(F3dat$DO_conc)
F3sat<-read.csv(paste0(drive, '/', 'F3_DOsatcalc.csv'))
F3sat$DateTime<-paste(F3sat$DateTime, '00:00:00')
F3sat$DateTime<-substr(F3sat$DateTime, 1, 19)
F3sat<-F3sat%>%
  dplyr::select(., DateTime, Stream, DOsat)
F3sat$DateTime<-lubridate::ymd_hms(F3sat$DateTime)
F3dat<-full_join(F3sat, F3dat)
#IF YOU WANT TO INCUDE Q
F3Q1<-read.csv(paste0(drive, '/F3_2223A_SUBM.csv'))
F3Q1<-F3Q1[-c(1:16),]
F3Q1$DateTime<-lubridate::mdy_hm(F3Q1$DATE_TIME)
F3Q2<-read.csv(paste0(drive, '/2223B data/F3_2223B_SUBM.csv'))
F3Q2$DateTime<-lubridate::mdy_hm(F3Q2$DATE_TIME)
F3Q<-full_join(F3Q1,F3Q2)
F3Q$DISCHARGE_RATE[is.nan(F3Q$DISCHARGE_RATE)]<-NA
F3Q$WATER_TEMP[is.nan(F3Q$WATER_TEMP)]<-NA
F3_all<-full_join(F3dat, F3Q, by="DateTime")
F3_all$WATER_TEMP[F3_all$WATER_TEMP<0]<-NA
F3_all<-F3_all%>%
  dplyr::filter(., !is.na(DISCHARGE_RATE)&!is.na(WATER_TEMP))
F3_all<-pad(F3_all, interval="15 min")
F3_all$DISCHARGE_RATE<-zoo::na.approx(F3_all$DISCHARGE_RATE, na.rm = FALSE)
F3_all$DOsat<-zoo::na.approx(F3_all$DOsat, na.rm=FALSE)
F3_all$DO_conc<-zoo::na.approx(F3_all$DO_conc, na.rm=FALSE) ## don't want to make up DO data on the end of the data, all others are fine? 
F3_all$WATER_TEMP<-zoo::na.approx(F3_all$WATER_TEMP, na.rm=FALSE) 
## remove any DO drops below 9 when the DO probe was buried 
F3_all$DO_conc[F3_all$DO_conc<9]<-NA


# ##Plot it to see if the DO data looks good, needs any QAQC'ing
F3_all2<-gather(F3_all, Variable, Value, DO_conc, WATER_TEMP, DISCHARGE_RATE, DO_conc)
##Plot each stream individually with Q and temp to see what is happening at individual days
F3Plot<-ggplot(F3_all2, aes(x=DateTime, y=Value))+geom_point()+
  facet_grid(`Variable`~ ., scales = "free_y", labeller = label_wrap_gen(10))+
  ggtitle('F3 22-23')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position="blank")+
  theme(text = element_text(size = 40))+
  xlab("Date")
### IF any DO data needs to be removed, do that here

F5dat<-read.table(paste0(drive, '/2223B data/F5_Aiken (cr1000x)_F5_Aiken.dat'), sep=",", skip=1, header=TRUE)
F5dat<-F5dat[-c(1, 2, 3), ]
F5dat$DateTime<-lubridate::ymd_hms(F5dat$`TIMESTAMP`)
F5dat<-F5dat%>%
  dplyr::filter(., DateTime<'2023-03-01')
F5dat$Stream<-'Aiken'
F5dat$DO_conc<-as.numeric(F5dat$DO_conc)
F5sat<-read.csv(paste0(drive, '/', 'F5_DOsatcalc.csv'))
F5sat$DateTime<-paste(F5sat$DateTime, '00:00:00')
F5sat$DateTime<-substr(F5sat$DateTime, 1, 19)
F5sat<-F5sat%>%
  dplyr::select(., DateTime, Stream, DOsat)
F5sat$DateTime<-lubridate::ymd_hms(F5sat$DateTime)
F5dat<-full_join(F5sat, F5dat)
#IF YOU WANT TO INCUDE Q
F5Q1<-read.csv(paste0(drive, '/F5_2223A_SUBM.csv'))
F5Q1<-F5Q1[-c(1:16),]
F5Q1$DateTime<-lubridate::mdy_hm(F5Q1$DATE_TIME)
F5Q2<-read.csv(paste0(drive, '/2223B data/F5_2223B_SUBM.csv'))
F5Q2$DateTime<-lubridate::mdy_hm(F5Q2$DATE_TIME)
F5Q<-full_join(F5Q1,F5Q2)
F5Q$DISCHARGE_RATE[is.nan(F5Q$DISCHARGE_RATE)]<-NA
F5Q$WATER_TEMP[is.nan(F5Q$WATER_TEMP)]<-NA
F5_all<-full_join(F5dat, F5Q, by="DateTime")
F5_all$WATER_TEMP[F5_all$WATER_TEMP<0]<-NA
F5_all<-F5_all%>%
  dplyr::filter(., !is.na(DISCHARGE_RATE)&!is.na(WATER_TEMP))
F5_all<-pad(F5_all, interval="15 min")
F5_all$DISCHARGE_RATE<-zoo::na.approx(F5_all$DISCHARGE_RATE, na.rm = FALSE)
F5_all$DOsat<-zoo::na.approx(F5_all$DOsat, na.rm=FALSE)
F5_all$DO_conc<-zoo::na.approx(F5_all$DO_conc, na.rm=FALSE) ## don't want to make up DO data on the end of the data, all others are fine? 
F5_all$WATER_TEMP<-zoo::na.approx(F5_all$WATER_TEMP, na.rm=FALSE) 
## remove any DO drops below 9 when the DO probe was buried 
F5_all$DO_conc[F5_all$DO_conc<8]<-NA

# ##Plot it to see if the DO data looks good, needs any QAQC'ing
F5_all2<-gather(F5_all, Variable, Value, DO_conc, WATER_TEMP, DISCHARGE_RATE, DO_conc)
##Plot each stream individually with Q and temp to see what is happening at individual days
F5Plot<-ggplot(F5_all2, aes(x=DateTime, y=Value))+geom_point()+
  facet_grid(`Variable`~ ., scales = "free_y", labeller = label_wrap_gen(10))+
  ggtitle('F5 22-23')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position="blank")+
  theme(text = element_text(size = 40))+
  xlab("Date")
# ### IF any DO data needs to be removed, do that here 

F6dat<-read.table(paste0(drive, '/2223B data/F6_Von Guerard (cr1000x)_F6_VonGuerard.dat'), sep=",", skip=1, header=TRUE)
F6dat<-F6dat[-c(1, 2, 3), ]
F6dat$DateTime<-lubridate::ymd_hms(F6dat$`TIMESTAMP`)
F6dat<-F6dat%>%
  dplyr::filter(., DateTime<'2023-03-01')
F6dat$Stream<-'Von Guerard'
F6dat$DO_conc<-as.numeric(F6dat$DO_conc)
F6sat<-read.csv(paste0(drive, '/', 'F6_DOsatcalc.csv'))
F6sat$DateTime<-paste(F6sat$DateTime, '00:00:00')
F6sat$DateTime<-substr(F6sat$DateTime, 1, 19)
F6sat<-F6sat%>%
  dplyr::select(., DateTime, Stream, DOsat)
F6sat$DateTime<-lubridate::ymd_hms(F6sat$DateTime)
F6dat<-full_join(F6sat, F6dat)
#IF YOU WANT TO INCUDE Q
F6Q1<-read.csv(paste0(drive, '/F6_2223A_SUBM.csv'))
F6Q1<-F6Q1[-c(1:16),]
F6Q1$DateTime<-lubridate::mdy_hm(F6Q1$DATE_TIME)
F6Q2<-read.csv(paste0(drive, '/2223B data/F6_2223B_SUBM.csv'))
F6Q2$DateTime<-lubridate::mdy_hm(F6Q2$DATE_TIME)
F6Q<-full_join(F6Q1,F6Q2)
F6Q$DISCHARGE_RATE[is.nan(F6Q$DISCHARGE_RATE)]<-NA
F6Q$DISCHARGE_RATE[is.na(F6Q$DISCHARGE_RATE)]<-0
F6Q$WATER_TEMP[is.nan(F6Q$WATER_TEMP)]<-NA
F6_all<-full_join(F6dat, F6Q, by="DateTime")
F6_all$WATER_TEMP[F6_all$WATER_TEMP<0]<-NA
F6_all<-F6_all%>%
  dplyr::filter(., DateTime>'2022-12-02 00:00:00')
# F6_all<-F6_all%>%
#   dplyr::filter(., !is.na(DISCHARGE_RATE)&!is.na(WATER_TEMP))
F6_all<-pad(F6_all, interval="15 min")
# F6_all$DISCHARGE_RATE[F6_all$DateTime>'2022-12-28 23:00:00'&F6_all$DateTime<'2023-01-11 00:00:00']<-0
F6_all$DISCHARGE_RATE<-zoo::na.approx(F6_all$DISCHARGE_RATE, na.rm = FALSE)
F6_all$DOsat<-zoo::na.approx(F6_all$DOsat, na.rm=FALSE)
F6_all$DO_conc<-zoo::na.approx(F6_all$DO_conc, na.rm=FALSE) ## don't want to make up DO data on the end of the data, all others are fine? 
F6_all$WATER_TEMP<-zoo::na.approx(F6_all$WATER_TEMP, na.rm=FALSE) 

# ##Plot it to see if the DO data looks good, needs any QAQC'ing
F6_all2<-gather(F6_all, Variable, Value, DO_conc, WATER_TEMP, DISCHARGE_RATE, DO_conc)
##Plot each stream individually with Q and temp to see what is happening at individual days
F6Plot<-ggplot(F6_all2, aes(x=DateTime, y=Value))+geom_point()+
  facet_grid(`Variable`~ ., scales = "free_y", labeller = label_wrap_gen(10))+
  ggtitle('F6 22-23')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position="blank")+
  theme(text = element_text(size = 40))+
  xlab("Date")
# ### IF any DO data needs to be removed, do that here 

F7dat<-read.table(paste0(drive, '/2223B data/F7_Harnish (cr1000x)_F7_Harnish.dat'), sep=",", skip=1, header=TRUE)
F7dat<-F7dat[-c(1, 2, 3), ]
F7dat$DateTime<-lubridate::ymd_hms(F7dat$`TIMESTAMP`)
F7dat<-F7dat%>%
  dplyr::filter(., DateTime<'2023-03-01')
F7dat$Stream<-'Harnish'
F7dat$DO_conc<-as.numeric(F7dat$DO_conc)
F7sat<-read.csv(paste0(drive, '/', 'F7_DOsatcalc.csv'))
F7sat$DateTime<-paste(F7sat$DateTime, '00:00:00')
F7sat$DateTime<-substr(F7sat$DateTime, 1, 19)
F7sat<-F7sat%>%
  dplyr::select(., DateTime, Stream, DOsat)
F7sat$DateTime<-lubridate::ymd_hms(F7sat$DateTime)
F7dat<-full_join(F7sat, F7dat)
#IF YOU WANT TO INCUDE Q
F7Q1<-read.csv(paste0(drive, '/F7_2223A_SUBM.csv'))
F7Q1<-F7Q1[-c(1:16),]
F7Q1$DateTime<-lubridate::mdy_hm(F7Q1$DATE_TIME)
F7Q2<-read.csv(paste0(drive, '/2223B data/F7_2223B_SUBM.csv'))
F7Q2$DateTime<-lubridate::mdy_hm(F7Q2$DATE_TIME)
F7Q<-full_join(F7Q1,F7Q2)
F7Q$DISCHARGE_RATE[is.nan(F7Q$DISCHARGE_RATE)]<-NA
F7Q$WATER_TEMP[is.nan(F7Q$WATER_TEMP)]<-NA
F7_all<-full_join(F7dat, F7Q, by="DateTime")
F7_all$WATER_TEMP[F7_all$WATER_TEMP<0]<-0
F7_all<-F7_all%>%
  dplyr::filter(., !is.na(DISCHARGE_RATE)&!is.na(WATER_TEMP))
F7_all<-pad(F7_all, interval="15 min")
F7_all$DISCHARGE_RATE[F7_all$DateTime>'2022-12-28 23:00:00'&F7_all$DateTime<'2023-01-11 00:00:00']<-0
F7_all$DISCHARGE_RATE<-zoo::na.approx(F7_all$DISCHARGE_RATE, na.rm = FALSE)
F7_all$DOsat<-zoo::na.approx(F7_all$DOsat, na.rm=FALSE)
F7_all$DO_conc<-zoo::na.approx(F7_all$DO_conc, na.rm=FALSE) ## don't want to make up DO data on the end of the data, all others are fine?
F7_all$WATER_TEMP<-zoo::na.approx(F7_all$WATER_TEMP, na.rm=FALSE)
F7_all$WATER_TEMP[F7_all$DateTime>'2022-12-28 23:00:00'&F7_all$DateTime<'2023-01-11 00:00:00']<-NA
F7_all$DO_conc[F7_all$DateTime>'2022-12-28 23:00:00'&F7_all$DateTime<'2023-01-11 00:00:00']<-NA
## remove any DO drops below 9 when the DO probe was buried 
F7_all$DO_conc[F7_all$DO_conc<9]<-NA

# ##Plot it to see if the DO data looks good, needs any QAQC'ing
F7_all2<-gather(F7_all, Variable, Value, DO_conc, WATER_TEMP, DISCHARGE_RATE)
##Plot each stream individually with Q and temp to see what is happening at individual days
F7Plot<-ggplot(F7_all2, aes(x=DateTime, y=Value))+geom_point()+
  facet_grid(`Variable`~ ., scales = "free_y", labeller = label_wrap_gen(10))+
  ggtitle('F7 22-23')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position="blank")+
  theme(text = element_text(size = 40))+
  xlab("Date")
# ### IF any DO data needs to be removed, do that here 

F8dat<-read.table(paste0(drive, '/2223B data/F8_Crescent (cr1000x)_F8_Crescent.dat'), sep=",", skip=1, header=TRUE)
F8dat<-F8dat[-c(1, 2, 3), ]
F8dat$DateTime<-lubridate::ymd_hms(F8dat$`TIMESTAMP`)
F8dat<-F8dat%>%
  dplyr::filter(., DateTime<'2023-03-01')
F8dat$Stream<-'Crescent'
F8dat$DO_conc<-as.numeric(F8dat$DO_conc)
F8sat<-read.csv(paste0(drive, '/', 'F8_DOsatcalc.csv'))
F8sat$DateTime<-paste(F8sat$DateTime, '00:00:00')
F8sat$DateTime<-substr(F8sat$DateTime, 1, 19)
F8sat<-F8sat%>%
  dplyr::select(., DateTime, Stream, DOsat)
F8sat$DateTime<-lubridate::ymd_hms(F8sat$DateTime)
F8dat<-full_join(F8sat, F8dat)
#IF YOU WANT TO INCUDE Q
F8Q1<-read.csv(paste0(drive, '/F8_2223A_SUBM.csv'))
F8Q1<-F8Q1[-c(1:16),]
F8Q1$DateTime<-lubridate::mdy_hm(F8Q1$DATE_TIME)
F8Q2<-read.csv(paste0(drive, '/2223B data/F8_2223B_SUBM.csv'))
F8Q2$DateTime<-lubridate::mdy_hm(F8Q2$DATE_TIME)
F8Q<-full_join(F8Q1,F8Q2)
F8Q$DISCHARGE_RATE[is.nan(F8Q$DISCHARGE_RATE)]<-NA
F8Q$WATER_TEMP[is.nan(F8Q$WATER_TEMP)]<-NA
F8_all<-full_join(F8dat, F8Q, by="DateTime")
F8_all$WATER_TEMP[F8_all$WATER_TEMP<0]<-0
F8_all<-F8_all%>%
  dplyr::filter(., !is.na(DISCHARGE_RATE)&!is.na(WATER_TEMP))
F8_all<-pad(F8_all, interval="15 min")
F8_all$DISCHARGE_RATE<-zoo::na.approx(F8_all$DISCHARGE_RATE, na.rm = FALSE)
F8_all$DOsat<-zoo::na.approx(F8_all$DOsat, na.rm=FALSE)
F8_all$DO_conc<-zoo::na.approx(F8_all$DO_conc, na.rm=FALSE) ## don't want to make up DO data on the end of the data, all others are fine? 
F8_all$WATER_TEMP<-zoo::na.approx(F8_all$WATER_TEMP, na.rm=FALSE) 

# ##Plot it to see if the DO data looks good, needs any QAQC'ing
F8_all2<-gather(F8_all, Variable, Value, DO_conc, WATER_TEMP, DISCHARGE_RATE, DO_conc)
##Plot each stream individually with Q and temp to see what is happening at individual days
F8Plot<-ggplot(F8_all2, aes(x=DateTime, y=Value))+geom_point()+
  facet_grid(`Variable`~ ., scales = "free_y", labeller = label_wrap_gen(10))+
  ggtitle('F8 22-23')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position="blank")+
  theme(text = element_text(size = 40))+
  xlab("Date")
### IF any DO data needs to be removed, do that here

F9dat<-read.table(paste0(drive, '/2223B data/F9_Green (cr1000x)_F9_Green.dat'), sep=",", skip=1, header=TRUE)
F9dat<-F9dat[-c(1, 2, 3), ]
F9dat$DateTime<-lubridate::ymd_hms(F9dat$`TIMESTAMP`)
F9dat<-F9dat%>%
  dplyr::filter(., DateTime<'2023-03-01')
F9dat$Stream<-'Green'
F9dat$DO_conc<-as.numeric(F9dat$DO_conc)
F9sat<-read.csv(paste0(drive, '/', 'F9_DOsatcalc.csv'))
F9sat$DateTime<-paste(F9sat$DateTime, '00:00:00')
F9sat$DateTime<-substr(F9sat$DateTime, 1, 19)
F9sat<-F9sat%>%
  dplyr::select(., DateTime, Stream, DOsat)
F9sat$DateTime<-lubridate::ymd_hms(F9sat$DateTime)
F9dat<-full_join(F9sat, F9dat)
#IF YOU WANT TO INCUDE Q
F9Q1<-read.csv(paste0(drive, '/F9_2223A_SUBM.csv'))
F9Q1<-F9Q1[-c(1:16),]
F9Q1$DateTime<-lubridate::mdy_hm(F9Q1$DATE_TIME)
F9Q2<-read.csv(paste0(drive, '/2223B data/F9_2223B_SUBM.csv'))
F9Q2$DateTime<-lubridate::mdy_hm(F9Q2$DATE_TIME)
F9Q<-full_join(F9Q1,F9Q2)
F9Q$DISCHARGE_RATE[is.nan(F9Q$DISCHARGE_RATE)]<-NA
F9Q$WATER_TEMP[is.nan(F9Q$WATER_TEMP)]<-NA
F9_all<-full_join(F9dat, F9Q, by="DateTime")
F9_all$WATER_TEMP[F9_all$WATER_TEMP<0]<-0
F9_all<-F9_all%>%
  dplyr::filter(., !is.na(DISCHARGE_RATE)&!is.na(WATER_TEMP))
F9_all<-pad(F9_all, interval="15 min")
F9_all$DISCHARGE_RATE[F9_all$DateTime>'2023-01-17 13:15:00'&F9_all$DateTime<'2023-01-27 13:30:00']<-NA
F9_all$DISCHARGE_RATE<-zoo::na.approx(F9_all$DISCHARGE_RATE, na.rm = FALSE)
F9_all$DOsat<-zoo::na.approx(F9_all$DOsat, na.rm=FALSE)
F9_all$DO_conc<-zoo::na.approx(F9_all$DO_conc, na.rm=FALSE) ## don't want to make up DO data on the end of the data, all others are fine? 
F9_all$WATER_TEMP<-zoo::na.approx(F9_all$WATER_TEMP, na.rm=FALSE) 
F9_all$WATER_TEMP[F9_all$DateTime>'2023-01-17 13:15:00'&F9_all$DateTime<'2023-01-27 13:30:00']<-NA
F9_all$DO_conc[F9_all$DateTime>'2023-01-17 13:15:00'&F9_all$DateTime<'2023-01-27 13:30:00']<-NA

##calculate DO sat for plotting
F9_all$DOsat_pct=(F9_all$DO_conc/F9_all$DOsat *100)
# ##Filter out dates to plot
# F9_all<-F9_all%>%
#   dplyr::filter(., DateTime>'2022-12-05 00:00'&DateTime<'2023-01-01 00:00')

# ##Plot it to see if the DO data looks good, needs any QAQC'ing
F9_all2<-gather(F9_all, Variable, Value, DO_conc, WATER_TEMP, DISCHARGE_RATE, DOsat_pct)
##Plot each stream individually with Q and temp to see what is happening at individual days
F9Plot<-ggplot(F9_all2, aes(x=DateTime, y=Value, color=Variable))+geom_point()+
  facet_grid(`Variable`~ ., scales = "free_y", labeller = label_wrap_gen(10))+
  ggtitle('')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position="blank")+
  theme(text = element_text(size = 40))+
  xlab("Date")
# ggsave(F9Plot, filename="F9_2223_plot.jpeg", device="jpeg", path=paste0('/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/Plots/'), width = 15, height = 12)
# ### IF any DO data needs to be removed, do that here 

F10dat<-read.table(paste0(drive, '/2223B data/F10_Delta (cr1000x)_F10_Delta.dat'), sep=",", skip=1, header=TRUE)
F10dat<-F10dat[-c(1, 2, 3), ]
F10dat$DateTime<-lubridate::ymd_hms(F10dat$`TIMESTAMP`)
F10dat<-F10dat%>%
  dplyr::filter(., DateTime<'2023-03-01')
F10dat$Stream<-'Delta'
F10dat$DO_conc<-as.numeric(F10dat$DO_conc)
F10sat<-read.csv(paste0(drive, '/', 'F10_DOsatcalc.csv'))
F10sat$DateTime<-paste(F10sat$DateTime, '00:00:00')
F10sat$DateTime<-substr(F10sat$DateTime, 1, 19)
F10sat<-F10sat%>%
  dplyr::select(., DateTime, Stream, DOsat)
F10sat$DateTime<-lubridate::ymd_hms(F10sat$DateTime)
F10dat<-full_join(F10sat, F10dat)
#IF YOU WANT TO INCUDE Q
F10Q1<-read.csv(paste0(drive, '/F10_2223A_SUBM.csv'))
F10Q1<-F10Q1[-c(1:16),]
F10Q1$DateTime<-lubridate::mdy_hm(F10Q1$DATE_TIME)
F10Q2<-read.csv(paste0(drive, '/2223B data/F10_2223B_SUBM.csv'))
F10Q2$DateTime<-lubridate::mdy_hm(F10Q2$DATE_TIME)
F10Q<-full_join(F10Q1,F10Q2)
F10Q$DISCHARGE_RATE[is.nan(F10Q$DISCHARGE_RATE)]<-NA
F10Q$WATER_TEMP[is.nan(F10Q$WATER_TEMP)]<-NA
F10_all<-full_join(F10dat, F10Q, by="DateTime")
F10_all$WATER_TEMP[F10_all$WATER_TEMP<0]<-NA
F10_all<-F10_all%>%
  dplyr::filter(., !is.na(DISCHARGE_RATE)&!is.na(WATER_TEMP))
F10_all<-pad(F10_all, interval="15 min")
F10_all$DISCHARGE_RATE<-zoo::na.approx(F10_all$DISCHARGE_RATE, na.rm = FALSE)
F10_all$DOsat<-zoo::na.approx(F10_all$DOsat, na.rm=FALSE)
F10_all$DO_conc<-zoo::na.approx(F10_all$DO_conc, na.rm=FALSE) ## don't want to make up DO data on the end of the data, all others are fine? 
F10_all$WATER_TEMP<-zoo::na.approx(F10_all$WATER_TEMP, na.rm=FALSE) 
## remove any DO drops below 9 when the DO probe was buried 
F10_all$DO_conc[F10_all$DO_conc<9]<-NA

# ##Plot it to see if the DO data looks good, needs any QAQC'ing
F10_all2<-gather(F10_all, Variable, Value, DO_conc, WATER_TEMP, DISCHARGE_RATE, DO_conc)
##Plot each stream individually with Q and temp to see what is happening at individual days
F10Plot<-ggplot(F10_all2, aes(x=DateTime, y=Value))+geom_point()+
  facet_grid(`Variable`~ ., scales = "free_y", labeller = label_wrap_gen(10))+
  ggtitle('F10 22-23')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position="blank")+
  theme(text = element_text(size = 40))+
  xlab("Date")
### IF any DO data needs to be removed, do that here

C1dat<-read.table(paste0(drive, '/2223B data/C1_Commonwealth (cr1000x)_C1_Commonwealth.dat'), sep=",", skip=1, header=TRUE)
C1dat<-C1dat[-c(1, 2, 3), ]
C1dat$DateTime<-lubridate::ymd_hms(C1dat$`TIMESTAMP`)
C1dat<-C1dat%>%
  dplyr::filter(., DateTime<'2023-03-01')
C1dat$Stream<-'Commonwealth'
C1dat$DO_conc<-as.numeric(C1dat$DO_conc)
C1sat<-read.csv(paste0(drive, '/', 'C1_DOsatcalc.csv'))
C1sat$DateTime<-paste(C1sat$DateTime, '00:00:00')
C1sat$DateTime<-substr(C1sat$DateTime, 1, 19)
C1sat<-C1sat%>%
  dplyr::select(., DateTime, Stream, DOsat)
C1sat$DateTime<-lubridate::ymd_hms(C1sat$DateTime)
C1dat<-full_join(C1sat, C1dat)
#IF YOU WANT TO INCUDE Q
C1Q1<-read.csv(paste0(drive, '/C1_2223A_SUBM.csv'))
C1Q1<-C1Q1[-c(1:16),]
C1Q1$DateTime<-lubridate::mdy_hm(C1Q1$DATE_TIME)
C1Q2<-read.csv(paste0(drive, '/2223B data/C1_2223B_SUBM.csv'))
C1Q2$DateTime<-lubridate::mdy_hm(C1Q2$DATE_TIME)
C1Q<-full_join(C1Q1,C1Q2)
C1Q$DISCHARGE_RATE[is.nan(C1Q$DISCHARGE_RATE)]<-NA
C1Q$WATER_TEMP[is.nan(C1Q$WATER_TEMP)]<-NA
C1_all<-full_join(C1dat, C1Q, by="DateTime")
C1_all$WATER_TEMP[C1_all$WATER_TEMP<0]<-NA
C1_all<-C1_all%>%
  dplyr::filter(., DateTime>'2022-12-01 00:00')
C1_all<-C1_all%>%
  dplyr::filter(., !is.na(DISCHARGE_RATE)&!is.na(WATER_TEMP))
C1_all<-pad(C1_all, interval="15 min")
C1_all$DISCHARGE_RATE<-zoo::na.approx(C1_all$DISCHARGE_RATE, na.rm = FALSE)
C1_all$DOsat<-zoo::na.approx(C1_all$DOsat, na.rm=FALSE)
C1_all$DO_conc<-zoo::na.approx(C1_all$DO_conc, na.rm=FALSE) ## don't want to make up DO data on the end of the data, all others are fine?
C1_all$WATER_TEMP<-zoo::na.approx(C1_all$WATER_TEMP, na.rm=FALSE)

# ##Plot it to see if the DO data looks good, needs any QAQC'ing
C1_all2<-gather(C1_all, Variable, Value, DO_conc, WATER_TEMP, DISCHARGE_RATE, DO_conc)
##Plot each stream individually with Q and temp to see what is happening at individual days
C1Plot<-ggplot(C1_all2, aes(x=DateTime, y=Value))+geom_point()+
  facet_grid(`Variable`~ ., scales = "free_y", labeller = label_wrap_gen(10))+
  ggtitle('C1 22-23')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position="blank")+
  theme(text = element_text(size = 40))+
  xlab("Date")
### IF any DO data needs to be removed, do that here

##Use processed PAR from Renee/ 
## most streams use Fryxell Met PAR
##the PAR data was combined from the website and from Renee/Hilary to fill the early season record
PAR<-read.csv(paste0(drive, '/FRLM_pressure_PAR.csv'), stringsAsFactors=FALSE)
PAR$DateTime<-lubridate::mdy_hm(PAR$TIMESTAMP)
# PAR<-PAR%>%
#   dplyr::rename(., PAR=`Processed.PAR`)
##pad the PAR data out
PAR<-pad(PAR, interval="15 min")
PAR$PAR<-zoo::na.approx(PAR$PAR, na.rm = TRUE) ## don't want to add data on either end, so keep leading and ending NAs

# OXSatData2<-full_join(OXSatData, PAR, by="DateTime")
F1all<-full_join(F1_all, PAR, by="DateTime")
F2all<-full_join(F2_all, PAR, by="DateTime")
F3all<-full_join(F3_all, PAR, by="DateTime")
F5all<-full_join(F5_all, PAR, by="DateTime")
F6all<-full_join(F6_all, PAR, by="DateTime")
F7all<-full_join(F7_all, PAR, by="DateTime")
F8all<-full_join(F8_all, PAR, by="DateTime")
F9all<-full_join(F9_all, PAR, by="DateTime")
F10all<-full_join(F10_all, PAR, by="DateTime")
C1all<-full_join(C1_all, PAR, by="DateTime")

##put Q into m3/s
F1all$discharge_m3s<-(F1all$DISCHARGE_RATE/1000)
F2all$discharge_m3s<-(F2all$DISCHARGE_RATE/1000)
F3all$discharge_m3s<-(F3all$DISCHARGE_RATE/1000)
F5all$discharge_m3s<-(F5all$DISCHARGE_RATE/1000)
F6all$discharge_m3s<-(F6all$DISCHARGE_RATE/1000)
F7all$discharge_m3s<-(F7all$DISCHARGE_RATE/1000)
F8all$discharge_m3s<-(F8all$DISCHARGE_RATE/1000)
F9all$discharge_m3s<-(F9all$DISCHARGE_RATE/1000)
F10all$discharge_m3s<-(F10all$DISCHARGE_RATE/1000)
C1all$discharge_m3s<-(C1all$DISCHARGE_RATE/1000)

##For depth, use depth~Q relationship developed for each stream (in excel) with log-log relationship and power equations for each
F1all$depth=(0.2978*F1all$DISCHARGE_RATE^0.1504)
F2all$depth=0.05
F3all$depth=(0.0526*F3all$DISCHARGE_RATE^0.2576)
F5all$depth=(0.0405*F5all$DISCHARGE_RATE^0.2828)
F6all$depth=(0.0404*F6all$DISCHARGE_RATE^0.3762) 
F7all$depth=(0.0364*F7all$DISCHARGE_RATE^0.2927) 
F8all$depth=(0.0341*F8all$DISCHARGE_RATE^0.2284)
F9all$depth=(0.055*F9all$DISCHARGE_RATE^0.2751) 
F10all$depth=(0.0404*F10all$DISCHARGE_RATE^0.3183)
C1all$depth=(0.0557*C1all$DISCHARGE_RATE^0.2818) 

F1_Metab<-F1all%>%
  dplyr::filter(., DateTime<'2023-03-01 00:00:00')%>%
  dplyr::rename(., `DO.obs`=`DO_conc`)%>%
  dplyr::rename(., `DO.sat`=`DOsat`)%>%
  dplyr::rename(., `temp.water`=`WATER_TEMP`)%>%
  dplyr::rename(., `light`=`PAR`)%>%
  dplyr::rename(., `discharge`=`discharge_m3s`)%>%
  # dplyr::rename(., `depth`=`stage_m`)%>%
  dplyr::rename(., `solar.time`=DateTime)%>%
  dplyr::select(., solar.time, DO.obs, DO.sat, temp.water, light, discharge, depth)
  # dplyr::filter(., !is.na(`DO.obs`))
#   dplyr::filter(., !is.na(`discharge`))
# F1_Metab<-F1_Metab%>%
#   dplyr::filter(., `DO.obs`>0)%>%
#   dplyr::filter(., `DO.sat`>0)
##write out all data to start from here and save memory
##Fill in depth and discharge for temporary purposes
# F1_Metab$discharge <- zoo::na.locf(F1_Metab$discharge, na.rm=FALSE)
# F1_Metab$depth <- zoo::na.locf(F1_Metab$depth, na.rm=FALSE)
write.csv(F1_Metab, paste0(drive, "/F1_sM_inputs.csv"), na = "")

F2_Metab<-F2all%>%
  dplyr::rename(., `DO.obs`=`DO_conc`)%>%
  dplyr::rename(., `DO.sat`=`DOsat`)%>%
  dplyr::rename(., `temp.water`=`WATER_TEMP`)%>%
  dplyr::rename(., `light`=`PAR`)%>%
  dplyr::rename(., `discharge`=`discharge_m3s`)%>%
  # dplyr::rename(., `depth`=`stage_m`)%>%
  dplyr::rename(., `solar.time`=DateTime)%>%
  dplyr::select(., solar.time, DO.obs, DO.sat, temp.water, light, discharge, depth)%>%
  dplyr::filter(., !is.na(`DO.obs`))
#   dplyr::filter(., !is.na(`discharge`))
# F2_Metab<-F2_Metab%>%
#   dplyr::filter(., `DO.obs`>0)%>%
#   dplyr::filter(., `DO.sat`>0)
##write out all data to start from here and save memory
write.csv(F2_Metab, paste0(drive, "/F2_sM_inputs.csv"), na = "")


F3_Metab<-F3all%>%
  dplyr::rename(., `DO.obs`=`DO_conc`)%>%
  dplyr::rename(., `DO.sat`=`DOsat`)%>%
  dplyr::rename(., `temp.water`=`WATER_TEMP`)%>%
  dplyr::rename(., `light`=`PAR`)%>%
  dplyr::rename(., `discharge`=`discharge_m3s`)%>%
  # dplyr::rename(., `depth`=`stage_m`)%>%
  dplyr::rename(., `solar.time`=DateTime)%>%
  dplyr::select(., solar.time, DO.obs, DO.sat, temp.water, light, discharge, depth)%>%
  dplyr::filter(., !is.na(`DO.obs`))
#   dplyr::filter(., !is.na(`discharge`))
# F3_Metab<-F3_Metab%>%
#   dplyr::filter(., `DO.obs`>0)%>%
#   dplyr::filter(., `DO.sat`>0)
##write out all data to start from here and save memory
write.csv(F3_Metab, paste0(drive, "/F3_sM_inputs.csv"), na = "")


F5_Metab<-F5all%>%
  dplyr::rename(., `DO.obs`=`DO_conc`)%>%
  dplyr::rename(., `DO.sat`=`DOsat`)%>%
  dplyr::rename(., `temp.water`=`WATER_TEMP`)%>%
  dplyr::rename(., `light`=`PAR`)%>%
  dplyr::rename(., `discharge`=`discharge_m3s`)%>%
  # dplyr::rename(., `depth`=`stage_m`)%>%
  dplyr::rename(., `solar.time`=DateTime)%>%
  dplyr::select(., solar.time, DO.obs, DO.sat, temp.water, light, discharge, depth)%>%
  dplyr::filter(., !is.na(`DO.obs`))
#   dplyr::filter(., !is.na(`discharge`))
F5_Metab<-F5_Metab[-c(5632, 5634, 5636), ]
# F5_Metab<-F5_Metab%>%
#   dplyr::filter(., `DO.obs`>0)%>%
#   dplyr::filter(., `DO.sat`>0)
##write out all data to start from here and save memory
write.csv(F5_Metab, paste0(drive, "/F5_sM_inputs.csv"), na = "")

F6_Metab<-F6all%>%
  dplyr::rename(., `DO.obs`=`DO_conc`)%>%
  dplyr::rename(., `DO.sat`=`DOsat`)%>%
  dplyr::rename(., `temp.water`=`WATER_TEMP`)%>%
  dplyr::rename(., `light`=`PAR`)%>%
  dplyr::rename(., `discharge`=`discharge_m3s`)%>%
  # dplyr::rename(., `depth`=`stage_m`)%>%
  dplyr::rename(., `solar.time`=DateTime)%>%
  dplyr::select(., solar.time, DO.obs, DO.sat, temp.water, light, discharge, depth)%>%
  dplyr::filter(., !is.na(`DO.obs`))
#   dplyr::filter(., !is.na(`discharge`))
# F6_Metab<-F6_Metab%>%
#   dplyr::filter(., `DO.obs`>0)%>%
#   dplyr::filter(., `DO.sat`>0)
##write out all data to start from here and save memory
write.csv(F6_Metab, paste0(drive, "/F6_sM_inputs.csv"), na = "")

F7_Metab<-F7all%>%
  dplyr::rename(., `DO.obs`=`DO_conc`)%>%
  dplyr::rename(., `DO.sat`=`DOsat`)%>%
  dplyr::rename(., `temp.water`=`WATER_TEMP`)%>%
  dplyr::rename(., `light`=`PAR`)%>%
  dplyr::rename(., `discharge`=`discharge_m3s`)%>%
  # dplyr::rename(., `depth`=`stage_m`)%>%
  dplyr::rename(., `solar.time`=DateTime)%>%
  dplyr::select(., solar.time, DO.obs, DO.sat, temp.water, light, discharge, depth)%>%
  dplyr::filter(., !is.na(`DO.obs`))
#   dplyr::filter(., !is.na(`discharge`))
# F7_Metab<-F7_Metab%>%
#   dplyr::filter(., `DO.obs`>0)%>%
#   dplyr::filter(., `DO.sat`>0)
##write out all data to start from here and save memory
write.csv(F7_Metab, paste0(drive, "/F7_sM_inputs.csv"), na = "")

F8_Metab<-F8all%>%
  dplyr::rename(., `DO.obs`=`DO_conc`)%>%
  dplyr::rename(., `DO.sat`=`DOsat`)%>%
  dplyr::rename(., `temp.water`=`WATER_TEMP`)%>%
  dplyr::rename(., `light`=`PAR`)%>%
  dplyr::rename(., `discharge`=`discharge_m3s`)%>%
  # dplyr::rename(., `depth`=`stage_m`)%>%
  dplyr::rename(., `solar.time`=DateTime)%>%
  dplyr::select(., solar.time, DO.obs, DO.sat, temp.water, light, discharge, depth)%>%
  dplyr::filter(., !is.na(`DO.obs`))
#   dplyr::filter(., !is.na(`discharge`))
# F8_Metab<-F8_Metab%>%
#   dplyr::filter(., `DO.obs`>0)%>%
#   dplyr::filter(., `DO.sat`>0)
F8_Metab<-F8_Metab[-c(5632, 5634, 5636), ]
##write out all data to start from here and save memory
write.csv(F8_Metab, paste0(drive, "/F8_sM_inputs.csv"), na = "")

F9_Metab<-F9all%>%
  dplyr::rename(., `DO.obs`=`DO_conc`)%>%
  dplyr::rename(., `DO.sat`=`DOsat`)%>%
  dplyr::rename(., `temp.water`=`WATER_TEMP`)%>%
  dplyr::rename(., `light`=`PAR`)%>%
  dplyr::rename(., `discharge`=`discharge_m3s`)%>%
  # dplyr::rename(., `depth`=`stage_m`)%>%
  dplyr::rename(., `solar.time`=DateTime)%>%
  dplyr::select(., solar.time, DO.obs, DO.sat, temp.water, light, discharge, depth)%>%
  dplyr::filter(., !is.na(`DO.obs`)) ## filter out the end of the PAR data
# dplyr::filter(., !is.na(`discharge`))
### INTERPOLATE THE data to try and address date with little data, do this for each one, pad data into 15 minute intervals and use linear interpolation
##write out all data to start from here and save memory
write.csv(F9_Metab, paste0(drive, "/F9_sM_inputs.csv"), na = "")

F10_Metab<-F10all%>%
  dplyr::rename(., `DO.obs`=`DO_conc`)%>%
  dplyr::rename(., `DO.sat`=`DOsat`)%>%
  dplyr::rename(., `temp.water`=`WATER_TEMP`)%>%
  dplyr::rename(., `light`=`PAR`)%>%
  dplyr::rename(., `discharge`=`discharge_m3s`)%>%
  # dplyr::rename(., `depth`=`stage_m`)%>%
  dplyr::rename(., `solar.time`=DateTime)%>%
  dplyr::select(., solar.time, DO.obs, DO.sat, temp.water, light, discharge, depth)%>%
  dplyr::filter(., !is.na(`DO.obs`))
#   dplyr::filter(., !is.na(`discharge`))
# F10_Metab<-F10_Metab%>%
#   dplyr::filter(., `DO.obs`>0)%>%
#   dplyr::filter(., `DO.sat`>0)\
F10_Metab<-F10_Metab%>%dplyr::arrange(., solar.time)
##write out all data to start from here and save memory
write.csv(F10_Metab, paste0(drive, "/F10_sM_inputs.csv"), na = "")

C1_Metab<-C1all%>%
  dplyr::rename(., `DO.obs`=`DO_conc`)%>%
  dplyr::rename(., `DO.sat`=`DOsat`)%>%
  dplyr::rename(., `temp.water`=`WATER_TEMP`)%>%
  dplyr::rename(., `light`=`PAR`)%>%
  dplyr::rename(., `discharge`=`discharge_m3s`)%>%
  # dplyr::rename(., `depth`=`stage_m`)%>%
  dplyr::rename(., `solar.time`=DateTime)%>%
  dplyr::select(., solar.time, DO.obs, DO.sat, temp.water, light, discharge, depth)%>%
  dplyr::filter(., !is.na(`DO.obs`))%>%
  dplyr::filter(., !is.na(`DO.sat`))
# dplyr::filter(., !is.na(`discharge`))
# C1_Metab<-C1_Metab%>%
#   dplyr::filter(., `DO.obs`>0)%>%
#   dplyr::filter(., `DO.sat`>0)
C1_Metab<-C1_Metab%>%dplyr::arrange(., solar.time)
##write out all data to start from here and save memory
write.csv(C1_Metab, paste0(drive, "/C1_sM_inputs.csv"), na = "")

##plot them to look at the data
F3_Metab %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() +
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')+
  ylim(0, 120)
