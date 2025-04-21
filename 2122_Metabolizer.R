##script to play around with the StreamMetabolism model for MDV streams
##Created by ATW 3/14/22

## data was updated, QAQC'd and combined in 2122_sM_inputdata_QAQC.R script

###NOTES/TODO
#notes
## NEEED TO RE-RUN WITH UPDATES FOR ONYX, INCLUDE UPDATED DEPTH

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
drive<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/2021_2022_data_ATW'
outdrive<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/2122 Model Outputs/'

## read in data that was put together in other R script
###############################################################################################################
F1_Metab<-read.csv(paste0(drive, '/F1_2122_sM_inputs.csv'), stringsAsFactors = FALSE)
F1_Metab<-F1_Metab%>%
  dplyr::select(., -X)
F1_Metab$solar.time<-as_datetime(F1_Metab$solar.time)

F3_Metab<-read.csv(paste0(drive, '/F3_2122_sM_inputs.csv'), stringsAsFactors = FALSE)
F3_Metab<-F3_Metab%>%
  dplyr::select(., -X)
F3_Metab$solar.time<-as_datetime(F3_Metab$solar.time)

F5_Metab<-read.csv(paste0(drive, '/F5_2122_sM_inputs.csv'), stringsAsFactors = FALSE)
F5_Metab<-F5_Metab%>%
  dplyr::select(., -X)
F5_Metab<-F5_Metab[-4962, ]
F5_Metab$solar.time<-as_datetime(F5_Metab$solar.time)

F6_Metab<-read.csv(paste0(drive, '/F6_2122_sM_inputs.csv'), stringsAsFactors = FALSE)
F6_Metab<-F6_Metab%>%
  dplyr::select(., -X)
F6_Metab$solar.time<-as_datetime(F6_Metab$solar.time)

F9_Metab<-read.csv(paste0(drive, '/F9_2122_sM_inputs.csv'), stringsAsFactors = FALSE)
F9_Metab<-F9_Metab%>%
  dplyr::select(., -X)
F9_Metab$solar.time<-as_datetime(F9_Metab$solar.time)

F10_Metab<-read.csv(paste0(drive, '/F10_2122_sM_inputs.csv'), stringsAsFactors = FALSE)
F10_Metab<-F10_Metab%>%
  dplyr::select(., -X)
F10_Metab$solar.time<-as_datetime(F10_Metab$solar.time)

C1_Metab<-read.csv(paste0(drive, '/C1_2122_sM_inputs.csv'), stringsAsFactors = FALSE)
C1_Metab<-C1_Metab%>%
  dplyr::select(., -X)
C1_Metab$solar.time<-as_datetime(C1_Metab$solar.time)

#############################################################
### FROM APPLING ET AL. #######

##a. Identify the name of the model structure you want using mm_name(). 
##b. Set the specifications for the model using defaults fromspecs() as a starting point.

## TYPES OF MODELS
## * `bayes` - Inverse Bayesian modeling of GPP, ER, and K600
## * `mle` - Inverse modeling by maximum likelihood estimation of GPP, ER, and optionally K600
## * `night` - Nighttime regression for estimation of K600 and ER

###BAYESIAN MODEL
## You can see the full list of available model structures by calling `mm_valid_names()
bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE, ode_method='trapezoid')
# bayes_name <- mm_name(type='bayes', pool_K600='linear')
# bayes_name <- mm_name(type='bayes', pool_K600='none', err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_name

###Once a model has been configured, you can fit the model to data with metab(). Bayesian models take a while to run, so be patient. 
#Or switch to an MLE model if you can afford to sacrifice some accuracy for speed.
bayes_specs <- specs(bayes_name)
bayes_specs
## can start with less steps to run and then add more when finalizing the model
## setting priors for GPP and ER? 
# bayes_specs <- specs(bayes_name, burnin_steps=500, saved_steps=100, n_cores=2, n_chains=2, day_start=4, day_end=28)
#From Appling et al. -- this should be the final, longer one  
# bayes_specs <- specs(bayes_name, burnin_steps=1000, saved_steps=500, n_cores=4, n_chains=4, GPP_daily_mu=3.1, ER_daily_mu=-7.1, day_start=4, day_end=28)
## without setting priors for GPP and ER and use 8 and 32 as day start and end based on PAR values (Want peak par roughly 12:00 noon)
bayes_specs <- specs(bayes_name, burnin_steps=1000, saved_steps=500, n_cores=4, n_chains=4, day_start=8, day_end=32)

### set K600 daily using the measured values from 22-23 season
## From email with Yuseung:
## If you have big variation of k600 with discharge, than try priors forcing k600 to each discharge bin.
## For binning method, discharge does not need to be the actual estimates. Arbitrary numbers are okay, so you can change your discharge into ‘1,2,3,…’ if you want to clearly classify discharge bins. 
## If not, sM will do that work with priors of nodes_centers and nodediffs_sdlog.
K600bin <- specs(bayes_name, burnin_steps=1000, saved_steps=500,
                 K600_lnQ_nodes_centers=c(1,2,3), #set the center of each discharge bin
                 K600_lnQ_nodediffs_sdlog=0.05, #not sure why, but we used 0.05 for centers 1 apart
                 K600_lnQ_nodes_meanlog=log(c(4,52,140)), #set priors of k600 for each discharge bin
                 K600_lnQ_nodes_sdlog=0.1, #small enough to constrain k600 estimates, normally 1/10 of mean values
                 K600_daily_sigma_sigma=0.24, # not sure what this is for, but 0.24 from Appling’s papep
                 GPP_daily_lower=0, ER_daily_upper=0, #to force GPP to positive and ER to negative
                 n_cores=4, n_chains=4, day_start=8, day_end=32) 

F1mm <- metab(K600bin, data=F1_Metab)
predictions_F1<- F1mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
head(predictions_F1)
write.csv(predictions_F1, paste0(outdrive, "F1_2122_Metab_predic_K600bin.csv"))

jpeg(file=paste0(outdrive, "F1_2122_metab_preds_K600bin.jpeg"), width = 900, height = 500, res = 150)
plot_metab_preds(F1mm)
dev.off()

F3mm <- metab(K600bin, data=F3_Metab)
predictions_F3<- F3mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
head(predictions_F3)
write.csv(predictions_F3, paste0(outdrive, "F3_2122_Metab_predic_K600bin.csv"))

jpeg(file=paste0(outdrive, "F3_2122_metab_preds_k600bin.jpeg"), width = 900, height = 500, res = 150)
plot_metab_preds(F3mm)
dev.off()

F5mm <- metab(K600bin, data=F5_Metab) 
predictions_F5<- F5mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
head(predictions_F5)
write.csv(predictions_F5, paste0(outdrive, "F5_2122_Metab_predic_K600bin.csv"))

jpeg(file=paste0(outdrive, "F5_2122_metab_preds_K600bin.jpeg"), width = 900, height = 500, res = 150)
plot_metab_preds(F5mm)
dev.off()

F6mm <- metab(K600bin, data=F6_Metab) 
predictions_F6<- F6mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
head(predictions_F6)
write.csv(predictions_F6, paste0(outdrive, "F6_2122_Metab_predic_K600bin.csv"))

jpeg(file=paste0(outdrive, "F6_2122_metab_preds_K600bin.jpeg"), width = 900, height = 500, res = 150)
plot_metab_preds(F6mm)
dev.off()

F9mm <- metab(K600bin, data=F9_Metab)
predictions_F9<- F9mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
head(predictions_F9)
write.csv(predictions_F9, paste0(outdrive, "F9_2122_Metab_predic_K600bin.csv"))

jpeg(file=paste0(outdrive, "F9_2122_metab_preds_K600bin.jpeg"), width = 900, height = 500, res = 150)
plot_metab_preds(F9mm)
dev.off()

F10mm <- metab(K600bin, data=F10_Metab)
predictions_F10<- F10mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
head(predictions_F10)
write.csv(predictions_F10, paste0(outdrive, "F10_2122_Metab_predic_K600bin.csv"))

jpeg(file=paste0(outdrive, "F10_2122_metab_preds_K600bin.jpeg"), width = 900, height = 500, res = 150)
plot_metab_preds(F10mm)
dev.off()

C1mm <- metab(K600bin, data=C1_Metab)
predictions_C1<- C1mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
head(predictions_C1)
write.csv(predictions_C1, paste0(outdrive, "C1_2122_Metab_predi_K600binc.csv"))

jpeg(file=paste0(outdrive, "C1_2122_metab_preds_K600bin.jpeg"), width = 900, height = 500, res = 150)
plot_metab_preds(C1mm)
dev.off()

##Once you've fit a model, you can inspect the output with functions including predict_metab() and 
#plot_metab_preds(), predict_DO() and plot_DO_preds(), get_params(), and get_fit().

predictions_OX<- OXmm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
head(predictions_OX)
write.csv(predictions_OX, paste0(outdrive, "OX_2122_Metab_predic.csv"))

jpeg(file=paste0(outdrive, "OX_2122_metab_preds.jpeg"), width = 900, height = 500, res = 150)
plot_metab_preds(OXmm)
dev.off()

# predict_metab(OXmm)
# plot_metab_preds(OXmm)

predict_DO(C1mm)
plot_DO_preds(C1mm)
get_params(F9mm)
get_info(F9mm)
get_fit(F9mm)

########################### GET INSTANTANEOUS VALUES AS WELL ###################################################
## From Github issue: https://github.com/USGS-R/streamMetabolizer/issues/222
##can already extract inst GPP, ER, and DO_mod from bayesian models if you ask for them ahead of time. below, plot_DO_preds() uses calc_dDOdt rather than mcmc output to compute the brown line, 
##but the black line from bayesian model nonetheless plots right on top, as it should.
F1inst <- metab(revise(bayes_specs, params_out=c(params_out,'GPP_inst', 'ER_inst', 'KO2_inst')), F1_Metab)
F3inst <- metab(revise(bayes_specs, params_out=c(params_out,'GPP_inst', 'ER_inst', 'KO2_inst')),  F3_Metab)
F5inst <- metab(revise(bayes_specs, params_out=c(params_out,'GPP_inst', 'ER_inst', 'KO2_inst')),  F5_Metab)
F6inst <- metab(revise(bayes_specs, params_out=c(params_out,'GPP_inst', 'ER_inst', 'KO2_inst')),  F6_Metab)
F9inst <- metab(revise(bayes_specs, params_out=c(params_out,'GPP_inst', 'ER_inst', 'KO2_inst')),  F9_Metab)
F10inst <- metab(revise(bayes_specs, params_out=c(params_out,'GPP_inst', 'ER_inst', 'KO2_inst')),  F10_Metab)
C1inst <- metab(revise(bayes_specs, params_out=c(params_out,'GPP_inst', 'ER_inst', 'KO2_inst')),  C1_Metab)
OXinst <- metab(revise(bayes_specs2, params_out=c(params_out,'GPP_inst', 'ER_inst', 'KO2_inst')),  OX_Metab)


##write out data so that we have instantenous rates of GPP, ER, and KO2
F1_inst<- F1inst@fit$inst %>% select(date,solar.time, GPP_inst_50pct,GPP_inst_mean, ER_inst_50pct, ER_inst_mean, KO2_inst_50pct, KO2_inst_mean)
head(F1_inst)
write.csv(F1_inst, paste0(outdrive, '/', "F1_instantaneous_Metab.csv"))

F3_inst<- F3inst@fit$inst %>% select(date,solar.time, GPP_inst_50pct,GPP_inst_mean, ER_inst_50pct, ER_inst_mean, KO2_inst_50pct, KO2_inst_mean)
head(F3_inst)
write.csv(F3_inst, paste0(outdrive, '/', "F3_instantaneous_Metab.csv"))

F5_inst<- F5inst@fit$inst %>% select(date,solar.time, GPP_inst_50pct,GPP_inst_mean, ER_inst_50pct, ER_inst_mean, KO2_inst_50pct, KO2_inst_mean)
head(F5_inst)
write.csv(F5_inst, paste0(outdrive, '/', "F5_instantaneous_Metab.csv"))

F6_inst<- F6inst@fit$inst %>% select(date,solar.time, GPP_inst_50pct,GPP_inst_mean, ER_inst_50pct, ER_inst_mean, KO2_inst_50pct, KO2_inst_mean)
head(F6_inst)
write.csv(F6_inst, paste0(outdrive, '/', "F6_instantaneous_Metab.csv"))

F9_inst<- F9inst@fit$inst %>% select(date,solar.time, GPP_inst_50pct,GPP_inst_mean, ER_inst_50pct, ER_inst_mean, KO2_inst_50pct, KO2_inst_mean)
head(F9_inst)
write.csv(F9_inst, paste0(outdrive, '/', "F9_instantaneous_Metab_K600bin.csv"))

F10_inst<- F10inst@fit$inst %>% select(date,solar.time, GPP_inst_50pct,GPP_inst_mean, ER_inst_50pct, ER_inst_mean, KO2_inst_50pct, KO2_inst_mean)
head(F10_inst)
write.csv(F10_inst, paste0(outdrive, '/', "F10_instantaneous_Metab.csv"))

C1_inst<- C1inst@fit$inst %>% select(date,solar.time, GPP_inst_50pct,GPP_inst_mean, ER_inst_50pct, ER_inst_mean, KO2_inst_50pct, KO2_inst_mean)
head(C1_inst)
write.csv(C1_inst, paste0(outdrive, '/', "C1_instantaneous_Metab.csv"))

OX_inst<- OXinst@fit$inst %>% select(date,solar.time, GPP_inst_50pct,GPP_inst_mean, ER_inst_50pct, ER_inst_mean, KO2_inst_50pct, KO2_inst_mean)
head(OX_inst)
write.csv(OX_inst, paste0(outdrive, '/', "OX_instantaneous_Metab.csv"))
 ggplot(OX_inst, aes(x=solar.time, y='GPP_inst_50 pct'))+geom_line()

