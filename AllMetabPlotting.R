#### Script to plot Stream Metabolism for 2021- 2023 for all streams

# -- setup----
library (magrittr)
library (dplyr)
library (ggplot2)
library (tidyr)
library(lubridate)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##input and output location for files
drive1<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/2122 Model Outputs/'
drive2<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/2223 Model Outputs/'
drive<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/'

# ##read in 2021-2022 data, add in "season"
# C121<-read.csv(paste0(drive1, 'C1_2122_Metab_predic_K600bin.csv'), stringsAsFactors = FALSE)
# C121$season<-'2021-2022'
# C121$Stream<-'Commonwealth'
# F121<-read.csv(paste0(drive1, 'F1_2122_Metab_predic_K600bin.csv'), stringsAsFactors = FALSE)
# F121$season<-'2021-2022'
# F121$Stream<-'Canada'
# F321<-read.csv(paste0(drive1, 'F3_2122_Metab_predic_K600bin.csv'), stringsAsFactors = FALSE)
# F321$season<-'2021-2022'
# F321$Stream<-'Lost Seal'
# F521<-read.csv(paste0(drive1, 'F5_2122_Metab_predic_K600bin.csv'), stringsAsFactors = FALSE)
# F521$season<-'2021-2022'
# F521$Stream<-'Aiken'
# F621<-read.csv(paste0(drive1, 'F6_2122_Metab_predic_K600bin.csv'), stringsAsFactors = FALSE)
# F621$season<-'2021-2022'
# F621$Stream<-'Von Guerard'
# F921<-read.csv(paste0(drive1, 'F9_2122_Metab_predic_K600bin.csv'), stringsAsFactors = FALSE)
# F921$season<-'2021-2022'
# F921$Stream<-'Green'
# F1021<-read.csv(paste0(drive1, 'F10_2122_Metab_predic_K600bin.csv'), stringsAsFactors = FALSE)
# F1021$season<-'2021-2022'
# F1021$Stream<-'Delta'
# 
# ##read in 2022-2023 data, add in "season"
# C122<-read.csv(paste0(drive2, 'C1_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
# C122$season<-'2022-2023'
# C122$Stream<-'Commonwealth'
# F122<-read.csv(paste0(drive2, 'F1_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
# F122$season<-'2022-2023'
# F122$Stream<-'Canada'
# F222<-read.csv(paste0(drive2, 'F2_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
# F222$season<-'2022-2023'
# F222$Stream<-'Huey'
# F322<-read.csv(paste0(drive2, 'F3_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
# F322$season<-'2022-2023'
# F322$Stream<-'Lost Seal'
# F522<-read.csv(paste0(drive2, 'F5_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
# F522$season<-'2022-2023'
# F522$Stream<-'Aiken'
# F622<-read.csv(paste0(drive2, 'F6_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
# F622$season<-'2022-2023'
# F622$Stream<-'Von Guerard'
# F722<-read.csv(paste0(drive2, 'F7_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
# F722$season<-'2022-2023'
# F722$Stream<-'Harnish'
# F822<-read.csv(paste0(drive2, 'F8_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
# F822$season<-'2022-2023'
# F822$Stream<-'Crescent'
# F922<-read.csv(paste0(drive2, 'F9_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
# F922$season<-'2022-2023'
# F922$Stream<-'Green'
# F1022<-read.csv(paste0(drive2, 'F10_2223_Metab_predic_K600bin_FullSeason.csv'), stringsAsFactors = FALSE)
# F1022$season<-'2022-2023'
# F1022$Stream<-'Delta'

# ##Combine them ALL together
# Alldata21<-full_join(C121, F121)
# Alldata21<-full_join(Alldata21, F321)
# Alldata21<-full_join(Alldata21, F521)
# Alldata21<-full_join(Alldata21, F621)
# Alldata21<-full_join(Alldata21, F921)
# Alldata21<-full_join(Alldata21, F1021)
# # create a second date for plotting purposes, keep the same year 
# Alldata21$date2=as.Date(Alldata21$date)
# 
# Alldata22<-full_join(C122, F122)
# Alldata22<-full_join(Alldata22, F222)
# Alldata22<-full_join(Alldata22, F322)
# Alldata22<-full_join(Alldata22, F522)
# Alldata22<-full_join(Alldata22, F622)
# Alldata22<-full_join(Alldata22, F722)
# Alldata22<-full_join(Alldata22, F822)
# Alldata22<-full_join(Alldata22, F922)
# Alldata22<-full_join(Alldata22, F1022)

data21<-read.csv(paste0(drive, 'All_2122Metab_Data.csv'))
data21$Season<-'2021-2022'
data22<-read.csv(paste0(drive, 'All_2223Metab_Data.csv'))
data22$Season<-'2022-2023'

data21$date2=as.Date(data21$date)

# create a second date with fake year for plotting purposes
data22$date2=as.Date(data22$date)
data22$date2<-gsub("2022","2021", data22$date2)
data22$date2<-gsub("2023","2022", data22$date2)
data22$date2=as.Date(data22$date2)

##now combine all together
Alldata<-full_join(data21, data22)
Alldata$date2<-as.Date(Alldata$date2)

###plot them all together, color by year and then facet-grid by stream
AllPlot<-ggplot(Alldata)+geom_line(aes(x=date2, y=GPP_daily_mean, color=Season))+
  geom_line(aes(x=date2, y=ER_daily_mean, color=Season))+
    facet_grid(`Stream`~ ., scales = "free_y", labeller = label_wrap_gen(10))+
    ggtitle('Fryxell Metabolism over two years')+
  theme(legend.position="bottom")+
  theme(text = element_text(size = 35))
ggsave(AllPlot, filename="FryxAllMetab.jpeg", device="jpeg", path=paste0(drive), width = 20, height = 20)

##calculate NEP over the season and plot for each year and stream
Alldata$NEP_daily_mean<-(Alldata$GPP_daily_mean + Alldata$ER_daily_mean)

##plot NEP vs. time for each stream
NEPplot<-ggplot(Alldata, aes(x=date2, y=NEP_daily_mean, color=Season))+geom_line(linewidth=2.5)+
  facet_grid(`Stream`~ ., scales = "free_y", labeller = label_wrap_gen(10))+
  geom_hline(yintercept=0, linewidth=2)+
  theme(text = element_text(size=35))+
  theme(legend.position="bottom")+
  # scale_x_break(c(as.Date('2022-03-01'), as.Date('2022-11-01')))+
  # scale_x_date(limits = as.Date(c('2021-12-01','2023-02-15')))+
  ylab(expression(daily~NEP~(g~O[2]/m^2/day)))+
  xlab('Date')
ggsave(NEPplot, filename="AllNEP.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 18, height = 20)

###Plot just NEP for 22-23 season, since that's the one that we have the entire season of data for
data22$NEP_daily_mean<-(data22$GPP_daily_mean + data22$ER_daily_mean)
data22tot<-data22%>%
  group_by(Stream)%>%
  summarise(NEPtot=sum(NEP_daily_mean, na.rm=TRUE), GPPtot=sum(GPP_daily_mean, na.rm=TRUE), ERtot=sum(ER_daily_mean, na.rm=TRUE))
# data22tot$NEPtot2=(data22tot$GPPtot + data22tot$ERtot)

# Tot22<-gather(data22tot, NEP, sum, 'NEPtot', 'NEPtot2')

NEP22<-ggplot(data22tot, aes(x=Stream, y=NEPtot, color=Stream, fill=Stream))+geom_bar(stat="identity", position="dodge")+
  # facet_grid(`Stream`~ ., scales = "free_y", labeller = label_wrap_gen(10))+
  geom_hline(yintercept=0, linewidth=2)+
  theme(text = element_text(size=35))+
  theme(legend.position="none")+
  # scale_x_break(c(as.Date('2022-03-01'), as.Date('2022-11-01')))+
  # scale_x_date(limits = as.Date(c('2021-12-01','2023-02-15')))+
  theme(axis.text.x = element_text(angle = 90))+
  ylab(expression(Total~NEP~(g~O[2]/m^2/year)))+
  xlab('Stream')
ggsave(NEP22, filename="2223TotalNEP.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 18, height = 20)


###calculate avg P:R
# Alldata$PR<-(Alldata$GPP_daily_mean/abs(Alldata$ER_daily_mean))

# ##plot daily P/R for each stream
# PRplot1<-ggplot(Alldata, aes(x=date2, y=PR_daily, color=Season))+geom_line(linewidth=2)+
#   facet_grid(`Stream`~ ., scales = "free_y", labeller = label_wrap_gen(10))+
#   geom_hline(yintercept=1)+
#   theme(text = element_text(size=35))+
#   theme(legend.position="none")+
#   # scale_x_break(c(as.Date('2022-03-01'), as.Date('2022-11-01')))+
#   # scale_x_date(limits = as.Date(c('2021-12-01','2023-02-15')))+
#   ylab(expression(avg~P/R))+
#   xlab('Date')
# ggsave(PRplot, filename="AllPR.jpeg", device="jpeg", path=paste0(drive), width = 20, height = 20)
# 

## plot the monthly averages for each stream and each season
# Alldata$month<-lubridate::month(Alldata$date)
# Mondata<-Alldata%>%
#   group_by(Stream, month, season)%>%
#   summarise(., GPP_avg=mean(GPP_daily_mean, na.rm=TRUE), ER_avg=mean(ER_daily_mean, na.rm=TRUE), NEP_avg=mean(NEP_daily_mean, na.rm=TRUE),
#             PR_avg=mean(PR, na.rm=TRUE))
# Mondata$mon[Mondata$month==12]<-1
# Mondata$mon[Mondata$month==1]<-2
# Mondata$mon[Mondata$month==2]<-3
# 
# NEPmon<-ggplot(Mondata, aes(x=mon, y=NEP_avg, color=season, fill=season))+
#   geom_bar(stat="identity", position="dodge")+
#   facet_grid(`Stream`~ ., scales = "free_y", labeller = label_wrap_gen(10))+
#   geom_hline(yintercept=0)+
#   theme(text = element_text(size=35))+
#   theme(legend.position="none")+
#   # scale_x_break(c(as.Date('2022-03-01'), as.Date('2022-11-01')))+
#   # scale_x_date(limits = as.Date(c('2021-12-01','2023-02-15')))+
#   ylab(expression(avg~NEP~(g~O[2]/m^2/day)))+
#   xlab('Month')+
#   scale_x_continuous(breaks=c(1,2,3),
#                      labels=c("Dec", "Jan", "Feb"))
# ggsave(NEPmon, filename="MonthlyNEP.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 12, height = 20)
# 

##Take the seasonal averages for each stream 
Avgdata<-Alldata%>%
  group_by(Stream, Season)%>%
  summarise(GPP_avg=mean(GPP_daily_mean, na.rm=TRUE), ER_avg=mean(ER_daily_mean, na.rm=TRUE), PR_avg=mean(PR_daily, na.rm=TRUE), PR_med=median(PR_daily, na.rm=TRUE))
Avgdata2<-gather(Avgdata, Variable, Avg, 'GPP_avg', 'ER_avg')

AvgPlot<-ggplot(Avgdata2, aes(x=Variable, y=Avg, color=Season, fill=Season))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(`Stream`~ ., labeller = label_wrap_gen(10), ncol=5)+
  theme(legend.position="bottom")+
  theme(text = element_text(size = 35))+
  ggtitle('Fryxell Metabolism avg daily metabolism')
ggsave(AvgPlot, filename="FryxAllMetabAvg.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 20, height = 20)

# Avgdata$PR_avg<-Avgdata$GPP_avg/abs(Avgdata$ER_avg)

# PRPlot1<-ggplot(Avgdata, aes(x=Stream, y=PR_avg, color="yellow", fill="yellow"))+
#   geom_bar(stat="identity", position="dodge")+
#   geom_hline(yintercept=1)+
#   # facet_wrap(`Stream`~ ., labeller = label_wrap_gen(10), ncol=5)+
#   theme(legend.position="none")+
#   theme(text = element_text(size = 30))+
#   theme(axis.text.x = element_text(angle = 90))+
#   ggtitle('Fryxell avg P:R')
# ggsave(PRPlot1, filename="FryxAllPRavg.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 12, height = 12)

# ##write out avg data
write.csv(Avgdata, paste0(drive, "AllMetab_Avgdata.csv"))

## Plot the P:R Ratio
PRdata<-Alldata%>%
  group_by(Stream)%>%
  dplyr::summarise(., PR_avg=mean(PR_daily, na.rm=TRUE), PR_med=median(PR_daily, na.rm=TRUE))

##number the streams so we can order how they are presented from inland to the coast
PRdata$str[PRdata$Stream=='Green']<-1
PRdata$str[PRdata$Stream=='Canada']<-2
PRdata$str[PRdata$Stream=='Delta']<-3
PRdata$str[PRdata$Stream=='Huey']<-4
PRdata$str[PRdata$Stream=='Crescent']<-5
PRdata$str[PRdata$Stream=='Harnish']<-6
PRdata$str[PRdata$Stream=='Von Guerard']<-7
PRdata$str[PRdata$Stream=='Lost Seal']<-8
PRdata$str[PRdata$Stream=='Aiken']<-9
PRdata$str[PRdata$Stream=='Commonwealth']<-10

PRPlot<-ggplot(PRdata, aes(x=str, y=PR_avg, color="yellow", fill="yellow"))+
  geom_bar(stat="identity", position="dodge")+
  geom_hline(yintercept=1)+
  # facet_wrap(`Stream`~ ., labeller = label_wrap_gen(10), ncol=5)+
  theme(legend.position="none")+
  theme(text = element_text(size = 30))+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle('Fryxell avg P:R')+
  scale_x_continuous(breaks=c(1,2,3, 4, 5, 6, 7, 8, 9, 10),
                     labels=c("Green", "Canada", "Delta", "Huey", "Crescent", "Harnish", "Von Guerard", "Lost Seal", "Aiken", "Commonwealth"))
ggsave(PRPlot, filename="FryxAllPRavg.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 12, height = 12)

PRmedPlot<-ggplot(PRdata, aes(x=str, y=PR_med, color="season", fill="season"))+
  geom_bar(stat="identity", position="dodge")+
  geom_hline(yintercept=1)+
  # facet_wrap(`Stream`~ ., labeller = label_wrap_gen(10), ncol=5)+
  theme(legend.position="bottom")+
  theme(text = element_text(size = 30))+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle('Fryxell median P:R')+
  scale_x_continuous(breaks=c(1,2,3, 4, 5, 6, 7, 8, 9, 10),
                     labels=c("Green", "Canada", "Delta", "Huey", "Crescent", "Harnish", "Von Guerard", "Lost Seal", "Aiken", "Commonwealth"))
ggsave(PRmedPlot, filename="FryxAllPRmed.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 12, height = 12)

### PR but include both seasons
## Plot the P:R Ratio
# PRdata2<-PRdata2%>%
#   add_row(Stream="Crescent", Season="2021-2022", PR_avg=NA, PR_med=NA, str=5)
PRdata2<-Alldata%>%
  group_by(Stream, Season)%>%
  dplyr::summarise(., PR_avg=mean(PR_daily, na.rm=TRUE), PR_med=median(PR_daily, na.rm=TRUE))


##number the streams so we can order how they are presented from inland to the coast
# PRdata2$str[PRdata2$Stream=='Green']<-1
# PRdata2$str[PRdata2$Stream=='Canada']<-2
# PRdata2$str[PRdata2$Stream=='Delta']<-3
# PRdata2$str[PRdata2$Stream=='Huey']<-4
# PRdata2$str[PRdata2$Stream=='Crescent']<-5
# PRdata2$str[PRdata2$Stream=='Harnish']<-6
# PRdata2$str[PRdata2$Stream=='Von Guerard']<-7
# PRdata2$str[PRdata2$Stream=='Lost Seal']<-8
# PRdata2$str[PRdata2$Stream=='Aiken']<-9
# PRdata2$str[PRdata2$Stream=='Commonwealth']<-10

##OR by stream length -- short to long
# PRdata2$str[PRdata2$Stream=='Green']<-1
# PRdata2$str[PRdata2$Stream=='Canada']<-2
# PRdata2$str[PRdata2$Stream=='Delta']<-9
# PRdata2$str[PRdata2$Stream=='Huey']<-4
# PRdata2$str[PRdata2$Stream=='Crescent']<-10
# PRdata2$str[PRdata2$Stream=='Harnish']<-7
# PRdata2$str[PRdata2$Stream=='Von Guerard']<-5
# PRdata2$str[PRdata2$Stream=='Lost Seal']<-3
# PRdata2$str[PRdata2$Stream=='Aiken']<-8
# PRdata2$str[PRdata2$Stream=='Commonwealth']<-6

##OR by average flow -- low to high 
PRdata2$str[PRdata2$Stream=='Green']<-7
PRdata2$str[PRdata2$Stream=='Canada']<-6
PRdata2$str[PRdata2$Stream=='Delta']<-3
PRdata2$str[PRdata2$Stream=='Huey']<-1
PRdata2$str[PRdata2$Stream=='Crescent']<-5
PRdata2$str[PRdata2$Stream=='Harnish']<-4
PRdata2$str[PRdata2$Stream=='Von Guerard']<-2
PRdata2$str[PRdata2$Stream=='Lost Seal']<-10
PRdata2$str[PRdata2$Stream=='Aiken']<-8
PRdata2$str[PRdata2$Stream=='Commonwealth']<-9

# ## INclude NAs for the ones we didn't measure in 22-23
# PRdata2$PR_med[PRdata2$Stream=='Crescent'&PRdata2$Season=='2021-2022']<-0
# PRdata2$PR_med[PRdata2$Stream=='Harnish'&PRdata2$Season=='2021-2022']<-0
# PRdata2$PR_med[PRdata2$Stream=='Huey'&PRdata2$Season=='2021-2022']<-0

PRPlot2<-ggplot(PRdata2, aes(x=str, y=PR_avg, color=Season, fill=Season))+
  geom_bar(stat="identity", position="dodge")+
  geom_hline(yintercept=1)+
  # facet_wrap(`Stream`~ ., labeller = label_wrap_gen(10), ncol=5)+
  theme(legend.position="bottom")+
  theme(text = element_text(size = 30))+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle('')+
  ylab('Avg P/R ratio')+
  xlab('Stream')+
  scale_x_continuous(breaks=c(1,2,3, 4, 5, 6, 7, 8, 9, 10),
                     labels=c("Huey", "Von Guerard", "Delta", "Harnish", "Crescent", "Canada", "Green", "Aiken", "Commonwealth", "Lost Seal"))
ggsave(PRPlot2, filename="FryxAllPRavg_Season.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 12, height = 12)

PRmedPlot2<-ggplot(PRdata2, aes(x=str, y=PR_med, color=Season, fill=Season))+
  geom_bar(stat="identity", position="dodge")+
  geom_hline(yintercept=1)+
  # facet_wrap(`Stream`~ ., labeller = label_wrap_gen(10), ncol=5)+
  theme(legend.position="right")+
  theme(text = element_text(size = 30))+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle('')+
  ylab('Median P/R ratio')+
  xlab('Stream')+
  ylim(0, 4)+
  scale_x_continuous(breaks=c(1,2,3, 4, 5, 6, 7, 8, 9, 10),
                     labels=c("Huey", "Von Guerard", "Delta", "Harnish", "Crescent", "Canada", "Green", "Aiken", "Commonwealth", "Lost Seal"))
ggsave(PRmedPlot2, filename="FryxAllPRmed_Season.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 18, height = 12)

# Alldata$str[Alldata$Stream=='Green']<-1
# Alldata$str[Alldata$Stream=='Canada']<-2
# Alldata$str[Alldata$Stream=='Delta']<-3
# Alldata$str[Alldata$Stream=='Huey']<-4
# Alldata$str[Alldata$Stream=='Crescent']<-5
# Alldata$str[Alldata$Stream=='Harnish']<-6
# Alldata$str[Alldata$Stream=='Von Guerard']<-7
# Alldata$str[Alldata$Stream=='Lost Seal']<-8
# Alldata$str[Alldata$Stream=='Aiken']<-9
# Alldata$str[Alldata$Stream=='Commonwealth']<-10
##OR by stream length -- short to long
##OR by average flow -- low to high 
Alldata$str[Alldata$Stream=='Green']<-7
Alldata$str[Alldata$Stream=='Canada']<-6
Alldata$str[Alldata$Stream=='Delta']<-3
Alldata$str[Alldata$Stream=='Huey']<-1
Alldata$str[Alldata$Stream=='Crescent']<-5
Alldata$str[Alldata$Stream=='Harnish']<-4
Alldata$str[Alldata$Stream=='Von Guerard']<-2
Alldata$str[Alldata$Stream=='Lost Seal']<-10
Alldata$str[Alldata$Stream=='Aiken']<-8
Alldata$str[Alldata$Stream=='Commonwealth']<-9

##PR boxplot across all data
PRbox<-ggplot(Alldata, aes(x=str, y=PR_daily, color=Stream))+
  geom_boxplot()+
  geom_hline(yintercept=1)+
  # facet_wrap(`Stream`~ ., labeller = label_wrap_gen(10), ncol=5)+
  theme(legend.position="none")+
  theme(text = element_text(size = 30))+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle('')+
  ylab('Daily P/R ratio')+
  xlab('Stream')+
  ylim(0, 10)+
  scale_x_continuous(breaks=c(1,2,3, 4, 5, 6, 7, 8, 9, 10),
                     labels=c("Huey", "Von Guerard", "Delta", "Harnish", "Crescent", "Canada", "Green", "Aiken", "Commonwealth", "Lost Seal"))
ggsave(PRbox, filename="FryxAllPR_boxplot.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 16, height = 12)


# AllAvg2<-Avgdata%>%
#   dplyr::summarise(GPP=mean(GPP_avg, na.rm=TRUE), ER=mean(ER_avg, na.rm=TRUE), PR=mean(PR, na.rm=TRUE))
# 
##number the streams so we can order how they are presented from inland to the coast
# Alldata$str[Alldata$Stream=='Green']<-'A'
# Alldata$str[Alldata$Stream=='Canada']<-'B'
# Alldata$str[Alldata$Stream=='Delta']<-'C'
# Alldata$str[Alldata$Stream=='Huey']<-'D'
# Alldata$str[Alldata$Stream=='Crescent']<-'E'
# Alldata$str[Alldata$Stream=='Harnish']<-'F'
# Alldata$str[Alldata$Stream=='Von Guerard']<-'G'
# Alldata$str[Alldata$Stream=='Lost Seal']<-'H'
# Alldata$str[Alldata$Stream=='Aiken']<-'I'
# Alldata$str[Alldata$Stream=='Commonwealth']<-'J'

##Actually organize it by stream length -- short to long
Alldata$str[Alldata$Stream=='Green']<-'G'
Alldata$str[Alldata$Stream=='Canada']<-'F'
Alldata$str[Alldata$Stream=='Delta']<-'C'
Alldata$str[Alldata$Stream=='Huey']<-'A'
Alldata$str[Alldata$Stream=='Crescent']<-'E'
Alldata$str[Alldata$Stream=='Harnish']<-'D'
Alldata$str[Alldata$Stream=='Von Guerard']<-'B'
Alldata$str[Alldata$Stream=='Lost Seal']<-'J'
Alldata$str[Alldata$Stream=='Aiken']<-'H'
Alldata$str[Alldata$Stream=='Commonwealth']<-'I'


## make box plot of avg daily values across streams
AllPlot<-ggplot(Alldata)+geom_boxplot(aes(x=str, y=GPP_daily_mean), color='dark green')+
  geom_boxplot(aes(x=str, y=ER_daily_mean), color='dark red')+
  facet_grid(`Season`~ .,)+
  ggtitle('')+
  ylab('daily rate (g O2/day)')+ xlab('Stream')+
  theme(text = element_text(size=30))+
  ylim(-2, 2)+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(breaks=c('A','B','C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'),
                   labels=c("Huey", "Von Guerard", "Delta", "Harnish", "Crescent", "Canada", "Green", "Aiken", "Commonwealth", "Lost Seal"))
ggsave(AllPlot, filename="All_MDV_boxplot.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 12, height = 14)

###Plot the seasonality of GPP and ER for each stream
Alldata$date<-as.Date(Alldata$date)
AllPlot2<-ggplot(Alldata)+geom_line(aes(x=date, y=GPP_daily_mean), color='dark green')+geom_point(aes(x=date, y=GPP_daily_mean), color='dark green')+
  geom_line(aes(x=date, y=ER_daily_mean), color='dark red')+geom_point(aes(x=date, y=ER_daily_mean), color='dark red')+
  facet_grid(`str`~ ., scales = "free_y")+ggtitle('')+
  ylab('daily rate (g O2/day)')+ xlab('Date')+
  theme(text = element_text(size=30))+
  # ylim(-2, 2)+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_date(date_labels = "%b")+
  scale_x_break(c(as.Date('2022-03-01'), as.Date('2022-11-01')))
  scale_x_discrete(breaks=c('A','B','C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'),
                   labels=c("Huey", "Von Guerard", "Delta", "Harnish", "Crescent", "Canada", "Green", "Aiken", "Commonwealth", "Lost Seal"))
  ggsave(AllPlot2, filename="All_MDV_timeseries.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 12, height = 16)

##Find the date when max Q and max GPP occur -- look at the lag between them
QMax<-Alldata%>%
  group_by(Stream, Season)%>%
  slice(which.max(Q_avg))
QStart<-Alldata%>%
  group_by(Stream, Season)%>%
  slice(which.min(date))
GPPMax<-Alldata%>%
  group_by(Stream, Season)%>%
  slice(which.max(GPP_daily_mean))

## convert daily avgs to g C from g O2
##this is grams of O2, need to convert to grams of C by converting to molar
# Alldata$GPPavg_gC<-((Alldata$GPP_daily_mean/31.99)*12.011)
# Alldata$ERavg_gC<-((Alldata$ER_daily_mean/31.99)*12.011)

###Plot NEP over the season (for each season)
Anndata<-Alldata%>%
  group_by(Stream, season)%>%
  dplyr::summarise(., GPPtotO=sum(GPP_daily_mean, na.rm=TRUE), ERtotO=sum(ER_daily_mean, na.rm=TRUE), str=str)
Anndata$NEP=(Anndata$GPPtotO - abs(Anndata$ERtotO))

NEPplot<-ggplot(Anndata, aes(x=str, y=NEP, color=season, fill=season))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle('Stream NEP')+
  ylab('NEP (g C)')+ xlab('Stream')+
  theme(legend.position="bottom")+
  theme(text = element_text(size=30))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_continuous(breaks=c(1, 2, 3,4, 5, 6, 7, 8,9, 10),
                   labels=c("Green", "Canada", "Delta","Huey", "Crescent", "Harnish", "Von Guerard", "Lost Seal", "Aiken", "Commonwealth"))

ggsave(NEPplot, filename="All_MDV_NEP.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 12, height = 12)


################################### TO TAKE OVERALL AVGS ##################
## we want to use the cropped 22-23 season to the same days as the 21-22 season, then take the average for the flow season 
## metab avgs from the "heart of the flow season"
##max(Alldata$date[Alldata$season=='2021-2022'])
##"2022-01-27"
Avgdata21<-read.csv(paste0(drive, 'Alldata2122_MainFlowSeason.csv'), stringsAsFactors = FALSE)
Avgdata21$season<-'2021-2022'
Avgdata22<-read.csv(paste0(drive, 'Alldata2223_MainFlowSeason.csv'), stringsAsFactors = FALSE)
Avgdata22$season<-'2022-2023'

Avgdata2<-full_join(Avgdata21, Avgdata22)

###calculate avg P:R
# Avgdata2$PR_avg<-(Avgdata2$GPP_avg/abs(Avgdata2$ER_avg))

##number the streams so we can order how they are presented from inland to the coast
# Avgdata2$str[Avgdata2$Stream=='Green']<-1
# Avgdata2$str[Avgdata2$Stream=='Canada']<-2
# Avgdata2$str[Avgdata2$Stream=='Delta']<-3
# Avgdata2$str[Avgdata2$Stream=='Huey']<-4
# Avgdata2$str[Avgdata2$Stream=='Crescent']<-5
# Avgdata2$str[Avgdata2$Stream=='Harnish']<-6
# Avgdata2$str[Avgdata2$Stream=='Von Guerard']<-7
# Avgdata2$str[Avgdata2$Stream=='Lost Seal']<-8
# Avgdata2$str[Avgdata2$Stream=='Aiken']<-9
# Avgdata2$str[Avgdata2$Stream=='Commonwealth']<-10

##OR by stream length -- short to long
Avgdata$str[Avgdata$Stream=='Green']<-1
Avgdata$str[Avgdata$Stream=='Canada']<-2
Avgdata$str[Avgdata$Stream=='Delta']<-9
Avgdata$str[Avgdata$Stream=='Huey']<-4
Avgdata$str[Avgdata$Stream=='Crescent']<-10
Avgdata$str[Avgdata$Stream=='Harnish']<-7
Avgdata$str[Avgdata$Stream=='Von Guerard']<-5
Avgdata$str[Avgdata$Stream=='Lost Seal']<-3
Avgdata$str[Avgdata$Stream=='Aiken']<-8
Avgdata$str[Avgdata$Stream=='Commonwealth']<-6

Avgdata3<-gather(Avgdata2, Variable, Avg, 'GPP_avg', 'ER_avg')

AvgPlot2<-ggplot(Avgdata3, aes(x=Variable, y=Avg, color=season, fill=season))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(`Stream`~ ., labeller = label_wrap_gen(10), ncol=5)+
  theme(legend.position="bottom")+
  theme(text = element_text(size = 35))+
  ggtitle('Fryxell Metabolism avg daily metabolism')
ggsave(AvgPlot2, filename="FryxAllMetabAvg_MainFlowSeason.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 20, height = 15)

PRPlot2<-ggplot(Avgdata2, aes(x=str, y=PR_daily_avg, color=season, fill=season))+
  geom_bar(stat="identity", position="dodge")+
  geom_hline(yintercept=1)+
  # facet_wrap(`Stream`~ ., labeller = label_wrap_gen(10), ncol=5)+
  theme(legend.position="none")+
  theme(text = element_text(size = 30))+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle('Fryxell avg daily P:R')+
  scale_x_continuous(breaks=c(1, 2, 3,4, 5, 6, 7, 8,9, 10),
                     labels=c("Green", "Canada", "Delta", "Huey", "Crescent", "Harnish", "Von Guerard", "Lost Seal", "Aiken", "Commonwealth"))
ggsave(PRPlot2, filename="FryxPRavg_MainFlowSeason.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 12, height = 12)

Avgdata4<-Avgdata2%>%
  group_by(Stream)%>%
  summarise(GPP_avg=mean(GPP_avg, na.rm=TRUE), ER_avg=mean(ER_avg, na.rm=TRUE), PR_avg=mean(PR_daily_avg, na.rm=TRUE), PR_med=median(PR_daily_avg, na.rm=TRUE))
##write out avg data -- across all streams! Main flow season
# write.csv(Avgdata4, paste0(drive, "AllMetab_Avgdata.csv"))

Avgdata4$PR_log<-log10(Avgdata4$PR_avg)
Avgdata4$PR_med_log<-log10(Avgdata4$PR_med)
Avgdata5<-gather(Avgdata4, Variable, Avg, 'GPP_avg', 'ER_avg', 'PR_log')

AvgPlot3<-ggplot(Avgdata5, aes(x=Variable, y=Avg, color=Variable, fill=Variable))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(`Stream`~ ., labeller = label_wrap_gen(10), ncol=5)+
  geom_hline(yintercept=0)+
  theme(legend.position="bottom")+
  theme(text = element_text(size = 35))+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle('Fryxell Metabolism avg daily metabolism')
ggsave(AvgPlot3, filename="FryxAllAvg_MainFlowSeason.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 20, height = 15)


PRPlot3<-ggplot(Avgdata4, aes(x=Stream, y=PR_log))+
  geom_bar(stat="identity", position="dodge")+
  geom_hline(yintercept=0)+
  theme(legend.position="bottom")+
  theme(text = element_text(size = 35))+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle('Fryxell Metabolism avg daily metabolism')
ggsave(PRPlot3, filename="FryxAllAvgPR_MainFlowSeason.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 20, height = 15)

PRmedPlot3<-ggplot(Avgdata4, aes(x=Stream, y=PR_med_log))+
  geom_bar(stat="identity", position="dodge")+
  geom_hline(yintercept=0)+
  theme(legend.position="bottom")+
  theme(text = element_text(size = 35))+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle('Fryxell Metabolism median daily metabolism')
ggsave(PRmedPlot3, filename="FryxAllMedPR_MainFlowSeason.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 20, height = 15)

### Plot the average Q and avg Temp of each season so that we can see why the averages and P/R might be different
Avgdata6<-gather(Avgdata2, Variable, Avg, 'Q_avg', 'Temp_avg')

AvgPlot4<-ggplot(Avgdata6, aes(x=str, y=Avg, color=season, fill=season))+
  geom_bar(stat="identity", position="dodge")+
  facet_grid(`Variable`~ ., labeller = label_wrap_gen(10), scales = "free_y")+
  geom_hline(yintercept=0)+
  theme(legend.position="bottom")+
  theme(text = element_text(size = 35))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_continuous(breaks=c(1, 2, 3,4, 5, 6, 7, 8,9, 10),
                     labels=c("Green", "Canada", "Delta", "Huey", "Crescent", "Harnish", "Von Guerard", "Lost Seal", "Aiken", "Commonwealth"))+
  ggtitle('Avg Q and Avg water temp')
ggsave(AvgPlot4, filename="FryxAvg_QandTemp.jpeg", device="jpeg", path=paste0(drive, 'Plots/'), width = 20, height = 15)



