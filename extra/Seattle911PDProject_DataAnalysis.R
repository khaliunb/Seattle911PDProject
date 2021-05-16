source("Seattle911PDProject_Script.R", local = knitr::knit_global())
library(lubridate)
#############################################################################
#### BEGIN: DATA SET FIELD NAME REFERENCES
#############EVENT CLEARANCE AND INCIDENT TYPE RELATED FIELDS################
#"Event Clearance Description","ECD"  | #"Initial Type Description","ITDesc"
#"Event Clearance SubGroup","ECSG"    | #"Initial Type Subgroup","ITSG"
#"Event Clearance Group","ECG"        | #"Initial Type Group","ITG"
#############################################################################

#############LOCATION RELATED FIELDS#########################################
#"Hundred Block Location","HBL"       | #"Incident Location","ILoc"
#"District/Sector","Dist_Sec"
#"Zone/Beat","Zone_Beat"
#"Census Tract","Census_Tract"
#"Longitude","Longitude"
#"Latitude","Latitude"
#############################################################################

#############TIMING RELATED FIELDS###########################################
#"Event Clearance Date","ECDt"        | #"At Scene Time","ASTm"

#Also we are adding some date and time fields for the analysis of data
#"Event Clearance Date Converted to Full data time format","EC_DateTime"
#"At Scene Time Converted to Full data time format","AS_DateTime"
S911IR<-S911IR%>%mutate(EC_DateTime=parse_date_time(ECDt,'%m/%d/%Y %I:%M:%S %p'),na.rm=TRUE)
S911IR<-S911IR%>%mutate(AS_DateTime=parse_date_time(ASTm,'%m/%d/%Y %I:%M:%S %p'),na.rm=FALSE)
#"Timespan in minutes between At Scene Time and Event Clearance Date Time","AS_TimeSpan"
S911IR<-S911IR%>%mutate(AS_TimeSpan=round(time_length(AS_DateTime %--% EC_DateTime,"minute")),na.rm=FALSE)

#Adding new field names and descriptions to S911IR_Head data frame for reference
S911IR_Head<-bind_rows(S911IR_Head,
                       data.frame(colDesc="Event Clearance Date Converted to Full data time format",colName="EC_DateTime"),
                       data.frame(colDesc="At Scene Time Converted to Full data time format",colName="AS_DateTime"),
                       data.frame(colDesc="Timespan in minutes between At Scene Time and Event Clearance Date Time",colName="AS_TimeSpan"))
#############################################################################
#### END: DATA SET FIELD NAME REFERENCES
#############################################################################

#  - What is the overall picture of overall occurence of event clearance description? Also, do some ECDs prevail over others on daily basis? We will view the plots side by side to get a clear idea.
S911IR%>%group_by(ECD)%>%summarise(overall = n()) %>%
  mutate(ECD = fct_reorder(ECD, overall)) %>% 
  ggplot(aes(ECD, overall)) + geom_col() + coord_flip() + theme(axis.text.y = element_blank())+
  labs(y="ECD Overall occurence", x="", subtitle="ECDs' Overall Occurence in Descending Order")
# We can also see that the picture changes when we consider daily averages.
S911IR%>%group_by(date(EC_DateTime),ECD)%>%summarise(daily = n()) %>% ungroup()%>%group_by(ECD)%>%summarise(daily_avg=mean(daily))%>%
  mutate(ECD = fct_reorder(ECD, daily_avg)) %>% 
  ggplot(aes(ECD, daily_avg)) + geom_col() + coord_flip() + theme(axis.text.y = element_blank())+
  labs(y="ECD Daily Average Occurence", x="", subtitle="ECDs' Daily Average Occurence in Descending Order")
# From the plot we see four bars that significantly exceed 20'000 in total occurence. 
#  - So, which four are the event clearance description that occur the most? And do they also prevail on daily basis
S911IR%>%group_by(date(EC_DateTime),ECD)%>%summarise(daily = n()) %>% ungroup()%>%group_by(ECD)%>%summarise(overall=sum(daily),daily_avg=mean(daily),daily_median=median(daily))%>%filter(overall>20000)%>%arrange(desc(overall))%>%select(ECD,overall,daily_avg,daily_median)
# We can also see group of bars that have occurences between 8'000 and 20'000. And do they what is their average occurence on daily basis 
#  - Which are the event clearance description that form the group?
S911IR%>%group_by(date(EC_DateTime),ECD)%>%summarise(daily = n()) %>% ungroup()%>%group_by(ECD)%>%summarise(overall=sum(daily),daily_avg=mean(daily),daily_median=median(daily))%>%filter(overall>=8000 & overall<=20000)%>%arrange(desc(overall))%>%select(ECD,overall,daily_avg,daily_median)
# Those ECDs that are present, but can be viewed as separate occurences should also be noted.
#  - So, what are the ECDs that have barely existent occurences?
S911IR%>%group_by(date(EC_DateTime),ECD)%>%summarise(daily = n()) %>% ungroup()%>%group_by(ECD)%>%summarise(overall=sum(daily),daily_avg=mean(daily),daily_median=median(daily))%>%filter(overall<50)%>%arrange(desc(overall))%>%select(ECD,overall,daily_avg,daily_median)
# We are using boxplot to see more clear picture. And here we are considering group of ECDs that have overall occurence of more than 8'000
S911IR%>%group_by(date(EC_DateTime),ECD)%>%summarise(daily = n()) %>% ungroup()%>%group_by(ECD)%>%
          summarise(overall=sum(daily),daily_avg=mean(daily),daily_median=median(daily))%>%
          filter(overall>=8000)%>%
          left_join(S911IR,by="ECD")%>%group_by(date(EC_DateTime),ECD)%>%summarise(daily = n())%>%
          ggplot(aes(x=reorder(ECD,daily,na.rm=TRUE),y=daily))+geom_boxplot()+coord_flip()+
          labs(y="Daily occurence", x="Event Clearance Description", subtitle="ECDs with large overall occurences:Daily occurence In Descending Order")
# Here, we are bringing in the Initial Type Description to see whether they match the resulting Event Clearance Description, or do people commonly mistake the symptoms of the situation very often.
S911IR%>%group_by(date(EC_DateTime),ECD)%>%summarise(daily = n()) %>% ungroup()%>%group_by(ECD)%>%
    summarise(overallECD=sum(daily),daily_avgECD=mean(daily))%>%
    filter(overallECD>20000)%>%select(ECD,overallECD,daily_avgECD)%>%
    left_join(S911IR,by="ECD")%>%group_by(date(EC_DateTime),ECD,ITDesc,overallECD,daily_avgECD)%>%
    summarise(dailyITDesc=n())%>%ungroup()%>%mutate(ECD = fct_reorder(ECD, overallECD))%>%group_by(ECD,ITDesc,overallECD,daily_avgECD)%>%
    summarise(overallITDesc=sum(dailyITDesc),daily_avgITDesc=mean(dailyITDesc))%>%filter(overallITDesc>500)%>%select(ITDesc,overallITDesc,overallECD,daily_avgECD,ECD,daily_avgITDesc)%>%
    ggplot(aes(x=ITDesc,y=overallITDesc))+geom_point(aes(colour=ECD,size=daily_avgECD,alpha=daily_avgITDesc))+
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
          legend.justification = c("right", "bottom"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6))+
          guides(color = guide_legend(order=3),
                 size = guide_legend(order=2),
                 alpha = guide_legend(order=1))+
    scale_y_log10(labels=NULL)+
    labs(y="", x="Initial Type Descriptions", subtitle="")
#From this "Total occurences of Initial Type Description within most occuring Event clearance Description" plot we see clearly that:
#- "NA"s is ITDesc occur most frequently
#- ECD "Suspicious person","Disturbance, Other" is most prevalent for most reasons. Which we can attribute to the fact that this type of description is applicable to most circumstances.
#- ECD "PARKING VIOLATIONS (EXCEPT ABANDONED VEHICLES)" have the least daily average for ECD, but the most daily average for ITDesc. Which means while this type of ECD happens less, people identify the reason for this ECD most clearly.

#Here is another version of the plot to see whether there are relationship between the ECD and ITDesc.
S911IR%>%group_by(date(EC_DateTime),ECD)%>%summarise(daily = n()) %>% ungroup()%>%group_by(ECD)%>%
  summarise(overallECD=sum(daily),daily_avgECD=mean(daily))%>%
  filter(overallECD>20000)%>%select(ECD,overallECD,daily_avgECD)%>%
  left_join(S911IR,by="ECD")%>%group_by(date(EC_DateTime),ECD,ITDesc,overallECD,daily_avgECD)%>%
  summarise(dailyITDesc=n())%>%ungroup()%>%mutate(ECD = fct_reorder(ECD, overallECD))%>%group_by(ECD,ITDesc,overallECD,daily_avgECD)%>%
  summarise(overallITDesc=sum(dailyITDesc),daily_avgITDesc=mean(dailyITDesc))%>%select(ITDesc,overallITDesc,overallECD,daily_avgECD,ECD,daily_avgITDesc)%>%
  ggplot(aes(x=daily_avgECD,y=daily_avgITDesc))+geom_point(aes(colour=ITDesc,shape=ECD,size=overallITDesc))+
  theme(legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))+
  guides(color = FALSE,
         size = FALSE,
         alpha = FALSE,
         shape=guide_legend(order=1))+
  scale_y_log10(labels=NULL)+
  labs(y="Daily Average ITDesc", x="Daily Average ECD", subtitle="")
#-----------------------------------------------------------
#  - Is there relation between location and event clearance description/subgroup/description?
#     - Is there certain prevalence in overall number of event clearance description/subgroup/description in certain location?

#locMatrix<-S911IR%>%group_by(ILoc,ECD)%>%summarize(count=n())%>%filter(count>100)%>%select(ILoc,ECD,count)%>%spread(key=ECD,value=count)
#locMatrix<-as.matrix(locMatrix)
#rownames(locMatrix)<- locMatrix[,1]
#locMatrix <- locMatrix[,-1]
#locMatrix%>%knitr::kable(align="c")
#dim(locMatrix)

#  - Is there relation between timing and event clearance description/subgroup/description?
#     - Was there increase/decrease in event clearance description/subgroup/description overall occurrence over time?
S911IR%>%mutate(EC_Day=date(EC_DateTime))%>%group_by(EC_Day)%>%summarise(daily_total = n())%>%
  ggplot(aes(x=EC_Day,y=daily_total))+geom_smooth()+
  labs(y="Daily Total ECD number", x="Date", subtitle="")
# From the plot, we see sharp drop in daily total number around 2014. So what happened here? We investigate by detailing the plot by ECD and faceting daily averages by year versus month
S911IR%>%mutate(EC_Day=date(EC_DateTime))%>%group_by(EC_Day,ECD)%>%summarise(daily = n()) %>%
    ggplot(aes(ECD, mean(daily))) + geom_col() + theme(axis.text.y = element_blank(),axis.text.x = element_blank())+facet_grid(year(EC_Day)~month(EC_Day))+
    labs(y="Daily Averages Faceted by Year", x="Event Clearance Descriptions", subtitle="")
# And here we see data largely missing between March of 2013 to June of 2014. And we also see that data after 1st of September, 2017 is missing. Therefore we have trimmed the data after that point.
  
#     - During which month event clearance description/subgroup/description overall occurrence increases/decreases?
S911IR%>%mutate(EC_Year=year(EC_DateTime),EC_Month=month(EC_DateTime))%>%group_by(EC_Year,EC_Month)%>%arrange(EC_Year,EC_Month)%>%summarize(count=n())%>%group_by(EC_Month)%>%summarise(avgbyMonth=mean(count))%>%
  ggplot(aes(x=EC_Month,y=avgbyMonth))+geom_col()+geom_smooth()+
  theme(axis.text.y = element_blank(),axis.text.x = element_blank())+
  labs(y="Montly Average", x="Month", subtitle="")

S911IR%>%mutate(EC_Day=date(EC_DateTime))%>%group_by(EC_Day,ECD)%>%summarise(daily = n()) %>%
  ggplot(aes(month(EC_Day), mean(daily))) + geom_col() + theme(axis.text.y = element_blank(),axis.text.x = element_blank())+
  labs(y="Daily Averages by Month", x="Month", subtitle="")

#     - During which season event clearance description/subgroup/description overall occurrence increases/decreases?
S911IR%>%mutate(EC_Year=year(EC_DateTime),EC_Quarter=quarter(EC_DateTime))%>%group_by(EC_Year,EC_Quarter)%>%arrange(EC_Year,EC_Quarter)%>%summarize(count=n())%>%group_by(EC_Quarter)%>%summarise(avgbyMonth=mean(count))%>%
  ggplot(aes(x=EC_Quarter,y=avgbyMonth))+geom_col()+geom_smooth()+
  theme(axis.text.y = element_blank(),axis.text.x = element_blank())+
  labs(y="Quarterly Average", x="Quarter", subtitle="")

S911IR%>%mutate(EC_Day=date(EC_DateTime))%>%group_by(EC_Day,ECD)%>%summarise(daily = n()) %>%
  ggplot(aes(quarter(EC_Day), mean(daily))) + geom_col() + theme(axis.text.y = element_blank(),axis.text.x = element_blank())+
  labs(y="Daily Averages by Quarter", x="Quarter", subtitle="")

#     - On which day of the week event clearance description/subgroup/description overall occurrence increases/decreases?
S911IR%>%mutate(EC_Day=date(EC_DateTime))%>%group_by(EC_Day,ECD)%>%summarise(daily = n()) %>%
  ggplot(aes(wday(EC_Day), mean(daily))) + geom_col() + theme(axis.text.y = element_blank(),axis.text.x = element_blank())+
  labs(y="Daily Averages by Weekday", x="Weekday", subtitle="")
#     - During which hour of the day event clearance description/subgroup/description overall occurrence increases/decreases?
S911IR%>%mutate(EC_Day=date(EC_DateTime),Hour=hour(EC_DateTime))%>%group_by(EC_Day,Hour)%>%summarise(hourly = n()) %>%
  ggplot(aes(Hour, mean(hourly))) + geom_col() + theme(axis.text.y = element_blank(),axis.text.x = element_blank())+
  labs(y="Hourly Average Occurences of ECD", x="Weekday", subtitle="")
#     - On average, how many minutes does it take from At Scene Time and Event clearance Time for Event clearance description/subgroup/group? And which Event Clearance description/subgroup/group is most time consuming? Which ones are most quick to be resolved?
S911IR%>%filter(!is.na(AS_TimeSpan)&AS_TimeSpan>0)%>%group_by(ECD,AS_TimeSpan)%>%arrange(ECD,AS_TimeSpan)%>%summarize(avg=mean(AS_TimeSpan),median=median(AS_TimeSpan))%>%arrange(desc(avg,median))%>%select(ECD,AS_TimeSpan,avg,median)%>%head(10)