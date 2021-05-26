#########################################################################################
### BEGIN: This group of code downloads data for project Seattle PD 911 IR data analysis
#The original data used for this project is zipped and located in Github repository
# https://github.com/khaliunb/Seattle911PDProject/blob/main/data/SeattlePD911IR_80_MB.zip
#########################################################################################

# This part of the code installs required packages for the project if not installed previously: Commented by Khaliun.B 2021.04.11 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
#

library(tidyverse)
library(caret)
library(lubridate)

dl <- tempfile()
download.file("https://github.com/khaliunb/Seattle911PDProject/raw/main/data/SeattlePD911IR_80_MB.zip", dl)
unzip(dl,exdir="data/")
rm(dl)

# Read the data
S911IR<-read_csv("data/SeattlePD911IR_80_MB.csv")
S911IR_Head<-read_csv("data/HeadS911PD.csv",col_names = c("colDesc", "colName"))
#S911IR<-read_csv("/home/spoonmediabigdata/Downloads/archive/dd.csv")
S911IR<-as.data.frame(S911IR)
#########################################################################################
### END: This group of code downloads data for project Seattle PD 911 IR data analysis
#########################################################################################

#########################################################################################
### BEGIN: This group of code performs the data wrangling for Seattle PD 911 IR data analysis
#########################################################################################

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
S911IR<-S911IR%>%mutate(EC_DateTime=parse_date_time(ECDt,'%m/%d/%Y %I:%M:%S %p'))
S911IR<-S911IR%>%mutate(AS_DateTime=parse_date_time(ASTm,'%m/%d/%Y %I:%M:%S %p'))
#"Timespan in minutes between At Scene Time and Event Clearance Date Time","AS_TimeSpan"
S911IR<-S911IR%>%mutate(AS_TimeSpan=round(time_length(AS_DateTime %--% EC_DateTime,"minute")))

#Adding new field names and descriptions to S911IR_Head data frame for reference
S911IR_Head<-bind_rows(S911IR_Head,
                       data.frame(colDesc="Event Clearance Date Converted to Full data time format",colName="EC_DateTime"),
                       data.frame(colDesc="At Scene Time Converted to Full data time format",colName="AS_DateTime"),
                       data.frame(colDesc="Timespan in minutes between At Scene Time and Event Clearance Date Time",colName="AS_TimeSpan"))
#############################################################################
#### END: DATA SET FIELD NAME REFERENCES
#############################################################################

#############################################################################
#### BEGIN: CLEANING THE DATA
#############################################################################

#Basic overview of data, revealed that we probably have incomplete records before June of 2010. Therefore we are trimming the original data to records between 1st of July, 2010 and 1st September, 2017
S911IR<-S911IR%>%filter(EC_DateTime>=make_date(year=2010,month=7,day=1) & EC_DateTime<=make_date(year=2017,month=9,day=1))
#Further examination of data, revealed that we probably have incomplete records before March of 2013 and July of 2014. Therefore we are trimming the original data.
S911IR<-S911IR%>%filter(!(EC_DateTime>make_date(year=2013,month=3,day=1) & EC_DateTime<make_date(year=2014,month=7,day=1)))

#NAs are not permitted in random forest predictors. But the ITDesc, ITSG and ITG field NAs are valuable. Therefore, we are correcting the data by changing ITDesc, ITSG, ITG NAs to "UNKNOWN" character values.
S911IR<-S911IR%>%replace_na(list(ITDesc="UNKNOWN",ITSG="UNKNOWN",ITG="UNKNOWN"))

#For training purpose, we are separating EC_DateTime values into separate fields
S911IR<-S911IR%>%mutate(EC_Year=year(EC_DateTime),EC_Quarter=quarter(EC_DateTime),EC_Month=month(EC_DateTime),EC_Day=day(EC_DateTime),EC_Weekday=wday(EC_DateTime))
#We are choosing features used for prediction, and dropping all the unneeded ones
S911IR<-S911IR%>%select(CAD_CDW_ID,ECC,ECD,ILoc,Longitude,Latitude,ITDesc,EC_Year,EC_Quarter,EC_Month,EC_Day,EC_Weekday,EC_DateTime)

#We are removing all NAs from the data set
S911IR<-S911IR%>%drop_na()
#############################################################################
#### END: CLEANING THE DATA
#############################################################################

####################################################################################
#### BEGIN: This group of code performs the analysis for the report
#### Copy of this group of code exists in the extra/Seattle911PDProject_DataAnalysis.R
####################################################################################

####################################################################################
#### Creating k-means clusters for ECD~ILoc pair
####################################################################################
set.seed(1)
ECDILocMatrix<-S911IR%>%group_by(ECD,ILoc)%>%summarise(count=as.integer(n()))%>%select(ECD,ILoc,count)%>%arrange(count)%>%spread(key=ECD,value=count,fill = 0)
ECDILocMatrix<-as.matrix(ECDILocMatrix)
rownames(ECDILocMatrix)<- ECDILocMatrix[,1]
ECDILocMatrix<- ECDILocMatrix[,-1]
mode(ECDILocMatrix)<-"integer"
rownamesECDILocMatrix<- rownames(ECDILocMatrix)
ECDILocMatrix<- ECDILocMatrix[,-1]
ECDILocMatrix<- sweep(ECDILocMatrix, 1, rowMeans(ECDILocMatrix, na.rm = TRUE))
ECDILocMatrix<- sweep(ECDILocMatrix, 2, colMeans(ECDILocMatrix, na.rm = TRUE))
ECDILocMatrix<-pmax(ECDILocMatrix,0)
rownames(ECDILocMatrix)<- rownamesECDILocMatrix

k <- kmeans(ECDILocMatrix, centers = 100, nstart=25)

#This part of the code assigns group ids calculated by 
#kmeans to "groups" variable
groups <- factor(k$cluster)
temp_g<-data.frame(ILoc=names(groups))

#Mutate group numbers back to the dataset
temp_g<-temp_g%>%mutate(ILocGroup=groups[names(groups)==.$ILoc])

#Mutate groups back to original data
S911IR <- S911IR %>% 
  left_join(temp_g, by='ILoc')

#Removing unnecessary variables
rm(ECDILocMatrix,rownamesECDILocMatrix,temp_g)
################################################################################
#### END: This group of code performs kmeans clustering for ECD~ILoc pair
################################################################################

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
ECDLST1<-S911IR%>%group_by(date(EC_DateTime),ECD)%>%summarise(daily = n())%>%
  ungroup()%>%group_by(ECD)%>%
  summarise(overall=sum(daily),daily_avg=mean(daily),daily_median=median(daily))%>%
  filter(overall>20000)%>%arrange(desc(overall))%>%
  select(ECD,overall,daily_avg,daily_median)
ECDLST1%>%knitr::kable()
# We can also see group of bars that have occurences between 8'000 and 20'000. And do they what is their average occurence on daily basis 
#  - Which are the event clearance description that form the group?
ECDLST2<-S911IR%>%group_by(date(EC_DateTime),ECD)%>%summarise(daily = n())%>%
  ungroup()%>%group_by(ECD)%>%
  summarise(overall=sum(daily),daily_avg=mean(daily),daily_median=median(daily))%>%
  filter(overall>=8000 & overall<=20000)%>%
  arrange(desc(overall))%>%
  select(ECD,overall,daily_avg,daily_median)
ECDLST2%>%knitr::kable()
# Those ECDs that are present, but can be viewed as separate occurences should also be noted.
#  - So, what are the ECDs that have barely existent occurences?
ECDLST3<-S911IR%>%filter(!((EC_DateTime>make_date(year=2014,month=2,day=1))&
                             (EC_DateTime<make_date(year=2014,month=7,day=1))))%>%
  group_by(date(EC_DateTime),ECD)%>%summarise(daily = n()) %>%
  ungroup()%>%group_by(ECD)%>%
  summarise(overall=sum(daily),daily_avg=mean(daily),daily_median=median(daily))%>%
  filter(overall<50)%>%arrange(overall)%>%select(ECD,overall,daily_avg,daily_median)
ECDLST3%>%knitr::kable()
# We are using boxplot to see more clear picture. And here we are considering group of ECDs that have overall occurence of more than 8'000
S911IR%>%group_by(date(EC_DateTime),ECD)%>%summarise(daily = n()) %>% ungroup()%>%group_by(ECD)%>%
  summarise(overall=sum(daily),daily_avg=mean(daily),daily_median=median(daily))%>%
  filter(overall>=8000)%>%
  left_join(S911IR,by="ECD")%>%group_by(date(EC_DateTime),ECD)%>%summarise(daily = n())%>%
  ggplot(aes(x=reorder(ECD,daily,na.rm=TRUE),y=daily))+geom_boxplot()+coord_flip()+
  labs(y="Daily occurence", x="Event Clearance Description", subtitle="ECDs with large overall occurences:Daily occurence In Descending Order")
#For full data, the trimming point for "Large overall" occurences had been above 20'000. But for Sample data it had been 8'000 for this plot. We are comparing the results side by side. But the overall picture is the same.

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
#For full data, the trimming point for "Large ECD" occurences had been above 50'000. But for Sample data it had been 20'000 for this plot. We are comparing the results side by side. But the overall picture is the same.

#For this Dot plot, alpha opaqueness represents high daily average Initial Type Description.

#From this "Total occurences of Initial Type Description within most occuring Event clearance Description" plot we see clearly that:
#- "NA"s is ITDesc occur most frequently
#- ECD "Suspicious person","Disturbance, Other" is most prevalent for most reasons. Which we can attribute to the fact that this type of description is applicable to most circumstances.
#- ECD "PARKING VIOLATIONS (EXCEPT ABANDONED VEHICLES)" have the least daily average for ECD, but the most daily average for ITDesc. Which means while this type of ECD happens less, people identify the reason for this ECD most clearly.

#From full data, we see that:
#- DETOX - REQUEST FOR result in DISTURBANCE, OTHER
#- PARKING VIOLATIONS (EXCEPT ABANDONED VEHICLES), TRAFFIC - MOVING VIOLATION in ITDesc match the ECD directly
#And here we see another more insightful aspect:
#- SHOTS - IP/JO - INCLUDES HEARD/NO ASSAULT results in Event Clearance Description of SUSPICIOUS PERSON
#This may describe the degree of fear of guns and shots in Seattle citizens, and the fact that these rarely are founded assumptions.

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
#For this version of the plot, color represents ITDesc groups, while ECDs are represented by shape.
#For full data, the trimming point for "Large ECD" occurences had been above 50'000. But for Sample data it had been 20'000 for this plot. We are comparing the results side by side. But the overall picture is the same.

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
  ggplot(aes(month(EC_Day), mean(daily))) + geom_col() + geom_smooth() + theme(axis.text.y = element_blank(),axis.text.x = element_blank())+
  labs(y="Daily Averages by Month", x="Month", subtitle="")
#From the histogram, we see surge of daily averages in July and August.

#     - During which season event clearance description/subgroup/description overall occurrence increases/decreases?
S911IR%>%mutate(EC_Year=year(EC_DateTime),EC_Quarter=quarter(EC_DateTime))%>%group_by(EC_Year,EC_Quarter)%>%arrange(EC_Year,EC_Quarter)%>%summarize(count=n())%>%group_by(EC_Quarter)%>%summarise(avgbyMonth=mean(count))%>%
  ggplot(aes(x=EC_Quarter,y=avgbyMonth))+geom_col()+geom_smooth()+
  theme(axis.text.y = element_blank(),axis.text.x = element_blank())+
  labs(y="Quarterly Average", x="Quarter", subtitle="")
#We can see definite surge of total incidents in 3rd quarter.

S911IR%>%mutate(EC_Day=date(EC_DateTime))%>%group_by(EC_Day,ECD)%>%summarise(daily = n()) %>%
  ggplot(aes(quarter(EC_Day), mean(daily))) + geom_col() + theme(axis.text.y = element_blank(),axis.text.x = element_blank())+
  labs(y="Daily Averages by Quarter", x="Quarter", subtitle="")
#We can see definite surge of daily average number of incidents in 3rd quarter.

#     - On which day of the week event clearance description/subgroup/description overall occurrence increases/decreases?
S911IR%>%mutate(EC_Day=date(EC_DateTime))%>%group_by(EC_Day,ECD)%>%summarise(daily = n()) %>%
  ggplot(aes(wday(EC_Day), mean(daily))) + geom_col() + theme(axis.text.y = element_blank(),axis.text.x = element_blank())+
  labs(y="Daily Averages by Weekday", x="Weekday", subtitle="")
# According to the plot, none of the weekdays can be viewed as special.

#     - During which hour of the day event clearance description/subgroup/description overall occurrence increases/decreases?
S911IR%>%mutate(EC_Day=date(EC_DateTime),Hour=hour(EC_DateTime))%>%group_by(EC_Day,Hour)%>%summarise(hourly = n()) %>%
  ggplot(aes(Hour, mean(hourly))) + geom_col() + theme(axis.text.y = element_blank(),axis.text.x = element_blank())+
  labs(y="Hourly Average Occurences of ECD", x="Hour", subtitle="")
#Hourly averages are practically the same. But in the sampled data, there is slight drop between 7AM and 8AM in daily average occurence.

#-----------------------------------------------------------
#  - Is there relation between location and event clearance description/subgroup/description?
#     - Is there certain prevalence in overall number of event clearance description/subgroup/description in certain location?

# We are now proceeding with the cleaned data prepared for training.
# - Are there certain locations where calls to 911 prevail?
#We are further considering the yearly median for each location. And we are going to determine the value for average yearly Incident Response encounter in Seattle for median by month of each location
loc_yearly_median<-S911IR%>%group_by(ILoc,EC_Year)%>%summarize(count=as.integer(n()))%>%select(ILoc,EC_Year,count)%>%group_by(ILoc)%>%summarise(year_med=median(count))%>%select(ILoc,year_med)
seattle_loc_med_year_mean<-mean(loc_yearly_median$year_med)
seattle_loc_med_year_mean
#Now we are ready to determine which locations exceed yearly norm for average location incident. From there, we are able to determine the monthly averages for these locations and filter out above average locations by months.
locMatrix<-S911IR%>%group_by(ILoc,EC_Year,EC_Month)%>%summarise(count=as.integer(n()))%>%group_by(ILoc,EC_Month,)%>%summarize(month_median=median(count))%>%select(ILoc,EC_Month,month_median)%>%spread(key=EC_Month,value=month_median,fill = 0)%>%mutate(month_sum = sum(c_across(where(is.numeric))))
locMatrix<-locMatrix%>%filter(month_sum>=seattle_loc_med_year_mean)%>%arrange(desc(month_sum))%>%select(-"month_sum")
locMatrix<-as.matrix(locMatrix)
rownames(locMatrix)<- locMatrix[,1]
locMatrix<- locMatrix[,-1]
mode(locMatrix)<-"integer"
locMatrix[1:4,]%>%knitr::kable(align="c")

locMatrix <- sweep(locMatrix, 2, colMeans(locMatrix, na.rm = TRUE))
mode(locMatrix)<-"integer"
#Replace all the negative values with 0
locMatrix<-pmax(locMatrix,0)
locMatrix<-locMatrix[rowSums(locMatrix)>0,]
locMatrix[1:4,]%>%knitr::kable(align="c")

locMatrix_Sums<-as.matrix(rowSums(locMatrix))
locMatrix_Sums[15400:15463,]%>%knitr::kable(align="c")

locMatrix_Sums <- sweep(locMatrix_Sums, 1, rowMeans(locMatrix, na.rm = TRUE))
#Here we have locations that have persistent occurences of ECDs by average and median
conLocs<-as.list(rownames(locMatrix_Sums>0))

#Now, let us see whether same ECDs occur at these locations. We count the groups of ECDs that have occurence for each of those locations and average the results by ECD.
ECDTypesByLoc<-S911IR%>%filter(ILoc %in% conLocs)%>%select(ILoc,ECD)%>%unique()%>%group_by(ILoc)%>%summarise(ECDTypeCount=log10(n()))%>%select(ILoc,ECDTypeCount)
ECDCountsByLoc<-S911IR%>%filter(ILoc %in% conLocs)%>%group_by(ILoc,Longitude,Latitude)%>%summarise(ECDCount=log10(n()))
ECDCountTypesByLoc<-ECDCountsByLoc%>%left_join(ECDTypesByLoc,by="ILoc")%>%mutate(ECDCountLog=log(ECDCount,ECDTypeCount))%>%arrange(desc(ECDCount),desc(ECDTypeCount))
#Let us see where exactly these points lie on the map by using Lattitude and Longitude. And how intense they look. But we are not using every location. Just the top 1000 locations by ocurrence of ECDs
ECDCountTypesByLoc%>%head(1000)%>%
  ggplot(aes(x=Longitude,y=Latitude))+geom_point(aes(colour=ECDTypeCount,size=ECDCount,alpha=0.05))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))+
  guides(color = guide_legend(order=1),
         size = guide_legend(order=2),
         alpha = FALSE)+
  scale_y_log10(labels=NULL)+
  labs(y="", x="ECD Intensity Map By Location", subtitle="")
#We can see that at outskirts of the city, same ECDs occur in larger number

#But what happens if we facet the same plot by month? But we have to choose the most intense locations for this. We are choosing 100 top intense locations.
intenseLocs<-ECDCountTypesByLoc%>%select(Longitude,Latitude,ILoc)%>%distinct()%>%head(100)%>%pull(ILoc)%>%as.list()
ECDTypesByLocMonth<-S911IR%>%filter(ILoc %in% intenseLocs)%>%select(ILoc,EC_Month,ECD)%>%unique()%>%group_by(ILoc,EC_Month)%>%summarise(ECDTypeCount=log10(n()))%>%select(ILoc,EC_Month,ECDTypeCount)
ECDCountsByLocMonth<-S911IR%>%filter(ILoc %in% intenseLocs)%>%group_by(ILoc,EC_Year,EC_Month,Longitude,Latitude)%>%summarise(ECDCount=n())%>%group_by(ILoc,EC_Month,Longitude,Latitude)%>%summarise(ECDCountMonthlyAvg=mean(ECDCount))
ECDCountTypesByLocMonth<-ECDCountsByLocMonth%>%left_join(ECDTypesByLocMonth,by=c("ILoc","EC_Month"))%>%mutate(ECDCountLog=log(ECDCountMonthlyAvg,ECDTypeCount))%>%arrange(EC_Month,desc(ECDCountMonthlyAvg),desc(ECDTypeCount))
ECDCountTypesByLocMonth%>%
  ggplot(aes(x=Longitude,y=Latitude))+geom_point(aes(colour=ECDTypeCount,size=ECDCountMonthlyAvg,alpha=0.05))+facet_wrap(.~EC_Month)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))+
  guides(color = guide_legend(order=1),
         size = guide_legend(order=2),
         alpha = FALSE)+
  scale_y_log10(labels=NULL)+
  labs(y="", x="ECD Intensity Map Faceted by Month (Top 100 locations)", subtitle="")

#We can do the same for weekdays?
ECDTypesByLocWeekday<-S911IR%>%filter(ILoc %in% intenseLocs)%>%select(ILoc,EC_Weekday,ECD)%>%unique()%>%group_by(ILoc,EC_Weekday)%>%summarise(ECDTypeCount=log10(n()))%>%select(ILoc,EC_Weekday,ECDTypeCount)
ECDCountsByLocWeekday<-S911IR%>%filter(ILoc %in% intenseLocs)%>%group_by(ILoc,EC_Year,EC_Month,EC_Day,EC_Weekday,Longitude,Latitude)%>%summarise(ECDCount=n())%>%group_by(ILoc,EC_Weekday,Longitude,Latitude)%>%summarise(ECDDayAvgByWeekday=mean(ECDCount))
ECDCountTypesByLocWeekday<-ECDCountsByLocWeekday%>%left_join(ECDTypesByLocWeekday,by=c("ILoc","EC_Weekday"))%>%arrange(EC_Weekday,desc(ECDDayAvgByWeekday),desc(ECDTypeCount))
ECDCountTypesByLocWeekday%>%
  ggplot(aes(x=Longitude,y=Latitude))+geom_point(aes(colour=ECDTypeCount,size=ECDDayAvgByWeekday,alpha=0.05))+facet_wrap(.~EC_Weekday)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))+
  guides(color = guide_legend(order=1),
         size = guide_legend(order=2),
         alpha = FALSE)+
  scale_y_log10(labels=NULL)+
  labs(y="", x="ECD Intensity Map Faceted by Weekday (Top 100 locations)", subtitle="")

#Performing chi-squared test for pairs of matrices ECD~ITDesc, ECD~Month, ECD~Weekday, ECD~Iloc
#As all of the data are non-continuous, we are turning to Pearson's Chi-Squared test for Count Data to determine the correlation between ECD~ITDesc, ECD~Month, ECD~Weekday, ECD~Iloc pairs separately.

#ECD~ITDesc pair results
ECDITDescMatrix<-S911IR%>%group_by(ECD,ITDesc)%>%summarise(count=as.integer(n()))%>%select(ECD,ITDesc,count)%>%arrange(count)%>%spread(key=ECD,value=count,fill = 0)
ECDITDescMatrix<-as.matrix(ECDITDescMatrix)
rownames(ECDITDescMatrix)<- ECDITDescMatrix[,1]
ECDITDescMatrix<- ECDITDescMatrix[,-1]
mode(ECDITDescMatrix)<-"integer"
ECDITDescMatrix[1:3,1:2]%>%knitr::kable(align="c")
ECDITDescMatrix<- sweep(ECDITDescMatrix, 1, rowMeans(ECDITDescMatrix, na.rm = TRUE))
ECDITDescMatrix<- sweep(ECDITDescMatrix, 2, colMeans(ECDITDescMatrix, na.rm = TRUE))
ECDITDescMatrix<-pmax(ECDITDescMatrix,0)
image(ECDITDescMatrix)

ECD_ITDescChi<-chisq.test(ECDITDescMatrix)
ECD_ITDescChi$p.value
#ECD~EC_Month pair results
ECDEC_MonthMatrix<-S911IR%>%group_by(ECD,EC_Month)%>%summarise(count=as.integer(n()))%>%select(ECD,EC_Month,count)%>%arrange(count)%>%spread(key=ECD,value=count,fill = 0)
ECDEC_MonthMatrix<-as.matrix(ECDEC_MonthMatrix)
rownames(ECDEC_MonthMatrix)<- ECDEC_MonthMatrix[,1]
ECDEC_MonthMatrix<- ECDEC_MonthMatrix[,-1]
mode(ECDEC_MonthMatrix)<-"integer"
image(ECDEC_MonthMatrix)

ECDEC_MonthMatrix<- sweep(ECDEC_MonthMatrix, 1, rowMeans(ECDEC_MonthMatrix, na.rm = TRUE))
ECDEC_MonthMatrix<- sweep(ECDEC_MonthMatrix, 2, colMeans(ECDEC_MonthMatrix, na.rm = TRUE))
ECDEC_MonthMatrix<-pmax(ECDEC_MonthMatrix,0)
image(ECDEC_MonthMatrix)
ECD_ITDescChi<-chisq.test(ECDEC_MonthMatrix)
ECD_ITDescChi$p.value
#ECD~EC_Weekday pair results
ECDEC_WeekdayMatrix<-S911IR%>%group_by(ECD,EC_Weekday)%>%summarise(count=as.integer(n()))%>%select(ECD,EC_Weekday,count)%>%arrange(count)%>%spread(key=ECD,value=count,fill = 0)
ECDEC_WeekdayMatrix<-as.matrix(ECDEC_WeekdayMatrix)
rownames(ECDEC_WeekdayMatrix)<- ECDEC_WeekdayMatrix[,1]
ECDEC_WeekdayMatrix<- ECDEC_WeekdayMatrix[,-1]
mode(ECDEC_WeekdayMatrix)<-"integer"
image(ECDEC_WeekdayMatrix)
ECD_ITDescChi<-chisq.test(ECDEC_WeekdayMatrix)
ECD_ITDescChi$p.value

ECDEC_WeekdayMatrix<- sweep(ECDEC_WeekdayMatrix, 1, rowMeans(ECDEC_WeekdayMatrix, na.rm = TRUE))
ECDEC_WeekdayMatrix<- sweep(ECDEC_WeekdayMatrix, 2, colMeans(ECDEC_WeekdayMatrix, na.rm = TRUE))
ECDEC_WeekdayMatrix<-pmax(ECDEC_WeekdayMatrix,0)
image(ECDEC_WeekdayMatrix)
ECD_ITDescChi<-chisq.test(ECDEC_WeekdayMatrix)
ECD_ITDescChi$p.value
#ECD~ILoc pair results
set.seed(4)
ECDILocMatrix<-S911IR%>%group_by(ECD,ILoc)%>%summarise(count=as.integer(n()))%>%select(ECD,ILoc,count)%>%arrange(count)%>%spread(key=ECD,value=count,fill = 0)
ECDILocMatrix<-as.matrix(ECDILocMatrix)
rownames(ECDILocMatrix)<- ECDILocMatrix[,1]
ECDILocMatrix<- ECDILocMatrix[,-1]
mode(ECDILocMatrix)<-"integer"
ECDILocMatrix<- ECDILocMatrix[,-1]
ECDILocMatrix<- sweep(ECDILocMatrix, 1, rowMeans(ECDILocMatrix, na.rm = TRUE))
ECDILocMatrix<- sweep(ECDILocMatrix, 2, colMeans(ECDILocMatrix, na.rm = TRUE))
ECDILocMatrix<-pmax(ECDILocMatrix,0)
ECD_ITDescChi<-chisq.test(ECDILocMatrix)
ECD_ITDescChi$p.value

############################
#We are re-creating the Seattle map ECD dot plot with groups as colors. We will deal with the largest group number 14 later
ECDTypesByLoc<-S911IR%>%filter(ILocGroup!=14)%>%select(ILoc,ILocGroup,ECD)%>%unique()%>%group_by(ILoc,ILocGroup)%>%summarise(ECDTypeCount=n())%>%select(ILoc,ILocGroup,ECDTypeCount)
ECDCountsByLoc<-S911IR%>%filter(ILocGroup!=14)%>%group_by(ILoc,Longitude,Latitude)%>%summarise(ECDCount=log10(n()))
ECDCountTypesByLoc<-ECDCountsByLoc%>%left_join(ECDTypesByLoc,by="ILoc")%>%mutate(ECDCountLog=log(ECDCount,ECDTypeCount))%>%arrange(desc(ECDCount),desc(ECDTypeCount))
ECDCountTypesByLoc%>%head()
#Let us see where exactly these points lie on the map by using Lattitude and Longitude. And how intense they look. But we are not using every location. This is the plot for all the data except for group 10.
ECDCountTypesByLoc%>%
  ggplot(aes(x=Longitude,y=Latitude))+geom_point(aes(colour=ILocGroup,alpha=ECDTypeCount))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))+
  guides(color = FALSE,
         size = FALSE,
         alpha = guide_legend(order=1))+
  scale_y_log10(labels=NULL)+
  labs(y="", x="KMeans Clusters Map By Location", subtitle="")

#Now, let's see where the group 10 lies on a separate plot. This group is the largest one
ECDTypesByLoc<-S911IR%>%filter(ILocGroup==14)%>%select(ILoc,ILocGroup,ECD)%>%unique()%>%group_by(ILoc,ILocGroup)%>%summarise(ECDTypeCount=n())%>%select(ILoc,ILocGroup,ECDTypeCount)
ECDCountsByLoc<-S911IR%>%filter(ILocGroup==14)%>%group_by(ILoc,Longitude,Latitude)%>%summarise(ECDCount=log10(n()))
ECDCountTypesByLoc<-ECDCountsByLoc%>%left_join(ECDTypesByLoc,by="ILoc")%>%mutate(ECDCountLog=log(ECDCount,ECDTypeCount))%>%arrange(desc(ECDCount),desc(ECDTypeCount))
#Let us see where exactly these points lie on the map by using Lattitude and Longitude. And how intense they look. But we are not using every location. This is the plot for group 10.
ECDCountTypesByLoc%>%
  ggplot(aes(x=Longitude,y=Latitude))+geom_point(aes(colour=ECDTypeCount,alpha=ECDCount))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))+
  guides(color = guide_legend(order=1),
         size = guide_legend(order=2),
         alpha = FALSE)+
  scale_y_log10(labels=NULL)+
  labs(y="", x="KMeans Clusters Map By Location: Largest Group", subtitle="")
####################################################################################
#### END: This group of code performs the analysis for the report
####################################################################################

####################################################################################
#### BEGIN: Preparing the data for training
####################################################################################
#And we are turning ECD value into numbers and naming the feature ECDn
ECDCodes<-S911IR%>%distinct(ECD)
ECDCodes<-as.data.frame(ECDCodes)
dfECDCodes<-data.frame(ECDn=seq(1:nrow(ECDCodes)),"ECD"=ECDCodes)
rm(ECDCodes)
#And we are turning ITDesc value into numbers and naming the feature ITDescN
ITDescCodes<-S911IR%>%distinct(ITDesc)
ITDescCodes<-as.data.frame(ITDescCodes)
dfITDescCodes<-data.frame(ITDescN=seq(1:nrow(ITDescCodes)),"ITDesc"=ITDescCodes)
rm(ITDescCodes)
#And we are turning ILoc value into numbers and naming the feature ILocN
ILocCodes<-S911IR%>%distinct(ILoc)
ILocCodes<-as.data.frame(ILocCodes)
dfILocCodes<-data.frame(ILocN=seq(1:nrow(ILocCodes)),"ILoc"=ILocCodes)
rm(ILocCodes)
#We are incorporating all the codes into the original data set
S911IR<-S911IR%>%left_join(dfECDCodes,by="ECD")
S911IR<-S911IR%>%left_join(dfITDescCodes,by="ITDesc")
S911IR<-S911IR%>%left_join(dfILocCodes,by="ILoc")
#And we are trimming the NA values again, just in case
S911IR<-S911IR%>%drop_na()
####################################################################################
#### END: Preparing the data for training
####################################################################################

#############################################################################
#### BEGIN: PREPARE TRAINING AND TEST SET
#############################################################################
#This part of the code divides S911IR data into
# 80%:20% training set named "train_set" and test set named "train_set": Commented by Khaliun.B 2021.05.24
#set.seed(47)
S911IR_trainsmall<-sample_n(S911IR,100000)

test_index <- createDataPartition(y = S911IR_trainsmall$ECD, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- S911IR_trainsmall[-test_index,]
test_set <- S911IR_trainsmall[test_index,]

#############################################################################
#### END: DATA SET FIELD NAME REFERENCES
#############################################################################

#########################################################################################
### END: This group of code performs the data wrangling for Seattle PD 911 IR data analysis
#########################################################################################

################################################################################
#### BEGIN: This group of code performs Random Forest and KNN training
#### This group of code takes a long time to run. So please be careful.
################################################################################
train_set<-train_set%>%select(ECD,Latitude,Longitude,ITDescN,EC_Year,EC_Quarter,EC_Month,EC_Day,EC_Weekday)
test_set<-test_set%>%select(ECD,Latitude,Longitude,ITDescN,EC_Year,EC_Quarter,EC_Month,EC_Day,EC_Weekday)

#### We are training the Random Forest model. Training script ran 80 minutes.
train_rf <-  train(train_set[, -1], factor(train_set[,1]),
                   method = "rf",
                   nTree = 500,
                   tuneGrid = data.frame(mtry = seq(10, 200, 10)),
                   nSamp = 10000)
ggplot(train_rf)
train_rf$bestTune%>%knitr::kable()
varImp(train_rf)
library(randomForest)
fit_rf <- randomForest(train_set[, -1], factor(train_set[,1]), ntree = 500, mtry = 170)
importance(fit_rf)
varImpPlot(fit_rf,type=2)
y_hat_rf <- predict(fit_rf, test_set[ ,-1])
cm <- confusionMatrix(y_hat_rf, factor(test_set[,1]))
cm$overall%>%knitr::kable()

#### Now we are training Knn model with 3 features. This script ran 192 minutes.
train_set<-train_set%>%select(ECD,Latitude,Longitude,ITDescN)
test_set<-test_set%>%select(ECD,Latitude,Longitude,ITDescN)

train_knn <- train(train_set[ ,-1], factor(train_set[,1]),
                   method = "knn",
                   tuneGrid = data.frame(k = seq(10, 300, 10)))
plot(train_knn)
train_knn$bestTune%>%knitr::kable()
max(train_knn$results$Accuracy)%>%knitr::kable()
################################################################################
#### END: This group of code performs KNN training and prediction
################################################################################