source("../Seattle911PDProject_Script.R", local = knitr::knit_global())

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

#     - On average, how many minutes does it take from At Scene Time and Event clearance Time for Event clearance description/subgroup/group? And which Event Clearance description/subgroup/group is most time consuming? Which ones are most quick to be resolved?
S911IR%>%filter(!is.na(AS_TimeSpan)&AS_TimeSpan>0)%>%group_by(ECD,AS_TimeSpan)%>%arrange(ECD,AS_TimeSpan)%>%summarize(avg=mean(AS_TimeSpan),median=median(AS_TimeSpan))%>%arrange(desc(avg,median))%>%select(ECD,AS_TimeSpan,avg,median)%>%head(10)
#Training set variable importance
#Chosen features for predictors are: ILoc, ITDesc, month(EC_DateTime), quarter(EC_DateTime)
#We are predicting: ECD

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