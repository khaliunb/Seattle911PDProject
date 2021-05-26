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
S911IR<-S911IR%>%select(CAD_CDW_ID,ECC,ECD,ILoc,Longitude,Latitude,ITDesc,EC_Year,EC_Quarter,EC_Month,EC_Day,EC_Weekday)

#We are removing all NAs from the data set
S911IR<-S911IR%>%drop_na()
#############################################################################
#### END: CLEANING THE DATA
#############################################################################

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