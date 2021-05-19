#########################################################################################
### BEGIN: This group of code downloads data for project Seattle PD 911 IR data analysis
#The original data used for this project is zipped and located in Github repository
# https://github.com/khaliunb/Seattle911PDProject/blob/main/data/SeattlePD911IR_80_MB.zip
#########################################################################################
library(tidyverse)
library(caret)
library(lubridate)

#dl <- tempfile()
#download.file("https://github.com/khaliunb/Seattle911PDProject/raw/main/data/SeattlePD911IR_80_MB.zip", dl)
#unzip(dl,exdir="data/")
#rm(dl)

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

#     (!) Basic overview of data, revealed that we probably have incomplete records before June of 2010. Therefore we are trimming the original data to records between 1st of July, 2010 and 1st September, 2017

#S911IR%>%filter(!is.na(EC_DateTime) & EC_DateTime>=make_date(year=2010,month=6,day=1))%>%mutate(EC_Year=year(EC_DateTime),EC_Month=month(EC_DateTime))%>%group_by(EC_Year,EC_Month)%>%arrange(EC_Year,EC_Month)%>%summarize(count=n())%>%arrange(EC_Year,EC_Month)%>%select(EC_Year,EC_Month,count)
#S911IR<-S911IR%>%filter(!is.na(EC_DateTime) & EC_DateTime>=make_date(year=2010,month=7,day=1) & EC_DateTime<=make_date(year=2017,month=9,day=1))

#############################################################################
#### BEGIN: PREPARE TRAINING AND TEST SET
#############################################################################
#This part of the code divides S911IR data into
# 80%:20% training set named "train_set" and test set named "train_set": Commented by Khaliun.B 2021.04.12
#set.seed(47)

test_index <- createDataPartition(y = S911IR$ECD, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- S911IR[-test_index,]
test_set <- S911IR[test_index,]

#This part of the code does the semi-joins test_set with training set first using movieId
#and second using userId: Commented by Khaliun.B 2021.04.12
test_set <- test_set %>% 
  semi_join(train_set, by = "CAD_CDW_ID")
#############################################################################
#### END: DATA SET FIELD NAME REFERENCES
#############################################################################

#########################################################################################
### END: This group of code performs the data wrangling for Seattle PD 911 IR data analysis
#########################################################################################