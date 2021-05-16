#########################################################################################
### BEGIN: This group of code downloads data for project Seattle PD 911 IR data analysis
#The original data used for this project is zipped and located in Github repository
# https://github.com/khaliunb/Seattle911PDProject/blob/main/data/SeattlePD911IR_80_MB.zip
#########################################################################################
library(tidyverse)
#library(caret)
library(data.table)
#dl <- tempfile()
#download.file("https://github.com/khaliunb/Seattle911PDProject/raw/main/data/SeattlePD911IR_80_MB.zip", dl)
#unzip(dl,exdir="data/")
#rm(dl)
# Read the data
S911IR_Head<-read_csv("data/HeadS911PD.csv",col_names = c("colDesc", "colName"))
S911IR<-read_csv("data/SeattlePD911IR_80_MB.csv")
S911IR<-as.data.frame(S911IR)
#########################################################################################
### END: This group of code downloads data for project Seattle PD 911 IR data analysis
#########################################################################################

#########################################################################################
### BEGIN: This group of code performs the data wrangling for Seattle PD 911 IR data analysis
#########################################################################################
#     (!) Basic overview of data, revealed that we probably have incomplete records before June of 2010. Therefore we are trimming the original data to records between 1st of July, 2010 and 1st September, 2017
S911IR%>%filter(!is.na(EC_DateTime) & EC_DateTime>=make_date(year=2010,month=6,day=1))%>%mutate(EC_Year=year(EC_DateTime),EC_Month=month(EC_DateTime))%>%group_by(EC_Year,EC_Month)%>%arrange(EC_Year,EC_Month)%>%summarize(count=n())%>%arrange(EC_Year,EC_Month)%>%select(EC_Year,EC_Month,count)
S911IR<-S911IR%>%filter(!is.na(EC_DateTime) & EC_DateTime>=make_date(year=2010,month=7,day=1) & EC_DateTime<=make_date(year=2017,month=9,day=1))

# We need to see distinct values for ECD, ESCG, ECG and compare them to ITDesc, ITSG, ITG
#S911IR%>%select(ECD,ECSG,ECG,ITDesc,ITSG,ITG)

# We also need to csee the values for ECC and figure out how the same ECC values are related

# We need to see what IT leads to what EC?

# Preparing the Escalation/De-Escalation values and mutating them back to the data set

#########################################################################################
### END: This group of code performs the data wrangling for Seattle PD 911 IR data analysis
#########################################################################################