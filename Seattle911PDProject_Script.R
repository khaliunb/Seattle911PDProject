#########################################################################################
### BEGIN: This group of code downloads data for project Seattle PD 911 IR data analysis
#The original data used for this project is zipped and located in Github repository
# https://github.com/khaliunb/Seattle911PDProject/blob/main/data/SeattlePD911IR_80_MB.zip
#########################################################################################
library(tidyverse)
dl <- tempfile()
download.file("https://github.com/khaliunb/Seattle911PDProject/raw/main/data/SeattlePD911IR_80_MB.zip", dl)
unzip(dl,exdir="data/")
rm(dl)
# Read the data
S911IR_Head<-read_csv("data/HeadS911PD.csv")
S911IR<-read_csv("data/SeattlePD911IR_80_MB.csv")
#########################################################################################
### END: This group of code downloads data for project Seattle PD 911 IR data analysis
#########################################################################################

#########################################################################################
### BEGIN: This group of code performs the data wrangling for Seattle PD 911 IR data analysis
#########################################################################################

# We need to see distinct values for ECD, ESCG, ECG and compare them to ITDesc, ITSG, ITG
S911IR%>%select(ECD,ECSG,ECG,ITDesc,ITSG,ITG)

# We also need to csee the values for ECC and figure out how the same ECC values are related

# We need to see what IT leads to what EC?

# Preparing the Escalation/De-Escalation values and mutating them back to the data set

#########################################################################################
### END: This group of code performs the data wrangling for Seattle PD 911 IR data analysis
#########################################################################################