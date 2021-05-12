#The original data used for this project is zipped and located in Github repository
# https://github.com/khaliunb/Seattle911PDProject/blob/main/data/SeattlePD911IR_80_MB.zip

library(tidyverse)
dl <- tempfile()
download.file("https://github.com/khaliunb/Seattle911PDProject/raw/main/data/SeattlePD911IR_80_MB.zip", dl)
S911IR<-read_csv(unzip(dl,exdir="data/"))
rm(dl)
summary(S911IR)
