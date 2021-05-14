source("../Seattle911PDProject_Script.R", local = knitr::knit_global())

S911IR%>%select(ECD,ECSG,ECG,ITDesc,ITSG,ITG)

#  - Which are the event clearance groups/subgroup/description that occur the most?



#  - Which are the most frequently occurring event clearance group and incident type group pair?/Is there relation between incident type group/subgroup/description and event clearance group/subgroup/description?
#  - Is there relation between location and event clearance description/subgroup/description?
#  - Is there certain prevalence in overall number of event clearance description/subgroup/description in certain location?
#  - Is there relation between timing and event clearance description/subgroup/description?
#  - Was there increase/decrease in event clearance description/subgroup/description overall occurrence over time?
#  - During which season event clearance description/subgroup/description overall occurrence increases/decreases?
#  - On which day of the week event clearance description/subgroup/description overall occurrence increases/decreases?
#  - During which hour of the day event clearance description/subgroup/description overall occurrence increases/decreases?
  
#  - Is there relation between number of recurrence of certain incident type group/subgroup/description in data and event clearance group/subgroup/description?
#  - Is there relation between number of recurrence of certain event clearance group/subgroup/description in data and timing of the incident type group/subgroup/description?
#  - Is there relation between number of recurrence of certain event clearance group/subgroup/description in data and location of the incident type group/subgroup/description?
#  - Is there certain pattern of increase/decrease recurrence of certain event clearance group/subgroup/description in data in relation to location?
#  - Is there certain prevalence in number of certain event clearance description/subgroup/description in certain location?
#  - Is there certain pattern of increase/decrease recurrence of certain event clearance group/subgroup/description in data in relation to timing?
#  - Was there increase/decrease in certain event clearance description/subgroup/description occurrence over time?
#  - During which season certain event clearance description/subgroup/description occurrence increases/decreases?
#  - On which day of the week certain event clearance description/subgroup/description occurrence increases/decreases?
#  - During which hour of the day certain event clearance description/subgroup/description occurrence increases/decreases?