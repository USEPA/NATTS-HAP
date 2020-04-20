library(dplyr)

################################################################
#  This R script creates some of the numbers that were used in #
#  the paper such as 5 year average cancer risk and ranges of  #
#  percent contribution to cancer risk from chemical groups    #
################################################################

df <- read.csv("../Data/FilteredNattsHaps.csv")

#### Five year average cancer risk by NATTS
avgCR <- df %>% group_by(LOCATION, AQS_PARAMETER_NAME) %>% dplyr::summarize("mean" = mean(CRinAmil))
avgCR <- avgCR %>% group_by(LOCATION) %>% dplyr::summarize("mean" = sum(mean))


#### Average percent contribution to cancer risk from chem groups (carbonyl, PAH, PM, VOC)
group_class <- df %>% group_by(LOCATION, ChemClass, AQS_PARAMETER_NAME, SETTING) %>% 
  dplyr::summarize("CRinAmil" = mean(CRinAmil),
            "numCheminGroup" = length(ChemClass))
grp5yravg <- group_class %>% group_by(LOCATION, ChemClass, SETTING) %>%
  dplyr::summarize("CR5YrAvg" = sum(CRinAmil),
            "numYears" = length(ChemClass))
group_site <- grp5yravg %>% group_by(LOCATION, SETTING) %>%
  dplyr::summarize("totalCRinAmil" = sum(CR5YrAvg))
merged <- merge(grp5yravg, group_site, by = c("LOCATION", "SETTING"), all.x = TRUE)
merged$pcentGroupCR <- merged$CR5YrAvg/merged$totalCRinAmil
merged <- merge(merged, avgCR, by = "LOCATION")
merged <- subset(merged, select = c("LOCATION", "ChemClass", "pcentGroupCR", "mean"))
merged$pcent <- merged$pcentGroupCR * 100

Carbonyl <- subset(merged, ChemClass == "Carbonyl")
min(Carbonyl$pcent)
max(Carbonyl$pcent)

VOC <- subset(merged, ChemClass == "VOC")
min(VOC$pcent)
max(VOC$pcent)

PAH <- subset(merged, ChemClass == "PAH")
min(PAH$pcent)
max(PAH$pcent)

PM <- subset(merged, ChemClass == "PM")
min(PM$pcent)
max(PM$pcent)


