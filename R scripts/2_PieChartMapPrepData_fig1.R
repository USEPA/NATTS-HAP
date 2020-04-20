library(dplyr); library(ggplot2)

#################################################################################
#  This script creates a dataframe that contains the percent contribution of    #
#  chemical classes (ie. VOC, Carbonyl, PM, PAH) to overall CR at each NATTS    #
#  based on 5 year average chemical concentrations. The resulting df is then    #
#  used by a python script to create pie charts that show % contribution from   #
#  each chem class and graph onto a map of the US                               #
#################################################################################

### Read in filtered NATTS df
df <- read.csv("../Data/filteredNATTS2017v3.csv")  # change path if necessary

### Calculate average 5 year cancer risk for each site-chemical
avgCR_chem <- df %>% group_by(LOCATION, ChemClass, AQS_PARAMETER_NAME, SETTING, Latitude, Longitude) %>%
                           summarize("CRinAmil" = mean(CRinAmil))

### Calculate average 5 year cancer risk for each site-chemical class 
avgCR_class <- avgCR_chem %>% group_by(LOCATION, ChemClass, SETTING, Latitude, Longitude) %>%
                              summarize("CR5YrAvg" = sum(CRinAmil))

### Calculate average 5 year cancer risk for each site
avgCR_site <- avgCR_class %>% group_by(LOCATION, SETTING, Latitude, Longitude) %>%
                              summarize("totalCRinAmil" = sum(CR5YrAvg))

### Calculate % contribution to cancer risk for each site-chemical class
merged <- merge(avgCR_class, avgCR_site, by = c("LOCATION", "SETTING", "Latitude", "Longitude" ), all.x = TRUE)
merged$pcent_class_CR <- merged$CR5YrAvg/merged$totalCRinAmil

### Subset and reshape data
merged <- subset(merged, select = c("LOCATION", "ChemClass", "pcent_class_CR", "Latitude",
                                    "Longitude", "totalCRinAmil"))
merged <- reshape2::dcast(merged, LOCATION + Latitude + Longitude + totalCRinAmil ~ ChemClass,
                 value.var = "pcent_class_CR")

### Write CSV to be put into Python script for plotting
write.csv(merged, "../Data/Fig1_PieChartMapData.csv", row.names = FALSE)


