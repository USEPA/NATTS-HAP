library(dplyr)

###################################################################################
##  ***Before running this script the annual average statistics xlsx must be     ##
##     downloaded from https://www3.epa.gov/ttnamti1/toxdat.html#data (bottom of ##
##     page) and then the annual_phase13 tab must be saved as a csv.***          ##
##  ---------------------------------------------------------------------------- ##
##  This R script takes in the annual HAP monitored concentrations data set and  ##
##  first matches the AMA_SITE_CODE and AQS_PARAMATER_NAME to dataframes         ##
##  so that only NATTS sites and chemicals with URE values are kept, we then     ##
##  performed other filtering methods as described below.                        ##
##                                                                               ##
##  Assuming the directory structure has been maintained (i.e. a directory that  ##
##  contains the folders R scripts, Data, and Figures, you can run all these     ##
##  scripts from the source file location, from R studio:                        ##
##  (Session -> Set Working Directory -> Source File Location)                   ##
###################################################################################

### Read in datasets (haps df, chemicals with unit risk estimates df, and natts site codes df)
df <- read.csv("../Data/annual_phase.csv")    # change path if necessary
URE <- read.csv("../Data/UREandChemGroup.csv")
codes <- read.csv("../Data/NATTScodes.csv")

### Select only columns we want to look at to make df easier to work with
data <- subset(df, select = c("STATE_ABBR", "AMA_SITE_CODE", "YEAR", "AQS_PARAMETER_NAME", "Percent_ND", "meanRos_ug_m3",
                              "DURATION_DESC", "meanratio", "MONITOR_LATITUDE", "MONITOR_LONGITUDE"))

data$AMA_SITE_CODE <- as.numeric(as.character(data$AMA_SITE_CODE))

### Keep only NATTS (identified by AMA_SITE_CODE), remove data from before 2012, 
### and add URE values to applicable chemicals
natts <- data %>% filter(AMA_SITE_CODE %in% codes$AMA_SITE_CODE & YEAR > 2012) %>%
                         merge(codes, by = c("STATE_ABBR", "AMA_SITE_CODE"), all.x = TRUE) %>%
                         merge(URE, by = "AQS_PARAMETER_NAME", all.x = TRUE) #add UREs

### Remove rows without a URE
natts <- natts %>% subset(!(is.na(URE)) & URE != 0)

### If more than 80 of data for the year was non-detect, set annual mean to 0 and mean ratio to 1
natts$meanRos_ug_m3 <- ifelse(natts$Percent_ND > 80, 0, natts$meanRos_ug_m3)
natts$meanratio <- ifelse(natts$Percent_ND > 80, 1, natts$meanratio)

### Remove data where meanratio was greater than 1.3 (ND determined to have large effect on annual mean)
natts <- natts %>% subset(meanratio <= 1.3)

### For chemicals that were measured at multiple time intervals only keep 24 hour interval,
natts$DURATION_DESC <- factor(natts$DURATION_DESC, levels = c("24 HOURS", "1 HOUR"))
natts <- natts[order(natts$DURATION_DESC),]
natts <- natts[!duplicated(natts[,c('YEAR', 'LOCATION', "AQS_PARAMETER_NAME")]),]

### Calculate cancer risk and cancer risk in a million columns (CR = annual mean * URE)
natts$CancerRisk <- (as.numeric(as.character(natts$URE)) * as.numeric(as.character(natts$meanRos_ug_m3)))
natts$CRinAmil <- natts$CancerRisk* 1000000    #add CR and CRinAmil

### For NATTS where lat/lon values different slightly, take the average to simplify map plotting
natts <- natts %>% group_by(LOCATION) %>%
                   dplyr::summarize("Latitude" = mean(MONITOR_LATITUDE),
                                    "Longitude" = mean(MONITOR_LONGITUDE)) %>%
                   merge(natts, by = "LOCATION", all = TRUE) %>%
                   subset(select = -c(MONITOR_LATITUDE, MONITOR_LONGITUDE))

### Write filtered data to new csv
write.csv(natts, "../Data/filteredNATTS2017v3.csv", row.names = FALSE)
