library(dplyr)

#############################################################################
#  ***Before running this script the PM mass files need to be downloaded    #
#   from https://aqs.epa.gov/aqsweb/airdata/download_files.html#Daily       #
#                                                                           #
#   Download PM2.5 Speciation and PM10 Speciation files for 2013-2017 and   #    
#   unzip the csvs to data folder.***    Alternatively I have included the  #
#   filteredNATTSwPMmass.csv that this file generates under Optional Files. #
#   If you wish to skip this script, add filteredNATTSwPMmass.csv to        #
#   NATTS-HAP/Data and do not run this script.                              #
# ------------------------------------------------------------------------  #
#  This R script used the PM10 and PM2.5 mass files from  the AQS website,  #
#  this data is provided as daily data so in this script we create annual   # 
#  averaged of PM10 and PM2.5 mass for each NATTS for the years 2013-2017.  #
#  We use PM10 and PM2.5 mass for the correlation matrix figure so the      #
#  output datafile of this script must be created before running the        # 
#  correlation matrix R script                                              #
#############################################################################

##  Read in all necessary files, this takes a while since the files are large
NATTScodes <- read.csv("../Data/NATTScodes.csv")           # Change paths if necessary
natts <- read.csv("../Data/FilteredNattsHaps.csv")

PM10_2017 <- read.csv("../Data/daily_81102_2017.csv")
PM25_2017 <- read.csv("../Data/daily_88101_2017.csv")

PM10_2016 <- read.csv("../Data/daily_81102_2016.csv")
PM25_2016 <- read.csv("../Data/daily_88101_2016.csv")

PM10_2015 <- read.csv("../Data/daily_81102_2015.csv")
PM25_2015 <- read.csv("../Data/daily_88101_2015.csv")

PM10_2014 <- read.csv("../Data/daily_81102_2014.csv")
PM25_2014 <- read.csv("../Data/daily_88101_2014.csv")

PM10_2013 <- read.csv("../Data/daily_81102_2013.csv")
PM25_2013 <- read.csv("../Data/daily_88101_2013.csv")

##  Function to calculate annual average of PM10 and PM2.5 mass at each NATTS for specific year
pm_mass <- function(PM10, PM25, year) {
  PM10NATTS <- merge(NATTScodes, PM10, by = c("State.Code", "County.Code", "Site.Num"), all.x = TRUE)
  PM10NATTS <- subset(PM10NATTS, AMA_SITE_CODE != "NA")

  PM25NATTS <- merge(NATTScodes, PM25, by = c("State.Code", "County.Code", "Site.Num"), all.x = TRUE)
  PM25NATTS <- subset(PM25NATTS, AMA_SITE_CODE != "NA")

  pm10_summ <- PM10NATTS %>%
    group_by(LOCATION, SETTING) %>%
    dplyr::summarize("meanRos_ug_m3" = mean(Arithmetic.Mean, na.rm = TRUE),
            "AQS_PARAMETER_NAME" = "PM10mass")

  pm25_summ <- PM25NATTS %>%
    group_by(LOCATION, SETTING) %>%
    dplyr::summarize("meanRos_ug_m3" = mean(Arithmetic.Mean, na.rm = TRUE),
              "AQS_PARAMETER_NAME" = "PM25mass")

  merged <- rbind.data.frame(pm10_summ, pm25_summ)
  merged$YEAR <- year
  return(merged)
}

##  Get dataframes of annual PM mass concentrations for each NATTS
pm2017 <- pm_mass(PM10_2017, PM25_2017, 2017)
pm2016 <- pm_mass(PM10_2016, PM25_2016, 2016)
pm2015 <- pm_mass(PM10_2015, PM25_2015, 2015)
pm2014 <- pm_mass(PM10_2014, PM25_2014, 2014)
pm2013 <- pm_mass(PM10_2013, PM25_2013, 2013)

##  Merge PM mass data with filtered NATTS data to be used in correlation matrix figure 
d2013up <- subset(natts, select = c("LOCATION", "SETTING", "AQS_PARAMETER_NAME", "meanRos_ug_m3", "YEAR"))
ALLtheDATA <- rbind.data.frame(pm2017, pm2016, pm2015, pm2014, pm2013, d2013up)

##  Save and write csv
write.csv(ALLtheDATA, "../Data/filteredNATTSwPMmass.csv", row.names = FALSE)



