library(rlist); library(dplyr); library(ggplot2)

########################################################################
# This script creates a line graph for each site that shows how cancer #
# risk has changed relative to 2013 cancer risk for that site, note    #
# only chemcials that were measured for all 5 years at a side were     #
# included                                                             #
########################################################################

### Read in data set: filtered NATTS
df <- read.csv("../Data/filteredNATTS2017v3.csv")   # Change path if necessary

###  Only pollutants measured all five years at a given site
complete <- df %>% group_by(LOCATION, AQS_PARAMETER_NAME) %>% filter(length(YEAR) > 4)
d1 <- as.data.frame(unique(df$LOCATION))
d2 <-  as.data.frame(unique(complete$LOCATION))
y1 <- complete %>% group_by(LOCATION, YEAR) %>% 
                   summarize("CRinAmil" = sum(CRinAmil))
y1 <- reshape2::dcast(y1, LOCATION ~ YEAR, value.var = "CRinAmil")

### Calculate standardized change in CR, using 2013 as baseline
y1$std13 <- 1
y1$std14 <- y1$`2014` / y1$`2013`
y1$std15 <- y1$`2015` / y1$`2013`
y1$std16 <- y1$`2016` / y1$`2013`
y1$std17 <- y1$`2017` / y1$`2013`

y2 <- reshape2::melt(y1, id= "LOCATION", measure.vars = c("std13", "std14", "std15",
                     "std16", "std17"), value.name = "CR")

### Group locations and label, for graphing so that there are ~5 lines per graph
y2 <- y2 %>% mutate("ID" = ifelse(LOCATION %in% c("Los Angeles, CA", "Washington, DC", "Phoenix, AZ", "Bountiful, UT"), 1,
                           ifelse(LOCATION %in% c("Grand Junction, CO", "Detroit, MI", "Rubidoux, CA", "St. Louis, MO"), 2,
                           ifelse(LOCATION %in% c("Pinellas County, FL", "Bronx, NY", "San Jose, CA", "Houston, TX"), 3,
                           ifelse(LOCATION %in% c("Roxbury, MA", "Tampa, FL", "Richmond, VA", "Chicago, IL"), 4,
                           ifelse(LOCATION %in% c("Chesterfield, SC", "Karnack, TX", "La Grande, OR", "Grayson Lake, KY", "Horicon, WI", "Underhill, VT"), 5,
                                  6 ))))))     
### Rename year variable to full year
y3 <- y2 %>% mutate("YEAR" = ifelse(variable == "std13", 2013,
                             ifelse(variable == "std14", 2014,
                             ifelse(variable == "std15", 2015,
                             ifelse(variable == "std16", 2016, 2017)))))

#### Create line graphs
p <- ggplot(y3, aes(x = YEAR, colour = LOCATION, group = LOCATION)) +
       labs(x = "YEAR", y = "Change in CR") +
       geom_point(aes(y = CR), size = 1.2) +
       geom_line(aes(y = CR), size = 0.8) +
       theme(axis.text = element_text(size = 16)) +
       scale_x_continuous(expand = c(0,0)) +
       facet_wrap(~ID)

### Write to pdf
pdf("../Figures/Fig4_timeSeries.pdf", paper = "USr", width = 11, height = 7)
p
dev.off()

### Data frame for graph
#write.csv(y1, "./Data/df_fig4.csv", row.names = F)             
