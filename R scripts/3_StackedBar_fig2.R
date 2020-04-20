library(dplyr); library(ggplot2)

#########################################################################
##  This script takes the filtered NATTS HAPs df and creates a stacked ##
##  bar plot of CR in-a-million vs NATTS location, with the top 10     ##
##  contributing pollutants to CR in-a-mil (based on 5 yr average)     ##
##  distinguished by color                                             ##
#########################################################################

## Read in filtered NATTS HAPs df 
df <- read.csv("../Data/FilteredNattsHaps.csv")    # Change path if necessary

### Calculate the five year average cancer risk for each site-chemical 
avgCR_chem <- df %>% group_by(LOCATION, SETTING, AQS_PARAMETER_NAME) %>%
                        dplyr::summarize("CRinAmilAvg" = mean(CRinAmil))

### Calculate five year average cancer risk for each NATTS 
avgCR_site <- avgCR_chem %>% group_by(LOCATION, SETTING) %>%
                         dplyr::summarize("AvgCR" = sum(CRinAmilAvg))

### Calculate the top 10 contributing chemicals to CR (based on 5 yr avg) for each site 
### and combine cancer risk from all other chemicals into an "other" group
top10chem  <- avgCR_chem %>% group_by(LOCATION) %>%
                             arrange(desc(CRinAmilAvg)) %>%
                             dplyr::mutate(AQS_PARAMETER_NAME = ifelse(1:n() > 10, "Other",
                                                              as.character(AQS_PARAMETER_NAME))) %>%
                             group_by(AQS_PARAMETER_NAME, LOCATION, SETTING) %>%
                             dplyr::summarize("CRinAmilAvg" = sum(CRinAmilAvg))

### Set up the order of sites for the stacked bar (so that they are presented in order of
### decreasing total CR, with urban and rural locations grouped)
avgCR_site <- aggregate(CRinAmilAvg ~ LOCATION, top10chem, sum)
top10chem$LOCATION <- factor(top10chem$LOCATION,
                             levels=avgCR_site[order(avgCR_site$CRinAmilAvg, decreasing = TRUE),
                             "LOCATION"])
top10chem$SETTING <- factor(top10chem$SETTING, levels = c("Urban", "Rural"))

### Create stacked bar plot 
p <- ggplot(top10chem, aes(x = interaction(LOCATION, SETTING), y = CRinAmilAvg, width = 0.8)) +
         geom_bar(stat = "identity", aes(fill = AQS_PARAMETER_NAME)) +
         theme(axis.text.x = element_text(angle = 90,
                                          vjust = 0.1,
                                          hjust = 1.0)) +
         scale_y_continuous(limits = c(0, 140), expand = c(0,0))

### Write to pdf 
pdf("../Figures/Fig2_stackedBar.pdf", paper = "USr", width = 11, height = 7)
print(p)
dev.off()

