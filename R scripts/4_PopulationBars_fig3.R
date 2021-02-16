library(reshape2); library(dplyr); library(ggplot2)

##############################################################################
##  This R script used the filtered NATTS data and block level census       ##
##  population data to create graphs that show how the total population     ##
##  varies by cancer risk and how the population varied by CR at each site  ## 
##############################################################################

### Read in data sets: filtered NATTS, and population values around monitors
df <- read.csv("../Data/FilteredNattsHaps.csv")                    # Change path if necessary
pop <- read.csv("../Data/CB_within_NATTS_blockpops.csv") %>%
                subset(select = c(LOCATION, win0.25mi, win1mi, win0.5mi))

### 5 year average CR at NATTS
avgCR_site <- df %>% group_by(LOCATION, AQS_PARAMETER_NAME, SETTING) %>%
                     dplyr::summarize("mean" = mean(CRinAmil)) %>%
                     group_by(LOCATION, SETTING) %>%
                     dplyr::summarize("CancerRisk" = sum(mean)) %>%
                     subset(select = c("LOCATION", "CancerRisk", "SETTING"))

### Match population to sites to get breakdown of population and cancer risk
popwCR <- merge(pop, avgCR_site, by = c("LOCATION"), all = T)

###############################################################################
##  Data and graph for figure 3b, a histogram of population                  ##
##  (w/in 0.25, 0.5, and 1 mile of each site) vs cancer risk (binned by 25)  ##        
###############################################################################
popwCR2 <- popwCR
popwCR2$win1mi <- popwCR$win1mi - popwCR$win0.5mi  # fix population values so they can be stacked
popwCR2$win0.5mi <- popwCR$win0.5mi - popwCR$win0.25mi
popwCR2 <- melt(popwCR2, id.vars = c("SETTING", "LOCATION", "CancerRisk"), 
                measure.vars = c("win0.25mi", "win1mi", "win0.5mi"))

fig3b <- popwCR2 %>% mutate(CR_ID = ifelse(CancerRisk >= 1 & CancerRisk < 25, "x1.25",
                                  ifelse(CancerRisk >= 25 & CancerRisk < 50, "x25.50",
                                  ifelse(CancerRisk >= 50 & CancerRisk < 75, "x50.75",
                                  ifelse(CancerRisk >= 75 & CancerRisk < 100, "x75.100",
                                  "100up")))))

fig3b <- fig3b %>% group_by(variable, CR_ID) %>%
                   dplyr::summarize("pop" = sum(value))

fig3b$CR_ID <- factor(fig3b$CR_ID, levels = c("x1.25", "x25.50", "x50.75", "x75.100", "100up"))
fig3b$variable <- factor(fig3b$variable, levels = c("win1mi", "win0.5mi", "win0.25mi"))

##############################################################################################
#  Layered bar plot showing populations in different CR ranges for different distances from  # 
#  monitor, win0.25mi is graphed but hidden behind other elements, brought to front in       #
#  illustrator                                                   #
##############################################################################################
p3b <- ggplot(fig3b, aes(x = CR_ID, y = pop, fill = variable)) +
           geom_bar(stat = "identity", position = "stack") +
           theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .25))

####################################################################################
##  Data and graph for figure 3a, layered/double y-axis bar graph that shows      ##
##  population at each site (w/in 0.25, 0.5, 1 mi) and average 5 year CR vs site  ##
##  (same issue with 0.25mi being hidden behind other elements)                   ##
####################################################################################
popwCR2$LOCATION <- factor(popwCR2$LOCATION,
                           levels=popwCR[order(popwCR$CancerRisk, decreasing = TRUE),
                                       "LOCATION"])
popwCR2$SETTING <- factor(popwCR2$SETTING, levels = c("Urban", "Rural"))

p3a <- ggplot(popwCR2, aes(width = 0.5)) +
           geom_bar(aes(x = interaction(LOCATION, SETTING), y = value, fill = variable),
                    stat = "identity", position = "stack") +
           geom_bar(aes(x = interaction(LOCATION, SETTING), y = CancerRisk* 1500),
                    stat = "identity", position = "identity", alpha = .1) +
           theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .25)) +
           scale_y_continuous(name = "Cummulative population",
                              limits = c(0, 1.6e5),
                              sec.axis = sec_axis(~., name = "5 year CR avg",
                                                  breaks = c(0, 0.75e5, 1.5e5),
                                                  labels = c("0", "50", "100")))

pdf("../Figures/Fig3_populationBar.pdf", paper = "USr", width = 11, height = 7)
print(p3a)
print(p3b)
dev.off()

### Dataframes for the figures
#write.csv(popwCR2, "./Data/df_fig3a.csv")
#write.csv(fig3b, "./Data/df_fig3bv2.csv")
