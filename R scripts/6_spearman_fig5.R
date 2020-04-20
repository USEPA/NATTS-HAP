library(dplyr); library(ggplot2); library(tidyverse); library(ggpubr); library(plyr)

#############################################################################
##  This R script uses the filtered NATTS data to perform spearman rank    ##
##  correlations for site-chemical combinations, the chemicals must have   ##
##  measured for at least 2 years for any given site. The resulting table  ##
##  shows which chemicals have increased/decreased at each site, and their ##
##  statistical significance. The printed table is not pretty and needs    ##
##  to be edited with illustrator, however the results can be printed as   ##
##  a dataframe and matched to paper figure.                               ##
#############################################################################

## Read in necessary file, filtered NATTS HAP data
df <- read.csv("../Data/FilteredNattsHaps.csv")   # Change path if necessary

## Keep only chemicals that had been measured for at least two years for each site
data <- df %>% group_by(LOCATION, AQS_PARAMETER_NAME) %>% dplyr::filter(n() >= 2)
data$meanRos_ug_m3 <- as.numeric(as.character(data$meanRos_ug_m3))

#### SPEARMAN RANK CORRELATION CALCULATIONS ####
corfun <- function(x,y) {
  corr = (cor.test(x, y, method = "spearman"))
}

res <- ddply(data, .(LOCATION, AQS_PARAMETER_NAME, SETTING, ChemClass), summarise,
             pVal = corfun(YEAR, meanRos_ug_m3)$p.value,
             RHO  = corfun(YEAR, meanRos_ug_m3)$estimate,
             numYears = length(SETTING))

#### SPEARMAN FIGURE CODE ####
#  For each site-chemical combination, the color of the cell is determined by rho and the pval.    #
#  Rho indicates if the concentration has increased or decreased over time (rho>0 means increase,  #
#  rho<0 means decrease). Pval indicates statistical significance, we used 0.1 and 0.5 as cut offs # 

df <- res %>% 
      dplyr::mutate(colour = ifelse(pVal > 0.1, "wht",
                             ifelse(pVal < 0.1 & pVal > 0.05 & RHO > 0, "ltrd",
                             ifelse(pVal < 0.1 & pVal > 0.05 & RHO < 0, "ltgr",
                             ifelse(pVal < 0.05 & RHO > 0, "rd",
                             ifelse(pVal < 0.05 & RHO < 0, "gr", "NA"))))))

df2 <- na.omit(df)

## We only kept chemicals that had been measured at at least 8 sites, to simplify the figure
df2 <- df2 %>% group_by(AQS_PARAMETER_NAME) %>% dplyr::filter(n() >= 8)
df2$RHO <- round(df2$RHO, digits = 1)        

## Create table figure
cols <- c("wht" = '#FDFEFE', "ltrd" = '#F1948A', "ltgr" = '#ABEBC6', "rd" = '#C0392B',
          "gr" = '#28B463',"NA" = '#F9E79F')
    
p <-  ggplot(df2, aes(x = interaction(AQS_PARAMETER_NAME, ChemClass), y = interaction(LOCATION, SETTING))) +
      geom_tile(aes(fill = colour)) +
      geom_text(aes(label = RHO, size = 0.5)) +
      scale_fill_manual(values = cols, na.value = "white") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            panel.grid.major = element_line(color = "black"), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = "black", fill=NA, size=2))

## Save to pdf
pdf("../Figures/Fig5_spearmanFig.pdf",  paper = "USr", width = 11, height = 7)  
print(p)
dev.off()
