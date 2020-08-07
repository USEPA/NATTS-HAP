library(mice);library(reshape2);library(ggplot2); library(dplyr)

##################################################################################
##  This R script creates two correlation matricies (for urban and rural site)  ##
##  We are comparing how pollutants correlate with each other across locations  ##
##  We used 5 year average pollutant concentrations for each site, and only     ##
##  included pollutants that were measured at at least 75% of sites             ##
##################################################################################

## Read in necessary date file -- it should be the one created by previous script, which 
## contains filtered NATTS data 
NATTS <- read.csv("../Data/filteredNATTS2017v3.csv")  # change path if not set to source file location

data <- NATTS %>% group_by(LOCATION, SETTING, AQS_PARAMETER_NAME) %>%
                             dplyr::summarize("ParamAvg" = mean(meanRos_ug_m3))

urban <- subset(data, SETTING == "Urban")
rural <- subset(data, SETTING == "Rural")

###########################################################################################
#  This function creates the correlation matrix using the mice package for missing data   #
#  This function returns a list where the first object is the plot and the second is the  #
#  bootstrap analysis, which was not included in our final paper                          #
###########################################################################################
corr_mat <- function(df, pmaxNA = 25) {
  settings <- subset(df, select = c("LOCATION", "SETTING"))
  settings <- settings[!duplicated(settings$LOCATION),]
  format <- subset(df, select = c("LOCATION", "AQS_PARAMETER_NAME", "ParamAvg"))
  mat <- reshape2::dcast(format, LOCATION ~ AQS_PARAMETER_NAME, value.var = "ParamAvg")
  merged <- merge(settings, mat, by = "LOCATION")
  maxNA = pmaxNA/100
  data <- merged[, colSums(is.na(merged)) < (maxNA * nrow(merged))]
  rownames(data) <- data[,1]
  format <- data[,-1:-2]
  write.csv(format, "tempMatrix.csv")
  newForm <- read.csv("tempMatrix.csv", row.names = 1)
  if (file.exists("tempMatrix.csv")) file.remove("tempMatrix.csv")
  temp <- mice(newForm, meth = "pmm", threshold = 0.99999999999999999999999999999, remove_collinear = FALSE)
  imputed <- complete(temp, 1)
  imputed <- imputed[, colSums(imputed != 0) > 0]
  cormat <- round(cor(imputed), 2)
  dd <- as.dist((1-cormat) /2)
  hc <- hclust(dd, method = "ward.D2")
  cormat <- cormat[hc$order, hc$order]
  melted <- melt(cormat, na.rm = TRUE)
  fit <- pvclust::pvclust(cormat, method.dist = "correlation", method.hclust = "ward.D2", nboot = 10000)
  
  p <- ggplot(data = melted, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name = "Pearson/nCorrelation") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    coord_fixed()
  list <- list(p, fit)
  return(list)
}   #return a correlation plot and bootstrapping analysis

urbanMat <- corr_mat(urban, 25)
pdf("../Figures/Fig6_UrbanCorrMat.pdf", paper = "USr", width = 11, height = 7)
print(urbanMat[[1]])  # Print correlation matrix plot
plot(urbanMat[[2]])   # Print bootstrap analysis
pvclust::pvrect(urbanMat[[2]], alpha=.95)
dev.off()

ruralMat <- corr_mat(rural, 25)
pdf("../Figures/Fig6_RuralCorrMat.pdf", paper = "USr", width = 11, height = 7)
print(ruralMat[[1]])
plot(ruralMat[[2]])
pvclust::pvrect(ruralMat[[2]], alpha=.95)
dev.off()
