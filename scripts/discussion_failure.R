library(data.table)
library(ggplot2)
library(plyr)
options(scipen=100000)

source("util.R")

oracleData = read.csv("../data/oracle-info.csv", sep = ';', dec= ",", header = T);

oracleData$id = 1:nrow(oracleData)
oracleData$classification = ifelse(oracleData$dev_answer <=3, "non-maintainer", "maintainer")

commercialData = subset(oracleData, systemsGroup == "Commercial")
ossData = subset(oracleData, systemsGroup == "OSS")



############# FAILURE ANALYSIS ###############

errorData = subset(oracleData, doa == "miss" | commit == "miss" | blame == "miss")
maintainersError = subset(errorData, classification == "maintainer")
allTechError = subset(maintainersError, doa == "miss" & commit == "miss" & blame == "miss")
######## RECENCY ########
allTechError$recency30days = ifelse(allTechError$daysLastCommit <=30, TRUE, FALSE )
cat("Recency 30 days: ", length(allTechError$recency30days[allTechError$recency30days]), "\n")
allTechError$recencyLastDev = ifelse(allTechError$nComAfter == 0 & allTechError$daysLastCommit >30, T, F)
cat("Recency Last dev: ", length(allTechError$recencyLastDev[allTechError$recencyLastDev]), "\n")
######## SMALL FILE ########
q = quantile(oracleData$LOC)
allTechError$small = ifelse(allTechError$LOC <=q["25%"], TRUE, FALSE)
cat("Recency Last dev: ", length(allTechError$small[allTechError$small]), "\n")



maintainersData = subset(oracleData, classification == "maintainer")
nonMaintainersData = subset(oracleData, classification == "non-maintainer")

######## Developers Commits Percentage ########
p1 = plotHitsViolin("", nonMaintainersData, c('commitsPerc'), F, TRUE, newnames = c("Developers Commits"), 
                    xlab="Developer Commits (%)", horizontal = T)


######## Distribution of declared (non-)maintainers. Dimension: recency and file size ########
p2 = plotMissViolinPlot("", oracleData, c("daysLastCommit", "LOC"), T, xlab="Dimension", ylab="Value (log)",
                        newnames = c("Recency (days)", "File Size (LOCs)"), mconfig=T)




