library(data.table)
library(ggplot2)
library(plyr)
options(scipen=100000)
cWidth=6.6
cHeight=2.8 

source("util.R")
plotPath = "plot/"

### Add more decimal in metrics value
oracleData = read.csv("../data/additional_analysis.csv", sep = ';', dec= ".", header = T);

oracleData$id = 1:nrow(oracleData)
oracleData$allLines = oracleData$LoCode + oracleData$LoComm
oracleData$commitsPerc =  oracleData$devCommits/oracleData$nCommits*100
oracleData$churnPerc =  oracleData$devChurn/oracleData$fileChurn*100
# classification = ifelse(oracleData$answer <=3, "non-maintainer", "maintainer")
oracleData$classification = ifelse(oracleData$answer <=3, "non-maintainer", "maintainer")
oracleData$doa = ifelse(oracleData$doa == TRUE, "miss", "hit" )
oracleData$commit = ifelse(oracleData$commit == TRUE, "miss", "hit" )
oracleData$blame = ifelse(oracleData$blame == TRUE, "miss", "hit" )
oracleData$newApproach = ifelse(oracleData$newApproach == TRUE, "miss", "hit" )

commercialData = subset(oracleData, systemsGroup == "Commercial")
ossData = subset(oracleData, systemsGroup == "OSS")

##########$ PAPER PLOTS ##############
maintainersData = subset(oracleData, classification == "maintainer")
nonMaintainersData = subset(oracleData, classification == "non-maintainer")



newHitData = getHitData(maintainersData, c('commitsPerc'), c("Developers Commits"))


# NEW (Review 2)

## maintainers_commits-two_violins.pdf
drawSimpleViolinPlot(newHitData, "", "Developer Commits (%)")


## control_violin_graph-color-no_legend.pdf
drawViolinPlot(oracleData, ylab1="Days", ylab2="LOC", width=0.06, colored = T, leg = F)


# OLD

p1 = drawSimpleSplitedViolinPlot(newHitData, "Developer Commits (%)", logInfo = "", T , F, F)

p7 = plotDeclaredViolinPlot("", oracleData, c("daysLC", "LoCode"), T, xlab="Dimension", ylab="Days (Recency) / LOC (Size)", h=0.3)



# pdf(file = paste(plotPath, "maintainers_commits-H.pdf",sep = ""), width=cWidth, cHeight)
p4 = plotHitsViolin("", maintainersData, c('commitsPerc'), F, T, newnames = c("Developers Commits"), 
                    xlab="Developer Commits (%)", horizontal = T)
# dev.off()

# pdf(file = paste(plotPath, "control_bean_graph-n.pdf",sep = ""), width=cWidth, cHeight+1)
p7 = plotDeclaredBeanPlot("", oracleData, c("daysLC", "LoCode"), T, xlab="Dimension", ylab="Value (log)",
                        newnames = c("Recency (days)", "File Size (LOCs)"), mconfig=T)
# dev.off()



f = function(v){
  return(paste(round(v*100,0), "%", sep = ""))
}
printResults = function(title, data, methods=c("doa", "commit", "blame")) {
  MaintainerData = subset(data, classification == "maintainer")
  NonMaintainerData = subset(data, classification == "non-maintainer")
  cat("\n**********", title, "**********", "\n")
  cat("method", paste("HR_M (", nrow(MaintainerData), ")" , sep = ""), paste("HR_NM (", nrow(NonMaintainerData), ")" , sep = ""), "      HM","\n", sep = "\t")
  for (method in methods){
    newData = MaintainerData[method]
    hitsM = length(newData[newData=="hit"])
    missesM = length(newData[newData=="miss"])
    HR_M = hitsM/(hitsM+missesM)
    
    
    newData = NonMaintainerData[method]
    hitsNM = length(newData[newData=="hit"])
    missesNM = length(newData[newData=="miss"])
    HR_NM = hitsNM/(hitsNM+missesNM)
    
    
    cat(method, f(HR_M), f(HR_NM), f(2*HR_M*HR_NM/(HR_M + HR_NM)),"\n", sep = "\t     ")
  }
  # cat("Maintainers: ", nrow(okMaintainerData), nrow(failMaintainerData), nrow(okMaintainerData)/(nrow(okMaintainerData)+nrow(failMaintainerData)), "\n")
  # cat("Non-Maintainers: ", nrow(okNonMaintainerData), nrow(failNonMaintainerData), nrow(okNonMaintainerData)/(nrow(okNonMaintainerData)+nrow(failNonMaintainerData)))
}


