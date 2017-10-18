library(ggplot2)
library(plyr)

source("util.R")
plotPath = "plot/"
oracleData = read.csv("../data/oracle-info.csv", sep = ';', dec= ",", header = T);
# names(oracleData) = c("system","username","name","email","file","adds","mods","acc","sent","obs","answer","filtered","filter_info")


commercialData = subset(oracleData, systemsGroup == "Commercial")
ossData = subset(oracleData, systemsGroup == "OSS")


### Distribution of Developer Answers and Percentage of Declared Maintainers/Non-Maintainers ####

system = c(rep("Commercial Systems", 7), rep("Open-source Systems", 7))
answer = factor(c(1,2,3,4,5,"non-maintainer","maintainer",1,2,3,4,5,"non-maintainer","maintainer"))
number = c(length(commercialData$dev_answer[commercialData$dev_answer==1]),
           length(commercialData$dev_answer[commercialData$dev_answer==2]),
           length(commercialData$dev_answer[commercialData$dev_answer==3]),
           length(commercialData$dev_answer[commercialData$dev_answer==4]),
           length(commercialData$dev_answer[commercialData$dev_answer==5]),
           length(commercialData$dev_answer[commercialData$dev_answer==1])+length(commercialData$dev_answer[commercialData$dev_answer==2])+length(commercialData$dev_answer[commercialData$dev_answer==3]),
           length(commercialData$dev_answer[commercialData$dev_answer==4])+length(commercialData$dev_answer[commercialData$dev_answer==5]),
           length(ossData$dev_answer[ossData$dev_answer==1]),
           length(ossData$dev_answer[ossData$dev_answer==2]),
           length(ossData$dev_answer[ossData$dev_answer==3]),
           length(ossData$dev_answer[ossData$dev_answer==4]),
           length(ossData$dev_answer[ossData$dev_answer==5]),
           length(ossData$dev_answer[ossData$dev_answer==1])+length(ossData$dev_answer[ossData$dev_answer==2])+length(ossData$dev_answer[ossData$dev_answer==3]),
           length(ossData$dev_answer[ossData$dev_answer==4])+length(ossData$dev_answer[ossData$dev_answer==5])
           )
percentage = c(number[1]/nrow(commercialData),
               number[2]/nrow(commercialData),
               number[3]/nrow(commercialData),
               number[4]/nrow(commercialData),
               number[5]/nrow(commercialData),
               number[6]/nrow(commercialData),
               number[7]/nrow(commercialData),
               number[8]/nrow(ossData),
               number[9]/nrow(ossData),
               number[10]/nrow(ossData),
               number[11]/nrow(ossData),
               number[12]/nrow(ossData),
               number[13]/nrow(ossData),
               number[14]/nrow(ossData)
               )
plotOracleDataNew = data.frame(system, answer, number, percentage)
plotOracleDataNew$group = c("Answers","Answers","Answers","Answers","Answers",
                            "Category","Category",
                            "Answers","Answers","Answers","Answers","Answers",
                            "Category","Category")

ggplot(plotOracleDataNew, aes(x=answer, y=percentage*100, fill=system)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Paired") +
  facet_grid(.~group, scales="free", space="free", shrink=TRUE) +
  xlab("") + ylab("Percentage (%)") +
  theme(legend.position = "top", legend.title=element_blank())+
  theme(legend.text=element_text(size=10))+
  theme(axis.text.x = element_text(size = 10)) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(strip.text.x  = element_text(size = 10)) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60))

### Wilcoxon-Mann-Whitney test
aswData = data.frame(count(commercialData$dev_answer)$x, count(commercialData$dev_answer)$freq, count(ossData$dev_answer)$freq )
names(aswData) = c("answer", "commercial", "openSource")
wilcox.test(aswData$commercial, aswData$openSource)

########################################################################################

############## GRAPHS: Harmonic Mean (HM), Hit Ratios (HR_M and HR_NM) #################

sourceData = read.csv("../data/basic_thresholds-step_0.1.csv", sep = ';', dec= ",", header = T);
sourceData$threshold = as.factor(sourceData$threshold)


comparativeData = subset(sourceData, system =="All Commercial"|
                                     system == "All OSS")
comparativeData$system = factor(as.character(comparativeData$system),
              level = c("All Commercial", "All OSS"))

authorHitData = c()
system = factor(c(as.character(comparativeData$system),
                     as.character(comparativeData$system),
                     as.character(comparativeData$system)),
                c("All Commercial", "All OSS"))
threshold = as.factor(c(as.character(comparativeData$threshold),
                        as.character(comparativeData$threshold),
                        as.character(comparativeData$threshold)))
times = nrow(comparativeData)
technique = factor(c(rep("DOA", times), rep("Commit", times), rep("Blame", times)) , 
                   c("DOA", "Commit", "Blame"))
value = c(comparativeData$doaAuthorHit/comparativeData$nAuthorsHit, 
          comparativeData$commitAuthorHit/comparativeData$nAuthorsHit, 
          comparativeData$blameAuthorHit/comparativeData$nAuthorsHit)

authorHitData = data.frame(system, threshold, technique, value)

notAuthorHitData = c()
system = factor(c(as.character(comparativeData$system),
                  as.character(comparativeData$system),
                  as.character(comparativeData$system)),
                c("All Commercial", "All OSS"))
threshold = as.factor(c(as.character(comparativeData$threshold),
                        as.character(comparativeData$threshold),
                        as.character(comparativeData$threshold)))
times = nrow(comparativeData)
technique = factor(c(rep("DOA", times), rep("Commit", times), rep("Blame", times)),
                   c("DOA", "Commit", "Blame"))
value = c(comparativeData$doaNotAuthorHit/comparativeData$nNotAuthorsHit, 
          comparativeData$commitNotAuthorHit/comparativeData$nNotAuthorsHit, 
          comparativeData$blameNotAuthorHit/comparativeData$nNotAuthorsHit)

notAuthorHitData = data.frame(system, threshold, technique, value)

allHitData = c()
system = factor(c(as.character(comparativeData$system),
                  as.character(comparativeData$system),
                  as.character(comparativeData$system)),
                c("All Commercial", "All OSS"))
threshold = as.factor(c(as.character(comparativeData$threshold),
                        as.character(comparativeData$threshold),
                        as.character(comparativeData$threshold)))
times = nrow(comparativeData)
technique = factor(c(rep("DOA", times), rep("Commit", times), rep("Blame", times)),
                   c("DOA", "Commit", "Blame"))
value = c(comparativeData$allHitsDoa/comparativeData$allHits, 
          comparativeData$allHitsCommits/comparativeData$allHits, 
          comparativeData$allHitsBlame/comparativeData$allHits)

allHitData = data.frame(system, threshold, technique, value)

hmData  = c()
system = factor(c(as.character(comparativeData$system),
                  as.character(comparativeData$system),
                  as.character(comparativeData$system)),
                c("Commercial #1", "Commercial #2", "All Commercial", "All OSS"))
threshold = as.factor(c(as.character(comparativeData$threshold),
                        as.character(comparativeData$threshold),
                        as.character(comparativeData$threshold)))
times = nrow(comparativeData)
technique = factor(c(rep("DOA", times), rep("Commit", times), rep("Blame", times)),
                   c("DOA", "Commit", "Blame"))
value = 2 * authorHitData$value * notAuthorHitData$value/(authorHitData$value + notAuthorHitData$value)

hmData = data.frame(system, threshold, technique, value)



metric =factor(c(rep("Maintainers", nrow(authorHitData)), 
          rep("Non-maintainers", nrow(authorHitData)), 
          rep("All", nrow(authorHitData)), 
          rep("Harmonic mean", nrow(authorHitData))),
          level = c("Maintainers", "Non-maintainers", "All", "Harmonic mean"))
newData = rbind(subset(authorHitData, system == "All Commercial" | system == "All OSS"),
                subset(notAuthorHitData, system == "All Commercial" | system == "All OSS"),
                subset(allHitData, system == "All Commercial" | system == "All OSS"),
                subset(hmData, system == "All Commercial" | system == "All OSS"))
newData$metric = metric

newData$system = as.character(newData$system)
newData$system[newData$system=="All Commercial"] = "Commercial Systems"
newData$system[newData$system=="All OSS"] = "Open-source Systems"
newData$system = as.factor(newData$system)

newData = subset(newData, metric != "All")

layout=metric~system


### Harmonic Mean of HR_M and HR_NM ##
ggplot(subset(newData, metric == "Harmonic mean"), aes(threshold, value*100, group = technique, colour = technique)) +
  geom_line() +
  facet_grid(.~system, scales="fixed", shrink=TRUE) +
  geom_point(aes(shape=technique, color = technique)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10)) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(strip.text.x  = element_text(size = 10)) +
  scale_color_discrete(name = "Technique") +
  xlab("Threshold (k)") + ylab("HM (%)") +
  scale_shape_manual(values=1:10, name = "Technique") +
  scale_y_continuous(limits = c(0, 100))

### Hit Ratio of Maintainers (HR_M) and Non-maintainers (HR_NM), and Harmonic Mean (HM) ###
ggplot(newData, aes(threshold, value*100, group = technique, colour = technique)) +
  geom_line() +
  geom_point(aes(shape=technique, color = technique)) +
  facet_grid(layout, scales="fixed", shrink=TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(strip.text.y  = element_text(size = 8)) +
  theme(axis.title.x = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 10)) +
  scale_color_discrete(name = "Technique") +
  xlab("Threshold (k)") + ylab("Hit ratio (%)") +
  scale_shape_manual(values=1:10, name = "Technique") +
  scale_y_continuous(limits = c(0, 100))
