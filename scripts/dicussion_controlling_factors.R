### Reference Values for recency and file size in each system ###
# repository,shortTime,smallFile,longTime,largeFile
# Commercial#1,32.0,19.0,124.0,92.75
# Commercial#2,252.0,60.25,1174.0,265.75
# puppet,636.0,16.0,1922.0,78.0
# moment/moment,179.0,22.0,539.0,220.25
# monolog,380.75,103.5,1067.25,273.0
# salt,366.25,33.0,961.5,182.5
# django,404.75,31.0,1154.25,232.25
# Faker,281.0,25.75,853.0,120.75
# ember.js,74.0,24.25,928.0,240.75
# fog,210.0,29.0,1023.0,59.0

library(data.table)
library(ggplot2)

oracleData = read.csv("../data/oracle-info.csv", sep = ';', dec= ",", header = T);
names(oracleData) = c("id","systemsGroup","system","file","devId","dev_answer","doa","commit","blame","doaV","CommitV","blameV","isFA", "DE", "AC","nCommits","LoCode","daysLC","nComAfter", "commitsPerc")
oracleData$classification = ifelse(oracleData$dev_answer <=3, "non-maintainer", "maintainer")
source("utilFactors.R")



# sink("log-factor.txt")

repository=c()
doa=c()
commit=c()
blame=c()
newDoa=c()
newCommit=c()
newBlame=c()



print("Computing Fog results ...")
data = subset(oracleData, system == "fog")
data$filtered = F
repository=c(repository,"Fog")
doa = c(doa, getHM(data, "doa"))
commit = c(commit, getHM(data, "commit"))
blame = c(blame, getHM(data, "blame"))
resFog = computeResults4Vars(data,210.0,29.0,1023.0,59.0, 0.05)
newDoa = c(newDoa,max(resFog$doa))
newCommit = c(newCommit, max(resFog$commit))
newBlame = c(newBlame, max(resFog$blame))

print("Computing Commercial#1 results ...")
data = subset(oracleData, system == "commercial#1")
data$filtered = F
repository=c(repository,"Commercial#1")
doa = c(doa, getHM(data, "doa"))
commit = c(commit, getHM(data, "commit"))
blame = c(blame, getHM(data, "blame"))
resComm1 = computeResults4Vars(data, 32.0,19.0,124.0,92.75, 0.05)
newDoa = c(newDoa,max(resComm1$doa))
newCommit = c(newCommit, max(resComm1$commit))
newBlame = c(newBlame, max(resComm1$blame))

print("Computing Commercial#2 results ...")
data = subset(oracleData, system == "commercial#2")
data$filtered = F
repository=c(repository,"Commercial#2")
doa = c(doa, getHM(data, "doa"))
commit = c(commit, getHM(data, "commit"))
blame = c(blame, getHM(data, "blame"))
resComm2 = computeResults4Vars(data,252.0,60.25,1174.0,265.75, 0.05)
newDoa = c(newDoa,max(resComm2$doa))
newCommit = c(newCommit, max(resComm2$commit))
newBlame = c(newBlame, max(resComm2$blame))

print("Computing Puppet results ...")
data = subset(oracleData, system == "puppet")
data$filtered = F
repository=c(repository,"Puppet")
doa = c(doa, getHM(data, "doa"))
commit = c(commit, getHM(data, "commit"))
blame = c(blame, getHM(data, "blame"))
resPuppet = computeResults4Vars(data,636.0,16.0,1922.0,78.0, 0.05)
newDoa = c(newDoa,max(resPuppet$doa))
newCommit = c(newCommit, max(resPuppet$commit))
newBlame = c(newBlame, max(resPuppet$blame))

print("Computing Moment results ...")
data = subset(oracleData, system == "moment/moment")
data$filtered = F
repository=c(repository,"Moment")
doa = c(doa, getHM(data, "doa"))
commit = c(commit, getHM(data, "commit"))
blame = c(blame, getHM(data, "blame"))
resMoment = computeResults4Vars(data,179.0,22.0,539.0,220.25, 0.05)
newDoa = c(newDoa,max(resMoment$doa))
newCommit = c(newCommit, max(resMoment$commit))
newBlame = c(newBlame, max(resMoment$blame))

print("Computing Monolog results ...")
data = subset(oracleData, system == "monolog")
data$filtered = F
repository=c(repository,"Monolog")
doa = c(doa, getHM(data, "doa"))
commit = c(commit, getHM(data, "commit"))
blame = c(blame, getHM(data, "blame"))
resMonolog = computeResults4Vars(data,380.75,103.5,1067.25,273.0, 0.05)
newDoa = c(newDoa,max(resMonolog$doa))
newCommit = c(newCommit, max(resMonolog$commit))
newBlame = c(newBlame, max(resMonolog$blame))

print("Computing Salt results ...")
data = subset(oracleData, system == "salt")
data$filtered = F
repository=c(repository,"Salt")
doa = c(doa, getHM(data, "doa"))
commit = c(commit, getHM(data, "commit"))
blame = c(blame, getHM(data, "blame"))
resSalt = computeResults4Vars(data,366.25,33.0,961.5,182.5, 0.05)
newDoa = c(newDoa,max(resSalt$doa))
newCommit = c(newCommit, max(resSalt$commit))
newBlame = c(newBlame, max(resSalt$blame))

print("Computing Django results ...")
data = subset(oracleData, system == "django")
data$filtered = F
repository=c(repository,"Django")
doa = c(doa, getHM(data, "doa"))
commit = c(commit, getHM(data, "commit"))
blame = c(blame, getHM(data, "blame"))
resDjango = computeResults4Vars(data,404.75,31.0,1154.25,232.25, 0.05)
newDoa = c(newDoa,max(resDjango$doa))
newCommit = c(newCommit, max(resDjango$commit))
newBlame = c(newBlame, max(resDjango$blame))

print("Computing Faker results ...")
data = subset(oracleData, system == "Faker")
data$filtered = F
repository=c(repository,"Faker")
doa = c(doa, getHM(data, "doa"))
commit = c(commit, getHM(data, "commit"))
blame = c(blame, getHM(data, "blame"))
resFaker = computeResults4Vars(data,281.0,25.75,853.0,120.75, 0.05)
newDoa = c(newDoa,max(resFaker$doa))
newCommit = c(newCommit, max(resFaker$commit))
newBlame = c(newBlame, max(resFaker$blame))

print("Computing Ember.Js results ...")
data = subset(oracleData, system == "ember.js")
data$filtered = F
repository=c(repository,"Ember.js")
doa = c(doa, getHM(data, "doa"))
commit = c(commit, getHM(data, "commit"))
blame = c(blame, getHM(data, "blame"))
resEmber = computeResults4Vars(data,74.0,24.25,928.0,240.75, 0.05)
newDoa = c(newDoa,max(resEmber$doa))
newCommit = c(newCommit, max(resEmber$commit))
newBlame = c(newBlame, max(resEmber$blame))

d = data.frame(repository, doa, newDoa, commit, newCommit, blame, newBlame)
d$doaDiff=d$newDoa-d$doa
d$commitDiff=d$newCommit-d$commit
d$blameDiff=d$newBlame-d$blame


value = c(d$newDoa, d$newCommit, d$newBlame)
### Rename
setDT(d)
for (jj in 1:ncol(d)) set(d, i = which(d[[jj]]=="Commercial#1"), j = jj, v = "Commercial #1")
for (jj in 1:ncol(d)) set(d, i = which(d[[jj]]=="Commercial#2"), j = jj, v = "Commercial #2")
setDF(d)
###########

repository = factor(c(as.character(d$repository),as.character(d$repository),as.character(d$repository)),
                    c("Moment", "Fog", "Ember.js", "Django", "Salt", "Commercial #2", "Commercial #1", "Puppet", "Faker", "Monolog"))
category = factor(rep(c("DOA", "Commit", "Blame"),c(10,10,10)), c("DOA", "Commit", "Blame"))

plotData = data.frame(category, repository, value)

### Bar plot of the Harmonic Mean after controlling for recency and file size ###
ggplot(data=plotData, aes(x=repository, y=value*100, fill=category)) +
  geom_bar(width=0.8, stat="identity", color="black", position=position_dodge())+
  theme_minimal() +
  xlab("System") + ylab("HM (%)") + ggtitle("") + 
  scale_fill_grey(name="Technique") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5))


# sink()