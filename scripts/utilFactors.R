################################# TEST FACTORS FUNCTIONS #################################
commercialThresholds = c(0.6, 0.2, 0.1)
ossThresholds = c(0.8, 0.6, 0.1)

getThreshold = function(systemGroup, metric){
  if (systemGroup == "Commercial"){
    if (metric == "DOA")
      return(commercialThresholds[1])
    if (metric == "Commit")
      return(commercialThresholds[2])
    if (metric == "Blame")
      return(commercialThresholds[3])
  }
  else if (systemGroup == "OSS"){
    if (metric == "DOA")
      return(ossThresholds[1])
    if (metric == "Commit")
      return(ossThresholds[2])
    if (metric == "Blame")
      return(ossThresholds[3])
  }
  else
    return(NULL)
}
thresholdList = function(systems, metric){
  retList = c()
  for (system in systems)
    retList = c(retList, getThreshold(system, metric))
  return (retList)
}


applyRecencyAndFileSize2 = function(data, shortTime=0, smalFile=0, longTime=0, largeFile=0, thresholdFactorFileSize=0.25, thresholdFactorRecency=0.25, toPrint = T){
  newData = data.frame(data)
  newData$newDoaV = ifelse(newData$filtered, newData$doaV,  
                      ifelse(newData$LoCode<=smalFile, newData$doaV*(1+thresholdFactorFileSize),
                        ifelse(newData$daysLC<=shortTime, newData$doaV*(1+thresholdFactorRecency), newData$doaV)))
  newData$newCommitV = ifelse(newData$filtered, newData$CommitV,  
                              ifelse(newData$LoCode<=smalFile, newData$CommitV*(1+thresholdFactorFileSize),
                                     ifelse(newData$daysLC<=shortTime, newData$CommitV*(1+thresholdFactorRecency), newData$CommitV)))
  newData$newBlameV = ifelse(newData$filtered, newData$blameV,  
                             ifelse(newData$LoCode<=smalFile, newData$blameV*(1+thresholdFactorFileSize),
                                    ifelse(newData$daysLC<=shortTime, newData$blameV*(1+thresholdFactorRecency), newData$blameV)))
  newData$changed = ifelse(newData$doaV!=newData$newDoaV|newData$CommitV!=newData$newCommitV|newData$blameV!=newData$newBlameV, T, F)
  if (toPrint)
    print(length(newData$changed[newData$changed]))
  if (longTime + largeFile>0){
    newData$newDoaV = ifelse(newData$filtered, newData$doaV,  
                             ifelse(newData$LoCode<=smalFile, newData$doaV*(1-thresholdFactorFileSize),
                                    ifelse(newData$daysLC<=shortTime, newData$doaV*(1-thresholdFactorRecency), newData$doaV)))
    newData$newCommitV = ifelse(newData$filtered, newData$CommitV,  
                                ifelse(newData$LoCode<=smalFile, newData$CommitV*(1-thresholdFactorFileSize),
                                       ifelse(newData$daysLC<=shortTime, newData$CommitV*(1-thresholdFactorRecency), newData$CommitV)))
    newData$newBlameV = ifelse(newData$filtered, newData$blameV,  
                               ifelse(newData$LoCode<=smalFile, newData$blameV*(1-thresholdFactorFileSize),
                                      ifelse(newData$daysLC<=shortTime, newData$blameV*(1-thresholdFactorRecency), newData$blameV)))
  }
  newData$changed = ifelse(newData$doaV!=newData$newDoaV|newData$CommitV!=newData$newCommitV|newData$blameV!=newData$newBlameV, T, F)
  
  if (toPrint)
    print(length(newData$changed[newData$changed]))
  
  
  newData$doaThreshold = thresholdList(newData$systemsGroup, "DOA")
  newData$commitThreshold = thresholdList(newData$systemsGroup, "Commit")
  newData$blameThreshold = thresholdList(newData$systemsGroup, "Blame")
  newData$newDOA = ifelse(newData$classification == "maintainer",
                          ifelse(newData$newDoaV>=newData$doaThreshold, "hit", "miss"),
                          ifelse(newData$newDoaV<newData$doaThreshold, "hit", "miss"))
  newData$newCommit = ifelse(newData$classification == "maintainer",
                             ifelse(newData$newCommitV>=newData$commitThreshold, "hit", "miss"),
                             ifelse(newData$newCommitV<newData$commitThreshold, "hit", "miss"))
  newData$newBlame = ifelse(newData$classification == "maintainer",
                            ifelse(newData$newBlameV>=newData$blameThreshold, "hit", "miss"),
                            ifelse(newData$newBlameV<newData$blameThreshold, "hit", "miss"))
  newData$changed2 = ifelse(newData$doa!=newData$newDOA|newData$commit!=newData$newCommit|newData$blame!=newData$newBlame, T, F)
  doaChanged = newData$newDOA[newData$doa!=newData$newDOA]
  commitChanged = newData$newCommit[newData$commit!=newData$newCommit]
  blameChanged = newData$newBlame[newData$blame!=newData$newBlame]
  if (toPrint){
    cat("\n\nChanged: ", length(doaChanged), length(commitChanged), length(blameChanged), length(newData$changed2[newData$changed2]), "\n")
    cat("Changed: ", 
        length(doaChanged[doaChanged=="hit"])/length(doaChanged), 
        length(commitChanged[commitChanged=="hit"])/length(commitChanged), 
        length(blameChanged[blameChanged=="hit"])/length(blameChanged), "\n\n")
  }
  
  newData$doa = newData$newDOA
  newData$commit = newData$newCommit
  newData$blame = newData$newBlame
  
  newData$doaV = newData$newDoaV
  newData$CommitV = newData$newCommitV
  newData$blameV = newData$newBlameV
  
  return(newData)
}
applyRecencyAndFileSize = function(data, shortTime=0, smalFile=0, longTime=0, largeFile=0, thresholdFactorS=0.25, thresholdFactorL=0.25, toPrint = T){
  newData = data.frame(data)
  newData$newDoaV = ifelse(!newData$filtered&(newData$LoCode<=smalFile|newData$daysLC<=shortTime), newData$doaV*(1+thresholdFactorS), newData$doaV)
  newData$newCommitV = ifelse(!newData$filtered&(newData$LoCode<=smalFile|newData$daysLC<=shortTime), newData$CommitV*(1+thresholdFactorS), newData$CommitV)
  newData$newBlameV = ifelse(!newData$filtered&(newData$LoCode<=smalFile|newData$daysLC<=shortTime), newData$blameV*(1+thresholdFactorS), newData$blameV)
  newData$changed = ifelse(newData$doaV!=newData$newDoaV|newData$CommitV!=newData$newCommitV|newData$blameV!=newData$newBlameV, T, F)
  if (toPrint)
    print(length(newData$changed[newData$changed]))
  if (longTime + largeFile>0){
    newData$newDoaV = ifelse(!newData$filtered&(newData$LoCode>=largeFile|newData$daysLC>=longTime), newData$doaV*(1-thresholdFactorL), newData$newDoaV)
    newData$newCommitV = ifelse(!newData$filtered&(newData$LoCode>=largeFile|newData$daysLC>=longTime), newData$CommitV*(1-thresholdFactorL), newData$newCommitV)
    newData$newBlameV = ifelse(!newData$filtered&(newData$LoCode>=largeFile|newData$daysLC>=longTime), newData$blameV*(1-thresholdFactorL), newData$newBlameV)
  }
  newData$changed = ifelse(newData$doaV!=newData$newDoaV|newData$CommitV!=newData$newCommitV|newData$blameV!=newData$newBlameV, T, F)
  
  if (toPrint)
    print(length(newData$changed[newData$changed]))
  
  
  newData$doaThreshold = thresholdList(newData$systemsGroup, "DOA")
  newData$commitThreshold = thresholdList(newData$systemsGroup, "Commit")
  newData$blameThreshold = thresholdList(newData$systemsGroup, "Blame")
  newData$newDOA = ifelse(newData$classification == "maintainer",
                          ifelse(newData$newDoaV>=newData$doaThreshold, "hit", "miss"),
                          ifelse(newData$newDoaV<newData$doaThreshold, "hit", "miss"))
  newData$newCommit = ifelse(newData$classification == "maintainer",
                             ifelse(newData$newCommitV>=newData$commitThreshold, "hit", "miss"),
                             ifelse(newData$newCommitV<newData$commitThreshold, "hit", "miss"))
  newData$newBlame = ifelse(newData$classification == "maintainer",
                            ifelse(newData$newBlameV>=newData$blameThreshold, "hit", "miss"),
                            ifelse(newData$newBlameV<newData$blameThreshold, "hit", "miss"))
  newData$changed2 = ifelse(newData$doa!=newData$newDOA|newData$commit!=newData$newCommit|newData$blame!=newData$newBlame, T, F)
  doaChanged = newData$newDOA[newData$doa!=newData$newDOA]
  commitChanged = newData$newCommit[newData$commit!=newData$newCommit]
  blameChanged = newData$newBlame[newData$blame!=newData$newBlame]
  if (toPrint){
    cat("\n\nChanged: ", length(doaChanged), length(commitChanged), length(blameChanged), length(newData$changed2[newData$changed2]), "\n")
    cat("Changed: ", 
        length(doaChanged[doaChanged=="hit"])/length(doaChanged), 
        length(commitChanged[commitChanged=="hit"])/length(commitChanged), 
        length(blameChanged[blameChanged=="hit"])/length(blameChanged), "\n\n")
  }
  
  newData$doa = newData$newDOA
  newData$commit = newData$newCommit
  newData$blame = newData$newBlame
  
  newData$doaV = newData$newDoaV
  newData$CommitV = newData$newCommitV
  newData$blameV = newData$newBlameV
  
  return(newData)
}


applyRecencyAndFileSizeAllMiss = function(data, shortTime=0, smalFile=0, thresholdFactorS=0.25){
  newData = data.frame(data)
  newData$newDoaV = ifelse(!newData$filtered&newData$classification == "maintainer" & newData$doa == "miss" & newData$commit == "miss" & newData$blame == "miss", ifelse(newData$LoCode<=smalFile|newData$daysLC<=shortTime, newData$doaV*(1+thresholdFactorS), newData$doaV), newData$doaV)
  newData$newCommitV = ifelse(!newData$filtered&newData$classification == "maintainer" & newData$doa == "miss" & newData$commit == "miss" & newData$blame == "miss", ifelse(newData$LoCode<=smalFile|newData$daysLC<=shortTime, newData$CommitV*(1+thresholdFactorS), newData$CommitV), newData$CommitV)
  newData$newBlameV = ifelse(!newData$filtered&newData$classification == "maintainer" & newData$doa == "miss" & newData$commit == "miss" & newData$blame == "miss", ifelse(newData$LoCode<=smalFile|newData$daysLC<=shortTime, newData$blameV*(1+thresholdFactorS), newData$blameV), newData$blameV)
  newData$changed = ifelse(newData$doaV!=newData$newDoaV|newData$CommitV!=newData$newCommitV|newData$blameV!=newData$newBlameV, T, F)
  cat("Changed: " , length(newData$changed[newData$changed]), "\n")
  
  newData$doaThreshold = thresholdList(newData$systemsGroup, "DOA")
  newData$commitThreshold = thresholdList(newData$systemsGroup, "Commit")
  newData$blameThreshold = thresholdList(newData$systemsGroup, "Blame")
  newData$newDOA = ifelse(newData$classification == "maintainer",
                          ifelse(newData$newDoaV>=newData$doaThreshold, "hit", "miss"),
                          ifelse(newData$newDoaV<newData$doaThreshold, "hit", "miss"))
  newData$newCommit = ifelse(newData$classification == "maintainer",
                             ifelse(newData$newCommitV>=newData$commitThreshold, "hit", "miss"),
                             ifelse(newData$newCommitV<newData$commitThreshold, "hit", "miss"))
  newData$newBlame = ifelse(newData$classification == "maintainer",
                            ifelse(newData$newBlameV>=newData$blameThreshold, "hit", "miss"),
                            ifelse(newData$newBlameV<newData$blameThreshold, "hit", "miss"))
  newData$changed2 = ifelse(newData$doa!=newData$newDOA|newData$commit!=newData$newCommit|newData$blame!=newData$newBlame, T, F)
  newData$doa = newData$newDOA
  newData$commit = newData$newCommit
  newData$blame = newData$newBlame
  cat("\n\nChanged: ", length(newData$changed2[newData$changed2]), "\n\n")
  return(newData)
}

thresholdAdjust = function(data, factor, valueRef, thresholdFactor = 0.25, leq=T, toPrint = F){
  newData = data.frame(data)
  if (leq){
    newData$newDoaV = ifelse(!newData$filtered&newData[[factor]]<=valueRef, newData$doaV*(1+thresholdFactor), newData$doaV)
    newData$newCommitV = ifelse(!newData$filtered&newData[[factor]]<=valueRef, newData$CommitV*(1+thresholdFactor), newData$CommitV)
    newData$newBlameV = ifelse(!newData$filtered&newData[[factor]]<=valueRef, newData$blameV*(1+thresholdFactor), newData$blameV)
    newData$changed = ifelse(newData$doaV!=newData$newDoaV|newData$CommitV!=newData$newCommitV|newData$blameV!=newData$newBlameV, T, F)
    if (toPrint)
      print(length(newData$changed[newData$changed]))
  }
  else{
    newData$newDoaV = ifelse(!newData$filtered&newData[[factor]]>=valueRef, newData$doaV*(1+thresholdFactor), newData$doaV)
    newData$newCommitV = ifelse(!newData$filtered&newData[[factor]]>=valueRef, newData$CommitV*(1+thresholdFactor), newData$CommitV)
    newData$newBlameV = ifelse(!newData$filtered&newData[[factor]]>=valueRef, newData$blameV*(1+thresholdFactor), newData$blameV)
    newData$changed = ifelse(newData$doaV!=newData$newDoaV|newData$CommitV!=newData$newCommitV|newData$blameV!=newData$newBlameV, T, F)
    if (toPrint)
      print(length(newData$changed[newData$changed]))
  }
  newData$doaThreshold = thresholdList(newData$systemsGroup, "DOA")
  newData$commitThreshold = thresholdList(newData$systemsGroup, "Commit")
  newData$blameThreshold = thresholdList(newData$systemsGroup, "Blame")
  newData$newDOA = ifelse(newData$classification == "maintainer",
                          ifelse(newData$newDoaV>=newData$doaThreshold, "hit", "miss"),
                          ifelse(newData$newDoaV<newData$doaThreshold, "hit", "miss"))
  newData$newCommit = ifelse(newData$classification == "maintainer",
                             ifelse(newData$newCommitV>=newData$commitThreshold, "hit", "miss"),
                             ifelse(newData$newCommitV<newData$commitThreshold, "hit", "miss"))
  newData$newBlame = ifelse(newData$classification == "maintainer",
                            ifelse(newData$newBlameV>=newData$blameThreshold, "hit", "miss"),
                            ifelse(newData$newBlameV<newData$blameThreshold, "hit", "miss"))
  newData$changed2 = ifelse(newData$doa!=newData$newDOA|newData$commit!=newData$newCommit|newData$blame!=newData$newBlame, T, F)
  newData$doa = newData$newDOA
  newData$commit = newData$newCommit
  newData$blame = newData$newBlame
  
  newData$doaV = newData$newDoaV
  newData$CommitV = newData$newCommitV
  newData$blameV = newData$newBlameV
  
  if (toPrint)
    cat("\n\nChanged: ", length(newData$changed2[newData$changed2]), "\n\n")
  return(newData)
}

thresholdAdjustBoth = function(data, factor, valueRef1, valueRef2, thresholdFactor = 0.25){
  newData = data.frame(data)
  newData$newDoaV = ifelse(!newData$filtered&newData[[factor]]<=valueRef1, newData$doaV*(1+thresholdFactor), newData$doaV)
  newData$newCommitV = ifelse(!newData$filtered&newData[[factor]]<=valueRef1, newData$CommitV*(1+thresholdFactor), newData$CommitV)
  newData$newBlameV = ifelse(!newData$filtered&newData[[factor]]<=valueRef1, newData$blameV*(1+thresholdFactor), newData$blameV)
  newData$changed = ifelse(newData$doaV!=newData$newDoaV|newData$CommitV!=newData$newCommitV|newData$blameV!=newData$newBlameV, T, F)
  print(length(newData$changed[newData$changed]))
  newData$newDoaV = ifelse(!newData$filtered&newData[[factor]]>=valueRef2, newData$doaV*(1-thresholdFactor), newData$newDoaV)
  newData$newCommitV = ifelse(!newData$filtered&newData[[factor]]>=valueRef2, newData$CommitV*(1-thresholdFactor), newData$newCommitV)
  newData$newBlameV = ifelse(!newData$filtered&newData[[factor]]>=valueRef2, newData$blameV*(1-thresholdFactor), newData$newBlameV)
  newData$changed = ifelse(newData$doaV!=newData$newDoaV|newData$CommitV!=newData$newCommitV|newData$blameV!=newData$newBlameV, T, F)
  print(length(newData$changed[newData$changed]))
  
  newData$doaThreshold = thresholdList(newData$systemsGroup, "DOA")
  newData$commitThreshold = thresholdList(newData$systemsGroup, "Commit")
  newData$blameThreshold = thresholdList(newData$systemsGroup, "Blame")
  newData$newDOA = ifelse(newData$classification == "maintainer",
                          ifelse(newData$newDoaV>=newData$doaThreshold, "hit", "miss"),
                          ifelse(newData$newDoaV<newData$doaThreshold, "hit", "miss"))
  newData$newCommit = ifelse(newData$classification == "maintainer",
                             ifelse(newData$newCommitV>=newData$commitThreshold, "hit", "miss"),
                             ifelse(newData$newCommitV<newData$commitThreshold, "hit", "miss"))
  newData$newBlame = ifelse(newData$classification == "maintainer",
                            ifelse(newData$newBlameV>=newData$blameThreshold, "hit", "miss"),
                            ifelse(newData$newBlameV<newData$blameThreshold, "hit", "miss"))
  newData$changed2 = ifelse(newData$doa!=newData$newDOA|newData$commit!=newData$newCommit|newData$blame!=newData$newBlame, T, F)
  newData$doa = newData$newDOA
  newData$commit = newData$newCommit
  newData$blame = newData$newBlame
  cat("\n\nChanged: ", length(newData$changed2[newData$changed2]), "\n\n")
  return(newData)
}

getHM = function(data, method){
  MaintainerData = subset(data, classification == "maintainer")
  NonMaintainerData = subset(data, classification == "non-maintainer")
  
  newData = MaintainerData[method]
  hitsM = length(newData[newData=="hit"])
  missesM = length(newData[newData=="miss"])
  HR_M = hitsM/(hitsM+missesM)
  
  
  newData = NonMaintainerData[method]
  hitsNM = length(newData[newData=="hit"])
  missesNM = length(newData[newData=="miss"])
  HR_NM = hitsNM/(hitsNM+missesNM)
  
  return(2*HR_M*HR_NM/(HR_M + HR_NM))
}

computeResults4Vars = function(data, shortTime=0, smallFile=0, longTime=0, largeFile=0, step=0.05){
  if(shortTime+smallFile+longTime+largeFile==0){
    shortTime = quantile(data$daysLC)["25%"]
    longTime = quantile(data$daysLC)["75%"]
    smallFile = quantile(data$LoCode)["25%"]
    largeFile = quantile(data$LoCode)["75%"]
  }
  numCalc = 1/step+1
  numCalc = numCalc^4
  smallT = c()
  largeT = c()
  shortT = c()
  longT = c()
  doa = c()
  commit = c()
  blame = c()
  count = 0
  for (sm in seq(0,1,step)){
    for (la in seq(0,1,step)){
      for (sh in seq(0,1,step)){
        for (lo in seq (0,1,step) ){
          count = count+1
          
          smallT = c(smallT, sm)
          largeT = c(largeT, -la)
          shortT = c(shortT, sh)
          longT = c(longT, -lo)
          temp = thresholdAdjust(data, "LoCode", smallFile, thresholdFactor = sm, leq=T, F)
          temp = thresholdAdjust(temp, "LoCode", largeFile, thresholdFactor = -la, leq=F, F)
          temp = thresholdAdjust(temp, "daysLC", shortTime, thresholdFactor = sh, leq=T, F)
          temp = thresholdAdjust(temp, "daysLC", longTime, thresholdFactor = -lo, leq=F, F)
          doa = c(doa, getHM(temp, "doa"))
          commit = c(commit, getHM(temp, "commit"))
          blame = c(blame, getHM(temp, "blame"))
          if (count%%10000==0)
            cat(as.character(Sys.time()), "(", count,"-",numCalc, ")", "-", "Max: ", 
                max(doa), "(", smallT[which(doa==max(doa))[1]], largeT[which(doa==max(doa))[1]], shortT[which(doa==max(doa))[1]], longT[which(doa==max(doa))[1]], ")", 
                max(commit), "(", smallT[which(commit==max(commit))[1]], largeT[which(commit==max(commit))[1]], shortT[which(commit==max(commit))[1]], longT[which(commit==max(commit))[1]], ")", 
                max(blame), "(", smallT[which(blame==max(blame))[1]], largeT[which(blame==max(blame))[1]], shortT[which(blame==max(blame))[1]], longT[which(blame==max(blame))[1]], ")", 
                "\n")
        }
      }
    }
  }
  cat("Final - Max: ", 
      max(doa), "(", smallT[which(doa==max(doa))[1]], largeT[which(doa==max(doa))[1]], shortT[which(doa==max(doa))[1]], longT[which(doa==max(doa))[1]], ")", 
      max(commit), "(", smallT[which(commit==max(commit))[1]], largeT[which(commit==max(commit))[1]], shortT[which(commit==max(commit))[1]], longT[which(commit==max(commit))[1]], ")", 
      max(blame), "(", smallT[which(blame==max(blame))[1]], largeT[which(blame==max(blame))[1]], shortT[which(blame==max(blame))[1]], longT[which(blame==max(blame))[1]], ")", 
      "\n")
  return(data.frame(smallT, largeT, shortT, longT, doa, commit, blame))
}

printBestValues = function(sData, data, method, shortTime=0, smallFile=0, longTime=0, largeFile=0){
  sData$filtered = F
  sm=data$smallT[which(data[method]==max(data[method]))[1]]
  la=data$largeT[which(data[method]==max(data[method]))[1]]
  sh=data$shortT[which(data[method]==max(data[method]))[1]] 
  lo=data$longT[which(data[method]==max(data[method]))[1]]
  temp = thresholdAdjust(sData, "LoCode", smallFile, thresholdFactor = sm, leq=T, F)
  temp = thresholdAdjust(temp, "LoCode", largeFile, thresholdFactor = la, leq=F, F)
  temp = thresholdAdjust(temp, "daysLC", shortTime, thresholdFactor = sh, leq=T, F)
  temp = thresholdAdjust(temp, "daysLC", longTime, thresholdFactor = lo, leq=F, F)
  m = subset(temp, classification == "maintainer")
  nm = subset(temp, classification == "non-maintainer")
  HR_M = length(m[method][m[method]=="hit"])/nrow(m)
  HR_NM = length(nm[method][nm[method]=="hit"])/nrow(nm)
  # cat(method,f(HR_M), f(HR_NM), f(2*HR_M*HR_NM/(HR_M + HR_NM)), "\n", sep=";") 
  # cat(method,length(m[method][m[method]=="hit"]), nrow(m), length(nm[method][nm[method]=="hit"]), nrow(nm), "\n", sep=";") 

  return(list(m_hits=length(m[method][m[method]=="hit"]), 
              m_total=nrow(m), 
              nm_hits=length(nm[method][nm[method]=="hit"]), 
              nm_total=nrow(nm)))
}

get4VarRes =  function(sData, data, shortTime=0, smallFile=0, longTime=0, largeFile=0){
  # classification = c()
  # total = c()
  # doa = c()
  # commit = c()
  # blame = c()
  doa=printBestValues(sData, data, "doa", shortTime, smallFile, longTime, largeFile)
  commit=printBestValues(sData, data, "commit", shortTime, smallFile, longTime, largeFile)
  blame=printBestValues(sData, data, "blame", shortTime, smallFile, longTime, largeFile)
  return(data.frame(m_total=doa$m_total, 
              m_doa=doa$m_hits, 
              m_commit=commit$m_hits, 
              m_blame=blame$m_hits, 
              nm_total=doa$nm_total, 
              nm_doa=doa$nm_hits, 
              nm_commit=commit$nm_hits, 
              nm_blame=blame$nm_hits))
}


computeResults = function(data, shortTime=0, smallFile=0, longTime=0, largeFile=0, step=0.05, toPrint=T){
  if(shortTime+smallFile+longTime+largeFile==0){
    shortTime = quantile(data$daysLC)["25%"]
    longTime = quantile(data$daysLC)["75%"]
    smallFile = quantile(data$LoCode)["25%"]
    largeFile = quantile(data$LoCode)["75%"]
  }
  sizeT = c()
  recencyT = c()
  doa = c()
  commit = c()
  blame = c()
  
  for (s in seq(0,1,step)){
    for (r in seq (0,1,step) ){
      sizeT = c(sizeT, s)
      recencyT = c(recencyT, r)
      temp = applyRecencyAndFileSize(data, shortTime, smallFile, longTime, largeFile, s, r, F)
      doa = c(doa, getHM(temp, "doa"))
      commit = c(commit, getHM(temp, "commit"))
      blame = c(blame, getHM(temp, "blame"))
    }
  }
  if (toPrint)
    cat("Max: ", max(doa), max(commit), max(blame),"\n")
  return(data.frame(sizeT, recencyT, doa, commit, blame))
}
getNewResults = function(oracleData){
  repository=c()
  doa=c()
  commit=c()
  blame=c()
  newDoa=c()
  newCommit=c()
  newBlame=c()
  
  data = subset(oracleData, system == "hot-marketplace" | system == "hot-vulcano")
  repository=c(repository,"Commercial#1")
  doa = c(doa, getHM(data, "doa"))
  commit = c(commit, getHM(data, "commit"))
  blame = c(blame, getHM(data, "blame"))
  data$filtered = F
  t = computeResults(data, toPrint=F)
  newDoa = c(newDoa,max(t$doa))
  newCommit = c(newCommit, max(t$commit))
  newBlame = c(newBlame, max(t$blame))
  data = subset(oracleData, system == "ring-daemon")
  repository=c(repository,"Commercial#2")
  doa = c(doa, getHM(data, "doa"))
  commit = c(commit, getHM(data, "commit"))
  blame = c(blame, getHM(data, "blame"))
  data$filtered = F
  t = computeResults(data, toPrint=F)
  newDoa = c(newDoa,max(t$doa))
  newCommit = c(newCommit, max(t$commit))
  newBlame = c(newBlame, max(t$blame))
  for (s in unique(ossData$system)){
    data = subset(ossData, system == s)
    if (s=="moment/moment")
      repository=c(repository,"moment")
    else
      repository=c(repository,s)
    doa = c(doa, getHM(data, "doa"))
    commit = c(commit, getHM(data, "commit"))
    blame = c(blame, getHM(data, "blame"))
    data$filtered = F
    t = computeResults(data, toPrint=F)
    newDoa = c(newDoa,max(t$doa))
    newCommit = c(newCommit, max(t$commit))
    newBlame = c(newBlame, max(t$blame))
    # cat(s, doa, newDoa, commit, newCommit, blame, newBlame, "\n", sep = ";")
    # q = quantile(data$LoCode)
    # t = computeResults(data, 30, q["25%"], 90, q["75%"])
  }
  d = data.frame(repository, doa, newDoa, commit, newCommit, blame, newBlame)
  d$doaDiff=d$newDoa-d$doa
  d$commitDiff=d$newCommit-d$commit
  d$blameDiff=d$newBlame-d$blame
  return(d)
}

marketRes = c()
ringRes = c()
ossRes = c()
getNewResults4Vars = function(oracleData){
  repository=c()
  doa=c()
  commit=c()
  blame=c()
  newDoa=c()
  newCommit=c()
  newBlame=c()
  
  data = subset(oracleData, system == "hot-marketplace" | system == "hot-vulcano")
  repository=c(repository,"Commercial#1")
  doa = c(doa, getHM(data, "doa"))
  commit = c(commit, getHM(data, "commit"))
  blame = c(blame, getHM(data, "blame"))
  data$filtered = F
  t = computeResults4Vars(data)
  marketRes = t
  newDoa = c(newDoa,max(t$doa))
  newCommit = c(newCommit, max(t$commit))
  newBlame = c(newBlame, max(t$blame))
  data = subset(oracleData, system == "ring-daemon")
  repository=c(repository,"Commercial#2")
  doa = c(doa, getHM(data, "doa"))
  commit = c(commit, getHM(data, "commit"))
  blame = c(blame, getHM(data, "blame"))
  data$filtered = F
  t = computeResults4Vars(data)
  ringRes = t
  newDoa = c(newDoa,max(t$doa))
  newCommit = c(newCommit, max(t$commit))
  newBlame = c(newBlame, max(t$blame))
  for (s in unique(ossData$system)){
    data = subset(ossData, system == s)
    if (s=="moment/moment")
      repository=c(repository,"moment")
    else
      repository=c(repository,s)
    doa = c(doa, getHM(data, "doa"))
    commit = c(commit, getHM(data, "commit"))
    blame = c(blame, getHM(data, "blame"))
    data$filtered = F
    t = computeResults4Vars(data)
    ossRes = t
    newDoa = c(newDoa,max(t$doa))
    newCommit = c(newCommit, max(t$commit))
    newBlame = c(newBlame, max(t$blame))
    # cat(s, doa, newDoa, commit, newCommit, blame, newBlame, "\n", sep = ";")
    # q = quantile(data$LoCode)
    # t = computeResults(data, 30, q["25%"], 90, q["75%"])
  }
  d = data.frame(repository, doa, newDoa, commit, newCommit, blame, newBlame)
  d$doaDiff=d$newDoa-d$doa
  d$commitDiff=d$newCommit-d$commit
  d$blameDiff=d$newBlame-d$blame
  return(d)
}
 