require(beanplot)
require(vioplot)
require(devtools)
require(digest)
require(sm)
#install_github("TomKellyGenetics/vioplotx")
require("vioplotx")
require("gridExtra")
source("vioplot.R")

getDrawData = function(data, info, labels=c("small", "medium", "large")){
  qSize = quantile(data[[info]])
  data$class = ifelse(data[info]<=qSize["25%"], labels[1], ifelse(data[info]<qSize["75%"], labels[2], labels[3]))
  plotData = count(data, c("class", "classification"))
  plotData$perc = ifelse(plotData$class==labels[1], plotData$freq/sum(plotData$freq[plotData$class==labels[1]]), 
                         ifelse(plotData$class==labels[2], plotData$freq/sum(plotData$freq[plotData$class==labels[2]]),
                                plotData$freq/sum(plotData$freq[plotData$class==labels[3]])))
  plotData$perc = plotData$perc*100
  names(plotData) = c("class", "classification", "freq", "perc")
  newData = data.frame(plotData$class, plotData$classification, plotData$freq, plotData$perc) 
  names(newData) =  c("class", "classification", "freq", "perc")
  newData$class =  factor(newData$class, labels)
  return(newData)
}

drawPercGraph = function(data, info, labels=c("small", "medium", "large"), xlab="", ylab="Percentage (%)"){
  newData = getDrawData(data, info, labels)
  print(newData)
  return(ggplot(data=newData, aes(x=class, y=perc, fill=classification)) +
           geom_bar(stat="identity")+xlab(xlab)+ylab(ylab))
  
}


# drawPercGraph(oracleData, "LoCode", xlab = "Lines of Code" )
# drawPercGraph(oracleData, "daysLC", c("short", "medium", "long"), xlab = "Days Last Developer Commit")


drawSplitViolinPlot = function(data, title, xlab, logInfo="auto", cutminInfo = FALSE, cutmaxInfo = FALSE){
  plot(x=NULL, y=NULL,
       xlim = c(0.5, length(levels(data$category))+1), ylim=c(min(data$value), max(data$value)),
       type="n", ann=FALSE, axes=F, log = logInfo) +
    axis(1, at=(1:length(levels(data$category))),  labels=levels(data$category))
  axis(2)
  for (i in unique(data$category)) {
    for (j in unique(data$group)){
      vioplot2(data$value[which(data$category == i & data$group == j)],
               
               at = match(i, unique(data$category)),
               side = ifelse(j == 1, "left", "right"),
               col = ifelse(j == 1, "purple", "lightblue"),
               add = T)
    }
  }
  title(title, xlab=xlab)
  legend("bottomright", fill = c("purple", "lightblue"),
         legend = unique(data$groupName), box.lty=0)
  
  
}

getDeclaredData = function(data, categories, newnames){
  maintainersTemp = subset(data, classification == "maintainer")
  numMain = nrow(maintainersTemp)
  maintainersTemp = toCategoryData(maintainersTemp, categories, 'id')
  maintainersTemp$groupName = "Declared \nmaintainers"
  maintainersTemp$group = 1
  
  nonMaintainersTemp = subset(data, classification == "non-maintainer")
  numNonMain = nrow(nonMaintainersTemp)
  nonMaintainersTemp = toCategoryData(nonMaintainersTemp, categories, 'id')
  nonMaintainersTemp$groupName = "Declared \nnon-maintainers"
  nonMaintainersTemp$group = 2
  
  cat("|maintainers| = ", numMain, " |non-maintainers| = ", numNonMain, "\n")
  
  
  classData = rbind(maintainersTemp, nonMaintainersTemp)
  newHitData = merge(classData, data, by.x = 'id', by.y = 'id')
  newHitData$groupName = factor(newHitData$groupName, c("Declared \nmaintainers", 'Declared \nnon-maintainers')) 
  fill = c( "lightblue","purple")
  
  
  # ggplot(data=newHitData, aes(x=daysLastChange, y=LoCode, color=groupName)) + geom_point(shape=1) 
  for (categ in categories){
    
    
    subSample = subset(maintainersTemp, category == categ)
    
    subOthers = subset(nonMaintainersTemp, category == categ)
    w = wilcox.test(subSample$value, subOthers$value)
    cat("=====", categ,"=====\nW = ", w$statistic, " p-value= ", w$p.value, "\n====================\n\n", sep = "")
  }
  
  p = NA
  if(length(newnames)>0){
    setDT(newHitData)
    aux = 1
    for (categ in categories){
      set(newHitData, i = which(newHitData[["category"]]==categ), j = match("category",names(newHitData)), v = newnames[aux])
      aux = aux+1
    }
    setDF(newHitData)
    newHitData$category = factor(newHitData$category, newnames)
  }
  return(newHitData)
}

plotDeclaredBeanPlot = function(title, data, categories, log = FALSE, xlab='', ylab='', newnames=c(), legend="topright",
                                horizontal = F, mconfig=F){
  
  declaredData = getDeclaredData(data, categories, newnames);
  
  
  if(log){
    if (length(declaredData$value[declaredData$value==0])>0)
      declaredData$value = declaredData$value+1
    p = drawBeanPlot(declaredData, title, '', logInfo = "y", ylim, fill = fill, xlab=xlab, ylab=ylab, legend=legend, horizontal=horizontal, mconfig=mconfig)
  }
  else
    p = drawBeanPlot(declaredData, title, '', logInfo = "", ylim, fill = fill, xlab=xlab, ylab=ylab, legend=legend, horizontal=horizontal, mconfig=mconfig)
  
  return(p)
  
}

plotDeclaredViolinPlot = function(title, data, categories, log = FALSE, xlab='', ylab='',newnames=c(), legend="topright",
                                  horizontal = F, mconfig=F, h=NULL){
  
  
  declaredData = getDeclaredData(data, categories, newnames)
  newDeclaredData = subset(declaredData, value>0)
  
  vioplotx(value~category, data= subset(newDeclaredData, group == 1), wex=0.7, ylog = log, 
           col = "gray", plotCentre = "line", side = "left", logLab = c(1:1000), h=h)
  vioplotx(value~category, data= subset(newDeclaredData, group == 2), wex=0.7, ylog = log, 
           col = "#2C2C2C", plotCentre = "line", side = "right", add=T, h=h)
  title(xlab = title, ylab = ylab)
  legend("top", fill = c("gray", "#2C2C2C"), legend = c("Declared Maintainer", "Declared Non-maintainer"), cex=0.85, bty = "n")
  
  
  # logInfo = ifelse(log, "y", "")
  # plot(x=NULL, y=NULL,
  #      xlim = c(0.5, 3), 
  #      ylim=c(min(newDeclaredData$value), max(newDeclaredData$value)),
  #      type="n", ann=FALSE, axes=F, log = logInfo) 
  # tempData = subset(newDeclaredData, categ == "Recency")
  # vioplotx(subset(tempData, group == 1)$value, at=0.5, wex=0.5, ylog = log, col = "gray", plotCentre = "line", side = "left")
  # vioplotx(subset(tempData, group == 2)$value, at=0.5, wex=0.5, ylog = log,  col = "#2C2C2C", plotCentre = "line", side = "right", add = T)
  # 
  # # tempData = subset(newDeclaredData, categ == "Size")
  # vioplotx(subset(tempData, group == 1)$value, at=1, wex=0.5, ylog = log, col = "gray", plotCentre = "line", side = "left")
  # vioplotx(subset(tempData, group == 2)$value, at=1, wex=0.5, ylog = log,  col = "#2C2C2C", plotCentre = "line", side = "right", add = T)
  
}
# p7 = plotDeclaredViolinPlot("", oracleData, c("daysLC", "LoCode"), T, xlab="Dimension", ylab="Days (Recency) / LOC (FileSize)",
#                             newnames = c("Recency", "FileSize"), T, mconfig=T, h=0.3)


# +axis(1, at=(1:length(levels(data$category))),  labels=levels(data$category))
# axis(1)
# 
# vioplot2(data$value[data$groupName == "AllMiss"],
#          at = 0.8,
#          side = "right",
#          col = fill[1],
#          add = T, horizontal = T, wex = 1.4, drawRect = drawRect)
# vioplot2(data$value[data$groupName == "AllHit"],
#          at = 0.8,
#          side = "left",
#          col = fill[2],
#          add = T, horizontal = T, wex = 0.3, drawRect = drawRect)



plotHitsViolin2 = function(title, data, categories, log = FALSE, bean = FALSE, new = FALSE, printBoxplot = FALSE, ylim = NULL, type = "AllHit", xlab='', ylab='', newnames=c()){
  allHit = subset(data, doa == "hit" & commit == "hit" & blame == "hit")
  allHit = toCategoryData(allHit, categories, 'id')
  allHit$groupName = "AllHit"
  allHit$group = 1
  allMiss = subset(data, doa == "miss" & commit == "miss" & blame == "miss")
  allMiss = toCategoryData(allMiss, categories, 'id')
  allMiss$groupName = "All Miss"
  allMiss$group = 2
  allSample = data
  allSample = toCategoryData(allSample, categories, 'id')
  allSample$groupName = "Oracle"
  allSample$group = 3
  
  if (type == "AllHit"){
    hitData = rbind(allHit, allSample)
    newHitData = merge(hitData, data, by.x = 'id', by.y = 'id')
    newHitData$groupName = factor(newHitData$groupName, c('AllHit', 'Oracle')) 
    fill = c( "lightgreen","purple")
  }
  
  else{
    hitData = rbind(allMiss, allSample)
    newHitData = merge(hitData, data, by.x = 'id', by.y = 'id')
    newHitData$groupName = factor(newHitData$groupName, c("All Miss", 'Oracle')) 
    fill = c( "lightblue","purple")
  }
  
  # ggplot(data=newHitData, aes(x=daysLastChange, y=LoCode, color=groupName)) + geom_point(shape=1) 
  for (categ in categories){
    
    if (type == "AllHit")
      subSample = subset(allHit, category == categ)
    else
      subSample = subset(allMiss, category == categ)
    subAllSample = subset(allSample, category == categ)
    w = wilcox.test(subSample$value, subAllSample$value)
    cat("=====", categ,"=====\nW = ", w$statistic, " p-value= ", w$p.value, "\n====================\n\n", sep = "")
  }
  
  p = NA
  if(length(newnames)>0){
    setDT(newHitData)
    aux = 1
    for (categ in categories){
      set(newHitData, i = which(newHitData[["category"]]==categ), j = match("category",names(newHitData)), v = newnames[aux])
      aux = aux+1
    }
    setDF(newHitData)
    newHitData$category = factor(newHitData$category, newnames)
  }
  
  
  if (new){
    if(log){
      if (length(newHitData$value[newHitData$value==0])>0)
        newHitData$value = newHitData$value+1
      p = drawSplitViolinPlot(newHitData, title, '', logInfo = "y")
    }
    else
      p = drawSplitViolinPlot(newHitData, title, '', logInfo = "")
  }
  else {
    if (bean){
      if(log){
        if (length(newHitData$value[newHitData$value==0])>0)
          newHitData$value = newHitData$value+1
        p = drawBeanPlot(newHitData, title, '', logInfo = "y", ylim, fill = fill, xlab=xlab, ylab=ylab)
      }
      else
        p = drawBeanPlot(newHitData, title, '', logInfo = "", ylim, fill = fill, xlab=xlab, ylab=ylab)
    }
    
    
    else{
      if (log)
        p = ggplot(newHitData, aes(category, value, fill=groupName)) + geom_split_violin() + scale_y_log10()
      else 
        p = ggplot(newHitData, aes(category, value, fill=groupName)) + geom_split_violin() + title( main = title)
    }
  }
  if (printBoxplot){
    
    boxplot(value~category, data = subset(newHitData, groupName=="AllHit"), main ="AllHit", log = "y")
    boxplot(value~category, data = subset(newHitData, groupName=="AllMiss"), main ="AllMiss", log = "y")
  }
  
  return(p)
}

getHitData = function(data, categories, newnames){
  allMiss = subset(data, doa == "miss" & commit == "miss" & blame == "miss")
  allMiss = toCategoryData(allMiss, categories, 'id')
  allMiss$groupName = "AllMiss"
  allMiss$group = 2
  allHit = subset(data, doa == "hit" & commit == "hit" & blame == "hit")
  allHit = toCategoryData(allHit, categories, 'id')
  allHit$groupName = "AllHit"
  allHit$group = 1
  someHit = subset(data, doa == "miss" | commit == "miss" | blame == "miss")
  someHit = toCategoryData(someHit, categories, 'id')
  someHit$groupName = "SomeHit"
  someHit$group = 3
  
  hitData = rbind(allHit, allMiss)
  newHitData = merge(hitData, data, by.x = 'id', by.y = 'id')
  newHitData$groupName = factor(newHitData$groupName, c('AllMiss', 'AllHit')) 
  
  # ggplot(data=newHitData, aes(x=daysLastChange, y=LoCode, color=groupName)) + geom_point(shape=1) 
  
  p = NA
  if(length(newnames)>0){
    setDT(newHitData)
    aux = 1
    for (categ in categories){
      set(newHitData, i = which(newHitData[["category"]]==categ), j = match("category",names(newHitData)), v = newnames[aux])
      aux = aux+1
    }
    setDF(newHitData)
    newHitData$category = factor(newHitData$category, newnames)
  }
  for (categ in categories){
    subAllHit = subset(allHit, category == categ)
    subAllMiss = subset(allMiss, category == categ)
    w = wilcox.test(subAllHit$value, subAllMiss$value)
    cat("=====", categ,"=====\nW = ", w$statistic, " p-value= ", w$p.value, "\n====================\n\n", sep = "")
  }
  return(newHitData)
}
plotHitsViolin = function(title, data, categories, log = FALSE, bean = FALSE, new = FALSE, 
                          printBoxplot = FALSE, ylim = NULL, xlab='', ylab='', newnames=c(),
                          horizontal = F, fill = c("gray", "#2C2C2C")){
  
  newHitData = getHitData(data, categories, newnames)
  
  
  if (new){
    if(log){
      if (length(newHitData$value[newHitData$value==0])>0)
        newHitData$value = newHitData$value+1
      p = drawSplitViolinPlot(newHitData, title, '', logInfo = "y")
    }
    else
      p = drawSplitViolinPlot(newHitData, title, '', logInfo = "")
  }
  else {
    if (bean){
      if(log){
        if (length(newHitData$value[newHitData$value==0])>0)
          newHitData$value = newHitData$value+1
        p = drawBeanPlot(newHitData, title, '', logInfo = "y", ylim, xlab=xlab, ylab=ylab, horizontal=horizontal, fill=fill)
      }
      else
        p = drawBeanPlot(newHitData, title, '', logInfo = "", ylim, xlab=xlab, ylab=ylab, horizontal=horizontal, fill=fill)
    }
    
    
    else{
      if (log)
        p = ggplot(newHitData, aes(category, value, fill=groupName)) + geom_split_violin() + scale_y_log10()
      else 
        p = ggplot(newHitData, aes(category, value, fill=groupName)) + geom_split_violin() + title( main = title)
      
      if (horizontal)
        p = p + coord_flip()
    }
  }
  if (printBoxplot){
    
    boxplot(value~category, data = subset(newHitData, groupName=="AllHit"), main ="AllHit", log = "y")
    boxplot(value~category, data = subset(newHitData, groupName=="AllMiss"), main ="AllMiss", log = "y")
  }
  
  return(p)
}

drawSimpleSplitedViolinPlot = function(data, xlab, logInfo="auto",  drawRect = T, circle=F, prop = F, fill= c("#2C2C2C", "gray")){
  if (prop){
    at = 0.8
    wex1 = 1.4
    wex2 = 0.4
    h = 4
  }
  else{
    at = 1
    wex1 = 1
    wex2 = 1
    h = NULL
  }
  plot(x=NULL, y=NULL,
       ylim = c(1,1), 
       xlim=c(min(data$value), max(data$value)),
       type="n", ann=FALSE, axes=F, log = logInfo) 
  # +axis(1, at=(1:length(levels(data$category))),  labels=levels(data$category))
  axis(1)
  vioplot2(data$value[data$groupName == "AllMiss"],
           at = at,
           side = "right",
           col = fill[1],
           add = T, horizontal = T, wex = wex1, drawRect = drawRect)
  vioplot2(data$value[data$groupName == "AllHit"],
           at = at,
           side = "left",
           col = fill[2],
           add = T, horizontal = T, wex = wex2, drawRect = drawRect, h=h)
  if (circle){
    points(median(data$value[data$groupName == "AllMiss"]), at, pch = 21,  bg = "white")
    points(median(data$value[data$groupName == "AllHit"]), at, pch = 21,  bg = "white")
  }
  
  title(xlab=xlab)
  legend("top", fill = fill,
         legend =c("AllMiss", "AllHit"), box.lty=0, horiz=T)
  
  
}

drawSimpleViolinPlot = function(data, xlab="", ylab = "", width=0.03){
  p = ggplot(data, aes(x=groupName, y=value)) + geom_violin() + 
    theme(plot.margin = margin(0, 2, 0, 2, "cm")) +
    geom_boxplot(width=width, outlier.shape = NA) +
    labs(x=xlab, y = ylab)
  return(p)
}
# drawSimpleViolinPlot(newHitData, "", "Developer Commits (%)")
data_names <- list(
  'daysLC'=" Recency",
  'LoCode'="File Size",
  '#3'="other3",
  '#4'="other4"
)
labeller <- function(variable,value){
  return(data_names[value])
}
drawViolinPlot = function(data, xlab="", ylab1 = "", ylab2 = "", categories = c("daysLC", "LoCode"),  width=0.05, colored=F, leg = T, specific=NULL){
  declaredData = getDeclaredData(data, categories, c())
  newDeclaredData = subset(declaredData, value>0)
  maxValue = 4000
  data1 = subset(newDeclaredData, category == categories[1])
  if (colored){
    p1 = ggplot(data1, aes(x=groupName, y=value, fill=groupName, show.legend = leg)) + geom_violin(show.legend = leg) + 
      geom_boxplot(width=width, show.legend=FALSE, outlier.shape = NA) +
      scale_y_log10(breaks=c(10,100,1000), limits = c(1, maxValue)) +
      facet_grid(. ~ category, scale="fixed", labeller=labeller) + labs(x=xlab, y = ylab1) + scale_fill_manual(values=c("white", "gray"), name = "", breaks=c("Declared \nmaintainers", "Declared \nnon-maintainers"),
                                                                                                               labels=c("Declared \nmaintainers", "Declared \nnon-maintainers")) + 
      guides(fill = guide_legend(keywidth = 1, keyheight = 1.7))
    
  }
  else{
    p1 = ggplot(data1, aes(x=groupName, y=value)) + geom_violin() + 
      geom_boxplot(width=width, show.legend=FALSE) +
      scale_y_log10(breaks=c(10,100,1000), limits = c(1, maxValue)) +
      facet_wrap(. ~ category, scale="free_y", labeller=labeller) +
      labs(x=xlab, y = ylab1)
  }
  data2 = subset(newDeclaredData, category == categories[2])
  if (colored){
    p2 = ggplot(data2, aes(x=groupName, y=value, fill=groupName, show.legend = leg)) + geom_violin(show.legend = leg) + 
      geom_boxplot(width=width, show.legend=FALSE, outlier.shape = NA) +
      scale_y_log10(breaks=c(10,100,1000), limits = c(1, maxValue)) +
      facet_grid(. ~ category, scale="fixed", labeller=labeller) + labs(x=xlab, y = ylab2) + scale_fill_manual(values=c("white", "gray"), name = "", breaks=c("Declared \nmaintainers", "Declared \nnon-maintainers"),
                                                                                                               labels=c("Declared \nmaintainers", "Declared \nnon-maintainers")) + 
      guides(fill = guide_legend(keywidth = 1, keyheight = 1.7))
    
  }
  else{
    p2 = ggplot(data2, aes(x=groupName, y=value)) + geom_violin() + 
      geom_boxplot(width=width, show.legend=FALSE) +
      scale_y_log10(breaks=c(10,100,1000), limits = c(1, maxValue)) +
      facet_wrap(. ~ category, scale="free_y", labeller=labeller) +
      labs(x=xlab, y = ylab2)
  }
  
  grid.arrange(p1, p2, nrow = 1)
}
# drawViolinPlot(oracleData, ylab1="Days", ylab2="LOC", width=0.06, colored = T, leg = F)


# p = drawViolinPlot(oracleData, ylab="Days/LOC", width=0.06)
# p
# p1 = drawSimpleViolinPlot(newHitData, "Developer Commits (%)", logInfo = "", T, T, T)


drawSimpleViolinPlotNew = function(data, xlab, log=F, cutminInfo = FALSE, cutmaxInfo = FALSE, fill= c("#2C2C2C", "gray"), drawRect = T){
  
  
  
  vioplotx(data$value[data$group==1], ylog = log, 
           col = "gray", plotCentre = "line", side = "left", 
           logLab = c(1,2,5), horizontal = T,
           at = 1, wex=0.5)
  vioplotx(data$value[data$group==2], ylog = log, 
           col = "#2C2C2C", plotCentre = "line", side = "right", 
           add=T,horizontal = T,
           at = 1)
  title(xlab = xlab)
  legend("top", fill = c("gray", "#2C2C2C"), legend = c("AllHit", "AllMiss"), cex=1, bty = "n", horiz = T)
  
}

# drawSimpleViolinPlotNew(newHitData, "Developer Commits (%)")

# newHitData = getHitData(nonMaintainersData, c('commitsPerc'), c("Developers Commits"))
# p1 = drawBeanSimplePlot(newHitData, "Developer Commits (%)", logInfo = "")


# newHitData = getHitData(nonMaintainersData, c('commitsPerc'), c("Developers Commits"))
# p2 = drawSplitViolinPlot(nonMaintainersData, c('commitsPerc'), c("Developers Commits"))

# p4 = plotHitsViolin("", nonMaintainersData, c('commitsPerc'), F, F, T, newnames = c("Developers Commits"), 
#                     xlab="Developer Commidts (%)", horizontal = T)
# p = plotHitsViolin2("All-Hits", oracleData, categories, T, T, F)
# p = plotHitsViolin("All-Hits", oracleData, categories, T, TRUE, F, T)

plotHitsMissesByTech = function(title, data, dimension, log = FALSE, bean = FALSE){
  title = paste(title, "(", dimension, ")")
  hit = subset(data, doa == "hit")
  hit = toCategoryData(hit, c(dimension), 'id')
  hit$groupName = "Hit"
  hit$group = 1
  miss = subset(data, doa == "miss")
  miss = toCategoryData(miss, c(dimension), 'id')
  miss$groupName = "Miss"
  miss$group = 2
  doaData = rbind(hit, miss)
  doaData$category = rep("DOA", nrow(doaData))
  
  hit = subset(data, commit == "hit")
  hit = toCategoryData(hit, c(dimension), 'id')
  hit$groupName = "Hit"
  hit$group = 1
  miss = subset(data, commit == "miss")
  miss = toCategoryData(miss, c(dimension), 'id')
  miss$groupName = "Miss"
  miss$group = 2
  commitData = rbind(hit, miss)
  commitData$category = rep("Commit", nrow(commitData))
  
  hit = subset(data, blame == "hit")
  hit = toCategoryData(hit, c(dimension), 'id')
  hit$groupName = "Hit"
  hit$group = 1
  miss = subset(data, blame == "miss")
  miss = toCategoryData(miss, c(dimension), 'id')
  miss$groupName = "Miss"
  miss$group = 2
  blameData = rbind(hit, miss)
  blameData$category = rep("Blame", nrow(blameData))
  
  
  newHitData = rbind(doaData, commitData, blameData)
  # newHitData = merge(hitData, data, by.x = 'id', by.y = 'id')
  newHitData$groupName = factor(newHitData$groupName, c('Hit', 'Miss')) 
  newHitData$category = factor(newHitData$category, c('DOA', 'Commit', 'Blame')) 
  p = NA
  for (categ in levels(newHitData$category)){
    subAllHit = subset(subset(newHitData, groupName == "Hit"), category == categ)
    subAllMiss = subset(subset(newHitData, groupName == "Miss"), category == categ)
    w = wilcox.test(subAllHit$value, subAllMiss$value)
    cat("=====", categ,"=====\np-value: ", w$p.value, "\n====================\n\n", sep = "")
  }
  if (bean){
    if(log){
      if (length(newHitData$value[newHitData$value==0])>0)
        newHitData$value = newHitData$value+1
      p = drawBeanPlot(newHitData, title, '', logInfo = "y")
    }
    else
      p = drawBeanPlot(newHitData, title, '', logInfo = "")
  }
  
  else{
    if (log)
      p = ggplot(newHitData, aes(category, value, fill=groupName)) + geom_split_violin() + scale_y_log10()
    else 
      p = ggplot(newHitData, aes(category, value, fill=groupName)) + geom_split_violin() + title( main = title)
  }
  return(p)
}

# 
# plotHitsViolin = function(title, data, categories, log = FALSE, bean = FALSE){
#   allHit = subset(data, doa == "hit" & commit == "hit" & blame == "hit")
#   allHit = toCategoryData(allHit, categories, 'id')
#   allHit$groupName = "AllHit"
#   someHit = subset(data, doa == "miss" | commit == "miss" | blame == "miss")
#   someHit = toCategoryData(someHit, categories, 'id')
#   someHit$groupName = "SomeHit"
#   allMiss = subset(data, doa == "miss" & commit == "miss" & blame == "miss")
#   allMiss = toCategoryData(allMiss, categories, 'id')
#   allMiss$groupName = "AllMiss"
#   
#   hitData = rbind(allHit, allMiss)
#   newHitData = merge(hitData, data, by.x = 'id', by.y = 'id')
#   newHitData$groupName = factor(newHitData$groupName, c('AllHit', 'AllMiss')) 
#   
#   if (bean){
#     if(log){
#       if (length(newHitData$value[newHitData$value==0])>0)
#         newHitData$value = newHitData$value+1
#       drawBeanPlot(newHitData, title, '', logInfo = "y")
#     }
#     else
#       drawBeanPlot(newHitData, title, '', logInfo = "")
#   }
#   
#   else{
#     if (log)
#       ggplot(newHitData, aes(category, value, fill=groupName)) + geom_split_violin() + scale_y_log10()
#     else 
#       ggplot(newHitData, aes(category, value, fill=groupName),
#              main = title) + geom_split_violin()
#   }
# }
# 
# 
# nonmaintainerData = subset(oracleData, classification == "non-maintainer")
# plotHitsViolin("Non-Maintainers", nonmaintainerData, categories, TRUE, TRUE)


GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, draw_group = function(self, data, ..., draw_quantiles = NULL){
  data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
  grp <- data[1,'group']
  newdata <- plyr::arrange(transform(data, x = if(grp%%2==1) xminv else xmaxv), if(grp%%2==1) y else -y)
  newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
  newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
  if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
    stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 
                                              1))
    quantiles <- create_quantile_segment_frame(data, draw_quantiles)
    aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
    aesthetics$alpha <- rep(1, nrow(quantiles))
    both <- cbind(quantiles, aesthetics)
    quantile_grob <- GeomPath$draw_panel(both, ...)
    ggplot2:::ggname("geom_split_violin", grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
  }
  else {
    ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
  }
})

geom_split_violin <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

drawBeanPlot = function(data, title, logInfo="auto", cutminInfo = FALSE, cutmaxInfo = FALSE, what=c(0,1,1,0), 
                        ylim = NULL, fill = c("gray", "#2C2C2C","red"), xlab="", ylab="", horizontal = F, legend = "topright", mconfig=F){
  
  op<-par(no.readonly=TRUE) #this is done to save the default settings 
  par(cex.lab=1,cex.axis=1)
  
  if(!mconfig){
    b = beanplot(data$value ~ data$group*data$category, ll = 0.02,
                 main = title, side = "both", xlab=xlab, ylab=ylab,
                 col = list(fill[1], fill[2]),
                 axes=F, show.names=T,  what=what, beanlines= "median", frame.plot =T,
                 log=logInfo, overallline="median", ylim = ylim, horizontal = horizontal)#, maxwidth = 0.2, boxwex = 0.5) 
    ## Use at to change the plot position
    if (horizontal){
      axis(1)
      if (length(levels(data$category))>1)
        axis(2, at=(1:length(levels(data$category))),  labels=levels(data$category))
    }
    else{
      axis(1, at=(1:length(levels(data$category))),  labels=levels(data$category))
      axis(2)
    }
    if (horizontal)
      legend = "top"
    legend(legend, fill = c(fill[2], fill[1]),
           legend = levels(data$groupName), box.lty=0, yjust = 0.5, horiz=horizontal)
    par(op)
    return(b)
  }
  else{
    fill = c("gray","#2C2C2C")
    b = beanplot(data$value ~ data$group*data$category, ll = 0.02,
                 main = title, side = "both", xlab=xlab, ylab=ylab,
                 col = list(fill[1], c(fill[2], "black")),
                 axes=F, show.names=T,  what=what, beanlines= "median", 
                 log=logInfo, overallline="median", ylim = ylim, horizontal = horizontal, boxwex = 0.5, at=c(0.8, 1.5) )
    axis(1, at=c(0.8,1.5),  labels=levels(data$category))
    axis(2)
    legend(legend, fill = fill,
           legend = levels(data$groupName), box.lty=1, yjust = 0.5, horiz=horizontal, y.intersp =2)
    par(op)
    return(b)
  }
  
}



toCategoryData = function(data, names, idname){
  category = c()
  value = c()
  id = c()
  idNames = data[idname]
  dataSize = nrow(data)
  for (name in names){
    id = c(id, idNames)
    category = c(category, rep(name, dataSize))
    value = c(value, as.list(data[name])) 
  }
  id= unlist(id)
  category = factor(category, names)
  value = unlist(value)
  return (data.frame(id, category, value))
}