require(beanplot)
require(vioplot)
require(devtools)
require(digest)

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

drawBeanPlot2 = function(data, title, xlab, logInfo="auto", cutminInfo = FALSE, cutmaxInfo = FALSE){
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
         legend = unique(data$group), box.lty=0)
  
  
}
plotMissViolinPlot = function(title, data, categories, log = FALSE, xlab='', ylab='', newnames=c(), legend="topright",
                              horizontal = F, mconfig=F){
  maintainersTemp = subset(data, classification == "maintainer")
  numMain = nrow(maintainersTemp)
  maintainersTemp = toCategoryData(maintainersTemp, categories, 'id')
  maintainersTemp$groupName = "Declared \nmaintainers"
  maintainersTemp$group = 1
  
  nonMaintainersTemp = subset(data, classification == "non-maintainer")
  numNonMain = nrow(nonMaintainersTemp)
  nonMaintainersTemp = toCategoryData(nonMaintainersTemp, categories, 'id')
  nonMaintainersTemp$groupName = "Declared \nnon-Maintainers"
  nonMaintainersTemp$group = 2
  
  cat("|maintainers| = ", numMain, " |non-maintainers| = ", numNonMain, "\n")
  
  
  hitData = rbind(maintainersTemp, nonMaintainersTemp)
  newHitData = merge(hitData, data, by.x = 'id', by.y = 'id')
  newHitData$groupName = factor(newHitData$groupName, c("Declared \nmaintainers", 'Declared \nnon-Maintainers')) 
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
  
  if(log){
    if (length(newHitData$value[newHitData$value==0])>0)
      newHitData$value = newHitData$value+1
    p = drawBeanPlot(newHitData, title, '', logInfo = "y", ylim, fill = fill, xlab=xlab, ylab=ylab, legend=legend, horizontal=horizontal, mconfig=mconfig)
  }
  else
    p = drawBeanPlot(newHitData, title, '', logInfo = "", ylim, fill = fill, xlab=xlab, ylab=ylab, legend=legend, horizontal=horizontal, mconfig=mconfig)
  
  return(p)
}

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
      p = drawBeanPlot2(newHitData, title, '', logInfo = "y")
    }
    else
      p = drawBeanPlot2(newHitData, title, '', logInfo = "")
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

plotHitsViolin = function(title, data, categories, log = FALSE, bean = FALSE, new = FALSE, 
                          printBoxplot = FALSE, ylim = NULL, xlab='', ylab='', newnames=c(),
                          horizontal = F, fill = c("gray", "#2C2C2C")){
  allMiss = subset(data, doa == "miss" & commit == "miss" & blame == "miss")
  allMiss = toCategoryData(allMiss, categories, 'id')
  allMiss$groupName = "AllMiss"
  allMiss$group = 1
  allHit = subset(data, doa == "hit" & commit == "hit" & blame == "hit")
  allHit = toCategoryData(allHit, categories, 'id')
  allHit$groupName = "AllHit"
  allHit$group = 2
  someHit = subset(data, doa == "miss" | commit == "miss" | blame == "miss")
  someHit = toCategoryData(someHit, categories, 'id')
  someHit$groupName = "SomeHit"
  someHit$group = 3
  
  hitData = rbind(allHit, allMiss)
  newHitData = merge(hitData, data, by.x = 'id', by.y = 'id')
  newHitData$groupName = factor(newHitData$groupName, c('AllHit', 'AllMiss')) 
  
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
  if (new){
    if(log){
      if (length(newHitData$value[newHitData$value==0])>0)
        newHitData$value = newHitData$value+1
      p = drawBeanPlot2(newHitData, title, '', logInfo = "y")
    }
    else
      p = drawBeanPlot2(newHitData, title, '', logInfo = "")
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
    }
  }
  if (printBoxplot){
    
    boxplot(value~category, data = subset(newHitData, groupName=="AllHit"), main ="AllHit", log = "y")
    boxplot(value~category, data = subset(newHitData, groupName=="AllMiss"), main ="AllMiss", log = "y")
  }
  
  return(p)
}
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
                 axes=F, show.names=T,  what=what, beanlines= "median", 
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
    legend(legend, fill = fill,
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

## Format functions

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
}