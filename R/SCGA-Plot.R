Plot <- function(y, media, stalling) {
y<- y[!is.na(y)]
  if(length(y)>=10)
    toplot=which(y<=min(y,na.rm = T)*1.1)
  else
    toplot=1:length(y)

  if (any(c(y[toplot], media[toplot]) <= 0))
    matplot(toplot,cbind(y[toplot], media[toplot]), type = c("l","l"),xlab = "Generations",ylab = "Objective Function")
  else
    matplot(toplot,cbind(y[toplot], media[toplot]), type = c("l","l"), log = "y",xlab = "Generations",ylab = "Objective Function")
  abline(a = 0, b = 0, col = "lightgray")

  text(
    x = max(c(toplot)),
    y = max(c(media[toplot])),
    pos = 2,
    lab = paste("Iter from improvement", stalling, "|| Value" , min(y))
  )
}


Plotsigmas <- function(toCompare, generations, path,printIt,sigma0,subpath="/opt-gen-sigma-") {

  if(!is.list(toCompare)){
    toCompare= t(t(toCompare) / sigma0)
  toCompareDF=NULL
  for (i in 1:(ncol(toCompare)-8)) {
    toCompareDF = rbind(toCompareDF,cbind(toCompare[,i],i))
  }

  colnames(toCompareDF) = c("value", "variable")
  toCompareDF           = as.data.frame(toCompareDF)
  toCompareDF$variable  = as.factor(toCompareDF$variable)
  g <- ggplot(toCompareDF,aes(x= value,fill=variable))  +geom_histogram()+facet_wrap(~variable, scales="free")

  if(printIt)
    ggsave(paste0(path,subpath,generations,".png"), width = 40, height = 20, units = "cm",g)
  } else{
    subpath = paste0(subpath,"Pop-",1:length(toCompare),"-")
    g <- mapply(Plotsigmas,toCompare=toCompare, subpath=subpath,MoreArgs = list(generations=generations,path=path,printIt=printIt,sigma0=sigma0),SIMPLIFY = FALSE)
  }

  return(g)
}

PlotPopulation <- function(newPop, generations,path,printIt,subpath="/opt-gen-"){
  if(!is.list(toCompare[[1]])){
  toCompare <- sapply(newPop, function(x){
    out= rep(NA, 60)
    out[x[,"feature"]]=x[,"value"]
    out
  }

  )
  toCompare = t(toCompare)
  toCompareDF=NULL
  for (i in 1:ncol(toCompare)) {
    toCompareDF = rbind(toCompareDF,cbind(toCompare[,i],i))
  }

  colnames(toCompareDF) = c("value", "variable")
  toCompareDF           = as.data.frame(toCompareDF)
  toCompareDF$variable  = as.factor(toCompareDF$variable)

  g <- ggplot(toCompareDF,aes(x= value,fill=variable))  +geom_histogram()+facet_wrap(~variable, scales="free")

  if(printIt)
    ggsave(paste0(path,"/opt-gen-",generations,".png"), width = 40, height = 20, units = "cm",g)
  } else{
    subpath = paste0(subpath,"Pop-",1:length(toCompare),"-")
    g <- mapply(plotPopulation,toCompare=toCompare, subpath=subpath,MoreArgs = list(generations=generations,path=path,printIt=printIt),SIMPLIFY = FALSE)
  }
  return(g)

}
