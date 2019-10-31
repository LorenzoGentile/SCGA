# Plot <- function(y, media, stalling) {
# y<- y[!is.na(y)]
#   if(length(y)>=10)
#     toplot=which(y<=min(y,na.rm = T)*1.1)
#   else
#     toplot=1:length(y)
#
#   if (any(c(y[toplot], media[toplot]) <= 0))
#     matplot(toplot,cbind(y[toplot], media[toplot]), type = c("l","l"),xlab = "Generations",ylab = "Objective Function")
#   else
#     matplot(toplot,cbind(y[toplot], media[toplot]), type = c("l","l"), log = "y",xlab = "Generations",ylab = "Objective Function")
#   abline(a = 0, b = 0, col = "lightgray")
#
#   text(
#     x = max(c(toplot)),
#     y = max(c(media[toplot])),
#     pos = 2,
#     lab = paste("Iter from improvement", stalling, "|| Value" , min(y))
#   )
# }

Plot <- function (y,yT,stalling,eval){
  y=y[seq(1,to=min(which( is.na(y))))-1]
  ydf = data.frame(cbind(y,yT,eval))
  colnames(ydf) = c("y","yT","eval")
  g = (ggplot(ydf, aes( x = eval)) +
         geom_line(mapping= aes(y= yT,colour="red"),show.legend=FALSE) +
         geom_point(mapping= aes(y= yT,colour="red"),show.legend=FALSE)+
         geom_line(mapping= aes(y= y,colour="black"),show.legend=FALSE) +
         geom_point(mapping= aes(y= y,colour="black"),show.legend=FALSE)+
         theme_minimal()+
         annotate(geom="text", x=mean(eval), y=max(c(y,yT)), label=paste("Iter from improvement", stalling,
                                                                         "|| Min " , prettyNum(min(y),digits=4),"|| Mean " , prettyNum(min(yT),digits=4)),
                  color="red")) + xlab("Objective Function Evaluations")

  if(all(c(y,yT)>0))
    g = g + scale_y_log10()

 return(g)

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

PlotPopulation <- function(toCompare, generations,path,printIt,subpath="/opt-gen-"){

  if(!is.list(toCompare[[1]])){
    toCompare <- sapply(toCompare, function(x){
      out= rep(NA, length(feature))
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

    g <- suppressWarnings( ggplot(toCompareDF,aes(x= value,fill=variable))  +geom_histogram()+facet_wrap(~variable, scales="free"))

    if(printIt)
      ggsave(paste0(path,"/opt-gen-",generations,".png"), width = 40, height = 20, units = "cm",g)
  } else{
    subpath = paste0(subpath,"Pop-",1:length(toCompare),"-")
    g <- mapply(plotPopulation,toCompare=toCompare, subpath=subpath,MoreArgs = list(generations=generations,path=path,printIt=printIt),SIMPLIFY = FALSE)
  }
  return(g)
}
plotFitness <- function(y,constList,fitness){

  df = data_frame(y=y,constraint=constList$constraint,fitness=fitness,feas=constList$resFeas )
  # scalefactotr=max(constList$constraintconstraint)/max(y)
  # print(ggplot(df,mapping=aes(x=fitness))+geom_point(aes(y=constraint,shape=as.factor(2),size=1.8,colour=feas))+geom_point(aes(y=y*scalefactotr,colour=feas,shape=as.factor(1),size=1.8)) + scale_y_continuous(sec.axis = sec_axis(~ ./scalefactotr)))
  g <- (ggplot(df,mapping=aes(x=fitness))+geom_point(aes(y=y,colour=feas,shape=as.factor(1),size=1.8))+scale_y_log10())+theme_minimal() + theme(legend.position="top") +guides(shape = FALSE, size = FALSE)
  return(g)
}
