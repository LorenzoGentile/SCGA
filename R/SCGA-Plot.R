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


Plot <- function (y,yT,stalling,eval,limit=Inf){
  y=y[seq(1,to=min(which( is.na(y))))-1]
  ydf = data.frame(cbind(y,yT,eval))
  colnames(ydf) = c("y","yT","eval")
  if(any(ydf$y <=limit))
    ydf <- ydf %>% filter(y<=limit)
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
    ranges  <- sapply(feature, function(x) return(c(max(x$bound()),min(x$bound()))),simplify = T)
    toCompareDF$xmin=ranges[1,toCompareDF$variable]
    toCompareDF$xmax=ranges[2,toCompareDF$variable]

    g <- suppressWarnings( ggplot(toCompareDF,aes(x= value,fill=variable))  +geom_histogram()+facet_wrap(~variable, scales="free")+
                             geom_blank(aes(x = xmin)) +
                             geom_blank(aes(x = xmax))+
                             theme_minimal()+
                             theme(legend.position = 'none')#+
                           # ggtitle("Best found solutions")
                             )

    if(printIt)
      ggsave(paste0(path,"/opt-gen-",generations,".pdf"), width = 40, height = 20, units = "cm",g)
  } else{
    subpath = paste0(subpath,"Pop-",1:length(toCompare),"-")
    g <- mapply(plotPopulation,toCompare=toCompare, subpath=subpath,MoreArgs = list(generations=generations,path=path,printIt=printIt),SIMPLIFY = FALSE)
  }
  return(g)
}
PlotPopulationBest <- function(toCompare, generations,path,printIt,subpath="/opt-gen-",toCompBest,algoNames=NULL){


    makeDataFrame <- function(toCompare,algo){
    if(is.list(toCompare)){
      toCompare <- sapply(toCompare, function(x){
        out= rep(NA, length(feature))
        out[x[,"feature"]]=x[,"value"]
        out
      }

      )
      toCompare = t(toCompare)
    }
    toCompareDF=NULL
    for (i in 1:ncol(toCompare)) {
      toCompareDF = rbind(toCompareDF,cbind(toCompare[,i],i,algo))
    }

    colnames(toCompareDF) = c("value", "variable","algoName")
    toCompareDF           = as.data.frame(toCompareDF)
    toCompareDF$value  = as.numeric(levels(toCompareDF$value))[toCompareDF$value]
    toCompareDF$variable  = as.factor(toCompareDF$variable)
    toCompareDF$algoName  = as.factor(toCompareDF$algoName)
    ranges  <- sapply(feature, function(x) return(c(max(x$bound()),min(x$bound()))),simplify = T)
    toCompareDF$xmin=ranges[1,toCompareDF$variable]
    toCompareDF$xmax=ranges[2,toCompareDF$variable]
toCompareDF
    }
    if(!is.list(toCompare[[1]])){
      if(is.null(algoNames))
        algoNames="unknown"
      toCompareDF <-   makeDataFrame(toCompare,algoNames)
    }

    else{
      if(is.null(algoNames))
        algoNames=paste0("unknown-",1:length(toCompare))
      toCompareDF <-   mapply(makeDataFrame,toCompare,algoNames,SIMPLIFY = F)
      toCompareDF <-  bind_rows(toCompareDF, .id = "column_label")
      toCompareDF <- toCompareDF[,2:ncol(toCompareDF)]
    }
    # toCompareDF$value  = as.numeric(levels(toCompareDF$value))[toCompareDF$value]
    toCompareDF$variable  = factor(toCompareDF$variable,levels = min(as.numeric(toCompareDF$variable)):max(as.numeric(toCompareDF$variable)))
    toCompBest$variable  = factor(toCompBest$variable,levels = min(as.numeric(toCompBest$variable)):max(as.numeric(toCompBest$variable)))

    toCompareDF$algoName  = as.factor(toCompareDF$algoName)
    # ordertoCompareDF=order(levels(toCompareDF$variable) %>% as.numeric())
    # levels(toCompBest$variable) = levels(toCompareDF$variable)[order(levels(toCompareDF$variable) %>% as.numeric())]
    # levels(toCompareDF$variable) = levels(toCompareDF$variable)[order(levels(toCompareDF$variable) %>% as.numeric())]
    # toCompareDF$variable = match(toCompareDF$variable,ordertoCompareDF)
    # toCompBest$variable =  match(toCompBest$variable,ordertoCompareDF)


if(length(algoNames)>1)
    g <- toCompareDF %>%
      ggplot( aes(x=value, fill=algoName)) +
      geom_histogram(  alpha=0.6, position = 'identity') +
      labs(fill="") +
      facet_wrap(~variable, scales="free")+
      geom_vline(data=toCompBest,aes(xintercept=toCompBest$value), linetype="dashed", color = "black") +
      geom_blank(aes(x = xmin)) +
      geom_blank(aes(x = xmax))+
      theme_minimal()+
      theme(legend.position = 'top')+
      ggtitle("Best found solutions",subtitle = "Dotted black line indicates the optimum value")
else




    g <- suppressWarnings( ggplot(toCompareDF,aes(x= value,fill=variable))  +geom_histogram(position = 'identity')+facet_wrap(~variable, scales="free")+ labs(fill="") +
                             geom_vline(data=toCompBest,aes(xintercept=toCompBest$value), linetype="dashed", color = "black") +
                             geom_blank(aes(x = xmin)) +
                             geom_blank(aes(x = xmax))+
                             theme_minimal()+
                             theme(legend.position = 'none')+
                              ggtitle("Best found solutions",subtitle = "Dotted black line indicates the optimum value")
    )
    if(printIt)
      ggsave(paste0(path,"/opt-gen-",generations,".pdf"), width = 40, height = 20, units = "cm",g)


  return(g)
}
PlotXBest<- function(toCompare, generations="post",path,printIt,subpath="/opt-gen-",evaluations=NULL,evalBest,types=NULL,variablesNames=NULL,variablesOrder=NULL){

  if(!is.list(toCompare[[1]])){
    if(is.list(toCompare)){
    toCompare <- sapply(toCompare, function(x){

      out= rep(NA, length(feature))
      out[x[,"feature"]]=x[,"value"]
      out
    }

    )
    toCompare = t(toCompare)
    }
    if(is.null(evaluations))
      evaluations=1:nrow(toCompare)
    toCompareDF=NULL
    for (i in 1:ncol(toCompare)) {
      toCompareDF = rbind(toCompareDF,cbind(toCompare[,i],i,evaluations))
    }

    colnames(toCompareDF) = c("value", "variable","evaluations")
    toCompareDF           = as.data.frame(toCompareDF)
    if(!is.null(types))
      toCompareDF$types      = types[toCompareDF$variable %>% as.numeric] %>% as.factor()

    toCompareDF$variable  = as.factor(toCompareDF$variable)

    ranges  <- sapply(feature, function(x) return(c(max(x$bound()),min(x$bound()))),simplify = T)
    if(!is.null(variablesOrder))
    ranges = ranges[,variablesOrder]

    toCompareDF =toCompareDF[!is.na(toCompareDF$value),]
    toCompareDF$xmin=ranges[1,toCompareDF$variable]
    toCompareDF$xmax=ranges[2,toCompareDF$variable]
    toCompBest <- toCompareDF %>% filter(evaluations==evaluations[[evalBest]])



    if(is.null(types))
    g <- suppressWarnings( ggplot(toCompareDF,aes(x= evaluations,y=value,color=variable))  +geom_point()+
                             geom_hline(data=toCompBest,aes(yintercept=toCompBest$value), linetype="dashed", color = "black") +
                             facet_wrap(~variable, scales="free_y")+

                             geom_blank(aes(y = xmin)) +
                             geom_blank(aes(y = xmax))+
                             theme_minimal()+
                             theme(legend.position = 'none', axis.text.x = element_text(angle = 30, hjust = 1)) +
                            scale_x_continuous( labels = scales::scientific )+xlab("Evaluations")+

                            ggtitle("Best found solutions history",subtitle = "Dotted black line indicates the optimum value")
    )
    else
      g <- suppressWarnings( ggplot(toCompareDF,aes(x= evaluations,y=value,color=types))  +geom_point()+
                               geom_hline(data=toCompBest,aes(yintercept=toCompBest$value), linetype="dashed", color = "black") +
                               facet_wrap(~variable, scales="free_y")+

                               geom_blank(aes(y = xmin)) +
                               geom_blank(aes(y = xmax))+
                               theme_minimal()+
                               theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1)) +
                               scale_x_continuous( labels = scales::scientific )+

                               ggtitle("Best found solutions history",subtitle = "Dotted black line indicates the optimum value")
      )
    if(printIt)
      ggsave(paste0(path,"/opt-gen-PopHistBest-",generations,".pdf"), width = 40, height = 20, units = "cm",g)
  } else{
    subpath = paste0(subpath,"PopHistBest-",1:length(toCompare),"-")
    g <- mapply(plotPopulation,toCompare=toCompare, subpath=subpath,MoreArgs = list(generations=generations,path=path,printIt=printIt),SIMPLIFY = FALSE)
  }
  return(list(g,toCompBest))
}
plotFitness <- function(y,constList,fitness){

  df = data_frame(y=y,constraint=constList$constraint,fitness=fitness,feas=constList$resFeas )
  # scalefactotr=max(constList$constraintconstraint)/max(y)
  # print(ggplot(df,mapping=aes(x=fitness))+geom_point(aes(y=constraint,shape=as.factor(2),size=1.8,colour=feas))+geom_point(aes(y=y*scalefactotr,colour=feas,shape=as.factor(1),size=1.8)) + scale_y_continuous(sec.axis = sec_axis(~ ./scalefactotr)))
  g <- (ggplot(df,mapping=aes(x=fitness))+geom_point(aes(y=y,colour=feas,shape=as.factor(1),size=1.8))+scale_y_log10())+theme_minimal() + theme(legend.position="top") +guides(shape = FALSE, size = FALSE)
  return(g)
}


figSave <-  function(g,file=Sys.time()) ggsave(paste0(file,".pdf"), width = 10.521, height = 7.443, units = "in",g)
saveMorePlot <- function(g=NULL,fileName=NULL){
  if(is.null(g))
    stop("Provide list of ggplot")
  if(is.null(fileName))
    stop("Provide name of the file")
  pdf(fileName)
  invisible(lapply(g, print))
  dev.off()
}

