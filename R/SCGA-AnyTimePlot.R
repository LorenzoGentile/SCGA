anyPlot <-function (data, yLim = NULL, xLim = NULL, ylog = F, xlog = F,
                    useMinMax = T, confidenceInterval = c(0.75,0.2),plotDF=NULL,funSample=F,excludeValueHigher=Inf)
{
  retPlotDf=F
  if(is.null(plotDF)){
    retPlotDf=T
    requireNamespace("ggplot2")
    dfNames <- c("algoName", "replication", "iteration", "iterBest")
    if (!all(dfNames %in% names(data))) {
      stop("Wrong df names were provided")
    }


    if(funSample)
      data= functionReduce(data)
    toRemove=data %>% group_by(algoName,replication) %>%  filter( iteration==max(iteration)) %>% summarise(toRemove=iterBest>excludeValueHigher)

    toRemove=toRemove %>% filter(toRemove==T)
    for (toRm in seq(1,length.out = nrow(toRemove),by = 1)) {
      data = data[- which(c(data$algoName==toRemove$algoName[toRm] & data$replication==toRemove$replication[toRm])), ]
    }

    if (is.null(confidenceInterval))
      plotDF <-  data %>% group_by(algoName,iteration) %>% mutate(mean=mean(iterBest))

    else
      plotDF <-  data %>% group_by(algoName,iteration) %>% mutate(mean=mean(iterBest),
                                                                  sd=sd(iterBest),
                                                                  min=quantile(iterBest,confidenceInterval[1]),
                                                                  max=quantile(iterBest,confidenceInterval[2])
      )
  }
  if (is.null(confidenceInterval))
  h <- ggplot(plotDF, aes(x = iteration,y = mean, color = algoName))
  else
  h <- ggplot(plotDF, aes(x = iteration,y = mean, color = algoName)) +  geom_ribbon(aes(ymin = min, ymax = max,
                                                                                        fill = algoName),linetype=2, alpha = 0.2, show.legend = T)

  h <- h + geom_line()
  if (ylog) {
    h <- h + scale_y_log10()
  }
  if (xlog) {
    h <- h + scale_x_log10()
  }
  h <- h + coord_cartesian(ylim = yLim, xlim = xLim)
  h <- h + scale_colour_manual(name = "Algorithm:",
                               values = c("blue"     ,   "#33a02c"   ,  "red3"     ,   "gray1" ,     "magenta2" ,   "gold"   ,
                                          "darkorange4" ,"darkorange1", "cadetblue2" , "plum1"    ,   "seagreen1"  , "darkorchid3" ,"yellow4","green","red"))

  h <- h + scale_fill_manual(name = "Algorithm:",
                             values = c("blue"     ,   "#33a02c"   ,  "red3"     ,   "gray1" ,     "magenta2" ,   "gold"   ,
                                        "darkorange4" ,"darkorange1", "cadetblue2" , "plum1"    ,   "seagreen1"  , "darkorchid3" ,"yellow4","green","red"))

  h <- h+ theme_minimal()+xlab("Evaluations")+ylab("Objective function")+ggtitle("Anytime Performance plot")+theme(text = element_text(size=20),
                                                                                                                   legend.position = c(0.7, 0.7)
  )
  if(retPlotDf)
    return(list( h,plotDF))
  else
    return(h)
}
sampleData=function(iter,iterBest,iteration){iterBest[which.min(abs(iteration-iter))]}

functionReduce <- function(data){

  df=list(NA,length(unique(data$algoName))*length(unique(data$replication)))
  ind=1
  evals=seq(1,5e4,100)
  for (algo in unique(data$algoName)){
    subData <- data %>% filter(algoName==algo)
    for (replica in  unique(subData$replication)) {
      subsubData <- subData %>% filter(replication==replica)
      samp=sapply(evals, sampleData,subsubData$iterBest,subsubData$iteration)
      df[[ind]]=as.data.frame(cbind(iterBest=samp,iteration=evals,algoName=algo,replication=replica))

      ind=ind+1
    }
  }

  df=dplyr::bind_rows(df, .id = "column_label")
  df=df[,2:ncol(df)]

  df$iteration <- as.numeric(levels(df$iteration))[df$iteration]
  df$iterBest <-  df$iterBest %>% as.numeric
  return(df)
}
