anyPlot <-function (data, yLim = NULL, xLim = NULL, ylog = F, xlog = F,
                    useMinMax = T, confidenceInterval = c(0.25,0.75),plotDF=NULL,funSample=F,excludeValueHigher=Inf, themePosX =.8,themePosY =.9,errorBar=F )
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

    if(data$iterBest %>% min() <=0 & ylog){
      data$iterBest = data$iterBest - data$iterBest %>% min() + 1e-3
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
  else{
    if(!errorBar)
      h <- ggplot(plotDF, aes(x = iteration,y = mean, color = algoName)) +  geom_ribbon(aes(ymin = min, ymax = max,
                                                                                            fill = algoName),linetype=2, alpha = 0.1, show.legend = T)
    else{
      iterSamp <- seq( min(plotDF$iteration),max(plotDF$iteration),length.out = 10)
      iterSamp <- sapply(iterSamp, function(i) plotDF$iteration[which.min((plotDF$iteration-i)^2)])
      redu <- plotDF %>% filter(iteration %in% iterSamp)
      h <- ggplot(plotDF, aes(x = iteration,y = mean, color = algoName)) +
        geom_errorbar(redu,mapping=aes(ymin=min, ymax=max), width=.2,
                      position=position_dodge(width = 1000))
    }
  }

  h <- h + geom_line()
  if (ylog) {
    h <- h + scale_y_log10()
  }
  if (xlog) {
    h <- h + scale_x_log10()
  }
  h <- h + coord_cartesian(ylim = yLim, xlim = xLim)
  h <- h + scale_colour_manual(name = "Algorithm:",
                               values = c("#0000FF" ,"#228B22", "#FF0000" ,"#000000", "#BF3EFF" ,"#FF8C00" ,"#00EEEE" ,"#FF1493" ,"#8B4500" ,"#00EE00" ,"#1C86EE", "#C1CDCD", "#EEC900",
                                          "#CD3700" ,"#FFBBFF" ,"#0000FF" ,"#228B22" ,"#FF0000",
                                          "#000000" ,"#BF3EFF" ,"#FF8C00", "#00EEEE" ,"#FF1493", "#8B4500", "#00EE00" ,"#1C86EE" ,"#C1CDCD" ,"#EEC900" ,"#CD3700", "#FFBBFF"))

  h <- h + scale_fill_manual(name = "Algorithm:",
                             values = c("#0000FF" ,"#228B22", "#FF0000" ,"#000000", "#BF3EFF" ,"#FF8C00" ,"#00EEEE" ,"#FF1493" ,"#8B4500" ,"#00EE00" ,"#1C86EE", "#C1CDCD", "#EEC900",
                                        "#CD3700" ,"#FFBBFF" ,"#0000FF" ,"#228B22" ,"#FF0000",
                                        "#000000" ,"#BF3EFF" ,"#FF8C00", "#00EEEE" ,"#FF1493", "#8B4500", "#00EE00" ,"#1C86EE" ,"#C1CDCD" ,"#EEC900" ,"#CD3700", "#FFBBFF"))

  h <- h+ theme_minimal()+xlab("Evaluations")+ylab("Objective function")+ggtitle("Best found solution history ")+theme(text = element_text(size=20),
                                                                                                                       legend.position = c(themePosX, themePosY)
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
functionReduce <- function(data,sampling=100){
  dataDivided = split(data,interaction(data$algoName,data$seed))
  dataDivided = dataDivided[!sapply(dataDivided, is.empty)]
  nomi= names(dataDivided)
  reduce <- function(data,nome){
    if(any(is.na(data$iteration)))
      return(NULL)
    # browser()

    name = unlist(strsplit(nome, split="\\."))
    evals=seq(min(data$iteration),max(data$iteration),length.out =  sampling)
    samp=sapply(evals,sampleData,data$iterBest,data$iteration)
    df=as.data.frame(cbind(iterBest=samp,iteration=evals,algoName=name[1],replication=name[2]))
    return(df)
  }
  browser()
  df=mapply(reduce, dataDivided,nomi,SIMPLIFY = F)
  df = df[!sapply(df, is.null)]
  df=dplyr::bind_rows(df, .id = "column_label")
  df=df[,2:ncol(df)]

  df$iteration <- df$iteration %>% as.numeric
  df$iterBest  <- df$iterBest %>% as.numeric
  return(df)
}
functionReduce <- function(data,sampling=100){
data=data[!is.na(data$iteration),]
  df=list(NA,length(unique(data$algoName))*length(unique(data$replication)))
  ind=1
  evals=seq(1,5e4,100)
  for (algo in unique(data$algoName)){
    subData <- data %>% filter(algoName==algo)
    evals=seq(min(subData$iteration),max(subData$iteration),length.out =  sampling)
    for (replica in  unique(subData$replication)) {

      subsubData <- subData %>% filter(replication==replica)

      samp=sapply(evals, sampleData,subsubData$iterBest,subsubData$iteration)
      df[[ind]]=as.data.frame(cbind(iterBest=samp,iteration=evals,algoName=algo,replication=replica))

      ind=ind+1
    }
  }

  df=dplyr::bind_rows(df, .id = "column_label")
  df=df[,2:ncol(df)]

  df$iteration <- df$iteration %>% as.numeric
  df$iterBest <-  df$iterBest %>% as.numeric
  return(df)
}
