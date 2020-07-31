#' This function returns the anytime performance plot and the summary dataframe out of
#'  Control is a list of the settings:
#' @param convergence Stopping criterion: absolute difference between the current best and
#' the known minimum
#' @param cpus numeric. indicatig number of cores over which parallelise
#' @param creatCandFun function. See \code{\link{createCandidate}}
#' @param createMutFun function. See \code{\link{createMutFun}}
#' @param crossFun function. See \code{\link{crossFun}}
#' @param dontChangeCross numeric vector. Feature number that not undergo to Crossover
#' @param dontChangeMut numeric vector. Feature number that not undergo to Mutation
#' @param elitism numeric. Number of candidates to preserve to the next population. Default is size / 10
#' @param evaluatePopDF function. See \code{\link{evaluatePopDF}}

anyPlot <-function (data, yLim = NULL, xLim = NULL, ylog = F, xlog = F,retPlotDf=F,
                    useMinMax = T, confidenceInterval = c(0.25,0.75),plotDF=NULL,funSample=F,excludeValueHigher=Inf,
                    themePosX =.8,themePosY =.9,errorBar=F,scaleManual=F )
{

  cat("\nAnytime plot started\n")
  if(is.null(plotDF)){

    requireNamespace("ggplot2")

    dfNames <- c("algoName", "seed", "iteration", "iterBest")

    if(!is.null(data$yBest))       data$iterBest <- data$yBest
    if(!is.null(data$evaluations)) data$iteration <- data$evaluations
    if (!all(dfNames %in% names(data))) {
      stop("Wrong df names were provided")
    }

    if(funSample){
      cat("Computing common values\n")
      data= functionReduce(data)
    }


    toRemove=data %>% group_by(algoName,seed) %>%  filter( iteration==max(iteration)) %>% summarise(toRemove=iterBest>excludeValueHigher)

    toRemove=toRemove %>% filter(toRemove==T)
    for (toRm in seq(1,length.out = nrow(toRemove),by = 1)) {
      data = data[- which(c(data$algoName==toRemove$algoName[toRm] & data$seed==toRemove$seed[toRm])), ]
    }

    if(data$iterBest %>% min() <=0 & ylog){
      data$iterBest = data$iterBest - data$iterBest %>% min() + 1e-3
    }
    if (is.null(confidenceInterval))
      plotDF <-  data %>% group_by(algoName,iteration) %>% mutate(mean=mean(iterBest))

    else if(length(confidenceInterval)==2)
      plotDF <-  data %>% group_by(algoName,iteration) %>% mutate(mean=mean(iterBest),
                                                                  sd=sd(iterBest),
                                                                  min=quantile(iterBest,confidenceInterval[1]),
                                                                  max=quantile(iterBest,confidenceInterval[2])
      )
    else
      plotDF <-  data %>% group_by(algoName,iteration) %>% mutate(mean=quantile(iterBest,confidenceInterval[2]),
                                                                  sd=sd(iterBest),
                                                                  min=quantile(iterBest,confidenceInterval[1]),
                                                                  max=quantile(iterBest,confidenceInterval[3]))


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


  h <- h + labs(colour="Algorithm:",fill="Algorithm:")

  if(scaleManual){
  h <- h + scale_colour_manual(name = "Algorithm:",
                               values = c("#0000FF" ,"#228B22", "#FF0000" ,"#000000", "#BF3EFF" ,"#FF8C00" ,"#00EEEE" ,"#FF1493" ,"#8B4500" ,"#00EE00" ,"#1C86EE", "#C1CDCD", "#EEC900",
                                          "#CD3700" ,"#FFBBFF" ,"#0000FF" ,"#228B22" ,"#FF0000",
                                          "#000000" ,"#BF3EFF" ,"#FF8C00", "#00EEEE" ,"#FF1493", "#8B4500", "#00EE00" ,"#1C86EE" ,"#C1CDCD" ,"#EEC900" ,"#CD3700", "#FFBBFF"))

  h <- h + scale_fill_manual(name = "Algorithm:",
                             values = c("#0000FF" ,"#228B22", "#FF0000" ,"#000000", "#BF3EFF" ,"#FF8C00" ,"#00EEEE" ,"#FF1493" ,"#8B4500" ,"#00EE00" ,"#1C86EE", "#C1CDCD", "#EEC900",
                                        "#CD3700" ,"#FFBBFF" ,"#0000FF" ,"#228B22" ,"#FF0000",
                                        "#000000" ,"#BF3EFF" ,"#FF8C00", "#00EEEE" ,"#FF1493", "#8B4500", "#00EE00" ,"#1C86EE" ,"#C1CDCD" ,"#EEC900" ,"#CD3700", "#FFBBFF"))

  }
  h <- h+ theme_minimal()+xlab("Evaluations")+ylab("Objective function")+ggtitle("Best found solution history ")+theme(text = element_text(size=20),
                                                                                                                       legend.position = c(themePosX, themePosY)

  )

  if(!is.null(data$problemName)) h <- h + labs(title=data$problemName[1],subtitle = "Best found solution history ")
  if(retPlotDf)
    return(list(plot=h,data=plotDF))
  else
    return(h)
}



functionReduce <- function(data,sampling=1000){

data=data[!is.na(data$iteration),]
  df   =list(NA,length(unique(data$algoName))*length(unique(data$seed)))
  ind  =1
  evals=seq(min(data$iteration),max(data$iteration),length.out =  sampling)

  for (algo in unique(data$algoName)){
    subData <- data %>% filter(algoName==algo)
    # evals=seq(min(subData$iteration),max(subData$iteration),length.out =  sampling)
    for (replica in  unique(subData$seed)) {

      subsubData <- subData %>% filter(seed==replica)

      samp=sapply(evals, sampleData,subsubData$iterBest,subsubData$iteration)
      df[[ind]]=as.data.frame(cbind(iterBest=samp,iteration=evals,algoName=algo,seed=replica))

      ind=ind+1
    }
  }

  df=dplyr::bind_rows(df, .id = "column_label")
  df=df[,2:ncol(df)]

  df$iteration <- df$iteration %>% as.numeric
  df$iterBest <-  df$iterBest %>% as.numeric
  return(df)
}
sampleData=function(iter,iterBest,iteration){iterBest[which.min(abs(iteration-iter))]}
functionReduce <- function(data,sampling=1000){
  data=data[!is.na(data$iteration),]
  # data  <- data %>% filter(iteration<200)
  evals <- seq(min(data$iteration),max(data$iteration),length.out =  sampling)
  data  <- data %>% group_by(seed,algoName) %>% dplyr:::group_split()
  data  <- sapply(data,newValues,evals,simplify = F)

  bind_rows(data)

}
newValues <- function(dataNew,evals){
  iterBest <- sapply(evals, sampleData,dataNew$iterBest,dataNew$iteration)
  dataOut  <- dataNew[1,setdiff(colnames(dataNew),c("iterBest","iteration"))]
  dataOut[1:length(iterBest),] <-  dataOut[1,]
  dataOut$iterBest <- iterBest
  dataOut$iteration <- evals
  dataOut
}
