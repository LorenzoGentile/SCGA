finaliseOutput <- function(env){

  list2env(env,envir=environment())
  suppressWarnings(summaryDf <- data.frame(yBest      =result$ybesthistory[!is.na(result$ybesthistory)],
                                           evaluations=result$evaluations,
                                           NAs        =result$NAs[!is.na(result$ybesthistory)],
                                           seed       =control$seed))
  if(control$constraint)
    summaryDf                <- cbind(summaryDf,
                       constBest  = result$consBesthistory,
                       cRef       = control$cRef)




  result$evaluations         <- evaluations
  result$exitMessage         <- names(conditions$mainLoop[ conditions$mainLoop==T])
  result$control             <- control
  result$lastSigma           <- sigma
  result$lastX               <- x
  result$newPop              <- newPop
  result$sigma0              <- sigma0
  result$summary             <- summaryDf


  if(control$constraint){
    result$xbest             <- bestFeasible$x
    result$ybest             <- bestFeasible$y
    result$consBest          <- consBest
  }  else {
    result$xbest             <- x[which(yForResults==best)]
    result$ybest             <- best
  }


  return(result)
}
