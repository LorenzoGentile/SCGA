finaliseOutput <- function(env){
  list2env(env,envir = environment())

  result$exitMessage         <- names(conditions$mainLoop[ conditions$mainLoop==T])
  result$control             <- control
  result$lastSigma           <- sigma
  result$lastX               <- x
  result$newPop              <- newPop
  result$sigma0              <- sigma0
  result$summary             <- createSummary(control,result)


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

createSummary <- function (control,result){

  suppressWarnings(summaryDf <- data.frame(yBest      = result$ybesthistory[!is.na(result$ybesthistory)],
                                           evaluations= result$evaluations,
                                           NAs        = result$NAs[!is.na(result$ybesthistory)],
                                           seed       = control$seed))
  if(control$constraint)
    summaryDf                <- cbind(summaryDf,
                                      constBest  = result$consBesthistory,
                                      cRef       = control$cRef)

return(summaryDf)
}
