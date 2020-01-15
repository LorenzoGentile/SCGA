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
    ind                      <- which.min(result$ybesthistory)
    result$xbest             <- result$xbesthistory[ind]
    result$ybest             <- result$ybesthistory[ind]
  }


  return(result)
}

createSummary <- function (control,result){

  suppressWarnings(summary <- data.frame(yBest      = result$ybesthistory[!is.na(result$ybesthistory)],
                                           evaluations= result$evaluations[!is.na(result$ybesthistory)],
                                           NAs        = result$NAs[!is.na(result$ybesthistory)],
                                           seed       = control$seed))
  if(control$constraint)
    summary                <- cbind(summary,
                                      constBest  = result$consBesthistory[!is.na(result$ybesthistory)],
                                      cRef       = control$cRef)
# class(summary) = "SCGASummary"
return(summary)
}

# SCGASummary.plot <-function(summary,limit=-Inf){
# browser()
#   if(any(summary$yBest <=limit))
#     summary <- summary %>% filter(yBest<=limit)
#   g = ggplot(summary, aes( x = evaluations)) +
#          geom_line(mapping= aes(y= yBest,colour="black"),show.legend=FALSE) +
#          geom_point(mapping= aes(y= yBest,colour="black"),show.legend=FALSE)+
#          theme_minimal()
#
#   if(all(c(y,yT)>0))
#     g = g + scale_y_log10()
#
#   return(g)
# }
