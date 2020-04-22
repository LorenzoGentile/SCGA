finaliseOutput <- function(env){
  list2env(env,envir = environment())

  result$exitMessage         <- names(conditions$mainLoop[ conditions$mainLoop==T])
  result$control             <- control
  result$lastSigma           <- sigma
  result$lastX               <- x
  result$newPop              <- newPop
  result$sigma0              <- sigma0
  result$summary             <- try(createSummary(control,result))


  if(control$constraint){
    result$xbest             <- bestFeasible$x
    result$ybest             <- bestFeasible$y
    result$consBest          <- consBest
  }  else {
    ind                      <- which.min(result$ybesthistory)
    result$xbest             <- result$xbesthistory[[ind]]
    result$ybest             <- result$ybesthistory[ind]
  }
notNaInd = !is.na(result$ybesthistory)
result$ybesthistory <- result$ybesthistory[notNaInd]
result$xbesthistory <- result$xbesthistory[notNaInd]

  return(result)
}

createSummary <- function (control,result){

  suppressWarnings(summary <- data.frame(  yBest       = result$ybesthistory[!is.na(result$ybesthistory)],
                                           evaluations = result$evaluations[!is.na(result$ybesthistory)],
                                           NAs         = result$NAs[which(!is.na(result$ybesthistory))],
                                           seed        = control$seed,
                                           algoName    = control$algoName,
                                           problemName = control$problemName))
  if(control$constraint)
    summary                <- cbind(summary,
                                      constBest  = result$consBesthistory[!is.na(result$ybesthistory)],
                                      cRef       = control$cRef)
# class(summary) = "SCGASummary"
return(summary)
}


finaliseOutputMultiPop <- function(env){
  list2env(env,envir = environment())

  result$exitMessage         <- names(conditions$mainLoop[ conditions$mainLoop==T])
  result$control             <- control

  result$lastSigma           <- purrr::map(out,"lastSigma")
  result$lastX               <- purrr::map(out,"lastX")
  result$newPop              <- purrr::map(out,"newPop")
  result$sigma0              <- purrr::map(out,"sigma0")

  result$ybesthistoryPop <- purrr:::transpose(result$ybesthistoryPop) %>% sapply(FUN=function(x)unlist(x,recursive = F),simplify = F)
  result$xbesthistoryPop <- purrr:::transpose(result$xbesthistoryPop) %>% sapply(FUN=function(x)unlist(x,recursive = F),simplify = F)


  summariesMod <- try(sapply(1:length(result$summaries),function(generations){
      if(generations==1)
        return(result$summaries[[1]])
      # y is all the summaries of the same generation
      y=sapply(1:length(result$summaries[[generations]]), function(pop){

        result$summaries[[generations]][[pop]]$evaluations =  result$summaries[[generations]][[pop]]$evaluations + sum(sapply(seq(1,(generations-1)),function(jj)max(result$summaries[[jj]][[pop]]$evaluations)))
        return(result$summaries[[generations]][[pop]])
      },simplify = F)
      return(y)},simplify = F))

if(!is.character(summariesMod)) result$summaries <- summariesMod


  result$summariesPop <-  try(purrr:::transpose(result$summaries) %>% sapply(FUN=function(x) dplyr::bind_rows(x) ,simplify = F))
  result$ybestPop       <- sapply(result$ybesthistoryPop,min)
  result$xbestPop       <- mapply(function(y,x)x[[which.min(y)]] ,result$ybesthistoryPop,result$xbesthistoryPop,SIMPLIFY = F)

  # result$summary             <- try(createSummary(control,result))


  if(control$constraint){
    result$xbest             <- bestFeasible$x
    result$ybest             <- bestFeasible$y
    result$consBest          <- consBest
  }  else {
    ind                      <- which.min(result$ybesthistory)
    result$xbest             <- result$xbesthistory[[ind]]
    result$ybest             <- result$ybesthistory[ind]
  }
  notNaInd = !is.na(result$ybesthistory)
  result$ybesthistory <- result$ybesthistory[notNaInd]
  result$xbesthistory <- result$xbesthistory[notNaInd]

  return(result)
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
