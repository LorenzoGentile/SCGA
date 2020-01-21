updateOutput <- function(env){


  list2env(env,envir = environment())

  result$NAs[generations]               <- NAs


  if(control$constraint){

    best                                <- bestFeasible$y
    consBest                            <- bestFeasible$constraint
    result$consBesthistory[generations] <- consBest
    if(!is.empty(feasibleRelax)){

      bestRel                           <- min(yForResults[feasibleRelax])
      consBestRel                       <- constraintForResults[feasibleRelax[which.min(yForResults[feasibleRelax])]]


    } else{
      bestRel                           <- NA
      consBestRel                       <- NA
    }


    result$yForResults                    <- yForResults

    result$ybesthistory[generations]      <- best

    control$tolerance                     <- .01*min(y)



    lastLocal                           <- max(which(result$localOpt==TRUE))               #not Working
    #not Working
    lastLocal                           <- ifelse(lastLocal==1,0,lastLocal)                #not Working
    #not Working
    ytoConsider                         <- result$ybesthistoryFeas[lastLocal:generations]  #not Working

    if(min(ytoConsider) <= stallRef - control$tolerance){

      stallRef                          <- min(ytoConsider)

      result$stalling                   <-  stalling <- 0

    } else
      result$stalling                   <-  stalling <- stalling + 1

  } else {

    best                                  <- min(y)

    result$yForResults                    <- yForResults

    result$ybesthistory[generations]      <- best

    result$xbesthistory[[generations]]      <- x[[which.min(yForResults)]]

    control$tolerance                     <- .01*min(y)


    lastLocal                           <- max(which(result$localOpt==TRUE))
    # lastLocal                         <- ifelse(lastLocal==1,0,lastLocal)
    ytoConsider                         <- result$ybesthistory[lastLocal:generations]

    if(min(ytoConsider) <= stallRef - control$tolerance){

      stallRef                          <- min(ytoConsider)

      result$stalling                   <-  stalling <- 0

    } else
      result$stalling                   <-  stalling <- stalling + 1

  }

  if(is.infinite( stalling ))
    result$stalling                     <-  stalling <- 0

  if (control$saveAll){

    result$x[[generations]]             <- x
    result$y[[generations]]             <- y
    result$sigma[[generations]]         <- sigma
  }


  evaluations                           <- evaluations + length(control$toEval)

  control$toEval                        <- seq(control$elitism+1, control$size)

  result$evaluations[generations]       <- evaluations

  return(mget(ls(),envir = environment()))

}
