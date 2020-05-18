localOptimisation <- function(env,...){


  list2env(env,envir = environment())
  rm(env)
  cat("\nlocal optimisation running\n")


  active=1:length(feature)
  LocalOptList                             <- control$localOptimiser(control,feat,newPop,y,active,evaluations,sigma,result,generations,...)

  newPop                                   <- LocalOptList$newPop
  evaluations                              <- LocalOptList$evaluations
  y                                        <- LocalOptList$y
  result                                   <- LocalOptList$result


  result$ybesthistory[generations -1]      <- min( y )

  result$xbesthistory[[generations-1]]     <- newPop[[1]]

  result$evaluations[generations-1]        <- evaluations

  stallRef                                 <- Inf

  result$localOpt[generations]             <- TRUE

  rm(LocalOptList)

  initPopAndSigmaList                      <- reinitialise(mget(ls(),envir = environment()),...)
  list2env(initPopAndSigmaList,envir=environment())
  rm(initPopAndSigmaList)


  return(mget(ls(),envir = environment()))
}



reinitialise <- function(env,...){

  list2env(env,envir = environment())
  rm(env)
  initPopAndSigmaList              <- suppressWarnings( InitPopAndSigma(control=control,feature,LAPPLY,...))
  initPopAndSigmaList$stallingFlag <- TRUE
  initPopAndSigmaList$stalling     <- 0
  control$toEval                   <- 1:control$size
  initPopAndSigmaList$control      <- control
  return(initPopAndSigmaList)
}


