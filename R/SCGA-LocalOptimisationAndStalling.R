localOptimisation <- function(env){


  list2env(env,envir = environment())
  cat("\nlocal optimisation running\n")


  active=1:length(feature)
  LocalOptList                 <- LocalOptimisation(control,feat,newPop,y,active,evaluations,sigma,result,generations,...)

  newPop                       <- LocalOptList$newPop
  evaluations                  <- LocalOptList$evaluations
  y                            <- LocalOptList$y
  result                       <- LocalOptList$result
  stallRef                     <- Inf
  result$localOpt[generations] <- TRUE

  rm(LocalOptList)


  if(evaluations >= control$maxEvaluations) return(result)

  initPopAndSigmaList          <- reinitialise()
  list2env(initPopAndSigmaList)
  rm(initPopAndSigmaList)


  return(mget(ls(),envir = environment()))
}



reinitialise <- function(control){

  initPopAndSigmaList              <- suppressWarnings( InitPopAndSigma(control=control,feature,LAPPLY))
  initPopAndSigmaList$stallingFlag <- TRUE
  initPopAndSigmaList$stalling     <- 0
  return(initPopAndSigmaList)
}


