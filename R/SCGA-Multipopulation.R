 Multipopulation <- function(initList,...){
  list2env(initList,envir = environment())

  ########## Create first populations and sigmas
  multiPopControlList   <- control$multiPopControl$multiPopStrategy(control,...)
  pb <- progress::progress_bar$new(total = control$maxEvaluations)
  cat("\nStart multi population optimization loop\n")

  while (evaluations < (control$maxEvaluations-(control$size+control$elitism)*control$multiPopControl$nPopulations)
         && abs(control$target - best) >= control$convergence ) {

    ########## Run optimisation over the indipendent populations
    out                   <- LAPPLY( X = multiPopControlList, FUN = SCGA,...)

    ########## Update populations simgmas and controls
    multiPopControlList <- evolutionMultiPop(control$multiPopControl,multiPopControlList,out)


    ####### Update output #################################################################################################
    evaluations           <- evaluations + sum( purrr::map(out,"evaluations") %>% as.integer() )

    #result$NAs[generations]           <- sum( purrr::map(out,"NAs") %>%unlist() )
    result$NAs[generations] =0         #################### to change##########################
    best                              <- min(purrr::map(out,"ybest") %>% as.numeric())
    result$yForResults                <- purrr::map(out,"ybest") %>% as.numeric()
    result$ybesthistory[generations]  <- best
    result$stalling       <- stalling <- generations - which.min(result$ybesthistory)

    if (control$saveX)
      result$x[[generations]]         <- purrr::map(out,"x")
    if (control$saveSigma)
      result$sigma[[generations]]     <- purrr::map(out,"sigma")


    ####### Backup

    if(control$saveIter){
      result$pop[[generations]] <- result$x[[generations]]
      result$obs[[generations]] <- result$yForResults
      back                      <- result
      if(generations%%10 == 0)
        save("back",file = paste0(  resumeFrom,".RData"))
    }

    ####### print output
    if(control$printIter){
      media <- c(media, mean(result$yForResults, na.rm = TRUE))
#
#       result <-  Output(best=best, control=control,evaluations=evaluations, generations=generations,media=media, NAs=result$NAs[generations], result, y=result$yForResults,
#                            x = purrr::map(out,"lastX"),sigma = purrr::map(out,"lastSigma"),sigma0=out[[1]]$sigma0,stalling=stalling, pb=pb)       # TODO correct this in a proper way


    }

    ####### increment generations
    generations    <- generations + 1

  }
  ########## Finalize the output ############################################################################################

  result$evaluations      <- evaluations
  result$exitMessage      <- "Optimisation did not exceeded maximum function evaluations"
  result$control          <- control
  result$lastSigma        <- purrr::map(out,"lastSigma")
  result$lastX            <- purrr::map(out,"lastX")
  result$newPop           <- purrr::map(out,"newPop")
  result$sigma0           <- purrr::map(out,"sigma0")
  result$xbest            <- result$lastX[which(result$yForResults==best)]
  result$ybest            <- best

  return(result)
}


evolutionMultiPop <- function(control,multiPopControlList,out){

  nToMigrate                           <- control$nMigrations
  popOrder                             <- permute::shuffle(control$nPopulations)


  for (i in 1:length(popOrder)) {

    multiPopControlList[[i]]$newPop    <- out[[i]]$lastX
    destinations = rep((1:control$nPopulations)[-popOrder[i]],nToMigrate)

    for (j in 1:nToMigrate) {

      indDest                          <- destinations[j]
      ind                              <- sample(multiPopControlList[[indDest]]$toEval,1)
      multiPopControlList[[popOrder[indDest]]]$lastX[[ind]] <- out[[popOrder[i]]]$newPop[[j]]

    }
  }
  return(multiPopControlList )
}

populationStrategyParallel <- function(control,...) {

  nPopulations                         <- control$multiPopControl$nPopulations
  multiPopControl                      <- control
  namesControl                         <- names(multiPopControl)
  printOrPlot                          <- c(grep("print",namesControl),grep("plot",namesControl))

  multiPopControl[printOrPlot]         <- FALSE
  multiPopControl$multiPopulation <- multiPopControl$parallel <- multiPopControl$saveX <- multiPopControl$saveSigma <- FALSE
  multiPopControl$cpus                 <- NA
  multiPopControl$localOptGenerations  <- Inf



  if(control$multiPopControl$migrationType == "generation"){
    multiPopControl$maxGenerations     <- control$multiPopControl$migrationInterval
    multiPopControl$maxEvaluations     <- multiPopControl$size + multiPopControl$sizeToEval * (multiPopControl$maxGenerations - 1)

  } else {
    multiPopControl$maxEvaluations     <- control$multiPopControl$migrationInterval
    multiPopControl$maxGenerations     <- 1 + (multiPopControl$maxEvaluations -
                                                 multiPopControl$size) %/% multiPopControl$sizeToEval
  }

  out                                  <- rep(list(multiPopControl),nPopulations)
  seeds                                <- sample(1e6,nPopulations)

  out                                  <- mapply(seeds, FUN=function(seed,out) {out$seed=seed ; return(out)},out=out,SIMPLIFY  = FALSE )
  return(out)
}
