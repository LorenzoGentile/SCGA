Multipopulation <- function(initList,extraParam=NULL,moreArgsMapply=NULL,...){
  list2env(initList,envir = environment())
browser()
  #   ____________________________________________________________________________
  #   Create first populations and sigmas                                     ####

  multiPopControlList   <- control$multiPopControl$multiPopStrategy(control,...)
  pb <- progress::progress_bar$new(total = control$maxEvaluations)
  cat("\nStart multi population optimization loop\n")

  while (evaluations < (control$maxEvaluations-(control$size+control$elitism)*control$multiPopControl$nPopulations)
         && abs(control$target - best) > control$convergence ) {

    #   ____________________________________________________________________________
    #    Run optimisation over the indipendent populations                      ####


    out                     <- mapply(SCGA, multiPopControlList,cases=extraParam)
    # out                   <- LAPPLY( X = multiPopControlList, FUN = SCGA,...)


    #   ____________________________________________________________________________
    #   Update populations simgmas and controls                                 ####

    print(generations)


    multiPopControlList   <- evolutionMultiPop(control$multiPopControl,multiPopControlList,out)

    #   ____________________________________________________________________________
    #   Update output                                                           ####

    evaluations                                <- evaluations + sum( sapply(out, function(x)x$evaluations %>% max) %>% as.integer() )
    result$evaluations[generations]            <- evaluations

    result$NAs[generations]                    <- sum( purrr::map(out,"NAs") %>% unlist() )

    result$yForResults                         <- purrr::map(out,"ybest") %>% as.numeric()

    best                                       <- min(result$yForResults)

    result$ybesthistory[generations]           <- best

    result$xbesthistory[[generations]]         <- purrr::map(out,"xbest")[[which.min(result$yForResults)]]

    result$ybesthistoryPop[[generations]]      <- purrr::map(out,"ybesthistory")

    result$xbesthistoryPop[[generations]]      <- purrr::map(out,"xbesthistory")

    result$summaries[[generations]]            <- purrr::map(out,"summary")

    result$stalling                            <- stalling <- generations - which.min(result$ybesthistory)

    result$evaluations[generations]            <- evaluations


    if (control$saveAll) {
      result$x[[generations]]     <- purrr::map(out,"x")
      result$y[[generations]]     <- purrr::map(out,"yForResults")
      result$sigma[[generations]] <- purrr::map(out,"sigma")
    }


    ##  ............................................................................
    ##  backup

    if(control$backup){
      if(generations%%control$backupInterval==0){
        back                      <- result
        back$summary              <- createSummary(control,result)
        back$ybesthistory         <- back$ybesthistory[1:generations]
        back$xbesthistory         <- back$xbesthistory[1:generations]
        back$y                    <- back$y[1:generations]
        back$x                    <- back$x[1:generations]
        save("back",file=paste0(control$resumeFrom,".RData"))
      }

    }

    ##  ............................................................................
    ##  print output                                                            ####

    if(control$printIter){
      media <- c(media, mean(result$yForResults, na.rm = TRUE))
      #
      #       result <-  Output(best=best, control=control,evaluations=evaluations, generations=generations,media=media, NAs=result$NAs[generations], result, y=result$yForResults,
      #                            x = purrr::map(out,"lastX"),sigma = purrr::map(out,"lastSigma"),sigma0=out[[1]]$sigma0,stalling=stalling, pb=pb)       # TODO correct this in a proper way

    }

    ##  ............................................................................
    ##  increment generations                                                   ####

    generations           <- generations + 1

  }
  ########## Finalize the output ############################################################################################

  result <- finaliseOutputMultiPop(mget(ls(),envir = environment()))

  return(result)
}


evolutionMultiPop <- function(control,multiPopControlList,out){

  nToMigrate                           <- control$nMigrations
  popOrder                             <- permute::shuffle(control$nPopulations)

  for (i in 1:length(popOrder)) multiPopControlList[[i]]$newPop    <- out[[i]]$lastX
  for (i in 1:length(popOrder)) {

    destinations = rep(popOrder[-popOrder[i]],nToMigrate)

    for (j in nToMigrate:1) {

      indDest                          <- destinations[j]
      notElitist                       <- (multiPopControlList[[indDest]]$size - multiPopControlList[[indDest]]$elitism): multiPopControlList[[indDest]]$size
      ind                              <- sample(notElitist,1)
      multiPopControlList[[popOrder[indDest]]]$newPop[[ind]] <- out[[popOrder[i]]]$newPop[[j]]

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
  multiPopControl$multiPopulation <- multiPopControl$parallel <- multiPopControl$saveAll<- FALSE
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
