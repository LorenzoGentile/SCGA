#' Run the SCGA optimisation
#'
#' Structured-Chromosome Genetic Algorithm
#'
#' @param control list of controls. See \code{\link{Initialise}}
#'
#' @return list
#' @examples
#'
SCGA <- function(control = list(),...) {

  #   ____________________________________________________________________________
  #     Initialise control and others                                         ####

  initList <- Initialise(control,...)
  list2env(initList,envir = environment())


  #   ____________________________________________________________________________
  #   Multipopulation                                                         ####

  if(control$multiPopulation){
    result <- Multipopulation(initList,...)
    return(result)
  }
  else
    rm(initList)


  #   ____________________________________________________________________________
  #    Create first population and sigmas                                     ####


  ########## if exists exists a resume File load it and continue the optimisation from that

  if( file.exists(paste0( control$resumeFrom,".RData" )) && control$resume){

    backList <- RestartFromBackup(control$resumeFrom)
    list2env(backList,envir = environment())
    rm(backList)


  } else {
    ########## else create the population and sigma

    initPopAndSigmaList <- suppressWarnings( InitPopAndSigma(control,feature,LAPPLY))
    list2env(initPopAndSigmaList,envir = environment())
    rm(initPopAndSigmaList)


  }
  ##%######################################################%##
  #                                                          #
  ####                Start the main loop                 ####
  #                                                          #
  ##%######################################################%##

  cat("\n Start optimization loop \n")
  pb <- progressBarCreate(control)

  while (all(!conditions$mainLoop)) {

    tictoc::tic("Optimisation loop time elasped")
    #TODO : add conditions to initialise

    ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
    ### stall and local optimisation conditions                                 ####

    conditions$stalling["reinitialise"]      <- stalling == control$maxStallGenerations
    conditions$stalling["localOptimisation"] <- stalling > control$localOptGenerations

    if(any(conditions$stalling)){

      conditionsList <- do.call(names(sconditions$stalling[conditions$stalling==T]),
                                args=list(mget(ls())))
      list2env(conditionsList)
      rm(conditionsList)

    }

    #   ____________________________________________________________________________
    #   Evaluation                                                              ####

    x <- newPop # duplicate the population for convenience

    tictoc::tic("\n Evaluation time elasped ")

    evaluteList <- evaluatePopulation(control,evaluateFun,newPop)
    list2env(evaluteList,envir = environment())
    rm(evaluteList)


    #   ____________________________________________________________________________
    #   fitness assignment                                                      ####
    fitnessList <- assignFitness(control,newPop,y)
    list2env(fitnessList,envir = environment())
    rm(fitnessList)


    #   ____________________________________________________________________________
    #   new population generation                                               ####

    operatorApplicationList <- operators(mget(ls(),envir = environment()))
    list2env(operatorApplicationList,envir = environment())
    rm(operatorApplicationList)


    #   ____________________________________________________________________________
    #   Update output                                                           ####


    updateOutputList <-  updateOutput(mget(ls(),envir = environment()))

    list2env(updateOutputList,envir = environment())
    rm(updateOutputList)
    ####### Backup

    if(control$backup){
      result$pop[[generations]] <- x
      result$obs[[generations]] <- yForResults
      back                      <- result
      if(generations%%control$backupInterval==0)
        save("back",file=paste0(resumeFrom,".RData"))
    }

    ##  ............................................................................
    ##  print on screen
    if(control$printIter){

      result <- Output(best=best,bestRel=bestRel, control,consBest=consBest,
                       consBestRel=consBestRel,
                       constList=constList,evaluations=evaluations,
                       eval= result$evaluations, fitness=fitness,
                       generations=generations,identicX=identicX,
                       media = mean(y, na.rm = TRUE), NAs=NAs,
                       result=result, y=yForResults,x=x,sigma=sigma,
                       sigma0=sigma0,stalling=stalling, pb=pb,cRef=control$cRef)
    }


    ##  ............................................................................
    ##  update conditions                                                       ####
    generations    <- generations + 1
    conditions$mainLoop["budgetOver"]    <- evaluations > (control$maxEvaluations
                                                            - control$size + control$elitism)
    conditions$mainLoop["targetReached"] <- abs(control$target - best) < control$convergence
  }

  #   ____________________________________________________________________________
  #   finalize output                                                         ####
browser()
  result <- finaliseOutput(mget(ls(),envir = environment()))

  return(result)
}
