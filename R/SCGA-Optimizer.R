#' Run the SCGA optimisation
#'
#' Structured-Chromosome Genetic Algorithm
#'
#' @param control list of controls. See \code{\link{Initialise}}
#'
#' @return list
#' @examples
#'
SCGA <- function(control = list(),extraParam=NULL,moreArgsMapply=NULL,...) {

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

  ########## create the population and sigma

  initPopAndSigmaList <- suppressWarnings( InitPopAndSigma(control,feature,LAPPLY,...))
  list2env(initPopAndSigmaList,envir = environment())
  rm(initPopAndSigmaList)

  ########## if exists exists a resume File load it and continue the optimisation from that

  if( file.exists(paste0( control$resumeFrom,".RData" )) && control$resume){

    backList <- RestartFromBackup(control$resumeFrom,newPop)
    list2env(backList,envir = environment())
    rm(backList)
  }

##%######################################################%##
#                                                          #
####            Only for branch SatTracking             ####
#                                                          #
##%######################################################%##

if(!is.null(control$budgetTot)) budget=control$budgetTot



  ##%######################################################%##
  #                                                          #
  ####                Start the main loop                 ####
  #                                                          #
  ##%######################################################%##

  cat("\n Start optimization loop \n")
  pb <- progressBarCreate(control)
  while (all(!conditions$mainLoop)) {

    # tictoc::tic("Optimisation loop time elasped")
    #TODO : add conditions to initialise


    #   ____________________________________________________________________________
    #   stall and local optimisation conditions                                 ####
    conditions$stalling["reinitialise"]      <- stalling == control$maxStallGenerations
    conditions$stalling["localOptimisation"] <- stalling > control$localOptGenerations

    if(any(conditions$stalling)){

      conditionsList <- do.call(names(conditions$stalling[conditions$stalling==T]),
                                args=list(mget(ls()),...))

      list2env(conditionsList,envir = environment())
      rm(conditionsList)

    }

    #   ____________________________________________________________________________
    #   Evaluation                                                              ####

    x <- newPop # duplicate the population for convenience
    if (!resuming){
      # tictoc::tic("\n Evaluation time elasped ")
      evaluteList <- evaluatePopulation(control,evaluateFun,newPop,y,...)
      list2env(evaluteList,envir = environment())
      rm(evaluteList)
    } else
      resuming <- FALSE


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


    ##  ............................................................................
    ##  print on screen

    if(control$printIter){

      result <- Output(best=best,bestRel=bestRel, control,consBest=consBest,
                       consBestRel=consBestRel,
                       constList=constList,evaluations=evaluations,
                       eval= result$evaluations, fitness=fitness,
                       generations=generations,identicX=identicX,
                       media = mean(y, na.rm = TRUE), NAs=NAs,newPop = newPop,
                       result=result, y=yForResults,x=x,sigma=sigma,
                       sigma0=sigma0,stalling=stalling, pb=pb,cRef=control$cRef)
    }


    ##  ............................................................................
    ##  update conditions                                                       ####
    generations    <- generations + 1
    conditions$mainLoop["budgetOver"]    <- evaluations > (control$maxEvaluations
                                                           - control$size + control$elitism)

    conditions$mainLoop["targetReached"] <- abs(best - control$target) <= control$convergence
  }

  #   ____________________________________________________________________________
  #   finalize output                                                         ####
  result <- finaliseOutput(mget(ls(),envir = environment()))

  return(result)
}
