operators <-function(env,...){

  list2env(env,envir = environment())

  #   ____________________________________________________________________________
  #   elitism                                                                 ####


  elitismList     <- elitism(control,constraint,fitness,newPop,sigma,y,x)
  list2env(elitismList,envir = environment())
  rm(elitismList)


  #   ____________________________________________________________________________
  #   crossover                                                               ####
  if(control$useCrossover){

    CrossoverList <- Crossover(APPLY,control,elitismSigma, feature, fitness,newPop,sigma,x,cl,...)
    newPop        <- CrossoverList$newPop
    sigma         <- CrossoverList$sigma
    analysePerformance=F
    if(control$analysePerformance){

      CrossPool   <- CrossoverList$CrossPool

      out = analyseOperationPerformance("crossover",list = list(CrossPool=CrossPool,yOld = yForResults[control$elitism:length(yForResults)] ,
                                                                toEvaluate = newPop[(control$elitism+1):length(newPop)]), evaluateFun=evaluateFun,...)

      crossoverY                                  = out[[1]]
      result$performance[generations,"crossover"] = out[[2]]
    }

    rm(CrossoverList)

  }


#   ____________________________________________________________________________
#   mutation                                                                ####

  MutationList    <- Mutation(APPLY,ChangeMut,cl,control,feature,LAPPLY,mutRate,newPop,nVar,sigma,sigma0,generations,oldPopulation=x)
  newPop          <- MutationList$newPop
  sigma           <- MutationList$sigma
  identicX        <- MutationList$identicX
  mutPool         <- MutationList$mutPool

  if(analysePerformance &!is.empty(mutPool)){

    result$performance[generations,"mutation"]=analyseOperationPerformance(
      "mutation",list = list(yOld = crossoverY[mutPool-1] , toEvaluate = newPop[mutPool]), evaluateFun=evaluateFun)
  }
  rm(MutationList)

  return(mget(ls(),envir = environment()))
}


