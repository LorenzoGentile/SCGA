assignFitness <- function(control,newPop,y,...){

if(control$constraint){


  ## precompute the feiasible and unfe
  feasible                    <- which(constraint <= control$cRef)
  unfeasible                  <- which(constraint > control$cRef)

  # if there is not unfeasible treat use the feasible

  if(is.empty(unfeasible))  unfeasible  <- feasible

  #if i have a feasible and it is better then the stored one -> update
  if(!is.empty(feasible)){

    if(min(y[feasible]) < bestFeasible$y){
      indMinFeasible          <- feasible[which.min(y[feasible])]
      bestFeasible$y          <- min(y[feasible])
      bestFeasible$x          <- newPop[[indMinFeasible]]
      bestFeasible$constraint <- constraint[indMinFeasible]

      # else replace the worst unfeasible with the best feasible
    } else if( !is.null(bestFeasible$x )){
      ## TODO add check if is already there
      toRemove                <- unfeasible[which.max(constraint[unfeasible])]
      newPop[[toRemove]]      <- bestFeasible$x
      y[toRemove]             <- bestFeasible$y
      constraint[toRemove]    <- bestFeasible$constraint
    }
  }


  constList                   <- constraintHandlerFitness(y,constraint,control$cRef,evaluations, control,...)
  fitness                     <- constList$fitness
  feasible                    <- constList$feasible
  feasibleRelax               <- constList$feasibleRelax
  constraint                  <- constList$constraint
  constraintForResults        <- constraint
  wC                          <- constList$wC
  wY                          <- constList$wY


  # print( plotFitness(y,constList,fitness))

}else{


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### NAs handling                                                            ####

  if(anyNA(y)){
    wCandidate                <- max(y[!is.na(y)])
    y[is.na(y)]               <- wCandidate + mean(y,na.rm = T) * 0.1
  }
  fitness                     <- control$fitnessFN(y)

}
  return(mget(ls(),envir = environment()))
}
