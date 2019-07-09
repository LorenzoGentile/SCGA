Mutation <- function(APPLY,ChangeMut,cl,control,feature,MutPool,newPop,nVar,sigma,sigma0){

  if(control$updateSigma){

    ########## update tau
    sigma[,(length(sigma0)-7):length(sigma0)]<- matrix(rep(updateTau(nVar=nVar),nrow(sigma)), nrow(sigma),byrow = T) # 7 is due to the tau and tau local

  }

  MutResult <-
    APPLY( X = matrix(MutPool, , 1), MARGIN = 1,  MutationDF, pop = newPop,
           feature = feature, maxMuting = ChangeMut, sigmas = sigma, createFun = control$createMutFun,
           dontChange = control$dontChangeMut, addnames = control$keep, repairMut = NULL,
           updateSigma= control$updateSigma ,report=control$mutationReport)

  newPop[MutPool] <- purrr::map(MutResult,1)
  #
  # lapply(   as.matrix(1:length(MutResult)),   1,
  #                                              FUN = function(i, x, ...) {     x[[i]][[1]]   },   x = MutResult )
  sigma[MutPool, ] <- t(apply(  as.matrix(1:length(MutResult)),  1,
                                FUN = function(i, x) {    x[[i]][[2]]  },  x = MutResult))

  if (!is.null(control$repairFun))
    newPop<- LAPPLY(X = newPop, control$repairFun,budgetTot=control$budgetTot)

  return(list(
    newPop = newPop,
    sigma  = sigma
    ))
}
