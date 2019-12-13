Mutation <- function(APPLY,ChangeMut,cl,control,feature,LAPPLY,mutRate,newPop,nVar,sigma,sigma0,generation,oldPopulation){
  # browser()
  allMutated <- MutPool <- which((sample( c(0, 1),   prob = c((1 - mutRate), mutRate),
                                          size = control$size-control$elitism,   replace = TRUE ) == 1)) + control$elitism
  toCompare= append(newPop,oldPopulation)
  identicCandidates <- checkIdentical(newPop,toCompare,control$elitism)
  identicX          <-  firstidenticX <-  length(identicCandidates)

  attempts <- 1
  while((identicX !=0 && attempts < 20) || attempts == 1 ){
    identicCandidates <- checkIdentical(newPop,toCompare,control$elitism)
    identicX = length(identicCandidates)

    if(identicX!=0){

      if(any(identicCandidates<=control$elitism))
        forceEvaluation <- TRUE

      # if(F){
      #   tempControl <- control
      #   tempControl$size <- length(identicCandidates)
      #   initPopAndSigmaList <- suppressWarnings( InitPopAndSigma(control=tempControl,feature,LAPPLY))
      #   newPop[identicCandidates] <- initPopAndSigmaList$newPop
      #   sigma[identicCandidates,] <- initPopAndSigmaList$sigma
      # }else{
      #   # browser()

      MutPool <- c(MutPool,identicCandidates)

      # sigma[identicCandidates,(1:(ncol(sigma)-8))] <- sigma[identicCandidates,(1:(ncol(sigma)-8))] * 10

      # }
    }

    if(control$updateSigma & attempts==1){

      ########## update tau
      sigma[,(length(sigma0)-7):length(sigma0)]<- matrix(rep(updateTau(nVar=nVar),nrow(sigma)), nrow(sigma),byrow = T) # 7 is due to the tau and tau local

    }
    if(!is.empty(MutPool)){
      MutResult <-
        APPLY( X = matrix(MutPool, , 1), MARGIN = 1,  MutationDF, pop = newPop,
               feature = feature, maxMuting = ChangeMut, sigmas = sigma, createFun = control$createMutFun,
               dontChange = control$dontChangeMut, addnames = control$keep, repairMut = NULL,
               updateSigma= control$updateSigma ,report=control$mutationReport,control=control,generation=generation)

      newPop[MutPool] <- purrr::map(MutResult,1)

      sigma[MutPool, ] <- t(apply(  as.matrix(1:length(MutResult)),  1,
                                    FUN = function(i, x) {    x[[i]][[2]]  },  x = MutResult))

      if (!is.null(control$repairFun))
        newPop[MutPool] <- LAPPLY(X = newPop[MutPool], control$repairFun,budgetTot=control$budgetTot)

      MutPool           <- NULL

      identicCandidates <- checkIdentical(newPop,toCompare,control$elitism)
      identicX          <- length(identicCandidates)
    }
    attempts          <- attempts + 1
  }


  return(list(
    newPop   = newPop,
    sigma    = sigma,
    identicX = firstidenticX,
    allMutated = unique(allMutated),
    mutPool = allMutated
  ))
}
