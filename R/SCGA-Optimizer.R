SCGA <-
  function(
    control = list(),
    ...) {

    ########## Initialise control and others ######################################################################################
    initList <- Initialise(control,...)
    list2env(initList,envir = environment())

    ########## Multipopulation ######################################################################################
    if(control$multiPopulation){
      result <- Multipopulation(initList,...)
      return(result)
    }
    else
      rm(initList)

    ########## Create first population and sigmas ######################################################################################

    ########## if exists exists a resume File load it and continue the optimisation from that

    if( file.exists(paste0( control$resumeFrom,".RData" )) && control$resume){

      backList <- RestartFromBackup(control$resumeFrom)
      list2env(backList,envir = environment())
      rm(backList)


    } else {
      ########## else create the population and sigma

      initPopAndSigmaList <- InitPopAndSigma(control,feature,LAPPLY)
      newPop <- initPopAndSigmaList$newPop
      sigma <- initPopAndSigmaList$sigma
      sigma0 <- initPopAndSigmaList$sigma0
      rm(initPopAndSigmaList)

    }

    ########## number of chromosome for operators

    ChangeList  <- EvalCand2Operator(newPop,control)
    ChangeCross <- ChangeList$ChangeCross
    ChangeMut   <- ChangeList$ChangeMut
    rm(ChangeList)


    ####### Start the main loop #################################################################################################
    cat("\n Start optimization loop \n")
    pb <- progress::progress_bar$new(total = control$maxEvaluations)

    while (evaluations <= (control$maxEvaluations-control$size+control$elitism) && abs(control$target - best) >= control$convergence ) {

      tictoc::tic("Optimisation loop time elasped")
      ####### Stall generations #################################################################################################
      if(stalling == control$maxStallGenerations){

        ########## Reinitialise the stalling flags

        stallingFlag <- TRUE
        stalling    <- 0

        ########## reinitialise the population and sigma
        initPopAndSigmaList <- InitPopAndSigma(control,feature,cl)
        list2env(initPopAndSigmaList,envir = environment())
        rm(initPopAndSigmaList)
        #Output # newPop
        # sigma
        # sigma0


        ########## number of chromosome to crossover

        ChangeList  <- EvalCand2Operator(newPop,control)
        ChangeCross <- ChangeList$ChangeCross
        ChangeMut   <- ChangeList$ChangeMut
        rm(ChangeList)
      }



      ####### Local optimisation #################################################################################################
      if (stalling > control$localOptGenerations){

        LocalOptList <- LocalOptimisation(control,feat,newPop,y,active,evaluations,sigma,result,generations,...)
        newPop           <- LocalOptList$newPop
        evaluations      <- LocalOptList$evaluations
        y                <- LocalOptList$y
        result           <- LocalOptList$result
        rm(LocalOptList)

        if(evaluations >= control$maxEvaluations)
          return(result)

      }

      x <- newPop # duplicate the population for convenience

      tictoc::tic("\n Evaluation time elasped ")


      ########## During the loop it saves elitism evaluations
      if (as.logical(control$elitism) && generations != 1 && !stallingFlag) {

        cat("\n","To evaluate",control$sizeToEval,"candidates","\n")

        y[control$toEval]   <- control$evaluateFN(Fun,newPop[control$toEval],
                                                  control$vectorOnly, control$vectorized,SAPPLY = SAPPLY,...)
        yForResults <- y

        ########## First Generation or no elitism
      } else{

        print(paste("To evaluate",control$size,"candidates"))

        yForResults <- y <- control$evaluateFN(Fun, newPop, control$vectorOnly, control$vectorized,SAPPLY = SAPPLY,...)

        stallingFlag=F
      }


      ########## Na handling

      if(anyNA(y)){
        ws          <- max(y[!is.na(y)])
        y[is.na(y)] <- ws + (mean(y[!is.na(y)])) * 0.1
      }

      tictoc::toc()

      ########## Fitness assignement
      fitness         <- control$fitnessFN(y)

      ########## Count the NAs
      NAs             <- sum(is.na(y))

      ########## Elitism

      if (as.logical(control$elitism)) {

        e = sort(fitness,index.return = T, decreasing = T)$ix[1:control$elitism]        # if elitism i save the control$elitism best candidates
        y[1:control$elitism]    <- y[e]                                                 # (and relative sigmas) untouched as firsts in the list of candidates)
        newPop                  <- x[e]
        elitismSigma            <- sigma[e, ]

      }

      ####### Crossover #################################################################################################
      if(control$useCrossover){

        CrossoverList <- Crossover(APPLY,ChangeCross,control,elitismSigma, feature, fitness,newPop,sigma,x,Change,cl,...)
        newPop <- CrossoverList$newPop
        sigma  <- CrossoverList$sigma
        rm(CrossoverList)

      } else
        newPop[-control$elitism] <- x[-e]

      ####### Mutation #################################################################################################

      MutPool <- which((sample( c(0, 1),   prob = c((1 - mutRate), mutRate),
                                size = control$sizeToEval,   replace = TRUE ) == 1)) + control$elitism

      if (!bazar::is.empty(MutPool)) {
        MutationList <- Mutation(APPLY,ChangeMut,cl,control,feature,MutPool,newPop,nVar,sigma,sigma0)
        newPop <- MutationList$newPop
        sigma  <- MutationList$sigma
        rm(MutationList)
      }

      ########## number of chromosome for operators

      ChangeList  <- EvalCand2Operator(newPop,control)
      ChangeCross <- ChangeList$ChangeCross
      ChangeMut   <- ChangeList$ChangeMut
      rm(ChangeList)

      ####### Update output #################################################################################################

      result$NAs[generations]           <- NAs

      best                              <- min(yForResults, na.rm = TRUE)
      result$yForResults                <- yForResults
      result$ybesthistory[generations]  <- best
      result$stalling       <- stalling <- generations - which.min(result$ybesthistory)
      if (control$saveX)
        result$x[[generations]]         <- x
      if (control$saveSigma)
        result$sigma[[generations]]     <- sigma


      if(generations==1)
        evaluations        <- evaluations + control$size
      else
        evaluations        <- evaluations + control$sizeToEval

      ####### Backup

      if(control$saveIter){
        result$pop[[generations]] <- x
        result$obs[[generations]] <- yForResults
        back                      <- result
        if(generations%%10==0)
          save("back",file=paste0(  resumeFrom,".RData"))
      }

      ####### ptint output
      if(control$printIter){
        media <- c(media, mean(y, na.rm = TRUE))
        result <- Output(best, control,evaluations, generations,media, NAs, result, y,x,sigma,sigma0,stalling, pb)
      }

      ####### increment generations
      generations    <- generations + 1

    }
    ########## Finalize the output ############################################################################################

    result$evaluations      <- evaluations
    result$exitMessage      <- "Optimisation did not exceeded maximum function evaluations"
    result$control          <- control
    result$lastSigma        <- sigma
    result$lastX            <- x
    result$newPop           <- newPop
    result$sigma0           <- sigma0
    result$xbest            <- x[which(yForResults==best)]
    result$ybest            <- best





    return(result)
  }
