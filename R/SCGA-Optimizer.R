#' Run the SCGA optimisation
#'
#' Structured-Chromosome Genetic Algorithm
#'
#' @param control list of controls. See \code{\link{Initialise}}
#'
#' @return list
#' @examples
#'
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

      initPopAndSigmaList <-suppressWarnings( InitPopAndSigma(control,feature,LAPPLY))
      list2env(initPopAndSigmaList,envir = environment())
      rm(initPopAndSigmaList)


    }


    ########## Start the main loop #########################################################################################
    cat("\n Start optimization loop \n")
    pb <- progress::progress_bar$new(total = control$maxEvaluations,format = "  optimising [:bar] :percent eta: :eta", clear = TRUE, width= 60)
    mutationReport               <- get("mutationReport",envir = .GlobalEnv) # report
    mutationReport = matrix(NA, 1,6)
    assign("mutationReport",mutationReport, envir = .GlobalEnv)               #report
    while (evaluations <= (control$maxEvaluations-control$size+control$elitism) && abs(control$target - best) >= control$convergence ) {
      # if(generations ==18)
      #   browser()
      # print("sd")
      # print(sapply(1:nrow(newPop[[1]]),function(i)  map(newPop,i) %>% as.numeric() %>% sd))
      # print("Candidates")
      # print(sapply(1:nrow(newPop[[1]]),function(i)  map(newPop,i) %>% as.numeric() ))
      # if(best <= 16.52)
      #   browser()
      #
      # if(generations%%50 ==0)

      if(generations %% 30==0 && generations!=1){
        if (control$printWF){
          plots =sapply(x[order(yForResults)], plotWindFarm,retG=T,simplify = F)
          saveMorePlot(plots,paste0("generation", generations,".pdf"))
        }
      }

# browser()
      # #Check bounds
      # if( any(! sapply(newPop, function(x) out = x[,1]<=sapply(x[,"feature"],function(x) max(feature[[x]]$bound())) & x[,1]>=sapply(x[,"feature"],function(x)feature[[x]]$bound()[1]))))
      # out = x[,1]<=sapply(x[,"feature"],function(x)feature[[x]]$bound()[2]) & x[,1]>=sapply(x[,"feature"],function(x)feature[[x]]$bound()[1])
      tictoc::tic("Optimisation loop time elasped")
      ####### Stall generations #################################################################################################
      if(stalling == control$maxStallGenerations || stalling > control$localOptGenerations){






        ####### Local optimisation #################################################################################################
        if (stalling > control$localOptGenerations){
          print("locale")
          active=1:length(feature)
          # LocalOptList <- LocalOptimisation(control,feat,newPop,y,active,evaluations,sigma,result,generations,...)

          LocalOptList <- localCassini(newPop[[1]][,1],newPop,evaluations,y,result,...)
          newPop           <- LocalOptList$newPop
          evaluations      <- LocalOptList$evaluations
          y                <- LocalOptList$y
          result           <- LocalOptList$result
          stallRef         <- Inf
          result$localOpt[generations] <- TRUE
          rm(LocalOptList)

          if(evaluations >= control$maxEvaluations)
            return(result)

        }
        ########## reinitialise the population and sigma
        tempControl            <- control
        tempControl$size       <- control$size - 1
        initPopAndSigmaList    <- suppressWarnings( InitPopAndSigma(control=tempControl,feature,LAPPLY))
        newPop[2:control$size] <- initPopAndSigmaList$newPop
        sigma[2:control$size,] <- initPopAndSigmaList$sigma
        rm(initPopAndSigmaList)
        rm(tempControl)


        ########## Reinitialise the stalling flags

        stallingFlag <- TRUE
        stalling    <- 0

      }
      ####### Evaluation #################################################################################################
      x <- newPop # duplicate the population for convenience

      tictoc::tic("\n Evaluation time elasped ")


      # if(evaluations>=914)
      #   browser()


      ########## During the loop it saves elitism evaluations


      if (as.logical(control$elitism) && generations != 1 && !stallingFlag || forceEvaluation) {

        forceEvaluation=FALSE
        cat("\n","To evaluate",control$sizeToEval,"candidates","\n")
        outEvaluation <- evaluateFun(newPop[control$toEval]  ,...)

        ###### Assign from evaluation output

        if(is.list(outEvaluation)){
          y[control$toEval]               <- outEvaluation[[1]]

          if(!is.null(outEvaluation$constraint))
            constraint[control$toEval]    <- outEvaluation$constraint

          if(!is.null(outEvaluation$x))
            mewPop[control$toEval]        <- outEvaluation$x

        }else
          y[control$toEval]               <- outEvaluation


      } else{

        ########## First Generation or no elitism or after stalling
        cat("\n","To evaluate",control$size,"candidates","\n")

        outEvaluation <- evaluateFun(newPop,...)

        ###### Assign from evaluation output

        if(is.list(outEvaluation)){
          y                  <- outEvaluation[[1]]

          if(!is.null(outEvaluation$constraint))
            constraint       <- outEvaluation$constraint

          if(!is.null(outEvaluation$x))
            mewPop        <- outEvaluation$x

        }else
          y                <- outEvaluation

        stallingFlag=F
      }

      yForResults        <- y

      tictoc::toc()

      ########## Count the NAs
      NAs             <- sum(is.na(y))


      ########## Fitness assignement
      if(control$constraint){
# browser()
        ## precompute the feiasible and unfe
        feasible   <- which(constraint <= control$cRef)
        unfeasible <- which(constraint > control$cRef)
        # if there is not unfeasible treat use the feasible
        if(is.empty(unfeasible))
          unfeasible <- feasible
        #if i have a feasible and it is better then the stored one -> update
        if(!is.empty(feasible)){

          if(min(y[feasible]) < bestFeasible$y){

            bestFeasible$y          <- min(y[feasible])
            bestFeasible$x          <- newPop[[feasible[which.min(y[feasible])]]]
            bestFeasible$constraint <- constraint[feasible[which.min(y[feasible])]]

          } else if( !is.null(bestFeasible$x )){ # else replace the worst unfeasible with the best feasible
            ## TODO add check if is already there
            toRemove             <- unfeasible[which.max(constraint[unfeasible])]
            newPop[[toRemove]]   <- bestFeasible$x
            y[toRemove]          <- bestFeasible$y
            constraint[toRemove] <- bestFeasible$constraint
          }
        }
        # if(!is.empty(feasible))
        #   browser()
        # print( bestFeasible$y )
        constList                  <- constraintHandlerFitness(y,constraint,control$cRef,evaluations, control,...)

        fitness                    <- constList$fitness
        feasible                   <- constList$feasible
        feasibleRelax              <- constList$feasibleRelax
        constraint                 <- constList$constraint
        constraintForResults       <- constraint
        wC                         <- constList$wC
        wY                         <- constList$wY
        # print( plotFitness(y,constList,fitness))

      }else{

        ########## Na handling

        if(anyNA(y)){
          ws          <- max(y[!is.na(y)])
          y[is.na(y)] <- ws + (mean(y[!is.na(y)])) * 0.1
        }
        # browser()
        fitness         <- control$fitnessFN(y)

      }



      ########## Elitism
      # if(!is.empty(constList$feasible))
      #   browser()
      if (as.logical(control$elitism)) {
        # if(any(y)<1)
        #   browser()
        e = sort(fitness,index.return = T, decreasing = T)$ix        # if elitism i save the control$elitism best candidates


        if(control$constraint)
          # e = e[e %in% unique(c(feasible[sort(fitness[feasible],index.return=T,decreasing = T)$ix],e))[1:control$elitism]] ### preserve feasible and not relaxed feasible
          e = e[1:control$elitism]
        else
          e = e[1:control$elitism]
        y[1:control$elitism]    <- y[e]                                                 # (and relative sigmas) untouched as firsts in the list of candidates)
        newPop                  <- x[e]
        elitismSigma            <- sigma[e, ]
        if(control$constraint)
          constraint            <- constraint[e]
      }
      # print(newPop[[1]])


      # browser()
      ####### Crossover ####################################################################################################
      if(control$useCrossover){

        CrossoverList <- Crossover(APPLY,control,elitismSigma, feature, fitness,newPop,sigma,x,cl,...)
        newPop        <- CrossoverList$newPop
        sigma         <- CrossoverList$sigma
        analysePerformance=F
        if(analysePerformance){

          CrossPool <- CrossoverList$CrossPool

            out = analyseOperationPerformance("crossover",list = list(CrossPool=CrossPool,yOld = yForResults[control$elitism:length(yForResults)] ,
                                      toEvaluate = newPop[(control$elitism+1):length(newPop)]), evaluateFun=evaluateFun,...)

            crossoverY                                 = out[[1]]
            result$performance[generations,"crossover"] = out[[2]]
        }

        rm(CrossoverList)

      } else
        newPop[-control$elitism] <- x[-e]

      ####### Mutation #################################################################################################

      MutationList <- Mutation(APPLY,ChangeMut,cl,control,feature,LAPPLY,mutRate,newPop,nVar,sigma,sigma0,generations,oldPopulation=x)
      newPop       <- MutationList$newPop
      sigma        <- MutationList$sigma
      identicX     <- MutationList$identicX
      mutPool      <- MutationList$mutPool
      if(analysePerformance &!is.empty(mutPool)){

      result$performance[generations,"mutation"]=analyseOperationPerformance("mutation",list = list(yOld = crossoverY[mutPool-1] ,
                                                                                              toEvaluate = newPop[mutPool]), evaluateFun=evaluateFun)
      }
      rm(MutationList)

      ####### Update output #################################################################################################

      result$NAs[generations]           <- NAs

      best                              <- y[1]
      # best                              <- min(y)

      ######### if Constraints
      if(control$constraint){
        best                                <- bestFeasible$y
        consBest                            <- bestFeasible$constraint
        result$consBesthistory[generations] <- consBest
        if(!is.empty(feasibleRelax)){

          bestRel                           <- min(yForResults[feasibleRelax])
          consBestRel                       <- constraintForResults[feasibleRelax[which.min(yForResults[feasibleRelax])]]


        }
        else{
          bestRel                           <- NA
          consBestRel                       <- NA
        }
      }

      result$yForResults                    <- yForResults
      result$ybesthistory[generations]      <- best


      control$tolerance=.01*min(y)
      ######### Stalling

      if(control$constraint){

        lastLocal    <- max(which(result$localOpt==TRUE))
        lastLocal    <- ifelse(lastLocal==1,0,lastLocal)
        ytoConsider  <- result$ybesthistoryFeas[lastLocal:generations]

        if(min(ytoConsider) <= stallRef - control$tolerance){

          stallRef <- min(ytoConsider)

          result$stalling       <-  stalling <- 0

        } else
          result$stalling       <-  stalling <- stalling + 1


      }else{

        lastLocal    <- max(which(result$localOpt==TRUE))
        # lastLocal    <- ifelse(lastLocal==1,0,lastLocal)
        ytoConsider  <- result$ybesthistory[lastLocal:generations]

        if(min(ytoConsider) <= stallRef - control$tolerance){

          stallRef <- min(ytoConsider)

          result$stalling       <-  stalling <- 0

        } else
          result$stalling       <-  stalling <- stalling + 1

      }

      if(is.infinite( stalling ))
        result$stalling       <-  stalling <- 0

      if (control$saveX)
        result$x[[generations]]         <- x

      if (control$saveSigma)
        result$sigma[[generations]]     <- sigma

      if(generations==1)
        evaluations        <- evaluations + control$size
      else
        evaluations        <- evaluations + control$sizeToEval

      result$evaluations[generations]    <- evaluations
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

        result <- Output(best=best,bestRel=bestRel, control,consBest=consBest,consBestRel=consBestRel,constList=constList,evaluations=evaluations,
                         eval= result$evaluations, fitness=fitness,generations=generations,identicX=identicX,media=media, NAs=NAs, result=result, y=yForResults,x=x,sigma=sigma,sigma0=sigma0,stalling=stalling, pb=pb,cRef=control$cRef)
      }
      ####### increment generations
      generations    <- generations + 1
      print(x[which.max(fitness)])

    # if(!exists("posOld"))
    # posOld=NULL
    # posOld <- plotWindFarm(x[[which.max(fitness)]],posOld)
    #
    }
    ########## Finalize the output ############################################################################################
    if(control$constraint){

      suppressWarnings(summaryDf <- data.frame(result$a,result$consBesthistory,control$cRef,result$evaluations, result$NAs,control$seed))
      colnames(summaryDf) = c("yBest", "constBest","cRef","evaluations","NAs","seed")

    }else{

      suppressWarnings( summaryDf <- data.frame(result$ybesthistory[!is.na(result$ybesthistory)],result$evaluations, result$NAs[!is.na(result$ybesthistory)],control$seed))
      colnames(summaryDf) = c("yBest","evaluations","NAs","seed")

    }

    result$evaluations      <- evaluations
    result$exitMessage      <- "Optimisation did not exceeded maximum function evaluations"
    result$control          <- control
    result$lastSigma        <- sigma
    result$lastX            <- x
    result$newPop           <- newPop
    result$sigma0           <- sigma0
    result$summary          <- summaryDf


    if(control$constraint){
      result$xbest            <- bestFeasible$x
      result$ybest            <- bestFeasible$y
    }  else{
      result$xbest            <- x[which(yForResults==best)]
      result$ybest            <- best
    }



    if(control$constraint)
      result$consBest <- consBest


    return(result)
  }
