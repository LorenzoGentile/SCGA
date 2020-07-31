assignFitness <- function(control,newPop,y,generations,...){

  if(control$constraint){

    ## precompute the feasible and unfeasible
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
  control$geneWindow=1
  control$windowWidth = 2
  control$fitnessNiches = F
  control$minimumNiche = 3
  initFitness <- fitness
  # fitness <- 40:1
  # y = 1:40
  size = length(fitness)

  if(control$fitnessNiches){

    windowList        <- computeWindowBounds(control, generations,fitness, newPop  )

    windowBounds      <- windowList[[2]]

    lwStar            <- windowList[[1]]

    geneWindowValues  <-  sapply(newPop, function(x,i)x[x[,"feature"]==i,"value"],i=control$geneWindow)

    inWindow          <- which(geneWindowValues>=min(windowBounds) & geneWindowValues <= max(windowBounds))

    outsideWindow     <- setdiff(seq_along(fitness),inWindow)

    niechePop         <- sapply(geneWindowValues[inWindow], function(g) max(which(windowBounds<=g)))

    fitness           <- reorderFitness(fitness,niechePop,inWindow,outsideWindow,lwStar,windowBounds,size)

    nieches           <-rep(NA,size)

    nieches[inWindow] <- niechePop

    printOrd          <- order(y)
   # print(cbind(y=y[printOrd ],fitnessOld =initFitness[printOrd ],nieches[printOrd ],fitness[printOrd ]))
    attr(fitness, "niechePop") <-  nieches

  }

  return(mget(ls(),envir = environment()))
}

computeWindowBounds <- function(control, generations=1,fitness,x ){

  geneWindow         <- sapply(x, function(x,i)x[x[,"feature"]==i,"value"],i=control$geneWindow)

  lwStar             <- geneWindow[which.max(fitness)]

  windowsExtremes    <- control$feature[[control$geneWindow]]$bound()

  countCandInWind    <- 1

  windowWidth        <- control$windowWidth

  list2env(computeBounds(windowsExtremes,windowWidth,lwStar),envir = environment())

  nichesWidths       <- rep(windowWidth,inWindow)

  it=0

  while(any(countCandInWind < control$minimumNiche)){
  it = it+1
  if(it>6)
    browser()

  if(lowerWindowExtreme==(windowsExtremes[1] - .1)){
    windowBounds      <- sapply(seq_len(inWindow+1),function(i)sum(c(0,nichesWidths)[1:i])) + lowerWindowExtreme

    windowBounds[windowBounds > (windowsExtremes[2] +.1)] <-  (windowsExtremes[2] +.1)

    windowBounds      <- unique(windowBounds)

    countCandInWind <- countNichesPresence(x,control,windowBounds)
    if(any(countCandInWind<3))
      browser()

    nichesToExpand <- which(countCandInWind<control$minimumNiche)

    nichesWidths[nichesToExpand] <- nichesWidths[nichesToExpand] + 1

  } else {

        windowBounds      <- sort(- sapply(seq_len(inWindow+1),function(i)sum(c(0,nichesWidths)[1:i])) + upperWindowExtreme)

        windowBounds[windowBounds < (windowsExtremes[1] -.1)] <-  (windowsExtremes[1] -.1)

        windowBounds      <- unique(windowBounds)

        countCandInWind <- countNichesPresence(x,control,windowBounds)

        nichesToExpand <- which(countCandInWind<control$minimumNiche)


        nichesWidths[which(rev(countCandInWind)<control$minimumNiche)] <- nichesWidths[which(rev(countCandInWind)<control$minimumNiche)] + 1

    }

  }
  return(list(lwStar, windowBounds))
}
computeBounds <- function(windowsExtremes,windowWidth,lwStar){

  inWindow           <- ceiling((windowsExtremes[2]-windowsExtremes[1])*.7/windowWidth)

  lowerWindowExtreme <- max(lwStar- windowWidth * ceiling(inWindow/2)+1,windowsExtremes[1]) - .1

  upperWindowExtreme <- min(lwStar+ windowWidth * floor(inWindow/2)+1,windowsExtremes[2]) + .1


  return(mget(ls(), envir = environment())) #########
}

countNichesPresence <- function(x,control,windowBounds){
  geneWindowValues  <-  sapply(x, function(x,i)x[x[,"feature"]==i,"value"],i=control$geneWindow)

  candInwind        <- which(geneWindowValues>=min(windowBounds) & geneWindowValues <= max(windowBounds))

  niechePop         <- sapply(geneWindowValues[candInwind], function(g) max(which(windowBounds<g)))

  countCandInWind   <- sapply(sort(unique(niechePop)),function (i) sum(niechePop==i))
}


reorderFitness <- function(fitness,niechePop,inWindow, outsideWindow,lwStar,windowBounds,size){

  fitnessC                   <- fitness[inWindow]

  orderOfNieches             <- order(abs(lwStar - (windowBounds[1:(length(windowBounds)-1)]+windowBounds[2:length(windowBounds)])/2))

  orderUpdated               <- sapply(orderOfNieches, function(i) {inds <- which(niechePop==i);inds[order(fitnessC[inds],decreasing = T)]}) %>% unlist


  orderCorrect <- as.numeric(sapply(seq_along(fitnessC), function(i) sapply(orderOfNieches,
                                                                           function(pos){
                                                                             try(out <- orderUpdated[niechePop[orderUpdated]==pos][i])
                                                                             if(is.numeric(out))
                                                                               return(out)
                                                                             else
                                                                               return(NULL)})))

  orderCorrect <- orderCorrect[!is.na(orderCorrect)]
  fitness[c(inWindow[orderCorrect])] <- seq(size,by=-1,length.out = length(orderCorrect))
  fitness[outsideWindow] <- 0.01
  fitness

}
