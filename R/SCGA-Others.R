analyseOperationPerformance <- function(operator,list,evaluateFun){

  list2env(list,envir = environment())
  ynew <- evaluateFun(toEvaluate)
  if(operator=="mutation"){
    performance <- sum (yOld > ynew)/length(yOld)
    return(performance)
  }
  if(operator=="crossover"){
    CrossPool <- CrossPool[sort(rep(1:nrow(CrossPool),2))[1:length(toEvaluate)],]
    yOld      <- apply(CrossPool, 1, function(i) min(yOld[i[1]],yOld[i[2]]))
    performance <- sum (yOld > ynew)/length(yOld)
    return(list(ynew,performance))
  }

}

progressBarCreate <- function(control)
  progress::progress_bar$new(total = control$maxEvaluations,format = "  optimising [:bar] :percent eta: :eta", clear = TRUE, width= 60)
RestartFromBackup <- function(resumeFrom,newPop){             #Function still To be checked
  load(paste0(resumeFrom,".RData" ))
  result                   <- back
  rm(back)
  evaluations              <- result[["evaluations"]][(max(which(!is.na(result[["evaluations"]]))))-1]
  ybesthistory             <- result[["ybesthistory"]]
  xbesthistory             <- result[["xbesthistory"]]
  yForResults              <- result[["yForResults"]]
  y                        <- result[["y"]]
  x                        <- result[["x"]]
  plots                    <- result[["plots"]]
  set.seed(result$control$seed)
  resuming                 <-  T
  if(result$control$saveAll){
    y                        <-  y[[max(which(!is.na(ybesthistory)))]]
    newPop                   <-  x[[max(which(!is.na(xbesthistory)))]]
  } else
    newPop[[1]]            <-  xbesthistory[[max(which(!is.na(xbesthistory)))]]

  generations              <-  length(ybesthistory[!is.na(ybesthistory)])
  # sigma0                 <- sigma[[1]]
  env                      <- mget(ls(), envir = environment())

  return(env)

}

EvalCand2Operator<- function(newPop,control){
  if (is.null(control$dontChangeCross)) {

    avglength     <- (length(unlist(newPop)) / ncol(newPop[[1]])) / control$size

  } else {

    avglength     <- round(mean(sapply(newPop,function(x)
      sum(x[,"feature"] %in% setdiff(x[,"feature"],control$dontChangeCross))
    )))
  }



  ChangeCross          <- round((avglength / 2) * control$maxChange)

  if (is.null(control$dontChangeMut)) {

    avglength     <- (length(unlist(newPop)) / ncol(newPop[[1]])) / control$size

  } else {

    avglength     <- round(mean(sapply(newPop,function(x)
      sum(x[,"feature"] %in% setdiff(x[,"feature"],control$dontChangeMut))
    )))
  }


  ChangeMut          <- round((avglength / 2) * control$maxChange)
  return(list(ChangeCross=ChangeCross,ChangeMut=ChangeMut))
}



assignFitnessProportional <- function(y) {
  ## scale the observation between 0 and 1. assigning 1 to the lowest value and 0 to the highest -> Minimization
  minumProb=0.05
  # fitness = -y
  # fitness = fitness - min(fitness, na.rm = TRUE)+minumProb
  #
  # fitness = fitness / (max(fitness, na.rm = TRUE)-min(fitness, na.rm = TRUE) + minumProb)
  fitness <- scales::rescale(-y)
  fitness[which(is.na(fitness))]=minumProb

  return(fitness)
}


assignFitnessRank <- function(y) {
  # rank the solutions
  fitness <- match(y,sort(y,na.last = FALSE,decreasing = T))
  return(fitness)
}


tourselection <- function(x,tsize){

  selected    <- Inf
  while (is.infinite(selected)) {
    tour      <- sample(length(x),tsize)
    winners   <- seq(1,length(x))[which(x==max(x[tour],na.rm = T))]

    if(length(winners)>1)
      winners <- sample(winners,1)

    selected  <- winners

    if(length(selected)>1)
      selected  <- Inf

  }

  return(selected)
}

selectpoolTournament<- function(fitness,size = (floor(length(fitness) / 2) + 1),tsize=max(ceiling(length(fitness)/10),2),...){

  newpool=matrix(ncol=2,nrow=size)
  for (i in 1:size){

    newpool[i,1] =tourselection(fitness,tsize)
    while ( is.na(newpool[i,2])) {

      newpool[i,2] =tourselection(fitness[-newpool[i,1]],tsize)
      # newpool[i,2]=ifelse(newpool[i,1]<newpool[i,2] ,newpool[i,2]+1,newpool[i,2])
      if(newpool[i,2]==newpool[i,1])
        newpool[i,2]=NA
    }
  }

  return(newpool)
}

selectPoolRouletteWheel <- function(fitness, size = (floor(length(fitness) / 2) + 1),...) {



  newpool=matrix(ncol=2,nrow=size) #initiliaze the matrix
  choices <- 1:length(fitness)    #create the indexes
  newpool[,1] <-sample(  length(fitness), size, replace = TRUE, prob = fitness) # assign values to the first column. prob is the fitness
  for (j  in 1:size) { #assign values to the second column.the value of the correspondent first column is excluded
    newpool[j,2] <-
      sample(
        (choices[-newpool[j,1]]),
        1,
        replace = TRUE,
        prob = fitness[-newpool[j,1]]
      )

  }
  return(newpool)
}
selectBest <- function(fitness, size = (floor(length(fitness) / 2) + 1),...) {

  nbest=size
  newpool=matrix(ncol=2,nrow=size) #initiliaze the matrix
  choices <- 1:nbest    #create the indexes
  indexes <- order(fitness,decreasing=T)[choices]
  fitness <- fitness[indexes]
  newpool[,1] <- sample( nbest, size, replace = TRUE, prob = fitness ) # assign values to the first column. prob is the fitness
  for (j  in 1:size) { #assign values to the second column.the value of the correspondent first column is excluded
    newpool[j,2] <-
      sample(
        (choices[-newpool[j,1]]),
        1,
        replace = TRUE,
        prob = fitness[-newpool[j,1]]
      )

  }
  newpool[,1] <- indexes[newpool[,1]]
  newpool[,2] <- indexes[newpool[,2]]
  return(newpool)
}


OptimizerClass<- function(job=NULL,resumeFrom=NULL,control){
  # Create  class

  result <- list(lastX              = NULL,
                 lastSigma          = NULL,
                 sigma0             = NULL,
                 observations       = NULL,
                 xbest              = NULL,
                 ybest              = Inf,
                 y                  = rep(list(NA),control$maxGenerations),
                 evaluations        = NULL,
                 sigma              = rep(list(NA),control$maxGenerations),
                 control            = control,
                 job                = job,
                 performance        = matrix(NA,control$maxGenerations,2,dimnames = list(NULL,c("crossover","mutation"))),
                 resumeFrom         = resumeFrom,
                 NAs                = rep(NA,control$maxGenerations),
                 ybesthistory       = rep(NA,control$maxGenerations),
                 localOpt           = c(TRUE,rep(FALSE,control$maxGenerations-1)),
                 sigma              = rep(list(NA),control$maxGenerations),
                 x                  = rep(list(NA),control$maxGenerations),
                 xbesthistory       = rep(list(NA),control$maxGenerations),
                 plots              = list(population= list(generations=NULL, plot=NULL),
                                           sigma= list(generations=NULL, plot=NULL))
  )
  class(result) <- "SCGAClass"

  return(result)
}

initSigma <- function(feat,donttouch){

  active=as.numeric(setdiff(getValues(x=feat, name = "label", Unique = F),donttouch))
  feature <- feat[active]
  nVar    <- NULL
  nVar[1] <- sum(getValues(x=feature, name = "type", Unique = F) == "numeric")
  nVar[2] <- sum(getValues(x=feature, name = "type", Unique = F) == "integer")
  nVar[3] <- sum(getValues(x=feature, name = "type", Unique = F) == "categorical")
  nVar[4] <- sum(getValues(x=feature, name = "type", Unique = F) == "repeater")


  tau <- updateTau(nVar)
  bounds <- sapply(1:length(feat), function(i) feat[[i]]$bound(),simplify = FALSE)
  # bounds <- apply(as.matrix(active), 1,getBounds)
  # boundsCorr <- matrix(0,length(active),2)
  boundsCorr <- matrix(0,length(feat),2)
  for ( i in active){
    boundsCorr[i,] <- c(min(bounds[[i]]),max(bounds[[i]]))
  }

  ranges <- numeric(length(feat))
  ranges<- boundsCorr[,2]- boundsCorr[,1]

  ireal <-
    which(getValues(x=feat, "type", Unique = FALSE) == "numeric")
  iint <-
    which(getValues(x=feat, "type", Unique = FALSE) == "integer")
  icat <-
    which(getValues(x=feat, "type", Unique = FALSE) == "categorical")
  irep <-
    which(getValues(x=feat, "type", Unique = FALSE) == "repeater")


  sigma0 <- numeric(length(feat))

  sigma0[ireal] <- ranges[ireal] * 0.1 * .5

  sigma0[c(iint, irep)] <- ranges[c(iint, irep)] * 0.33

  sigma0[icat] <- 0.1
  sigma0 <- c(sigma0,tau=tau)
  return(sigma0)
}
updateTau<- function(nVar){
  tau=NULL

  tau[1] <- 1 / sqrt(2 * nVar[1])   #global learning rate tauReal
  tau[2] <- 1 / sqrt(2 * nVar[2])   #global learning rate tauInt
  tau[3] <- 1 / sqrt(2 * nVar[3])   #global learning rate tauCat
  tau[4] <- 1 / sqrt(2 * nVar[4])   #global learning rate tauRep

  tau[5] <- 1 / sqrt(2 * sqrt(nVar[1]))   #local learning rate tauRealDash
  tau[6] <- 1 / sqrt(2 * sqrt(nVar[2]))   #local learning rate tauIntDash
  tau[7] <- 1 / sqrt(2 * sqrt(nVar[3]))   #local learning rate tauCatDash
  tau[8] <- 1 / sqrt(2 * sqrt(nVar[4]))   #local learning rate tauRepDash
  return(tau)
}

str2vec <- function(str1){
  return(as.integer(unlist(strsplit(str1, ","))))

}

getValues <- function(x, name="label", Unique = TRUE,forC=NA) {
  # return the values checkingg recursevely in the list

  if (is.na(forC)) {
    if (name == "value") {
      Unique <- FALSE
      out <-
        unname(as.numeric(unlist(x)[which(grepl(name, names(unlist(x))))]))
    } else {
      if (Unique)
        out <-
          unique(unname(unlist(x)[which(grepl(name, names(unlist(x))))]))
      else
        out <- unname(unlist(x)[which(grepl(name, names(unlist(x))))])
    }

  } else{
    out <-
      any(unique(unname(unlist(x)[which(grepl(name, names(unlist(x))))])) == forC)
  }

  return(unlist(out))
}
constraintHandlerFitness <- function(y,con,cRef,evaluations, control,...){

  ###### Define worst in current population  ########
  wY               <- max(y,na.rm = T)
  # wC               <- max(con,na.rm = T)
  wC               <- mean(con,na.rm = T)

  ###### Handling NA ########
  con[is.na(y)]    <- wC + (mean(c(con[!is.na(y)])))
  y[is.na(y)]      <- wY + (mean(y[!is.na(y)]))

  ###### Penalise not feasible solutions ########

  feasible        <- which( con <= cRef )
  notFeasible     <- which( con > cRef )
  # browser()
  if(!is.empty(feasible))
    wY               <- max(y[feasible])

  ###### Evaluate relaxCRef ########

  if(control$pureFeasibility != 1 && control$maxRelaxation!=0){
    relaxedCRef      <- calcCurrCref(evaluations, control$maxEvaluations,control$pureFeasibility ,control$maxRelaxation,cRef)

    if(is.na( relaxedCRef) || is.infinite( relaxedCRef) )
      relaxedCRef    <- cRef

  } else
    relaxedCRef      <- cRef

  relaxedCRef        <- max(relaxedCRef ,cRef)

  if(!is.empty(feasible))
    feasibleRelax    <- which( con <= relaxedCRef & con > cRef & y < min(y[feasible]))
  else
    feasibleRelax    <- which( con <= relaxedCRef & con > cRef )

  unfeasibleRelax    <- which(con > relaxedCRef)

  ###### Assign fitness ########

  scaledCons         <- pmax(con[unfeasibleRelax] / wC * wY,wY)
  y[unfeasibleRelax] <- scaledCons + y[unfeasibleRelax] # probblema qui : i non fieasible lono troppo piccoli

  # fitness            <- assignFitnessProportional(y)
  fitness            <- assignFitnessRank(y)

  # fitness[fitness<quantile(fitness,.05)] <- quantile(fitness,.05)
  ###### create a resuming vector ########
  resFeas                                <- rep("unfeasible",length(y))
  resFeas[feasibleRelax]                 <- "feasibleRelax"
  resFeas[feasible]                      <- "feasible"
  resFeas                                <- as.factor(resFeas)

  return(list(fitness=fitness,wC=wC,wY=wY,constraint=con,feasible=feasible,feasibleRelax=feasibleRelax,resFeas=resFeas,relaxedCRef=relaxedCRef))

}
calcCurrCref <- function(evaluations, maxEvalutions,pureFeasibility ,maxRelaxation,cRef){

  evaluationsPureFeasible <- ( 1 - pureFeasibility) * maxEvalutions
  MaxCRefRelaxed          <- ( 1 + maxRelaxation) * cRef

  return(
    MaxCRefRelaxed - (((MaxCRefRelaxed - cRef) / (evaluationsPureFeasible)) * evaluations)
  )

}# order <- function(x,todo=x[,"id"]){
#   done = x[!x[,"prec"]%in%todo,"id"]
#   x[setdiff(todo,done),]sapply(done ,order,x=x)
#
#s
#
# }
#
# checkIdentical <- function(newPop,...){
#   toMutate=NULL
#
#   for (i in (length(newPop)-1):1) {
#     for(j in (i+1):(length(newPop))){
#
#       if(identical(newPop[[i]][,-c(3,4)],newPop[[j]][,-c(3,4)])){
#         toMutate=c(toMutate,j)
#         break()
#       }
#
#     }
#
#   }
#   return(toMutate)
#
# }


checkIdentical <- function(newPop,toCompare,elitism){
  toMutate=NULL
  toMutate = sapply(1:(length(newPop)-1), function(i){ any(sapply (toCompare[(i+1):length(toCompare)],function(toCompare ,newPop) {identical(toCompare[,-c(3,4)],newPop)},newPop[[i]][,-c(3,4)] ))})
  toMutate = which(toMutate)
  toMutate = toMutate[toMutate>elitism]

  return(toMutate)

}





















