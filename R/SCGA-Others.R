RestartFromBackup <- function(resumeFrom){             #Function still To be checked
  load(paste0(resumeFrom,".RData"))
  return(list(
    result         = back,
    fromResuming   = length(result$ybesthistory),
    newPop         = result$pop[[fromResuming]],
    generations    = fromResuming,
    yForResults    = result$yForResults,
    y              = result$obs[[fromResuming]],
    ybesthistory   = result$ybesthistory,
    NAs            = result$NAs ,
    sigma          = result$sigma ,
    best           = min(yForResults, na.rm = TRUE),
    stalling       = result$stalling,
    result         = result,
    control        = control
  )
  )
}

EvalCand2Operator<- function(newPop,control){
  if (is.null(control$dontChangeCross)) {

    avglength     <- (length(unlist(newPop)) / ncol(newPop[[1]])) / control$size

  } else {

    avglength     <- round(mean(apply(as.matrix(1:length(newPop)), MARGIN = 1, FUN = function(i, x) {
      sum(!(x[[i]][, "feature"] %in% control$dontChangeCross))  }, x = newPop )))
  }



  ChangeCross          <- round((avglength / 2) * control$maxChange)

  if (is.null(control$dontChangeMut)) {

    avglength     <- (length(unlist(newPop)) / ncol(newPop[[1]])) / control$size

  } else {

    avglength     <- round(mean(apply(as.matrix(1:length(newPop)), MARGIN = 1, FUN = function(i, x) {
      sum(!(x[[i]][, "feature"] %in% control$dontChangeMut))  }, x = newPop )))
  }


  ChangeMut          <- round((avglength / 2) * control$maxChange)
  return(list(ChangeCross=ChangeCross,ChangeMut=ChangeMut))
}
InitPopAndSigma <- function(control,feature,LAPPLY){

  if(is.null(control$newPop)){

    ########## Create population
    suppressWarnings( newPop <- control$popCreateFun(feature,size = control$size,
                                                     control$createCandFun,addnames = control$keep)
    )

    ########## Repair
    if(!is.null(control$repairFun))

      newPop        <- LAPPLY(X=newPop, control$repairFun,budgetTot=control$budgetTot)
  } else{

    newPop          <- control$newPop

  }


  if(is.null(control$sigma)){

    ########## Sigma
    if(is.null(control$sigma0))
      sigma0        <- initSigma(feature, control$dontChangeMut)
    else
      sigma0        <- control$sigma0

    Names           <- names(sigma0)
    sigma           <- matrix(rep(sigma0, control$size), control$size, , byrow = T)
    colnames(sigma) <- Names

  } else{

    sigma         <- control$sigma

  }
  return(list(
    newPop = newPop,
    sigma  = sigma,
    sigma0 = sigma0

  ))
}

evaluatePopDF <- function(Fun, x, vectorOnly=FALSE, vectorized=FALSE, SAPPLY, ...) {

  if(vectorized && vectorOnly )
    y <- Fun(x[1:length(x)][,"value"],...)

  else if (vectorized && !vectorOnly )
    y <- Fun(x,...)

  else if (!vectorized && !vectorOnly )
    y <- SAPPLY( X = x,Fun,...)

  else if (!vectorized && vectorOnly )
    y <- SAPPLY( X = x[1:length(x)][,"value"], Fun,...)

  return(y)

}

assignFitnessProportional <- function(y, x) {

  ## scale the observation between 0 and 1. assigning 1 to the lowest value and 0 to the highest -> Minimization
  minumProb=0.05
  y = -y
  y = y - min(y, na.rm = TRUE)+minumProb

  y = y / (max(y, na.rm = TRUE)-min(y, na.rm = TRUE) + minumProb)
  y[which(is.na(y))]=minumProb

  return(y)
}


assignFitnessRank <- function(y, x) {
  # rank the solutions
  y <- match(y,sort(y,na.last = FALSE,decreasing = T))
  return(y)
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
  newpool[,1] <- # assign values to the first column. prob is the fitness
    sample(
      length(fitness),
      size,
      replace = TRUE,
      prob = fitness
    )
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


OptimizerClass<- function(job=NULL,resumeFrom=NULL,control){
  # Create  class

  result <- list(lastX              = NULL,
                 lastSigma          = NULL,
                 sigma0             = NULL,
                 observations       = NULL,
                 xbest              = NULL,
                 ybest              = Inf,
                 y                  = NULL,
                 evaluations        = NULL,
                 sigma              = list(),
                 control            = control,
                 job                = job,
                 resumeFrom         = resumeFrom,
                 NAs                = rep(NA,control$maxGenerations),
                 ybesthistory       = rep(NA,control$maxGenerations),
                 sigma              = rep(list(NA),control$maxGenerations),
                 x                  = rep(list(NA),control$maxGenerations),
                 plots              =list(population= list(generations=NULL, plot=NULL),
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
  ranges[active] <- boundsCorr[,2]- boundsCorr[,1]

  ireal <-
    which(getValues(x=feat, "type", Unique = FALSE) == "numeric")
  iint <-
    which(getValues(x=feat, "type", Unique = FALSE) == "integer")
  icat <-
    which(getValues(x=feat, "type", Unique = FALSE) == "categorical")
  irep <-
    which(getValues(x=feat, "type", Unique = FALSE) == "repeater")


  sigma0 <- numeric(length(feat))

  sigma0[ireal] <- ranges[ireal] * 0.1

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
