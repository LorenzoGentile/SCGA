MutationDF <- function(X,pop,feature,maxMuting=Inf,sigmas=sigma0,createFun,
                           dontChange=dontChange, replicates=F,repairMut=NULL,
                           updateSigma,...) {

  index       <- X
  x           <- pop
  back        <- x <- x[[index]]
  sigmas      <- sigmas[index,]
  avoid<- NULL

  possible    <- setdiff(unique(x[,"feature"]),dontChange)
  repetition  <- probability <- rep(1, times = max(as.integer(getValues(feature, name = "label")))) #deafault
  probability <- probability[possible]
  repetition  <- repetition[possible]

  if (replicates){
    r <- rle(sort(x[,"feature"]))
    repetition<- r[[1]][match(possible,r[[2]])]
  }

  maxMuting   <- round(maxMuting * rnorm(1,1,.2))
  exchanges   <- min(sum(repetition),maxMuting)

  if (length(possible) > 1)
    featuretochange <- sample(possible,exchanges,prob = probability*repetition,replace = T)

  else
    featuretochange <- rep(possible,exchanges)


  if(updateSigma)
    sigmas <- updatesigmas(sigmas,unique(featuretochange),feature)

  for (i in featuretochange) {

    rows <- getIndexMut(x,i,avoid)
    row <- which(x[,"id"]==rows)

    if(is.empty(row))
      break

    x.copy=x
    x <- mutateDepDF( x=x ,i=i,feature=feature,row=row,sigmas=sigmas,createFun,...)

    if(any(is.na(x[,"value"]))){
      print("ERROR in Mutation: NA created")
      break
    }

    avoid <- c(avoid,CrossDF(x,rows))
  }

  if(!is.null(repairMut))
    x <- repairMut(x,feature)

  return(list(x,sigmas))
}

mutateDepDF <- function(x,i, feature,row,sigmas,createFun,...) {

  if (feature[[i]]$type == "numeric")
    x <- MutateRealDF(x, i, feature,row,sigmas,createFun,...)

  else if (feature[[i]]$type == "integer")
    x <- MutateIntDF(x, i, feature,row,sigmas,createFun,...)

  else if (feature[[i]]$type == "categorical")
    x <- MutateCatDF(x, i, feature,row,sigmas,createFun,...)

  else if (feature[[i]]$type == "repeater")
    x <- MutateRepDF(x, i, feature,row,sigmas,createFun,...)

  return(x)
}

MutateRealDF <- function(x,i,feature,row,sigmas,createFun,report,...){
  report=FALSE
  sigmaDash <- sigmas[i]
  if(report){

    mutationReport               <-  get("mutationReport",envir = .GlobalEnv)                # report
    nextReport                   <- nrow(mutationReport) +1                                  #report
    mutationReport               <- rbind(mutationReport,matrix(NA,1,5))
    mutationReport[nextReport,1] <- x[row,"value"]                                           #report
    mutationReport[nextReport,4] <- i                                                        #report
    mutationReport[nextReport,5] <- sigmaDash                                                #report

  }

  bounds                         <- feature[[i]]$bound(x=x, id=x[row,"id"])

  x[row,"value"]                 <-  as.numeric( x[row,"value"]) + sigmaDash * rnorm(1,0,1)

  if(report){
    mutationReport[nextReport,2] <- x[row,"value"]                                            #report
  }

  x[row,"value"]                <- min( bounds[2],x[row,"value"])

  x[row,"value"]                <- max( bounds[1],x[row,"value"])

  if(report){

    mutationReport[nextReport,3] <- x[row,"value"]                                         #report
    assign("mutationReport",mutationReport, envir = .GlobalEnv)                           #report
  }

  x                            <- muteAllDep(row,x,feature,sigmas,createFun,...)

  return(x)

}

MutateCatDF <- function(x,i,feature,row,sigmas,createFun,report,...){

  sigmaDash                      <- sigmas[i]
  report                         <- FALSE

  if(report){

    mutationReport               <-  get("mutationReport",envir = .GlobalEnv)         # report
    nextReport                   <- nrow(mutationReport) +1                           # report
    mutationReport               <- rbind(mutationReport,matrix(NA,1,5))
    mutationReport[nextReport,1] <- x[row,"value"]                                    # report
    mutationReport[nextReport,4] <- i                                                 # report
    mutationReport[nextReport,5] <- sigmaDash                                         # report
  }

  firstmatr                      <- CrossDFRet(x,x[row,"id"])
  firstmatr                      <- x [which(x[,"id" ] %in% firstmatr),,drop=FALSE]



  bounds                         <- feature[[i]]$bound(x=x, id=x[row,"id"])

  if (length(bounds) > 1)
    bounds                      <- setdiff(bounds,x[row,"value"])

  u                             <- runif(1)
  inds <- (u < sigmaDash)

  if (inds){

    if(length(bounds>1))
      x[row,"value"]            <- sample(bounds,1)                        # to be changed if parallelized

    else if(length(bounds==1))
      x[row,"value"]           <- bounds

    x <- muteAllDep(row,x,feature,sigmas,createFun,...)
  }

  if(report){

    mutationReport[nextReport,3] <- x[row,"value"]                              #report
    mutationReport[nextReport,2] <- x[row,"value"]                              #report
    assign("mutationReport",mutationReport, envir = .GlobalEnv)

  }

  return(x)
}

MutateRepDF <- function(x, i, feature, row,sigmas,createFun,...) {

  x[row, "value"] <- mutateIntegerValueDF (x[row, "value"] , i, feature,sigmas,X=x,row=row)
  # if( is.na(x[row, "value"]))
  #   browser()
  diff <- x[row, "value"] - length(which(x[,"prec"] == x[row, "id"]))

  if (diff < 0) {
    if(length(which(x[,"prec"] == x[row, "id"]))==1)
      toRemove <- x[which(x[,"prec"] == x[row, "id"]), "id"]
    else
      toRemove <- x[sample(which(x[,"prec"] == x[row, "id"]), abs(diff)), "id"]
    while (!bazar::is.empty(toRemove)) {
      nextToRemove <- x[which(x[, "prec"] %in% toRemove), "id"]
      x <- x[-which(x[, "id"]  %in% toRemove), ]
      toRemove <- nextToRemove
    }
  } else if(diff > 0){
    # will create a new row with dependent from the current
    for (k in 1:diff) {
      x <- rbind(x,createFun(feature,feature[[i]]$dependent,id = max(x[, "id"]) + 1,
                             prec = x[row, "id"],x=x,...))
      # x <- rbind(x[1:row,],createFun(feature,feature[[i]]$dependent,id = max(x[, "id"]) + 1,
      #         prec = x[row, "id"],x=x,...),x[(row+1):nrow(x),])
    }
  }
  return(x)
}

MutateIntDF <- function (x,i ,feature,row,sigmas,createFun,...){
  x[row,"value"] <- mutateIntegerValueDF (x[row,"value"] , i, feature,sigmas,X=x,row=row,...)
  x <- muteAllDep(row,x,feature,sigmas,createFun,...)
  return(x)
}

mutateIntegerValueDF <- function(x,i,feature,sigmas,X,row,report,...){
  ######Just for this problem
  report=FALSE
  firstmatr<- CrossDFRet(X,X[row,"id"])
  firstmatr<-X [which(X[,"id" ]%in% firstmatr),,drop=FALSE]

  sigmaDash <- sigmas[i]
  #############
  if(report){
    mutationReport               <- get("mutationReport",envir = .GlobalEnv) # report

    nextReport                   <- nrow(mutationReport) +1                    #report
    mutationReport = rbind(mutationReport,matrix(NA,1,5))
    mutationReport[nextReport,1] <- x                                        #report
    mutationReport[nextReport,4] <- i                                        #report
    mutationReport[nextReport,5] <- sigmaDash                                #report
  }
  bounds <- feature[[i]]$bound( x=X, id=X[row,"id"])

  feat <- getValues(feature,name="type",Unique = FALSE)

  nint <- length(which(feat==feat[i]))
  nint <- 1
  p <- sigmaDash/nint

  p <- 1-p/(1+sqrt(1+p^2))

  p <- max(p,1e-3)

  G1 <- rgeom(nint,prob=p)

  G2 <- rgeom(nint,prob=p)

  x <- as.numeric(x) + G1-G2
  if(report){
    mutationReport[nextReport,2] <- x                                       #report
  }
  #individual[iint] <- Tlu(newval,lower[iint],upper[iint]) #fix solution parameters to bounds with transformation
  x <- min( bounds[2],x) #fix solution parameters to bounds #TODO: Tab(x) implementation

  x <- max( bounds[1],x)
  if(report){
    mutationReport[nextReport,3] <- x                                         #report
    assign("mutationReport",mutationReport, envir = .GlobalEnv)               #report
  }
  return(x)

}

muteAllDep <- function(row,x,feature,sigmas,createFun,...){

  featOfRow <- x[row,"feature"]
  dependencesAll<- feature[[featOfRow]]$dependent
  if(!anyNA(dependencesAll)){
    idDepExisting <-  x[which(x[,"prec"]==x[row,"id"]),"id"]
    depNotExisting <- setdiff (dependencesAll,x[which(x[,"id"] %in%idDepExisting ),"feature"])

    i= x[row,"feature"]
    for (k in idDepExisting) {
      create=feature[[i]]$condOfExistence(dependent=x[which(x[,"id"]==k),"feature"])
      if(is.na(create) || x[row, "value"]==create )
        x <- mutateDepDF(x=x,i=as.numeric(x[which(x[,"id"]==k),"feature"]),feature=feature,row=which(x[,"id"]==k),sigmas,createFun,...)
      else
        x=x[-which(x[,"id"]==k),,drop=FALSE]
    }

    for (k in depNotExisting){
      create=feature[[i]]$condOfExistence(dependent=k)
      if(is.na(create)|| x[row, "value"]==create ){
        # count=get("count",envir=.GlobalEnv)
        # assign("count",count+1,envir=.GlobalEnv)

        newRow <- createFun(feature,k,id = max(x[, "id"]) + 1,
                            prec = x[row, "id"],x=x,...)
        x <- rbind(x,newRow)
      }

    }
  }
  return(x)
}

getIndexMut <- function(x.,i,avoid){

  row1 <-x.[which(x.[,"feature"] == i),"id"]
  row <- setdiff(row1,avoid)
  if (bazar::is.empty(row))
    row <- row1
  if (length(row)>1)
    row <- sample(row ,1)
  return(row)
}

updateNFeatures<- function(pop,len=3,...){
  return(apply(as.matrix(1:len), 1,function(X,pop){sum(pop[,"feature"]==X)},pop=pop))
}

updatesigmas <- function(sigmas,toUpdate,feature){


  types <- getValues(x=feature[toUpdate],name="type",Unique = FALSE)

  icat  <- toUpdate[which(types=="categorical")]
  ireal <- toUpdate[which(types=="numeric")]
  iint  <- toUpdate[which(types=="integer")]
  irep  <- toUpdate[which(types=="repeater")]

  Nc    <- rnorm(1,0,1)

  if (!is.empty(ireal)){
    tauReal       <- sigmas["tau1"]
    tauRealDash   <- sigmas["tau5"]
    sigmas[ireal] <- sigmas[ireal] * exp(tauReal * Nc + tauRealDash * rnorm(length(ireal),0,1))
  }
  if (!is.empty(iint)){
    tauInt        <- sigmas["tau2"]
    tauIntDash    <- sigmas["tau6"]
    sigmas[iint]  <- pmax(1,sigmas[iint] * exp(tauInt * Nc + tauIntDash * rnorm(length(iint),0,1)))
  }
  if (!is.empty(irep)){
    tauRep        <- sigmas["tau4"]
    tauRepDash    <- sigmas["tau8"]
    sigmas[irep]  <- pmax(1,sigmas[irep] * exp(tauRep * Nc + tauRepDash * rnorm(length(irep),0,1)))
  }
  if (!is.empty(icat)){
    tauCat       <- sigmas["tau3"]
    tauCatDash   <- sigmas["tau7"]

    sigmas[icat] <- 1/(1+((1-sigmas[icat])/sigmas[icat] * exp(-tauCat * Nc - tauCatDash * rnorm(length(icat),0,1))))

    sigmas[icat] <-  pmin(0.5,sigmas[icat])

    sigmas[icat] <-  pmax(1/(3*length(icat)),sigmas[icat])
  }

  return(sigmas)
}

NewValueMutation <-
  function(feature, i, id, prec = NA, y = NULL, x=NULL,addnames = NULL,...) {
    names = c("value", "feature", "prec", "id", addnames)
    y = matrix(c(NA, i, prec, id, rep(NA, length(names) - 4)), 1, length(names))
    colnames(y) <- names

    for (k in addnames) {

      if (!is.null(feature[[i]][[k]])){
        addfeat <- ifelse(is.function(feature[[i]][[k]]),feature[[i]][[k]](i=i, y=rbind(x,y), id=id),feature[[i]][[k]])
        y[,k] <- addfeat
      }
      else
        y[,k] <- NA
    }

    bounds <- feature[[i]]$bound(x=rbind(x,y), id=id)

    if (feature[[i]]$type == "numeric") {
      y[, "value"] = runif(1, bounds[1], bounds[2])
    } else if (feature[[i]]$type == "dummy") {
      y[, "value"] = bounds[1]
    } else if (feature[[i]]$type == "categorical") {
      if(length(bounds)>1)
        y[, "value"] = sample(bounds,1)
      else
        y[, "value"] = bounds
    } else {
      y[, "value"] = floor(runif(
        1,
        bounds[1],
        bounds[2] + 1 - .Machine$double.eps
      ))
    }
    if (!anyNA(feature[[i]]$dependent)) {
      dependence= ifelse(feature[[i]]$type == "repeater", y[1, "value"],1)

      for (k in numeric(dependence)){
        for (j in 1:length(feature[[i]]$dependent)) {
          y <-
            rbind(y, createDepDF(   feature = feature,   i = feature[[i]]$dependent[j],   id = id + 1,
                                    prec = y[1, "id"],   x=rbind(x,y),   addnames=addnames ))
          id = max(0, y[, "id"])
        }
      }
    }
    return(y)
  }
