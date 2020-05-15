LocalOptimisation <- function(control,feature,newPop,y,active,evaluations,sigma,result,generations,...){
  if(is.null(control$varForLocalOpt))                                                                    # Choose feature to use in the local optimisation
    RealOptVar=which(getValues(x=feature, name = "type", Unique = F) == "numeric")                          # Choose feature to use in the local optimisation
  else                                                                                                   # Choose feature to use in the local optimisation
    RealOptVar=control$varForLocalOpt                                                                    # Choose feature to use in the local optimisation


  x0 <- newPop[[1]]                                                                                      # Starting point
  y0 <- abs(y[1])                                                                                             # Starting point


  localActive      <- which(x0[,"feature"] %in% active[RealOptVar])
  localNotActive   <- setdiff(1:nrow(x0),localActive)
  localActiveFeat  <- x0[which(x0[,"feature"] %in% RealOptVar),"feature"]
  boundsLocalOpt   <- t(sapply(localActiveFeat, function(i) feature[[i]]$bound()))
  # boundsLocalOpt   <- t(apply(as.matrix(localActiveFeat), 1,getBounds))
  startPoint       <- x0[localActive,"value"] - boundsLocalOpt[,1]
  startPoint       <- startPoint /(boundsLocalOpt[,2]-boundsLocalOpt[,1])

  if(is.null(control$makeLocalObjFunction))
    localFun        <- control$Fun
  else {
    argsToPassNames <- formalArgs(control$makeLocalObjFunction)
    extraArgs       <- sapply(argsToPassNames, function(x)identical(x,"..."))
    argsToPass      <- mget(argsToPassNames[!extraArgs])
    if(any(extraArgs))
      argsToPass <- append(argsToPass,list(...))

    localFun <- do.call(control$makeLocalObjFunction,argsToPass)
  }

  objLocal<- function(x,...){
    X=x0

    xScaled= x*(boundsLocalOpt[,2]-boundsLocalOpt[,1])+boundsLocalOpt[,1]

    X[localActive,"value"] = xScaled

    out <- try(localFun(X,...)/y0)
    if(is.character(out))
      out <- 1
    return(out)

  }


  # res=optimx::optimx(par=startPoint,objLocal,method = c("L-BFGS-B","CG"),lower = rep(0,length(localActive)),upper = rep(1,length(localActive)),itnmax=1000,...)
  # newPop[[1]][localActive,"value"]=res$pn*(boundsLocalOpt[,2]-boundsLocalOpt[,1])+boundsLocalOpt[,1]
  # y[[1]]=res$value*y0
  # res$fevals

  if(control$differentiable){
    res <- optim(par=startPoint,objLocal,method = control$localMethod,lower = rep(0,length(localActive)),
                 upper = rep(1,length(localActive)),control=list(trace=0),...)


    evaluations                      <- evaluations +  res$counts[[1]] + res$counts[[2]] * 2 * length(res$par)
  }
  else{
    res <- nloptr::bobyqa(startPoint,objLocal,lower = rep(0,length(localActive)),upper = rep(1,length(localActive)),
                          control = list(stopval=control$target, xtol_rel= control$convergence,
                                         maxeval=control$maxEvaluations-evaluations),...)

    evaluations                      <- evaluations +  res$iter

  }

  y[[1]]                           <- res$value*y0
  newPop[[1]][localActive,"value"] <- res$par*(boundsLocalOpt[,2]-boundsLocalOpt[,1])+boundsLocalOpt[,1]



  if(evaluations> control$maxEvaluations){

    result$ybest          <- y[[1]]
    result$xbest          <- newPop[[1]]
    result$lastPopulation <- newPop
    result$count          <- evaluations
    result$control        <- control
    result$sigma          <- sigma
    result$ybesthistory   <- c(result$ybesthistory, y[[1]])
    result$exitMessage    <- "Optimisation exceeded maximum function Evaluations"
    result$localOptimisation <- list(generation=generations, res=res)
  }


  return(list(
    newPop      = newPop,
    y           = y,
    evaluations = evaluations,
    result      = result
  ))
}
