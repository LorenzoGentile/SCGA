LocalOptimisationMatlab <- function(control,feat,newPop,y,active,evaluations,sigma,result,generations,matlab,...){

  if(is.null(control$varForLocalOpt))                                                                    # Choose feature to use in the local optimisation
    RealOptVar=which(getValues(x=feat, name = "type", Unique = F) == "numeric")                          # Choose feature to use in the local optimisation
  else                                                                                                   # Choose feature to use in the local optimisation
    RealOptVar=control$varForLocalOpt                                                                    # Choose feature to use in the local optimisation


  x0 <- newPop[[1]]                                                                                      # Starting point
  y0 <- y[1]                                                                                             # Starting point


  localActive      <- which(x0[,"feature"] %in% active[RealOptVar])
  localNotActive   <- setdiff(1:nrow(x0),localActive)
  localActiveFeat  <- x0[,"feature"][x0[,"feature"] %in% active[RealOptVar]]
  boundsLocalOpt   <- t(sapply(localActiveFeat, function(i) feature[[i]]$bound()))


  X=Lower
  X[x0[,"fixID"]]=x0[,"value"]
  x0 = X

  # x0 = x0[,"value"]
  evaluate(matlab , paste0( "x0 = [ ", paste0(x0,sep=" ", collapse = ""), "];"))
  evaluate(matlab , paste0( "lb = [ ", paste0(boundsLocalOpt[,1],sep=" ", collapse = ""), "];"))
  evaluate(matlab , paste0( "ub = [ ", paste0(boundsLocalOpt[,2],sep=" ", collapse = ""), "];"))
  evaluate(matlab , paste0( "active = [ ", paste0(localActiveFeat,sep=" ", collapse = ""), "];"))
  evaluate(matlab , paste0( "objective = @(x) ", control$functionMatlab,";"))
  evaluate(matlab, paste0("[xf,J,flag,cunt] = locopt_mga_dsm(x0 ,objective,active,lb,ub)"))

 res = getVariable(matlab, c("flag","xf","J","cunt"))

  newPop[[1]][localActive,"value"] <- res$xf


  y[[1]]          <-  result$ybesthistory[generations]         <- res$J

  #   ____________________________________________________________________________
  #   modify things
  NAs = 0
  yForResults=y[[1]]
  x=newPop
  evaluations     <- evaluations +  as.numeric(res$cunt)

  if (control$saveAll){

    result$x[[generations]]             <- newPop[[1]]
    result$y[[generations]]             <- res$J
  }

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
