Crossover <- function(APPLY,control,elitismSigma, feature, fitness,newPop,sigma,x,cl,...){

  oldPop     <-  x
  oldFitness <- fitness
  oldSigma   <- sigma
  control$crossProbability <- .8


  poolSize   <- ceiling((   control$size - control$elitism ) / 2 * control$crossProbability)

  CrossPool  <- control$selection(fitness, poolSize, tsize = control$tournamentSize)  # matrix ncol=2 nrow size/2 indicating the couples to make crossover




  ########## Crossover for the hyperparamters of the mutation

  t.sigma                                                <- t(apply(CrossPool, 1, CrossSigma, sigma, feature, fitness)) # store temporary the generated hp the number is equal to the number of couples
  row.sigma                                              <- nrow(t.sigma)
  sigma <-  t.sigma[sort(rep(1:nrow(t.sigma),2))[1:(2*row.sigma)],]
  # sigma[seq(from = 1, by = 2, length.out = row.sigma), ] <- t.sigma
  # sigma[seq(from = 2, by = 2, length.out = row.sigma), ] <- sigma[seq(from = 1, by = 2, length.out = row.sigma), ]

  if (as.logical(control$elitism))
    sigma <- rbind(elitismSigma, sigma)

  ########## Crossover for candidates

    newPop[seq( from = control$elitism + 1, by = 1, length.out = 2 * row.sigma )] <- unlist(    # I start replacing the candidates starting from control$elitism +1
    APPLY( X = CrossPool, MARGIN = 1, CrossOperation, pop = x, feature = feature,
           keep = control$keep,
           repairCross = control$repairFun, budgetTot = control$budgetTot,
           control=control,probability=control$probability )
    ,recursive = F )

if(control$size - nrow(sigma) >0){
  #leave some untouched
  unthouched <-  sample(length(oldFitness),size=control$size - nrow(sigma) ,prob = oldFitness)

  sigma  <- rbind(sigma,oldSigma[unthouched,])
  newPop <- append(newPop,oldPop[unthouched])

}
  ## Remove the possible exceeding candidates
  sigma <- sigma[1:control$size, ]
  newPop[seq(control$size + 1, length.out =  (-control$size + length(newPop)))] = NULL

  return(list(
    newPop = newPop,
    sigma  = sigma,
    CrossPool=CrossPool
  ))
}

CrossOperation <- function(indexs,pop,feature,keep = NULL,repairCross = NULL,budgetTot,control,probability,...) {
  ########## Initialise ######################################################################################
  toadd        <- add <- index <-  list()

  candidates   <- pop[indexs]
  avoid = list(NULL, NULL)
  minLength    <- round(min(sapply(candidates,function(x) sum(x[,"feature"] %in% setdiff(x[,"feature"],control$dontChangeCross)))))
  maxChanges   <- round(minLength * control$percCross)
  ########## select the possible feature to swap
  possible     <- intersect(candidates[[1]][, "feature"], candidates[[2]][, "feature"])
  possible     <- setdiff(possible, control$dontChangeCross)
  repetition   <- rep(1, times =  purrr::map(feature,"label") %>% as.numeric() %>% max()) #deafault
  if(is.null(probability))
  probability  <- repetition
  # replicates <- any(sapply(condidates, function(cand) sapply(unique(x[]), function)))
  probability  <- probability[possible]
  repetition   <- repetition[possible]


  r            <- rle(sort(candidates[[1]][, "feature"]))
  R            <- rle(sort(candidates[[2]][, "feature"]))
  replicates   <- ifelse(any(c(r$lengths,R$lengths)>1),T,F)
  if (replicates) {
    repetition <-
      pmin(r[[1]][match(possible, r[[2]])], R[[1]][match(possible, R[[2]])])
  }

  exchanges <-
    min(sum(repetition), maxChanges, floor(min(sapply(candidates, nrow)) / 2))


  ########## select the feature to swap
  if (exchanges > 1) {

    if (length(possible) > 1){
      if(replicates)
        featuretochange <- sample(possible, exchanges, prob = probability * repetition, replace = T)
      else
        featuretochange <- sample(possible, exchanges, prob = probability * repetition)
    }


    else
      featuretochange <- rep(possible, exchanges)

  } else if (exchanges == 1) {
    featuretochange <- possible

  } else {
    featuretochange <- NULL

  }


  featuretochange <- sort(featuretochange)

  ########## Crossover operations

  for (i in featuretochange) {
    # if(i==45)
    #   browser()
    ########## Choose the index to swap of the first

    index  <- mapply(IndicesToSwap, x. = candidates,avoid = avoid, MoreArgs = list(i=i), SIMPLIFY=FALSE)

    if (any(sapply(index, FUN = is.null)))
      break()
    # id.prec index of the first variable to replace(consistency of the prec field)

    id.prec <-
      matrix(sapply( X = 1:2,  FUN = getIndexPrec, x = candidates, index = index,  keep = keep),,2 )

    # all the indexes involved in the exchange
    toadd <-lapply(  1:2,  FUN = function(i, x, index) {which(x[[i]][, "id"] %in% index[[i]])},  x = candidates,  index)

    # matrixes to append
    add <-lapply(1:2, function(i, x, toadd) {  x[[i]][toadd[[i]], ]}, x = candidates, toadd = toadd)

    # modify the field prec ,id in order to have consisency.keep field are also modified in order to inherit from parent
    add <-lapply(  1:2,  modIdPrec,  id.prec = id.prec,  candidates = candidates,  toadd = toadd,  keep = keep)

    # replace or  remove and add the new values
    candidates <- lapply( 1:2, finalCand, toadd = toadd, candidates = candidates, add = add )

    # update the indexes to avoid to change again
    avoid[[1]] <- unique(c(avoid[[1]], add[[2]][, "id"]))
    avoid[[2]] <- unique(c(avoid[[2]], add[[1]][, "id"]))

    if(!is.null(repairCross))
      candidates <- lapply(candidates, repairCross,budgetTot=budgetTot)
  }

  candidates <- lapply(candidates, newId)
  return(candidates)
}

IndicesToSwap <- function(x.,i,avoid){

  id1    <- x.[which(x.[,"feature"] == i),"id"]
  id     <- setdiff(id1,avoid)
  if (bazar::is.empty(id))
    id   <- id1
  if (length(id) > 1)
    id   <- sample(id ,1)
  id     <- IndicesOfDependents(x.,id)
  return(id)
}

IndicesOfDependents<-function(x,toSelect){
  archive        <- NULL

  while (!bazar::is.empty(toSelect)) {

    nextToSelect <- x[which(x[, "prec"] %in% toSelect), "id"]
    archive      <- c(archive,toSelect)
    toSelect     <- nextToSelect

  }

  return(archive)
}

checking <-function (x,error=NULL){
  rep=NULL
  for ( i in which(is.na(x[, "prec"]))) {
    add <-  IndicesOfDependents(x,x[i, "id"])
    add <- which(x[,"id"]%in%add)
    add <- x[add, ,drop=FALSE]

    add[1, "value"] <- length(which(add[, "feature"] == 2))

  }
  return(rep)}

getIndexPrec <- function(i,x,index,keep){

  ret      <- NULL
  row      <- which(x[[i]][,"id"]==index[[i]][1])
  ret[1]   <- x[[i]][row,"prec"]
  j=2
  for (k in keep) {
    ret[j] <- x[[i]][row,k]

    j     <- j+1
  }
  return(ret)
}

modIdPrec <- function(i,id.prec,candidates,toadd,keep){

  add                  <- candidates[[i]][toadd[[i]],,drop=FALSE]
  j                    <- ifelse(i==1,2,1)

  add[,c("prec","id")] <- add[,c("prec","id")] + max(candidates[[j]][,"id"]) - min(add[,c("prec","id")],na.rm = TRUE) + 1
  add[1,"prec"]        <- id.prec[1,j]

  for (k in seq(from = 1,length.out=length(keep)))
    add[1,keep[k]]     <- id.prec[(1+k),j]

  return(add)
}

finalCand <- function(i,candidates,toadd,add){

  dimensions                     <- lengths(toadd)
  j                              <- c(2,1)[match(i,1:2)]
  diff                           <- dimensions[i]-dimensions[j] # if < 0 there are more chromosome to add

  if (diff==0){
    candidates[[i]][toadd[[i]],] <- add[[j]]

  } else if (diff < 0){
    last                         <- max(toadd[[i]])
    candidates[[i]][toadd[[i]],] <- add[[j]][1:dimensions[i],] # the common are relplacedadd [[j]][j[1:dimensions[i]],]

    if(last==nrow(candidates[[i]]))
      candidates[[i]]            <- rbind(candidates[[i]],add[[j]][(dimensions[i]+1):dimensions[j],])

    else
      candidates[[i]]            <- rbind(candidates[[i]][1:last,],add[[j]][(dimensions[i]+1):dimensions[j],],
                                          candidates[[i]][(last+1):(nrow(candidates[[i]])),])

  } else {
    candidates[[i]][toadd[[i]][1:dimensions[j]],] <- add[[j]]
    candidates[[i]]              <- candidates[[i]][-(toadd[[i]][(dimensions[j]+1):dimensions[i]]),]

  }

  return(candidates[[i]])
}

newId<- function(x){

  ran        <- rank(x[,"id"])
  prec       <- ran[match(x[,"prec"],x[,"id"])]
  x[,"id"]   <- ran
  x[,"prec"] <- prec

  return(x)
}

CrossSigma <- function(toCross,sigma,feature,fitness){

  sigm <- rbind(sigma[toCross[1],],sigma[toCross[2],])
  fit  <- c(fitness[toCross[1]],fitness[toCross[2]])
  # ret  <-apply(sigm,2,weighted.mean,fit)
  ret  <-apply(sigm,2,mean)

  return(ret)
}

CrossDF<-function(x,toSelect){
  archive=NULL
  while (!bazar::is.empty(toSelect)) {
    nextToSelect <- x[which(x[, "prec"] %in% toSelect), "id"]
    archive <- c(archive,toSelect)
    toSelect <- nextToSelect
  }
  return(archive)

}

CrossDFRet<-function(x,toSelect){

  archive=NULL
  while (!anyNA(toSelect)) {
    nextToSelect <- x[which(x[,"id"]%in%toSelect),"prec"]
    archive <- unique(c(archive,toSelect))
    toSelect <- nextToSelect
  }
  return(archive)

}




