
createPopulation <- function(feature,size,createCandidate=createCandidate,cl=NULL,...){
if(is.null(cl))
  pop <- lapply(X=floor(runif(size,min=0,max=1e6)), FUN=createCandidate,feature=feature,newCand=TRUE,...)
else
  pop <- parLapply(cl=cl,X=floor(runif(size,min=0,max=1e6)), fun=createCandidate,feature=feature,newCand=TRUE,...)
  return(pop)
}

createCandidate <- function(X,feature,...){

  x = NULL
  set.seed(X)

  dependent    <- sapply(feature, function(x) x$dependent(),simplify = F)
  dependent    <- dependent[!is.na(dependent)] %>% unlist() %>% unique()
  notdependent <- setdiff(1:length(feature),dependent)


  for ( i in notdependent){


    x <- rbind(x,createDepDF(feature,i,id=max(0,nrow(x))+1,xDone=x,...))
  }

  return(x)

}

 createDepDF <- function(feature, i, id, prec = NA, x = NULL, xDone=NULL, addnames = NULL, newCand=FALSE,...) {

  names = c("value", "feature", "prec", "id", addnames)
  x = matrix(c(NA, i, prec, id, rep(NA, length(names) - 4)), 1, length(names))
  colnames(x) <- names


  for (k in addnames) {

    if (!is.null(feature[[i]][[k]])){
      addfeat <- ifelse(is.function(feature[[i]][[k]]),feature[[i]][[k]](i=i, x=rbind(xDone,x), id=id),feature[[i]][[k]])
      x[,k] <- addfeat
    }
    else
      x[,k] <- NA
  }

  bounds <- feature[[i]]$bound(x=rbind(xDone,x), id=id,newCand=newCand)

  if (feature[[i]]$type == "numeric") {
    x[, "value"] = runif(1, bounds[1], bounds[2])

  }  else if (feature[[i]]$type == "categorical") {
    if(length(bounds)>1)

      x[, "value"] = sample(bounds,1)
    else
      x[, "value"] = bounds
  } else if (feature[[i]]$type %in% c("repeater", "integer")) {
    x[, "value"] = floor(runif(
      1,
      bounds[1],
      bounds[2] + 1 - .Machine$double.eps
    ))
  }

  dependent <- feature[[i]]$dependent(x=x,id=id,value =x[which(x[, "id"]==id), "value"] )

  if (!anyNA(dependent)) {

    dependence= ifelse(feature[[i]]$type == "repeater", x[1, "value"],1)

    for (k in numeric(dependence)){

      for (j in dependent) {

        x <-  rbind(x,createDepDF(  feature = feature,  i = j,  id = id + 1,
                                prec = x[1, "id"],  xDone=rbind(xDone,x),  addnames=addnames,newCand = TRUE))
        id = max(0, x[, "id"])

      }
    }
  }
  return(x)
}
