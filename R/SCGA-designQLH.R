createPopulationLHD <- function(feature,size,createCandidate=createCandidateLHD,cl=NULL,...){

  getBounds <- function(x){
    bounds <- x$bound()
    if(length(bounds)==1)
      return(rep(bounds,2))
    else if (length(bounds)>2)
      return(c(min(bounds),max(bounds)))
    else
      return(bounds)
  }
  countingPresence   <- function(cand){
    temp             <- rle(sort(cand[, "feature"] ))
    out[temp$values] <- temp$lengths
    out
  }
  out <- rep(0,length(feature))


  #   ____________________________________________________________________________
  #   create the first DOE                                                    ####

  dependent    <- sapply(feature, function(x) x$dependent(),simplify = F)
  dependent    <- dependent[!is.na(dependent)] %>% unlist() %>% unique()
  notdependent <- setdiff(1:length(feature),dependent)
  bounds       <- sapply(feature,getBounds )
  types        <- purrr::map(feature,"type") %>% unlist
  firstLHD     <- designLHD(lower = bounds[1,],upper = bounds[2,],control=list(size = size,types=types))


  #   ____________________________________________________________________________
  #   create the first population using the DOE for the indipendent genes     ####

  independentLhd <- firstLHD[,notdependent,drop=F]
  seeds          <- floor(runif(size,min=0,max=1e6))

  pop <- sapply(seq_len(size), function(i) createCandidateLHD(X = seeds[i],independentLhd =independentLhd[i,], notdependent=notdependent,
                                                                feature=feature,newCand=TRUE,...),simplify = F)

  #   ____________________________________________________________________________
  #   try to improve the population sparsity                                  ####


  ##  ............................................................................
  ##  count how many of each type exist in the DOE                            ####

  presence <- sapply(pop, countingPresence)
  counts   <- rowSums(presence) %>% sort(index.return=T)


##  ............................................................................
##  generate LHDs to match the occurencies in the first population          ####

  lhds <- sapply(unique(counts$x), function(i){

    involved <- counts$ix[counts$x==i]
    if(i==size) lhd <- firstLHD[,involved,drop=F]
    else if(i==0)return(list(lhd=NULL,involved=NULL))
    else{
    bounds   <- sapply(feature[involved],getBounds )
    types    <- purrr::map(feature[involved],"type") %>% unlist
    lhd      <- designLHD(lower = bounds[1,],upper = bounds[2,],control=list(size = i,types=types))
    }
    colnames(lhd) <- involved
    return(list(lhd=lhd,involved = involved))
  },
  simplify = F)

  indexes <- purrr::map(lhds,"involved")
  lhd     <- purrr::map(lhds,"lhd")
  lhd     <- lhd[!sapply(lhd, is.null)]
  lhd     <- lhd  %>% reshape2::melt()

  # for (i in seq_along(pop)) {
  popOld <- pop
  for (i in 1:(length(pop))) {

    for (j in seq_along(pop[[i]][,"feature"])) {

      feat                <- pop[[i]][j,"feature"]
      ind                 <- which(lhd$Var2==feat) %>% min
      pop[[i]][j,"value"] <- lhd[ind,"value"]
      lhd                 <- lhd[-ind,]

    }
  }
  feasibles <- sapply(pop, checkCandidateLHD,feature,notdependent)

  cat("\n ",sum(feasibles),"candidates generated by means quasi LHD and", size -sum(feasibles),"random uniform. \n")

  pop       <- append(pop[feasibles],popOld[!feasibles])
  return(pop)
}


checkBounds <- function(feature,x,row){

  toCorrect=F
  bounds <- feature[[x[row,"feature"]]]$bound( x = x , id = x[row,"id"] )
  type   <- feature[[x[row,"feature"]]]$type
  if(type=="categorical")
    if(!(x[row,"value"]%in% bounds))
      toCorrect = T
  if(type%in%c("integer","repetear"))
    if( !((x[row,"value"] <=bounds[2]) & (x[row,"value"] >= bounds[1]) & (floor(x[row,"value"])==x[row,"value"])) )
      toCorrect = T
  if(type=="numeric")
    if(!((x[row,"value"] <= bounds[2]) & (x[row,"value"] >= bounds[1])) )
      toCorrect = T
  return(toCorrect)
}
check <- function(row,x,feature,...){

  featOfRow <- x[row,"feature"]
  dependencesAll <- feature[[featOfRow]]$dependent(x,x[row,"id"],x[row,"value"])
  if(!anyNA(dependencesAll)){
    idDepExisting         <- x[which(x[,"prec"]==x[row,"id"]),"id"]
    depExistingNoNeeded   <- setdiff( x[which(x[,"prec"]==x[row,"id"]),"feature"] ,dependencesAll)
    depNotExisting        <- setdiff (dependencesAll,x[which(x[,"id"] %in% idDepExisting ),"feature"])

    i= x[row,"feature"]
    for (k in idDepExisting) {

      if(! x[which(x[,"id"]==k),"feature"] %in% depExistingNoNeeded){
        row= which(x[,"id"]==k)
        toCorrect <- checkBounds(feature,x,row)
        if(toCorrect)
          return(F)
      }
      else
        x=x[-which(x[,"id"]==k),,drop=FALSE]
    }
    if(!bazar::is.empty(depNotExisting))
      return(F)
    else
      return(T)
  }
  return(T)
}
checkCandidateLHD <- function(x,feature,notdependent,...){
  out=NULL
  for ( i in notdependent){
    out <- c(out,check(which(x[,"id"]==i),x,feature))
    if(any(out==F))
      return(F)
  }

  return(T)

}

createCandidateLHD <- function(X,feature,independentLhd,notdependent,...){

  x = NULL
  set.seed(X)
  j=1

  for ( i in notdependent){
    x <- rbind(x,createDepDF(feature,i,id=max(0,nrow(x))+1,xDone=x,value = independentLhd[j],...))
    j <- j + 1
  }
  return(x)
}
