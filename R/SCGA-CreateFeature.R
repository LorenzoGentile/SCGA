CreateFeature <- function( bounds,dependence=NULL, label=NULL,types=NULL,others = NULL,...){

  if(!is.null(bounds$lower) && !is.null(bounds$upper)){

    if(is.null(types))
      types <- rep("numeric", length(bounds$lower))

    bounds <- mapply(TreatBounds, bounds$lower,bounds$upper, types ,SIMPLIFY = FALSE)
  }
  if(is.null(label))
    label <- 1:length(bounds)

  if(is.null(types))
    types <- rep("numeric", length(bounds))

  if(is.null(dependence))
    dependence <- rep(NA, length(bounds))
  if(is.null(others))
    others <- rep(list(NULL), length(bounds))

  if(length(others)==length(bounds)){
    feature <- try( mapply(CreateFeat, bounds,dependence,label, types, others,SIMPLIFY = FALSE),silent=T)
    if(is.character(feature))
      others <-  TreatOthers(others,length(bounds))
  } else
    others <-  TreatOthers(others,length(bounds))

  feature <- mapply(CreateFeat, bounds,dependence,label, types, others,SIMPLIFY = FALSE)

  return(feature)

}

TreatBounds <- function(lower,upper,types){

  if(types == "categorical")
    postBounds <- lower : upper
  else
    postBounds <- c(lower,upper)

  return(postBounds)
}
TreatOthers <- function(others,dim){

    dimsOfObjects <- sapply(others, length)
    if(all(dimsOfObjects==dim)){
      name = names(others)
      newOther=rep(list(rep(list(NA),length(others))),dim)

      for ( j in 1:dim) {
        for (i in 1:length(others)) {
          newOther[[j]][[i]] = others[[i]][[j]]
          names(newOther[[j]]) <- name
        }
      }
      return(newOther)
    }else
      stop("ERROR: wrong in dimensions of others")


}

CreateFeat<- function(bounds,dependence,label,types, others){
  feature <- list(
    bound                 = CreateBoundsInt(bounds),
    dependent             = CreateDependent(dependence),
    type                  = types,
    label                 = label
  )

  for (i in seq(1,length.out = length(others))) {

    if(is.function(others[[i]]))
      feature[[names(others)[i]]] = others[[i]](feature)

    else
      feature[[names(others)[i]]] = others[[i]]
  }

  return(feature)
}


CreateBoundsInt <- function(FUN){
  return(ifelse(is.function(FUN),FUN,function(...){return(FUN)}))
}
CreateDependent <- function(FUN){
  return(ifelse(is.function(FUN),FUN,function(...){return(FUN)}))
}


CreateGetBounds<- function(feature){
  getBounds        <- function(i, x = NULL, id = NULL, ...) { feature[[i]]$bound(x, id, ...) }           # Create Get bounds
  return(getBounds)
}
CreateGetDependent<- function(feature){
  getDependent    <- function(i, x = NULL, id = NULL, ...) { feature[[i]]$dependent(x, id,value, ...) }           # Create Get bounds
  return(getDependent)
}



# CreateFeature <- function( bounds,dependence=NULL, label=NULL,types=NULL,depToMutate=NULL,others = NULL,...){
#
#   if(!is.null(bounds$lower) && !is.null(bounds$upper)){
#
#     if(is.null(types))
#       types <- rep("numeric", length(bounds$lower))
#
#     bounds <- mapply(TreatBounds, bounds$lower,bounds$upper, types ,SIMPLIFY = FALSE)
#   }
#   if(is.null(label))
#     label <- 1:length(bounds)
#
#   if(is.null(types))
#     types <- rep("numeric", length(bounds))
#
#   if(is.null(dependence))
#     dependence <- rep(NA, length(bounds))
#
#   if(is.null(others))
#     others <- rep(list(NULL), length(bounds))
#
#   if(is.null(depToMutate))
#     depToMutate <- dependence
#
#   else if(length(others)!=length(bounds))
#     others <-  TreatOthers(others,length(bounds))
#
#   feature <- mapply(CreateFeat, bounds,dependence,label, types,depToMutate, others,SIMPLIFY = FALSE)
#
#   return(feature)
#
# }
#
# TreatBounds <- function(lower,upper,types){
#
#   if(types == "categorical")
#     postBounds <- lower : upper
#   else
#     postBounds <- c(lower,upper)
#
# return(postBounds)
# }
# TreatOthers <- function(others,dim){
#
#   if(dim !=length(others)){
#     dimsOfObjects <- sapply(others, length)
#     if(all(dimsOfObjects==dim)){
#       name = names(others)
#       newOther=rep(list(rep(list(NA),length(others))),dim)
#
#       for ( j in 1:dim) {
#         for (i in 1:length(others)) {
#           newOther[[j]][[i]] = others[[i]][[j]]
#           names(newOther[[j]]) <- name
#         }
#       }
#       return(newOther)
#     }else
#       stop("ERROR: wrong in dimensions of others")
#
#   } else
#     stop("ERROR: wrong in dimensions of others")
# }
#
# CreateFeat<- function(bounds,dependence,label,types, depToMutate,others){
#
#   feature <- list(
#     bound                 = asFunction(bounds),
#     dependent             = asFunction(dependence),
#     depToMutate           = asFunction(depToMutate),
#     type                  = types,
#     label                 = label
#   )
#
#   for (i in seq(1,length.out = length(others))) {
#
#     if(is.function(others[[i]]))
#       feature[[names(others)[i]]] = others[[i]](feature)
#
#     else
#       feature[[names(others)[i]]] = others[[i]]
#   }
#
#   return(feature)
# }
#
#
# asFunction <- function(FUN){
#   return(ifelse(is.function(FUN),FUN,function(...){return(FUN)}))
# }
#
#
#
#
