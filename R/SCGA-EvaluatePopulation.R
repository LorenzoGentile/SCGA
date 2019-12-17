evaluatePopulation <- function(control,evaluateFun,newPop,y,...){
  cat("\n","To evaluate",length(control$toEval),"candidates","\n")
  if(is.null(y)) y <- constraint <- NULL

  outEvaluation <- evaluateFun(newPop[control$toEval]  ,...)
  if(is.list(outEvaluation)){
    y[control$toEval]               <- outEvaluation[[1]]

    if(!is.null(outEvaluation$constraint))
      constraint[control$toEval]    <- outEvaluation$constraint

    if(!is.null(outEvaluation$x))
      mewPop[control$toEval]        <- outEvaluation$x

  }else
    y[control$toEval]               <- outEvaluation


  yForResults        <- y

  tictoc::toc()

  ########## Count the NAs
  NAs             <- sum(is.na(y))


  return(mget(ls(),environment()))
}


