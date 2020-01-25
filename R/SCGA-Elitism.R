elitism <- function(control,constraint=NULL,fitness,newPop,sigma,y,x){

  if (as.logical(control$elitism)) {

    elite   = order(fitness,decreasing = T)       # if elitism i save the control$elitism best candidates


    if(control$constraint)
      # elite = elite[elite %in% unique(c(feasible[order(fitness[feasible],index.return=T,decreasing = T)],elite))[1:control$elitism]] ### preserve feasible and not relaxed feasible
      elite = elite[1:control$elitism]
    else
      elite = elite[1:control$elitism]

    y[1:control$elitism]       <- y[elite]                                                 # (and relative sigmas) untouched as firsts in the list of candidates)
    newPop[1:control$elitism]  <- x[elite]
    elitismSigma               <- sigma[elite, ]
    if(control$constraint)
      constraint               <- constraint[elite]
  }

  return(mget(ls(),envir = environment()))
}
