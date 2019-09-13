Output <- function(best, control,consBest=NULL, evaluations,eval,generations,media, NAs, result, y,x,sigma,sigma0,stalling, pb){
  tictoc::toc()

  cat("\014")

  if(control$constraint)
    cat(
      paste( "SCGA | iter = ", generations, " | Eval = ",evaluations, " | Best = ", prettyNum(best,digits=4)," | Const = ", prettyNum(consBest,digits=4),
             " | Mean = ", prettyNum(mean(y, na.rm = TRUE),digits=4),
             " | Stalling = ", stalling, "| NAs = ", NAs,"\n"))

  else

    cat(
      paste( "SCGA | iter = ", generations, " | Eval = ",evaluations, " | Best = ", prettyNum(best,digits=4),
             " | Mean = ", prettyNum(mean(y, na.rm = TRUE),digits=4),
             " | Stalling = ", stalling, "| NAs = ", NAs,"\n"))



  # pb$update(evaluations/control$maxEvaluations)
  # TODO : ERROR THAT HAS TO BE FIXED
  pb$update(min(.99,evaluations/control$maxEvaluations))
  pb$tick()

  ####### plots

  if( generations %% control$plotInterval == 0 && any(c(control$plotSigma,control$plotEvolution,control$plotPopulation))){

    if(!file.exists(file.path("runResults")) && control$printPlot)
      dir.create(file.path("runResults"))

    if(!file.exists(file.path("runResults",control$job$algo.name)) && control$printPlot)
      dir.create(file.path("runResults",control$job$algo.name))

    if(!file.exists(file.path("runResults",control$job$algo.name,control$seed)) && control$printPlot)
      dir.create(file.path("runResults",control$job$algo.name,control$seed))

    path = file.path("runResults",control$job$algo.name,control$seed)

    if (control$plotSigma){
      if(control$updateSigma){
        ind                                        <- length(result$plots$sigma$generations)+1
        result$plots$sigma$generations[[ind]]      <- generations
        result$plots$sigma$plot[[ind]]             <- Plotsigmas(sigma,generations=generations, path=path,printIt=control$printPlot,sigma0=sigma0)
      }

      else
        print("WARNGING: Sigma plot requested but updateSigma == FALSE ")
    }

    if (control$plotPopulation ){
      ind                                         <- length(result$plots$population$generations) + 1
      result$plots$population$generations[[ind]]  <- generations
      result$plots$population$plot[[ind]]         <- PlotPopulation(x,generations,path,printIt=control$printPlot)
    }

    if (control$plotEvolution){
      Plot(result$ybesthistory, media, stalling,eval)
    }
  }

  if (control$printSigma) {
    cat("\n Sigma normalised")
    print(t(t(sigma) / sigma0))
  }

  if(control$printXMin){
    cat("\n Best found x")
    print(x[[which.min(y)]])
  }
  return(result)
}
