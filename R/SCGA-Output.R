Output <- function(best,bestRel=NULL, control,consBest=NULL,consBestRel=NULL,constList,evaluations,eval,fitness,generations,identicX,media, NAs, newPop,result, y,x,sigma,sigma0,stalling, pb,cRef=NULL){
  # tictoc::toc()

  cat("\014")

  if(control$constraint)
    cat( paste( "SCGA | iter = ", generations, " | Eval = ",evaluations," | Identical = " ,identicX," | Best = ", prettyNum(best,digits=4)," | ConstRatio = ", prettyNum(consBest/cRef,digits=4),
             " | BestRel = ", prettyNum(bestRel,digits=4)," | ConstRatioRel = ", consBestRel/cRef," | relaxedCref = ",prettyNum(constList$relaxedCRef/cRef,digits=4),
             " | Mean = ", prettyNum(mean(y, na.rm = TRUE),digits=4),
             " | Stalling = ", stalling, "| NAs = ", NAs,"\n"))
  else
    cat(paste( "SCGA | iter = ", generations, " | Eval = ",evaluations, " | Best = ", prettyNum(best,digits=4),
             " | Mean = ", prettyNum(mean(y, na.rm = TRUE),digits=4),
             " | Stalling = ", stalling, "| NAs = ", NAs,"\n"))



  # pb$update(evaluations/control$maxEvaluations)
  # TODO : ERROR THAT HAS TO BE FIXED
  pb$update(min(.99,evaluations/control$maxEvaluations))
  pb$tick()

  ##  ............................................................................
  ##  backup
  if(control$backup){
    if(generations%%control$backupInterval==0){
      back              <- result
      back$newPop       <- newPop
      back$summary      <- createSummary(control,result)
      back$y            <- back$y[1:generations]
      back$ybesthistory <- back$ybesthistory[1:generations]
      back$xbesthistory <- back$xbesthistory[1:generations]
      back$x            <- back$x[1:generations]
      save("back",file  = paste0(control$resumeFrom,".RData"))
    }

  }


  ####### plots
  if( (generations %% control$plotInterval == 0 || generations == 1) && any(c(control$plotSigma,control$plotEvolution,control$plotPopulation,control$plotFitness))){
    G1 =G2 =G3= NULL

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
        result$plots$sigma$plot[[ind]]    <- G3    <- Plotsigmas(sigma,generations=generations, path=path,printIt=control$printPlot,sigma0=sigma0)
      }

      else
        print("WARNGING: Sigma plot requested but updateSigma == FALSE ")
    }

    if (control$plotPopulation ){
      ind                                         <- length(result$plots$population$generations) + 1
      result$plots$population$generations[[ind]]  <- generations
      result$plots$population$plot[[ind]]   <-   G3    <- PlotPopulation(x,generations,path,printIt=control$printPlot)
    }

    if (control$plotEvolution){

      G1 <- Plot(result$ybesthistory, media, stalling,eval,control$plotEvolutionLimit)
    }
    if(control$plotFitness){
      ind                                        <- length(result$plots$sigma$generations)+1
      G2 <-  result$plots$fitness$plot[[ind]]    <- plotFitness(y,constList,fitness)
    }


    if(!is.null(G1) & !is.null(G2) & !is.null(G3))
      print(ggpubr::ggarrange(G1,G2, G3))

    else if(!is.null(G1) & is.null(G2) & !is.null(G3) )
      print(ggpubr::ggarrange(G1, G3))
    else if(is.null(G1) & !is.null(G2) & !is.null(G3) )
      print(ggpubr::ggarrange(G2, G3))
    else if(!is.null(G1) & !is.null(G2) & is.null(G3) )
      print(ggpubr::ggarrange(G1, G2))


    else if(!is.null(G1) & is.null(G2) & is.null(G3))
      print(G1)
    else if(is.null(G1) & !is.null(G2) & is.null(G3) )
      print(G2)
    else if(is.null(G1) & is.null(G2) & !is.null(G3) )
      print(G3)
  }


  if (control$printSigma) {
    cat("\n Sigma normalised")
    print(t(t(sigma) / sigma0)[,1:(ncol(sigma)-8)])
  }

  if(control$printXMin){
    cat("\n Best found x")
    print(x[[which.min(y)]])
  }
  return(result)
}
