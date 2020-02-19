fast20ResDF <- function(resultList,invsign=FALSE){
  getData <- function(e){
    algoName <- e$job$algo.name
    problemName <- e$job$prob.name
    replication <- e$control$seed
    seed <- e$control$seed
    iteration <- e$summary$evaluations
    iterResults <- e$clusterAnalysis$iterResults

    iterResults <- e$ybesthistory
    if (invsign)
      iterResults=-iterResults
    iterBests <- unlist(lapply(1:length(iterResults),function(x)min(iterResults[1:x],na.rm = T)))

    nEntries <- length(iterBests)
    return(data.frame(rep(algoName,nEntries),
                      rep(problemName, nEntries),
                      rep(replication, nEntries),
                      rep(seed, nEntries),
                      iteration,
                      iterBests))
  }
  df <- data.frame(data.table::rbindlist(lapply(resultList,getData)))
  names(df) <- c("algoName", "problemName", "replication", "seed", "iteration","iterBest")
  return(df)
}
