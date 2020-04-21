#' This function returns the default controls and other useful variables.
#'  Control is a list of the settings:
#' @param convergence Stopping criterion: absolute difference between the current best and
#' the known minimum
#' @param cpus numeric. indicatig number of cores over which parallelise
#' @param creatCandFun function. See \code{\link{createCandidate}}
#' @param createMutFun function. See \code{\link{createMutFun}}
#' @param crossFun function. See \code{\link{crossFun}}
#' @param dontChangeCross numeric vector. Feature number that not undergo to Crossover
#' @param dontChangeMut numeric vector. Feature number that not undergo to Mutation
#' @param elitism numeric. Number of candidates to preserve to the next population. Default is size / 10
#' @param evaluatePopDF function. See \code{\link{evaluatePopDF}}
#' @param feature list or function that creates the list. See \code{\link{feature}}
#' @param fitnessFN function. Receives the observations of the objective functions and returns
#' @param Fun function. Objective function
#' a vector of the same length repesententing the fitness. Default is Ranking fitness.
#' @param maxStallGenerations numeric. Maximum number of iterations without improvements. If overcomen, the population is reinitialised.
#' @param keep vector of characters. Additional columns in the matrix representing the candidate.
#' @param localOptGenerations numeric. Maximum number of iterations without improvements. If overcomen,
#'  a local optimisation on the numeric variables starts from the best solution found freezing the remaining genes. Then, the population is reinitialised.
#' @param localOptimiser function. Function that performs the local optimisation. Default si optim. function. \code{\link{LocalOptimisationMatlab}} is also an option. It starts
#' connection with matlab and uses fmnincon.
#' @param maxEvaluations numeric. Stopping criterion. Maximum number of evaluations allowed. If more stopping criterion are given, the more strict will be used.
#' @param maxGenerations numeric. Stopping criterion. Maximum number of generations allowed. If more stopping criterion are given, the more strict will be used.
#' @param multiPopulation Boolean. Use or not multiPopulation strategy. controls ar specified in multiPopControl
#' @param maxRelaxation numeric. Value in [0,1]. Indicates the fraction of constraint relaxation at the beginning of the optimisation.
#' @param multiPopControl list. controls are :...
#' @param mutRate numeric. Value in [0,1]. Probablity to mutate a candidate
#' @param parallel Boolean. Indicates wheter to create a cluster with \code{\link{MakeCluster}} command using the number of cores indicated by @param cpus.
#' @param percCross numeric. Value in [0,1]. Indicates the maximum percentage of genes to swap during crossover.
#' @param percMut numeric. Value in [0,1]. Indicates the maximum percentage of genes to mutate
#' @param plotEvolution Boolean. plot the evoluation of the best found solution.
#' @param plotEvolutionLimit numeric. Upper limit for the plotEvolution plot. Helps the visualisation when the initial best is far from final best.
#' @param plotFitness Boolean. If there are constraints, it produces a plot that shows the fitness in respect of the objective function value and feasibility.
#' @param plotPopulation Boolean. Plot an historgram for every gene. The histograms show the count of the values assumed in the current population.
#' @param plotSigma Boolean. Plot an historgram for every sigma The histograms show the count of the values assumed in the current population.
#' @param plotInterval integer. Create the plots every plotInterval generations.
#' @param popCreateFun function. It creates new candidates. Default is \code{\link{createCandidate}}
#' @param printIter Boolean. Print on screen the evolution of the optimisation.
#' @param printSigma Boolean. Print on screen the mean values of sigma.
#' @param printXMin Boolean. Print on screen the current xbest.
#' @param printPlot Boolean. Save plots in a dedicated folder: currentDirectory/runResults/control$job$algo.name/control$seed
#' @param probability vector. It specifies the probability of every gene to be selected by the operators. Default is all 1.
#' @param pureFeasibility numeric. Value in [0,1]. Fraction of the available budget to be spent without constraint relaxation.
#' @param repairFun function. Repair function used to repair the possible corrupted candidates.
#' @param resume Boolean. Restart the optimisation loading a backup RData names as @param resumeFrom.
#' @param resumeFrom character. Name for a possible backup RData
#' @param saveAll  Boolean. save all the x at each iteration
#' @param seed  integer. Seed to use for repetitivity .
#' @param selection function. selection method function. See \code{\link{selectpoolTournament}}
#' @param size  integer. Population size
#' @param target numeric. Knwown minimum value achievable. Stopping criterion. If reached wihin the specified tolerance @param convergence
#' @param tournamentSize integer. tournament size for \code{\link{selectpoolTournament}}
#' @param updateSigma Boolean. To use adaptive step size mutation
#' @param useCrossover Boolean. To crossover as operator.
#' @param vectorOnly Boolean. Pass to the objective function the candidate as vector.
#' @param vectorized Boolean. Pass to the objective function the entire population.

Initialise <- function(control = list(),...) {

  require(bazar)
  # require(tictoc)
  require(SPOT)
  require(purrr)
  require(ggplot2)
  require(parallel)

  ########## Initialise Control ########################################################################################################################################################

  cat(paste0(" \n the seed is ",control$seed) )
  control <- createControl(control)
  ########## Initialise other #############################################################################################################
  constList                                   <- NULL
  constraint    <- constraintForResults       <- NULL
  feature                                     <- control$feature                                              # Initialise #
  forceEvaluation                             <- FALSE
  Fun                                         = control$Fun                                                   # Initialise #
  stallinFlag                                 <- FALSE                                                        # Initialise #
  media                                       <- NULL
  stalling <-  ws <- evaluations              <- 0
  generations                                 <- 1
  best                                        <- Inf
  consBest      <- consBestRel <- bestRel     <- NULL
  wY                                          <- NULL
  wC                                          <- NULL
  stallRef                                    <- Inf
  resuming                                    <- FALSE

  if (is.null(control$job)){
    control$job=list()
    control$job$algo.name="anonymousAlgo"
  }


  if(control$constraint){
    control$fitnessFN                    <- constraintHandlerFitness
    bestFeasible                         <- list(y = Inf, x = NULL, constraint = NULL)
  }else
    bestFeasible = NULL
  set.seed(control$seed, kind = "Mersenne-Twister", normal.kind = "Inversion")                               # set.seed

  if (is.null(feature))                                                                                      # Check feature
    stop("feature is not provided")                                                                          # Check feature
  else if (is.function(feature))                                                                             # Check feature
    feature                          <- feature()

  if (control$parallel) {                                                                                    # Cluster Settings
    print("setting up the cluster ")                                                                         # Cluster Settings
    cltype  <- ifelse(.Platform$OS.type != "windows", "FORK", "PSOCK")                                       # Cluster Settings
    cpus    <- min(detectCores() - 1, control$cpus, na.rm = TRUE)                                            # Cluster Settings
    cl      <- makeCluster(cpus, type = cltype)                                                              # Cluster Settings
    # Cluster Settings
    clusterExport(cl, varlist = "Fun", envir = environment())                                                # Cluster Settings
    #clusterEvalQ(cl, "orbit2R")                                                                             # Cluster Settings
    clusterEvalQ(cl, "bazar")                                                                                # Cluster Settings
    print(paste0("loaded cluster: - ",cpus," - nodes"))                                                      # Cluster Settings
  } else                                                                                                     # Cluster Settings
    cl      <-  NULL



  if (!is.null(control$dontChangeMut)){
    active  <- as.numeric(setdiff(getValues(x = feature, name = "label", Unique = F),control$dontChangeMut))      # Active feature
    feat    <- feature[active]                                                                                 # Feature of only Active
  } else
    feat    <- feature

  nVar      <- NULL
  nVar[1]   <- sum(getValues(x=feat, name = "type", Unique = F) == "numeric")
  nVar[2]   <- sum(getValues(x=feat, name = "type", Unique = F) == "integer")
  nVar[3]   <- sum(getValues(x=feat, name = "type", Unique = F) == "categorical")
  nVar[4]   <- sum(getValues(x=feat, name = "type", Unique = F) == "repeater")

  result    <- OptimizerClass(job=control$job,resumeFrom=control$resumeFrom,control)                     # create a result object of class result

  conditions <-  initializeConditions()


  if(is.null(cl)){
    LAPPLY <- lapply
    APPLY  <- apply
    SAPPLY <- sapply
  }
  else{
    LAPPLY <- function(...){
      parLapply(cl,...)
    }
    APPLY  <- function(...){
      parApply(cl,...)
    }

    SAPPLY <- function(...){
      parSapply(cl,...)
    }

  }

  ####### Ridefine objective function  ########

  if(control$vectorized && control$vectorOnly )
    evaluateFun <- function(x,...) Fun(x[1:length(x)][,"value"],...)

  else if (control$vectorized && !control$vectorOnly )
    evaluateFun <- function(x,...) Fun(x,...)

  else if (!control$vectorized && !control$vectorOnly )
    evaluateFun <- function(x,...) SAPPLY( X = x,Fun,...)

  else if (!control$vectorized && control$vectorOnly )
    evaluateFun <- function(x,...)  SAPPLY( X = x, function (x) Fun(x[,"value"]),...)



  mutRate  <- control$mutRate

  return(list(
    APPLY           = APPLY,
    best            = best,
    bestFeasible    = bestFeasible,
    cl              = cl,
    conditions      = conditions,
    consBest        = consBest,
    constraint      = NULL,
    control         = control,
    evaluateFun     = evaluateFun,
    evaluations     = evaluations,
    feat            = feat,
    feature         = feature,
    forceEvaluation = forceEvaluation,
    generations     = generations,
    LAPPLY          = LAPPLY,
    media           = media,
    mutRate         = mutRate,
    NAs             = 0,
    nVar            = nVar,
    result          = result,
    resuming        = resuming,
    SAPPLY          = SAPPLY,
    stalling        = stalling,
    stallinFlag     = stallinFlag,
    stallRef        = stallRef,
    y               = NULL,
    ws              = ws,
    wY              = wY,
    wC              = wC
  )
  )
}



createControl <- function(control) {

  #initializatio of hyperparatmeter for Optimization
  con <- list(
    algoName                = "SCGA",
    analysePerformance      = F,
    backup                  = F,
    backupInterval          = 30,
    budgetTot               = 1,
    cRef                    = 1e-4,
    constraint              = FALSE,
    convergence             = 0.001,                    # diffenrence between target and current best
    cpus                    = NA,
    createCandFun           = createCandidate,          # function used to create the candidate
    createMutFun            = NewValueMutation,     # function used in the mutation
    crossFun                = CrossOperation,
    dontChangeCross         = NULL,                     # feature that don' t have to be used in crossover and mutation
    dontChangeMut           = NULL,                      # feature that don' t have to be used in crossover and mutation
    elitism                 = NULL,
    feature                 = NULL,
    fitnessFN               = assignFitnessRank,        # Default evaluation function
    Fun                     = NULL,
    #maxStallGenerations    = maxGenerations             # maximum number of iterations without improvement
    job                     = NULL,
    keep                    = NULL,                     # vector of fields that don't have to be touched
    #localOptGenerations    = maxGenerations
    localOptimiser          = LocalOptimisation,
    maxEvaluations          = NULL,
    maxGenerations          = NULL,
    multiPopulation         = FALSE,
    maxRelaxation           = 0    ,
    # multiPopControl       = NULL,
    mutRate                 = 0.8,                      # likelihood to perform mutation
    mutationReport          = FALSE,
    parallel                = FALSE,                     # parallelize the evaluation of the objective function
    percCross               = 0.2  ,                     # ratio between the number of chromosome to corssover and the avarege length of the candidates
    percMut                 = 0.2  ,                     # ratio between the number of chromosome to mutate length of the candidate
    plotCross               = FALSE,
    plotCrossR              = FALSE,
    plotEvolution           = FALSE,                     # Print evolution of bests
    plotEvolutionLimit      = Inf,
    plotFitness             = FALSE,
    plotPopulation          = FALSE,
    plotSigma               = FALSE,                     # Print maximum values of sigmas
    plotInterval            = 1,
    popCreateFun            = createPopulation,       # function used to create the initial population   createPopulationLHD
    printIter               = TRUE,
    printSigma              = FALSE,
    printXMin               = FALSE,
    printPlot               = FALSE,
    probability             = NULL,
    problemName             = "anonimousFunction",
    pureFeasibility         = 0   ,
    repairFun               = NULL,
    repairMutation          = NULL,
    resume                  = FALSE,
    resumeFrom              = "anonimousFunction",
    saveAll                 = FALSE,
    seed                    = sample(1e6, 1),
    selection               = selectpoolTournament,
    size                    = 30,                       # Size of population
    target                  = -Inf,                     # best value achievable
    # tournamentSize          = 6,
    updateSigma             = FALSE,
    useCrossover            = TRUE,
    vectorOnly              = FALSE,                    # pass only the values to the obj
    vectorized              = FALSE,                    # the obj accepts all the candidates togheter
    x                       = NULL
  )

  con[names(control)]  = control
  con$repairFun        = control$repairFun
  control              <- con
  if(is.null(control$resumeFrom))
    control$resumeFrom   <- paste(control$resumeFrom,Sys.time(),sep="-" )
  rm ("con")
  if(is.null(control$tournamentSize))
    control$tournamentSize = max(2,control$size / 10)
  if (is.null(control$elitism))
    control$elitism  <- floor(control$size * 0.075 + 1)
  control$toEval     <- 1 : control$size
  control$sizeToEval <- length(control$toEval)

  if (!is.null(control$maxGenerations) & !is.null(control$maxEvaluations) ){
    control$maxEvaluations <- min(control$maxEvaluations,control$size+(control$maxGenerations-1)*(control$size-control$elitism))
    cat("\n Both maxGenerations and maxEvaluations provided.The minimum will be used \n")

    } else if(!is.null(control$maxGenerations) & is.null(control$maxEvaluations)){
      control$maxEvaluations <- control$size + (control$maxGenerations-1)*(control$size-control$elitism)
  } else if ( is.null(control$maxGenerations) & !is.null(control$maxEvaluations)){
    control$maxGenerations  <- 1 + (control$maxEvaluations - control$size) %/% length(control$toEval-control$elitism)
  } else if (is.null(control$maxGenerations) & is.null(control$maxEvaluations) )
    stop("Provide maxGenerations or maxEvaluations")


  if(is.null(control$maxStallGenerations))
    control$maxStallGenerations                  <- Inf

  if(is.null(control$localOptGenerations))
    control$localOptGenerations                  <- Inf

  ########## multiPopulation  ###########################################################################################################
  if(control$multiPopulation){
    control$algoName                             <- "SCGA-MultiPop"
    if(is.null(control$multiPopControl))
      control$multiPopControl                    <- list()

    if(is.null(control$multiPopControl$migrationType))
      control$multiPopControl$migrationType      <- "evaluation"

    if(is.null(control$multiPopControl$multiPopStrategy))
      control$multiPopControl$multiPopStrategy   <- populationStrategyParallel

    if(is.null(control$multiPopControl$nMigrations))
      control$multiPopControl$nMigrations        <- control$elitism

    if(is.null(control$multiPopControl$nPopulations)){

      if(!control$parallel){
        control$multiPopControl$nPopulations      <- 2

      }else if(control$parallel)
        control$multiPopControl$nPopulations      <- control$cpus
    }
    if(is.null(control$multiPopControl$migrationInterval)){
      if(control$multiPopControl$migrationType == "generation")
        control$multiPopControl$migrationInterval <- control$maxGenerations %/% 10
      else
        control$multiPopControl$migrationInterval <- (control$maxEvaluations/control$multiPopControl$nPopulations) %/% 10
    }
  }
  return(control)
}



initializeConditions <- function()list(mainLoop = c(budgetOver=FALSE,targetReached=FALSE),stalling=c(reinitialise=FALSE,localOptimisation=FALSE) )



















