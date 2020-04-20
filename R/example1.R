runExample1 <- function(dim=4){
feature =CreateFeature(bounds=list(lower=rep(0,dim),upper=rep(1,dim)),condOfExcistance = NULL,types = NULL,dependence = NULL,
                             others = list(nick  = paste(1:dim), label=1:dim,fixID = 1:dim))

control <- list(
  budgetTot                = 1,
  convergence              = 0.00,                    # diffenrence between target and current best
  cpus                     = 1,
                   # function used to create the candidate           # function used in the mutation

  dontChangeCross          = NULL,                     # feature that don' t have to be used in crossover and mutation
  dontChangeMut            = NULL,                      # feature that don' t have to be used in crossover and mutation

  elitism                  = 2,
  feature                  = feature,

  Fun                      = function(x,...){ sum(x[,"value"]^2)},
  maxStallGenerations      = NULL ,                       # maximum number of iterations without improvement
  job                      = NULL,
  keep                     = "fixID",                     # vector of fields that don't have to be touched
  maxChange                = .8,                        # ratio between the number of chromosome to corssover and the half of the avarege length of the candidates
  maxEvaluations           = 1000,
  maxGenerations           = NULL,
  multiPopulation          = T,
  mutRate                  = .5,                      # likelihood to perform mutation
  parallel                 = FALSE,                     # parallelize the evaluation of the objective function
  percMut                  = 1,
  plotCross                = FALSE,
  plotEvolution            = F,
  plotCrossR               = FALSE,                         # Print evolution of bests
  plotPopulation           = FALSE,
  plotSigma                = FALSE,                     # Print maximum values of sigmas
  plotInterval             = 3,

  printSigma               = FALSE,      # Print maximum values of sigmas
  probability              = NULL,
  repairCross              = NULL,
  repairFun                = NULL,
  repairMutation           = NULL,
  resume                   = FALSE,
  resumeFrom               = NULL,
  saveIter                 = FALSE,
  seed                     = 1,

  size                     = 5,#Pop,                       # Size of population
  target                   = -1,                     # best value achievable
  tournamentSize           = 2,
  updateSigma              = T,
  useCrossover             = TRUE,
  vectorOnly               = FALSE,                  # pass only the values to the obj
  vectorized               = FALSE,                    # the obj accepts all the candidates togheter
  x                        = NULL,
  y                        = NULL
)
out <- SCGA(
  control=control
)

}
# files <- dir(getwd())
# files <- files[grep("SCGA",files)]
# sapply(files,source)
