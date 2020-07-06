# sapply(paste0("R/",dir("R",pattern = "SCGA")),source)
# dim=2
# obj <- smoof::makeSphereFunction(dim)
# for (i in seq(4,30,4)) {
#   OUT <- SCGA(control=list(size=4,maxEvaluations=i,Fun=obj,feature=CreateFeature(list(lower=rep(0,dim),upper=rep(1,dim))),
#                            vectorOnly=T,elitism=1,seed=1,backup=T,backupInterval=1,saveAll=T,printIter=T,resume=T))
#
# }
# OUT <- SCGA(control=list(size=4,maxEvaluations=200,Fun=obj,feature=CreateFeature(list(lower=rep(0,dim),upper=rep(1,dim))),
#                          vectorOnly=T,elitism=1,seed=1,backup=T,backupInterval=1,saveAll=T,printIter=T,resume=T))
# con = createControl(list(size=6,maxEvaluations=4000))
