obj <- smoof::makeSphereFunction(20)

OUT <- SCGA(control=list(size=4,maxEvaluations=4000,Fun=obj,feature=CreateFeature(list(lower=rep(0,20),upper=rep(1,20))),vectorOnly=T,elitism=1,seed=1,backup=T,backupInterval=1))
con = createControl(list(size=6,maxEvaluations=4000))
