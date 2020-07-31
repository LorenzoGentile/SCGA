sapply(paste0("R/",dir("R",pattern = "SCGA")),source)
dim=2
obj <- smoof::makeSphereFunction(dim)
for (i in seq(4,30,4)) {
  OUT <- SCGA(control=list(size=4,maxEvaluations=i,Fun=obj,feature=CreateFeature(list(lower=rep(0,dim),upper=rep(1,dim))),
                           vectorOnly=T,elitism=1,seed=1,backup=T,backupInterval=1,saveAll=T,printIter=T,resume=F))

}
OUT <- SCGA(control=list(size=4,maxEvaluations=200,Fun=obj,feature=CreateFeature(list(lower=rep(0,dim),upper=rep(1,dim))),
                         vectorOnly=T,elitism=1,seed=1,backup=T,backupInterval=1,saveAll=T,printIter=T,resume=T))
con = createControl(list(size=6,maxEvaluations=4000))

sapply(paste0("R/",dir("R",pattern = "SCGA")),source)

obj3 <- function(rad)cos(rad* 4*pi)
obj4 <- function(rad)(( -rad^2+rad )*20/9 +1 )
obj4 <- function(rad)(( rad )*.5 +1 )
computeScale <- function(x,ub=10,lb=2)(x-lb)/(ub-lb)
obj1 <- function(x,ub=10,lb=2){
  rad <- computeScale(x) ;
  out <- obj3(rad) * obj4(rad) }
obj2 <- function(x){
  -mean(x[x[,"feature"]==3,"value"]) + mean(x[x[,"feature"]==4,"value"])
  }

obj <- function(x){

  out1 <- obj1(x[x[,"feature"]==1,"value"])
  out2 <- obj2(x[x[,"feature"]!=1,])
  out <- -out1*out2

}

 run <- function(i) {out <- SCGA(control=list(size=40,maxEvaluations=3000,Fun=obj,feature=
                             CreateFeature(list(lower=c(2,0,1,1),upper=c(10,0,2,2)),types=c("repeater","integer","numeric","numeric"),
                                           dependence=list(2,c(3,4),NA,NA) ),vectorized=F,
                           vectorOnly=F,elitism=1,seed=2,backup=F,backupInterval=1,saveAll=T,printIter=T,resume=F))


 }
report <- pbapply::pbsapply(1:20, run,simplify = F)
sapply(report,function(x) x$ybest)

tab = data.frame(
rad=sapply((2:10),computeScale),
obj3=sapply(sapply((2:10),computeScale),obj3),
obj4=sapply(sapply((2:10),computeScale),obj4),
obj1=sapply(2:10,obj1))
tab = cbind(x=2:10,reshape2:::melt(tab))
ggplot(tab,aes(x=x,y=value,color=variable))+geom_line()
