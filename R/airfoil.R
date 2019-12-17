airfoilOptObjVec      <- function(x){
  len = length(x)
  folders = paste0(getwd(),"/runningDir-",1:len)
  folderManage(folders)
  y = mapply(airfoilOptObj,x,folders)
}
folderManage<-function(folders){

  if(!file.exists(folders))
    sapply(folders,function (x) system(paste0("cp -rp template ", x)))
}
airfoilOptObj         <- function(x,directory){
  x                   <- x[order(x[,"feature"]),]
  # browser()
  writeInput(x,directory)
  executeJob(directory)
  out                 <- readOutput(directory)
}
executeJob            <- function(directory){
  system(paste0("cd ",directory," && ./run_xfoil.sh"),wait = T)
}
readOutput            <-function(directory){
  out                 <- read.table(paste0(directory,"/eval_obj.out"))
  file.remove(paste0(directory,"/eval_obj.out"))
  file.remove(paste0(directory,"/eval_obj.in"))
  if(out$V3[out$V1 == "ERROR"] != 0 || out$V3[out$V1 == "Airfoil_leading_edge_radius"] < 0.007 || out$V3[out$V1 == "Airfoil_trailing_edge_angle"] < 13 )
    return(1000)
  else
    return(out$V3[out$V1 == "CD"])

}
writeInput            <- function(x,directory){
  browser()

  additionalVariables <- x[,"value"]
  addtionaleText = paste0("W(",0:(length(additionalVariables)-1),") = ",additionalVariables," \n",collapse = "")

  toWrite= paste0("CL = 0.5 \nMACH = 0.0 \nReynolds_number = 500000 \nXTRLO = 0.95 \nXTRUP = 1 \nMAX_THICK = 0.12",addtionaleText)



  fileName = paste0(directory,"/eval_obj.in")


  write(toWrite,file = fileName)

}
