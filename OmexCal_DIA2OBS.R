DIA2OBS <- function (DIA,localdata,p) {
  # Average Model value for the depth of data in localdata
  ModelDepths <- Grid$x.mid
  
  modval<-localdata$value
  for (i in (1:nrow(localdata))){
     modval[i]<- mean(
       DIA$y[ which( ModelDepths>localdata$UpperDepth[i] & ModelDepths<localdata$LowerDepth[i]) , as.character(localdata$variable[i])])
  }

  localdata$modval<-modval
  return(localdata)
}
