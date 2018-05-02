################
#
# This script is part of the OmexdiaCalibration suite (https://github.com/MAST-ULiege/OmexdiaCalibrationPackage) 
# This toolbox exploits essentially codes and methods developped by K. Soetaert (NIOZ)
#
# Arthur Capet (acapet@ulg.ac.be), Oct 2017.
#
################
# Contributors : 
# A. Capet , acapet@ulg.ac.be 
# A. Eisele, annika.eisele@hzg.de
################

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
