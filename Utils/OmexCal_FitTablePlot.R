################
#
# This script is part of the OmexdiaCalibration suite (https://github.com/MAST-ULiege/OmexdiaCalibrationPackage) 
# This toolbox exploits essentially codes and methods developed by K. Soetaert (NIOZ)
#
# Arthur Capet (acapet@ulg.ac.be), Oct 2017.
#
################
# Contributors : 
# A. Capet , acapet@ulg.ac.be 
# A. Eisele, annika.eisele@hzg.de
################
#
# Description :
# This function provides a table summarizing model cost functions qunatifying the model skills in reproducing observations
# Both variable-specific and all-variables metrics are provided. 
#
# Argument : 
# * IN : paramter vector OR OMEXDIA model outputs
#
# Value : 
# * t : a ggplot table object.  
################

fittableplot<-function(IN) {
  
  if (class(IN)=="modCost"){
    fdf<-data.frame("SSR"=IN$var$SSR)
    rownames(fdf)<-IN$var$name
    fdf["ModelSSR",]<-IN$model
  } else { # assuming modFit class
    fdf<-data.frame("MSR"=IN$var_ms)
    fdf["ModelSSR",]<-IN$ssr
  }
    
  t<-qplot(1:10, 1:10, geom = "blank")+theme_bw()+
    theme(line = element_blank(),text = element_blank())+
    annotation_custom(grob = tableGrob(format(fdf,digits = 3,nsmall=0,scientific=F,drop0trailing=T))) 
  
return(t)
}