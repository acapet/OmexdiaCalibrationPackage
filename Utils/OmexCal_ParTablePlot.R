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
# This function  provides a table summarizing parameter values
#
# Argument : 
# * p : parameter vector
#
# Value : 
# * t : a ggplot table object.  
################

partableplot<-function(p, tableoutput=TRUE) {
  
  parsdfl<-parsdf
  parsdfl[names(parSta),"guess"]<-as.numeric(parSta)
  parsdfl[c("Temp","bwO2","bwNH3","bwNOx","bwODU","bwDIC","DispO2","DispNOx","DispNH3","DispDIC","DispODU","DispSIO","DispPO4"),"origin"]<-"sta"
  parsdfl[names(p),"guess"]<-NA
  parsdfl[names(p),"Cal."]<-as.numeric(p)
  parsdfl[names(p),"origin"]<-"CALIB"
  parsdfl[which(is.na(parsdfl$origin)),"origin"]<-""
  
  parsdfl["DispO2","printunit"]<-"cm2/d"
  parsdfl["DispNOx","printunit"]<-"cm2/d"
  parsdfl["DispNH3","printunit"]<-"cm2/d"
  parsdfl["DispODU","printunit"]<-"cm2/d"
  parsdfl["DispDIC","printunit"]<-"cm2/d"
  parsdfl["DispSIO","printunit"]<-"cm2/d"
  parsdfl["DispPO4","printunit"]<-"cm2/d"
  
  parsdfl[c("DispO2","DispNOx","DispNH3","DispDIC","DispODU","DispSIO","DispPO4"),"printfactor"]<-1
  
  parsdfforprint<-subset(parsdfl,select = c("guess","printunit","origin","Cal."))#,"min","max"
  parsdfforprint[which(parsdfforprint$origin=="sta"), "Sta."]<-parsdfforprint[which(parsdfforprint$origin=="sta"), "guess"]
  parsdfforprint[which(parsdfforprint$origin=="sta"), "guess"]<-NA
  parsdfforprint$guess <- parsdfforprint$guess*parsdfl$printfactor
  parsdfforprint$"Sta." <- parsdfforprint$"Sta."*parsdfl$printfactor
  parsdfforprint$"Cal." <- parsdfforprint$"Cal."*parsdfl$printfactor
  parsdfforprint<-parsdfforprint[,c("guess","Sta.","Cal.","printunit")]
  fpdf<-format(parsdfforprint,digits = 3,nsmall=0,scientific=F,drop0trailing=T)
  fpdf[ grep("NA",fpdf[,1]),1]<-""
  fpdf[ grep("NA",fpdf[,2]),2]<-""
  fpdf[ grep("NA",fpdf[,3]),3]<-""
  fpdf[ grep("NA",fpdf[,4]),4]<-""

  parsdfforprint$guess<-parsdfl$guess*parsdfl$printfactor
  parsdfforprint$unit<-parsdfl$printunit
  parsdfforprint<-parsdfforprint[c("Temp","biot","mixL","AlphIrr",
                                 "IrrEnh","w","Flux","rFast","rSlow","pFast","pRef",
                                 "NCrFdet","NCrSdet","pdepo","NH3Ads","rnit","bwO2",
                                 "bwNH3","bwNOx","bwODU","bwDIC","MeanFlux",
                                 "DispO2","DispNOx","DispNH3","DispDIC","DispODU"),]

if(tableoutput){
t<-qplot(1:10, 1:10, geom = "blank")+theme_bw()+
  theme(line = element_blank(),text = element_blank())+
  annotation_custom( grob = tableGrob(fpdf  )  ) 
}

if(tableoutput){
return(list(t=t, df=fpdf))
}else{
  return(df=fpdf)
}



}