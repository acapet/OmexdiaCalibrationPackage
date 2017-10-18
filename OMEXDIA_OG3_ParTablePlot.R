partableplot<-function(p) {
  
  parsdfl<-parsdf
  
#   [c("Temp","biot","mixL","AlphIrr","IrrEnh","w","MeanFlux","rFast","rSlow","pFast","pRef",
#                           "NCrFdet","NCrSdet","pdepo","NH3Ads","rnit","bwO2","bwNH3","bwNO3","bwODU","bwDIC"),]
#   
  parsdfl[names(parSta),"guess"]<-as.numeric(parSta)
  parsdfl[c("Temp","bwO2","bwNH3","bwNO3","bwODU","bwDIC","DispO2","DispNO3","DispNH3","DispDIC","DispODU","Flux"),"origin"]<-"sta"
#  parsdfl[names(p),"guess"]<-as.numeric(p)
 #parsdfl[names(p),"origin"]<-"CALIB"
  parsdfl[names(p),"guess"]<-NA
  parsdfl[names(p),"Cal."]<-as.numeric(p)
 parsdfl[names(p),"origin"]<-"CALIB"
parsdfl[which(is.na(parsdfl$origin)),"origin"]<-""
  
  parsdfl["WPOC","printunit"]<-"m/d"
  parsdfl["DispO2","printunit"]<-"cm2/d"
  parsdfl["DispNO3","printunit"]<-"cm2/d"
  parsdfl["DispNH3","printunit"]<-"cm2/d"
  parsdfl["DispODU","printunit"]<-"cm2/d"
  parsdfl["DispDIC","printunit"]<-"cm2/d"
  parsdfl["Flux","printunit"]<-"mmol C/m3"
  parsdfl[c("WPOC","DispO2","DispNO3","DispNH3","DispDIC","DispODU","Flux"),"printfactor"]<-1
  
#parsdfforprint<-subset(parsdfl,select = c("guess","unit","origin"))#,"min","max"
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
#parsdfforprint$min<-parsdfl$min*parsdfl$printfactor
#parsdfforprint$max<-parsdfl$max*parsdfl$printfactor
parsdfforprint$unit<-parsdfl$printunit
parsdfforprint<-parsdfforprint[c("Temp","biot","mixL","AlphIrr","IrrEnh","w","Flux","rFast","rSlow","pFast","pRef",
                     "NCrFdet","NCrSdet","pdepo","NH3Ads","rnit","bwO2","bwNH3","bwNO3","bwODU","bwDIC","WPOC","DispO2","DispNO3","DispNH3","DispDIC","DispODU"),]

t<-qplot(1:10, 1:10, geom = "blank")+theme_bw()+
  theme(line = element_blank(),text = element_blank())+
  annotation_custom( grob = tableGrob(fpdf  )  ) 

return(t)
}