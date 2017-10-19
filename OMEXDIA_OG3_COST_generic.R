OCOST_GEN <- function (p,Vlist=NULL,Flist=NULL){
	
  # Vlist: Profile variables to be considered in the Mod Cost ( here all those variables are given at the same depths) 
  # Flist: Fluxes   to be considered in the Mod Cost
  # To consider microprofiling, we may need to complete this function ... maybe with a Mlist ..
  
  DIA<-OCALL (p)

  localdata <- subset(localdata,variable %in% Vlist)
  
  localdata <- DIA2OBS(DIA,localdata,p)
  Mp <- dcast(localdata,time~variable,value.var="modval")
  
  O<- localdata[,c("variable","time","value","err")]
  Op<-O[which(O$variable%in% Vlist),]
  Opf<-Op[which(!is.na(Op$value+ Op$err  )),]
  
#  M<-Mp
#  O<-Op

 # Fluxes 
if(!is.null(Flist))
{  flist<-lapply(Flist,function(Fi){
    ff<-( (DIA[[paste0(Fi,"flux")]] +IntegratedRate(DIA[[paste0(Fi,"Irrflux")]]))/100)
    if (Fi =="O2"){
      ff<- ff-
        (DIA[[paste0("ODU","flux")]] +IntegratedRate(DIA[[paste0("ODU","Irrflux")]]))/100
    }
    return(ff)
  })
  
  Mf<-as.data.frame(flist)
  colnames(Mf)<-paste0(Flist,"flux")
  bfl1<-subset(localdatafl,variable %in% paste0(Flist,"flux"),select=c("variable","time","value","err"))
  
  CostF<-modCost(model = Mf,
                obs   = bfl1,
                x=NULL,
                y= "value",
                err= "err",
                scaleVar = TRUE) 

    Cost<-modCost(model = Mp,
                obs   = Opf,
                x="time",
                y= "value",
                err= "err",
                scaleVar = TRUE,
                cost = CostF)
  
}else{ 
  # If only profiles are considered and no fluxes
    Cost<-modCost(model = Mp,
                obs   = Opf,
                x="time",
                y= "value",
                err= "err",
                scaleVar = TRUE)
  
}
  return (Cost)
}
