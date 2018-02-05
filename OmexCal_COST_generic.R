################
#
# This script is part of the OmexdiaCalibration suite (https://github.com/acapet/OmexdiaCalibrationPackage) 
# This toolbox exploits essentially codes and methods developped by K. Soetaert (NIOZ)
#
# Arthur Capet (acapet@uliege.be), Oct 2017.
#
################
# Contributors : 
# A. Capet   , acapet@uliege.be 
# A. Eiesele ,  
################
#
# Description :
# This functions receives a vector of parameter and the list of observations variable names (profiles and fluxes) 
# It returns a model cost object (see ?modCost ) that quantifies the model skill in reproducing selected observations
#
################

OCOST_GEN <- function (p,Vlist=NULL,Flist=NULL,Mlist=NULL){
  
  # Vlist: Profile variables to be considered for Cost computation (here all those variables are given at the same depths) 
  # Flist: Fluxes to be considered in the Mod Cost
  # To consider microprofiling, we may need to complete this function ... maybe with a Mlist ..
  
  # Simulation
  DIA<-OCALL (p)
  
  # Shaping Profiles Data 
  llocaldata <- subset(localdata,variable %in% Vlist & MidDepth>0 & !is.na(localdata$value+ localdata$err))
  llocaldata <- DIA2OBS(DIA,llocaldata,p)
  sindex <-1
  llocaldata <- ddply(llocaldata, .(LowerDepth, UpperDepth), function(d){
    d$slice<-sindex
    sindex <<-sindex+1
    return(d)})
  
  Mp <- dcast(llocaldata,slice~variable,value.var="modval")
  Op <- llocaldata[,c("variable","slice","value","err")]
  
  # Shaping Fluxes Data 
  if(!is.null(Flist) )
  {  flist<-lapply(Flist,function(Fi){
    # It is considered that irrigative and diffusive fluxes should be merged to match measured (incubation) fluxes
    ff<-( (DIA[[paste0(Fi,"flux")]] +IntegratedRate(DIA[[paste0(Fi,"Irrflux")]]))/100)
    if (Fi =="O2"){
      ff<- ff-
        (DIA[[paste0("ODU","flux")]] +IntegratedRate(DIA[[paste0("ODU","Irrflux")]]))/100
    }
    return(-ff) # Changing convention to positive frmo sediment to water column (upward) 
  })
  
  Mf<-as.data.frame(flist)
  colnames(Mf)<-paste0(Flist,"flux")
  
  llocaldatafl <- subset(localdatafl,variable %in% paste0("F",Flist))
  llocaldatafl$variable<-paste0(substr(llocaldatafl$variable,2,50),"flux")
  
  Of<-llocaldatafl[,c("variable","value","err")]
  
  
  
  # Shaping MicroProfiler Data 
  
  if(!is.null(Mlist))
  { 
    
    #flist<-lapply(gsub('micro','',Mlist),function(Fi){
    #print(Fi)
    
    Fi<-"O2"
    
    obsMicro<-subset(localdatamicro,(variable==Fi), select=c("variable","Depth","value","err"))
    Mmicro  <- cbind(Grid$x.mid,DIA$y[,Fi])
    
    colnames(Mmicro)<-c('Depth',Fi)
    # llocaldata <- DIA2OBS(DIA,llocaldata,p)
  #}
  #)

}

  
  CostF<-modCost(model = Mf,
                 obs   = Of,
                 x=NULL,
                 y= "value",
                 err= "err",
                 scaleVar = TRUE) 
  
  Cost<-modCost(model = Mp,
                obs   = Op,
                x="slice",
                y= "value",
                err= "err",
                scaleVar = TRUE,
                cost = CostF)
  
  Cost<-modCost(model = Mmicro,
                obs=obsMicro,
                x="Depth",
                y="value",
                err = "err",
                scaleVar=TRUE, 
                cost = Cost)
  
  
  } else { 
    # If only profiles are considered and no fluxes
    Cost<-modCost(model = Mp,
                  obs   = Op,
                  x="slice",
                  y= "value",
                  err= "err",
                  scaleVar = TRUE)
    
  }
  return (Cost)
}
