################
#
# This script is part of the OmexdiaCalibration suite (https://github.com/acapet/OmexdiaCalibrationPackage) 
# This toolbox exploits essentially codes and methods developped by K. Soetaert (NIOZ)
#
# Arthur Capet (acapet@ulg.ac.be), Oct 2017.
#
################
# Contributors : 
# A. Capet , acapet@ulg.ac.be 
################
#
# Description :
# This functions plots some OMEXDIA Steady-State solution
# Arguments : 
# * A set of parameters (the corresponding steady-state solution is computed)
# * plotdata=FALSE (True to display observations from the global localdata variable)
# * YL=20 (depth range in centimeters)
#
################

SimplotMicro<-function(p, plotdata=FALSE, YL=2, runnames=NULL) {
  
   if (is.list(p)){
    DIs<-lapply(p,OCALL)
  }else{
    DI<-OCALL(p)
    DIs<-list(DI)
  }
  
  # Setting names if not provided
  if (is.null(runnames)){runnames<-as.character(seq(1,length(DIs))) }
  

  bo<-lapply(DIs, function(DI){
    as.data.frame(cbind("O2"=DI$y[,"O2"],grid=Grid$x.mid))
  })
  
  
  for (i in 1:length(bo)){
    bo[[i]]$runname<-runnames[i]
  }
  
  bos<-do.call(rbind,bo)
  
  if (plotdata) {
    localdata <- subset(localdatamicro,variable %in% c("O2") & Depth>0 & !is.na(localdatamicro$value+ localdatamicro$err))  
     #b         <- DIA2OBS(DIs[[1]],localdata,p)
  }
  
  p1 <- ggplot( melt(bos,id.vars = c("grid","runname")) , aes(y=grid,x=value,color=runname))+
    geom_path()+facet_wrap(~variable,nrow=5,ncol=3,scales="free")+theme_light()+
    ylim(YL,0)+ylab(label="depth, cm")+scale_color_brewer(palette = "RdYlGn")
  
  if (plotdata) {
    p1 <- p1 +
        geom_errorbarh(data=localdata,aes(x=value,y=(Depth)/2,xmin=value-err,xmax=value+err),color='black')+
      geom_point(data=localdata,aes(x=value,y=(Depth)/2),color='black')
    
  }
  
  return(p1)
}