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

Simplot<-function(p, plotdata=FALSE, YL=20, runnames=NULL) {
  
   if (is.list(p)){
    DIs<-lapply(p,OCALL)
  }else{
    DI<-OCALL(p)
    DIs<-list(DI)
  }
  
  # Setting names if not provided
  if (is.null(runnames)){runnames<-as.character(seq(1,length(DIs))) }
  
  bo<-lapply(DIs, function(DI){
    as.data.frame(cbind(DI$y,grid=Grid$x.mid,por=porGrid$mid))
  })
  
  for (i in 1:length(bo)){
    bo[[i]]$runname<-runnames[i]
  }
  
  bos<-do.call(rbind,bo)
  
  if (plotdata) {
    localdata <- subset(localdata,variable %in% colnames(DIs[[1]]$y) & MidDepth>0 & !is.na(localdata$value+ localdata$err))  
     b         <- DIA2OBS(DIs[[1]],localdata,p)
  }
  
  p1<-ggplot( melt(bos,id.vars = c("grid","runname")) , aes(y=grid,x=value,color=runname))+
    geom_path()+facet_wrap(~variable,nrow=5,ncol=3,scales="free")+theme_light()+
    ylim(YL,0)+ylab(label="depth, cm")+scale_color_brewer(palette = "RdYlGn")
  
  if (plotdata) {
    p1 <- p1 +
      geom_errorbar(data=b,aes(x=value,y=(LowerDepth+UpperDepth)/2,ymin=LowerDepth,ymax=UpperDepth),color='black')+
      # geom_errorbar(data=b,aes(x=modval,y=(LowerDepth+UpperDepth)/2,ymin=LowerDepth,ymax=UpperDepth),color='red',size=2)+
      geom_errorbarh(data=b,aes(x=value,y=(LowerDepth+UpperDepth)/2,xmin=value-err,xmax=value+err),color='black')
  }
  
  return(p1)
}