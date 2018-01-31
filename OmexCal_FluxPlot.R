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
# This functions provides a table with observed and simulated fluxes and a summary of integrated diagnostics
#
################
Fluxplot<-function(p, plotdata=FALSE, YL=20, runnames=NULL) {
  
   if (is.list(p)){
    fdfs<-lapply(p,fluxtable,tableoutput=FALSE)
  }else{
    fdf<-fluxtable(p,tableoutput=FALSE)
    fdfs<-list(fdf)
  }

  # Setting names if not provided
  if (is.null(runnames)){runnames<-as.character(seq(1,length(fdfs))) }
  
  bo<-lapply(fdfs, function(fdf){
    subset(fdf, subset=grepl("flux",fdf$variable))
    })
  
  for (i in 1:length(bo)){
    bo[[i]]$runname<-runnames[i]
  }
  bos<-do.call(rbind,bo)
  
  if (plotdata) {
    localdata <- subset(localdatafl,paste0(substring(as.vector(localdatafl$variable),2),"flux") %in% bos$variable )
  }
  
  p1<-ggplot( bos,aes(y=value,color=runname))+
    geom_point(aes(x = runname))+facet_wrap(~variable,scales="free")+theme_light()+scale_color_brewer(palette = "RdYlGn")+ylab("mmol/m²/d")
  
  if (plotdata) {
    localdata$variable <- paste0(substring(as.vector(localdata$variable),2),"flux")
    p1 <- p1 +
      geom_rect(data=localdata,aes(ymin=value-err, ymax=value+err, xmin=0, xmax=Inf), alpha=0.3, color= "black",fill="grey")
  }
  
  return(p1)
}