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
################
#
# Description :
# This functions plots enveloppe of OMEXDIA Steady-State solution from distributions range for parameters (given as MCMC output, ie. list of values) 
# Arguments : 
# * A set of parameters (the corresponding steady-state solution is computed)
# * plotdata=FALSE (True to display observations from the global localdata variable)
# * YL=20 (depth range in centimeters)
#
################

MCFluxPlot<-function(MCMC, plotdata=FALSE, YL=20, NumRun = 2000) {

    MCMCr<- MCMC  

if(!is.null(NumRun)){
  NLINES <- max(length(MCMC$SS),NumRun)
  # not sure which best way to select within the MCMC
  if (TRUE){
    inds <- which(MCMC$SS <= quantile(MCMC$SS,NumRun/NLINES))
  }else{
    inds <-  (length(MCMC$SS)-NumRun): length(MCMC$SS)    
    }
  MCMCr$pars  <- MCMC$pars  [inds,]
  MCMCr$SS    <- MCMC$SS    [inds]
  MCMCr$sig   <- MCMC$sig   [inds]
  MCMCr$prior <- MCMC$prior [inds]
} 

    ddd<-fluxtable(parSta,tableoutput=FALSE)
    sR <- sensRange(fluxtable,parInput = MCMCr$pars,map=NULL,tableoutput=FALSE)
    sR <- subset(sR, select = !(substr(colnames(sR),start = 1,stop=8)=="variable"))
    sR <- subset(sR, select = (substr(colnames(sR),start = 1,stop=5)=="value"))
    colnames(sR)[which(substr(colnames(sR),start = 1,stop=5)=="value")] <- as.character(ddd$variable)
    
    pflux<-ggplot(melt(sR),aes(x = value))+
      geom_density()+facet_wrap(~variable, scales = "free")+      coord_flip()
    
    if (plotdata) {
      localdata <- subset(localdatafl,paste0(substring(as.vector(localdatafl$variable),2),"flux") %in% melt(sR)$variable )
      localdata$variable <- paste0(substring(as.vector(localdata$variable),2),"flux")
      pflux <- pflux +
        geom_rect(data=localdata,aes(xmin=value-err, xmax=value+err, ymin=0, ymax=Inf), alpha=0.3, color= "green",fill="green")
      }
    
    return(pflux=pflux)
}
    