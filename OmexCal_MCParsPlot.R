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
# This functions plots enveloppe of OMEXDIA Steady-State solution from distributions range for parameters (given as MCMC output, ie. list of values) 
# Arguments : 
# * A set of parameters (the corresponding steady-state solution is computed)
# * plotdata=FALSE (True to display observations from the global localdata variable)
# * YL=20 (depth range in centimeters)
#
################

MCParsPlot<-function(MCMC, plotdata=FALSE, YL=20, NumRun = 2000) {

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

    pd<-parsdf  #[c("guess","min","max",)]
    pd$Var2<-rownames(parsdf)
    pd<-subset(pd, Var2 %in% colnames(MCMCr$pars))

    pparms<-
      ggplot(melt(MCMCr$pars))+
      geom_density(aes(x = value))+facet_wrap(~Var2, scales = "free")+
      geom_vline(data=pd,aes(xintercept = min), color="red")+
      geom_vline(data=pd,aes(xintercept = max), color="red")+
      coord_flip()
    

    return(pparms=pparms)
}
    