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


mcsubset <- function(MCMC,NumRun=2000){
  MCMCr<-MCMC
  if(!is.null(NumRun)){
    NLINES <- max(length(MCMC$SS),NumRun)
    # not sure which best way to select within the MCMC
    if (TRUE){
      inds <- which( MCMC$SS <= quantile(MCMC$SS,NumRun/NLINES))
      if (length(inds)>NumRun){
        inds <- inds[ (length(inds)-NumRun +1) : length(inds) ]
      }
    }else{
      inds <-  (length(MCMC$SS)-NumRun): length(MCMC$SS)    
    }
    MCMCr$pars  <- MCMC$pars  [inds,]
    MCMCr$SS    <- MCMC$SS    [inds]
    MCMCr$sig   <- MCMC$sig   [inds]
    MCMCr$prior <- MCMC$prior [inds]
  } 
  return(MCMCr)
}
