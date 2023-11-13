################
#
# This script is part of the OmexdiaCalibration suite (https://github.com/MAST-ULiege/OmexdiaCalibrationPackage) 
# This toolbox exploits essentially codes and methods developed by K. Soetaert (NIOZ)
#
# Arthur Capet (acapet@ulg.ac.be), Oct 2017.
#
################
# Contributors : 
# A. Capet   , acapet@ulg.ac.be 
# A. Eisele ,  2017, annika.eisele@hzg.de
################
#
# Description :
#' 
#' @param dfstation a dataframe of stations and campaigns (as set by OmexCal)
#' @param v a (list of) station-wise variable(s) (eg. flux at SWI, integrated quantity, .. )
#' @param return.error=TRUE if True, an error column ('err') is added to the data frame
#' @param MC=FALSE if TRUE, the MCMC calibration is used
#' @return A data frame of the requested values for all stations  
#' @examples
#' source('OmexCal_MinimumExmaple.R')
#' 
#' ################

OmexCal_Collect <- function(dfstation, v=NULL, return.error=TRUE, MC=FALSE) {
  
  #dfstation<-dfStations
  #MC=TRUE
   dcollect <- ddply(dfstation,.(Station,Campaign),function(dsub){  
    # icamosta<-1
    sta<-dsub$Station
    cam<-dsub$Campaign
    
    # Get a flux value
    totdir <- paste0('Cases/',casedir,'/',cam,'/',sta,'/')
    load( file = paste(totdir,"FitFinal_pSta.RData",sep=""))

    localdata    <<- subset(dfProfiles, Station==sta & Campaign == cam)
    localdatafl  <- subset(dfFluxes,   Station==sta & Campaign == cam)
    localdatasta <<- subset(dfStations, Station==sta & Campaign == cam)
    localdatamicro <- subset(dfO2micro, Station==sta & Campaign == cam)
    
    parSta      <<- OmexCal_AdaptForSta(pars, station = sta, campaign = cam)
    if (MC){
    load(file = paste(totdir,"Fit","_MCMC.RData",sep=""))
    df<-MCFluxPlot(MCMC, dfout = TRUE,NumRun = 500)
    } else {
      df<-fluxtable(parSta,tableoutput=FALSE)
    }
    dsta<- dcast(melt(df,id.vars = "variable", variable.name = "val"), .~variable+val)
    
    return(cbind(dsub,dsta))
   })
  }


#####


