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
#
# This function adapts parameters for local value.  By local we mean, related to the station and campaign under consideration.
# All parameters are given an initial value (global) in the OmexCal_BasicSetup.R files. 
# This functions overwrites global values with station values (Porosity grid, diffusion grid) 
# !! This means that those local value will remains afterwards !!
#
# The parameters to be adapted are : 
# * Porosity : 
# * Sedimentation accumulation rate : 
# * Bottom water concentrations :
#
# Input : 
#   * [p]    : parameter vector. The global parameter vectors set-up (pars) by OmexCal_BasicSetup.R is used by default.
#   * [station]  : station name, 
# Output : 
#   * A modified parameter vector. 
#   * ! In addition global values are modified ! This concerns
#       - porGrid
#       - porGridSolid
#       -
# Potential updates : 
#   Consider local Temp and Sal to update chemical diffusion coefficients
# Oxygen codes for generting help file
#' Adapt global parameter vector with local values for a give station
#' 
#' @param p An omexdia parameter vector. The global parameter vector (pars) set up by OmexCal_BasicSetup.R is used by default.
#' @param station A station name corresponding to an entry in the data file.
#' @param campaign A campaign name corresponding to an entry in the data file.
#' @return adapted p vector, where accumulation rate and bottom concetration have been updated on the basis of the user data-file.
#'  Note that global porosity parameter are modified when this function is called.
#' @examples
#' source('OmexCal_MinimumExmaple.R')
################

OmexCal_AdaptForSta <- function (p=pars, station=sta, campaign=cam) {
  
  # parsl will be updated below
  parsl<-p
  
  ############
  # Porosity #
  ############
  if('Porosity'%in% unique(dfProfiles$variable)){
    ## First option : porosity is provided as a profile in the "Profiles" sheet of the input .xls file.
    
    dfPor <- subset(localdata, variable=="Porosity", select=c(MidDepth,value))
    dfPor <- subset(dfPor,!is.na(MidDepth)&!is.na(value))
    
    porGrid <- setup.prop.1D(xy = as.matrix(dfPor),
                             grid = Grid,
                             interpolate = "linear")
    
    porGridSolid$mid <- 1-porGrid$mid
    porGridSolid$int <- 1-porGrid$int
    
  } else {
    ## Second option: Info for set up porosity profile are present in the "Station" sheet of the input .xls file.
    # portop : Porosity below the SWI (considered to be given at)
    # porbot : Porosity at depth (considered as asymptotic value for great depth)
    # pora   : Exponential decrease rate for the porosity profile

    if (!is.null(localdatasta$portop)){# & !is.na(localdatasta$portop)){
      warning(paste0(c('Using local portop value for ',station,' ',campaign)))
      portop <- localdatasta$portop
    }else{
      warning(paste0(c('Using global portop value for ',station,' ',campaign)))
      portop <- p['portop']
    }
    
    if (!is.null(localdatasta$porbot)){# & !is.na(localdatasta$porbot)){
      warning(paste0(c('Using local porbot value for ',station,' ',campaign)))
      porbot <- localdatasta$porbot
    }else{
      warning(paste0(c('Using global porbot value for ',station,' ',campaign)))
      porbot <- p['porbot']
    }
    
    if (!is.null(localdatasta$pora)){# & !is.na(localdatasta$pora)){
      warning(paste0(c('Using local pora value for ',station,' ',campaign)))
      pora <- localdatasta$pora
    }else{
      warning(paste0(c('Using global pora value for ',station,' ',campaign)))
      pora <- p['pora']
    }
    
    porGrid             <- setup.prop.1D(func  = exp.profile, x.0 = 0,
                                         y.0   = portop ,
                                         y.inf = porbot ,
                                         x.att = 1/pora , 
                                         grid = Grid)
    porGrid$mid[N]   <- porGrid$mid[N-1]
    porGrid$int[N+1] <- porGrid$mid[N]
    porGrid$int[N]   <- porGrid$mid[N]
    porGridSolid$mid <- 1-porGrid$mid
    porGridSolid$int <- 1-porGrid$int
  }
  
  # ! Those are global variables, ie. they're not passed as argument between functions, but defined globally
  porGrid      <<- porGrid
  porGridSolid <<- porGridSolid
    
  #########################
  # Bottom Concentrations #
  #########################
  
  bwnames <- names(p)[which(substr(names(p),0,2)=='bw')]
  bwlocal <- localdatasta[which(substr(names(localdatasta),0,2)=='bw')]
  bwlocal <- bwlocal[which(names(bwlocal)%in% names(p))]
  
  parsl[names(bwlocal)] <- as.numeric(bwlocal)
  
  # The following gives sediment advection at depth in cm/d
  
  if (!is.null(localdatasta$Accumulation)){# & !is.na(localdatasta$pora)){
    warning(paste0(c('Using local \'w\' value for ',station,' ',campaign)))
    warning(paste0("Assuming Accumulation given in ", datafile,
                   " are gr/cm²/yr and a dry sediment density of 2.5 gr/cm³.
          Consider adapting Utils/OmexCal_AdaptParsForStation if needed (or, better, convert your data)"))
    
    parsl["w"]<-as.numeric(localdatasta["Accumulation"]/2.5/365 )
  }else{
    warning(paste0(c('Using default \'w\' value for ',station,' ',campaign)))
  }
  return(parsl)
}