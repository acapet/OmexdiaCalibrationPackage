################
#
# This script is part of the OmexdiaCalibration suite (https://github.com/acapet/OmexdiaCalibrationPackage) 
# This toolbox exploits essentially codes and methods developped by K. Soetaert (NIOZ)
#
# Arthur Capet (acapet@ulg.ac.be), Oct 2017.
#
################
# Contributors : 
# A. Capet   , acapet@ulg.ac.be 
# A. Eiesele ,  
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
#   * p is a parameter vector. The global parameter vectors set-up by OmexCal_BasicSetup.R is used by default.
#
# Output : 
#   * A modified parameter vector. 
#   * ! In addition global values are modified ! This concerns
#       - porGrid
#       - porGridSolid
#       -
# Potential updates : 
#   Consider local Temp and Sal to update chemical diffusion coefficients

################

OmexCal_AdaptForSta <- function (p=pars) {
  
  # parsl will be updated below
  parsl<-p
  
  ############
  # Porosity #
  ############
  if('Porosity'%in% unique(dfProfiles$variable)){
    ## First option : porosity is provided as a profile in the "Profiles" sheet of the input .xls file.
    print('Not tested yet, please complete OmexCal_AdaptParsForStation.R if needed')
    
    dfPor <- subset(dfProfiles, variable==Porosity, select=c(MidDepth,value))
    
    porGrid <- setup.prop.1D(xy = dfPor,
                             grid = Grid,
                             interpolate = "linear")
    
    porGridSolid$mid <- 1-porGrid$mid
    porGridSolid$int <- 1-porGrid$int
    
  } else {
    ## Second option: Info for set up porosity profile are present in the "Station" sheet of the input .xls file.
    # portop : Porosity below the SWI (considered to be given at)
    # porbot : Porosity at depth (considred as asymptotic value for great depth)
    # pora   : Exponential decrease rate for the porosity profile

    if (!is.null(localdatasta$portop)){# & !is.na(localdatasta$portop)){
      warning(paste0(c('Using local portop value for ',sta,' ',cam)))
      portop <- localdatasta$portop
    }else{
      warning(paste0(c('Using global portop value for ',sta,' ',cam)))
      portop <- p['portop']
    }
    
    if (!is.null(localdatasta$porbot)){# & !is.na(localdatasta$porbot)){
      warning(paste0(c('Using local porbot value for ',sta,' ',cam)))
      porbot <- localdatasta$porbot
    }else{
      warning(paste0(c('Using global porbot value for ',sta,' ',cam)))
      porbot <- p['porbot']
    }
    
    if (!is.null(localdatasta$pora)){# & !is.na(localdatasta$pora)){
      warning(paste0(c('Using local pora value for ',sta,' ',cam)))
      pora <- localdatasta$pora
    }else{
      warning(paste0(c('Using global pora value for ',sta,' ',cam)))
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
  
  porGrid      <<- porGrid
  porGridSolid <<- porGridSolid
    
  #########################
  # Bottom Concentrations #
  #########################
  
  bwnames <- names(p)[which(substr(names(p),0,2)=='bw')]
  bwlocal <- localdatasta[which(substr(names(localdatasta),0,2)=='bw')]
  bwlocal <- bwlocal[which(names(bwlocal)%in% names(p))]
  
  parsl[names(bwlocal)]<-as.numeric(bwlocal)
  
  # The following gives sediment advection at depth in cm/d
  
  if (!is.null(localdatasta$Accumulation)){# & !is.na(localdatasta$pora)){
    warning(paste0(c('Using local \'w\' value for ',sta,' ',cam)))
    warning(paste0("Assuming Accumulation given in ", datafile,
                   " are gr/cm²/yr and a dry sediment density of 2.5 gr/cm³.
          Consider adapting OmexCal_AdaptParsForStation if needed (or, better, convert your data)"))
    
    parsl["w"]<-as.numeric(localdatasta["Accumulation"]/2.5/365 )
  }else{
    warning(paste0(c('Using default \'w\' value for ',sta,' ',cam)))
  }
  return(parsl)
}