################################################################################################################
#
# This script is part of the OmexdiaCalibration suite (https://github.com/MAST-ULiege/OmexdiaCalibrationPackage) 
# This toolbox exploits essentially codes and methods developed by K. Soetaert (NIOZ)
#
# Arthur Capet (acapet@ulg.ac.be), Oct 2017.
#
################################################
# Contributors : 
# A. Capet , acapet@ulg.ac.be 
# A.Eisele, 2017, annika.eisele@hzg.de
################################################
#
# Description :
# The present script implement a calibration of the OMEXDIA model on user data, looping through stations, 
# or campaigns. Calibration is based on the FME package.
# * The first phase follows the stepwise approach defined in Capet et al . (in prep.)
# * Second phase considers a MCMC procedure
################################################
# Load model and observation data
################################################

# load OMXEDIA model
source("Utils/OmexCal_Load.R")

# load station data
  casedir   <- 'HAMMOND'
  stalist    <- c("H2","H4","H6","H7")
  camlist    <- c("Sep89")
  # The Stepwise calibration procedure consists in using subset of data to approximate subset of parameters. 
  # It ends with a global calibration starting from the guess value obtained from previous steps
  doStepWise <- TRUE
  pseudoNrun <- 500
  
  # The MCMC calibration needs an acceptable starting point, as may be provided from the stepwise procedure
  doMCMC     <- TRUE
  NitMC      <- 10000 # MCMC iterations
  
userfile  <- paste0('Cases/',casedir,'/','UsersDefinitions.R')
source(userfile)

source("Utils/OmexCal_Load_Data.R") 
# Summarize ( generate summary plot in the case directory)
if (TRUE){
  source('Utils/OmexCal_PlotCase.R')
}

# subsetting station list
dsStasub <- subset(dfStations,Station%in%stalist & Campaign %in% camlist)

#Looping
for (icamosta in (1:nrow(dsStasub))){
  sta<-dsStasub$Station[icamosta]  
  cam<-dsStasub$Campaign[icamosta]
  
  # Create "local" dataframes, specific to one station/campaign.
  localdata    <- subset(dfProfiles, Station==sta & Campaign == cam)
  localdatafl  <- subset(dfFluxes,   Station==sta & Campaign == cam)
  localdatasta <- subset(dfStations, Station==sta & Campaign == cam)
  localdatamicro <- subset(dfO2micro, Station==sta & Campaign == cam)
  
  # Set the non-local irrigation framework
  parsdf["AlphIrr","guess"]<-10/365
  parsdf["IrrEnh","guess"]<-1 
  
  # Load parameters
  parRange    <- parsdf[,c("guess","min","max","unit","printfactor","printunit")]
  
  # Update parameters with station-specifc values (+ update of the global porosity grid )
  parSta      <<- OmexCal_AdaptForSta(pars)
  
  # Create new folder
  dir.create(paste0('Cases/',casedir,'/',cam))
  dir.create(paste0('Cases/',casedir,'/',cam,'/',sta))
  
  totdir <- paste0('Cases/',casedir,'/',cam,'/',sta,'/')
  
  # Defining the list of parameters that will be calibrated in at least one of the calibration steps
  parRange <- parRange[which(rownames(parRange) %in% unlist(PLIST)),] 
  parsmin  <- as.numeric(as.matrix(parRange$min)); names(parsmin) <- rownames(parRange); 
  parsmax  <- as.numeric(as.matrix(parRange$max)); names(parsmax) <- rownames(parRange); 
  
  # # # # # # # 
  # Stepwise  #
  # # # # # # # 
  print(sta)
  
  if (doStepWise) {
  
  Fitlist  <- list()
  Parlist  <- list()
  Costlist <- list()
    
  # The first step is 
  Parlist[[1]]<-parSta
  
  Costlist[[1]] <-OCOST_GEN(parSta,
                  Vlist=unique(unlist(VLIST)),
                  Flist=unique(unlist(FLIST)),
                  Mlist=unique(unlist(MLIST)))$var
  
  for (ifit in c(1:length(PLIST))){
    print(paste('Fit',ifit))
    Fit <- modFit(f=OCOST_GEN,
                  p=parSta[PLIST[[ifit]]],
                  Vlist=tryCatch(VLIST[[ifit]], error = function (e) NULL),
                  Flist=tryCatch(FLIST[[ifit]], error = function (e) NULL),
                  Mlist=tryCatch(MLIST[[ifit]], error = function (e) NULL),
                  control=list(numiter=pseudoNrun),
                  lower=parsmin[PLIST[[ifit]]],
                  upper=parsmax[PLIST[[ifit]]], 
                  method="Pseudo")
    paste("Fit", ifit, "done")
    
    #Updating the value of parSta with calibrated values 
    parSta[c(names(Fit$par))] <- as.numeric(Fit$par)
    
    Parlist [[ifit+1]] <- parSta
    Fitlist [[ifit]]   <- Fit
    Costlist[[ifit+1]] <- OCOST_GEN(parSta,
                              Vlist=unique(unlist(VLIST)),
                              Flist=unique(unlist(FLIST)),
                              Mlist=unique(unlist(MLIST)))$var
    
    save(list = 'Fit', file = paste(totdir,"Fit",ifit,".RData",sep=""))
    save(list = 'parSta', file = paste(totdir,"Fit",ifit,"_pSta.RData",sep=""))
    #print(parSta)
  }
  
  ##########################
  ## Collinearity + Refit ##
  ##########################
  
  # Assessing parameters sensitivity
  Sens <- sensFun(func=OCOST_GEN,
                  parms=parSta[unique(unlist(PLIST))],
                  Vlist=unique(unlist(VLIST)),
                  Flist=unique(unlist(FLIST)),
                  Mlist=unique(unlist(MLIST))
                  )
  
  # Some Sensitivity Plots
  pdf(paste(totdir,"Sens.pdf",sep=""),width=10,height=10)
  pairs(Sens)
  dev.off()
  
  pdf(paste(totdir,"Sens2.pdf",sep=""),width=10,height=10)
  plot(Sens, which=unique(unlist(VLIST)))
  dev.off()
  
  sS<-summary(Sens)
  sS$param<-rownames(sS)
  sS$param <- factor(sS$param, levels = sS$param[order( sS$L1,decreasing = T)])
  
  g1<-ggplot(as.data.frame(sS),aes(x=param,y=L1))+geom_point()
  pdf(paste(totdir,"Sens3.pdf",sep=""),width=10,height=10)
  print(g1)
  dev.off()
  
  ## <Arthur 29012018
  ##  Should include a warning there if some parameters show a sensitivity of 0 
  
  # Assessing parameters collinearity, based on the sensitivity analysis
  cc<-collin(Sens)
  
  plot(cc, ylim=c(0,100))
  abline(h = 20, col = "red")
  
  c2<-cc[cc[,"collinearity"] < 20 &
           cc[,"mixL"]==1  &
           cc[,"AlphIrr"]==1,]
  
  c2<-c2[which(c2$N==max(c2$N)),]
  
  cbest<-c2[which.min(c2$collinearity),]
  cbest
  
  PLISTFinal<-names(parSta[unique(unlist(PLIST))])[cbest==1]
  
  FitFinal <- modFit(f=OCOST_GEN,
                     p=parSta[PLISTFinal],
                     Vlist=unique(unlist(VLIST)),
                     Flist=unique(unlist(FLIST)),
                     Mlist=unique(unlist(MLIST)),
                     control=list(numiter=pseudoNrun*10),
                     lower=parsmin[PLISTFinal],
                     upper=parsmax[PLISTFinal], 
                     method="Pseudo")
  
  parSta[c(names(FitFinal$par))]<-as.numeric(FitFinal$par)

  Fitlist[[length(PLIST)+1]]<-FitFinal
  Parlist[[length(PLIST)+2]]<-parSta
  
  save(list = 'FitFinal', file = paste(totdir,"FitFinal.RData",sep=""))
  save(list = 'parSta', file = paste(totdir,"FitFinal_pSta.RData",sep=""))
  
  Cost <- OCOST_GEN(parSta,
                  Vlist=unique(unlist(VLIST)),
                  Flist=unique(unlist(FLIST)),
                  Mlist=unique(unlist(MLIST))
                  )$var

  Costlist[[length(PLIST)+2]] <- Cost
  
  save(list = 'Cost', file = paste(totdir,"Fit","_Cost.RData",sep=""))
  ReportGen(userfile,Parlist, Costlist, totdir,sta,cam)
  
  }
  
  # # # # # 
  # MCMC  #
  # # # # #
  
  if (doMCMC){
    
  if (!doStepWise){
    load( file = paste(totdir,"FitFinal.RData",sep=""))
    save( file = paste(totdir,"FitFinal_pSta.RData",sep=""))
  }

  
  sF    <- tryCatch (summary(FitFinal), error = function (e) {
    print ("Leaving this case because of following error")
    print(e)
    next
    })
  
  svar0 <- FitFinal$var_ms_unweighted
  covar <- sF$cov.scaled*2.4^2/5
  
  FitFinal$ssr
  
  MCMC<- modMCMC(f=OCOST_GEN,
                 p=FitFinal$par,
                 Vlist=unique(unlist(VLIST)),
                 Flist=unique(unlist(FLIST)),
                 Mlist=unique(unlist(MLIST)),
                 lower=parsmin[names(FitFinal$par)],
                 upper=parsmax[names(FitFinal$par)], 
                 niter=NitMC,
                 wvar0=.5,
                 updatecov = 2000,
                 jump=covar, 
                 var0=svar0  )
  
  
  save(list = 'MCMC', file = paste(totdir,"Fit","_MCMC.RData",sep=""))
  
  MC_ReportGen(userfile,MCMC, totdir,sta,cam)
  
  } 
}


