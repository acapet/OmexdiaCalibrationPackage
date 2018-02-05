################################################################################################################
#
# This script is part of the OmexdiaCalibration suite (https://github.com/acapet/OmexdiaCalibrationPackage) 
# This toolbox exploits essentially codes and methods developped by K. Soetaert (NIOZ)
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
# The present script implement a stepwise calibration of user data, looping through stations, 
# or campaigns, following the stepwise approach defined in Capet et al . (in prep.)
#
################################################
# Load model and observation data
################################################

# ! <Annika        : loop problematical for second loop iteration! needs to remove variables before second loop can be started!

# load OMXEDIA model
source("OmexCal_Load.R")  

# load station data
if (FALSE){
  userfile   <- 'UsersDefinitions_HAMMOND.R'
  stalist    <- c("H1") #,"H2","H4","H6","H7"
  camlist    <- "Sep89"
  pseudoNrun <- 5000
} else {
  userfile <- 'UsersDefinitions_NOAH.R'
  stalist  <- "C"
  camlist  <- c("HE432")
  pseudoNrun <- 2000
}

source(userfile)
source('OmexCal_Load_Data.R') 

# subsetting station list
dsStasub <- subset(dfStations,Station%in%stalist & Campaign %in% camlist)

#Looping
for (icamosta in (1:nrow(dsStasub))){
  #  icamosta <- 1
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
  
  # Update parameters with local value (+ update of the global porosity grid )
  parSta    <<- OmexCal_AdaptForSta(pars)
  
  # Create new folder
  totdir <- paste(plotdir,'FITNEW_',sta,'/',sep="")    
  dir.create(totdir) 
  
  # Defining the list of parameters that may be calibrated in one of the calibration steps
  parRange <- parRange[which(rownames(parRange) %in% unlist(PLIST)),] 
  parsmin  <- as.numeric(as.matrix(parRange$min)); names(parsmin) <- rownames(parRange); 
  parsmax  <- as.numeric(as.matrix(parRange$max)); names(parsmax) <- rownames(parRange); 
  
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
    #ifit<-1
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
    
    save(list = 'Fit', file = paste(totdir,"_Fit",ifit,".RData",sep=""))
    save(list = 'parSta', file = paste(totdir,"_Fit",ifit,"_pSta.RData",sep=""))
    print(parSta)
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
  pdf(paste(totdir,"_Sens.pdf",sep=""),width=10,height=10)
  pairs(Sens)
  dev.off()
  
  pdf(paste(totdir,"_Sens2.pdf",sep=""),width=10,height=10)
  plot(Sens)
  dev.off()
  
  sS<-summary(Sens)
  sS$param<-rownames(sS)
  sS$param <- factor(sS$param, levels = sS$param[order( sS$L1,decreasing = T)])
  
  g1<-ggplot(as.data.frame(sS),aes(x=param,y=L1))+geom_point()
  pdf(paste(totdir,"_Sens3.pdf",sep=""),width=10,height=10)
  print(g1)
  dev.off()
  
  ## <Arthur 2901018
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
                     control=list(numiter=pseudoNrun),
                     lower=parsmin[PLISTFinal],
                     upper=parsmax[PLISTFinal], 
                     method="Pseudo")
  
  
  FitFinal <- modFit(f=OCOST_GEN,
                     p=FitFinal$par,
                     Vlist=unique(unlist(VLIST)),
                     Flist=unique(unlist(FLIST)),
                     Mlist=unique(unlist(MLIST)),
                     control=list(numiter=pseudoNrun),
                     lower=parsmin[PLISTFinal],
                     upper=parsmax[PLISTFinal], 
                     method="Marq")
  
  
  

  parSta[c(names(FitFinal$par))]<-as.numeric(FitFinal$par)

  Fitlist[[length(PLIST)+1]]<-FitFinal
  Parlist[[length(PLIST)+2]]<-parSta
  
  save(list = 'Fit', file = paste(totdir,"_FitFinal.RData",sep=""))
  save(list = 'parSta', file = paste(totdir,"_FitFinal_pSta.RData",sep=""))
  
  Cost <- OCOST_GEN(parSta,
                  Vlist=unique(unlist(VLIST)),
                  Flist=unique(unlist(FLIST)),
                  Mlist=unique(unlist(MLIST)))$var

  Costlist[[length(PLIST)+2]] <- Cost
  
  save(list = 'Cost', file = paste(totdir,"_Fit","_Cost.RData",sep=""))
  ReportGen(userfile,Parlist, Costlist, totdir,paste0(sta,"_",cam))

}


