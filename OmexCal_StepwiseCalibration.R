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

# ! <Annika : loop problematical for second loop iteration! needs to remove variables before second loop can be started!


# load OMXEDIA model
source("OmexCal_Load.R")  

# load station data
if (TRUE){
  source('UsersDefinitions_HAMMOND.R')
  stalist<-c("H1","H2")
  camlist<-"Sep89"
} else{
  source('UsersDefinitions_NOAH.R')
  stalist<-"C"
  camlist<-"HE432"
}


source('OmexCal_Load_Data.R') 

#Looping
dsStasub <- subset(dfStations,Station%in%stalist & Campaign %in% camlist)

for (icamosta in (1:nrow(dsStasub))){
#  icamosta <- 1
  sta<-dsStasub$Station[icamosta]  
  cam<-dsStasub$Campaign[icamosta]
  
  print(sta)
  # We then create "local" dataframes, specific to one station/campaign.
  localdata    <- subset(dfProfiles, Station==sta & Campaign == cam)
  localdatafl  <- subset(dfFluxes,   Station==sta & Campaign == cam)
  localdatasta <- subset(dfStations, Station==sta & Campaign == cam)
  localdatamicro <- subset(dfO2micro, Station==sta & Campaign == cam)
  
  
  # Setting the Non-local irrigation framework
  parsdf["AlphIrr","guess"]<-10/365
  parsdf["IrrEnh","guess"]<-1 

  # Load parameters
  parRange    <- parsdf[,c("guess","min","max","unit","printfactor","printunit")]

  # Update parameters with local value (+ background update of the global porosity grid )
  parSta    <- OmexCal_AdaptForSta(pars)
  
  Simplot(parSta)
  
  totdir=paste(plotdir,'FITNEW_',sta,sep="")    
  dir.create(totdir) #create new folder
  
  # Plot 0 : No Fit
  pdf(paste(totdir,"_Fit0.pdf",sep=""),width=5*(3+1)+2+5,height=15)
  grid.arrange(Simplot(parSta,TRUE)+ggtitle(paste(sta,"_",cam,"0. No Fit")),
               arrangeGrob(partableplot(parSta)),
               arrangeGrob(fluxtable(parSta)$p),
               ncol = 3,nrow=1, widths=c(5*3,7,5), heights = c(12))
  dev.off()

  # Defining the list of parameters that may be calibrated in one of the calibration steps
  parRange <- parRange[which(rownames(parRange) %in% unlist(PLIST)),] 
  parsvect <- as.numeric(as.matrix(parRange$guess)); names(parsvect) <- rownames(parRange); 
  parsmin  <- as.numeric(as.matrix(parRange$min)); names(parsmin) <- rownames(parRange); 
  parsmax  <- as.numeric(as.matrix(parRange$max)); names(parsmax) <- rownames(parRange); 
  
  pseudoNrun<-300   
  
  for (ifit in c(1:length(PLIST))){
    
    Fit <- modFit(f=OCOST_GEN,
                  p=parSta[PLIST[[ifit]]],
                   Vlist=VLIST[[ifit]],
                   Flist=FLIST[[ifit]],
                   control=list(numiter=pseudoNrun),
                   lower=parsmin[PLIST[[ifit]]],
                   upper=parsmax[PLIST[[ifit]]], 
                  method="Pseudo")
    
    Simplot(Fit$par)
    paste("Fit", ifit, "done")
    
    Fit$ssr
    
    #Updating the value of parsvect and parSta with calibrated values 
    parsvect[c(names(Fit$par))]<-as.numeric(Fit$par)
    parSta[c(names(Fit$par))]<-as.numeric(Fit$par)
    
    pdf(paste(totdir,"_Fit",ifit,".pdf",sep=""),width=5*(3+1)+2,height=15)
    grid.arrange(Simplot(parSta,TRUE)+ggtitle(paste(sta,"_",cam,"1. Pseudo")),
                 arrangeGrob(partableplot(parSta)),
                 arrangeGrob(fluxtable(parSta)$p,
                             fittableplot(Fit),ncol=1,heights=c(6,4)),
                 ncol = 3,nrow=1, widths=c(5*3,7,3), heights = c(12))
    dev.off()
    
    save(list = 'Fit', file = paste(totdir,"_Fit",ifit,".RData",sep=""))
    save(list = 'parSta', file = paste(totdir,"_Fit",ifit,"_pSta.RData",sep=""))
    print(parSta)
  }
    
  ##########################
  ## Collinearity + Refit ##
  ##########################
  
  # Assessing parameters sensitivity
  Sens <- sensFun(func=OCOST_GEN,
                  parms=parsvect,
                  Vlist=unique(unlist(VLIST)),
                  Flist=unique(unlist(FLIST))
                  )
  
  
  # Some Sensitivity Plots
  pdf(paste(totdir,"_Sens.pdf",sep=""),width=10,height=10)
  pairs(Sens)
  dev.off()
  
  pdf(paste(totdir,"_Sens2.pdf",sep=""),width=10,height=10)
  plot(Sens)
  dev.off()
  
  pdf(paste(totdir,"_Sens3.pdf",sep=""),width=10,height=10)
  sS<-summary(Sens)
  sS$param<-rownames(sS)
  sS$param <- factor(sS$param, levels = sS$param[order( sS$L1,decreasing = T)])
  ggplot(as.data.frame(sS),aes(x=param,y=L1))+geom_point()
  dev.off()
  
  
  # Assessing parameters collinearity, based on the sensitivity analysis
  cc<-collin(Sens)
  
  plot(cc)
  abline(h = 20, col = "red")
  
  # We want to select the parameter subset that 
  #   1) has a collinearity index below 20
  #   2) contains at least mixL and AlphIrr
  #   3) contains the most parameter
  
  c2<-cc[cc[,"collinearity"] < 20 &
           cc[,"mixL"]==1  &
           cc[,"AlphIrr"]==1,]
  
  c2<-c2[which(c2$N==max(c2$N)),]
  
  cbest<-c2[which.min(c2$collinearity),]
  cbest
  
  PLISTFinal<-names(parsvect)[cbest==1]
  
  FitFinal <- modFit(f=OCOST_GEN,
                  p=parsvect[PLISTFinal],
                  Vlist=unique(unlist(VLIST)),
                  Flist=unique(unlist(FLIST)),
                  control=list(numiter=pseudoNrun),
                  lower=parsmin[PLISTFinal],
                  upper=parsmax[PLISTFinal], 
                  method="Pseudo")
  
  
  summary(FitFinal)
  
  Simplot(FitFinal$par,TRUE)
  
  Fit<-FitFinal
  parsvect[c(names(Fit$par))]<-as.numeric(Fit$par)
  parSta[c(names(Fit$par))]<-as.numeric(Fit$par)
  
  pdf(paste(totdir,"_FitFinal.pdf",sep=""),width=5*(3+1)+2,height=15)
  grid.arrange(Simplot(parSta,TRUE)+ggtitle(paste(sta,"_",cam,"Final")),
               arrangeGrob(partableplot(parSta)),
               arrangeGrob(fluxtable(parSta)$p,
                           fittableplot(Fit),
                           ncol=1,heights=c(6,4)),
               ncol = 3,nrow=1, widths=c(5*3,7,3), heights = c(15))
  dev.off()
  save(list = 'Fit', file = paste(totdir,"_FitFinal.RData",sep=""))
  save(list = 'parSta', file = paste(totdir,"_FitFinal_pSta.RData",sep=""))
  
  Cost<-OCOST_GEN(parSta,
                      Vlist=unique(unlist(VLIST)),
                      Flist=unique(unlist(FLIST))
                      )
  
  save(list = 'Cost', file = paste(totdir,"_Fit","_Cost.RData",sep=""))
  
  remain<-c("PLIST", "VLIST", "FLIST","icamosta")
  rm(list=setdiff(ls(), remain))
  
}