# This script is part of the OmexdiaCalibration suite.
# A. Capet, 2017, acapet@ulg.ac.be

# The present script implement a stpewise calibration of user data, looping through stations, 
# and following the stepwise approach defined in Capet et al . 2017


###############
# USER CORNER #
###############   
totdir = 'Fit1/'   
pseudoNrun<-2000  
StationLoadFile <- 'HammondLoad.R'

# Here follows the definition of the calibration steps
# Should be updated to allow more specifc steps redefinition


# To consider for the first calibration step
## Parameters 
PLIST1 <- c("pFast","WPOC","pRef","biot","NCrref","NCrSdet","mixL","rSlow") 
# Observation profiles 
VLIST1 <- c("TOC","TN")
# Observation fluxes
FLIST1 <- c("DIC")
  

PLIST2 <- c("NCrSdet","mixL","biot","AlphIrr")  
VLIST2 <- c("TOC","DIC","NH3","TN")
FLIST2 <- c("DIC","NH3","NO3","O2")
 
PLIST3 <- c("rSi","SiCdet","EquilSiO")
VLIST3 <- c("SIO","SiDet")
FLIST3 <- c("SIO")
  
# 
PLIST4 <- c("PCrSdet","rCaPprod")
VLIST4 <- "PO4"
FLIST4 <- "PO4"

###################

source("OMEXDIA_OG3_Load.R")
dir.create(totdir)

source(StationLoadFile)
datadfsta<-datadffl

for (ista in c(1,2)){
  #ista<-1
  
  sta<-datadfsta$Station[ista]
  print(sta)
  
  # We then create "localdata" dataframes, specific to one station.
  localdata   <- OBSatstaSSf(sta)
  localdata$variable<-as.character(localdata$variable)
  
  localdatafl <- subset(localdata, LowerDepth==0&UpperDepth==0)
  localdata   <- subset(localdata, !(LowerDepth==0&UpperDepth==0))

  
  # Setting the Non-local irrigation framework
  parsdf["AlphIrr","guess"]<-10/365
  parsdf["IrrEnh","guess"]<-1 
  
  
  # The following wil have to be considered to load station specific- environmental info
  # into global variables, ie. Porosity parameters, bottom water conditions, sedimentation rate, etc ...
  # adapted <-AdaptParsForStation_SS(pars,station)
  # parSta <-adapted$parslpars
  parSta <-pars
  
  
  
  parRange<-parsdf[,c("guess","min","max","unit","printfactor","printunit")]
  pars<-as.numeric(parRange[,"guess"])
  names(pars)<-rownames(parRange)
  
  
  
  # Plot 0 : No Fit
  pdf(paste(totdir,sta,"_Fit0.pdf",sep=""),width=5*(3+1)+2+5,height=15)
  grid.arrange(Simplot(parSta,TRUE)+ggtitle(paste(sta,"0. No Fit")),
               arrangeGrob(partableplot(parSta)),
               arrangeGrob(fluxtable(parSta)$p),
               ncol = 3,nrow=1, widths=c(5*3,7,5), heights = c(12))
  dev.off()
  
  
  # Defining the list of parameters that may be calibrated in one of the calibration steps
  parRange <- parRange[which(rownames(parRange) %in% c(PLIST1,PLIST2,PLIST3,PLIST4)),] 
  parsvect <- as.numeric(as.matrix(parRange$guess)); names(parsvect) <- rownames(parRange); 
  parsmin  <- as.numeric(as.matrix(parRange$min)); names(parsmin) <- rownames(parRange); 
  parsmax  <- as.numeric(as.matrix(parRange$max)); names(parsmax) <- rownames(parRange); 
  
  #######################################
  # First Calibration :  Only TOC       #
  #######################################
  
  Fit1 <- modFit(f=OCOST_GEN,
                 p=parSta[PLIST1],
                 Vlist=VLIST1,
                 Flist=FLIST1,
                 control=list(numiter=pseudoNrun),
                 lower=parsmin[PLIST1],
                 upper=parsmax[PLIST1], method="Pseudo")
  print("Fit 1 done")
    
  Fit1$ssr
  
  #Updating the value of parsvect and parSta with calibrated values 
  Fit<-Fit1
  parsvect[c(names(Fit$par))]<-as.numeric(Fit$par)
  parSta[c(names(Fit$par))]<-as.numeric(Fit$par)
  
  pdf(paste(totdir,sta,"_Fit1.pdf",sep=""),width=5*(3+1)+2,height=15)
  grid.arrange(Simplot(parSta,TRUE)+ggtitle(paste(sta,"1. Pseudo")),
               arrangeGrob(partableplot(parSta)),
               arrangeGrob(fluxtable(parSta)$p,
                           fittableplot(Fit),ncol=1,heights=c(6,4)),
               ncol = 3,nrow=1, widths=c(5*3,7,3), heights = c(12))
  dev.off()
  
  save(list = 'Fit', file = paste(totdir,sta,"_Fit1.RData",sep=""))
  save(list = 'parSta', file = paste(totdir,sta,"_Fit1_pSta.RData",sep=""))
  print(parSta)
  
  ###########################################
  # Second  Fit :  Adding nitrogen species  #
  ###########################################
  
  Fit2 <- modFit(f=OCOST_GEN,
                  p=parSta[PLIST2],
                  Vlist=VLIST2,
                  Flist=FLIST2,
                  control=list(numiter=pseudoNrun),
                  lower=parsmin[PLIST2],
                  upper=parsmax[PLIST2], method="Pseudo")
  
  Simplot(Fit2$par)
  print("Fit 2 done")
  Fit2$ssr
  
  Fit<-Fit2
  
  parsvect[c(names(Fit$par))]<-as.numeric(Fit$par)
  parSta[c(names(Fit$par))]<-as.numeric(Fit$par)
  
  pdf(paste(totdir,sta,"_Fit2.pdf",sep=""),width=5*(3+1)+2,height=15)
  grid.arrange(Simplot(parSta, TRUE)+ggtitle(paste(sta,"2. Pseudo")),
               arrangeGrob(partableplot(parSta)),
               arrangeGrob(fluxtable(parSta)$p,
                           fittableplot(Fit),ncol=1,heights=c(8,2)),
               ncol = 3,nrow=1, widths=c(5*3,7,3), heights = c(12))
  dev.off()
  save(list = 'Fit', file = paste(totdir,sta,"_Fit2.RData",sep=""))
  save(list = 'parSta', file = paste(totdir,sta,"_Fit2_pSta.RData",sep=""))
  print(parSta)
  
  ####################
  # Fit3 : Sio Only
  ####################

  Fit3 <- modFit(f=OCOST_GEN,
                  p=parSta[PLIST3],
                  Vlist=VLIST3,
                  Flist=FLIST3,
                  control=list(numiter=pseudoNrun),
                  lower=parsmin[PLIST3],
                  upper=parsmax[PLIST3], method="Pseudo")
  
  print("Fit 3 done")
  Fit3S$ssr
  
  Fit<-Fit3
  parsvect[c(names(Fit$par))]<-as.numeric(Fit$par)
  parSta[c(names(Fit$par))]<-as.numeric(Fit$par)
  
  pdf(paste(totdir,sta,"_Fit3S.pdf",sep=""),width=5*(3+1)+2,height=15)
  grid.arrange(Simplot(parSta,TRUE)+
                 ggtitle(paste(sta,"3")),
               arrangeGrob(partableplot(parSta)),
               arrangeGrob(fluxtable(parSta)$p,
                           fittableplot(Fit),ncol=1,heights=c(8,2)),
               ncol = 3,nrow=1, widths=c(5*3,7,3), heights = c(12))
  dev.off()
  save(list = 'Fit', file = paste(totdir,sta,"_Fit3.RData",sep=""))
  save(list = 'parSta', file = paste(totdir,sta,"_Fit3_pSta.RData",sep=""))
  print(parSta)
  
 ####################
 # Fit4 : Pho Only
 ####################
   
  Fit4 <- modFit(f=OCOST_GEN,
                  p=parSta[PLIST4],
                  Vlist=VLIST4,
                  Flist=FLIST4,
                  control=list(numiter=pseudoNrun),
                  lower=parsmin[PLIST4],
                  upper=parsmax[PLIST4], method="Pseudo")

  print("Fit 4 done")
  Fit4$ssr

  Fit<-Fit4
  parsvect[c(names(Fit$par))]<-as.numeric(Fit$par)
  parSta[c(names(Fit$par))]<-as.numeric(Fit$par)

  pdf(paste(totdir,sta,"_Fit4.pdf",sep=""),width=5*(3+1)+2,height=15)
  grid.arrange(Simplot(parSta,TRUE)+ggtitle(paste(sta,"4")),
               arrangeGrob(partableplot(parSta)),
               arrangeGrob(fluxtable(parSta)$p,
                           fittableplot(Fit),ncol=1,heights=c(8,2)),
               ncol = 3,nrow=1, widths=c(5*3,7,3), heights = c(12))
  dev.off()
  save(list = 'Fit', file = paste(totdir,sta,"_Fit4.RData",sep=""))
  save(list = 'parSta', file = paste(totdir,sta,"_Fit4_pSta.RData",sep=""))
  print(parSta)
  
  ##########################
  ## Collinearity + Refit ##
  ##########################
  
  # Assessing parameters sensitivity
  Sens <- sensFun(func=OCOST_GEN,
                  parms=parsvect,
                  Vlist=unique(c(VLIST1,VLIST2,VLIST3,VLIST4)),
                  Flist=unique(c(FLIST1,FLIST2,FLIST3,FLIST4))
                  )
  
  
  # Some Sensitivity Plots
  pairs(Sens)
  plot(Sens)
  
  sS<-summary(Sens)
  sS$param<-rownames(sS)
  sS$param <- factor(sS$param, levels = sS$param[order( sS$L1,decreasing = T)])
  ggplot(as.data.frame(sS),aes(x=param,y=L1))+geom_point()
  
  pdf(paste(totdir,sta,"_Sens.pdf",sep=""),width=10,height=10)
  pairs(Sens)
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
                  Vlist=unique(c(VLIST1,VLIST2,VLIST3,VLIST4)),
                  Flist=unique(c(FLIST1,FLIST2,FLIST3,FLIST4)),
                  control=list(numiter=pseudoNrun),
                  lower=parsmin[PLISTFinal],
                  upper=parsmax[PLISTFinal], method="Pseudo")
  
  
  summary(FitFinal)
  
  Simplot(FitFinal$par,TRUE)
  
  Fit<-FitFinal
  parsvect[c(names(Fit$par))]<-as.numeric(Fit$par)
  parSta[c(names(Fit$par))]<-as.numeric(Fit$par)
  
  pdf(paste(totdir,sta,"_FitFinal.pdf",sep=""),width=5*(3+1)+2,height=15)
  grid.arrange(Simplot(parSta,TRUE)+ggtitle(paste(sta,"4")),
               arrangeGrob(partableplot(parSta)),
               arrangeGrob(fluxtable(parSta)$p,
                           fittableplot(Fit),
                           ncol=1,heights=c(6,4)),
               ncol = 3,nrow=1, widths=c(5*3,7,3), heights = c(15))
  dev.off()
  save(list = 'Fit', file = paste(totdir,sta,"_FitFinal.RData",sep=""))
  save(list = 'parSta', file = paste(totdir,sta,"_FitFinal_pSta.RData",sep=""))
  
  Cost<-OCOST_GEN(parSta,
                      Vlist=unique(c(VLIST1,VLIST2,VLIST3,VLIST4)),
                      Flist=unique(c(FLIST1,FLIST2,FLIST3,FLIST4))
                      )
  
  save(list = 'Cost', file = paste(totdir,sta,"_Fit4_Cost.RData",sep=""))

  
}
