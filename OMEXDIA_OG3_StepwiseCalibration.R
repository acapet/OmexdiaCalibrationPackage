# This script is part of the OmexdiaCalibration suite.
# A. Capet, 2017, acapet@ulg.ac.be 
# changed by A.Eisele, 2017, annika.eisele@hzg.de

# The present script implement a stpewise calibration of user data, looping through stations, 
# or campaigns, following the stepwise approach defined in Capet et al . 2017

rm(list=ls()) #clears the environment


###############
# USER CORNER #
############### 

pseudoNrun<-300   

# Here follows the definition of the calibration steps
# Should be updated to allow more specifc steps redefinition


# To consider for the first calibration step
## Parameters 
PLISTC <- c("pFast","WPOC","mixL","w") #pRef,"rSlow","NCrref","NCrSdet"
# Observation profiles 
VLISTC <- c("DIC")#,"TN","TOC","por") #c("TOC","TN") "CN"
# Observation fluxes
FLISTC <- c("O2")

PLISTN <- c("pFast","WPOC","biot","mixL","AlphIrr","bwO2","w") #pRef,"rSlow","NCrref","NCrSdet"
# Observation profiles 
VLISTN <- c("NH3","NO3")#,"TN","TOC","por") #c("TOC","TN") "CN"
# Observation fluxes
FLISTN <- c("NH3","NO3","O2")


PLISTSIO <- c("rSi","SiCdet","EquilSiO")
VLISTSIO <- c("SIO") #"SiDet"
FLISTSIO <- c("SIO")
  
PLISTPO4 <- c("PCrSdet","rCaPprod")
VLISTPO4 <- c("PO4")
FLISTPO4 <- c("PO4")


# added by A.Eisele 24.10.2017
#generalized list for automatical calibration procedure on desired fitting steps
PLIST<-list(PLISTC,PLISTN,PLISTSIO,PLISTPO4)
VLIST<-list(VLISTC,VLISTN,VLISTSIO,VLISTPO4)
FLIST<-list(FLISTC,FLISTN,FLISTSIO,FLISTPO4)




#added by A.Eisele 24.10.2017
#loop iteration over different stations or campaigns

##################################
# Load model and observation data
#################################

#loop problematical for second loop iteration! needs to remove variables before second loop can be started!
#for (icamosta in c(1:length(datadfsta$Station))){


source('Load_Data.R') #load station data


icamosta<-1
  sta<-datadfsta$Station[icamosta]  
  cam<-datadfsta$Campaign[icamosta]
  
  print(sta)
  print(cam)
  
  
  source("OMEXDIA_OG3_Load.R") #load OMXEDIA model


  # We then create "localdata" dataframes, specific to one station.
  localdata   <- OBSatstaSSf(sta,cam)
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
  
  
  
  totdir=paste('FITNEW_',sta, '_', cam,sep="")    
  dir.create(totdir) #create new folder
  
  
  # Plot 0 : No Fit
  pdf(paste(totdir,"_Fit0.pdf",sep=""),width=5*(3+1)+2+5,height=15)
  grid.arrange(Simplot(parSta,TRUE)+ggtitle(paste(sta,"_",cam,"0. No Fit")),
               arrangeGrob(partableplot(parSta)),
               arrangeGrob(fluxtable(parSta)$p),
               ncol = 3,nrow=1, widths=c(5*3,7,5), heights = c(12))
  dev.off()

  
  #!!!Error in DIA$y[which(ModelDepths > localdata$UpperDepth[i] & ModelDepths <  : 
  #!!!                       subscript out of bounds
  
  # Defining the list of parameters that may be calibrated in one of the calibration steps
  parRange <- parRange[which(rownames(parRange) %in% unlist(PLIST)),] 
  parsvect <- as.numeric(as.matrix(parRange$guess)); names(parsvect) <- rownames(parRange); 
  parsmin  <- as.numeric(as.matrix(parRange$min)); names(parsmin) <- rownames(parRange); 
  parsmax  <- as.numeric(as.matrix(parRange$max)); names(parsmax) <- rownames(parRange); 
  
  
  #######################################
  # A.Eisele 23.10.2017
  # Inserted loop over Fitting procedure depending on desired Fitting steps
  #######################################

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
  
  remain<-c("PLIST", "VLIST", "FLIST","icamosta","pseudoNrun")
  rm(list=setdiff(ls(), remain))
  
#}