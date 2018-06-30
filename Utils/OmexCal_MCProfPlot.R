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



LOCALF <- function(p){
  d<-fluxtable(p)$d
  d[which(d$variable == "O2flux"),2]<-d[which(d$variable == "O2flux"),2]-d[which(d$variable == "ODUflux"),2]
  d<-subset(d,variable %in% c("NO3flux","NH3flux","P_Nloss","SIOflux","O2flux","PO4flux","SIOflux","DICflux"))
  
  d<-dcast(d,...~variable)
  return(  d[2:length(d)]  )
}

LOCALF2 <- function(p){
  DIA<-OCALL (p)
  return(  cbind(data.frame(Depth=Grid$x.mid), DIA$y)  )
}


MCProfPlot<-function(MCMC, plotdata=FALSE, YL=20, NumRun = 2000) {

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

    sRT<-sensRange(LOCALF2,parInput = MCMCr$pars,map=1,num=nrow(MCMCr$pars)) 
    SDNL     <- summary(sRT)
    
    ddd<-OCALL(parSta)
    svlist<-llply(colnames(ddd$y),function(n){rep(n,length(Grid$x.mid))} )
    SDNL$variable  <- do.call(c,svlist)
    
    p1<-ggplot(SDNL, aes(x=x))+
      geom_ribbon(aes(ymin=Min, ymax=Max), alpha=.2)+
      geom_ribbon(aes(ymin=q05, ymax=q95), alpha=.2)+
      geom_ribbon(aes(ymin=q25, ymax=q75), alpha=.2)+
      geom_path(aes(y=Mean))+
      facet_wrap(~variable,scales="free", ncol=3)+
      theme_bw()+
      scale_x_reverse()+
      coord_flip(xlim=c(0,YL))+
      xlab(label="Sed. depth - [cm]")+
      xlab(label="")
    
    if (plotdata) {
      localdata <- subset(localdata,variable %in% colnames(ddd$y) & MidDepth>0 & !is.na(localdata$value+ localdata$err))  
      p1 <- p1 +
        geom_errorbarh(data=localdata,aes(y=value,x=(LowerDepth+UpperDepth)/2,xmin=LowerDepth,xmax=UpperDepth),color='black')+
        # geom_errorbar(data=b,aes(x=modval,y=(LowerDepth+UpperDepth)/2,ymin=LowerDepth,ymax=UpperDepth),color='red',size=2)+
        geom_errorbar(data=localdata,aes(y=value,x=(LowerDepth+UpperDepth)/2,ymin=value-err,ymax=value+err),color='black')
    }
    
    # Second plot is for smaller depth variables
    p2<-ggplot(subset(SDNL,variable %in% c('O2','NOx')), aes(x=x))+
      geom_ribbon(aes(ymin=Min, ymax=Max), alpha=.2)+
      geom_ribbon(aes(ymin=q05, ymax=q95), alpha=.2)+
      geom_ribbon(aes(ymin=q25, ymax=q75), alpha=.2)+
      geom_path(aes(y=Mean))+
      facet_wrap(~variable,scales="free", ncol=3)+
      theme_bw()+
      scale_x_reverse()+
      coord_flip(xlim=c(0,3))+
      xlab(label="Sed. depth - [cm]")+
      ylab(label="")
    
    if (plotdata) {
    localdata <- subset(localdatamicro,variable %in% c("O2") & Depth>0 & !is.na(localdatamicro$value+ localdatamicro$err))  
    if (nrow(localdata)>1){
    p2 <- p2 +
      geom_errorbar(data=localdata,aes(y=value,x=(Depth)/2,ymin=value-err,ymax=value+err),color='black')+
      geom_point(data=localdata,aes(y=value,x=(Depth)/2),color='black')
    }
    }
    
    
    
    return(list(pProfs=p1, pMicro=p2))
}
    