################
#
# This script is part of the OmexdiaCalibration suite (https://github.com/acapet/OmexdiaCalibrationPackage) 
# This toolbox exploits essentially codes and methods developped by K. Soetaert (NIOZ)
#
# Arthur Capet (acapet@ulg.ac.be), Oct 2017.
#
################
# Contributors : 
# A. Capet , acapet@ulg.ac.be 
################
#
# Description :
# This functions provides a table with observed and simulated fluxes and a summary of integrated diagnostics
#
################

fluxtable<-function(p1) {
  
  treatp<-function (p){
    if ("deSolve" %in% class(p))
    {DI<-p}
    else
    {DI<-OCALL(p) }
    
    if (is.na(p["NCrFdet"])){
      p["NCrFdet"]<-parSta["NCrFdet"]
    }
    if (is.na(p["pdepo"])){
      p["pdepo"]<-parSta["pdepo"]
    }
    DI$Norgflux      <- as.numeric(DI$Corgflux*p["pFast"]*(1-p["pRef"])*p["NCrFdet"] +
                                     DI$Corgflux*(1-p["pFast"])*(1-p["pRef"])*p["NCrSdet"])
    
    Cb<-as.data.frame(Cbudget(DI,p))
    Cb$"Burial[%]" <- -Cb$burial/Cb$surfOrgflux*100
    Cb$"Irr[%]"    <- Cb$surfIrrflux/(Cb$surfIrrflux+Cb$surfDiffflux)*100
    
    Nb<-as.data.frame(Nbudget(DI))
    Nb$"Burial[%]"   <- -Nb$burial/Nb$surfOrgflux*100
    Nb$"Loss[%]"     <- Nb$loss/Nb$surfOrgflux*100
    Nb$"Irr[%]"      <- Nb$surfIrrflux/(Nb$surfIrrflux+Nb$surfDiffflux)*100
    Nb$"DenitEff[%]" <-  Nb$loss/( abs(Nb$surfIrrflux)+abs(Nb$surfDiffflux)+ abs(Nb$loss))*100
    
    Ob<-as.data.frame(Obudget(DI))
    Ob$"Irr[%]"    <- Ob$surfIrrflux/(Ob$surfIrrflux+Ob$surfDiffflux)*100
    Ob$"Oxic[%]"   <- Ob$OlossOxicmin/Ob$loss*100
    Ob$"Nitri[%]"  <- Ob$OlossNitri/Ob$loss*100
    Ob$"OduOx[%]"  <- Ob$OlossOduOx/Ob$loss*100
    Ob$"BFE"       <- (Ob$surfIrrflux+Ob$surfDiffflux)/(Ob$surfDiffflux)
    ODUb<-as.data.frame(ODUbudget(DI,p))
    
    #   Sib<-as.data.frame(Sibudget(DI))
    #   Ob$"Irr[%]"    <- Ob$surfIrrflux/(Ob$surfIrrflux+Ob$surfDiffflux)*100
    #   Ob$"Oxic[%]"   <- Ob$OlossOxicmin/Ob$loss*100
    #   Ob$"Nitri[%]"  <- Ob$OlossNitri/Ob$loss*100
    #   Ob$"OduOx[%]"  <- Ob$OlossOduOx/Ob$loss*100
    #   Ob$"BFE"       <- (Ob$surfIrrflux+Ob$surfDiffflux)/(Ob$surfDiffflux)
    #   ODUb<-as.data.frame(ODUbudget(DI,p))
    
    return ( data.frame(DICflux       = - (DI$DICflux+IntegratedRate(DI$DICIrrflux))/1e6*1e4,                            # from nmol/cm2/d -> mmol/m2/d
                        O2flux        = - (DI$O2flux +IntegratedRate(DI$O2Irrflux ))/1e6*1e4,                            # from nmol/cm2/d -> mmol/m2/d
                        NO3flux       = - (DI$NO3flux+IntegratedRate(DI$NO3Irrflux))/1e6*1e4,                            # from nmol/cm2/d -> mmol/m2/d
                        NH3flux       = - (DI$NH3flux+IntegratedRate(DI$NH3Irrflux))/1e6*1e4,                            # from nmol/cm2/d -> mmol/m2/d
                        ODUflux       = - (DI$ODUflux+IntegratedRate(DI$ODUIrrflux))/1e6*1e4,                            # from nmol/cm2/d -> mmol/m2/d
                        SIOflux       = - (DI$SIOflux+IntegratedRate(DI$SIOIrrflux))/1e6*1e4,  
                        PO4flux       = - (DI$PO4flux+IntegratedRate(DI$PO4Irrflux))/1e6*1e4,  
                        # Pirr_DIC      =IntegratedRate(DI$DICIrrflux)/(DI$DICflux+IntegratedRate(DI$DICIrrflux))*100, # Contribution of irrigation to DIC fluxes
                        # Pirr_O2       =IntegratedRate(DI$O2Irrflux)/(DI$O2flux+IntegratedRate(DI$O2Irrflux))*100, # Contribution of irrigation to O2 fluxes
                        # Pirr_NO3      =IntegratedRate(DI$NO3Irrflux)/(DI$NO3flux+IntegratedRate(DI$NO3Irrflux))*100, # Contribution of irrigation to NO3 fluxes
                        # Pirr_NH3      =IntegratedRate(DI$NH3Irrflux)/(DI$NH3flux+IntegratedRate(DI$NH3Irrflux))*100, # Contribution of irrigation to NH3 fluxes
                        # Pirr_ODU      =IntegratedRate(DI$ODUIrrflux)/(DI$ODUflux+IntegratedRate(DI$ODUIrrflux))*100, # Contribution of irrigation to ODU fluxes
                        # logdeltC      =Cb$logdelta,
                        # logdeltN      =Nb$logdelta,
                        # logdeltO      =Ob$logdelta,
                        # logdeltODU    =ODUb$logdelta,
                        # P_Cburial     =(DI$Corgdeepflux  + DI$DICdeepflux)/DI$Corgflux*100,                         # percentage of buried carbon
                        # Nitrif         = Nb$Nitrif/100,
                        # P_renewal1     =-(IntegratedRate(DI$y[,"PO4"])+IntegratedRateSolid(DI$y[,"FeP"])+IntegratedRateSolid(DI$y[,"CaP"]))/((DI$PO4flux+IntegratedRate(DI$PO4Irrflux))/1e6*1e4),
                        # P_renewal2     =-(IntegratedRateSolid(DI$y[,"FeP"])+IntegratedRateSolid(DI$y[,"CaP"]))/((DI$PO4flux+IntegratedRate(DI$PO4Irrflux))/1e6*1e4),
                        # P_renewal3     =-(IntegratedRateSolid(DI$y[,"FeP"]))/((DI$PO4flux+IntegratedRate(DI$PO4Irrflux))/1e6*1e4),
                        # FEPstock10     = IntegratedRateSolid(DI$y[,"FeP"],10),
                        # FEPstockR      = IntegratedRateSolid(DI$y[,"FeP"],10)/(IntegratedRateSolid(DI$y[,"FeP"],10) +  IntegratedRate(DI$y[,"PO4"],10))*100,
                        # PO4stock10     = IntegratedRate(DI$y[,"PO4"],10),
                        # CaPstock10     = IntegratedRateSolid(DI$y[,"CaP"],10),
                        # totPStock10    = IntegratedRateSolid(DI$y[,"FeP"],10) +  IntegratedRate(DI$y[,"PO4"],10) ,
                        P_Nloss        = Nb$"Loss[%]",
                        P_DenitEff     = Nb$"DenitEff[%]",
                        P_Oxic         = IntegratedRate(DI$OxicMin)/IntegratedRate(DI$Cprod)*100,
                        P_Denitr       = IntegratedRate(DI$Denitrific)/IntegratedRate(DI$Cprod)*100,
                        P_Anoxic       = IntegratedRate(DI$AnoxicMin)/IntegratedRate(DI$Cprod)*100)
    ) 
  }
  
  pout1<-treatp(p1)
  d<-melt(pout1)
  
  dforp<-subset(d,select=value)
  rownames(dforp)<-d$variable
  
  p1<-qplot(1:10, 1:10, geom = "blank")+theme_bw()+
    theme(line = element_blank(),text = element_blank())+
    annotation_custom(grob = tableGrob(format(dforp,digits = 3,nsmall=0,scientific=F,drop0trailing=T))) 
  
  if (exists("localdatafl")){
    
    localdatafl$variable<-paste0(substr(localdatafl$variable,2,50),"flux")
    
    dforp[as.character(localdatafl$variable),"Meas."]<-localdatafl$value
    fpdf<-format(dforp,digits = 3,nsmall=0,scientific=F,drop0trailing=T)
    fpdf[ grep("NA",fpdf[,2]),2]<-""
    
    p1<-qplot(1:10, 1:10, geom = "blank")+theme_bw()+
      theme(line = element_blank(),text = element_blank())+
      annotation_custom(grob = tableGrob(fpdf)) 
  }
  
  return(list(p=p1,d=d))
}