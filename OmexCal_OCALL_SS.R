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
# A. Eisele, annika.eisele@hzg.de
################
#
# Description :
# This functions receives a vector of parameter and returns the correpsonding Steady-State Omexdia model solution
#
################


OCALL <- function (p) {
  parpert<-parSta
  parpert[c(names(p))]<-as.numeric(p)
  
  ## Update of global variables
  DbGrid               <<- setup.prop.1D(func = exp.profile, x.0 = parpert["mixL"],
                                         y.0 = parpert["biot"], y.inf = 0, x.att = 1, 
                                         grid = Grid)
  
  AlphIrrGrid         <<- setup.prop.1D(func = exp.profile, x.0 = parpert["mixL"],
                                        y.0 = parpert["AlphIrr"], y.inf = 0, x.att = 1, 
                                        grid = Grid)
  #plot(AlphIrrGrid$mid)
  IrrEnhGrid          <<- setup.prop.1D(func = exp.profile  , x.0 = parpert["mixL"],
                               y.0 = parpert["IrrEnh"], y.inf = 1, x.att = 1, 
                               grid = Grid)
  #plot(IrrEnhGrid$mid)
  
  Flux            <- parpert["WPOC"]*100
  
  initpar <- c(parpert[-which(names(parpert) %in% c("WPOC","biot","AlphIrr","IrrEnh","por","porinf","pora","Sal","mixL","NCrref"))],
               Grid$dx, Grid$dx.aux, porGrid$mid, porGrid$int,DbGrid$int, AlphIrrGrid$mid,IrrEnhGrid$int)
  IC   <- rep(10,nspec*N)
  nout <- 2221 # express this using N 
  outnames <- c("O2flux" , rep("O2Irrflux",N) , "O2deepflux" ,
                "NO3flux", rep("NO3Irrflux",N), "NO3deepflux",
                "NH3flux", rep("NH3Irrflux",N), "NH3deepflux",
                "ODUflux", rep("ODUIrrflux",N), "ODUdeepflux",
                "DICflux", rep("DICIrrflux",N), "DICdeepflux",
                "SIOflux", rep("SIOIrrflux",N), "SIOdeepflux",
                "PO4flux", rep("PO4Irrflux",N), "PO4deepflux",
                "FePdeepflux", "CaPdeepflux", 
                "Corgflux","Corgdeepflux","Norgdeepflux","Porgdeepflux","SiDetdeepflux",
                rep("Denitrific",N),rep("OxicMin",N),rep("AnoxicMin",N),
                rep("Nitri",N),rep("OduOx",N), rep("Cprod",N),rep("Nprod",N),rep("Pprod",N),
                rep("NH3adsorption",N),rep("PO4adsorption",N),rep("TOC",N),
                rep("FePadsorp",N),rep("FePdesorp",N),rep("CaPprod",N),rep("CaPdiss",N)
  )
  ynames <- svarnames

    DIA  <- steady.1D(y=as.double(IC),
                    func="oomexdia_mod",
                    initfunc="initomexdia",
                    names = ynames,
                    initforc = "initforc",
                    forcings = mean(Flux),  
                    initpar=initpar,
                    nspec=nspec,
                    dllname="omexdia_OG3", 
                    nout=nout,
                    outnames = outnames,
                    positive=TRUE)
  
  DIA$y[,"SiDet"]<-DIA$y[,"SiDet"]*28*100*1e-9/2.5
  
  DIA$y<-cbind(DIA$y,TOC=DIA$TOC)
  DIA$y<-cbind(DIA$y,TN=(DIA$y[,"FDET"]*initpar["NCrFdet"]+
                           DIA$y[,"SDET"]*initpar["NCrSdet"]+
                           Flux*initpar["pRef"]/initpar["w"]/(1-porGrid$int[N+1])*parpert["NCrref"])*14*100*1e-9/2.5)
  
  
  
  return (DIA)
}
