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
# A. Eisele, annika.eisele@hzg.de
################
#
# Description :
# This function receives a vector of parameter and returns the corresponding Steady-State Omexdia model solution
#
################


OCALL23 <- function (mycase,p) {
  caseloc              <- mycase
  parpert              <- mycase$defpars
  parpert[c(names(p))] <- as.numeric(p)
  
  ## Update of global variables
  caseloc$DbGrid       <- setup.prop.1D(func = exp.profile, x.0 = parpert["mixL"],
                                         y.0 = parpert["biot"], y.inf = 0, x.att = 1, 
                                         grid = mycase$grid)
  
  caseloc$AlphIrrGrid  <- setup.prop.1D(func = exp.profile, x.0 = parpert["mixL"],
                                        y.0 = parpert["AlphIrr"], y.inf = 0, x.att = 1, 
                                        grid = mycase$grid)
  
  caseloc$IrrEnhGrid  <- setup.prop.1D(func = exp.profile  , x.0 = parpert["mixL"],
                                        y.0 = parpert["IrrEnh"], y.inf = 1, x.att = 1, 
                                        grid = mycase$grid)
  
  Grid <- mycase$grid
  Flux            <- parpert["MeanFlux"]
  
  initpar <- c(
    parpert[c("Temp","w","MeanFlux","rFast","rSlow","pFast","pRef", "NCrFdet","NCrSdet",
              "rSi","SiCdet","EquilSiO","PCrFdet","PCrSdet","rFePdesorp","rFePadsorp","rCaPprod","rCaPdiss","CPrCaP",
              "PO4ads","Q","pdepo","NH3Ads","rnit","ksO2nitri","rODUox","ksO2oduox","ksO2oxic","ksNOxdenit",
              "kinO2denit","kinNOxanox","kinO2anox","bwO2","bwNH3","bwNOx","bwODU","bwDIC","bwSIO","bwPO4",
              "DispO2","DispNOx","DispNH3","DispODU","DispDIC","DispSIO","DispPO4")],
    Grid$dx,
    Grid$dx.aux,
    porGrid$mid,
    porGrid$int,
    DbGrid$int,
    AlphIrrGrid$mid,
    IrrEnhGrid$int)
  
  IC   <- rep(10,nspec*N)
  
  nout <- 2221 # express this using N 
  
  outnames <- c("O2flux" , rep("O2Irrflux",N) , "O2deepflux" ,
                "NOxflux", rep("NOxIrrflux",N), "NOxdeepflux",
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

  DIA <- AddDiagnostics(DIA,parpert)  
  
  return (DIA)
}
