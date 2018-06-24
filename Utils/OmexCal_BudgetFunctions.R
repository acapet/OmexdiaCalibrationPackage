################
#
# This script is part of the OmexdiaCalibration suite (https://github.com/MAST-ULiege/OmexdiaCalibrationPackage) 
# This toolbox exploits essentially codes and methods developped by K. Soetaert (NIOZ)
#
# Arthur Capet (acapet@uliege.be), Oct 2017.
#
################

IntegratedRate <- function(rate, depth = NULL)  {      # integrated rate for liquids
  # print(paste0(" IntegratedRate porGrid$mid(end) :  " ,toString(porGrid$mid[length(porGrid$mid)])))
  if (is.null(depth))
    sum(rate* porGrid$mid * Grid$dx)
  else
    sum(rate* porGrid$mid * Grid$dx * (Grid$x.mid < depth))
}  

IntegratedRateSolid <- function(rate, depth = NULL) {  #                     solids
  # print(paste0(" IntegratedRateSolid porGridSolid$mid(end) :  " ,toString(porGridSolid$mid[length(porGridSolid$mid)])))
  if (is.null(depth))
    sum(rate* porGridSolid$mid * Grid$dx)
  else
    sum(rate* porGridSolid$mid * Grid$dx * (Grid$x.mid < depth))
}  

Nbudget <- function(out) {
  SurfDiffflux  <- out$NOxflux + out$NH3flux 
  SurfIrrflux   <- IntegratedRate(out$NOxIrrflux) + IntegratedRate(out$NH3Irrflux)
  SurfOrgflux   <- out$Norgflux
  Surfflux      <- SurfDiffflux + SurfIrrflux + SurfOrgflux
  Deepflux      <- out$NOxdeepflux + out$NH3deepflux + out$Norgdeepflux
  NlossDenit    <- IntegratedRate(out$Denitrific)*0.8
  Nitrif        <- IntegratedRate(out$Nitri)
  Nadsorp       <- IntegratedRate(out$NH3adsorption)
  delta         <- Surfflux - Deepflux - NlossDenit 
  return(list(loss         = NlossDenit, 
              Nitrif       = Nitrif,
              Nadsorp      = Nadsorp,         
              surfOrgflux  = SurfOrgflux,
              surfDiffflux = SurfDiffflux,
              surfIrrflux  = SurfIrrflux,
              burial       = -Deepflux,
              logdelta     = log10(abs(delta)) ))
}

Cbudget <- function(out,p) {
  SurfDiffflux  <- out$DICflux / (as.numeric(p["IrrEnh"]))
  SurfIrrflux   <- IntegratedRate(out$DICIrrflux) + out$DICflux *(1-1/(as.numeric(p["IrrEnh"])))
  Remin         <- IntegratedRate(out$Cprod)
  SurfOrgflux   <- out$Corgflux
  Surfflux      <- SurfDiffflux + SurfIrrflux + SurfOrgflux
  Deepflux      <- out$DICdeepflux + out$Corgdeepflux + out$CaPdeepflux*as.numeric(p["CPrCaP"])
  delta         <- Surfflux - Deepflux
  
  return(list(loss         = 0, 
              surfOrgflux  = SurfOrgflux,
              surfDiffflux = SurfDiffflux,
              surfIrrflux  = SurfIrrflux,
              burial       = -Deepflux,
              Remin        = Remin,
              logdelta     = log10(abs(delta)) ))
}

Obudget <- function(out) {
  SurfDiffflux  <- out$O2flux 
  SurfIrrflux   <- IntegratedRate(out$O2Irrflux)
  Surfflux      <- SurfDiffflux + SurfIrrflux 
  Deepflux      <- out$O2deepflux 
  OlossOxicmin  <- IntegratedRate(out$OxicMin)
  OlossNitri    <- IntegratedRate(out$Nitri)*2
  OlossOduOx    <- IntegratedRate(out$OduOx)
  delta         <- Surfflux - Deepflux - OlossOxicmin - OlossNitri - OlossOduOx
  return(list(loss         = OlossOxicmin + OlossNitri + OlossOduOx, 
              OlossOxicmin = OlossOxicmin,
              OlossNitri   = OlossNitri, 
              OlossOduOx   = OlossOduOx,
              surfDiffflux = SurfDiffflux,
              surfIrrflux  = SurfIrrflux,
              logdelta        = log10(abs(delta)) ))
}

ODUbudget <- function(out,p) {
  SurfDiffflux  <- out$ODUflux 
  SurfIrrflux   <- IntegratedRate(out$ODUIrrflux)
  Surfflux      <- SurfDiffflux + SurfIrrflux  
  Deepflux      <- out$ODUdeepflux 
  Source      <- IntegratedRate(out$AnoxicMin)
  Lossdepo      <- IntegratedRate(out$AnoxicMin)*p["pdepo"]
  LossOduOx     <- IntegratedRate(out$OduOx)
  delta         <- Surfflux - Deepflux + Source - Lossdepo- LossOduOx
  return(list(loss         = Lossdepo + LossOduOx, 
              Lossdepo     = Lossdepo,
              LossOduOx    = LossOduOx, 
              surfDiffflux = SurfDiffflux,
              surfIrrflux  = SurfIrrflux,
              logdelta     = log10(abs(delta)) ))
}

Sibudget <- function(out,p) {
  SurfOrgflux   <- out$Corgflux*p["SiCdet"]
  SurfDiffflux  <- out$SIOflux 
  SurfIrrflux   <- IntegratedRate(out$SIOIrrflux)
  Surfflux      <- SurfDiffflux + SurfIrrflux  
  Deepflux      <- out$SIOdeepflux + out$SiDetdeepflux
  delta         <- SurfOrgflux +Surfflux - Deepflux 
  return(list(surfDiffflux = SurfDiffflux,
              surfIrrflux  = SurfIrrflux,
              logdelta     = log10(abs(delta)) ))
}

Pbudget <- function(out) {
  SurfDiffflux  <- out$PO4flux 
  SurfIrrflux   <- IntegratedRate(out$PO4Irrflux) 
  SurfOrgflux   <- out$Porgflux
  Surfflux      <- SurfDiffflux + SurfIrrflux + SurfOrgflux
  Deepflux      <- out$PO4deepflux + out$FePdeepflux + out$CaPdeepflux + out$Porgdeepflux
  Padsorp       <- IntegratedRate(out$PO4adsorption)
  delta         <- Surfflux - Deepflux 
  return(list(Padsorp      = Padsorp,         
              surfOrgflux  = SurfOrgflux,
              surfDiffflux = SurfDiffflux,
              surfIrrflux  = SurfIrrflux,
              burial       = -Deepflux,
              logdelta     = log10(abs(delta)) ))
}

#source("ObudgetPlot2.R")
#source("CbudgetPlot2.R")
#source("NbudgetPlot.R")
#source("PbudgetPlot2.R")
#source("SibudgetPlot2.R")
  




