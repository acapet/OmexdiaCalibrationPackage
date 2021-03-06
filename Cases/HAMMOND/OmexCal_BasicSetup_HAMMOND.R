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
# This script : 
# * defines essential objects for OMEXIDA simulations (grids, etc ..)
# * Provide guess value and bounds for parameters
# 
# This is a script, not a function, so variables defined here are global.
################

require(ReacTran)   
require(marelac)
require(ggplot2)
require(gridExtra)

# Grid: 100 layers; total length=50 cm, first box=0.01 cm
Grid  <- setup.grid.1D(N = 100, dx.1 = 0.01, L = 50)
Depth <- Grid$x.mid
N     <- Grid$N

# exponential function
exp.profile <- function(x, x.0, y.0, y.inf, x.att)
  return(y.inf + (y.0-y.inf)*exp(-pmax(0.,(x-x.0))/x.att))

# reference 

## Parameters value are here updated according to calibrated Stations (Example: Hammond) 
parsdf<-rbind(
  
  # abiotic conditions
  data.frame(row.names="Temp"       ,guess=14       ,min=5        ,max=26       ,unit="°C"       ,printfactor=1,printunit=NA    ,  constrain ='Pel. Mod.')      , # Temperature
  data.frame(row.names="Sal"        ,guess=36       ,min=36       ,max=38       ,unit="psu"      ,printfactor=1,printunit=NA     ,  constrain ='Pel. Mod.')       , # Salinity
  data.frame(row.names="portop"     ,guess=0.65     ,min=0.5      ,max=0.95     ,unit="W. Cont." ,printfactor=1,printunit=NA      ,  constrain ='Fixed Map')    , # surface porosity
  data.frame(row.names="porbot"     ,guess=0.6      ,min=0.4      ,max=0.7      ,unit="W. Cont." ,printfactor=1,printunit=NA    ,  constrain ='Fixed Map')   , # porosity  at depth
  data.frame(row.names="pora"       ,guess=0.5      ,min=0.25     ,max=0.75     ,unit=" "        ,printfactor=1,printunit=NA     ,  constrain ='Fixed Map')   , # porosity decrease
  
  # Bioturbation
  data.frame(row.names="biot"       ,guess=10/365   ,min=0.01/365 ,max=30/365   ,unit="cm2/d"    ,printfactor=365, printunit="cm2/yr" , constrain ='Biol.') ,     # bioturbation coefficient % range from TROMP1995
  data.frame(row.names="mixL"       ,guess=15       ,min=5        ,max=30       ,unit="cm"       ,printfactor=1,printunit=NA          , constrain ='Biol.')  ,     # depth of mixed layer
  data.frame(row.names="AlphIrr"    ,guess=91/365   ,min=0/365    ,max=180/365  ,unit="/d"       ,printfactor=1, printunit="cm2/d"    , constrain ='Biol.')    ,     # depth of mixed layer
  data.frame(row.names="IrrEnh"     ,guess=1        ,min=1        ,max=18       ,unit="-"        ,printfactor=1, printunit=NA         , constrain ='Stat. Calib.')      ,     # depth of mixed layer
  
  # organic matter dynamics  #
  data.frame(row.names="w"          ,guess=0.16/365 ,min=0.001/365 ,max=2/365   ,unit="cm/d"     ,printfactor=365, printunit="cm/yr"   , constrain ='Fixed Map')  ,     # advection rate
  data.frame(row.names="MeanFlux"   ,guess=3000     ,min=10        ,max=8000    ,unit="nmol/cm2/d",printfactor=1/100, printunit="mmol/m2/d",  constrain ='Pel. Mod.'), # C deposition

  # On 1 Aug 2017, changed for fixed rslow: 1 /yr, instead of guess of 0.1 previously.
  data.frame(row.names="rFast"      ,guess=12/365   ,min=2/365     ,max=80/365  ,unit="/d",printfactor=365, printunit="/yr"     , constrain ='Benth. Mod.')      , # decay rate fast decay det.
  data.frame(row.names="rSlow"      ,guess=.1/365    ,min=0.1/365   ,max=2/365   ,unit="/d",printfactor=365, printunit="/yr"     , constrain ='Benth. Mod.')      , # decay rate slow decay det.
  data.frame(row.names="pFast"      ,guess=0.27     ,min=0.05      ,max=0.95    ,unit="-",printfactor=1,printunit=NA            , constrain ='Benth. Mod.')         , # fraction fast det. in flux
  data.frame(row.names="pRef"       ,guess=0.018    ,min=0.001     ,max=0.85    ,unit="-",printfactor=1,printunit=NA            , constrain ='Benth. Mod.')         , # fraction fast det. in flux
  data.frame(row.names="NCrFdet"    ,guess=1/6.625  ,min=1/25      ,max=1/3     ,unit="molN/molC",printfactor=1,printunit=NA    , constrain ='Pel. Mod.') , # NC ratio fast decay det.
  data.frame(row.names="NCrSdet"    ,guess=1/25     ,min=1/100      ,max=1/6     ,unit="molN/molC",printfactor=1,printunit=NA    , constrain ='Pel. Mod.') , # NC ratio Slow decay det.
  data.frame(row.names="NCrref"     ,guess=1/25     ,min=1/50      ,max=1/6     ,unit="molN/molC",printfactor=1,printunit=NA    , constrain ='Pel. Mod.') , # NC ratio "Slow decay"ref" det.
  data.frame(row.names="rSi"        ,guess=.02/365  ,min=0.01/365  ,max=10/365  ,unit="/d" ,printfactor=365, printunit="/yr"    , constrain ='Benth. Mod.'),         # Dissolution rate for SiDET
  data.frame(row.names="SiCdet"     ,guess=1/15     ,min=1/60      ,max=1/2     ,unit="-",printfactor=1,printunit=NA            , constrain ='Pel. Mod.'), # Sinking SiDet Flux
  data.frame(row.names="EquilSiO"   ,guess=400      ,min=150       ,max=900     ,unit="mmol/m3",printfactor=1,printunit=NA      , constrain ='Stat. Calib.'),    # Equilibrium concentration with Opaline dissolution @ 20°C
  data.frame(row.names="PCrFdet"    ,guess=1/106    ,min=1/400     ,max=1/50    ,unit="molP/molC",printfactor=1,printunit=NA    , constrain ='Pel. Mod.'),    #
  data.frame(row.names="PCrSdet"    ,guess=1/800    ,min=1/800     ,max=1/50    ,unit="molP/molC",printfactor=1,printunit=NA    , constrain ='Pel. Mod.'),    #
  data.frame(row.names="rFePdesorp" ,guess=105/365  ,min=1/365     ,max=200/365 ,unit="/d",printfactor=365, printunit="/yr"     , constrain ='Stat. Calib.'), # 
  data.frame(row.names="rFePadsorp" ,guess=77/365   ,min=.2        ,max=2.5     ,unit="/d",printfactor=365, printunit="/yr"     , constrain ='Stat. Calib.'), # 
  data.frame(row.names="rCaPprod"   ,guess=0.001/365 ,min=0.0001/365  ,max=10/365  ,unit="/d",printfactor=365, printunit="/yr"     , constrain ='Stat. Calib.'), # 
  data.frame(row.names="rCaPdiss"   ,guess=0*0.000001/365  ,min=0.1/365   ,max=50/365  ,unit="/d",printfactor=365, printunit="/yr"     , constrain ='Stat. Calib.'), # 
  data.frame(row.names="CPrCaP"     ,guess=1.32/4.6 ,min=1/4.6     ,max=4/4.6   ,unit="/d",printfactor=365, printunit="/yr"     , constrain ='Stat. Calib.'), # 
  data.frame(row.names="PO4ads"     ,guess=2        ,min=1        ,max=400     ,unit="",printfactor=1,printunit=NA             , constrain ='Stat. Calib.'), #
  
  # in Slomp et al two values for P Eq adsorption : 200 in zone I and 2 in zone II ....Difficult to let this be fitted
  data.frame(row.names="Q"          ,guess=2        ,min=1.5       ,max=2.5     ,unit="-",printfactor=1,printunit=NA            , constrain ='Stat. Calib.'), # Q10 for mineralisation (both slow and fast)
  data.frame(row.names="pdepo"      ,guess=0.3      ,min=0.08      ,max=0.3     ,unit="-",printfactor=1,printunit=NA            , constrain ='Stat. Calib.'), # Q10 for mineralisation (both slow and fast)
  
  # Nutrient parameters
  data.frame(row.names="NH3Ads"     ,guess=1.3     ,min=0        ,max=2        ,unit="-",printfactor=1,printunit=NA            , constrain ='Stat. Calib.'), # Adsorption coeff ammonium
  data.frame(row.names="rnit"       ,guess=20      ,min=1        ,max=500       ,unit="/d",printfactor=1,printunit=NA           , constrain ='Stat. Calib.'), # Max nitrification rate
  data.frame(row.names="ksO2nitri"  ,guess=10.     ,min=.5       ,max=2        ,unit="umolO2/m3",printfactor=1,printunit=NA    , constrain ='Stat. Calib.'), # half-sat O2 in nitrification
  data.frame(row.names="rODUox"     ,guess=50.     ,min=1        ,max=300      ,unit="/d",printfactor=1,printunit=NA           , constrain ='Stat. Calib.'), # Max rate oxidation of ODU
  data.frame(row.names="ksO2oduox"  ,guess=1.      ,min=.5       ,max=21       ,unit="mmolO2/m3",printfactor=1,printunit=NA    , constrain ='Stat. Calib.'), # half-sat O2 in oxidation of ODU
  data.frame(row.names="ksO2oxic"   ,guess=3.      ,min=1        ,max=5        ,unit="mmolO2/m3",printfactor=1,printunit=NA    , constrain ='Stat. Calib.'), # half-sat O2 in oxic mineralis
  data.frame(row.names="ksNOxdenit" ,guess=30.     ,min=10       ,max=50       ,unit="mmolNOx/m3",printfactor=1,printunit=NA   , constrain ='Stat. Calib.'), # half-sat NOx in denitrif
  data.frame(row.names="kinO2denit" ,guess=10      ,min=1        ,max=20       ,unit="mmolO2/m3",printfactor=1,printunit=NA    , constrain ='Stat. Calib.'), # half-sat O2 inhib denitrif
  data.frame(row.names="kinNOxanox" ,guess=10      ,min=.5       ,max=20       ,unit="mmolNOx/m3",printfactor=1,printunit=NA   , constrain ='Stat. Calib.'), # half-sat NOx inhib anoxic min
  data.frame(row.names="kinO2anox"  ,guess=8       ,min=.5       ,max=20       ,unit="mmolO2/m3",printfactor=1,printunit=NA    , constrain ='Stat. Calib.'), # half-sat O2 inhib anoxic min
 
   # Nutrient bottom water conditions
  data.frame(row.names="bwO2"       ,guess=300     ,min=0        ,max=500      ,unit="mmol/m3",printfactor=1,printunit=NA      , constrain ='Pel. Mod.'),    # Bottom Water O2 Concentration
  data.frame(row.names="bwNH3"      ,guess=2       ,min=1        ,max=10       ,unit="mmol/m3",printfactor=1,printunit=NA      , constrain ='Pel. Mod.'),    # Bottom Water NH3 Concentration
  data.frame(row.names="bwNOx"      ,guess=2       ,min=0        ,max=30       ,unit="mmol/m3",printfactor=1,printunit=NA      , constrain ='Pel. Mod.'),    # Bottom Water NOx Concentration
  data.frame(row.names="bwODU"      ,guess=0       ,min=0        ,max=0.2      ,unit="mmol/m3",printfactor=1,printunit=NA      , constrain ='Pel. Mod.'),    # Bottom Water ODU Concentration
  data.frame(row.names="bwDIC"      ,guess=2800    ,min=2400     ,max=3500     ,unit="mmol/m3",printfactor=1,printunit=NA      , constrain ='Pel. Mod.'),    # Bottom Water DIC Concentration
  data.frame(row.names="bwSIO"      ,guess=15      ,min=1        ,max=25       ,unit="mmol/m3",printfactor=1,printunit=NA      , constrain ='Pel. Mod.'),     # Bottom Water SiO Concentration
  data.frame(row.names="bwPO4"      ,guess=.5      ,min=.01      ,max=1        ,unit="mmol/m3",printfactor=1,printunit=NA      , constrain ='Pel. Mod.')     # Bottom Water SiO Concentration
)

# To limit the number of variable parameter some relationships can be used
# most found in the literature use w, or bottom depth, for instance
#pDepo      <- min(1,0.233*(pars["w"]*365)**0.336 )

parsdf$unit<-as.character(parsdf$unit)
parsdf$printunit[which(is.na(parsdf$printunit))]<-parsdf$unit[which(is.na(parsdf$printunit))]

pars<-as.numeric(parsdf[,"guess"])
names(pars)<-rownames(parsdf)

parsdfforprint<-subset(parsdf,select = c("guess","min","max","unit","constrain"))
parsdfforprint$guess<-parsdf$guess*parsdf$printfactor
parsdfforprint$min<-parsdf$min*parsdf$printfactor
parsdfforprint$max<-parsdf$max*parsdf$printfactor
parsdfforprint$unit<-parsdf$printunit

qplot(1:10, 1:10, geom = "blank")+theme_bw()+
  theme(line = element_blank(),text = element_blank())+
  annotation_custom(grob = tableGrob(format(parsdfforprint,digits = 3,nsmall=0,scientific=F,drop0trailing=T))) 

# names of state variables and initial conditions
svarnames   <- c("FDET", "SDET", "O2", "NOx", "NH3", "ODU","DIC","SiDet","SIO","PO4","FeP","CaP")
nspec       <- length(svarnames)
Cini        <- rep(10, N*nspec)

DbGrid      <- setup.prop.1D(func = exp.profile, x.0 = pars["mixL"],
                             y.0 = pars["biot"]  , y.inf = 0 , x.att = 1, 
                             grid = Grid)

AlphIrrGrid <- setup.prop.1D(func = exp.profile, x.0 = pars["mixL"],
                             y.0 = pars["AlphIrr"], y.inf = 0, x.att = .1, 
                             grid = Grid)

IrrEnhGrid  <- setup.prop.1D(func = exp.profile  , x.0 = pars["mixL"],
                             y.0 = pars["IrrEnh"], y.inf = 1, x.att = .1, 
                             grid = Grid)

DiffCoeffs  <- diffcoeff(S = pars["Sal"], t=pars["Temp"])*3600*24*1e4 # from m2/s -> cm2/d

pars["DispO2"]     <- as.numeric(DiffCoeffs["O2"])  
pars["DispNOx"]    <- mean(as.numeric(DiffCoeffs[c("NO3","NO2")] ))
pars["DispNH3"]    <- as.numeric(DiffCoeffs["NH3"] )
pars["DispODU"]    <- as.numeric(DiffCoeffs["HSO4"])
pars["DispDIC"]    <- mean(as.numeric(DiffCoeffs[c("CO2","HCO3","CO3")]))
pars["DispSIO"]    <- as.numeric(DiffCoeffs["H4SiO4"])
pars["DispPO4"]    <- as.numeric(DiffCoeffs["PO4"])

############
# Porosity #
############

  # Opt1 : Constant porosity (ie., no compaction)
  porGrid            <- setup.prop.1D(value =   pars["portop"] , grid = Grid)  
  porGridSolid       <- setup.prop.1D(value = 1-pars["portop"] , grid = Grid )

  if (TRUE) { 
  # Opt2 : Exponential porosity decrease (ie., compaction)
  porGrid             <- setup.prop.1D(func  = exp.profile, x.0 = 0,
                                       y.0   = pars["portop"],
                                       y.inf = pars["porbot"],
                                       x.att = 1/pars["pora"] , 
                                       grid = Grid)
  porGrid$mid[N]   <- porGrid$mid[N-1]
  porGrid$int[N+1] <- porGrid$mid[N]
  porGrid$int[N]   <- porGrid$mid[N]
  porGridSolid$mid <- 1-porGrid$mid
  porGridSolid$int <- 1-porGrid$int
}
