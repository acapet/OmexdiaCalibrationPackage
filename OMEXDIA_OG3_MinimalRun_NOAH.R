# The package to read .xls
require(gdata)
# The package with the solution methods
require(ReacTran)   
require(marelac)
# Calibration procedures
require(FME) 

require(plyr)
library(ggplot2)
library(RColorBrewer)
require(gridExtra)
require(reshape2)

# Parameters and grid information 
source("OMEXDIA_OG3_BasicSetup.R")

# Get observations for a selected station
#source("OMEXDIA_OG3_ObsAtSta.R")

# Convert model outputs to observation-equivalent
#source("OMEXDIA_OG3_DIA2OBS.R")

# Runs the model for a given subset of parameters and returns the model ouptut
source("OMEXDIA_OG3_OCALL_SS.R")

# Alters the current Porosity profiles and other things according to station info
# ( It is probably possible to get rid of that one) 
#source("OMEXDIA_OG3_AdaptParsForStation.R")

# Utilities for result display 
source("OMEXDIA_OG3_SimplePlot.R")
source("OMEXDIA_OG3_ParTablePlot.R")
source("OMEXDIA_OG3_FluxTable.R")
source("OMEXDIA_OG3_FitTablePlot.R")


# load the librairy from the Fortran OMEXDIA model 
system("R CMD SHLIB omexdia_OG3.f")

if (.Platform$OS.type == "unix"){
  dyn.load("omexdia_OG3.so")
}else {
  dyn.load("omexdia_OG3.dll")  
}

parSta<-pars

# Model outputs are store in there 
DIA <-OCALL(pars)

# But you could directly display 
pars["AlphIrr"]<-0.5

Simplot(pars)


################
##  WITH DATA ##

# The following should load data inside "datadf" and "datadffl" dataframes.
# Test with the example 'HammondLoad.R' to have an idea of the requested format, then build your own "...Load.R" script based on your own data.

source('Load_Data.R')
source('OMEXDIA_OG3_ObsAtSta_NOAH.R')

sta<-"C"
cam<-"HE432"

localdata   <- OBSatstaSSf(sta,cam)
localdata$variable<-as.character(localdata$variable)

localdatafl <- subset(localdata, LowerDepth==0&UpperDepth==0)
localdata   <- subset(localdata, !(LowerDepth==0&UpperDepth==0))

ggplot(localdata,
       aes(x=value,y=UpperDepth/2+LowerDepth/2,
             ymax=UpperDepth,ymin=LowerDepth,
             xmin=value-err, xmax=value+err))+
  geom_errorbar()+
  geom_errorbarh()+
  geom_point(size=2)+
  facet_wrap(~variable,scales = "free")+ylim(c(20,0))

#  Cost function 
source('OMEXDIA_OG3_COST_generic.R')
source('OMEXDIA_OG3_DIA2OBS.R')
source('OMEXDIA_OG3_BudgetFunctions.R')

C1<-OCOST_GEN(pars,Vlist = c("NH3"))
C1<-OCOST_GEN(pars,Vlist = c("SIO"))

C2<-OCOST_GEN(pars,Vlist = c("SIO","NH3"))
C3<-OCOST_GEN(pars,Vlist = c("SIO","NH3"), Flist = c("NH3","NO3"))

#C1<-OCOST_GEN(pars,Vlist = c("NH3"))
#C2<-OCOST_GEN(pars,Vlist = c("NH3","DIC"))
#C3<-OCOST_GEN(pars,Vlist = c("NH3","DIC"), Flist = c("DIC","NH3"))

# Better to test one by one : 
Simplot(pars,TRUE)+
  ggtitle(paste(sta,"0. No Fit"))

partableplot(pars)

fluxtable(pars)$p

# Collect all on the same plot
pdf(paste(sta,"_Fit0.pdf",sep=""),width=5*(3+1)+2,height=15)
grid.arrange(Simplot(parSta,TRUE)+ggtitle(paste(sta,"1. Pseudo")),
             arrangeGrob(partableplot(parSta)),
             arrangeGrob(fluxtable(parSta)$p,
                         fittableplot(C3),ncol=1,heights=c(6,4)),
             ncol = 3,nrow=1, widths=c(5*3,7,3), heights = c(12))
dev.off()









