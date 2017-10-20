# This script is part of the OmexdiaCalibration suite.
# It loads all the auxiliarry functions, and runs + display a first simulation.
# It then provides an example of how to load data, compute cost function for specific variables, 
# and display the comparison with model ouptuts
#
# A. Capet, 2017, acapet@ulg.ac.be

############################
# LIBRAIRIES and SOURCES   #
############################

# The package to read .xls
require(gdata)
# The package with the solution methods
require(ReacTran)   
require(marelac)
# Calibration procedures
require(FME) 

# Tools
require(plyr)
library(ggplot2)
library(RColorBrewer)
require(gridExtra)
require(reshape2)

# Parameters and grid information 
source("OMEXDIA_OG3_BasicSetup.R")

# Get observations for a selected station
source("OMEXDIA_OG3_ObsAtSta.R")

# Convert model outputs to observation-equivalent
source("OMEXDIA_OG3_DIA2OBS.R")

# Runs the model for a given subset of parameters and returns the model ouptut
source("OMEXDIA_OG3_OCALL_SS.R")

# Generic cost function
source('OMEXDIA_OG3_COST_generic.R')

# BudgetDiagnostic tools
source('OMEXDIA_OG3_BudgetFunctions.R')

# Alters the current Porosity profiles and other things according to station info
#source("OMEXDIA_OG3_AdaptParsForStation.R")

# Utilities for result display 
source("OMEXDIA_OG3_SimplePlot.R")
source("OMEXDIA_OG3_ParTablePlot.R")
source("OMEXDIA_OG3_FluxTable.R")
source("OMEXDIA_OG3_FitTablePlot.R")

# load the Fortran OMEXDIA model 
system("R CMD SHLIB omexdia_OG3.f")

if (.Platform$OS.type == "unix"){
  dyn.load("omexdia_OG3.so")
}else {
  dyn.load("omexdia_OG3.dll")  
}

############################
#      EXAMPLE OF USE      #
############################

# The variable global parSta is used inside auxiliary functions
# It consists in the full parameter list, adapted for the station 
# Here we just copy the general parameters value as given in "OMEXDIA_OG3_BasicSetup.R"
parSta<-pars

# OCALL gets the model solution for parameters given in argument
DIA <-OCALL(pars)

# Display can be done directly with parameters value
Simplot(pars)

# .. or with model outputs -> TO UPDATE
# Simplot(DIA)

################
##  USER DATA ##
################

# The following should load all stations data inside "datadf" and "datadffl" dataframes.
# Test with the example 'HammondLoad.R' to have an idea of the requested format.
#    then build your own "...Load.R" script based on your own data.
source('HammondLoad.R')
sta<-"H2"

# We then create "localdata" dataframes, specific to one station.
localdata   <- OBSatstaSSf(sta)
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

# parSta<-AdaptParsForStation_SS(sta)

#  Cost function can be called with a list of profile variables and a list of flux variables
C1<-OCOST_GEN(pars,Vlist = "NH3")
C2<-OCOST_GEN(pars,Vlist = c("NH3","DIC"))
C3<-OCOST_GEN(pars,Vlist = c("NH3","DIC"), Flist = c("DIC","NH3"))

# Better to test those display script one by one : 
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









