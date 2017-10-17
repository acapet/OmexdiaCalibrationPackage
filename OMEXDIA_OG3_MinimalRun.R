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
#source("OMEXDIA_OG3_ParTablePlot.R")
#source("OMEXDIA_OG3_FitTablePlot.R")
#source("OMEXDIA_OG3_FluxTable.R")

# load the librairy from the Fortran OMEXDIA model 
system("R CMD SHLIB omexdia_OG3.f")
dyn.load("omexdia_OG3.so")

parSta<-pars

# Model outputs are store in there 
DIA <-OCALL(pars)

# But you could directly display 
pars["AlphIrr"]<-0.5

Simplot(pars)
