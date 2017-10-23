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
