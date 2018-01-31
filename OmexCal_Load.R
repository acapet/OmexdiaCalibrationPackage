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
# This script loads all the auxiliary functions, and runs + display a first simulation.
# It then provides an example of how to load data, compute model misfits for specific variables, 
# and display the comparison with model ouptuts
#
################

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
source("OmexCal_BasicSetup.R")

# Convert model outputs to observation-equivalent
source("OmexCal_DIA2OBS.R")

# Runs the model for a given subset of parameters and returns the model ouptut
source("OmexCal_OCALL_SS.R")

# Generic cost function
source('OmexCal_COST_generic.R')

# BudgetDiagnostic tools
source('OmexCal_BudgetFunctions.R')
#source("OmexCal_AddDiagnostics.R")

# Update porosity & bottom water conditions
source('OmexCal_AdaptParsForStation.R')

# Alters the current Porosity profiles and other things according to station info
#source("OMEXDIA_OG3_AdaptParsForStation.R")

# Utilities for result display 
source("OmexCal_SimplePlot.R")
source("OmexCal_ParTablePlot.R")
source("OmexCal_FluxTable.R")
source("OmexCal_FitTablePlot.R")

# load the Fortran OMEXDIA model 
system("R CMD SHLIB omexdia_OG3.f")

if (.Platform$OS.type == "unix"){
  dyn.load("omexdia_OG3.so")
}else {
  dyn.load("omexdia_OG3.dll")  
}

