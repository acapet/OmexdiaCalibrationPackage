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
source("Utils/OmexCal_BasicSetup.R")

# Convert model outputs to observation-equivalent
source("Utils/OmexCal_DIA2OBS.R")

# Runs the model for a given subset of parameters and returns the model ouptut
source("Utils/OmexCal_OCALL_SS.R")

# Generic cost function
source('Utils/OmexCal_COST_generic.R')

# BudgetDiagnostic tools
source('Utils/OmexCal_BudgetFunctions.R')

# Update porosity & bottom water conditions
source('Utils/OmexCal_AdaptParsForStation.R')

# Utilities for result display 
source("Utils/OmexCal_SimplePlot.R")
source("Utils/OmexCal_SimplePlotMicro.R")
source("Utils/OmexCal_ParTablePlot.R")
source("Utils/OmexCal_FluxTable.R")
source("Utils/OmexCal_FluxPlot.R")
source("Utils/OmexCal_FitTablePlot.R")
source("Utils/OmexCal_ReportGen.R")
source("Utils/OmexCal_MCReportGen.R")
source("Utils/OmexCal_MCParsPlot.R")
source("Utils/OmexCal_MCProfPlot.R")
source("Utils/OmexCal_MCFluxPlot.R")

# load the Fortran OMEXDIA model 
system("R CMD SHLIB Fortran/omexdia_OG3.f")

if (.Platform$OS.type == "unix"){
  dyn.load("Fortran/omexdia_OG3.so")
}else {
  dyn.load("Fortran/omexdia_OG3.dll")  
}

