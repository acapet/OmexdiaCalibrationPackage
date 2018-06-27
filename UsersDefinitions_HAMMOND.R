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

###################### 
#About this script
#####################

# This is a script to define the usage of your data-model calibration procedure using the toolbox OMEXDIACalibrationPackage

# When you run the script your observation data is automatically loaded and converted to a data frame in R 
# for further processing and running the StepwiseCalibration script.
# Futhermore you can define if you wish that your locations of the observation stations are mapped
# Define the usage and you can run the script without further editing
# The Input "datafile" should be in the general setup defined and described at OMEXDIACalibrationPackage


#################
# Define usage #
################

##Define Excel-File
datafile<-"Datas/HAMMOND_Data.xls"  #data-file including sheets of nutrients, fluxes, station informations and later on microprofiles

#########################
# Calibration Procedure #
#########################

# Here follows the definition of the calibration steps
PLIST<-list()
VLIST<-list()
FLIST<-list()
MLIST<-list()

## 1. calibration step
# Parameters 
PLIST[[1]] <- c("pFast","MeanFlux","pRef","biot","NCrref","NCrSdet","mixL","rSlow")
# Observation profiles 
VLIST[[1]] <- c("TOC","TN")
# Observation fluxes
FLIST[[1]] <- c("DIC")
# Observation Microprofiles
MLIST[[1]] <- c() #c("O2micro")

## 2. calibration step
PLIST[[2]] <- c("NCrSdet","mixL","biot","AlphIrr") 
VLIST[[2]] <- c("TOC","DIC","NH3","TN")
FLIST[[2]] <- c("DIC","NH3","NOx","O2")
MLIST[[2]] <- c()

## 3. calibration step
PLIST[[3]] <- c("rSi","SiCdet","EquilSiO")
VLIST[[3]] <- c("SIO","SiDet") #
FLIST[[3]] <- c("SIO")
MLIST[[3]] <- c()

## 4. calibration step
PLIST[[4]] <- c("PCrSdet","rCaPprod")
VLIST[[4]] <- c("PO4")
FLIST[[4]] <- c("PO4")
MLIST[[4]] <- c()

stepNames <-c("Fit C","Fit N","Fit Si","Fit PO")

#####################
#Plotting and Mapping
#####################
# Directory for Plot outputs
plotdir  <- paste0(getwd(),"/Plots_HAM/")

#Define if you wish to plot your observation data before the calibration procedure
plotting<-0 #set to 1 if you want to produce plots of your observation data
#Define if you wish to map the locations of your observational stations
mapping<-0 #set to 1 if you want to produce maps


#If yes please define the following variables
##Define variables that should be plotted
plotvars<-c("NH3","DIC","SIO","PO4","TOC","TN","SiDet")

##Define labels and color code affiliation 
xlabname<-"value [mmol/m^2/d]"
ylabname<-"Depth [cm]"
camosta<-"Station" #or Campaign #color code affiliation

##Location of Stations
#mydata_Loc <- c(12.52, 43.84, 13.25 ,44.25)
Loc_stamen <- c(11.5, 43, 14, 46)
Loc_google <- c(11, 43, 15, 47)  


##########################################
## Adaptation for model/obs comparison!
# Changes in units
# Diagnostic from the model state variable
##########################################

AddDiagnostics <- function (Dy,p) {
  ###########
  ## SiDet ##
  ###########
  # Converting detrital silicate in %dry weight
  Dy[,"SiDet"] <- Dy[,"SiDet"]*28*100*1e-9/2.5
  
  ###########
  ##  TOC  ##
  ###########
  Dy<-cbind(Dy,TOC=( Dy[,"FDET"]+Dy[,"SDET"]+                                # Slow and Fast OrgC
                       p["MeanFlux"]*p["pRef"]/p["w"]/(1-porGrid$int[N+1]))*   # "Refractory", not accounted for by Omexdia, derived from parameters
              14*100*1e-9/2.5                                         # [nmolC/cm³ ] -> [% of dry weight] ; 2.5 gr/cm³ is the bulk sediment desnity
  )
  
  ########
  ## TN ##
  ########
  Dy<-cbind(Dy,TN=( Dy[,"FDET"]*p["NCrFdet"]+Dy[,"SDET"]*p["NCrSdet"]+
                      p["MeanFlux"]*p["pRef"]/p["w"]/(1-porGrid$int[N+1])*p["NCrref"])*
              14*100*1e-9/2.5
  )
  
  return (Dy)
}


##############################
## now you can run the script!
##############################

# source(OmexCal_StepwiseCalibration_MC.R)