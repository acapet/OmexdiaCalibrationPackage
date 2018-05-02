#title: "UsersDefinitions"
#author: "Annika Eisele (Helmholtz-Zentrum Geesthacht) and Arthur Capet (Universite de Liege)"
#comment: "R-coding alterations by A.Eisele based on R-scripts provided by A.Capet"" 
#date: "18 Oktober 2017"

###################### 
#About this script
#####################

#This is a script to define the usage of your data-model calibration procedure using the toolbox OMEXDIACalibrationPackage

# When you run the script your observation data is automatically loaded and converted to a data frame in R 
# for further processing and running the StepwiseCalibration script.
# Futhermore you can define if you wish that your locations of the observation stations are mapped
# Define the usage and you can run the script without further editing
# The Input "datafile" should be in the general setup defined and described at OMEXDIACalibrationPackage
# Remeber: R is case-sensitive, so check for letters in your Excel-Files and defined variables!

##!Set working directory to corresponding file directory before running the script!
rm(list=ls()) #clears the environment


#################
# Define usage #
################

##Define Excel-File
datafile<-"Hammond_Data.xlsx"  #data-file including sheets of nutrients, fluxes, station informations and later on microprofiles

##Define variables that should be considered in the calibration procedure
varlimod<-c("NH3","DIC","SIO","PO4","TOC","TN","SiDet") # List of variables that should be used for the calibration procedure


#########################
# Calibration Procedure #
#########################

# Here follows the definition of the calibration steps

## 1. calibration step
# Parameters 
PLISTC <- c("pFast","WPOC","pRef","biot","NCrref","NCrSdet","mixL","rSlow")
# Observation profiles 
VLISTC <- c("TOC","TN")
# Observation fluxes
FLISTC <- c("DIC")

## 2. calibration step
PLISTN <- c("NCrSdet","mixL","biot","AlphIrr") 
VLISTN <- c("TOC","DIC","NH3","TN")
FLISTN <- c("DIC","NH3","NO3","O2")

## 3. calibration step
PLISTSIO <- c("rSi","SiCdet","EquilSiO")
VLISTSIO <- c("SIO") #"SiDet"
FLISTSIO <- c("SIO")

## 4. calibration step
PLISTPO4 <- c("PCrSdet","rCaPprod")
VLISTPO4 <- c("PO4")
FLISTPO4 <- c("PO4")


# added by A.Eisele 24.10.2017 
#generalized list for automatical calibration procedure on desired fitting steps

#Define which calibration steps should be included in the calibration procedure
PLIST<-list(PLISTC,PLISTN,PLISTSIO,PLISTPO4)
VLIST<-list(VLISTC,VLISTN,VLISTSIO,VLISTPO4)
FLIST<-list(FLISTC,FLISTN,FLISTSIO,FLISTPO4)




#####################
#Plotting and Mapping
#####################

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


##############################
## now you can run the script!
##############################

source(OMEXDIA_OG3_StepwiseCalibration.R)
