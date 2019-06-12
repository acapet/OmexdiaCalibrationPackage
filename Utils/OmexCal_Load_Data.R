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

### About this script
#This is a script to upload observation data and convert them to a data frame in R for further processing
#Futhermore locations of the observation stations are mapped
#Define the usage and you can run the script without further editing
#Input "datafile" should contain at least data on Station, MidDepth, UpperDepth, LowerDepth, date and each variable columnwise
#Input "datafluxfile" should contain at least data on station, lon, lat, date, depth and variable fluxes columnwise

dfprvarsstay<-c("Station","Campaign","UpperDepth","LowerDepth","MidDepth") # Entities that should be exluded from the merging to a variable and value cloumn
dfflvarsstay<-c("Station","Campaign")                                      # Entities that should be exluded from the merging to a variable and value cloumn
dfstavarsstay<-c("Station","Campaign","Date","Lat","Lon","BottomDepth")
dfmicrovarsstay<-c("Station","Campaign","Depth","Time")

###Load necessary packages
require(reshape2)
require(gdata)
library(readxl)
require(plyr)
library(ggmap)

### Loading data from Excel-Sheets
sheet_profiles  <- "Profiles"  # data-file of profile data and depth
sheet_fluxes    <- "Fluxes"    # data-file of flux data including location of stations (lon,lat)
sheet_stations  <- "Stations"  # data-file including station data on location, porosity etx
sheet_variables <- "Variables" # data-file including variable information (e.g unit, relative error)
sheet_O2micro   <- "O2Microprofiles" #data-file of O2-microprofiles

##Load nutrient data and create dataframe
dfProfiles  <-read.xls(datafile, sheet_profiles, 
                       na.strings=c("#"),
                       as.is = TRUE,
                       fileEncoding="latin1",header=T) #Loading data

##Load benthic fluxes and create dataframe
dfFluxes    <- read.xls(datafile, sheet_fluxes, na.strings=c("#"),as.is = TRUE,fileEncoding="latin1")

##Load station information and create dataframe
dfStations  <- read.xls(datafile, sheet_stations, na.strings=c("#"),as.is = TRUE,fileEncoding="latin1")

##Load variable information and create dataframe
dfVariables <- read.xls(datafile, sheet_variables, na.strings=c("#"),as.is = TRUE,fileEncoding="latin1")

## Load O2 microprofiles and create dataframe
dfO2micro   <- read.xls(datafile, sheet_O2micro, na.strings=c("#"),as.is = TRUE,fileEncoding="latin1")

#########################################
# Completing the profiles dataframe

## Adding MidDepth
dfProfiles  <- cbind(dfProfiles,MidDepth=(dfProfiles$LowerDepth+dfProfiles$UpperDepth)/2)

## Assign error to profile data
# For each variable VAR the error can be (in the order of interpretation priority) :
#     1) provided as an extra column VAR_ERR
#     2) computed from variable relative error (given in the .xls file in the Sheet "Variables")

dfProfiles              <- melt(dfProfiles,id.vars=dfprvarsstay)     # converting to a data frame
dfProfiles_err          <- subset(dfProfiles,grepl("_ERR",dfProfiles$variable)) # data frame subset with the assumed error for variables
dfProfiles_err$variable <- strsplit(as.vector(dfProfiles_err$variable),"_ERR") 

dfProfiles              <- subset(dfProfiles,!grepl("_ERR",variable))

# The following might probably be better coded with a "plyr" approach
for (VAR in unique(dfProfiles$variable)){
  if(VAR %in% dfProfiles_err$variable ) { # If a column VAR_ERR is present in the .xls file, then it is used for the error.
    dfProfiles[ which(dfProfiles$variable==VAR) , "err"]  <- dfProfiles_err[ which(dfProfiles_err$variable==VAR) , "value"]
  } else {
    # Else, we look for a corresponding relative error in the "Variables" sheet 
    if(VAR %in% dfVariables$Variable ){
      dfProfiles[ which(dfProfiles$variable==VAR), "err"] <- 
        dfProfiles[ which(dfProfiles$variable==VAR), "value"] * dfVariables[which(dfVariables$Variable==VAR),"RelativeError"]
     
      dfProfiles[ which( dfProfiles$variable==VAR  &
                           dfProfiles$err < dfVariables[which(dfVariables$Variable==VAR),"MinError"] ),
                  "err"] <- dfVariables[which(dfVariables$Variable==VAR),"MinError"]
    } else {
      # If no relative error is given in the .xls file we assume 30% error
      dfProfiles[ which(dfProfiles$variable==VAR), "err"] <-dfProfiles[ which(dfProfiles$variable==VAR), "value"]*0.3
      
      dfProfiles[ which( dfProfiles$variable==VAR  &
                           dfProfiles$err < dfVariables[which(dfVariables$Variable==VAR),"MinError"] ),
                  "err"] <- dfVariables[which(dfVariables$Variable==VAR),"MinError"]
    }    
  } 
}

rm(dfProfiles_err)
#########################################
# Completing the Fluxes dataframe
dfFluxes <- melt(dfFluxes,id.vars=dfflvarsstay)     # converting to long data frame

dfFluxes_err          <- subset(dfFluxes,grepl("_ERR",dfFluxes$variable))
dfFluxes_err$variable <- strsplit(as.vector(dfFluxes_err$variable),"_ERR")

dfFluxes              <- subset(dfFluxes,!grepl("_ERR",variable))

for (VAR in unique(dfFluxes$variable)){
  if(VAR %in% dfFluxes_err$variable ) { # If a column VAR_ERR is present in the .xls file, then it is used for the error.
    dfFluxes[ which(dfFluxes$variable==VAR) , "err"]  <- dfFluxes_err[ which(dfFluxes_err$variable==VAR) , "value"]
  }
}

rm(dfFluxes_err)

# Completing the Microprofile dataframe
dfO2micro              <- melt(dfO2micro,id.vars=dfmicrovarsstay)     # converting to long data frame
dfO2micro_err          <- subset(dfO2micro,grepl("_ERR",dfO2micro$variable))
dfO2micro_err$variable <- strsplit(as.vector(dfO2micro_err$variable),"_ERR")

dfO2micro              <- subset(dfO2micro,!grepl("_ERR",variable))

# The following might probably be better coded with a "plyr" approach
for (VAR in unique(dfO2micro$variable)){
  if(VAR %in% dfO2micro_err$variable ) { # If a column VAR_ERR is present in the .xls file, then it is used for the error.
    dfO2micro[ which(dfO2micro$variable==VAR) , "err"]  <- dfO2micro_err[ which(dfO2micro_err$variable==VAR) , "value"]
  } else {
    # Else, we look for a corresponding relative error in the "Variables" sheet 
    if(VAR %in% dfVariables$Variable ){
      dfO2micro[ which(dfO2micro$variable==VAR), "err"] <- 
        dfO2micro[ which(dfO2micro$variable==VAR), "value"] * dfVariables[which(dfVariables$Variable==VAR),"RelativeError"]
      
      dfO2micro[ which( dfO2micro$variable==VAR  &
                          dfO2micro$err < dfVariables[which(dfVariables$Variable==VAR),"MinError"] ),
                 "err"] <- dfVariables[which(dfVariables$Variable==VAR),"MinError"]
    } else {
      # If no relative error is given in the .xls file we assume 30% error
      dfO2micro[ which(dfO2micro$variable==VAR), "err"] <-dfO2micro[ which(dfO2micro$variable==VAR), "value"]*0.3
      
      dfO2micro[ which( dfO2micro$variable==VAR  &
                          dfO2micro$err < dfVariables[which(dfVariables$Variable==VAR),"MinError"] ),
                 "err"] <- dfVariables[which(dfVariables$Variable==VAR),"MinError"]
    }    
  } 
}

rm(dfO2micro_err)

