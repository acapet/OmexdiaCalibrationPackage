#title: "Load_Observation_Data"
#author: "Annika Eisele (Helmholtz-Zentrum Geesthacht) and Arthur Capet (Universite de Liege)"
#comment: "R-coding alterations by A.Eisele based on R-scripts provided by A.Capet"" 
#date: "18 Oktober 2017"

### About this script
#This is a script to upload observation data and convert them to a data frame in R for further processing
#Futhermore locations of the observation stations are mapped
#Define the usage and you can run the script without further editing
#Input "datafile" should contain at least data on Station, MidDepth, UpperDepth, LowerDepth, date and each variable columnwise
#Input "datafluxfile" should contain at least data on station, lon, lat, date, depth and variable fluxes columnwise
#Remeber: R is case-sensitive, so check for letters in your Excel-Files and define variables!


dfprvarsstay<-c("Station","Campaign","UpperDepth","LowerDepth","MidDepth") #entities that should be exluded from t+he merging to a variable and value cloumn

dfflvarsstay<-c("Station","Campaign") #entities that should be exluded from the merging to a variable and value cloumn

dfstavarsstay<-c("Station","Campaign","Date","Lat","Lon","BottomDepth")

###Load necessary packages
require(reshape2)
require(gdata)
library(readxl)
require(plyr)
library(ggmap)

### Loading data
sheet_profiles<-"Profiles"  #data-file of profile data and depth
sheet_fluxes<-"Fluxes" #data-file of flux data including location of stations (lon,lat)
sheet_stations<-"Stations" #data-file including station data on location, porosity etx

##Load nutrient data and create dataframe
datadfpr<-read.xls(datafile, sheet_profiles, na.strings=c("#"),as.is = TRUE,fileEncoding="latin1",header=T) #Loading data

##Load benthic fluxes and create dataframe
datadffl<- read.xls(datafile, sheet_fluxes, na.strings=c("#"),as.is = TRUE,fileEncoding="latin1")

##Load station information and create dataframe
datadfsta<- read.xls(datafile, sheet_stations, na.strings=c("#"),as.is = TRUE,fileEncoding="latin1")


##Deal with missing data
datadfpr<-cbind(datadfpr,MidDepth=(datadfpr$LowerDepth+datadfpr$UpperDepth)/2)




if (plotting==1) {

###Convert datafiles to data frames and subsets of data

datadfprforp<-melt(datadfpr,id.vars=dfprvarsstay) #creating data frame
datadfprforp2<-subset(datadfprforp,variable %in% plotvars) #creating subset of data
# ART 22102017: added a test to consider only the elements of "dfflvarsstay" that are effectively present as column in the "FLUX" sheet 
datadfflforp<-melt(datadffl,id.vars=dfflvarsstay[which(dfflvarsstay %in% colnames(datadffl))]) #creating data frame
datadfpoforp<-melt(datadfpo,id.vars=dfpovarsstay) #creating data frame

# ART 22102017
# Identifying errors from variables
datadfprforpv_err   <- subset(datadfprforp,grepl("_ERR",variable))
datadfprforpv       <- subset(datadfprforp,!grepl("_ERR",variable))
datadfprforpv$error <- datadfprforpv_err$value

datadfflforpv_err   <- subset(datadfflforp,grepl("_ERR",variable))
datadfflforpv       <- subset(datadfflforp,!grepl("_ERR",variable))
datadfflforpv$error <- datadfflforpv_err$value

datadfpoforpv_err   <- subset(datadfpoforp,grepl("_ERR",variable))
datadfpoforpv       <- subset(datadfpoforp,!grepl("_ERR",variable))
datadfpoforpv$error <- datadfpoforpv_err$value

###Plot data according to Campaigns

##Specfiy color affiliation
coloraffiname<-camosta
colordfnuaffi<-eval(parse(text=coloraffiname),envir = datadfprforpv)
colordfnuaffi2<-eval(parse(text=coloraffiname),envir = datadfprforp2) 
colordfflaffi<-eval(parse(text=coloraffiname),envir = datadfflforpv) 
colordfpoaffi<-eval(parse(text=coloraffiname),envir = datadfpoforp) 

##Plot nutrient data
ggplot(datadfprforpv, aes(y=MidDepth, x=value, color=colordfnuaffi))+ #colordfnuaffi is a generalized term for the color affiliation in this plot. The user can specify in the beginning if he wants to plot different stations or different cruises
#  geom_point()+geom_path()+
  geom_errorbarh(aes(xmin=value-error,xmax=value+error))+geom_path()+
  facet_wrap(~variable,scales="free")+scale_y_reverse()+scale_color_discrete(name=coloraffiname)+
  ylab(ylabname)+xlab(xlabname)

##Plot subset of nutrient data
ggplot(datadfprforp2, aes(y=MidDepth, x=value, color=colordfnuaffi2))+ 
#ggplot(datadfprforp2, aes(y=MidDepth, x=value, color=factor(colordfnuaffi2)))+ 
  geom_point()+geom_path()+
  facet_wrap(~variable,scales="free")+scale_y_reverse()+scale_color_discrete(name=coloraffiname)+
  ylab(ylabname)+xlab(xlabname)

##Plot flux data according to Campaigns
datadfflforpv$BottomDepth=datadfsta$BottomDepth
ggplot(datadfflforpv, aes(y=BottomDepth, x=value, color=colordfflaffi))+ 
  #geom_point()+geom_path()+
  geom_errorbarh(aes(xmin=value-error,xmax=value+error))+
  facet_wrap(~variable,scales="free")+scale_y_reverse()+scale_color_discrete(name=coloraffiname)+
  ylab(ylabname)+xlab(xlabname)

##Plot porosity data
ggplot(datadfpoforp, aes(y=MidDepth, x=value, color=colordfpoaffi))+ #colordfnuaffi is a generalized term for the color affiliation in this plot. The user can specify in the beginning if he wants to plot different stations or different cruises
  geom_point()+geom_path()+
#  geom_errorbarh(aes(xmin=value-error,xmax=value+error))+geom_path()+
  facet_wrap(~variable,scales="free")+scale_y_reverse()+scale_color_discrete(name=coloraffiname)+
  ylab(ylabname)+xlab(xlabname)



#?ggplot(melt(datadffl,id.vars=c("Station","Lon","Lat")),aes(x=Station, y=))
}

### Mapping NOAH Stations

if (mapping==1) {

  ##Mapping with stamen
  myMap_stamen <- get_map(location=Loc_stamen,source="stamen", maptype="watercolor")
  ms<-ggmap(myMap_stamen)
  ms1<-
    ms+
    geom_point(data=datadfsta, aes(x = Lon, y = Lat, colour=factor(Station),label=Station),size=10)+
    geom_text( data=datadfsta, aes(x = Lon, y = Lat, label=Station),hjust=.5, vjust=.5,size=2)
  Sys.sleep(10) 
  ms1

  ##Mapping with google
  myMap_google <- get_map(location=Loc_google,source="google", maptype="satellite", crop=FALSE)
  mg<-ggmap(myMap_google)
  mg1<-
    mg+
    geom_point(data=datadfsta, aes(x = Lon, y = Lat, colour=factor(Station),label=Station),size=10)+
    geom_text( data=datadfsta, aes(x = Lon, y = Lat, label=Station),hjust=.5, vjust=.5,size=2)
  Sys.sleep(10) 
  mg1
  }
