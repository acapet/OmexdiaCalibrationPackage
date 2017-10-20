#title: "Load_Observation_Data"
#author: "Annika Eisele (Helmholtz-Zentrum Geesthacht) and Arthur Carpet (Universite de Liege)"
#comment: "R-coding alterations by A.Eisele based on R-scripts provided by A.Carpet"" 
#date: "18 Oktober 2017"

### About this script
#This is a script to upload observation data and convert them to a data frame in R for further processing
#Futhermore locations of the observation stations are mapped
#Define the usage and you can run the script without further editing
#Input "datafile" should contain at least data on Station, MidDepth, UpperDepth, LowerDepth, date and each variable columnwise
#Input "datafluxfile" should contain at least data on station, lon, lat, date, depth and variable fluxes columnwise
#Remeber: R is case-sensitive, so check for letters in your Excel-Files and define variables!



### Define usage

##!Set working directory to corresponding file directory before running the script!

##Define Excel-Files
datafile<-"NOAH_C_Nutrients.xlsx"  #data-file of nutrient data and depth
datafluxfile<-"NOAH_C_Fluxes.xlsx" #data-file of flux data including location of stations (lon,lat)

##Define variable use
dfvarsstay<-c("Station","Cruise","UpperDepth","LowerDepth","MidDepth",
              "yearday","date") #entities that should be exluded from the merging to a variable and value cloumn
dfvarsspec<-c("NOx","NO2","NO3","NH4_OPA","PO4","Si_OH_4") #variables that should be plotted in an extra plot

dfflvarsstay<-c("Station","Cruise","BottomDepth","yearday","date","Lon",
                "Lat") #entities that should be exluded from the merging to a variable and value cloumn

varlimod<-c("NH3","SIO","PO4","NO3","TN","SiDet") # List of variables that have to be retained


##Define labels and color code affiliation 
xlabname<-"value [mmol/m^2/d]"
ylabname<-"Depth [cm]"

coloraffiname<-"Cruise" #or Station

plotting<-0 #set to 1 if you want to produce plots

##Location of Stations
#mydata_Loc <- c(8.015, 54.07)
Loc_stamen <- c(4, 53, 9, 56)
Loc_google <- c(3, 53, 10, 56)  

mapping<-0 #set to 1 if you want to produce maps

## now you can run the script!

## Output-variables for further usage are:
#datadf
#datadfforp
#datadfforp2
#datadffl
#datadfflforp




###Load necessary packages
require(reshape2)
require(gdata)
library(readxl)
require(plyr)
library(ggmap)


### Loading data

##Load nutrient data and create dataframe
datadf<-read.xls(datafile, 
                   na.strings=c("#"),as.is = TRUE,fileEncoding="latin1",header=T) #Loading data


##Load benthic fluxes and create dataframe
datadffl<- read.xls(datafluxfile, na.strings=c("#"),as.is = TRUE,fileEncoding="latin1")


##Define variable names
colnames(datadf)[which(colnames(datadf)=="Top")]<-"UpperDepth"
colnames(datadf)[which(colnames(datadf)=="Bottom")]<-"LowerDepth"
colnames(datadf)[which(colnames(datadf)=="NH4_OPA")]<-"NH3"
colnames(datadf)[which(colnames(datadf)=="NOx")]<-"TN"
colnames(datadf)[which(colnames(datadf)=="Si_OH_4")]<-"SIO"
colnames(datadf)[which(colnames(datadf)=="Si_frozen")]<-"SiDet"
#colnames(datadf)[which(colnames(datadf)=="OC")]<-"TOC"

colnames(datadffl)[which(colnames(datadffl)=="FNH4")]<-"FNH3"
colnames(datadffl)[which(colnames(datadffl)=="FNH4_ERR")]<-"FNH3_ERR"
colnames(datadffl)[which(colnames(datadffl)=="FNOx")]<-"FNO3"
colnames(datadffl)[which(colnames(datadffl)=="FNOx_ERR")]<-"FNO3_ERR"
colnames(datadffl)[which(colnames(datadffl)=="FSiO4")]<-"FSIO"
colnames(datadffl)[which(colnames(datadffl)=="FSiO4_ERR")]<-"FSIO_ERR"

##Deal with missing data
datadf<-cbind(datadf,MidDepth=(datadf$LowerDepth+datadf$UpperDepth)/2)
#datadffl<-cbind(datadffl,MidDepth=(datadffl$Bottom+datadffl$Top)/2)

##Add variables
#datadf<-ddply(datadf,.(Station), function(dsub){
#  print(dsub)
#  Surnind<-which(dsub[,"MidDepth"]==0)
#  print(Surnind)
 # DICdelt <- dsub[,"DIC"]-dsub[Surnind,"DIC"]
#  DINdelt <- dsub[,"NH4_OPA"]-dsub[Surnind,"NH4_OPA"]
#  DIPdelt <- dsub[,"PO4"]-dsub[Surnind,"PO4"]
#  cbind(dsub, data.frame( 
#                          #DICdelt = DICdelt,
#                          DINdelt = DINdelt,
#                          DIPdelt = DIPdelt,
#                          #CN = DICdelt/DINdelt,
#                          #CP = DICdelt/DIPdelt,
#                          NP = DINdelt/DIPdelt)
#  )
#}
#)

###Convert datafiles to data frames and subsets of data

datadfforp<-melt(datadf,id.vars=dfvarsstay) #creating data frame
datadfforp2<-subset(datadfforp,variable %in% dfvarsspec) #creating subset of data
datadfflforp<-melt(datadffl,id.vars=dfflvarsstay) #creating data frame


###Plot data according to cruises

if (plotting==1) {
  
##Specfiy color affiliation
colordfaffi<-eval(parse(text=coloraffiname),envir = datadfforp)
colordfaffi2<-eval(parse(text=coloraffiname),envir = datadfforp2) 
colordfflaffi<-eval(parse(text=coloraffiname),envir = datadfflforp) 

##Plot nutrient data
ggplot(datadfforp, aes(y=MidDepth, x=value, color=factor(colordfaffi)))+ 
  geom_point()+geom_path()+
  facet_wrap(~variable,scales="free")+ylim(-35,10)+scale_color_discrete(name=coloraffiname)+
  ylab(ylabname)+xlab(xlabname)

##Plot subset of nutrient data
ggplot(datadfforp2, aes(y=MidDepth, x=value, color=factor(colordfaffi2)))+ 
  geom_point()+geom_path()+
  facet_wrap(~variable,scales="free")+ylim(-35,10)+scale_color_discrete(name=coloraffiname)+
  ylab(ylabname)+xlab(xlabname)

##Plot flux data according to cruises
ggplot(datadfflforp, aes(y=BottomDepth, x=value, color=factor(colordfflaffi)))+ 
  geom_point()+geom_path()+
  facet_wrap(~variable,scales="free")+scale_color_discrete(name=coloraffiname)+
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
    geom_point(data=datadffl, aes(x = Lon, y = Lat, colour=factor(Station),label=Station),size=10)+
    geom_text( data=datadffl, aes(x = Lon, y = Lat, label=Station),hjust=.5, vjust=.5,size=2)
  Sys.sleep(10) 
  ms1


  ##Mapping with google
  myMap_google <- get_map(location=Loc_google,source="google", maptype="satellite", crop=FALSE)
  mg<-ggmap(myMap_google)
  mg1<-
    mg+
    geom_point(data=datadffl, aes(x = Lon, y = Lat, colour=factor(Station),label=Station),size=10)+
    geom_text( data=datadffl, aes(x = Lon, y = Lat, label=Station),hjust=.5, vjust=.5,size=2)
  Sys.sleep(10) 
  mg1
  }
