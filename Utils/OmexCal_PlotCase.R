#########################################
# Plotting (According to flags in User file)

dir.create(plotdir)
###Convert datafiles to data frames and subsets of data
dfProfilesforp <-subset(dfProfiles,variable %in% plotvars) # creating subset of data with only model variables (given in svarnames)

dfProfilesforp <- subset(dfProfilesforp, !is.na(value))

###Plot data according to Campaigns

##Specfiy color affiliation
coloraffiname  <- camosta
colordfnuaffi  <- eval(parse(text=coloraffiname),envir = dfProfiles)
colordfnuaffi2 <- eval(parse(text=coloraffiname),envir = dfProfilesforp) 
colordfflaffi  <- eval(parse(text=coloraffiname),envir = dfFluxes) 
colordfmicroaffi<-eval(parse(text=coloraffiname),envir = dfO2micro) 

##Plot nutrient data
xlabname<-"value [mmol/m^3]"

G1 <- ggplot( subset(dfProfiles, !is.na(value)), aes(y=MidDepth, x=value, color=factor(Station), shape= factor(Campaign)))+ #colordfnuaffi is a generalized term for the color affiliation in this plot. The user can specify in the beginning if he wants to plot different stations or different cruises
  geom_point()+
  geom_errorbarh(aes(xmin=value-err,xmax=value+err))+
  geom_path()+
  facet_wrap(~variable,scales="free")+
  scale_y_reverse()+
  scale_color_discrete(name=coloraffiname)+
  ylab(ylabname)+xlab(xlabname)

pdf(paste0(plotdir,"/ProfileData1.pdf"))
print(G1)
dev.off()

##Plot subset of nutrient data
G2 <- ggplot(dfProfilesforp, aes(y=MidDepth, x=value, color=factor(Station)))+ 
  geom_point()+
  geom_errorbarh(aes(xmin=value-err,xmax=value+err))+geom_path()+
  facet_wrap(~variable,scales="free")+scale_y_reverse()+scale_color_discrete(name=coloraffiname)+
  ylab(ylabname)+xlab(xlabname)

pdf(paste0(plotdir,"/ProfileData2.pdf"))
print(G2)
dev.off()

##Plot flux data according to Campaigns
xlabname<-"value [mmol/m^2/d]"

dfFluxesforpv <- dfFluxes 
dfFluxesforpv$stacam<-paste(dfFluxesforpv$Station,dfFluxesforpv$Campaign)
bi<-subset(dfStations,select=c("Station","Campaign","BottomDepth"))
bi$stacam<-paste(bi$Station,bi$Campaign)             
dfFluxesforpv <-  merge(dfFluxesforpv,subset(bi,select=c("BottomDepth",'stacam')),by="stacam")

dfFluxesforpv<-subset(dfFluxesforpv, !is.na(value))
G3 <-
  ggplot(dfFluxesforpv, aes(x=BottomDepth, y=value, color=stacam, shape=factor(Campaign)))+ 
  geom_point()+
  geom_errorbar(aes(ymin=value-err,ymax=value+err))+
  facet_wrap(~variable,scales="free")+
  scale_x_reverse()+coord_flip()+
  scale_color_discrete(name="Station Campaign")+#geom_smooth(aes(group=Campaign))+
  xlab("Water Depth - [m]")+ylab(xlabname)

pdf(paste0(plotdir,"/FluxData2.pdf"))
print(G3)
dev.off()

##Plot O2microprofile data
if (nrow(dfO2micro)>1){
  G4 <- ggplot(dfO2micro, aes(y=Depth, x=value, color=colordfmicroaffi))+ #colordfnuaffi is a generalized term for the color affiliation in this plot. The user can specify in the beginning if he wants to plot different stations or different cruises
    geom_point()+
    geom_errorbarh(aes(xmin=value-err,xmax=value+err))+geom_path()+
    facet_wrap(~variable,scales="free")+scale_y_reverse()+scale_color_discrete(name=coloraffiname)+
    ylab(ylabname)+xlab(xlabname)
  
  pdf(paste0(plotdir,"/MicroprofileData1.pdf"))
  print(G4)
  dev.off()
}


### Mapping Stations
##Mapping with stamen
if (maporigin == "stammen"){
  myMap_stamen <- get_map(location=Loc_stamen,source="stamen", maptype="watercolor")
  mgs<-ggmap(myMap_stamen)
}else{
  ##Mapping with google
  myMap_google <- get_map(location=Loc_google,source="google", maptype="satellite", crop=FALSE)
  mgs<-ggmap(myMap_google)
}
ms1<-
  mgs+
  geom_point(data=dfStations, aes(x = Lon, y = Lat, colour=Station,shape=Campaign),size=6)+
  geom_text( data=dfStations, aes(x = Lon, y = Lat, label=Station),hjust=.5, vjust=.5,size=2)

pdf(paste0(plotdir,"/StationMap.pdf"))
print(ms1)
dev.off()


