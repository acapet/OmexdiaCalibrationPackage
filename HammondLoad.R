require(reshape2)
require(gdata)

datadf<- read.xls("HAMMOND3.xls", na.strings=c("#"),as.is = TRUE,fileEncoding="latin1",header=T)

datadf<-cbind(datadf,Mid=(datadf$Bottom+datadf$Top)/2)
colnames(datadf)[which(colnames(datadf)=="TCO2")]<-"DIC"

datadffl<- read.xls("HAMMOND_FL2.xls", na.strings=c("#"),as.is = TRUE,fileEncoding="latin1")

colnames(datadf)[which(colnames(datadf)=="Mid")]<-"MidDepth"
colnames(datadf)[which(colnames(datadf)=="Top")]<-"UpperDepth"
colnames(datadf)[which(colnames(datadf)=="Bottom")]<-"LowerDepth"

if(F){

  ##  PLOTTING  ##
require(ggplot2)

datadf<-ddply(datadf,.(Station), function(dsub){
  print(dsub)
  Surnind<-which(dsub[,"Bottom"]==0)
  print(Surnind)
  DICdelt <- dsub[,"DIC"]-dsub[Surnind,"DIC"]
  DINdelt <- dsub[,"NH3"]-dsub[Surnind,"NH3"]
  DIPdelt <- dsub[,"PO4"]-dsub[Surnind,"PO4"]
  cbind(dsub, data.frame( DICdelt = DICdelt,
                          DINdelt = DINdelt,
                          DIPdelt = DIPdelt,
                          CN = DICdelt/DINdelt,
                          CP = DICdelt/DIPdelt,
                          NP = DINdelt/DIPdelt)
  )
}
)

datadfforp<-melt(datadf,id.vars=c("Station","Top","Bottom","Mid","yday"))
  
ggplot(datadfforp, aes(y=(Bottom+Top)/2, x=value, color=factor(Station)))+
  geom_point()+geom_path()+
  facet_wrap(~variable,scales="free")+ylim(20,0)+scale_color_discrete(name="Stations")+ylab("Depth-[cm]")

datadfforp2<-subset(datadfforp,variable %in% c("DIC","NH3","PO4","SiO2","OC","Bsi"))
ggplot(datadfforp2, aes(y=(Bottom+Top)/2, x=value, color=factor(Station)))+
  geom_point()+geom_path()+
  facet_wrap(~variable,scales="free")+ylim(20,0)+scale_color_discrete(name="Stations")+ylab("Depth-[cm]")

library(ggmap)

myLocation <- c(12.52, 43.84, 13.25 ,44.25)
adriLoc <- c(11, 42, 16, 48)

myMap <- get_map(location=adriLoc,source="google", maptype="satellite", crop=FALSE)
m<-ggmap(myMap)
m1<-
  m+
  geom_point(data=datadffl, aes(x = Lon, y = Lat, colour=factor(Station),label=Station),size=10)+
  geom_text( data=datadffl, aes(x = Lon, y = Lat, label=Station),hjust=.5, vjust=.5,size=2)

ggplot(melt(datadffl,id.vars=c("Station","Lon","Lat")),aes(x=Station, y=))

# EGU MAP
myLocation <- c(12.52, 43.84, 13.25 ,44.25)
adriLoc <- c(11.5, 43, 14, 46)
myMap <- get_map(location=adriLoc,source="stamen", maptype="watercolor")
m<-ggmap(myMap)
m


adriLoc <- c(11, 43, 15, 47)
myMap <- get_map(location=adriLoc,source="google", maptype="satellite", crop=FALSE)
m<-ggmap(myMap)
m

m1<-
  m+
  geom_point(data=subset(datadffl,Station!="H3"), aes(x = Lon, y = Lat, colour=Station,label=Station),size=6)+
  geom_text( data=subset(datadffl,Station!="H3"), aes(x = Lon, y = Lat, label=Station),hjust=.5, vjust=.5,size=2)+
  theme(legend.position="left")

m1

pdf("/home/arthur/Desktop/DOCS/MEETINGS/2016_04_EGU/PosterBenthic/FIGS/Hmap2.pdf",width=5)
m1
dev.off()
}
