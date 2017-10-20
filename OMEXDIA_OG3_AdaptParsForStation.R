# This script is part of the OmexdiaCalibration suite.
# It takes a particular station in argument, and update the global porosity value
# 
# A. Capet, 2017, acapet@ulg.ac.be

AdaptParsForStation_SS <- function (parsl,sta) {
  
  # Take a params set and changes values according to station info 
  # Compute an average porosity profile
  
  # Hammond porosity are averages for 0-1 cm 
  ppp<-datadffl[which(datadffl$Station==sta$Station[1]),"Porosity"]
  yinf<-as.numeric(ppp*0.85)
  x0<- -2.01
  a<-1.5
  
  pgr <- yinf+(1-yinf)*exp(-( c(1.5,2.5,4,6) -x0)/a)
  
  pordat<-data.frame(MidDepth=c(1.5,2.5,4,6),Porosity=pgr )
  pordat<-rbind(c(0.5,as.numeric(ppp)),pordat)
  
  mod <- nls( Porosity ~ yinf+(1-yinf)*exp(-(MidDepth-X0)/a),
              data = pordat,
              start = list(yinf=0.3, a = 1,X0=2))
  
  porGrid      <<- setup.prop.1D(xy = matrix(c(pordat$MidDepth,predict(mod,newdata = data.frame(MidDepth=pordat$MidDepth))),ncol=2),
                                 grid = Grid,
                                 interpolate = "linear")
  
  porGridSolid$mid <- 1-porGrid$mid 
  porGridSolid$int <- 1-porGrid$int
  porGridSolid <<- porGridSolid
  
  plot(x=as.numeric(ppp), y= 0.5,  ylim=c(10,0),xlim=c(0,1), xlab="Porosity",ylab="Depth - [cm]")
  lines(x=porGrid$mid , y=Grid$x.mid)
  points(x=pordat$Porosity,y=pordat$MidDepth,pch="x")
  
  fl<-forcingTMdf[station$Station[1],-which(colnames(forcingTMdf)=="Station")]
  parsl[colnames(fl)]<-as.numeric(fl[colnames(fl)]) 
  
  # Accumulation from HAMMOND et al 1999 are in gr/cm²/yr
  # The following gives sediment advection at depth in cm/d
  parsl["w"]<-datadffl[which(datadffl$Station==sta$Station[1]),"Accumulation"]/2.5/365  
  
  return(list("parsl"=parsl,"porGrid"=porGrid,"porGridSolid"=porGridSolid))
}