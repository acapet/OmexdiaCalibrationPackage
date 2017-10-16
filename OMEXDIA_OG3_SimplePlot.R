Simplot<-function(p, plotdata=FALSE, YL=20) {
  
DI<-OCALL(p)

if (plotdata) {
b <- DIA2OBS(DI,b,p)
}

dfvarplot <- as.data.frame(cbind(DI$y,grid=Grid$x.mid,por=porGrid$mid))

p1<-ggplot(  melt(dfvarplot,id.vars = "grid") , aes(y=grid,x=value))+
  geom_path(color='red')+facet_wrap(~variable,nrow=5,ncol=3,scales="free")+theme_light()+
  ylim(YL,0)+ylab(label="depth, cm")

if (plotdata) {
  p1 <- p1 +
  geom_errorbar(data=b,aes(x=value,y=(LowerDepth+UpperDepth)/2,ymin=LowerDepth,ymax=UpperDepth),color='black',size=2)+
  geom_errorbar(data=b,aes(x=modval,y=(LowerDepth+UpperDepth)/2,ymin=LowerDepth,ymax=UpperDepth),color='red',size=2)+
  geom_errorbarh(data=b,aes(x=value,y=(LowerDepth+UpperDepth)/2,xmin=value-err,xmax=value+err),color='black')
}

return(p1)
}