fittableplot<-function(IN) {
  
  if (class(IN)=="modCost"){
    fdf<-data.frame("SSR"=IN$var$SSR)
    rownames(fdf)<-IN$var$name
    fdf["ModelSSR",]<-IN$model
  } else { # assuming modFit class
    fdf<-data.frame("MSR"=IN$var_ms)
    fdf["ModelSSR",]<-IN$ssr
  }
    
  t<-qplot(1:10, 1:10, geom = "blank")+theme_bw()+
    theme(line = element_blank(),text = element_blank())+
    annotation_custom(grob = tableGrob(format(fdf,digits = 3,nsmall=0,scientific=F,drop0trailing=T))) 
  
return(t)
}