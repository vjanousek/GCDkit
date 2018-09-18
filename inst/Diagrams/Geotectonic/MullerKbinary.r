#####################################################################
#             Geochemical Discrimination Between Shoshonitic        #
#                and Potassic Volcanic Rocks in Different           #
#                            Tectonic Settings                      #
#                            Muller et al.(1992)                    #
#####################################################################

MullerKbinary<-function(plot.txt=getOption("gcd.plot.text")){
    plate<-as.list(1:6)
    plate.data<-as.list(1:6)
    plate$nrow<-3
    plate$ncol<-2
    plate$title<-"Geotectonic classification of potassic rocks - Muller et al. (1992)"
    plate$plot.position<-304
    plate$plot.name<-"Mueller et al. (1992) Potassic rocks binary"
    cex.axis<-1.2
    cex.lab<-1.35

############
# Plot 1a   #
############
    plate.data[[1]]$x<-"TiO2/Al2O3"
    plate.data[[1]]$y<-"Zr/Al2O3"

    line1x<-c(-1.333,-0.675)
    line1y<-c(1.716,0.920)
    line2x<-c(-1.665,-1.230,-0.681)
    line2y<-c(0.855,0.743,0.367)

    temp1<-list(
        axis1=list("axis",side=1,at=c(0.01,0.1,0.8),labels=c(0.01,0.1,0.8),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=10^(0:2),labels=10^(0:2),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        rug1=list("rug",x=c(seq(0.01,0.1,by=0.01),seq(0.1,1,by=0.1)),ticksize=0.03,side=1,lwd=0.5,col="black"),        
        rug2=list("rug",x=c(1:10,seq(10,100,by=10)),ticksize=0.03,side=2,lwd=0.5,col="black"),
        lines1=list("lines",x=10^line1x,y=10^line1y,col=plt.col[2],lty="dashed"),
        lines2=list("lines",x=10^line2x,y=10^line2y,col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="TiO2/Al2O3 - Zr/Al2O3 (Muller et al.1992)")
   )
    temp2<-list(
        text1=list("text",x=0.07,y=60,text="WIP",cex=1,col=plt.col[2]),
        text2=list("text",x=0.025,y=20,text="CAP+\nPAP",cex=1,col=plt.col[2]),
        text3=list("text",x=0.025,y=4,text="IOP+\nLOP ",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[1]]<-list(demo=list(fun="plot",call=list(xlim=c(0.01,0.8),ylim=c(1,110),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab=annotate("TiO2/Al2O3"),ylab=annotate("Zr/Al2O3"),cex.lab=cex.lab,new=FALSE),template=temp))

############
# Plot 1b   #
############
    plate.data[[2]]$x<-"TiO2/Al2O3"
    plate.data[[2]]$y<-"Zr/Al2O3"

    line1x<-c(0.03,0.053,0.1)
    line1y<-c(6.85,5.85,3.6)
    
    temp1<-list(
        axis1=list("axis",side=1,at=c(0.03,0.05,0.1),labels=c(0.03,0.05,0.1),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(3,10,20),labels=c(3,10,20),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        rug1=list("rug",x=seq(0.03,0.1,by=0.01),ticksize=0.03,side=1,lwd=0.5,col="black"),        
        rug2=list("rug",x=3:20,ticksize=0.03,side=2,lwd=0.5,col="black"),
        lines1=list("lines",x=line1x,y=line1y,col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="TiO2/Al2O3 - Zr/Al2O3 (Muller et al.1992)") 
    )
  
    temp2<-list(
        text1=list("text",x=0.032,y=12,text="CAP+\nPAP",cex=1,adj=0,col=plt.col[2]),
        text2=list("text",x=0.032,y=5,text="IOP+\nLOP",cex=1,adj=0,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[2]]<-list(demo=list(fun="plot",call=list(xlim=c(0.03,0.1),ylim=c(3,20),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab=annotate("TiO2/Al2O3"),ylab=annotate("Zr/Al2O3"),cex.lab=cex.lab,new=FALSE),template=temp))


############
# Plot 2   #
############
    plate.data[[3]]$x<-"P2O5/Al2O3"
    plate.data[[3]]$y<-"Zr/Al2O3"
    
    line1x<-c(-2.130,-1.734)
    line1y<-c(0.913,0.131)
  
    temp1<-list(
        axis1=list("axis",side=1,at=c(0.005,0.01,0.02,0.05),labels=c(0.005,0.01,0.02,0.05),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(1,2,5,10,20),labels=c(1,2,5,10,20),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        rug1=list("rug",x=c(seq(0.005,0.01,by=0.001),seq(0.01,0.1,by=0.01),seq(0.1,1,by=0.1)),ticksize=0.03,side=1,lwd=0.5,col="black"),        
        rug2=list("rug",x=1:20,ticksize=0.03,side=2,lwd=0.5,col="black"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=10^line1x,y=10^line1y,col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="P2O5/Al2O3 - Zr/Al2O3 (Muller et al.1992)")
   )
  
    temp2<-list(
        text1=list("text",x=0.020,y=10,text="LOP",cex=1,col=plt.col[2]),
        text2=list("text",x=0.009,y=2,text="IOP",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[3]]<-list(demo=list(fun="plot",call=list(xlim=c(0.005,0.08),ylim=c(1,25),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab=annotate("P2O5/Al2O3"),ylab=annotate("Zr/Al2O3"),cex.lab=cex.lab,new=FALSE),template=temp))   

############
# Plot 3   #
############
    plate.data[[4]]$x<-"Zr/TiO2"
    plate.data[[4]]$y<-"Ce/P2O5"
    
    line1x<-c(1.918,2.519)
    line1y<-c(2.450,1.655)
    temp1<-list(
        axis1=list("axis",side=1,at=c(50,100,200,400),labels=c(50,100,200,400),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(20,50,100,200,1000),labels=c(20,50,100,200,1000),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        rug1=list("rug",x=seq(100,1000,100),ticksize=0.03,side=1,lwd=0.5,col="black"),        
        rug2=list("rug",x=seq(100,1000,100),ticksize=0.03,side=2,lwd=0.5,col="black"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=10^line1x,y=10^line1y,col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="Zr/TiO2 - Ce/P2O5 (Muller et al.1992)")
   )
  
    temp2<-list(
        text1=list("text",x=180,y=500,text="CAP",cex=1,col=plt.col[2]),
        text2=list("text",x=70,y=100,text="PAP",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[4]]<-list(demo=list(fun="plot",call=list(xlim=c(40,1200),ylim=c(20,800),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab=annotate("Zr/TiO2"),ylab=annotate("Ce/P2O5"),cex.lab=cex.lab,new=FALSE),template=temp))

############
# Plot 4   #
############
    plate.data[[5]]$x<-"Al2O3"
    plate.data[[5]]$y<-"TiO2"
    
   temp1<-list(
        axis1=list("axis",side=1,at=seq(6,20,by=2),labels=seq(6,20,by=2),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=0:6,labels=0:6,cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(6,20),y=c(0,2.628),col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="Al2O3 - TiO2 (Muller et al.1992)")
   )
  
    temp2<-list(
        text1=list("text",x=10,y=3,text="WIP",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[5]]<-list(demo=list(fun="plot",call=list(xlim=c(6,20),ylim=c(0,6),main="",bg="white",fg="black",axes=FALSE,log="",xlab=annotate("Al2O3"),ylab=annotate("TiO2"),cex.lab=cex.lab,new=FALSE),template=temp))
    
############
# Plot 5   #
############
    plate.data[[6]]$x<-"Zr"
    plate.data[[6]]$y<-"Y"    
    
    line1x<-c(2.412,2.813)
    line1y<-c(2,0.698)
    temp1<-list(
        axis1=list("axis",side=1,at=c(10,20,50,100,200,500,1000),labels=c(10,20,50,100,200,500,1000),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(10,20,50,100),labels=c(10,20,50,100),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        rug1=list("rug",x=c(seq(10,100,by=10),seq(100,1000,100)),ticksize=0.03,side=1,lwd=0.5,col="black"),        
        rug2=list("rug",x=seq(10,100,by=10),ticksize=0.03,side=2,lwd=0.5,col="black"),box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=10^line1x,y=10^line1y,col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="Zr - Y (Muller et al.1992)")
   )
  
    temp2<-list(
        text1=list("text",x=180,y=500,text="CAP",cex=1,col=plt.col[2]),
        text2=list("text",x=600,y=50,text="WIP",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
   
    plate[[6]]<-list(demo=list(fun="plot",call=list(xlim=c(10,1000),ylim=c(5,100),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab=annotate("Zr"),ylab=annotate("Y"),cex.lab=cex.lab,new=FALSE),template=temp))
    
    assign("plate",plate,.GlobalEnv)
    assign("plate.data",plate.data,.GlobalEnv)
    
}


# Plotting position 304 BINARY PLOTS
# "Mueller et al. (1992) Potassic rocks binary","plotPlate(\"MullerKbinary\")"
