#########################################################################
# Geotectonic diagrams for (ultra-)basic rocks of Agrawal et al. (2008) #
#       based on natural log-transformed ratios  of trace elements      #
#########################################################################

Agrawal<-function(plot.txt=getOption("gcd.plot.text"),GUI=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    plate<-as.list(1:5)
    plate.data<-as.list(1:5)
    plate$nrow<-2
    plate$ncol<-3
    plate$title<-"Geotectonic diagrams for (ultra-)basic rocks - Agrawal (2008)"
    plate$plot.position<-202
    plate$plot.name<-"Agrawal et al. (2008), La, Sm, Yb, Nb, Th based"
    cex.axis<-1.2
    cex.lab<-1.35
    i<-WR[,"SiO2"]<52
    
    if(all(!i)){
        if(GUI)winDialog(type="ok","No data found with SiO2<52 wt.%!")
        options(show.error.messages=FALSE);stop()
    }
    
    ee<-filterOut(WR[i,],c("La", "Sm", "Yb", "Nb", "Th"),1)
    if(nrow(ee)==0){
        if(GUI)winDialog(type="ok","No analyses containing all of La, Sm, Yb, Nb and Th found!")
        options(show.error.messages=FALSE);stop()
    }
    
##############
#  Fig. 1    #
##############

plate.data[[1]]$x<- 0.3518*log(WR[i,"La"]/WR[i,"Th"])+0.6013*log(WR[i,"Sm"]/WR[i,"Th"])-1.3450*log(WR[i,"Yb"]/WR[i,"Th"])+2.1056*log(WR[i,"Nb"]/WR[i,"Th"])-5.4763
plate.data[[1]]$y<- -0.3050*log(WR[i,"La"]/WR[i,"Th"])-1.1801*log(WR[i,"Sm"]/WR[i,"Th"])+1.6189*log(WR[i,"Yb"]/WR[i,"Th"])+1.2260*log(WR[i,"Nb"]/WR[i,"Th"])-0.9944

    temp1<-list(
        axis1=list("axis",side=1,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(-0.12,-1.03),y=c(-8,-0.47),col=plt.col[2]),
        lines2=list("lines",x=c(8,-1.03),y=c(7.32,-0.47),col=plt.col[2]),
        lines3=list("lines",x=c(-8,-1.03),y=c(5.6,-0.47),col=plt.col[2])
        )

    temp2<-list(
        text1=list("text",x=-6,y=-6,text="IAB",cex=1,col=plt.col[2]),
        text2=list("text",x=5,y=2,text="CRB+OIB",cex=1,col=plt.col[2]),
        text3=list("text",x=-5,y=7,text="MORB",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[1]]<-list(demo=list(fun="plot",call=list(xlim=c(-8,8),ylim=c(-8,8),main="",bg="white",fg="black",axes=FALSE,xlab="DF1",ylab="DF2",cex.lab=cex.lab,new=FALSE),template=temp))

##############
#  Fig. 2A   #
##############

plate.data[[2]]$x<- 0.5533*log(WR[i,"La"]/WR[i,"Th"])+0.2173*log(WR[i,"Sm"]/WR[i,"Th"])-0.0969*log(WR[i,"Yb"]/WR[i,"Th"])+2.0454*log(WR[i,"Nb"]/WR[i,"Th"])-5.6305
plate.data[[2]]$y<- -2.4498*log(WR[i,"La"]/WR[i,"Th"])+4.8562*log(WR[i,"Sm"]/WR[i,"Th"])-2.1240*log(WR[i,"Yb"]/WR[i,"Th"])-0.1567*log(WR[i,"Nb"]/WR[i,"Th"])+0.94

    temp1<-list(
        axis1=list("axis",side=1,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(-1.12,-2.34),y=c(0.71,-8),col=plt.col[2]),
        lines2=list("lines",x=c(-1.12,8),y=c(0.71,-2.08),col=plt.col[2]),
        lines3=list("lines",x=c(-1.12,-1.8),y=c(0.71,8),col=plt.col[2])
        )

    temp2<-list(
        text1=list("text",x=-4,y=0,text="IAB",cex=1,col=plt.col[2]),
        text2=list("text",x=3,y=5,text="OIB",cex=1,col=plt.col[2]),
        text3=list("text",x=3,y=-3,text="CRB",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[2]]<-list(demo=list(fun="plot",call=list(xlim=c(-8,8),ylim=c(-8,8),main="",bg="white",fg="black",axes=FALSE,xlab="DF1",ylab="DF2",cex.lab=cex.lab,new=FALSE),template=temp))
    
##############
#  Fig. 2B   #
##############
plate.data[[3]]$x<- 0.3305*log(WR[i,"La"]/WR[i,"Th"])+0.3484*log(WR[i,"Sm"]/WR[i,"Th"])-0.9562*log(WR[i,"Yb"]/WR[i,"Th"])+2.0777*log(WR[i,"Nb"]/WR[i,"Th"])-4.5628
plate.data[[3]]$y<- -0.1928*log(WR[i,"La"]/WR[i,"Th"])-1.1989*log(WR[i,"Sm"]/WR[i,"Th"])+1.7531*log(WR[i,"Yb"]/WR[i,"Th"])+0.6607*log(WR[i,"Nb"]/WR[i,"Th"])-0.4384

    temp1<-list(
        axis1=list("axis",side=1,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(-0.44,-1.22),y=c(-0.39,-8.00),col=plt.col[2]),
        lines2=list("lines",x=c(-0.44,8),y=c(-0.39,3.65),col=plt.col[2]),
        lines3=list("lines",x=c(-0.44,-7.18),y=c(-0.39,8),col=plt.col[2])
        )

    temp2<-list(
        text1=list("text",x=0,y=4,text="MORB",cex=1,col=plt.col[2]),
        text2=list("text",x=-4,y=-4,text="IAB",cex=1,col=plt.col[2]),
        text3=list("text",x=4,y=-4,text="CRB",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[3]]<-list(demo=list(fun="plot",call=list(xlim=c(-8,8),ylim=c(-8,8),main="",bg="white",fg="black",axes=FALSE,xlab="DF1",ylab="DF2",cex.lab=cex.lab,new=FALSE),template=temp))
    
##############
#  Fig. 2C   #
##############

plate.data[[4]]$x<- 1.7517*log(WR[i,"Sm"]/WR[i,"Th"])-1.9508*log(WR[i,"Yb"]/WR[i,"Th"])+1.9573*log(WR[i,"Nb"]/WR[i,"Th"])-5.0928
plate.data[[4]]$y<- -2.2412*log(WR[i,"Sm"]/WR[i,"Th"])+2.2060*log(WR[i,"Yb"]/WR[i,"Th"])+1.2481*log(WR[i,"Nb"]/WR[i,"Th"])-0.8243

    temp1<-list(
        axis1=list("axis",side=1,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(-0.36,0.03),y=c(-0.78,-8),col=plt.col[2]),
        lines2=list("lines",x=c(-0.36,8),y=c(-0.78,7.03),col=plt.col[2]),
        lines3=list("lines",x=c(-0.36,-8),y=c(-0.78,6.06),col=plt.col[2])
        )

    temp2<-list(
        text1=list("text",x=0,y=4,text="MORB",cex=1,col=plt.col[2]),
        text2=list("text",x=-4,y=-4,text="IAB",cex=1,col=plt.col[2]),
        text3=list("text",x=4,y=-4,text="OIB",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[4]]<-list(demo=list(fun="plot",call=list(xlim=c(-8,8),ylim=c(-8,8),main="",bg="white",fg="black",axes=FALSE,xlab="DF1",ylab="DF2",cex.lab=cex.lab,new=FALSE),template=temp))
    
##############
#  Fig. 2D   #
##############

plate.data[[5]]$x<- -0.5558*log(WR[i,"La"]/WR[i,"Th"])-1.4260*log(WR[i,"Sm"]/WR[i,"Th"])+2.2935*log(WR[i,"Yb"]/WR[i,"Th"])-0.6890*log(WR[i,"Nb"]/WR[i,"Th"])+4.1422
plate.data[[5]]$y<- -0.9207*log(WR[i,"La"]/WR[i,"Th"])+3.6520*log(WR[i,"Sm"]/WR[i,"Th"])-1.9866*log(WR[i,"Yb"]/WR[i,"Th"])+1.0574*log(WR[i,"Nb"]/WR[i,"Th"])-4.4283

    temp1<-list(
        axis1=list("axis",side=1,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(0.67,-8),y=c(0.48,-2.06),col=plt.col[2]),
        lines2=list("lines",x=c(.67,1.58),y=c(0.48,8),col=plt.col[2]),
        lines3=list("lines",x=c(.67,2.31),y=c(0.48,-8),col=plt.col[2])
        )

    temp2<-list(
        text1=list("text",x=-4,y=4,text="OIB",cex=1,col=plt.col[2]),
        text2=list("text",x=-4,y=-4,text="CRB",cex=1,col=plt.col[2]),
        text3=list("text",x=4,y=0,text="MORB",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[5]]<-list(demo=list(fun="plot",call=list(xlim=c(-8,8),ylim=c(-8,8),main="",bg="white",fg="black",axes=FALSE,xlab="DF1",ylab="DF2",cex.lab=cex.lab,new=FALSE),template=temp))

    assign("plate",plate,.GlobalEnv)
    assign("plate.data",plate.data,.GlobalEnv)
}

# Plotting position 202
# "Agrawal et al. (2008), La, Sm, Yb, Nb, Th based","plotPlate(\"Agrawal\",getOption(\"gcd.plot.text\"))"
