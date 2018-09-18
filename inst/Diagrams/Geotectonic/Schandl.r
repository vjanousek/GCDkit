#####################################################################
#                   Geotectonic classification of                   #
#      volcanic rocks (volcanogenic massive sulphide deposits)      #
#                      Schandl and Gorton(2002)                     #
#####################################################################

Schandl<-function(plot.txt=getOption("gcd.plot.text")){
    plate<-as.list(1:4)
    plate.data<-as.list(1:4)
    plate$nrow<-2
    plate$ncol<-2
    plate$title<-"Geotectonic classification of volcanic rocks - Schandl and Gorton (2002)"
    plate$plot.position<-108
    plate$plot.name<-"Schandl + Gorton (2002)"
    cex.axis<-1.2
    cex.lab<-1.35

############
# Plot 1   #
############

    plate.data[[1]]$x<-"Ta/Yb"
    plate.data[[1]]$y<-"Th/Yb"
    a<-(log10(90)-log10(0.6))/(log10(100)-log10(1))
    
    temp1<-list(
        axis1=list("axis",side=1,at=c(0.01,0.1,1,10,100),labels=c("0.01","0.1","1","10","100"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(0.01,0.1,1,10,100),labels=c("0.01","0.1","1","10","100"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("abline",a=log10(0.6)-a*log10(1),b=a,col=plt.col[2],lty="dashed"),
        lines2=list("abline",a=log10(4)-a*log10(2),b=a,col=plt.col[2],lty="dashed"),
        lines3=list("abline",a=log10(7)-a*log10(1),b=a,col=plt.col[2],lty="dashed"),
        lines4=list("abline",a=log10(0.8)-a*log10(0.04),b=a,col=plt.col[2],lty="dashed"),
        lines5=list("abline",a=log10(2)-a*log10(0.03),b=a,col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="Ta/Yb - Th/Yb (Schandl + Gorton 2002)")
   )

    temp2<-list(
        text1=list("text",x=10.5,y=15,text="WPB",cex=1,col=plt.col[2]),
        text2=list("text",x=0.08,y=0.07,text="MORB",cex=1,col=plt.col[2]),
        text3=list("text",x=2.5,y=10,text="WPVZ",cex=1,srt=45,col=plt.col[2]),
        text4=list("text",x=0.7,y=10,text="ACM",cex=1,srt=45,col=plt.col[2]),
        text5=list("text",x=0.2,y=10,text="Oceanic Arcs",cex=1,srt=45,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[1]]<-list(demo=list(fun="plot",call=list(xlim=c(0.01,100),ylim=c(0.01,100),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab="Ta/Yb",ylab="Th/Yb",cex.lab=cex.lab,new=FALSE),template=temp))



############
# Plot 2   #
############

    plate.data[[2]]$x<-"Ta"
    plate.data[[2]]$y<-"Th"
    
    temp1<-list(
        axis1=list("axis",side=1,at=c(0,1,2,3,4,5,6),labels=c("0","1","2","3","4","5","6"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(0,5,10,15,20,25),labels=c("0","5","10","15","20","25"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("abline",a=0,b=5,col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="Ta - Th (Schandl + Gorton 2002)")
   )

    temp2<-list(
        text1=list("text",x=2,y=20,text="Active Continental Margin",cex=1,col=plt.col[2]),
        text2=list("text",x=4,y=5,text="Within Plate\nVolcanic Zones",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[2]]<-list(demo=list(fun="plot",call=list(xlim=c(0,6),ylim=c(0,25),main="",bg="white",fg="black",axes=FALSE,xlab="Ta",ylab="Th",cex.lab=cex.lab,new=FALSE),template=temp))


############
# Plot 3   #
############


    plate.data[[3]]$x<-"Ta/Hf"
    plate.data[[3]]$y<-"Th/Hf"
    
    temp1<-list(
        axis1=list("axis",side=1,at=c(0,0.1,0.2,0.3,0.4,0.5,0.6),labels=c("0","0.1","0.2","0.3","0.4","0.5","0.6"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(0,1,2,3,4,5,6),labels=c("0","1","2","3","4","5","6"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("abline",a=0,b=2/0.3,col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="Ta/Hf - Th/Hf (Schandl + Gorton 2002)")
   )

    temp2<-list(
        text1=list("text",x=0.1,y=4,text="Active Continental Margin",cex=1,col=plt.col[2],adj=0),
        text2=list("text",x=0.5,y=1.5,text="Within Plate\nVolcanic Zones",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[3]]<-list(demo=list(fun="plot",call=list(xlim=c(0,0.6),ylim=c(0,6),main="",bg="white",fg="black",axes=FALSE,xlab="Ta/Hf",ylab="Th/Hf",cex.lab=cex.lab,new=FALSE),template=temp))


############
# Plot 4   #
############

    plate.data[[4]]$x<-"Yb"
    plate.data[[4]]$y<-"Th/Ta"
    
    temp1<-list(
        axis1=list("axis",side=1,at=c(0,10,20,30,40),labels=c("0","10","20","30","40"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(0,10,20,30,35),labels=c("0","10","20","30","35"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("abline",h=1.2,col=plt.col[2],lty="dashed"),
        lines2=list("abline",h=6,col=plt.col[2],lty="dashed"),
        lines3=list("abline",h=23,col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="Yb - Th/Ta (Schandl + Gorton 2002)")
   )

    temp2<-list(
        text1=list("text",x=2,y=0.8,text="MORB",cex=1,col=plt.col[2],adj=0),
        text2=list("text",x=2,y=4,text="Within Plate Volcanic Zones",cex=1,col=plt.col[2],adj=0),
        text2=list("text",x=2,y=13,text="Active Continental Margins",cex=1,col=plt.col[2],adj=0),
        text2=list("text",x=2,y=25,text="Oceanic Arcs",cex=1,col=plt.col[2],adj=0)
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[4]]<-list(demo=list(fun="plot",call=list(xlim=c(0,40),ylim=c(0,35),main="",bg="white",fg="black",axes=FALSE,xlab="Yb",ylab="Th/Ta",cex.lab=cex.lab,new=FALSE),template=temp))

    assign("plate",plate,.GlobalEnv)
    assign("plate.data",plate.data,.GlobalEnv)
}

# Plotting position 18
# "Schandl + Gorton (2002)","plotPlate(\"Schandl\")"
