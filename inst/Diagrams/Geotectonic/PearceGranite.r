#####################################################################
#                Geotectonic classification of granitoids           #
#                          Pearce et al. 1984                       #
#####################################################################
PearceGranite<-function(plot.txt=getOption("gcd.plot.text")){
    plate<-as.list(1:4)
    plate.data<-as.list(1:4)
    plate$nrow<-2
    plate$ncol<-2
    plate$title<-"Granite tectonic discrimination - Pearce et al. (1984)"
    plate$plot.name<-"Pearce et al. (1984)"
    plate$plot.position<-105
    cex.axis<-1.2
    cex.lab<-1.35
    
    ############
    # Plot 1   #
    ############
    plate.data[[1]]$x<-"Y+Nb"
    plate.data[[1]]$y<-"Rb"
    
    temp1<-list(
        axis1=list("axis",side=1,at=c(1,10,100,1000),labels=c("1","10","100","1000"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(1,10,100,1000),labels=c("1","10","100","1000"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(2,55),y=c(80,300),col=plt.col[2]),
        lines2=list("lines",x=c(55,400),y=c(300,2000),col=plt.col[2]),
        lines3=list("lines",x=c(55,51.5),y=c(300,8),col=plt.col[2]),
        lines4=list("lines",x=c(51.5,50),y=c(8,1),col=plt.col[2]),
        lines5=list("lines",x=c(51.5,2000),y=c(8,400),col=plt.col[2]),
        GCDkit=list("NULL",plot.type="binary",plot.name="Y+Nb - Rb (Pearce et al. 1984)")
    )

    temp2<-list(
        text1=list("text",x=500,y=2,text="ORG",cex=1,col=plt.col[2]),
        text2=list("text",x=5,y=2,text="VAG",cex=1,col=plt.col[2]),
        text3=list("text",x=500,y=500,text="WPG",cex=1,col=plt.col[2]),
        text4=list("text",x=5,y=500,text="syn-COLG",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[1]]<-list(demo=list(fun="plot",call=list(xlim=c(1,2000),ylim=c(1,2000),main="",bg="white",fg="black",asp=1,axes=FALSE,log="xy",xlab="Y+Nb",ylab="Rb",cex.lab=cex.lab,new=FALSE),template=temp))
    
    ############
    # Plot 2   #
    ############
    plate.data[[2]]$x<-"Y"
    plate.data[[2]]$y<-"Nb"

    temp1<-list(
        axis1=list("axis",side=1,labels=c("1","10","100","1000"),at=c(1,10,100,1000),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,abels=c("1","10","100","1000"),at=c(1,10,100,1000),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),lines1=list("lines",x=c(1,50),y=c(200,10),col=plt.col[2]),
        lines2=list("lines",x=c(40,50),y=c(1,10),col=plt.col[2]),
        lines3=list("lines",x=c(50,1000),y=c(10,100),col=plt.col[2]),
        lines4=list("lines",x=c(25,1000),y=c(25,400),col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="Y - Nb (Pearce et al. 1984)")
    )
    
    temp2<-list(
         text1=list("text",x=800,y=3,text="ORG",cex=1,col=plt.col[2]),
         text2=list("text",x=5,y=25,text="VAG+\nsyn-COLG",cex=1,col=plt.col[2]),
         text3=list("text",x=20,y=200,text="WPG",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[2]]<-list(demo=list(fun="plot",call=list(xlim=c(1,2000),ylim=c(1,2000),main="",bg="white",fg="black",asp=1,axes=FALSE,log="xy",xlab="Y",ylab="Nb",cex.lab=cex.lab,new=FALSE),template=temp))
    
    ############
    # Plot 3   #
    ############
    plate.data[[3]]$x<-"Yb+Ta"
    plate.data[[3]]$y<-"Rb"
 
    temp1<-list(
        axis1=list("axis",side=1,labels=c("1","10","100"),at=c(1,10,100),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,labels=c("0.1","1","10","100","1000"),at=c(0.1,1,10,100,1000),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),lines1=list("lines",x=c(0.5,6),y=c(140,200),col=plt.col[2]),
        lines2=list("lines",x=c(6,50),y=c(200,2000),col=plt.col[2]),
        lines3=list("lines",x=c(6,6),y=c(200,8),col=plt.col[2]),
        lines4=list("lines",x=c(6,6),y=c(8,1),col=plt.col[2]),
        lines5=list("lines",x=c(6,200),y=c(8,400),col=plt.col[2]),
        GCDkit=list("NULL",plot.type="binary",plot.name="Yb+Ta - Rb (Pearce et al. 1984)")
    )
    
    temp2<-list(
        text1=list("text",x=1.8,y=800,text="syn-COLG",cex=1,col=plt.col[2]),
        text2=list("text",x=50,y=800,text="WPG",cex=1,col=plt.col[2]),
        text3=list("text",x=1.8,y=5,text="VAG",cex=1,col=plt.col[2]),
        text4=list("text",x=50,y=5,text="ORG",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[3]]<-list(demo=list(fun="plot",call=list(xlim=c(0.5,200),ylim=c(1,2000),main="",bg="white",fg="black",asp=1,axes=FALSE,log="xy",xlab="Ta+Yb",ylab="Rb",cex.lab=cex.lab,new=FALSE),template=temp))
    
    ############
    # Plot 4   #
    ############
    plate.data[[4]]$x<-"Yb"
    plate.data[[4]]$y<-"Ta"
 
    temp1<-list(
        axis1=list("axis",side=1,labels=c("0.1","1","10","100"),at=c(0.1,1,10,100),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,labels=c("0.1","1","10","100"),at=c(0.1,1,10,100),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),lines1=list("lines",x=c(0.55,3),y=c(20,2),col=plt.col[2]),
        lines2=list("lines",x=c(0.1,3),y=c(0.35,2),col=plt.col[2]),
        lines3=list("lines",x=c(3,5),y=c(2,1),col=plt.col[2]),
        lines4=list("lines",x=c(5,5),y=c(0.05,1),col=plt.col[2]),
        lines5=list("lines",x=c(5,100),y=c(1,7),col=plt.col[2]),
        lines6=list("lines",x=c(3,100),y=c(2,20),col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="Yb - Ta (Pearce et al. 1984)")
    )  
    
    temp2<-list(
        text1=list("text",x=0.4,y=4,text="syn-COLG",cex=1,col=plt.col[2]),
        text2=list("text",x=5,y=15,text="WPG",cex=1,col=plt.col[2]),
        text3=list("text",x=0.5,y=0.3,text="VAG",cex=1,col=plt.col[2]),
        text4=list("text",x=20,y=0.3,text="ORG",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[4]]<-list(demo=list(fun="plot",call=list(xlim=c(0.1,100),ylim=c(0.05,100),main="",bg="white",fg="black",asp=1,axes=FALSE,log="xy",xlab="Yb",ylab="Ta",cex.lab=cex.lab,new=FALSE),template=temp))

    assign("plate",plate,.GlobalEnv)
    assign("plate.data",plate.data,.GlobalEnv)
}

# Plotting position 105
# "Pearce et al. (1984)","plotPlate(\"PearceGranite\")"
        
