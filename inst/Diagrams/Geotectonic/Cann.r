#####################################################################
#                      Classification of basalts                    #
#       Pearce and Cann 1973; coordinates from Rollison 1993        #
#####################################################################

Cann<-function(plot.txt=getOption("gcd.plot.text")){
    plate<-as.list(1:3)
    plate.data<-as.list(1:3)
    plate$nrow<-1
    plate$ncol<-3
    plate$title<-"Basalt tectonic discrimination - Pearce and Cann (1973)"
    plate$plot.position<-205
    plate$plot.name<-"Pearce + Cann (1973)"
    cex.axis<-1.2
    cex.lab<-1.35


############
# Plot 1   #
############

what<-WR[,"Zr"]>0 & WR[,"Y"]>0 & WR[,"Ti"]>0
what<-names(what[!is.na(what)])

aa<-WR[what,"Zr"]
bb<-WR[what,"Ti"]/100
cc<-WR[what,"Y"]*3
suma<-aa+bb+cc
aa<-aa/suma
bb<-bb/suma
cc<-cc/suma
plate.data[[1]]$x<-cc+bb/2
plate.data[[1]]$y<-sqrt(3)*bb/2
                   
temp1<-list(
    lines1=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
    lines2=list("lines",x=c(0.325,0.27,0.365,0.52,0.6,0.59,0.505,0.4,0.325,0.29,0.325,0.4025,0.4875,0.52),
        y=c(0.20785,0.24249,0.43301,0.41569,0.33775,0.25981,0.16454,0.0866,0.0866,0.13856,0.20785,0.25548,0.36806,0.41569),col=plt.col[2],lty="dashed"),
    lines3=list("lines",x=c(0.505,0.4025),y=c(0.16454,0.25548),col=plt.col[2],lty="dashed"),
    lines4=list("lines",x=c(0.59,0.4875),y=c(0.25981,0.36806),col=plt.col[2],lty="dashed"),
    A=list("text",x=0,y=-0.03,text="Zr",adj=0.5),
    C=list("text",x=1,y=-0.03,text="3*Y",adj=0.5),
    B=list("text",x=0.5,y=sqrt(3)/2+.03,text="Ti/100",adj=0.5),
    GCDkit=list("NULL",plot.type="ternary",plot.name="Zr - Ti/100 - Y*3 (Pearce and Cann 1973)")
)

temp2<-list(
    text4=list("text",x=0.537,y=0.362,text="IAT",cex=1,col=plt.col[2]),
    text5=list("text",x=0.5,y=0.250,text="MORB,\nCAB,\nIAT",cex=1,col=plt.col[2]),
    text6=list("text",x=0.392,y=0.166,text="CAB",cex=1,col=plt.col[2]),
    text7=list("text",x=0.39,y=0.334,text="WPB",cex=1,col=plt.col[2])
)
    
if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

plate[[1]]<-list(demo=list(fun="plot",call=list(xlim=c(-0.03,1.03),ylim=c(-0.05,1.03),main="",bg="white",fg="black",asp=1,axes=FALSE,xlab="",ylab="",cex.lab=cex.lab,new=FALSE),template=temp))

############
# Plot 2   #
############

what<-WR[,"Zr"]>0 & WR[,"Sr"]>0 & WR[,"Ti"]>0
what<-names(what[!is.na(what)])

aa<-WR[what,"Zr"]
bb<-WR[what,"Ti"]/100
cc<-WR[what,"Sr"]/2
suma<-aa+bb+cc
aa<-aa/suma
bb<-bb/suma
cc<-cc/suma
plate.data[[2]]$x<-cc+bb/2
plate.data[[2]]$y<-sqrt(3)*bb/2
                 
temp1<-list(
    lines1=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
    lines2=list("lines",x=c(0.345,0.295,0.345,0.445,0.525,0.675,0.7875,0.825,0.4225,0.345,0.53,0.565,0.525),
        y=c(0.20785,0.26847,0.34641,0.38971,0.35507,0.29445,0.17754,0.0433,0.08227,0.20785,0.20785,0.26847,0.35507),col=plt.col[2],lty="dashed"),
    lines3=list("lines",x=c(0.825,0.711,0.53),
        y=c(0.0433,0.15762,0.20785),col=plt.col[2],lty="dashed"),
    A=list("text",x=0,y=-0.03,text="Zr",adj=0.5),
    C=list("text",x=1,y=-0.03,text="Sr/2",adj=0.5),
    B=list("text",x=0.5,y=sqrt(3)/2+.03,text="Ti/100",adj=0.5),
    GCDkit=list("NULL",plot.type="ternary",plot.name="Zr - Ti/100 - Sr/2 (Pearce and Cann 1973)")
)

temp2<-list(
    text1=list("text",x=0.669,y=0.202,text="IAT",cex=1,col=plt.col[2]),
    text2=list("text",x=0.55,y=0.13,text="CAB",cex=1,col=plt.col[2]),
    text3=list("text",x=0.434,y=0.291,text="MORB",cex=1,col=plt.col[2])
)
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
plate[[2]]<-list(demo=list(fun="plot",call=list(xlim=c(-0.03,1.03),ylim=c(-0.05,1.03),main="",bg="white",fg="black",asp=1,axes=FALSE,xlab="",ylab="",cex.lab=cex.lab,new=FALSE),template=temp))


############
# Plot 3   #
############

    plate.data[[3]]$x<-"Zr"
    plate.data[[3]]$y<-"Ti"

    temp1<-list(
        axis1=list("axis",side=1,at=c(0,50,100,150,200,250),labels=c("0","50","100","150","200","250"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(0,5000,10000,15000),labels=c("0","5000","10000","15000"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(12,18,52,78,89,113,135),y=c(1700,4600,8500,6400,7400,7400,7100),col=plt.col[2],lty="dashed"),
        lines2=list("lines",x=c(78,39,31,42,84,131,167,135,192,190,113,65,12),y=c(6400,3100,4100,6000,10400,13000,10900,7100,6600,3400,2400,2400,1700),col=plt.col[2],lty="dashed"),
        lines3=list("lines",x=c(65,65),y=c(2400,5400),col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="Zr - Ti (Pearce and Cann 1973)")
     )

    temp2<-list(
        text1=list("text",x=23.75,y=3042,text="IAT",cex=1,col=plt.col[2]),
        text2=list("text",x=42,y=5700,text="   MORB,\n  CAB,\nIAT",adj=0,cex=1,col=plt.col[2]),
        text3=list("text",x=122,y=4850,text="CAB",cex=1,col=plt.col[2]),
        text4=list("text",x=115,y=9714,text="MORB",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[3]]<-list(demo=list(fun="plot",call=list(xlim=c(0,250),ylim=c(0,18000),main="",bg="white",fg="black",axes=FALSE,xlab="Zr (ppm)",ylab="Ti (ppm)",cex.lab=cex.lab,new=FALSE),template=temp))    
    
    assign("plate",plate,.GlobalEnv)
    assign("plate.data",plate.data,.GlobalEnv)
}

# Plotting position 205
# "Pearce + Cann (1973)","plotPlate(\"Cann\")"
