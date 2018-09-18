#####################################################################
#             Major-element base classification of granitoids       #
#                        Maniar and Piccoli 1989                    #
#####################################################################
Maniar<-function(plot.txt=getOption("gcd.plot.text")){
    plate<-as.list(1:6)
    plate.data<-as.list(1:6)
    plate$nrow<-2
    plate$ncol<-3
    plate$title<-"Granite tectonic discrimination - Maniar and Piccoli (1989)"
    plate$plot.position<-102
    plate$plot.name<-"Maniar + Piccoli (1989)"
    cex.axis<-1.2
    cex.lab<-1.35

#############
# Plot Si-K #
#############
plate.data[[1]]$x<-"SiO2"
plate.data[[1]]$y<-"K2O"
temp1<-list(
        axis1=list("axis",side=1,at=c(60,65,70,75,80),labels=c("60","65","70","75","80"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(0,1,2,3,4,5,6,7),labels=c("0","1","2","3","4","5","6","7"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(50,90),y=c(1,1),col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="SiO2 - K2O (Maniar + Piccoli 1989)")
    )

    temp2<-list(
        text1=list("text",x=61,y=1.5,text="IAG+CAG+CCG+RRG+CEUG+POG",cex=1,col=plt.col[2],adj=0),
        text2=list("text",x=61,y=.3,text="OP",cex=1,col=plt.col[2],adj=0)
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[1]]<-list(demo=list(fun="plot",call=list(xlim=c(60,80),ylim=c(0,7),main="",bg="white",fg="black",axes=FALSE,xlab=annotate("SiO2"),ylab=annotate("K2O"),cex.lab=cex.lab,new=FALSE),template=temp))
    
##############
# Plot Si-Al #
##############
plate.data[[2]]$x<-"SiO2"
plate.data[[2]]$y<-"Al2O3"
temp1<-list(
        axis1=list("axis",side=1,at=c(70,72,74,76,78,80),labels=c("70","72","74","76","78","80"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(10,11,12,13,14,15,16,17),labels=c("10","11","12","13","14","15","16","17"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(60,90),y=c(16.4,9.95),col=plt.col[2],lty="dashed"),
        lines2=list("lines",x=c(70.5,70.5,79.5,79.5,70.5),y=c(13.1,15.2,12.9,10.6,13.1),col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="SiO2 - Al2O3 (Maniar + Piccoli 1989)")
    )

    temp2<-list(
        text1=list("text",x=75,y=16,text="IAG+CAG+CCG",cex=1,col=plt.col[2]),
        text2=list("text",x=75,y=10.5,text="RRG+CEUG",cex=1,col=plt.col[2]),
        text3=list("text",x=78,y=12.5,text="POG",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[2]]<-list(demo=list(fun="plot",call=list(xlim=c(70,80),ylim=c(10,17),main="",bg="white",fg="black",axes=FALSE,xlab=annotate("SiO2"),ylab=annotate("Al2O3"),cex.lab=cex.lab,new=FALSE),template=temp))


############
# Plot 3   #
############
plate.data[[3]]$x<-"SiO2"
plate.data[[3]]$y<-"FeOt/(FeOt+MgO)"
temp1<-list(
        axis1=list("axis",side=1,at=c(60,65,70,75,80),labels=c("60","65","70","75","80"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(0.5,0.6,0.7,0.8,0.9,1),labels=c("0.5","0.6","0.7","0.8","0.9","1"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(60,80),y=c(0.8,0.9),col=plt.col[2],lty="dashed"),
        lines1=list("lines",x=c(68,76,79.5,75,68),y=c(0.78,0.975,0.875,0.75,0.78),col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="SiO2 - FeOt/(FeOt+MgO) (Maniar + Piccoli 1989)")
    )

    temp2<-list(
        text1=list("text",x=61,y=.55,text="IAG+CAG+CCG",cex=1,col=plt.col[2],adj=0),
        text2=list("text",x=76,y=.75,text="POG",cex=1,col=plt.col[2]),
        text3=list("text",x=61,y=.95,text="RRG+CEUG",cex=1,col=plt.col[2],adj=0)
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[3]]<-list(demo=list(fun="plot",call=list(xlim=c(60,80),ylim=c(0.5,1),main="",bg="white",fg="black",axes=FALSE,xlab=annotate("SiO2"),ylab=annotate("FeO[t]/(FeO[t]+MgO)"),cex.lab=cex.lab,new=FALSE),template=temp))
    


############
# Plot AFM #
############

plate.data[[4]]$x<-"100*MgO/(Al2O3+Na2O+K2O+FeOt+MgO)"
plate.data[[4]]$y<-"100*FeOt/(Al2O3+Na2O+K2O+FeOt+MgO)"
temp1<-list(
        axis1=list("axis",side=1,at=c(0,5,10,15,20,25,30),labels=c("0","5","10","15","20","25","30"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(0,10,20,30,40,50,60),labels=c("0","10","20","30","40","50","60"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(0,13),y=c(20,38),col=plt.col[2],lty="dashed"),
        lines2=list("lines",x=c(1,2,10,3.5,1),y=c(12,45,32,7,12),col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="AFM (Maniar + Piccoli 1989)")
    )

    temp2<-list(
        text1=list("text",x=15,y=2,text="IAG+CAG+CCG",cex=1,col=plt.col[2]),
        text1=list("text",x=6.5,y=38,text="POG",cex=1,col=plt.col[2]),
        text1=list("text",x=15,y=55,text="RRG+CEUG",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[4]]<-list(demo=list(fun="plot",call=list(xlim=c(0,30),ylim=c(0,60),main="",bg="white",fg="black",axes=FALSE,xlab=annotate("M/AFM"),ylab=annotate("F/AFM"),cex.lab=cex.lab,new=FALSE),template=temp))


############
# Plot ACF #
############
plate.data[[5]]$x<-"100*CaO/(Al2O3+Na2O+K2O+FeOt+MgO+CaO)"
plate.data[[5]]$y<-"100*(FeOt+MgO)/(Al2O3+Na2O+K2O+FeOt+MgO+CaO)"

temp1<-list(
        axis1=list("axis",side=1,at=c(0,5,10,15,20,25,30),labels=c("0","5","10","15","20","25","30"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(0,10,20,30,40,50,60),labels=c("0","10","20","30","40","50","60"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(0,11,20.5),y=c(16,20,41),col=plt.col[2],lty="dashed"),
        lines1=list("lines",x=c(3.5,6,20.5,11,3.5),y=c(12,41.5,31,7,12),col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="ACF (Maniar + Piccoli 1989)")
    )

    temp2<-list(
        text1=list("text",x=15,y=2,text="IAG+CAG+CCG",cex=1,col=plt.col[2]),
        text1=list("text",x=20,y=20,text="POG",cex=1,col=plt.col[2]),
        text1=list("text",x=15,y=55,text="RRG+CEUG",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[5]]<-list(demo=list(fun="plot",call=list(xlim=c(0,30),ylim=c(0,60),main="",bg="white",fg="black",axes=FALSE,xlab=annotate("C/ACF"),ylab=annotate("F/ACF"),cex.lab=cex.lab,new=FALSE),template=temp))
#    plate[[5]]<-list(demo=list(fun="plot",call=list(xlim=c(0,30),ylim=c(0,60),main="",bg="white",fg="black",axes=FALSE,xlab=annotate("100*CaO/(Al2O3+Na2O+K2O+FeO[t]+MgO+CaO)"),ylab=annotate("100*(FeO[t]+MgO)/(Al2O3+Na2O+K2O+FeO[t]+MgO+CaO)"),cex.lab=cex.lab,new=FALSE),template=temp))

##############
# Plot Shand #
##############
plate.data[[6]]$x<-"A/CNK"
plate.data[[6]]$y<-"A/NK"

temp1<-list(
        axis1=list("axis",side=1,at=c(0.5,1,1.5,2),labels=c("0.5","1","1.5","2"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(0,1,2,3,4,5,6,7),labels=c("0","1","2","3","4","5","6","7"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
    lines1=list("lines",x=c(1,1,0.5,0.5,1),y=c(1,7,7,1,1)),
    lines2=list("lines",x=c(1,2,2,1,1),y=c(1,1,7,7,1)),
    lines3=list("lines",x=c(1,1,0.5,0.5,1),y=c(1,0,0,1,1)),
    lines4=list("lines",x=c(1,2,2,1,1),y=c(1,1,0,0,1)),
    lines11=list("abline",h=1,col=plt.col[2],lty="dashed"),
    lines12=list("abline",v=1,col=plt.col[2],lty="dashed"),
    lines13=list("lines",x=c(0,7),y=c(0,7),col=plt.col[2],lty="dashed"),
    GCDkit=list("NULL",plot.type="binary",plot.name="A/CNK - A/NK (Maniar + Piccoli 1989)")
    )

    temp2<-list(
    text1=list("text",x=0.6,y=2.8,text="Metaluminous",col=plt.col[2],adj=0),
    text2=list("text",x=1.5,y=2.8,text="Peraluminous",col=plt.col[2],adj=0),
    text3=list("text",x=0.6,y=0.75,text="Peralkaline",col=plt.col[2],adj=0)
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[6]]<-list(demo=list(fun="plot",call=list(xlim=c(0.5,1.9),ylim=c(0.6,3),main="",bg="white",fg="black",axes=FALSE,xlab="A/CNK",ylab="A/NK",cex.lab=cex.lab,new=FALSE),template=temp))

    assign("plate",plate,.GlobalEnv)
    assign("plate.data",plate.data,.GlobalEnv)
}

# Plotting position 102
# "Maniar + Piccoli (1989)","plotPlate(\"Maniar\")"
