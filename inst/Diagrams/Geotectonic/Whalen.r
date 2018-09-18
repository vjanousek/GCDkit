#####################################################################
#                 Discrimination of A-type  granitoids              #
#                          Whalen et al. 1987                       #
#####################################################################

Whalen<-function(plot.txt=getOption("gcd.plot.text")){
    plate<-as.list(1:12)
    plate.data<-as.list(1:12)
    plate$nrow<-3
    plate$ncol<-4
    plate$title<-"Plots to distinguish A-type granitoids - Whalen (1987)"
    plate$plot.position<-104
    plate$plot.name<-"A type granitoids (Whalen et al. 1987)"
    cex.axis<-1.2
    cex.lab<-1.35

A.mjr<-c(73.81,0.26,12.40,1.24,1.58,0.06,0.20,0.75,4.07,4.65,0.04,0.95,2.70)
names(A.mjr)<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","MnO","MgO","CaO","Na2O","K2O","P2O5","A.I.","FeOt")
 
A.trc<-c(352,169,48,24,23,5,528,37,75,137,4,6,120,24.6,3.75)
names(A.trc)<-c("Ba","Rb","Sr","Pb","Th","U","Zr","Nb","Y","Ce","Sc","V","Zn","Ga","Ga/Al")

###################################
# Whalen et al. (1987) Fig. 5 A   # 
###################################

# Zr+Nb+Ce+Y vs FeOt/MgO
    plate.data[[1]]$x<-"Zr+Nb+Ce+Y"
    plate.data[[1]]$y<-"FeOt/MgO"

    temp1<-list(
        axis1=list("axis",side=1,at=c(50,500,5000),labels=c("50","500","5000"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(1,3,10,30,100,500),labels=c("1","3","10","30","100","500"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(350,350,50),y=c(1,16,16),col=plt.col[2]),
        lines2=list("lines",x=c(50,350),y=c(4,4),col=plt.col[2],lty="dashed")
     )

    temp2<-list(
        text1=list("text",x=A.trc["Zr"]+A.trc["Nb"]+A.trc["Ce"]+A.trc["Y"],y=A.mjr["FeOt"]/A.mjr["MgO"],text="A",cex=1,col=plt.col[2]),
        text2=list("text",x=180,y=8,text="FG",cex=1,col=plt.col[2]),
        text3=list("text",x=130,y=2,text="OTG",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[1]]<-list(demo=list(fun="plot",call=list(xlim=c(50,5000),ylim=c(1,500),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab="Zr+Nb+Ce+Y",ylab=annotate("FeOt/MgO"),cex.lab=cex.lab,new=FALSE),template=temp))    
    
    

###################################
# Whalen et al. (1987) Fig. 5 B   # 
###################################
  
# Zr+Nb+Ce+Y vs(Na2O+K2O)/CaO

    plate.data[[2]]$x<-"Zr+Nb+Ce+Y"
    plate.data[[2]]$y<-"(K2O+Na2O)/CaO"

    temp1<-list(
        axis1=list("axis",side=1,at=c(50,500,5000),labels=c("50","500","5000"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(1,3,10,30,100,300),labels=c("1","3","10","30","100","300"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(350,350,50),y=c(1,28,28),col=plt.col[2]),
        lines2=list("lines",x=c(50,350),y=c(7,7),col=plt.col[2],lty="dashed")
     )

    temp2<-list(
        text1=list("text",x=A.trc["Zr"]+A.trc["Nb"]+A.trc["Ce"]+A.trc["Y"],y=(A.mjr["Na2O"]+A.mjr["K2O"])/A.mjr["CaO"],text="A",cex=1,col=plt.col[2]),
        text2=list("text",x=130,y=15,text="FG",cex=1,col=plt.col[2]),
        text3=list("text",x=130,y=3,text="OTG",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[2]]<-list(demo=list(fun="plot",call=list(xlim=c(50,5000),ylim=c(1,300),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab="Zr+Nb+Ce+Y",ylab=annotate("(Na2O+K2O)/CaO"),cex.lab=cex.lab,new=FALSE),template=temp))

###################################
# Whalen et al. (1987) Fig. 1 A   # 
###################################

# 10000Ga/Al vs Na2O+K2O
   
    plate.data[[3]]$x<-"Ga/(Al2O3*0.52913)"
    plate.data[[3]]$y<-"K2O+Na2O"

    temp1<-list(
        axis1=list("axis",side=1,at=c(1,3,10,20),labels=c("1","3","10","20"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(1,3,10,20),labels=c("1","3","10","20"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(2.6,2.6,1),y=c(1,8.5,8.5),col=plt.col[2])
     )

    temp2<-list(
        text1=list("text",x=A.trc["Ga/Al"],y=A.mjr["Na2O"]+A.mjr["K2O"],text="A",cex=1,col=plt.col[2]),
        text2=list("text",x=1.6,y=3,text="I & S",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[3]]<-list(demo=list(fun="plot",call=list(xlim=c(1,20),ylim=c(1,20),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab="10000*Ga/Al",ylab=annotate("Na2O+K2O"),cex.lab=cex.lab,new=FALSE),template=temp))


###################################
# Whalen et al. (1987) Fig. 1 B   # 
###################################

# 10000Ga/Al vs (Na2O+K2O)/CaO

    plate.data[[4]]$x<-"Ga/(Al2O3*0.52913)"
    plate.data[[4]]$y<-"(K2O+Na2O)/CaO"

    temp1<-list(
        axis1=list("axis",side=1,at=c(1,3,10,20),labels=c("1","3","10","20"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(1,10,100),labels=c("1","10","100"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(2.6,2.6,1),y=c(1,10,10),col=plt.col[2])
     )

    temp2<-list(
        text1=list("text",x=A.trc["Ga/Al"],y=(A.mjr["Na2O"]+A.mjr["K2O"])/A.mjr["CaO"],text="A",cex=1,col=plt.col[2]),
        text2=list("text",x=1.6,y=4,text="I & S",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[4]]<-list(demo=list(fun="plot",call=list(xlim=c(1,20),ylim=c(1,200),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab="10000*Ga/Al",ylab=annotate("(Na2O+K2O)/CaO"),cex.lab=cex.lab,new=FALSE),template=temp))


###################################
# Whalen et al. (1987) Fig. 1 C   # 
###################################

# 10000Ga/Al vs K2O/MgO

    plate.data[[5]]$x<-"Ga/(Al2O3*0.52913)"
    plate.data[[5]]$y<-"K2O/MgO"


    temp1<-list(
        axis1=list("axis",side=1,at=c(1,3,10,20),labels=c("1","3","10","20"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(1,10,100,500),labels=c("1","10","100","500"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(2.6,2.6,1),y=c(1,16,16),col=plt.col[2])
     )

    temp2<-list(
        text1=list("text",x=A.trc["Ga/Al"],y=(A.mjr["K2O"])/A.mjr["MgO"],text="A",cex=1,col=plt.col[2]),
        text2=list("text",x=1.6,y=4,text="I & S",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[5]]<-list(demo=list(fun="plot",call=list(xlim=c(1,20),ylim=c(1,500),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab="10000*Ga/Al",ylab=annotate("K2O/MgO"),cex.lab=cex.lab,new=FALSE),template=temp))

   
###################################
# Whalen et al. (1987) Fig. 1 D   # 
###################################

# 10000Ga/Al vs FeOt/MgO

    plate.data[[6]]$x<-"Ga/(Al2O3*0.52913)"
    plate.data[[6]]$y<-"FeOt/MgO"

        temp1<-list(
        axis1=list("axis",side=1,at=c(1,3,10,20),labels=c("1","3","10","20"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(1,10,100,500),labels=c("1","10","100","500"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(2.6,2.6,1),y=c(1,10,10),col=plt.col[2])
     )

    temp2<-list(
        text1=list("text",x=A.trc["Ga/Al"],y=(A.mjr["FeOt"])/A.mjr["MgO"],text="A",cex=1,col=plt.col[2]),
        text2=list("text",x=1.6,y=3,text="I & S",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[6]]<-list(demo=list(fun="plot",call=list(xlim=c(1,20),ylim=c(1,500),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab="10000*Ga/Al",ylab=annotate("FeO[t]/MgO"),cex.lab=cex.lab,new=FALSE),template=temp))


###################################
# Whalen et al. (1987) Fig. 2 A   # 
###################################

# 10000Ga/Al vs Zr

    plate.data[[7]]$x<-"Ga/(Al2O3*0.52913)"
    plate.data[[7]]$y<-"Zr"

        temp1<-list(
        axis1=list("axis",side=1,at=c(1,3,10,20),labels=c("1","3","10","20"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(1,10,100,1000,5000),labels=c("1","10","100","1000","5000"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(2.6,2.6,1),y=c(10,250,250),col=plt.col[2])
     )

    temp2<-list(
        text1=list("text",x=A.trc["Ga/Al"],y=(A.mjr["Zr"]),text="A",cex=1,col=plt.col[2]),
        text2=list("text",x=1.6,y=50,text="I & S",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[7]]<-list(demo=list(fun="plot",call=list(xlim=c(1,20),ylim=c(10,5000),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab="10000*Ga/Al",ylab="Zr",cex.lab=cex.lab,new=FALSE),template=temp))


###################################
# Whalen et al. (1987) Fig. 2 B   # 
###################################

# 10000Ga/Al vs Nb

    plate.data[[8]]$x<-"Ga/(Al2O3*0.52913)"
    plate.data[[8]]$y<-"Nb"
        temp1<-list(
        axis1=list("axis",side=1,at=c(1,3,10,20),labels=c("1","3","10","20"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(1,10,100,300),labels=c("1","10","100","300"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(2.6,2.6,1),y=c(1,20,20),col=plt.col[2])
     )

    temp2<-list(
        text1=list("text",x=A.trc["Ga/Al"],y=(A.mjr["Nb"]),text="A",cex=1,col=plt.col[2]),
        text2=list("text",x=1.6,y=4,text="I & S",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[8]]<-list(demo=list(fun="plot",call=list(xlim=c(1,20),ylim=c(1,300),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab="10000*Ga/Al",ylab="Nb",cex.lab=cex.lab,new=FALSE),template=temp))

###################################
# Whalen et al. (1987) Fig. 2 C   # 
###################################

# 10000Ga/Al vs Ce

    plate.data[[9]]$x<-"Ga/(Al2O3*0.52913)"
    plate.data[[9]]$y<-"Ce"

        temp1<-list(
        axis1=list("axis",side=1,at=c(1,3,10,20),labels=c("1","3","10","20"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(1,10,100,1000),labels=c("1","10","100","1000"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(2.6,2.6,1),y=c(1,100,100),col=plt.col[2])
     )

    temp2<-list(
        text1=list("text",x=A.trc["Ga/Al"],y=(A.mjr["Ce"]),text="A",cex=1,col=plt.col[2]),
        text2=list("text",x=1.6,y=10,text="I & S",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[9]]<-list(demo=list(fun="plot",call=list(xlim=c(1,20),ylim=c(1,1000),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab="10000*Ga/Al",ylab="Ce",cex.lab=cex.lab,new=FALSE),template=temp))

###################################
# Whalen et al. (1987) Fig. 2 D   # 
###################################

# 10000Ga/Al vs Y

    plate.data[[10]]$x<-"Ga/(Al2O3*0.52913)"
    plate.data[[10]]$y<-"Y"

        temp1<-list(
        axis1=list("axis",side=1,at=c(1,3,10,20),labels=c("1","3","10","20"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(1,10,100),labels=c("1","10","100"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(2.6,2.6,1),y=c(1,80,80),col=plt.col[2])
     )

    temp2<-list(
        text1=list("text",x=A.trc["Ga/Al"],y=(A.mjr["Y"]),text="A",cex=1,col=plt.col[2]),
        text2=list("text",x=1.6,y=10,text="I & S",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[10]]<-list(demo=list(fun="plot",call=list(xlim=c(1,20),ylim=c(1,300),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab="10000*Ga/Al",ylab="Y",cex.lab=cex.lab,new=FALSE),template=temp))

###################################
# Whalen et al. (1987) Fig. 2 E   # 
###################################

# 10000Ga/Al vs Zn

    plate.data[[11]]$x<-"Ga/(Al2O3*0.52913)"
    plate.data[[11]]$y<-"Zn"
        temp1<-list(
        axis1=list("axis",side=1,at=c(1,3,10,20),labels=c("1","3","10","20"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(1,10,100,1000),labels=c("1","10","100","1000"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(2.6,2.6,1),y=c(1,100,100),col=plt.col[2])
     )

    temp2<-list(
        text1=list("text",x=A.trc["Ga/Al"],y=(A.mjr["Zn"]),text="A",cex=1,col=plt.col[2]),
        text2=list("text",x=1.6,y=10,text="I & S",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[11]]<-list(demo=list(fun="plot",call=list(xlim=c(1,20),ylim=c(1,1000),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab="10000*Ga/Al",ylab="Zn",cex.lab=cex.lab,new=FALSE),template=temp))

###################################
# Whalen et al. (1987) Fig. F     # 
###################################

# 10000Ga/Al vs A.I.
    #WR<<-addOn("A.I.",(WR[,"Na2O"]/MW["Na2O"] + WR[,"K2O"]/MW["K2O"])/(WR[,"Al2O3"]/MW["Al2O3"])) 

    plate.data[[12]]$x<-"Ga/(Al2O3*0.52913)"
    plate.data[[12]]$y<-"(Na2O/MW[\"Na2O\"]+K2O/MW[\"K2O\"])/(Al2O3/MW[\"Al2O3\"])"

        temp1<-list(
        axis1=list("axis",side=1,at=c(1,3,10,20),labels=c("1","3","10","20"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(0.5,1,2),labels=c("0.5","1","2"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(2.6,2.6,1),y=c(0.5,0.85,0.85),col=plt.col[2])
     )

    temp2<-list(
        text1=list("text",x=A.trc["Ga/Al"],y=(A.mjr["Zn"]),text="A",cex=1,col=plt.col[2]),
        text2=list("text",x=1.7,y=0.65,text="I & S",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[12]]<-list(demo=list(fun="plot",call=list(xlim=c(1,20),ylim=c(0.5,2),main="",bg="white",fg="black",axes=FALSE,log="xy",xlab="10000*Ga/Al",ylab="Agpaitic index",cex.lab=cex.lab,new=FALSE),template=temp))

    assign("plate",plate,.GlobalEnv)
    assign("plate.data",plate.data,.GlobalEnv)
}


# Plotting position 104
# "A type granitoids (Whalen et al. 1987)","plotPlate(\"Whalen\")"
