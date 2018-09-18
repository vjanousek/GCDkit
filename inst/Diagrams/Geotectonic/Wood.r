#####################################################################
#                Triangular diagrams for basalt discrimination      #
#                          Wood 1980                                #
#####################################################################
Wood<-function(plot.txt=getOption("gcd.plot.text")){
    plate<-as.list(1:3)
    plate.data<-as.list(1:3)
    plate$nrow<-1
    plate$ncol<-3
    plate$title<-"Triangular diagrams of the Th-Hf-Ta-Zr-Nb system, Wood 1980"
    plate$plot.position<-210
    plate$plot.name<-"Wood (1980)"
    cex.axis<-1.2
    cex.lab<-1.35
    xxx<-filterOut(WR,c("Ta","Th","Hf","Nb","Zr"),5)
    
    xxx<-addOn("Ta",NA,xxx)
    xxx<-addOn("Th",NA,xxx)
    xxx<-addOn("Hf",NA,xxx)
    xxx<-addOn("Nb",NA,xxx)
    xxx<-addOn("Zr",NA,xxx)
    
    ############
    # Plot 1   #
    ############

#what<-rownames(xxx)[xxx[,"SiO2"]<52]
#what<-what[!is.na(what)]
#what<-xxx[what,"Th"]>0 & xxx[what,"Hf"]>0 & xxx[what,"Ta"]>0

what<-xxx[,"Th"]>0 & xxx[,"Hf"]>0 & xxx[,"Ta"]>0
what<-names(what[!is.na(what)])
aa<-xxx[what,"Th"]
bb<-xxx[what,"Hf"]/3
cc<-xxx[what,"Ta"]
suma<-aa+bb+cc
aa<-aa/suma
bb<-bb/suma
cc<-cc/suma
plate.data[[1]]$x<-cc+bb/2
plate.data[[1]]$y<-sqrt(3)*bb/2


temp1<-list(
        lines1=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        lines2=list("lines",x=c(0.425,0.465,0.455,0.42,0.3225,0.33,0.33,0.54,0.58,0.565,0.33),
            y=c(0.7361216,0.6668396,0.5975575,0.3983717,0.2121762,0.1732051,0.06928203,0.1039230,0.1732051,0.2857884,0.1732051),col=plt.col[2]),
        lines3=list("lines",x=c(0.455,0.3125,0.05),y=c(0.5975575,0.2121762,0),col=plt.col[2]),
        lines4=list("lines",  x=c(0.565,0.6525,0.675,0.605,0.42),y=c(0.2857884,0.3680608,0.476314,0.5629165,0.3983717),col=plt.col[2]),
        lines5=list("lines",x=c(0.605,0.5325,0.465),y=c(0.5629165,0.7577722,0.6668396),col=plt.col[2]),  
        lines6=list("lines",x=c(0.25,0.37),y=c(0.4330127,0.3637307),col=plt.col[2],lty="dashed"),
        A=list("text",x=0,y=-0.03,text="Th",adj=0.5),
        C=list("text",x=1,y=-0.03,text="Ta",adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text="Hf/3",adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.name="Triangular diagram with apices Th, Hf/3 and Ta - Wood (1980)")
        )      

temp2<-list(
        text4=list("text",x=0.45,y=0.15,text="WPA",col=plt.col[2]),
        text5=list("text",x=0.525,y=0.615,text="N-\nMORB",col=plt.col[2]),
        text6=list("text",x=0.35,y=0.5,text="IAT",col=plt.col[2]),
        text7=list("text",x=0.52,y=0.35,text="E-MORB,\nWPT",col=plt.col[2]),
        text8=list("text",x=0.2,y=0.22,text="CAB",col=plt.col[2])
        )

if(plot.txt){
    temp<-c(temp1,temp2)}
    else{
    temp<-temp1
}    

    plate[[1]]<-list(demo=list(fun="plot",call=list(xlim=c(-.03,1.08),ylim=c(-0.05,1.03),main="",bg="white",fg="black",asp=1,axes=FALSE,xlab="",ylab="",cex.lab=cex.lab,new=FALSE),template=temp))


    ############
    # Plot 2   #
    ############

what<-xxx[,"Th"]>0 & xxx[,"Hf"]>0 & xxx[,"Nb"]>0

what<-names(what[!is.na(what)])
    aa<-xxx[what,"Th"]
    bb<-xxx[what,"Hf"]/3
    cc<-xxx[what,"Nb"]/16
    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma
    plate.data[[2]]$x<-cc+bb/2
    plate.data[[2]]$y<-sqrt(3)*bb/2

temp1<-list(
        lines1=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        lines2=list("lines",x=c(5e-02,0.32,0.445,0.4635,0.425),y=c(0,0.2165064,0.5629165,0.6642415,0.7361216),col=plt.col[2]),
        lines3=list("lines",x=c(0.25,0.3715),y=c(0.4330127,0.3663287),col=plt.col[2],lty="dashed"),
        lines4=list("lines",x=c(0.445,0.4205,0.605,0.5275,0.4635),y=c(0.5629165,0.3940416,0.5629165,0.7577722,0.6642415),col=plt.col[2]),
        lines5=list("lines",x=c(0.605,0.675,0.66,0.57,0.33),y=c(0.5629165,0.476314,0.3637307,0.2944486,0.1732051),col=plt.col[2]),  
        lines6=list("lines",x=c(0.4205,0.33,0.3285,0.54,0.58,0.57),y=c(0.3940416,0.2165064,0.06668396,0.1039230,0.1732051,0.2944486),col=plt.col[2]),  
        A=list("text",x=0,y=-0.03,text="Th",adj=0.5),
        C=list("text",x=1,y=-0.03,text="Nb/16",adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text="Hf/3",adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.name="Triangular diagram with apices Th, Hf/3 and Nb/16 - Wood (1980)")
        )
temp2<-list(        
        text4=list("text",x=0.45,y=0.15,text="WPA",col=plt.col[2]),
        text5=list("text",x=0.525,y=0.615,text="N-\nMORB",col=plt.col[2]),
        text6=list("text",x=0.35,y=0.5,text="IAT",col=plt.col[2]),
        text7=list("text",x=0.52,y=0.35,text="E-MORB,\nWPT",col=plt.col[2]),
        text8=list("text",x=0.2,y=0.22,text="CAB",col=plt.col[2])
        )

if(plot.txt){
    temp<-c(temp1,temp2)}
    else{
    temp<-temp1
}    

plate[[2]]<-list(demo=list(fun="plot",call=list(xlim=c(-.03,1.08),ylim=c(-0.05,1.03),main="",bg="white",fg="black",asp=1,axes=FALSE,xlab="",ylab="",cex.lab=cex.lab,new=FALSE),template=temp))

    
    ############
    # Plot 3   #
    ############


what<-xxx[,"Th"]>0 & xxx[,"Zr"]>0 & xxx[,"Nb"]>0

what<-names(what[!is.na(what)])
    aa<-xxx[what,"Th"]
    bb<-xxx[what,"Zr"]/117
    cc<-xxx[what,"Nb"]/16
    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma
    plate.data[[3]]$x<-cc+bb/2
    plate.data[[3]]$y<-sqrt(3)*bb/2

temp1<-list(
        lines1=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        lines2=list("lines",x=c(0.05,0.32,0.445,0.4635,0.425),y=c(0,0.2165064,0.5629165,0.6642415,0.7361216),col=plt.col[2]),
        lines3=list("lines",x=c(0.25,0.3715),y=c(0.4330127,0.3663287),col=plt.col[2],lty="dashed"),
        lines4=list("lines",x=c(0.445,0.4205,0.605,0.5275,0.4635),y=c(0.5629165,0.3940416,0.5629165,0.7577722,0.6642415),col=plt.col[2]),        
        lines5=list("lines",x=c(0.605,0.675,0.66,0.57,0.33),y=c(0.5629165,0.476314,0.3637307,0.2944486,0.1732051),col=plt.col[2]),  
        lines6=list("lines",x=c(0.4205,0.33,0.3285,0.54,0.58,0.57),y=c(0.3940416,0.2165064,0.06668396,0.1039230,0.1732051,0.2944486),col=plt.col[2]),  
        A=list("text",x=0,y=-0.03,text="Th",adj=0.5),
        C=list("text",x=1,y=-0.03,text="Nb/16",adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text="Zr/117",adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.name="Triangular diagram with apices Th, Zr/117 and Nb/16 - Wood (1980)")
)
temp2<-list(        
        text4=list("text",x=0.45,y=0.15,text="WPA",col=plt.col[2]),
        text5=list("text",x=0.525,y=0.615,text="N-\nMORB",col=plt.col[2]),
        text6=list("text",x=0.35,y=0.5,text="IAT",col=plt.col[2]),
        text7=list("text",x=0.52,y=0.35,text="E-MORB,\nWPT",col=plt.col[2]),
        text8=list("text",x=0.2,y=0.22,text="CAB",col=plt.col[2])        
)

if(plot.txt){
    temp<-c(temp1,temp2)
    }else{
    temp<-temp1
}    

plate[[3]]<-list(demo=list(fun="plot",call=list(xlim=c(-.03,1.08),ylim=c(-0.05,1.03),main="",bg="white",fg="black",asp=1,axes=FALSE,xlab="",ylab="",cex.lab=cex.lab,new=FALSE),template=temp))
    assign("plate",plate,.GlobalEnv)
    assign("plate.data",plate.data,.GlobalEnv)
}

# Plotting position 210
#  "Wood (1980)","plotPlate(\"Wood\")"
