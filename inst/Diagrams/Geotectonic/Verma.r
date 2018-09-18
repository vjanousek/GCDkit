######################################################################
# Geotectonic diagrams for (ultra-)basic rocks of Verma et al. (2006)#
#     based on natural log-transformed ratios of major elements      #
######################################################################

Verma<-function(FeMiddlemost=FALSE,GUI=FALSE){
    on.exit(options("show.error.messages"=TRUE)) 
    if(!any(colnames(WR)=="SiO2")){
        if(GUI)winDialog(type="ok","No SiO2 data found!")
        options(show.error.messages=FALSE);stop(call. = FALSE)
    }
    
    i<-WR[,"SiO2"]<52
    if(all(!i)){
        if(GUI)winDialog(type="ok","No data found with SiO2<52 wt.%!")
        options(show.error.messages=FALSE);stop(call. = FALSE)
    }
    
    ee<-filterOut(WR[i,],c("SiO2","TiO2","Al2O3","FeOt","MnO", "MgO", "CaO", "Na2O", "K2O", "P2O5"),1)
    if(nrow(ee)==0){
        if(GUI)winDialog(type="ok","No data found with all majors and SiO2<52 wt.%!")
        options(show.error.messages=FALSE);stop(call. = FALSE)
    }
    
    if(GUI){
        adjustFe<-(winDialog(type="yesno","Adjust Fe2O3/FeO ratio according to Middlemost (1989)?"))
        adjustFe<-adjustFe=="YES" 
    }else{
        adjustFe<-FeMiddlemost
    }
       
    if(adjustFe){
        WRanh<-FeMiddlemost()
    }else{
        ee1<-filterOut(WR[i,],c("Fe2O3","FeO"),1)
        if(nrow(ee1)==0){
            msg<-"Verma: No FeO and Fe2O3 data found and no FeOt recalculation option chosen!"
            if(GUI)winDialog(type="ok",msg) #else cat(msg,"\n")
            options(show.error.messages=FALSE);stop(call. = FALSE)
        }
        #ee<-c(ee,ee1)
    }    
    
    plate<-as.list(1:5)
    plate.data<-as.list(1:5)
    plate$nrow<-2
    plate$ncol<-3
    plate$title<-"Geotectonic diagrams for (ultra-)basic rocks of Verma et al. (2006)"
    plate$plot.position<-201
    plate$plot.name<-"Verma et al. (2006), based on major elements"
    cex.axis<-1.2
    cex.lab<-1.35



##############
#  Fig. 5    #
##############

plate.data[[1]]$x<--4.6761*log(WRanh[i,"TiO2"]/WRanh[i,"SiO2"])+2.5330*log(WRanh[i,"Al2O3"]/WRanh[i,"SiO2"])-0.3884*log(WRanh[i,"Fe2O3"]/WRanh[i,"SiO2"])+3.9688*log(WRanh[i,"FeO"]/WRanh[i,"SiO2"])+0.8980*log(WRanh[i,"MnO"]/WRanh[i,"SiO2"])-0.5832*log(WRanh[i,"MgO"]/WRanh[i,"SiO2"])-0.2896*log(WRanh[i,"CaO"]/WRanh[i,"SiO2"])-0.2704*log(WRanh[i,"Na2O"]/WRanh[i,"SiO2"])+1.0810*log(WRanh[i,"K2O"]/WRanh[i,"SiO2"])+0.1845*log(WRanh[i,"P2O5"]/WRanh[i,"SiO2"])+1.5445
plate.data[[1]]$y<-0.6751*log(WRanh[i,"TiO2"]/WRanh[i,"SiO2"])+4.5895*log(WRanh[i,"Al2O3"]/WRanh[i,"SiO2"])+2.0897*log(WRanh[i,"Fe2O3"]/WRanh[i,"SiO2"])+0.8514*log(WRanh[i,"FeO"]/WRanh[i,"SiO2"])-0.4334*log(WRanh[i,"MnO"]/WRanh[i,"SiO2"])+1.4832*log(WRanh[i,"MgO"]/WRanh[i,"SiO2"])-2.3627*log(WRanh[i,"CaO"]/WRanh[i,"SiO2"])-1.6558*log(WRanh[i,"Na2O"]/WRanh[i,"SiO2"])+0.6757*log(WRanh[i,"K2O"]/WRanh[i,"SiO2"])+0.4130*log(WRanh[i,"P2O5"]/WRanh[i,"SiO2"])+ 13.1639

    temp1<-list(
        axis1=list("axis",side=1,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(1.160, 5.912),y=c(-0.333, 8.000),col=plt.col[2],lty="dashed"),
        lines2=list("lines",x=c(-0.266, -4.190),y=c(0.020, 8.000),col=plt.col[2],lty="dashed"),
        lines3=list("lines",x=c(-8.000, -0.266),y=c(-2.490, 0.020),col=plt.col[2],lty="dashed"),
        lines4=list("lines",x=c(1.160, 3.431),y=c(-0.333, -8.000),col=plt.col[2],lty="dashed"),
        lines5=list("lines",x=c(1.160,-0.266),y=c( -0.333, 0.020),col=plt.col[2],lty="dashed")
    )

    temp2<-list(
        text1=list("text",x=-6,y=4,text="OIB",cex=1,col=plt.col[2]),
        text2=list("text",x=-6,y=-4,text="MORB",cex=1,col=plt.col[2]),
        text3=list("text",x=0,y=7,text="CRB",cex=1,col=plt.col[2]),
        text4=list("text",x=6,y=4,text="IAB",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[1]]<-list(demo=list(fun="plot",call=list(xlim=c(-8,8),ylim=c(-8,8),main="",bg="white",fg="black",axes=FALSE,xlab="DF1",ylab="DF2",cex.lab=cex.lab,new=FALSE),template=temp))

##################
#    #Fig. 6A    #
##################

plate.data[[2]]$x<-3.9998*log(WRanh[i,"TiO2"]/WRanh[i,"SiO2"])-2.2385*log(WRanh[i,"Al2O3"]/WRanh[i,"SiO2"])+0.8110*log(WRanh[i,"Fe2O3"]/WRanh[i,"SiO2"])-2.5865*log(WRanh[i,"FeO"]/WRanh[i,"SiO2"])-1.2433*log(WRanh[i,"MnO"]/WRanh[i,"SiO2"])+0.4872*log(WRanh[i,"MgO"]/WRanh[i,"SiO2"])-0.3153*log(WRanh[i,"CaO"]/WRanh[i,"SiO2"])+0.4325*log(WRanh[i,"Na2O"]/WRanh[i,"SiO2"])-1.0262*log(WRanh[i,"K2O"]/WRanh[i,"SiO2"])+0.0514*log(WRanh[i,"P2O5"]/WRanh[i,"SiO2"])-0.5718
plate.data[[2]]$y<--1.3705*log(WRanh[i,"TiO2"]/WRanh[i,"SiO2"])+3.0104*log(WRanh[i,"Al2O3"]/WRanh[i,"SiO2"])+0.3239*log(WRanh[i,"Fe2O3"]/WRanh[i,"SiO2"])+1.8998*log(WRanh[i,"FeO"]/WRanh[i,"SiO2"])-1.9746*log(WRanh[i,"MnO"]/WRanh[i,"SiO2"])+1.4411*log(WRanh[i,"MgO"]/WRanh[i,"SiO2"])-2.2656*log(WRanh[i,"CaO"]/WRanh[i,"SiO2"])+1.8665*log(WRanh[i,"Na2O"]/WRanh[i,"SiO2"])+0.2872*log(WRanh[i,"K2O"]/WRanh[i,"SiO2"])+0.8138*log(WRanh[i,"P2O5"]/WRanh[i,"SiO2"])+1.8202


    temp1<-list(
        axis1=list("axis",side=1,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(-0.733,-3.788),y=c(-1.405,8.000),col=plt.col[2],lty="dashed"),
        lines2=list("lines",x=c(-0.733,8.000),y=c(-1.405,5.428),col=plt.col[2],lty="dashed"),
        lines3=list("lines",x=c(-1.343,-0.733),y=c(-8.000,-1.405),col=plt.col[2],lty="dashed")
    )

    temp2<-list(
        text1=list("text",x=6,y=-3,text="OIB",cex=1,col=plt.col[2]),
        text3=list("text",x=-1,y=6,text="CRB",cex=1,col=plt.col[2]),
        text4=list("text",x=-6,y=6,text="IAB",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[2]]<-list(demo=list(fun="plot",call=list(xlim=c(-8,8),ylim=c(-8,8),main="",bg="white",fg="black",axes=FALSE,xlab="DF1",ylab="DF2",cex.lab=cex.lab,new=FALSE),template=temp))

################
#    Fig. 6B   #
################

plate.data[[3]]$x<--1.5736*log(WRanh[i,"TiO2"]/WRanh[i,"SiO2"])+6.1498*log(WRanh[i,"Al2O3"]/WRanh[i,"SiO2"])+1.5544*log(WRanh[i,"Fe2O3"]/WRanh[i,"SiO2"])+3.4134*log(WRanh[i,"FeO"]/WRanh[i,"SiO2"])-0.0087*log(WRanh[i,"MnO"]/WRanh[i,"SiO2"])+1.2480*log(WRanh[i,"MgO"]/WRanh[i,"SiO2"])-2.1103*log(WRanh[i,"CaO"]/WRanh[i,"SiO2"])-0.7576*log(WRanh[i,"Na2O"]/WRanh[i,"SiO2"])+1.1431*log(WRanh[i,"K2O"]/WRanh[i,"SiO2"])+0.3524*log(WRanh[i,"P2O5"]/WRanh[i,"SiO2"])+16.8712
plate.data[[3]]$y<-3.9844*log(WRanh[i,"TiO2"]/WRanh[i,"SiO2"])+0.2200*log(WRanh[i,"Al2O3"]/WRanh[i,"SiO2"])+1.1516*log(WRanh[i,"Fe2O3"]/WRanh[i,"SiO2"])-2.2036*log(WRanh[i,"FeO"]/WRanh[i,"SiO2"])-1.6228*log(WRanh[i,"MnO"]/WRanh[i,"SiO2"])+1.4291*log(WRanh[i,"MgO"]/WRanh[i,"SiO2"])-1.2524*log(WRanh[i,"CaO"]/WRanh[i,"SiO2"])+0.3581*log(WRanh[i,"Na2O"]/WRanh[i,"SiO2"])-0.6414*log(WRanh[i,"K2O"]/WRanh[i,"SiO2"])+0.2646*log(WRanh[i,"P2O5"]/WRanh[i,"SiO2"])+5.0506


    temp1<-list(
        axis1=list("axis",side=1,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(8.000,0.361),y=c(-1.332,-0.619),col=plt.col[2],lty="dashed"),
        lines2=list("lines",x=c(-2.673,0.361),y=c(8.000,-0.619),col=plt.col[2],lty="dashed"),
        lines3=list("lines",x=c(-6.779,0.361),y=c(-8.000,-0.619),col=plt.col[2],lty="dashed")
    )

    temp2<-list(
        text1=list("text",x=6,y=-5,text="IAB",cex=1,col=plt.col[2]),
        text3=list("text",x=1,y=6,text="CRB",cex=1,col=plt.col[2]),
        text4=list("text",x=-5,y=6,text="MORB",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[3]]<-list(demo=list(fun="plot",call=list(xlim=c(-8,8),ylim=c(-8,8),main="",bg="white",fg="black",axes=FALSE,xlab="DF1",ylab="DF2",cex.lab=cex.lab,new=FALSE),template=temp))


################
#    #Fig. 6C  #
################

plate.data[[4]]$x<-5.3396*log(WRanh[i,"TiO2"]/WRanh[i,"SiO2"])-1.6279*log(WRanh[i,"Al2O3"]/WRanh[i,"SiO2"])+0.8338*log(WRanh[i,"Fe2O3"]/WRanh[i,"SiO2"])-4.7362*log(WRanh[i,"FeO"]/WRanh[i,"SiO2"])-0.1254*log(WRanh[i,"MnO"]/WRanh[i,"SiO2"])+0.6452*log(WRanh[i,"MgO"]/WRanh[i,"SiO2"])+1.5153*log(WRanh[i,"CaO"]/WRanh[i,"SiO2"])-0.8154*log(WRanh[i,"Na2O"]/WRanh[i,"SiO2"])-0.8888*log(WRanh[i,"K2O"]/WRanh[i,"SiO2"])-0.2255*log(WRanh[i,"P2O5"]/WRanh[i,"SiO2"])+5.7755
plate.data[[4]]$y<-1.1799*log(WRanh[i,"TiO2"]/WRanh[i,"SiO2"])+5.5114*log(WRanh[i,"Al2O3"]/WRanh[i,"SiO2"])+2.7737*log(WRanh[i,"Fe2O3"]/WRanh[i,"SiO2"])-0.1341*log(WRanh[i,"FeO"]/WRanh[i,"SiO2"])+0.6672*log(WRanh[i,"MnO"]/WRanh[i,"SiO2"])+1.1045*log(WRanh[i,"MgO"]/WRanh[i,"SiO2"])-1.7231*log(WRanh[i,"CaO"]/WRanh[i,"SiO2"])-3.8948*log(WRanh[i,"Na2O"]/WRanh[i,"SiO2"])+0.9471*log(WRanh[i,"K2O"]/WRanh[i,"SiO2"])-0.1082*log(WRanh[i,"P2O5"]/WRanh[i,"SiO2"])+15.4984


    temp1<-list(
        axis1=list("axis",side=1,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(-0.830,-1.824),y=c(1.224,8.000),col=plt.col[2],lty="dashed"),
        lines2=list("lines",x=c(-0.830,8.000),y=c(1.224,-3.583),col=plt.col[2],lty="dashed"),
        lines3=list("lines",x=c(-4.865,-0.830),y=c(-8.000,1.224),col=plt.col[2],lty="dashed")
    )

    temp2<-list(
        text1=list("text",x=-4,y=6,text="IAB",cex=1,col=plt.col[2]),
        text3=list("text",x=-1,y=-6,text="MORB",cex=1,col=plt.col[2]),
        text4=list("text",x=1,y=6,text="OIB",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[4]]<-list(demo=list(fun="plot",call=list(xlim=c(-8,8),ylim=c(-8,8),main="",bg="white",fg="black",axes=FALSE,xlab="DF1",ylab="DF2",cex.lab=cex.lab,new=FALSE),template=temp))


################
#   #Fig. 6D   #
################

plate.data[[5]]$x<--0.5183*log(WRanh[i,"TiO2"]/WRanh[i,"SiO2"])+4.9886*log(WRanh[i,"Al2O3"]/WRanh[i,"SiO2"])+2.2204*log(WRanh[i,"Fe2O3"]/WRanh[i,"SiO2"])+1.1801*log(WRanh[i,"FeO"]/WRanh[i,"SiO2"])-0.3008*log(WRanh[i,"MnO"]/WRanh[i,"SiO2"])+1.3297*log(WRanh[i,"MgO"]/WRanh[i,"SiO2"])-2.1834*log(WRanh[i,"CaO"]/WRanh[i,"SiO2"])-1.9319*log(WRanh[i,"Na2O"]/WRanh[i,"SiO2"])+0.6976*log(WRanh[i,"K2O"]/WRanh[i,"SiO2"])+0.8998*log(WRanh[i,"P2O5"]/WRanh[i,"SiO2"])+13.2625
plate.data[[5]]$y<-5.0509*log(WRanh[i,"TiO2"]/WRanh[i,"SiO2"])-0.4972*log(WRanh[i,"Al2O3"]/WRanh[i,"SiO2"])+1.0046*log(WRanh[i,"Fe2O3"]/WRanh[i,"SiO2"])-3.3848*log(WRanh[i,"FeO"]/WRanh[i,"SiO2"])+0.5528*log(WRanh[i,"MnO"]/WRanh[i,"SiO2"])+0.2925*log(WRanh[i,"MgO"]/WRanh[i,"SiO2"])+0.4007*log(WRanh[i,"CaO"]/WRanh[i,"SiO2"])-2.8637*log(WRanh[i,"Na2O"]/WRanh[i,"SiO2"])-0.2189*log(WRanh[i,"K2O"]/WRanh[i,"SiO2"])-1.0558*log(WRanh[i,"P2O5"]/WRanh[i,"SiO2"])+2.8877


    temp1<-list(
        axis1=list("axis",side=1,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(-8,-4,0,4,8),labels=c("-8","-4","0","4","8"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=c(0.029,8.000),y=c(-0.222,4.322),col=plt.col[2],lty="dashed"),
        lines2=list("lines",x=c(0.029,-6.177),y=c(-0.222,8.000),col=plt.col[2],lty="dashed"),
        lines3=list("lines",x=c(-0.819,0.029),y=c(-8.000,-0.222),col=plt.col[2],lty="dashed")
    )

    temp2<-list(
        text1=list("text",x=-6,y=2,text="MORB",cex=1,col=plt.col[2]),
        text3=list("text",x=5,y=-2,text="CRB",cex=1,col=plt.col[2]),
        text4=list("text",x=-1,y=6,text="OIB",cex=1,col=plt.col[2])
    )

    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    plate[[5]]<-list(demo=list(fun="plot",call=list(xlim=c(-8,8),ylim=c(-8,8),main="",bg="white",fg="black",axes=FALSE,xlab="DF1",ylab="DF2",cex.lab=cex.lab,new=FALSE),template=temp))

    assign("plate",plate,.GlobalEnv)
    assign("plate.data",plate.data,.GlobalEnv)

}

# Plotting position 201
# "Verma et al. (2006), based on major elements","plotPlate(\"Verma\",getOption(\"gcd.plot.text\"))"        
