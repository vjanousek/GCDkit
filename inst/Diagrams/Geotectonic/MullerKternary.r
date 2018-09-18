#####################################################################
#             Geochemical Discrimination Between Shoshonitic        #
#                and Potassic Volcanic Rocks in Different           #
#                            Tectonic Settings                      #
#                            Muller et al.(1992)                    #
#####################################################################

MullerKternary<-function(plot.txt=getOption("gcd.plot.text")){
    plate<-as.list(1:3)
    plate.data<-as.list(1:3)
    plate$nrow<-1
    plate$ncol<-3
    plate$title<-"Geotectonic classification of potassic rocks - Muller et al. (1992)"
    plate$plot.position<-304.5
    plate$plot.name<-"Mueller et al. (1992) Potassic rocks ternary"   
    cex.axis<-1.2
    cex.lab<-1.35
    
    ############
    # Plot 1   #
    ############
    what<-WR[,"La"]>0 & WR[,"TiO2"]>0 & WR[,"Hf"]>0
    what<-names(what[!is.na(what)])
    aa<-WR[what,"La"]
    bb<-WR[what,"TiO2"]*1e2
    cc<-WR[what,"Hf"]*10
    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma
    plate.data[[1]]$x<-cc+bb/2
    plate.data[[1]]$y<-sqrt(3)*bb/2

    temp1<-list(
        lines1=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        lines2=list("lines",x=c(0.6825/2,1-0.6825/2),
            y=c(sqrt(3)/2*0.6825,sqrt(3)/2*0.6825),col=plt.col[2],lty="dashed"),
        A=list("text",x=0,y=-0.03,text="La",adj=0.5),
        C=list("text",x=1,y=-0.03,text="10*Hf",adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text=annotate("TiO2/100"),adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.name="Geotectonic classification of potassic rocks - Muller et al. (1992)")
    )      

    temp2<-list(
        text1=list("text",x=0.5,y=0.7,text="IOP+\nLOP",adj=0.5,col=plt.col[2]),
        text2=list("text",x=0.25,y=0.3,text="CAP+\nPAP",col=plt.col[2])
    )

    if(plot.txt){
        temp<-c(temp1,temp2)
    }else{
        temp<-temp1
    }    

    plate[[1]]<-list(demo=list(fun="plot",call=list(xlim=c(-.03,1.08),ylim=c(-0.05,1.03),main="",bg="white",fg="black",asp=1,axes=FALSE,xlab="",ylab="",cex.lab=cex.lab,new=FALSE),template=temp))
    #ternary("La","TiO2*1e2","10*Hf",alab="La",blab="TiO2/100",clab="10*Hf",col=labels$Colour,pch=labels$Symbol,cex=labels$Size,new=F)


    ############
    # Plot 2   #
    ############
    what<-WR[,"La"]>0 & WR[,"TiO2"]>0 & WR[,"P2O5"]>0
    what<-names(what[!is.na(what)])
    
    aa<-WR[what,"La"]*10
    bb<-WR[what,"TiO2"]*1e3
    cc<-WR[what,"P2O5"]*1e3
    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma
    plate.data[[2]]$x<-cc+bb/2
    plate.data[[2]]$y<-sqrt(3)*bb/2

    temp1<-list(
        lines1=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        lines2=list("lines",x=c(0.7217/2,1-0.7217/2),
            y=c(sqrt(3)/2*0.7217,sqrt(3)/2*0.7217),col=plt.col[2],lty="dashed"),
        A=list("text",x=0,y=-0.03,text="La*10",adj=0.5),
        C=list("text",x=1,y=-0.03,text=annotate("P2O5/10"),adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text=annotate("TiO2/10"),adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.name="Geotectonic classification of potassic rocks - Muller et al. (1992)")
    )      

    temp2<-list(
        text1=list("text",x=0.5,y=0.75,text="IOP",adj=0.5,col=plt.col[2]),
        text2=list("text",x=0.25,y=0.3,text="LOP",col=plt.col[2])
    )

    if(plot.txt){
        temp<-c(temp1,temp2)
    }else{
        temp<-temp1
    }    

    plate[[2]]<-list(demo=list(fun="plot",call=list(xlim=c(-.03,1.08),ylim=c(-0.05,1.03),main="",bg="white",fg="black",asp=1,axes=FALSE,xlab="",ylab="",cex.lab=cex.lab,new=FALSE),template=temp))
    #ternary("10*La","TiO2*1e3","P2O5*1e3",alab="La*10",blab="TiO2/10",clab="P2O5/10",col=labels$Colour,pch=labels$Symbol,cex=labels$Size,new=F)
       
    ############
    # Plot 3   #
    ############
    what<-WR[,"Nb"]>0 & WR[,"Zr"]>0 & WR[,"Ce"]>0& WR[,"P2O5"]>0
    what<-names(what[!is.na(what)])
    aa<-WR[what,"Nb"]*50
    bb<-WR[what,"Zr"]*3
    cc<-WR[what,"Ce"]/WR[what,"P2O5"]
    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma
    plate.data[[3]]$x<-cc+bb/2
    plate.data[[3]]$y<-sqrt(3)*bb/2

    temp1<-list(
        lines1=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        lines2=list("lines",x=c(0.15,0.6),
            y=c(0,0.8*sqrt(3)/2),col=plt.col[2],lty="dashed"),
        A=list("text",x=0,y=-0.03,text="50*Nb",adj=0.5),
        C=list("text",x=1,y=-0.03,text=annotate("Ce/P2O5"),adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text="3*Zr",adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.name="Geotectonic classification of potassic rocks - Muller et al. (1992)")
    )      

    temp2<-list(
        text1=list("text",x=0.45,y=0.615,text="PAP",adj=0.5,col=plt.col[2]),
        text2=list("text",x=0.5,y=0.3,text="CAP",adj=0.5,col=plt.col[2])
    )

    if(plot.txt){
        temp<-c(temp1,temp2)
    }else{
        temp<-temp1
    }    

    plate[[3]]<-list(demo=list(fun="plot",call=list(xlim=c(-.03,1.08),ylim=c(-0.05,1.03),main="",bg="white",fg="black",asp=1,axes=FALSE,xlab="",ylab="",cex.lab=cex.lab,new=FALSE),template=temp))
    #ternary("50*Nb","3*Zr","Ce/P2O5",new=F)

    assign("plate",plate,.GlobalEnv)
    assign("plate.data",plate.data,.GlobalEnv)
}

# Plotting position 304.5 TERNARY PLOTS
# "Mueller et al. (1992) Potassic rocks ternary","plotPlate(\"MullerKternary\")"
