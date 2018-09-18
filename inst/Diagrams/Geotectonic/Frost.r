#################################################################
#    Compiled by Frost et al. (2001) - Journal of Petrology     #
#################################################################


Frost<-function(plot.txt=getOption("gcd.plot.text"),clssf=FALSE,GUI=FALSE){
    plate<-as.list(1:3)
    plate.data<-as.list(1:3)
    plate$nrow<-1
    plate$ncol<-3
    plate$title<-"Granite tectonic discrimination - Frost et al. (2001)"
    plate$plot.name<-"Frost et al. (2001)"
    plate$plot.position<-103
    cex.axis<-1.2
    cex.lab<-1.35

    ############
    # Plot 1   #
    ############

    if( all(!is.na(WR[,"FeO"])&!is.na(WR[,"Fe2O3"]))){
        if(GUI) typ<-winDialog(type="yesno","Use FeO?\n(if NO, total Fe as FeO will be used)") else typ<-"YES"
    }else{
        typ<-"NO"
    }

    if (typ=="YES"){ 
        koef<-0.446
        FeNr<-"FeO/(FeO+MgO)" 
    }else{
        koef<-0.486
        FeNr<-"FeOt/(FeOt+MgO)"
    }

    x<-50:77

    plate.data[[1]]$x<-"SiO2"
    plate.data[[1]]$y<-FeNr

    temp1<-list(
        axis1=list("axis",side=1,at=c(50,55,60,65,70,75,80),labels=c("50","55","60","65","70","75","80"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(0,0.2,0.4,0.6,0.8,1),labels=c("0","0.2","0.4","0.6","0.8","1"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=x,y=koef+0.0046*x,col=plt.col[2]),
        GCDkit=list("NULL",plot.type="binary",plot.name="SiO2 - FeNr (Frost et al. 2001)")
    )

    temp2<-list(
        text1=list("text",x=55,y=0.9,text="ferroan",col=plt.col[2]),
        text2=list("text",x=65,y=0.6,text="magnesian",cex=1,col=plt.col[2])
    )
    
    if(getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[1]]<-list(demo=list(fun="plot",call=list(xlim=c(50,80),ylim=c(0,1),main="",bg="white",fg="black",axes=FALSE,xlab=annotate("SiO2"),ylab=annotate(FeNr),cex.lab=cex.lab,new=FALSE),template=temp))
    
    ############
    # Plot 2   #
    ############

    MALI<-"Na2O+K2O-CaO"
   
    plate.data[[2]]$x<-"SiO2"
    plate.data[[2]]$y<-MALI
    
    temp1<-list(
        axis1=list("axis",side=1,at=c(50,55,60,65,70,75,80),labels=c("50","55","60","65","70","75","80"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(-8,-4,0,4,8,12),labels=c("-8","-4","0","4","8","12"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("lines",x=x,y=-41.86+1.112*x-0.00572*x^2,col=plt.col[2]),
        lines2=list("lines",x=x,y=-44.72+1.094*x-0.00527*x^2,col=plt.col[2]),
        lines3=list("lines",x=x,y=-45.36+1.0043*x-0.00427*x^2,col=plt.col[2]),
        GCDkit=list("NULL",plot.type="binary",plot.name="SiO2 - MALI (Frost et al. 2001)")
    )

    temp2<-list(
        text1=list("text",x=55,y=7.7,text="alkalic",cex=1,col=plt.col[2]),
        text2=list("text",x=60,y=3.5,text="alkali-calcic",cex=1,col=plt.col[2],srt=25),
        text3=list("text",x=60,y=1,text="calc-alkalic",cex=1,col=plt.col[2],srt=25),
        text4=list("text",x=65,y=-2,text="calcic",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[2]]<-list(demo=list(fun="plot",call=list(xlim=c(50,80),ylim=c(-8,12),main="",bg="white",fg="black",axes=FALSE,xlab=annotate("SiO2"),ylab=annotate(MALI),cex.lab=cex.lab,new=FALSE),template=temp))

    ############
    # Plot 3   #
    ############

    p<-milli[,"P2O5"]
    if(any(is.na(p))) x<-winDialog(type="yesno","Replace missing P2O5 values with zero?")
    if (x=="YES") p[is.na(p)]<-0

    #ASI<-milli[,"Al2O3"]/(2*milli[,"CaO"]-1.67*p+milli[,"Na2O"]+milli[,"K2O"])
    ASI<-milli[,"Al2O3"]/(2*milli[,"CaO"]-3.33*p+milli[,"Na2O"]+milli[,"K2O"])
    ASI_orig<-milli[,"Al2O3"]/(milli[,"CaO"]-1.67*p+milli[,"Na2O"]+milli[,"K2O"]) 

    plate.data[[3]]$x<-ASI
    plate.data[[3]]$y<-"A/NK"
    
    temp1<-list(
        axis1=list("axis",side=1,at=c(0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9),labels=c("0.5","0.7","0.9","1.1","1.3","1.5","1.7","1.9"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=c(0.6,1,1.4,1.8,2.2,2.6,3),labels=c("0.6","1","1.4","1.8","2.2","2.6","3"),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        box=list("box",which="plot",col="black",lwd=1),
        lines1=list("abline",h=1,col=plt.col[2]),
        lines2=list("abline",v=1,col=plt.col[2]),
        GCDkit=list("NULL",plot.type="binary",plot.name="ASI - A/NK (Frost et al. 2001)")
    )

    temp2<-list(
        text1=list("text",x=0.7,y=2.8,text="metaluminous",cex=1,col=plt.col[2]),
        text2=list("text",x=1.7,y=2.8,text="peraluminous",cex=1,col=plt.col[2]),
        text3=list("text",x=0.7,y=0.9,text="peralkaline",cex=1,col=plt.col[2])
    )
    
    if (getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
     
    plate[[3]]<-list(demo=list(fun="plot",call=list(xlim=c(0.5,1.9),ylim=c(0.6,3),main="",bg="white",fg="black",axes=FALSE,xlab="ASI",ylab="A/NK",cex.lab=cex.lab,new=FALSE),template=temp))


    FeNr<-calcCore(FeNr)$r
    MALI<-calcCore(MALI)$r

    ret<-cbind(FeNr,MALI,ASI,ASI_orig)
    colnames(ret)<-c("Fe Number","MALI","ASI","ASI orig")
    if (!getOption("gcd.shut.up")) print(ret)

    assign("plate",plate,.GlobalEnv)
    assign("plate.data",plate.data,.GlobalEnv)


    ##############################
    # Classification algorithm   #
    ##############################

    if(clssf){    
    name<-vector()
    for (i in 1:nrow(WR)){
        mis<-F
        if (!is.na(FeNr[i])) if (FeNr[i]<(koef+0.0046*WR[i,"SiO2"])) tmpa<-"magnesian" else tmpa<-"ferroan"
        else {tmpa<-"missing data"; mis<-T}
        if (!is.na(MALI[i])){
            if (MALI[i]<(-41.86+1.112*WR[i,"SiO2"]-0.00572*WR[i,"SiO2"]^2)) tmpb<-"alkali-calcic" else tmpb<-"alkalic"
            if (MALI[i]<(-44.72+1.094*WR[i,"SiO2"]-0.00527*WR[i,"SiO2"]^2)) tmpb<-"calc-alkalic"
            if (MALI[i]<(-45.36+1.0043*WR[i,"SiO2"]-0.00427*WR[i,"SiO2"]^2)) tmpb<-"calcic"
        }else {tmpb<-"missing data"; mis<-T}
        if (!(is.na(ASI[i]) | is.na(milli[i,"K2O"]) | is.na(milli[i,"Na2O"]) | is.na(milli[i,"Al2O3"]))){
            if (ASI[i]>1) tmpc<-"peraluminous" else {
                if (milli[i,"K2O"]+milli[i,"Na2O"]<milli[i,"Al2O3"]) tmpc<-"metaluminous" else tmpc<-"peralkaline"
        }}else {tmpc<-"missing data"; mis<-T}
        if (mis) name<-c(name,"unclassified") else name<-c(name,paste(tmpa,tmpb,tmpc,sep="; "))
    }
       
    names(name)<-rownames(WR)
    if (!getOption("gcd.shut.up")) {
        cat("Samples classified by: Frost et al (2001)","\n")
        print(tapply(name, factor(name), length))
        cat("to view classification of individual samples type 'results'\n\n")
        flush.console()
    }
    
    results<<-name
    #groups<<-name
    #grouping<<--1
    if(GUI){
        x<-winDialog(type="yesno","Append current classification to labels?")
        if(x=="YES"){
            name<-winDialogString("Name of the new column","Frost")
            labels<<-cbind(labels,results)
            colnames(labels)[ncol(labels)]<<-name
        }
        x<-winDialog(type="yesno","View classification key?")
        if (x=="YES") browseURL(paste("file:\\\\",gcdx.dir,"\\Diagrams\\Geotectonic\\FrostTable.htm",sep=""))
    }
}
    invisible()
}

# Plotting position 103
# "Frost et al. (2001)","plotPlate(\"Frost\",clssf=TRUE,GUI=TRUE)"
