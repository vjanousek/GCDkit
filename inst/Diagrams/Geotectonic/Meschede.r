#####################################################################
#                      Classification of basalts                    #
#            Meschede,1986 coordinates from Rollison 1993           #
#####################################################################


Meschede<-function(){

if(!getOption("gcd.plot.bw")){
    col1<-plt.col[1]
    col2<-plt.col[2]
}else{
    col1<-"black"
    col2<-"black"
}

    aa<-WR[,"Zr"]/4
    bb<-2*WR[,"Nb"]
    cc<-WR[,"Y"]
    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma
    x.data<<-cc+bb/2
    y.data<<-sqrt(3)*bb/2


temp<-list(
        lines1=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        lines2=list("lines",x=c(0.500,0.340,0.305,0.465,0.770),
            y=c(0,0.1039230,0.2944486,0.6841601,0),col=col2),
        lines3=list("lines",x=c(0.3400,0.4200,0.5525),y=c(0.1039230,0.4330127,0.4893044),col=col2,lty="dashed"),
        lines4=list("lines",x=c(0.34,0.45,0.58),y=c(0.1039230,0.3031089,0.4330127),col=col2,lty="dashed"),
        lines5=list("lines",x=c(0.450,0.485,0.520,0.455),y=c(0.30310889,0.26846788,0.19052559,0.02598076),col=col2,lty="dashed"),
        lines6=list("lines",x=c(0.545,0.680),y=c(0.2078461,0.1905256),col=col2,lty="dashed"),
        A=list("text",x=0,y=-0.03,text="Zr/4",adj=0.5),
        C=list("text",x=1,y=-0.03,text="Y",adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text=annotate("2*Nb"),adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.position=203,plot.name="Meschede (1986) Zr/4-2Nb-Y")
        )

if(getOption("gcd.plot.text")){
    temp<-c(temp,list(
        text4=list("text",x=0.46,y=0.543,text="AI",col=col2,cex=0.8),
        text5=list("text",x=0.456,y=0.387,text="AII",col=col2,cex=0.8),
        text6=list("text",x=0.566,y=0.302,text="B",col=col2,cex=0.8),
        text7=list("text",x=0.431,y=0.163,text="C",col=col2,cex=0.8),
        text8=list("text",x=0.605,y=0.092,text="D",col=col2,cex=0.8)
        ))
}


sheet<<-list(demo=list(fun="plot",
                       call=list(xlim=c(-.03,1.03),
                                 ylim=c(-0.08,1.03),
                                 main=annotate("Zr/4-2Nb-Y (Meschede 1986)"),
                                 bg="transparent",
                                 fg="black",
                                 asp=1,
                                 axes=F,
                                 type="p",
                                 xlab="",
                                 ylab=""),
                       template=temp))  
}
