
Mullen<-function(){
    if(!getOption("gcd.plot.bw")){
        col1<-plt.col[3]
        col2<-plt.col[2]
    }else{
        col1<-"black"
        col2<-"black"
    }
    what<-which(WR[,"SiO2"]<54 & WR[,"SiO2"]>45)
    #what<-rownames(WR)[WR[,"SiO2"]<54 & WR[,"SiO2"]>45]
    #what<-what[!is.na(what)]

    aa<-WR[what,"MnO"]*10
    bb<-WR[what,"TiO2"]
    cc<-WR[what,"P2O5"]*10
    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma
    x.data<<-cc+bb/2
    y.data<<-sqrt(3)*bb/2


    temp<-list(
        lines1=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        lines2=list("lines",x=c(0.295,0.455,0.585,0.700,0.920),y=c(0.5109550,0.2338269,0.2338269,0.1558846,0),col=col2,lty="dashed"),
        lines3=list("lines",x=c(0.385,0.555,0.585),y=c(0.6668396,0.2511474,0.2338269),col=col2),
        lines4=list("lines",x=c(0.195,0.300,0.700),y=c(0.3377499,0.1558846,0.1558846),col=col2,lty="dashed"),
        lines5=list("lines",x=c(0.585,0.775),y=c(0.2338269,0.3897114),col=col2,lty="dashed"),     
        A=list("text",x=0,y=-0.03,text=annotate("10*MnO"),adj=0.5),
        C=list("text",x=1,y=-0.03,text=annotate("10*P2O5"),adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text=annotate("TiO2"),adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.position=204,plot.name="Mullen (1983) 10MnO-TiO2-10P2O5")
    )

    if(getOption("gcd.plot.text")){
        temp<-c(temp,list(
            text14=list("text",x=0.45,y=0.1,text="CAB",col=col2,cex=0.8),
            text5=list("text",x=0.55,y=0.5,text="OIT",col=col2,cex=0.8),
            text6=list("text",x=0.43,y=0.4,text="MORB",col=col2,cex=0.8),
            text7=list("text",x=0.32,y=0.3,text="IAT",col=col2,cex=0.8),
            text8=list("text",x=0.77,y=0.22,text="OIA",col=col2,cex=0.8),
            text9=list("text",x=0.2,y=0.15,text="Bon",col=col2,cex=0.8),
            text10=list("text",x=0.8,y=0.9,text=expression(45*symbol(" ")*wt.*symbol("%") < {SiO[2]}),col=col1),
            text11=list("text",x=0.8,y=0.85,text=expression({SiO[2]} < 54*symbol(" ")*wt.*symbol("%")),col=col1)
        ))
    }

    sheet<<-list(demo=list(fun="plot",
                       call=list(xlim=c(-.03,1.03),
                                 ylim=c(-0.08,1.03),
                                 main=annotate("10MnO-TiO2-10P2O5 (Mullen 1983)"),
                                 bg="transparent",
                                 fg="black",
                                 asp=1,
                                 axes=F,
                                 type="p",
                                 xlab="",
                                 ylab=""),
                       template=temp))  
}
