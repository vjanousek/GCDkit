Harris<-function(){
if(!getOption("gcd.plot.bw")){
    col1<-plt.col[1]
    col2<-plt.col[2]
}else{
    col1<-"black"
    col2<-"black"
}

    aa<-WR[,"Hf"]
    bb<-WR[,"Rb"]/30
    cc<-WR[,"Ta"]*3
    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma
    x.data<<-cc+bb/2
    y.data<<-sqrt(3)*bb/2

temp<-list(
        lines1=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        lines2=list("lines",x=c(0.617,0.5,0.479,0.453,0.904),
            y=c(0.664,0.624,0.44,0.194,0.169),col=col2),
        lines3=list("lines",x=c(0.479,0.85),y=c(0.44,0.261),col=col2),
        lines4=list("lines",  x=c(0.255,0.322,0.391,0.456),y=c(0.054,0.136,0.194,0.227),col=col2),
        A=list("text",x=0,y=-0.03,text="Hf",adj=0.5),
        C=list("text",x=1,y=-0.03,text=annotate("3Ta"),adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text="Rb/30",adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.position=106,plot.name="Harris et al. (1986)")
        )      
if(getOption("gcd.plot.text")){ 
    temp<-c(temp,list(
        text4=list("text",x=0.323,y=0.314,text="VA",col=col2),
        text5=list("text",x=0.6,y=0.087,text="WP",col=col2),
        text6=list("text",x=0.6,y=0.477,text="Group 2",col=col2),
        text7=list("text",x=0.6,y=0.268,text="Group 3",col=col2)
    ))
}


sheet<<-list(demo=list(fun="plot",
             call=list(xlim=c(-.03,1.03),
                       ylim=c(-0.05,1.03),
                       main=annotate("Hf-Rb/30-3Ta (Harris et al. 1986)"),
                       bg="transparent",
                       fg="black",
                       asp=1,
                       axes=F,
                       type="p",
                       xlab="",
                       ylab=""),
             template=temp))  
}
