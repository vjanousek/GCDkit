#####################################################################
#                 AFM diagram - Th vs CA rock series                #
#                                                                   #
#####################################################################

AFM<-function(equ=FALSE){
    aa<-WR[,"K2O"]+WR[,"Na2O"]
    bb<-WR[,"FeOt"]
    cc<-WR[,"MgO"]
    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma
    x.data<<-cc+bb/2
    y.data<<-sqrt(3)*bb/2


if(equ){
    #Irvine and Baragar 1971: equation p. 547
    #aa<-c(70,62.1,47.4,34.3,25.7,21.1,19.2,18.3,17.5,16.1,14.4,12.6,11.2,10.4,10.1,10.3,10.3,10,9.2)
    #bb<-c(30,32.9,42.6,50.7,54.3,53.9,50.8,46.7,42.5,38.9,35.6,32.4,28.8,24.6,19.9,14.7,9.7,5,0.8)

    xx<-c(0.1500,0.2145,0.3130,0.4035,0.4715,0.5195,0.5540,0.5835,0.6125,0.6445,0.6780,0.7120,0.7440,0.7730,0.7995,0.8235,0.8485,0.8750,0.9040,0.90952)
    yy<-c(0.259807621,0.284922358,0.368926822,0.439074880,0.470251794,0.466787693,0.439940905,0.404433864,0.368060797,0.336883882,0.308305044,0.280592231,0.249415316,0.213042249,0.172339055,0.127305734,0.084004464,0.043301270,0.006928203,0)
temp1<-list(
        clssf=list("NULL",use=2:3,rcname=c("vápenato-alkalická série","tholeiitová série")),
        lines0=list("NULL",x=c(0,xx,0),y=c(0,yy,0),lwd=3,col=plt.col[2]),
        lines1=list("NULL",x=c(1,0.5,xx,1),y=c(0,sqrt(3)/2,yy,0),lwd=3,col="red"),
        lines2=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        lines3=list("lines",x=xx,y=yy,col=plt.col[2]),
        A=list("text",x=0,y=-0.05,text="A",adj=0.5),
        C=list("text",x=1,y=-0.05,text="M",adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text="F",adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.position=11,plot.name="AFM (Irvine + Baragar 1971)")
        )

}else{
    #Irvine and Baragar, 1971 Fig. 2A , p.528 - see Rickwood 1989 
    #aa<-c(58.8,47.6,29.6,25.4,21.4,19.4,18.9,16.6,15)
    #bb<-c(36.2,42.4,53.6,54.6,54.6,52.8,51.1,43.4,35)
    #boundaries outside range given by authors, used for classification, were computed by linear extrapolation
    xx<-c(0.2310,0.3120,0.4360,0.4730,0.5130,0.5420,0.5555,0.6170,0.6750)
    yy<-c(0.3135012,0.3671948,0.4641896,0.4728499,0.4728499,0.4572614,0.4425390,0.3758550,0.3031089)

temp1<-list(
        clssf=list("NULL",use=2:3,rcname=c("vápenato-alkalická série","tholeiitová série")),
        lines0=list("NULL",x=c(0,0.15,xx,0.91667,0),y=c(0,0.25981,yy,0,0),lwd=3,col=plt.col[2]),
        lines1=list("NULL",x=c(1,0.5,0.15,xx,0.91667,1),y=c(0,sqrt(3)/2,0.25981,yy,0,0),lwd=3,col="red"),
        lines2=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        lines3=list("lines",x=xx,y=yy,col=plt.col[2]),
        A=list("text",x=0,y=-0.03,text="A",adj=0.5),
        C=list("text",x=1,y=-0.03,text="M",adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text="F",adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.position=11,plot.name="AFM (Irvine + Baragar 1971)")
        )
}
temp2<-list(
        text4=list("text",x=0.5,y=0.55,text="Tholeiitová série",col=plt.col[2]),
        text5=list("text",x=0.5,y=0.1,text="Vápenato-alkalická série",col=plt.col[2])
        )

if(getOption("gcd.plot.text")){
    temp<-c(temp1,temp2)
    }else{
    temp<-temp1
}

sheet<<-list(demo=list(fun="plot",call=list(xlim=c(-.03,1.03),ylim=c(-0.05,1.03),main=annotate("AFM diagram (Irvine a Baragar 1971)"),xlab="",ylab="",bg="transparent",fg="black",asp=1,axes=FALSE),template=temp))

}
