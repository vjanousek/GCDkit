#####################################################################
#                                                                   #
#     Grebennikov AV (2014) A-type granites and related rocks:      #
#  petrogenesis and classification. Russ Geol Geophys 55:1353-1366  #
#                                                                   #
#####################################################################

Grebennikov<-function(){
    if(!getOption("gcd.plot.bw")){
        col1<-plt.col[3]
        col2<-plt.col[2]
    }else{
        col1<-"black"
        col2<-"black"
    }
    
    aa<-5*WR[,"FeOt"]*1.11135/MW["Fe2O3"]
    bb<-WR[,"Na2O"]/MW["Na2O"] + WR[,"K2O"]/MW["K2O"]
    cc<-5*(WR[,"CaO"]/MW["CaO"]+WR[,"MgO"]/MW["MgO"])

    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma
    x.data<<-cc+bb/2
    y.data<<-sqrt(3)*bb/2
    
    temp<-list(
        lines1=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        lines2=list("lines",x=c(0.200,0.225,0.505,0.440,0.330,0.200),y=c(0.3464102,0.1818653,0.1818653,0.5369358,0.5715768,0.3464102),col=col1),
        lines3=list("lines",x=c( 0.330,0.440,0.505,0.635,0.605,0.475,0.330),y=c(0.5715768,0.5369358,0.1818653,0.1645448,0.3897114,0.8227241,0.5715768),col=col1),     
        A=list("text",x=0,y=-0.03,text=annotate("5*Fe2O3t"),adj=0.5),
        C=list("text",x=1,y=-0.03,text=annotate("5*(CaO+MgO)"),adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text=annotate("Na2O+K2O"),adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.position=100,plot.name="Grebennikov (2014) 5Fe2O3t - Na2O+K2O - 5(CaO+MgO)")
    )

    if(getOption("gcd.plot.text")){
        temp<-c(temp,list(
            text1=list("text",x=0.35,y=0.3,text=expression(A[1]),col=col2,cex=1),
            text5=list("text",x=0.54,y=0.3,text=expression(A[2]),col=col2,cex=1)
        ))
    }
    
     sheet<<-list(demo=list(fun="plot",
                       call=list(xlim=c(-.03,1.03),
                                 ylim=c(-0.08,1.03),
                                 main=annotate("5Fe2O3t-Na2O+K2O-5(CaO+MgO) (Grebennikov 2014)"),
                                 bg="transparent",
                                 fg="black",
                                 asp=1,
                                 axes=F,
                                 type="p",
                                 xlab="",
                                 ylab=""),
                       template=temp))  
}
    
