###############################################################################
#                   Classification of volcanic rocks                          #
#  Cabanis B, Lecolle M (1989) The La/10-Y/15-Nb/8 diagram: a tool for        #
#  discrimination volcanic series and evidencing continental crust magmatic   #
#  mixtures and/or contamination. Comptes rendus de l'Académie des sciences   #
#  Série 2, Mécanique, Physique, Chimie, Sciences de l'univers, Sciences de   #
#  la Terre 309:2023-2029                                                     #
###############################################################################

# Coordinates and graph layout from website of Kurt Hollocher, 
# http://minerva.union.edu/hollochk/c_petrology/discrim.html

Cabanis<-function(){
if(!getOption("gcd.plot.bw")){
    col1<-plt.col[3]
    col2<-plt.col[2]
}else{
    col1<-"black"
    col2<-"black"
}

    aa<-WR[,"La"]/10
    bb<-WR[,"Y"]/15
    cc<-WR[,"Nb"]/8
    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma
    x.data<<-cc+bb/2
    y.data<<-sqrt(3)*bb/2

    aa<-c(62,32.86,43,32.86,28.52,19.5,28.52,25.42,33,25.42,12.4,0,12.4,16.2,0,16.2,29.25,0,29.25,38)/100
    bb<-c(0,47,57,47,54,58,54,59,67,59,80,100,80,68,76.5,68,27.5,42.5,27.5,0)/100
    cc<-c(38,20.14,0,20.14,17.48,22.5,17.48,15.58,0,15.58,7.6,0,7.6,15.8,23.5,15.8,43.25,57.5,43.25,62)/100

    x<-cc + bb/2
    y<-sqrt(3)*bb/2

    temp<-list(
        lines1=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        lines2=list("lines",x=x,y=y,col=col2),
        arrows1=list("arrows",x0=0.5243,y0=0.883,x1=0.651,y1=0.680,code=3,col="gray70",length=0.15),
        arrows2=list("arrows",x0=0.651,y0=0.680,x1=0.81462,y1=0.392,code=3,col="gray70",length=0.15),
        arrows3=list("arrows",x0=0.81462,y0=0.392,x1=1.0255,y1=0.0145,code=3,col="gray70",length=0.15),
        arrows4=list("arrows",x0=-0.02,y0=0.0219,x1=0.3086,y1=0.600,code=3,col="gray70",length=0.15),
        arrows5=list("arrows",x0=0.234,y0=0.521,x1=0.462,y1=0.883,code=3,col="gray70",length=0.15),
        A=list("text",x=0,y=-0.03,text=annotate("La/10"),adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text="Y/15",adj=0.5),
        C=list("text",x=1,y=-0.03,text=annotate("Nb/8"),adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.position=302,plot.name="Cabanis + Lecolle (1989) La/10-Y/15-Nb/8")
    )

    if(getOption("gcd.plot.text")){
        temp<-c(temp,list(
            text1=list("text",x=0.5963,y=0.7762,text="N-MORB",col=col1,cex=1,srt=-60),
            text2=list("text",x=0.7452,y=0.5182,text="E-MORB",col=col1,cex=1,srt=-60),
            text3=list("text",x=0.93,y=0.1708,text="Alkaline",col=col1,cex=1,srt=-60),
            text4=list("text",x=0.097,y=0.2477,text="Calc-alkaline",col=col1,cex=1,srt=60),
            text5=list("text",x= 0.34,y=0.7167,text="Tholeiites",col=col1,cex=1,srt=60),
            text6=list("text",x=0.22,y=0.15,text="Orogenic \n(compressive)\ndomains (island \narcs,active \nmargins)",col=col2,cex=0.7),
            text7=list("text",x=0.50,y=0.15,text="Late- \nto post-\norogenic\n(compressive\nto distesive)\nintra-continent.\ndomains",col=col2,cex=0.7),
            text8=list("text",x=0.75,y=0.15,text="Anorogenic\n(distensive) domains",col=col2,cex=0.7)))
    }

    sheet<<-list(demo=list(fun="plot",
                       call=list(xlim=c(-.03,1.03),
                                 ylim=c(-0.08,1.03),
                                 main=annotate("La/10-Y/15-Nb/8 (Cabanis + Lecolle 1989)"),
                                 bg="transparent",
                                 fg="black",
                                 asp=1,
                                 axes=F,
                                 type="p",
                                 xlab="",
                                 ylab=""),
                       template=temp))  
}
