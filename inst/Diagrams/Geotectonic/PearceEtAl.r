#####################################################################
#                  PearceEtAl. 1977, sour. NewPet                 #
#####################################################################
PearceEtAl<-function(){
if(!getOption("gcd.plot.bw")){
    col1<-plt.col[1]
    col2<-plt.col[2]
}else{
    col1<-"black"
    col2<-"black"
}

    aa<-WR[,"MgO"]
    bb<-WR[,"FeOt"]
    cc<-WR[,"Al2O3"] 
    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma
    x.data<<-cc+bb/2
    y.data<<-sqrt(3)*bb/2
    
temp<-list(
        lines1=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        lines2=list("lines",x=c(0.460,0.625,0.605),y=c(0.2771281,0.2771281,0.4416730),col=col2,lty="dashed"),
        lines3=list("lines",x=c(0.625,0.6525,0.65,0.645),y=c(0.2771281,0.238157,0.2078461,0.1299038),col=col2,lty="dashed"),
        lines4=list("lines",x=c(0.6525,0.655,0.67,0.6875,0.715,0.7425,0.795),
            y=c(0.238157,0.2684679,0.2944486,0.2987788,0.2857884,0.2468172,0.1818653),col=col2,lty="dashed"),
        lines5=list("lines",x=c(0.6875,0.686,0.685,0.675),y=c(0.2987788,0.3290897,0.3723909,0.4243524),col=col2,lty="dashed"),
              
        A=list("text",x=0,y=-0.03,text="MgO",adj=0.5),
        C=list("text",x=1,y=-0.03,text=annotate("Al2O3"),adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text=annotate("FeO^T"),adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.position=207,plot.name="Pearce et al. (1977) MgO-FeOt-Al2O3")
        )
#temp2<-list(
#        text4=list("text",x=0.744,y=0.327,text="1",col=col2),
#        text5=list("text",x=0.708,y=0.194,text="2",col=col2),
#       text6=list("text",x=0.555,y=0.209,text="3",col=col2),
#       text7=list("text",x=0.530,y=0.366,text="4",col=col2),
#       text8=list("text",x=0.648,y=0.355,text="5",col=col2)
#        )

if(getOption("gcd.plot.text")){
    temp<-c(temp,list(
       text4=list("text",x=1.05,y=0.327,adj=1,text="Spreading Center\nIsland",col=col2,cex=0.8),
       text5=list("text",x=0.73,y=0.15,text="Orogenic",col=col2,cex=0.8),
       text6=list("text",x=0.6,y=0.209,adj=1,text="Ocean Ridge\nand Floor",col=col2,cex=0.8),
       text7=list("text",x=0.59,y=0.366,adj=1,text="Ocean Island",col=col2,cex=0.8),
       text8=list("text",x=0.65,y=0.4,srt=90,text="Continental",col=col2,cex=0.8)
       ))
}


sheet<<-list(demo=list(fun="plot",
                       call=list(xlim=c(-.03,1.03),
                                 ylim=c(-0.08,1.03),
                                 main=annotate("Pearce et al. (1977)"),
                                 bg="transparent",
                                 fg="black",
                                 asp=1,
                                 axes=F,
                                 type="p",
                                 xlab="",
                                 ylab=""),
  template=temp))  


#x<-c("1 = Spreading Center Island","2 = Orogenic", "3 = Ocean Ridge and Floor", "4 = Ocean Island","5 = Continetal")
#legend(-0.1,1.02, x, col = "black",cex=0.7)

}
