#####################################################################
#                 Jensen plot - Jensen 1976                         #
#    field boundaries Rickwood 1989, corrected by Rollinson 1993    #
#####################################################################

Jensen<-function(){

    aa<-milli[,"Al2O3"]
    #bb<-milli[,"FeO"]+milli[,"Fe2O3"]+milli[,"TiO2"]
    bb<-WR[,"FeOt"]/MW["FeO"]*1000+milli[,"TiO2"]
    cc<-milli[,"MgO"]

    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma
    x.data<<-cc+bb/2
    y.data<<-sqrt(3)*bb/2


template=list(
        clssf=list("NULL",use=2:12,rcname=c("TH - Rhyolite","TH - Dacite","TH - Andesite","CA - Rhyolite","CA - Dacite","CA - Andesite","CA - Basalt","TH - high-Mg tholeiite basalt","KO - Komatiitic basalt","KO - Komatiite s.s.","TH - high-Fe tholeiite basalt")),
        lines1=list("NULL",x=c(0.05,0.19932,0.15,0.05),y=c(0.0866,0.17439,0.25981,0.0866)),
        lines2=list("NULL",x=c(0.19932,0.27397,0.2,0.15,0.19932),y=c(0.17439,0.21829,0.34641,0.25981,0.17439)),
        lines3=list("NULL",x=c(0.27397,0.3225,0.33,0.34,0.3325,0.325,0.25,0.2,0.27395),y=c(0.21829,0.24682,0.25115,0.25115,0.29012,0.30311,0.43301,0.34641,0.21829)),
        lines4=list("NULL",x=c(0.2,0.12466,0.05,0,0.2),y=c(0,0.13050,0.0866,0,0)),
        lines5=list("NULL",x=c(0.3,0.19932,0.12466,0.2,0.3),y=c(0,0.17439,0.13050,0,0)),
        lines6=list("NULL",x=c(0.3,0.4,0.27397,0.19932,0.3),y=c(0,0,0.21829,0.17439,0)),
        lines7=list("NULL",x=c(0.4225,0.392,0.372,0.3575,0.34,0.33,0.3225,0.27397,0.4,0.4225),y=c(0.10825,0.17321,0.21651,0.23816,0.25115,0.25115,0.24682,0.21829,0,0.10825)),
        lines8=list("NULL",x=c(0.5,0.5005,0.375,0.37263,0.392,0.4225,0.5),y=c(0,0.28839,0.21651,0.21515,0.17321,0.10825,0)),
        lines9=list("NULL",x=c(0.6,0.8,0.5,0.5,0.6),y=c(0,0.34641,0.47631,0,0)),
        lines10=list("NULL",x=c(1,0.8,0.6,1),y=c(0,0.34641,0,0)),
        lines11=list("NULL",x=c(0.37263,0.375,0.5005,0.5,0.25,0.325,0.3325,0.34,0.3579,0.372,0.37263),y=c(0.21515,0.21651,0.28839,0.47631,0.43301,0.30311,0.29012,0.25115,0.23816,0.21651,0.21515)),

        lines21=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        lines22=list("lines",x=c(0.5,0.5),y=c(0,0.476314),col=plt.col[2]),
        lines23=list("lines",x=c(0.0500,0.3225,0.3300,0.3400,0.3575,0.3720,0.3920,0.4225),
            y=c(0.08660254,0.24681724,0.25114737,0.25114737,0.23815699,0.21650635,0.17320508,0.10825318),
            col=plt.col[2]),
        lines24=list("lines",x=c(0.6,0.8),y=c(0,0.3464102),col=plt.col[2],lty="dashed"),
#        lines25=list("lines",x=c(0.365,0.443),y=c(0.2598076,0.3464102),col=plt.col[2],lty="dashed"),  # variation proposed by Viljoen et al (1992)
#        lines26=list("lines",x=c(0.320,0.443),y=c(0.3983717,0.3464102),col=plt.col[2],lty="dashed"),  # dtto
        lines27=list("lines",x=c(0.5005,0.3750),y=c(0.2883865,0.2165064),col=plt.col[2],lty="dashed"),
        lines28=list("lines",x=c(0.250,0.325),y=c(0.4330127,0.3031089),col=plt.col[2],lty="dashed"),
        lines29=list("lines",x=c(0.3325,0.3400),y=c(0.2901185,0.2511474),col=plt.col[2],lty="dashed"),
        lines30=list("lines",x=c(0.20,0.35),y=c(0.34641016,0.08660254),col=plt.col[2],lty="dashed"),
        lines31=list("lines",x=c(0.15,0.30),y=c(0.2598076,0),col=plt.col[2],lty="dashed"),
        lines32=list("lines",x=c(0.1,0.2),y=c(0.1732051,0),col=plt.col[2],lty="dashed"),

        A=list("text",x=0,y=-0.03,text="Al",adj=0.5),
        C=list("text",x=1,y=-0.03,text="Mg",adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text=annotate("Fe^T+Ti"),adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.position=24,plot.name="Jensen (1976)")
)
  if(getOption("gcd.plot.text")){
        template<-c(template,list(
        text4=list("text",x=0.39,y=0.40,text="High-Fe\ntholeiite\nbasalt",cex=0.85,col=plt.col[2]),
        text5=list("text",x=0.26,y=0.32,text="Andesite",srt=-60,cex=0.85,col=plt.col[2]),
        text6=list("text",x=0.20,y=0.25,text="Dacite",srt=-60,cex=0.85,col=plt.col[2]),
        text7=list("text",x=0.12,y=0.17,text="Rhyolite",srt=30,cex=0.7,col=plt.col[2]),
        text8=list("text",x=0.095,y=0.046,text="Rhyolite",srt=-60,cex=0.7,col=plt.col[2]),
        text9=list("text",x=0.20,y=0.074,text="Dacite",srt=-60,cex=0.85,col=plt.col[2]),
        text10=list("text",x=0.285,y=0.117,text="Andesite",srt=-60,cex=0.85,col=plt.col[2]),
        text11=list("text",x=0.35,y=0.163,text="Basalt",srt=-60,cex=0.85,col=plt.col[2]),
        text12=list("text",x=0.445,y=0.17,text="High-Mg\ntholeiite\nbasalt",cex=0.7,col=plt.col[2]),
        text13=list("text",x=0.625,y=0.29,text="Komatiitic\nbasalt",srt=-60,col=plt.col[2]),
        text14=list("text",x=0.80,y=0.12,text="Komatiite",srt=-60,col=plt.col[2]),

        text15=list("text",x=0.414,y=0.30,text="TH",col=plt.col[2]),
        text16=list("text",x=0.368,y=0.043,text="CA",col=plt.col[2]),
        text17=list("text",x=0.75,y=0.27,text="KOMATIITE",srt=-60,col=plt.col[2])))

}


sheet<<-list(demo=list(fun="plot",call=list(xlim=c(-.03,1.03),ylim=c(-0.05,1.03),main=annotate("Jensen (1976)"),bg="transparent",fg="black",asp=1,axes=FALSE,xlab="",ylab=""),template=template))
}
