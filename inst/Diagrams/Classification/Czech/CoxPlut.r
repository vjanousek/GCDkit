#####################################################################
#                  TAS diagram for magmatites                       #
#             Cox et al. 1979 adapted by Wilson 1989                #
#     Dividing line between alkaline and subalkaline series         #
#     Irvine and Baragar, 1971 Fig. 3B , p.532 - see Rickwood 1989  #
#####################################################################
CoxPlut<-function(alkline=TRUE){

template<-list(
      clssf=list("NULL",use=2:12,rcname=c("gabro","gabro","diorit","kvarc diorit (granodiorit)","granit","nedefinováno","nedefinováno","syenodiorit","syenit","ijolit","nefelinický syenit")),
      lines1=list("lines",x=c(41,44,52,52,53,46.4,44.5,41),y=c(3,2,1.75,5.6,7.2,7,5.75,3),col=plt.col[2]),
      lines2=list("lines",x=c(52,55,54.75,52,52),y=c(1.75,1.75,5.5,5.6,1.75),col=plt.col[2]),
      lines3=list("lines",x=c(55,57,63,62.5,54.75,55),y=c(1.75,2,3.5,7,5.5,1.75),col=plt.col[2]),
      lines4=list("lines",x=c(63,70,65,62.5,63),y=c(3.5,5.5,9,7,3.5),col=plt.col[2]),
      lines5=list("lines",x=c(70,75,75,69.3,65,70),y=c(5.5,8,9,11.9,9,5.5),col=plt.col[2]),
      lines6=list("lines",x=c(52,54.75,62.5,65,62,57,53,52),y=c(5.6,5.5,7,9,10,9,7.2,5.6),col=plt.col[2]),
      lines7=list("lines",x=c(39.5,41,44.5,46.4,48,50.25,54.5,51.5,49,40.5,43.5,39.5),
                          y=c(4,3,5.75,7,8.3,9.25,11,13.2,15,9.5,8.33,4),col=plt.col[2]),
      lines8=list("lines",x=c(46.4,53,57,54.25,50.25,48,46.4),y=c(7,7.2,9,9.35,9.25,8.3,7),col=plt.col[2]),
      lines9=list("lines",x=c(50.25,54.25,57,62,65,69.3,62,57.75,54.5,50.25),y=c(9.25,9.35,9,10,9,11.9,14,11.25,11,9.25),col=plt.col[2]),
      lines10=list("lines",x=c(36,39.5,43.5,40.5,36,36),y=c(5.9,4,8.33,9.5,6.5,5.9),col=plt.col[2]),
      lines11=list("lines",x=c(49,51.5,54.5,57.75,62,52.2,51.5,49),y=c(15,13.2,11,11.25,14,16.25,16.25,15),col=plt.col[2]),
      lines12=list("lines",x=c(57.75,62),y=c(11.25,10),col=plt.col[2]),
      lines13=list("lines",x=c(43.5,45.35,51.5),y=c(8.33,9.375,13.2),col=plt.col[2]),
      lines14=list("lines",x=c(45.35,48),y=c(9.375,8.3),col=plt.col[2]),
      lines15=list("lines",x=c(44.5,52),y=c(5.75,5.6),col=plt.col[2]),
      GCDkit=list("NULL",plot.type="binary",plot.position=31,plot.name="TAS (Cox et al. 1979) ")
       )

if(getOption("gcd.plot.text")){
    template<-c(template,list(
        text1=list("text",x=70,y=8.5,text="Granit",col=plt.col[2]),
        text2=list("text",x=66,y=6,text="Kvarc\ndiorit\n(granodiorit)",col=plt.col[2]),
        text3=list("text",x=59,y=4.5,text="Diorit",col=plt.col[2]),
        text4=list("text",x=48.5,y=3.5,text="Gabro",col=plt.col[2]),
        text5=list("text",x=64,y=11.5,text="Syenit",col=plt.col[2]),
        text6=list("text",x=51.5,y=8.3,text="Syeno-\ndiorit",col=plt.col[2]),
        text7=list("text",x=56,y=13.5,text="Nefelinický\nsyenit",col=plt.col[2]),
        text8=list("text",x=49.5,y=6.2,text="Gabro",col=plt.col[2]),
        text9=list("text", x=39,y=6.5,text="Ijolit",col=plt.col[2]),
        text10=list("text",x=56.5,y=10,text="Syenit",col=plt.col[2])
        )
    )
}

if(alkline){
    template<-c(template,list(
        lines16=list("lines",x=c(39.2,40,43.2,45,48,50,53.7,55,60,65,77.4),
            y=c(0,0.4,2,2.8,4,4.75,6,6.4,8,8.8,10),col=plt.col[3]),
        lines17=list("abline",v=45,lty="dashed",col=plt.col[3]),
        lines18=list("abline",v=52,lty="dashed",col=plt.col[3]),
        lines19=list("abline",v=63,lty="dashed",col=plt.col[3])
        )
    )

    if(getOption("gcd.plot.text")){
        template<-c(template,list(
            text11=list("text",x=38.5,y=2,text="Alkalický",col=plt.col[3],font=4),
            text12=list("text",x=42,y=1,text="Subalkalický/Tholeiitický",col=plt.col[3],adj=0,font=4),
            text13=list("text",x=40,y=16,text="Ultrabazický",col=plt.col[3]),
            text14=list("text",x=49,y=16,text="Bazický",col=plt.col[3]),
            text15=list("text",x=58,y=16,text="Intermediální",col=plt.col[3]),
            text16=list("text",x=68,y=16,text="Kyselý",col=plt.col[3])
            )
        )
    }
}
x.data<<-WR[,"SiO2"]
y.data<<-WR[,"Na2O"]+WR[,"K2O"]
sheet<<-list(demo=list(fun="plot",call=list(xlim=c(35,76),ylim=c(0,17),col="green",main=annotate("TAS (Cox et al. 1979)"),bg="transparent",fg="black",xlab=annotate("SiO2"),ylab=annotate("Na2O+K2O")),template=template))
}
