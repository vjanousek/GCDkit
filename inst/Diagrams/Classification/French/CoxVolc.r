#####################################################################
#                       TAS diagram                                 #
#                           Cox                                     #
#     Dividing line between alkaline and subalkaline series         #
#     Irvine and Baragar, 1971 Fig. 3B, p.532 - see Rickwood 1989   #
#####################################################################

CoxVolc<-function(alkline=TRUE){

template<-list(
      clssf=list("NULL",use=2:16,rcname=c("basalte","andésite basalteique","andésite","dacite","rhyolite","hawaite","trachyandésite","basanite/téphrite","mugéarite","benmoréite","trachyte","néphélinite","néphélinite phonolitique","téphrite phonolitique","phonolite")),
      lines1=list("lines",x=c(41,44,52,52,44.5,41),y=c(3,2,1.75,5.6,5.75,3),col=plt.col[2]),
      lines2=list("lines",x=c(52,55,54.75,52,52),y=c(1.75,1.75,5.5,5.6,1.75),col=plt.col[2]),
      lines3=list("lines",x=c(55,57,63,62.5,54.75,55),y=c(1.75,2,3.5,7,5.5,1.75),col=plt.col[2]),
      lines4=list("lines",x=c(63,70,65,62.5,63),y=c(3.5,5.5,9,7,3.5),col=plt.col[2]),
      lines5=list("lines",x=c(70,75,75,69.3,65,70),y=c(5.5,8,9,11.9,9,5.5),col=plt.col[2]),
      lines6=list("lines",x=c(44.5,52,53,46.4,44.5),y=c(5.75,5.6,7.2,7,5.75),col=plt.col[2]),
      lines7=list("lines",x=c(52,54.75,62.5,65,62,57,53,52),y=c(5.6,5.5,7,9,10,9,7.2,5.6),col=plt.col[2]),
      lines8=list("lines",x=c(39.5,41,44.5,46.4,48,45.35,43.5,39.5),y=c(4,3,5.75,7,8.3,9.375,8.33,4),col=plt.col[2]),
      lines9=list("lines",x=c(46.4,53,57,54.25,50.25,48,46.4),y=c(7,7.2,9,9.35,9.25,8.3,7),col=plt.col[2]),
      lines10=list("lines",x=c(50.25,54.25,57,62,57.75,54.5,50.25),y=c(9.25,9.35,9,10,11.25,11,9.25),col=plt.col[2]),
      lines11=list("lines",x=c(57.75,62,65,69.3,62,57.75),y=c(11.25,10,9,11.9,14,11.25),col=plt.col[2]),
      lines12=list("lines",x=c(36,39.5,43.5,40.5,36,36),y=c(5.9,4,8.33,9.5,6.5,5.9),col=plt.col[2]),
      lines13=list("lines",x=c(40.5,43.5,45.35,51.5,49,40.5),y=c(9.5,8.33,9.375,13.2,15,9.5),col=plt.col[2]),
      lines14=list("lines",x=c(45.35,48,50.25,54.5,51.5,45.35),y=c(9.375,8.3,9.25,11,13.2,9.375),col=plt.col[2]),
      lines15=list("lines",x=c(49,51.5,54.5,57.75,62,52.2,51.5,49),y=c(15,13.2,11,11.25,14,16.25,16.25,15),col=plt.col[2]),
      GCDkit=list("NULL",plot.type="binary",plot.position=22,plot.name="TAS (Cox et al. 1979)")
     )
if(getOption("gcd.plot.text")){
    template<-c(template,list(
        text1=list("text",x=70,y=8.5,text="Rhyolite",col=plt.col[2]),
        text2=list("text",x=65.5,y=6.5,text="Dacite",col=plt.col[2]),
        text3=list("text",x=59,y=4.5,text="Andésite",col=plt.col[2]),
        text4=list("text",x=48,y=4,text="Basalte",col=plt.col[2]),
        text5=list("text",x=64,y=11.5,text="Trachyte",col=plt.col[2]),
        text6=list("text",x=51.5,y=8,text="Mugéarite",col=plt.col[2]),
        text7=list("text",x=56,y=13.5,text="Phonolite",col=plt.col[2]),
        text8=list("text",x=49.1,y=6.3,text="Hawaite",col=plt.col[2]),
        text9=list("text", x=39.5,y=7,text="Néphélinite",col=plt.col[2]),
        text11=list("text",x=59,y=8,text="Trachy-\nandésite",col=plt.col[2],cex=1),
        text12=list("text",x=53.5,y=4,text="Andésite\nbasaltique",col=plt.col[2],cex=1,srt=90),
        text13=list("text",x=56.5,y=10,text="Benmoréite",col=plt.col[2],cex=1),
        text14=list("text",x=45,y=11,text="Néphélinite\nphonolitique",col=plt.col[2],cex=1,srt=45),
        text15=list("text",x=50,y=10.4,text="Téphrite\nphonolitique",col=plt.col[2],cex=1,srt=45),
        text16=list("text",x=43,y=6,text="Basanite\ntephrite",col=plt.col[2],cex=1,srt=45)
        )
    )
}

if(alkline){
    template<-c(template,list(
        lines14=list("lines",x=c(39.2,40,43.2,45,48,50,53.7,55,60,65,77.4),
            y=c(0,0.4,2,2.8,4,4.75,6,6.4,8,8.8,10),col=plt.col[3]),
        lines15=list("abline",v=45,lty="dashed",col=plt.col[3]),
        lines16=list("abline",v=52,lty="dashed",col=plt.col[3]),
        lines15=list("abline",v=63,lty="dashed",col=plt.col[3])
        )
    )

    if(getOption("gcd.plot.text")){
        template<-c(template,list(
            text11=list("text",x=39,y=2,text="Alcalin",col=plt.col[3],font=4),
            text12=list("text",x=42,y=1,text="Subalcalin/Tholéitique",col=plt.col[3],adj=0,font=4),
            text13=list("text",x=40,y=16,text="Ultrabasique",col=plt.col[3]),
            text14=list("text",x=49,y=16,text="Basique",col=plt.col[3]),
            text15=list("text",x=58,y=16,text="Intermédiaire",col=plt.col[3]),
            text16=list("text",x=68,y=16,text="Acide",col=plt.col[3])
            )
        )
    }
}

x.data<<-WR[,"SiO2"]
y.data<<-WR[,"Na2O"]+WR[,"K2O"]
sheet<<-list(demo=list(fun="plot", call=list(xlim=c(35,73),ylim=c(0,17),main=annotate("TAS (Cox et al. 1979)"),col="green",bg="transparent",fg="black",xlab=annotate("SiO2"),ylab=annotate("Na2O+K2O")),template=template))
}
