#####################################################################
#              TAS diagram - LeBas and Streckeisen (1991)           #
#           dividing line between alkaline and subalkaline          #
#               series after Irvine and Baragar (1971)              #
#####################################################################

TAS<-function(cutoff=95){

temp1<-list(
        clssf=list("NULL",use=2:16,rcname=c("feldespatoide","picrobasaltoo","basalto","andesita  basáltica","andesita","dacita","riolita","traquibasaltoo","traquiandesita basáltica","traquiandesita","traquita/traquidacita","tefrita/basanita","fonotefrita","tefrifonolita","fonolita")),
        lines1=list("lines",x=c(30,41,41,45,48.4,52.5,30),y=c(0,0,7,9.4,11.5,14,24.15),col=plt.col[2]),
        lines2=list("lines",x=c(41,45,45,41),y=c(0,0,3,3),col=plt.col[2]),
        lines3=list("lines",x=c(45,52,52,45),y=c(0,0,5,5),col=plt.col[2]),
        lines4=list("lines",x=c(52,57,57,52),y=c(0,0,5.9,5),col=plt.col[2]),
        lines5=list("lines",x=c(57,63,63,57),y=c(0,0,7,5.9),col=plt.col[2]),
        lines6=list("lines",x=c(63,77,69,63),y=c(0,0,8,7),col=plt.col[2]),
        lines7=list("lines",x=c(77,100,100,69,69),y=c(0,0,25,25,8),col=plt.col[2]),
        lines8=list("lines",x=c(45,52,49.4),y=c(5,5,7.3),col=plt.col[2]),
        lines9=list("lines",x=c(52,57,53,49.4),y=c(5,5.9,9.3,7.3),col=plt.col[2]),
        lines10=list("lines",x=c(57,63,57.6,53),y=c(5.9,7,11.7,9.3),col=plt.col[2]),
        lines11=list("lines",x=c(63,69,69,57.6),y=c(7,8,17.73,11.7),col=plt.col[2]),
        lines12=list("lines",x=c(41,45,45,49.4,45,41),y=c(3,3,5,7.3,9.4,7),col=plt.col[2]),
        lines13=list("lines",x=c(49.4,53,48.4,45),y=c(7.3,9.3,11.5,9.4),col=plt.col[2]),
        lines14=list("lines",x=c(53,57.6,52.5,48.4),y=c(9.3,11.7,14,11.5),col=plt.col[2]),
        lines15=list("lines",x=c(57.6,69,30),y=c(11.7,17.73,24.15),col=plt.col[2]),
        lines16=list("abline",v=45,lty="dashed",col=plt.col[3]),
        lines17=list("abline",v=52,lty="dashed",col=plt.col[3]),
        lines18=list("abline",v=63,lty="dashed",col=plt.col[3]),
        lines19=list("lines",x=c(39.2,40,43.2,45,48,50,53.7,55,60,65,77.4),y=c(0,0.4,2,2.8,4,4.75,6,6.4,8,8.8,10),lty="dashed",col=plt.col[2]),
        GCDkit=list("NULL",plot.type="binary",plot.position=21,plot.name="TAS (Le Bas et al. 1986)")
)

temp2<-list(
        text1=list("text",x=43,y=1.55,text="Picrobasalto",col=plt.col[2],srt=90,cex=0.8),
        text2=list("text",x=48.5,y=2.8,text="Basalto",col=plt.col[2],srt=90),
        text3=list("text",x=54.8,y=3,text="Andesita \nbasáltica",col=plt.col[2],srt=90),
        text4=list("text",x=59.9,y=3,text="Andesita",col=plt.col[2],srt=90),
        text5=list("text",x=67,y=3,text="Dacita",col=plt.col[2],srt=90),
        text6=list("text",x=75,y=8.3,text="Riolita",col=plt.col[2]),
        text7=list("text",x=63.5,y=11.2,text="Traquita\nTraquidacita",col=plt.col[2]),
        text8=list("text",x=57.8,y=8.5,text="Traqui-\nandesita",col=plt.col[2]),
        text9=list("text",x=52.95,y=7,text="Traqui-\nandesita\nbasáltica",col=plt.col[2],cex=0.8),
        text10=list("text",x=49.2,y=5.65,text="Traqui-\nbasalto",col=plt.col[2],cex=0.8),
        text11=list("text",x=45,y=7,text="Tefrita\nBasanita",col=plt.col[2]),
        text12=list("text",x=49.2,y=9.3,text="Fono-\ntefrita",col=plt.col[2]),
        text13=list("text",x=53,y=11.5,text="Tefri-\nfonolita",col=plt.col[2]),
        text14=list("text",x=57,y=14,text="Fonolita",col=plt.col[2]),
        text15=list("text",x=43,y=12,text="feldespatoide",col=plt.col[2]),
        text16=list("text",x=38,y=2,text="Alcalína",col=plt.col[2],srt=45),
        text17=list("text",x=55,y=1,text="subalcalino/Toleítico",col=plt.col[2],srt=5),
        text18=list("text",x=41,y=15.5,text="Ultrabásica",col=plt.col[3]),
        text19=list("text",x=49,y=15.5,text="Básica",col=plt.col[3]),
        text20=list("text",x=58,y=15.5,text="Intermedia",col=plt.col[3]),
        text21=list("text",x=67,y=15.5,text="Ácida",col=plt.col[3])
        )

if(getOption("gcd.plot.text")){
    temp<-c(temp1,temp2)}
    else{
    temp<-temp1
}
x.data<<-WRanh[,"SiO2"]
y.data<<-WRanh[,"Na2O"]+WRanh[,"K2O"]
sheet<<-list(demo=list(fun="plot",call=list(xlim=c(35,80),ylim=c(0,16),main=annotate("TAS (Le Bas et al. 1986)"),col="green",bg="transparent",fg="black",xlab=annotate("SiO2"),ylab=annotate("Na2O+K2O")),template=temp))
}
