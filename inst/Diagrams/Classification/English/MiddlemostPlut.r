#####################################################################
#                  Classification of magmatic rocks                 #
#  Middlemost, 1985; "Magmas and Magmatic Rocks," Longman, Essex.   #
#####################################################################
MiddlemostPlut<-function(){

temp1<-list(
        clssf=list("NULL",use=2:15,rcname=c("D, Gb","monzodiorite","monzonite","syenite","af syenite","diorite","qtz monzodiorite","qtz monzonite","qtz syenite","af qtz syenite","tonalite","granodirite","granite","af granite")),
        lines1=list("lines",x=c(44.5,50.5,45.5,44.5),y=c(1.75,4.9,4,1.75),col=plt.col[2]),
        lines2=list("lines",x=c(45.5,50.5,56,52,45.5),y=c(4,4.9,7,6.5,4),col=plt.col[2]),
        lines3=list("lines",x=c(52,56,59.5,56,52),y=c(6.5,7,8.5,8,6.5),col=plt.col[2]),
        lines4=list("lines",x=c(56,59.5,62.1,59.5,56),y=c(8,8.5,11.1,11.2,8),col=plt.col[2]),
        lines5=list("lines",x=c(59.5,62.1,63,62,59.5),y=c(11.2,11.1,12.75,13.5,11.2),col=plt.col[2]),
        lines6=list("lines",x=c(44.5,57.4,60.25,50.5,44.5),y=c(1.75,4.75,5.5,4.9,1.75),col=plt.col[2]),
        lines7=list("lines",x=c(50.5,60.25,64,56,50.5),y=c(4.9,5.5,7.5,7,4.9),col=plt.col[2]),
        lines8=list("lines",x=c(56,64,67,59.5,56),y=c(7,7.5,9.5,8.5,7),col=plt.col[2]),
        lines9=list("lines",x=c(59.5,67,68,62.1,59.5),y=c(8.5,9.5,10.4,11.1,8.5),col=plt.col[2]),
        lines10=list("lines",x=c(68,69,63,62.1,68),y=c(10.4,11.1,12.75,11.1,10.4),col=plt.col[2]),
        lines11=list("lines",x=c(80,80,64,60.25,57.4,80),y=c(2.67,3.7,5.4,5.5,4.75,2.67),col=plt.col[2]),
        lines12=list("lines",x=c(80,80,74,64,60.25,64,80),y=c(3.7,5.1,6.5,7.5,5.5,5.4,3.7),col=plt.col[2]),
        lines13=list("lines",x=c(80,80,68,67,64,74,80),y=c(5.1,7.3,10.4,9.5,7.5,6.5,5.1),col=plt.col[2]),
        lines14=list("lines",x=c(80,80,69,68,80),y=c(7.3,7.9,11.1,10.4,7.3),col=plt.col[2]),
        GCDkit=list("NULL",plot.type="binary",plot.position=37,plot.name="Middlemost (1985) ")
    )

temp2<-list(
        text1=list("text",x=61,y=12,text="Af\nSyenite",col=plt.col[2],cex=0.75),
        text2=list("text",x=65,y=11.5,text="Af\nQtz\nSyenite",col=plt.col[2],cex=0.75),
        text3=list("text",x=70,y=10.5,text="Af\nGranite",col=plt.col[2],cex=0.75),
        text4=list("text",x=59.5,y=10,text="Syenite",col=plt.col[2],cex=0.75,srt=45),
        text5=list("text",x=64,y=10,text="Qtz\nSyenite",col=plt.col[2],cex=0.75),
        text6=list("text",x=70,y=8.5,text="Granite",col=plt.col[2],cex=0.75),
        text7=list("text",x=56,y=7.5,text="Mz",col=plt.col[2],cex=0.75),
        text8=list("text",x=61.5,y=8,text="Qtz\nMonzonite",col=plt.col[2],cex=0.75),
        text9=list("text",x=50.5,y=5.5,text="Monzo\nd",col=plt.col[2],cex=0.75,srt=45),
        text10=list("text",x=58,y=6.5,text="Qtz\nMonzodiorite",col=plt.col[2],cex=0.75),
        text11=list("text",x=68,y=6.4,text="Granodiorite",col=plt.col[2],cex=0.75),
        text12=list("text",x=46.5,y=3.5,text="D,\nGb",col=plt.col[2],cex=0.75),
        text13=list("text",x=52,y=4.3,text="Qtz\nDiorite",col=plt.col[2],cex=0.75),
        text14=list("text",x=66,y=4.75,text="Tonalite",col=plt.col[2],cex=0.75)
    )

if(getOption("gcd.plot.text")){
    temp<-c(temp1,temp2)}
    else{
    temp<-temp1
}
x.data<<-WR[,"SiO2"]
y.data<<-WR[,"Na2O"]+WR[,"K2O"]
sheet<<-list(demo=list(fun="plot",call=list(xlim=c(42,80),ylim=c(0,15),col="green",bg="transparent",fg="black",main=annotate("Middlemost (1985)"),xlab=annotate("SiO2"),ylab=annotate("Na2O+K2O")),template=temp))

}
