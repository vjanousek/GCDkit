#####################################################################
#              TAS diagram - LeBas and Streckeisen (1991)           #
#                    adapted by Middlemost (1994)                   #
#                                                                   #
#####################################################################

TASMiddlemostPlut<-function(){

temp1<-list(
        clssf=list("NULL",use=2:19,rcname=c("feldspathoidite","peridotgabbro","gabbro","diorite gabbroique","diorite","granodiorite","granite","monzogabbro","monzodiorite","monzonite","quartzmonzonite","syénite","gabbro a f.oid.","foid monzodiorite","monzo syénite a f.oid.","syénite a f.oid.","tawite/urtite/italite","quartzolite")),
        lines1=list("lines",x=c(37,41,41,45,48.4,52.5,52.5,37,35,37),y=c(3,3,7,9.4,11.5,14,18,14,9,3),col=plt.col[2]),
        lines2=list("lines",x=c(45,45,41,41),y=c(0,3,3,0),col=plt.col[2]),
        lines3=list("lines",x=c(45,52,52,45),y=c(0,0,5,5),col=plt.col[2]),
        lines4=list("lines",x=c(52,57,57,52),y=c(0,0,5.9,5),col=plt.col[2]),
        lines5=list("lines",x=c(57,63,63,57),y=c(0,0,7,5.9),col=plt.col[2]),
        lines6=list("lines",x=c(63,77.3,69,63),y=c(0,0,8,7),col=plt.col[2]),
        lines7=list("lines",x=c(77.3,87.5,85.9,71.8,69),y=c(0,4.7,6.8,13.5,8),col=plt.col[2]),
        lines8=list("lines",x=c(45,52,49.4),y=c(5,5,7.3),col=plt.col[2]),
        lines9=list("lines",x=c(52,57,53,49.4),y=c(5,5.9,9.3,7.3),col=plt.col[2]),
        lines10=list("lines",x=c(57,63,57.6,53),y=c(5.9,7,11.7,9.3),col=plt.col[2]),
        lines12=list("lines",x=c(63,69,71.8,61),y=c(7,8,13.5,8.6),col=plt.col[2]),
        lines13=list("lines",x=c(61,71.8,63,61,57.6),y=c(8.6,13.5,16.2,13.5,11.7),col=plt.col[2]),
        lines14=list("lines",x=c(41,45,45,49.4,45,41),y=c(3,3,5,7.3,9.4,7),col=plt.col[2]),
        lines15=list("lines",x=c(49.4,53,48.4,45),y=c(7.3,9.3,11.5,9.4),col=plt.col[2]),
        lines16=list("lines",x=c(53,57.6,52.5,48.4),y=c(9.3,11.7,14,11.5),col=plt.col[2]),
        lines17=list("lines",x=c(57.6,61,63,57,52.5,52.5),y=c(11.7,13.5,16.2,18,18,14),col=plt.col[2]),
        lines15=list("NULL",x=c(37,52.5,57,71.8,37.99),y=c(14,18,18,13.5,29.78),col=plt.col[2]),
        lines15=list("NULL",x=c(77.3,87.5,85.9,100),y=c(0,4.7,6.8,0),col=plt.col[2]),
        GCDkit=list("NULL",plot.type="binary",plot.position=32,plot.name="TAS Middlemost (1994)")
        )

temp2<-list(
        text1=list("text",x=43,y=1.4,text="Gabbro\na olivine",col=plt.col[2],srt=90),
        text2=list("text",x=48.5,y=2.8,text="Gabbro",col=plt.col[2],srt=90),
        text3=list("text",x=54.8,y=3,text="Diorite\ngabbroique",col=plt.col[2],srt=90),
        text4=list("text",x=59.9,y=3,text="Diorite",col=plt.col[2],srt=90),
        text5=list("text",x=67,y=3,text="Granodiorite",col=plt.col[2],srt=90),
        text6=list("text",x=79,y=6,text="Granite",col=plt.col[2]),
        text7=list("text",x=63.5,y=11.5,text="Syénite",col=plt.col[2],srt=45),
        text22=list("text",x=66,y=9,text="Quartz-\nmonzonite",col=plt.col[2],srt=45),
        text8=list("text",x=57.8,y=8.5,text="Monzonite",col=plt.col[2],cex=0.8),
        text9=list("text",x=52.95,y=7,text="Monzo-\ndiorite",col=plt.col[2],cex=0.8),
        text10=list("text",x=49.2,y=5.65,text="Monzo-\ngabbro",col=plt.col[2],cex=0.8),
        text11=list("text",x=45,y=7,text="Gabbro\na f.oid.",col=plt.col[2],srt=50),
        text12=list("text",x=49.2,y=9.3,text="Monzo\ngabbro\na f.oid.",col=plt.col[2],srt=50,cex=0.8),
        text13=list("text",x=53,y=11.5,text="Monzo\nsyénite\na f.oid.",col=plt.col[2],srt=50,cex=0.8),
        text14=list("text",x=57,y=14,text="Syénite\na f.oid.",col=plt.col[2],srt=50,cex=0.8),
        text15=list("text",x=44,y=12,text="Feldspathoidite",col=plt.col[2]),

        text22=list("text",x=55,y=18.5,text="Tawite/Urtite/Italite",col=plt.col[2]),
        text23=list("text",x=85,y=1,text="Quartzolite",col=plt.col[2])#,
        #text16=list("text",x=40,y=14,text="Alcalin",col=plt.col[2]),
        #text17=list("text",x=60,y=1,text="Subalcalin/Tholéitique",col=plt.col[2]),
        #text18=list("text",x=40,y=15.5,text="Ultrabasique",col=plt.col[3]),
        #text19=list("text",x=49,y=15.5,text="Basique",col=plt.col[3]),
        #text20=list("text",x=58,y=15.5,text="Intermédiaire",col=plt.col[3]),
        #text21=list("text",x=68,y=15.5,text="Acide",col=plt.col[3])
        )

if(getOption("gcd.plot.text")){
    temp<-c(temp1,temp2)}
    else{
    temp<-temp1
}
x.data<<-WRanh[,"SiO2"]
y.data<<-WRanh[,"Na2O"]+WRanh[,"K2O"]
sheet<<-list(demo=list(fun="plot",call=list(xlim=c(34,90),ylim=c(0,19),main=annotate("Middlemost (1994)"),col="green",bg="transparent",fg="black",xlab=annotate("SiO2"),ylab=annotate("Na2O+K2O")),main="Diagramme TAS pour roches plutoniques (Middlemost 1994)",template=temp))
}
