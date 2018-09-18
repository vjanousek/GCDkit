#####################################################################
#                Th-Co diagram, IAT, CA, HKCA, SHO series           #
#                             Hastie et al 2007                     #
#####################################################################

Hastie<-function(){

x.data<<-WR[,"Co"]
y.data<<-WR[,"Th"]

temp<-list(
        #classification
        clssf=list("NULL",use=2:4,rcname=c("série tholéitique","série alcaline","high-K calc-alkaline and shoshonite series")),
        lines1=list("NULL",x=c(70,0,0,70,70),y=log10(c(0.01,0.01,1.35,0.245,0.01)),col="red", lwd=3),
        lines2=list("NULL",x=c(70,0,0,70,70),y=log10(c(0.245,1.35,9,2.2,0.245)),col="red", lwd=3),
        lines3=list("NULL",x=c(70,0,0,70,70),y=log10(c(2.2,9,100,100,2.2)),col="red", lwd=3),
        #plot
        lines4=list("lines",x=c(70,0),y=c(0.245,1.35),col=plt.col[2]),
        lines5=list("lines",x=c(70,0),y=c(2.2,9),col=plt.col[2]),
        lines6=list("lines",x=c(38.4,24),y=c(0.01,100),col=plt.col[2]),
        lines7=list("lines",x=c(23,7),y=c(0.01,100),col=plt.col[2]),
        GCDkit=list("NULL",plot.type="binary",plot.position=13,plot.name="Co - Th (Hastie et al. 2007)")
)

temp1<-list(
        text1=list("text",x=68,y=0.1,text="Série\ntholéitique",col=plt.col[2],adj=c(0,0.5)),
        text2=list("text",x=68,y=1,text="Série\nalcaline",col=plt.col[2],adj=c(0,0.5)),
        text3=list("text",x=68,y=60,text="Séries calco-alcalines\net shoshonitiques",col=plt.col[2],adj=c(0,0.5)),
        text4=list("text",x=50,y=0.02,text="B",col=plt.col[2],adj=0.5),
        text5=list("text",x=30,y=0.02,text="BA/A",col=plt.col[2],adj=0.5),
        text6=list("text",x=10,y=0.02,text="D/R*",col=plt.col[2],adj=0.5)
)

if(getOption("gcd.plot.text"))temp<-c(temp,temp1)
sheet<<-list(demo=list(fun="plot",call=list(xlim=c(70,0),ylim=c(0.01,100),main=annotate("Diagramme Co-Th (Hastie et al. 2007)"),col="green",bg="transparent",fg="black",xlab="Co",ylab="Th", log="y"),template=temp))
}
