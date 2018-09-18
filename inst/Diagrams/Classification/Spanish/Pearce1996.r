#####################################################################
#                           Nb/Y vs Zr/Ti diagram                   #
#                             Pearce (1996)                         #
#####################################################################

Pearce1996<-function(){
x.data<<-WR[,"Nb"]/WR[,"Y"]
y.data<<-WR[,"Zr"]/WR[,"Ti"]
temp<-list(
        clssf=list("NULL",use=2:11,rcname=c("Basalto","Basaltoo alcalíno","feldespatoide","Andesita, Andesita Basáltica","trachyAndesita","tefrigonolita","riolita, dacita","traquita","fonolita","riolita alcalína")),
        lines1=list("NULL",x=log10(c(0.01,0.67,0.67,0.01,0.01)),y=log10(c(0.001,0.001,0.024,0.0076,0.001)),col="red", lwd=3), 
        lines2=list("NULL",x=log10(c(0.67,2.81,2.81,0.67,0.67)),y=log10(c(0.001,0.001,0.0355,0.024,0.001)),col="red", lwd=3),
        lines3=list("NULL",x=log10(c(2.81,50,50,2.81,2.81)),y=log10(c(0.001,0.001,0.0781,0.0355,0.001)),col="red", lwd=3),
        lines4=list("NULL",x=log10(c(0.01,0.67,0.67,0.01,0.01)),y=log10(c(0.0076,0.024,0.082,0.026,0.0076)),col="red", lwd=3),
        lines5=list("NULL",x=log10(c(0.67,2.81,2.81,0.67,0.67)),y=log10(c(0.024,0.0355,0.12,0.082,0.024)),col="red", lwd=3),
        lines6=list("NULL",x=log10(c(2.81,50,50,2.81,2.81)),y=log10(c(0.0355,0.0781,0.2663,0.12,0.0355)),col="red", lwd=3),
        lines7=list("NULL",x=log10(c(0.01,0.67,0.67,0.065,0.01,0.01)),y=log10(c(0.026,0.082,0.2,2,2,0.026)),col="red", lwd=3),
        lines8=list("NULL",x=log10(c(0.67,2.81,2.81,0.67,0.67)),y=log10(c(0.082,0.12,0.99,.2,0.082)),col="red", lwd=3),
        lines9=list("NULL",x=log10(c(2.81,50,50,5.52,2.81,2.81)),y=log10(c(0.12,0.2663,2,2,0.99,0.12)),col="red", lwd=3),
        lines10=list("NULL",x=log10(c(0.065,0.67,2.81,5.52,0.065)),y=log10(c(2,0.2,0.99,2.,2)),col="red", lwd=3),
        lines11=list("lines",x=c(0.01,0.67,2.81,50),y=c(0.0076,0.024,0.0355,0.0781),col=plt.col[2]),
        lines12=list("lines",x=c(0.01,0.67,2.81,50),y=c(0.026,0.082,0.12,0.2663),col=plt.col[2]),
        lines13=list("lines",x=c(0.67,0.67),y=c(0.001,0.20),col=plt.col[2]),
        lines14=list("lines",x=c(2.81,2.81),y=c(0.001,0.99),col=plt.col[2]),
        lines15=list("lines",x=c(0.065,0.67,5.52),y=c(2,0.2,2),col=plt.col[2]),
        GCDkit=list("NULL",plot.type="binary",plot.position=27,plot.name="Diagrama Nb/Y-Zr/Ti (modificado de Pearce 1996)")
)

temp1<-list(
        text1=list("text",x=0.08,y=0.003,text="Basalto",col=plt.col[2],adj=0.5),
        text2=list("text",x=1.5,y=0.01,text="Basaltoo\nalcalíno",col=plt.col[2],adj=0.5),
        text3=list("text",x=8,y=0.01,text="feldespatoide",col=plt.col[2],adj=c(0,0.5)),
        text4=list("text",x=0.1,y=0.03,text="Andesita\nAndesita Basáltica",col=plt.col[2],adj=0.5,srt=17),
        text5=list("text",x=1.5,y=0.06,text="trachy-\nAndesita",col=plt.col[2],adj=0.5,srt=17),
        text6=list("text",x=10,y=0.095,text="tefrigonolita",col=plt.col[2],adj=0.5,srt=17),
        text7=list("text",x=0.1,y=0.2,text="riolita\ndacita",col=plt.col[2],adj=0.5),
        text8=list("text",x=1.5,y=0.2,text="traquita",col=plt.col[2],adj=0.5),
        text9=list("text",x=10,y=0.4,text="fonolita",col=plt.col[2],adj=0.5),
        text10=list("text",x=0.8,y=0.6,text="riolita\nalcalína",col=plt.col[2],adj=0.5)    
    )

if(getOption("gcd.plot.text"))temp<-c(temp,temp1)


sheet<<-list(demo=list(fun="plot",call=list(xlim=c(0.01,100),ylim=c(0.001,2),main=annotate("Diagrama Nb/Y-Zr/Ti (modificado de Pearce 1996)"),col="green",bg="transparent",fg="black",xlab=annotate("Nb/Y"),ylab=annotate("Zr/Ti"),log="xy"),template=temp))
}
