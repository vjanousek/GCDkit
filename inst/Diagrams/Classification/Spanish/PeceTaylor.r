#####################################################################
#                SiO2-K2O diagram Th, CA, KHCA, SH series           #
#      Peccerillo and Taylor, 1976, Fig.2 - see Rickwood 1989       #
#####################################################################

PeceTaylor<-function(){

extrapolated<-TRUE #change to 'extrapolated<-FALSE' if you want to use the original, non-extrapolated boundaries of P&T

x.data<<-WR[,"SiO2"]
y.data<<-WR[,"K2O"]

#extrapolated boundaries in the diagram:
temp1<-list(
        clssf=list("NULL",use=2:5,rcname=c("serie toleítica","serie calcoalcalina","high-K serie calcoalcalina","serie shoshonítica")),
        lines11=list("NULL",x=c(78,78,70,63,56,52,48,48,78),y=c(0,1.6,1.3,1,0.7,0.5,0.3,0,0),col="red", lwd=3),
        lines12=list("NULL",x=c(78,75,70,63,56,52,48,48,52,56,63,70,78),y=c(1.6,3.43,3,2.4,1.8,1.5,1.2,0.3,0.5,0.7,1,1.3,1.6),col="red", lwd=3),
        lines13=list("NULL",x=c(75,70,63,56,52,48,48,52,56,63,70,75),y=c(3.43,4.8,4,3.2,2.4,1.6,1.2,1.5,1.8,2.4,3,3.43),col="red", lwd=3),
        lines14=list("NULL",x=c(70,70,48,48,52,56,63,70),y=c(4.8,7,7,1.6,2.4,3.2,4,4.8),col="red", lwd=3),
        lines15=list("lines",x=c(48,52,56,63,70,78),y=c(0.3,0.5,0.7,1,1.3,1.6),col=plt.col[2]),
        lines16=list("lines",x=c(48,52,56,63,70,75),y=c(1.2,1.5,1.8,2.4,3,3.43),col=plt.col[2]),
        lines17=list("lines",x=c(48,52,56,63,70),y=c(1.6,2.4,3.2,4,4.8),col=plt.col[2]),
        GCDkit=list("NULL",plot.type="binary",plot.position=12,plot.name="SiO2 - K2O (Peccerillo + Taylor 1976)")

        )
#original boundaries in the diagram:
temp2<-list(
        clssf=list("NULL",use=2:5,rcname=c("serie toleítica","serie calcoalcalina","high-K serie calcoalcalina","serie shoshonítica")),
        lines1=list("NULL",x=c(48,78,78,70,63,56,52,48),y=c(0,0,1.6,1.3,1,0.7,0.5,0.3),col="blue", lwd=3),
        lines2=list("NULL",x=c(48,52,56,63,70,78,70,63,56,52,48),y=c(0.3,0.5,0.7,1,1.3,1.6,3,2.4,1.8,1.5,1.2),col="blue", lwd=3),
        lines3=list("NULL",x=c(48,52,56,63,70,63,56,52,48),y=c(1.2,1.5,1.8,2.4,3,4,3.2,2.4,1.6),col="blue", lwd=3),
        lines4=list("NULL",x=c(48,52,56,63,48),y=c(1.6,2.4,3.2,4,4),col="blue", lwd=3),
        lines5=list("lines",x=c(48,52,56,63,70,78),y=c(0.3,0.5,0.7,1,1.3,1.6),col=plt.col[2]),
        lines6=list("lines",x=c(48,52,56,63,70),y=c(1.2,1.5,1.8,2.4,3),col=plt.col[2]), #second value for y (1.5) missprinted in original as 1.3
        lines7=list("lines",x=c(48,52,56,63),y=c(1.6,2.4,3.2,4),col=plt.col[2]),
        GCDkit=list("NULL",plot.type="binary",plot.position=12,plot.name="SiO2 - K2O (Peccerillo + Taylor 1976)")
    )

temp3<-list(
        text1=list("text",x=77,y=0.7,text="Serie Toleítica",col=plt.col[2],adj=c(1,0.5)),
        text2=list("text",x=77,y=2.4,text="serie calco-\nalcalina",col=plt.col[2],adj=c(1,0.5)),
        text3=list("text",x=77,y=4,text="Serie calcoalcalina\npotásica ",col=plt.col[2],adj=c(1,0.5)),
        text4=list("text",x=47,y=4.5,text="Serie shoshonítica",col=plt.col[2],adj=0)
    )

if(extrapolated)
    temp<-temp1
else
    temp<-temp2

if(getOption("gcd.plot.text"))
    temp<-c(temp,temp3)


sheet<<-list(demo=list(fun="plot",call=list(xlim=c(45,78),ylim=c(0,7),main=annotate("Diagrama SiO2-K2O (Pecerillo y Taylor 1976)"),col="green",bg="transparent",fg="black",xlab=annotate("SiO2"),ylab=annotate("K2O")),template=temp))
}
