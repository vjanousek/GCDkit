#####################################################################
#      SiO2 vs FeOt/MgO, Tholeiitic vs. Calc-alkaline series        #
#                      Miyashiro, 1974                              #
#####################################################################

Miyashiro<-function(){


x.data<<-WR[,"SiO2"]
y.data<<-WR[,"FeOt"]/WR[,"MgO"]

temp<-list(
        clssf=list("NULL",use=2:3,rcname=c("calc-alkaline series","tholeiite series")),
        lines11=list("NULL",x=c(42.8,100.4,100.4),y=c(0,9,0),col="red", lwd=3),
        lines12=list("NULL",x=c(42.8,100.4,100.4,42.8),y=c(0,9,20,20),col="red", lwd=3),
        lines13=list("lines",x=c(42.8,100.4),y=c(0,9),col="black"),
        GCDkit=list("NULL",plot.type="binary",plot.position=11.5,plot.name="SiO2-FeOt/MgO (Miyashiro 1974)")
        )

temp2<-list(
        text1=list("text",x=77,y=0.7,text="Calc-alkaline\nSeries",col=plt.col[2],adj=c(1,0.5)),
        text2=list("text",x=47,y=4,text="Tholeiite Series",col=plt.col[2],adj=c(0,0.5))
        )


if(getOption("gcd.plot.text"))
    temp<-c(temp,temp2)


sheet<<-list(demo=list(fun="plot",call=list(xlim=c(45,78),ylim=c(0,6),main=annotate("SiO2-FeOt/MgO plot (Miyashiro 1974)"),col="green",bg="transparent",fg="black",xlab=annotate("SiO2"),ylab=annotate("FeOt/MgO")),template=temp))
}
