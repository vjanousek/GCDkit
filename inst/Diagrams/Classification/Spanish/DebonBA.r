#####################################################################
#                 B-A plot of Debon and Le Fort (1988)              #
#                                                                   #
#####################################################################

DebonBA<-function(){

temp1<-list(
        #classification
        clssf=list("NULL",use=2:7,rcname=c("I","II","III","IV","V","VI")),
        lines1=list("NULL",x=c(0,435,0,0),y=c(0,500,500,0)),
        lines2=list("NULL",x=c(0,435,500,600,0),y=c(0,500,500,250,0)),
        lines3=list("NULL",x=c(0,600,1500,0),y=c(0,250,0,0)),
        lines4=list("NULL",x=c(0,1500,500,0),y=c(0,0,-239.36,0)),
        lines5=list("NULL",x=c(0,500,500,420,0),y=c(0,-239.36,-885,-885,0)),
        lines6=list("NULL",x=c(0,420,0,0),y=c(0,-885,-885,0)),
        #plot
        lines7=list("lines",x=c(0,420),y=c(0,-885),col=plt.col[2],lty="dashed"),
        lines8=list("lines",x=c(0,470),y=c(0,-225),col=plt.col[2],lty="dashed"),
        lines9=list("lines",x=c(0,1500),y=c(0,0),col=plt.col[2],lty="dashed"),
        lines10=list("lines",x=c(0,600),y=c(0,250),col=plt.col[2],lty="dashed"),
        lines11=list("lines",x=c(0,435),y=c(0,500),col=plt.col[2],lty="dashed"),
        lines12=list("lines",x=c(38.8,38.8),y=c(-500,500),col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.position=35,plot.name="B-A (Debon + Le Fort 1983)")
        )
temp2<-list(
        text1=list("text",x=70,y=-400,text="VI",col=plt.col[2]),
        text2=list("text",x=400,y=-400,text="V",col=plt.col[2]),
        text3=list("text",x=450,y=-100,text="IV",col=plt.col[2]),
        text4=list("text",x=450,y=50,text="III",col=plt.col[2]),
        text5=list("text",x=400,y=250,text="II",col=plt.col[2]),
        text6=list("text",x=100,y=300,text="I",col=plt.col[2])
        )

if(getOption("gcd.plot.text")){
    temp<-c(temp1,temp2)
}else{
    temp<-temp1
}
debon<-DebonCalc(milli)
x.data<<-debon[,"B"]
y.data<<-debon[,"A"]
sheet<<-list(demo=list(fun="plot", call=list(xlim=c(0,500),ylim=c(-500,500),col="green",bg="transparent",fg="black",main=annotate("Diagrama B-A (Debon y Le Fort)"),xlab="B = Fe + Mg +Ti",ylab="A = Al - (K + Na + 2Ca)"),template=temp))
ee<-DebonCalc(milli)
return(ee)
}
