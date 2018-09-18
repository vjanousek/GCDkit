Shand<-function(){
x.data<<-WR[,"A/CNK"]
y.data<<-WR[,"A/NK"]

temp1<-list(
    clssf=list("NULL",use=2:5,rcname=c("métalumineux","péralumineux","peralcalin","indéfini")),
    lines1=list("lines",x=c(1,1,0.5,0.5,1),y=c(1,7,7,1,1)),
    lines2=list("lines",x=c(1,2,2,1,1),y=c(1,1,7,7,1)),
    lines3=list("lines",x=c(1,1,0.5,0.5,1),y=c(1,0,0,1,1)),
    lines4=list("lines",x=c(1,2,2,1,1),y=c(1,1,0,0,1)),
    lines11=list("abline",h=1,col=plt.col[2]),
    lines12=list("abline",v=1,col=plt.col[2]),
    lines13=list("lines",x=c(0,7),y=c(0,7),col=plt.col[2],lty="dashed"),
    GCDkit=list("NULL",plot.type="binary",plot.position=14,plot.name="A/CNK - A/NK (Shand 1943)")
    )
temp2<-list(
    text1=list("text",x=0.55,y=6,text="Métalumineux",col=plt.col[2],adj=0),
    text2=list("text",x=1.05,y=6,text="Péralumineux",col=plt.col[2],adj=0),
    text3=list("text",x=0.55,y=0.5,text="Peralcalin",col=plt.col[2],adj=0)
    )
if(getOption("gcd.plot.text")){
    temp<-c(temp1,temp2)}
    else{
    temp<-temp1
}

sheet<<-list(demo=list(fun="plot", call=list(xlim=c(0.5,1.5),ylim=c(0,7),main=annotate("Diagramme A/CNK-A/NK (Shand 1943)"),col="green",bg="transparent",fg="black",xlab="A/CNK",ylab="A/NK",main=""),template=temp))
}
