#####################################################################
#                                                                   #
#                                                                   #
#####################################################################

OConnorVolc<-function(){
if (!just.sheets){

    x<-CIPW(WR)
    aaa<-"Ab"
    bbb<-"An"
    ccc<-"Or"
    aa<-x[,aaa]
    bb<-x[,bbb]
    cc<-x[,ccc]

suma<-(aa+bb+cc)/100
aa<-aa/suma
bb<-bb/suma
cc<-cc/suma
x.data<<-cc+bb/2
y.data<<-sqrt(3)*bb/2
results<<-cbind(aa,bb,cc)
colnames(results)<<-c(aaa,bbb,ccc)
rownames(results)<<-rownames(x)
}


template<-list(
        clssf=list("NULL",use=2:6,rcname=c("cuarzo keratópiro","riolita","latita cuarcífera","riodacita","dacita")),
        polygon1=list("NULL",x=c(0/2+0,0/2+30,17.5/2+30,25/2+0),y=sqrt(3)*c(0/2,0/2,17.5/2,25/2),lwd=3,col=plt.col[2]),
        polygon2=list("NULL",x=c(0/2+30,0/2+100,17.5/2+30),y=sqrt(3)/2*c(0,0,17.5),lwd=3,col=plt.col[2]),
        polygon3=list("NULL",x=c(16.25/2+35,12.5/2+50,27.5/2+50,35.75/2+35),y=sqrt(3)/2*c(16.25,12.5,27.5,35.75),lwd=3,col=plt.col[2]),
        polygon4=list("NULL",x=c(20/2+20,16.25/2+35,35.75/2+35,44/2+20),y=sqrt(3)/2*c(20,16.25,35.75,44),lwd=3,col=plt.col[2]),
        polygon5=list("NULL",x=c(25/2+0,20/2+20,44/2+20,55/2+0),y=sqrt(3)/2*c(25,20,44,55),lwd=3,col=plt.col[2]),
        
        lines1=list("lines",x=c(0/2+30,17.5/2+30),y=sqrt(3)/2*c(0,17.5),col=plt.col[2]),
        lines1=list("lines",x=c(20/2+20,44/2+20),y=sqrt(3)/2*c(20,44),col=plt.col[2]),
        lines1=list("lines",x=c(16.25/2+35,35.75/2+35),y=sqrt(3)/2*c(16.25,35.75),col=plt.col[2],lty="dashed"),
        lines1=list("lines",x=c(12.5/2+50,27.5/2+50),y=sqrt(3)/2*c(12.5,27.5),col=plt.col[2],lty="dashed"),
        lines1=list("lines",x=c(25/2+0,12.5/2+50),y=sqrt(3)/2*c(25,12.5),col=plt.col[2]),
        lines1=list("lines",x=c(12.5/2+50,2.5/2+90),y=sqrt(3)/2*c(12.5,2.5),col=plt.col[2],lty="dashed"),
        lines1=list("lines",x=c(2.5/2+90,5.5/2+90),y=sqrt(3)/2*c(2.5,5.5),col=plt.col[2],lty="dashed"),
                                        
        lines2=list("lines",x=c(0,100,50,0),y=c(0,0,sqrt(3)*100/2,0),col="black"),
        A=list("text",x=0,y=-5,text="Ab",adj=0.5),
        C=list("text",x=100,y=-5,text="Or",adj=0.5),
        B=list("text",x=50,y=sqrt(3)*100/2+3,text="An",adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.position=29.5,plot.name="CIPW-normative feldspar triangle (O'Connor 1965)")
        )
temp<-list(        
        text1=list("text",x=20,y=sqrt(3)*10/2,text="Cuarzo\nkeratópiro",col=plt.col[2]),
        text2=list("text",x=40+7/2,y=sqrt(3)*7/2,text="Riolita",col=plt.col[2]),
        text3=list("text",x=42+25/2,y=sqrt(3)*25/2,text="Latita\ncuarcífera",col=plt.col[2],srt=60),
        text4=list("text",x=27+30/2,y=sqrt(3)*30/2,text="Riodacita",col=plt.col[2],srt=60),
        text5=list("text",x=10+40/2,y=sqrt(3)*40/2,text="Dacita",col=plt.col[2])
        )

if(getOption("gcd.plot.text"))template<-c(template,temp)

sheet<<-list(demo=list(fun="plot",call=list(xlim=c(-3,103),main=annotate(template$GCDkit$plot.name),ylim=c(-10,103),xlab="",ylab="",bg="transparent",fg="black",asp=1,axes=FALSE),template=template))

}
