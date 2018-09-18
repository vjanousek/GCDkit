
#####################################################################
#             Ternary plot Na2O - Al2O3 - K2O (mol. %)              #
#                                                                   #
#####################################################################


NaAlK<-function(){
    aa<-WR[,"Na2O"]/MW["Na2O"]
    bb<-WR[,"Al2O3"]/MW["Al2O3"]
    cc<-WR[,"K2O"]/MW["K2O"]
    results<-cbind(aa,bb,cc,(aa+cc)/bb,cc/bb,cc/aa)
    colnames(results)<-c("Na2O","Al2O3","K2O","(Na2O+K2O)/Al2O3","K2O/Al2O3","K2O/Na2O")
    
    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma
    x.data<<-cc+bb/2
    y.data<<-sqrt(3)*bb/2

    temp1<-list(
        clssf=list("NULL",use=2:7,rcname=c("metaluminous/peraluminous","sodic","peralkaline","potassic","perpotassic","peralkaline","ultrapotassic")),
        lines1=list("NULL",x=c(0.25,0.5,0.5,0.25),y=c(sqrt(3)/4,sqrt(3)/4,sqrt(3)/2,sqrt(3)/4),col="red"),
        lines2=list("NULL",x=c(0,0.5,0.5,0.25,0),y=c(0,0,sqrt(3)/4,sqrt(3)/4,0),col="blue"),
        lines3=list("NULL",x=c(0.5,0.75-1/8,0.5,0.5),y=c(sqrt(3)/4,sqrt(3)/4,sqrt(3)/2,sqrt(3)/4),col="orange"),
        lines4=list("NULL",x=c(0.5,0.75,0.75-1/8,0.5,0.5),y=c(0,0,sqrt(3)/4,sqrt(3)/4,0),col="green"),
        lines5=list("NULL",x=c(0.75-1/8,0.75,0.5,0.75-1/8),y=c(sqrt(3)/4,sqrt(3)/4,sqrt(3)/2,sqrt(3)/4),col="blue"),
        lines6=list("NULL",x=c(0.75,1,0.75,0.75-1/8,0.75),y=c(0,0,sqrt(3)/4,sqrt(3)/4,0),col="black"),
        lines21=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        lines22=list("lines",x=c(0.5,0.5),y=c(0,sqrt(3)/2),col=plt.col[2],lty="dashed"),
        lines23=list("lines",x=c(0,0.75),y=c(sqrt(3)/4,sqrt(3)/4),col=plt.col[2],lty="dashed"),
        lines24=list("lines",x=c(0.75,0.5),y=c(0,sqrt(3)/2),col=plt.col[2],lty="dashed"),
        lines25=list("lines",x=c(0.5,0.75),y=c(0,sqrt(3)/4),col=plt.col[2],lty="dashed"),
        A=list("text",x=0,y=-0.03,text=annotate("Na2O"),adj=0.5),
        C=list("text",x=1,y=-0.03,text=annotate("K2O"),adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text=annotate("Al2O3"),adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.position=13.5,plot.name="Molar Na2O-Al2O3-K2O plot")
        )

    temp2<-list(
        text1=list("text",x=0.45,y=0.05,text="Sodic",col=plt.col[2]), 
        text2=list("text",x=0.58,y=0.05,text="Potassic",col=plt.col[2]),
        text3=list("text",x=0.85,y=0.05,text="Ultrapotassic",col=plt.col[2]),
        text4=list("text",x=0.0,y=0.5,text="Metaluminous\nPeraluminous",col=plt.col[2],adj=0),
        text5=list("text",x=0.0,y=0.4,text="Peralkaline",col=plt.col[2],adj=0), 
        text6=list("text",x=0.7,y=0.3,text="Perpotassic",col=plt.col[2],srt=60)
    )

   if(getOption("gcd.plot.text")){
        temp<-c(temp1,temp2)
    }else{
        temp<-temp1
    }
sheet<<-list(demo=list(fun="plot",call=list(xlim=c(-.03,1.03),ylim=c(-0.08,1.03),main=annotate("Molar Na2O-Al2O3-K2O plot"),bg="transparent",fg="black",asp=1,axes=FALSE,xlab="",ylab=""),template=temp))
assign("results",results,.GlobalEnv)
}
