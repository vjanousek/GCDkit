#####################################################################
#                      Classification of basalts                    #
#                            Shervais 1982                          #
#####################################################################

Shervais<-function(){ 
 
if(!getOption("gcd.plot.bw")){
    col1<-plt.col[1]
    col2<-plt.col[2]
}else{
    col1<-"black"
    col2<-"black"
}
    i<-which(WR[,"Ti"]>0 & WR[,"V"]>0)
    #i<-WR[,"Ti"]>0 & WR[,"V"]>0
    x.data<<-WR[i,"Ti"]/1000
    y.data<<-WR[i,"V"]


temp<-list(
        lines1=list("abline",a=0,b=100,lty="dashed",col=col2),
        lines2=list("abline",a=0,b=20,lty="dashed",col=col2),
        lines3=list("abline",a=0,b=50,col=col2),
        lines4=list("abline",a=0,b=10,lty="dashed",col=col2),     
        GCDkit=list("NULL",plot.type="binary",plot.position=209,plot.name="Shervais (1982)")
)

if(getOption("gcd.plot.text")){    
    temp<-c(temp,list(      
        text1=list("text",x=10,y=570,text="ARC",cex=1,col=col2),
        text2=list("text",x=15,y=570,text="OFB",cex=1,col=col2),
        text3=list("text",x=3,y=540,text="Ti/V=10",cex=0.7,col=col2),
        text4=list("text",x=13.2,y=540,text="Ti/V=20",cex=0.7,col=col2),
        text5=list("text",x=23.5,y=290,text="Ti/V=100",cex=0.7,col=col2),
        text6=list("text",x=23.5,y=540,text="Ti/V=50",cex=0.7,col=col2)
    ))
}

sheet<<-list(demo=list(fun="plot", 
                       call=list(xlim=c(0,25),
                                 ylim=c(0,600),
                                 main=annotate("Ti/V (Shervais 1982)"),
                                 col="green",
                                 bg="transparent",
                                 fg="black",
                                 xlab="Ti (ppm)/1000",
                                 ylab="V (ppm)"),
                       template=temp))

}
