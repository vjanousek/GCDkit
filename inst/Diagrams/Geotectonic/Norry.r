#####################################################################
#                      Classification of basalts                    #
#                         Pearce and Norry 1979                     #
#####################################################################
Norry<-function(){
if(!getOption("gcd.plot.bw")){
    col1<-plt.col[1]
    col2<-plt.col[2]
}else{
    col1<-"black"
    col2<-"black"
}

what<-which(WR[,"Y"]>0&WR[,"Zr"]>0)
#what<-WR[,"Zr"]>0 & WR[,"Y"]>0

x.data<<-WR[what,"Zr"]
y.data<<-WR[what,"Zr"]/WR[what,"Y"]

temp<-list(
        lines1=list("lines",x=c(17,35,115,59,17),y=c(1.35,3.2,3.2,1.35,1.35),col=col2,lty="dashed"),
        lines2=list("lines",x=c(35,72.5,285,130,35),y=c(1.9,3.6,3.6,1.9,1.9),col=col2,lty="dashed"),
        lines3=list("lines",x=c(72.5,210,600,210),y=c(3.6,10,10,3.6),col=col2,lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.position=206,plot.name="Pearce + Norry (1979)")  
)

if(getOption("gcd.plot.text")){       
    temp<-c(temp,list(
        text1=list("text",x=600,y=5.8,text="Within-Plate\nBasalts",col=col2),
        text2=list("text",x=17,y=2.2,text="Island Arc\nBasalts",col=col2),
        text3=list("text",x=280,y=2.3,text="Mid-Ocean\nRidge Basalts",col=col2)
    ))
}   

sheet<<-list(demo=list(fun="plot",call=list(
                            main=annotate("Zr-Zr/Y (Pearce and Norry 1979)"),
                            xlim=c(10,1000),
                            ylim=c(1,20),
                            col="green",
                            bg="transparent",
                            fg="black",
                            log="xy",
                            xlab="Zr",
                            ylab="Zr/Y"),
                template=temp))
}
