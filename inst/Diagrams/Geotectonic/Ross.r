###############################################################################
#                   Classification of subalkaline rocks                       #
#  Ross PS, Bédard LP (2009) Magmatic affinity of modern and ancient          #
#  subalkaline volcanic rocks determined from trace-element discriminant      #
#  diagrams. Can J Earth Sci 46:823-839                                       #
###############################################################################

Ross<-function(){
    if(!getOption("gcd.plot.bw")){
        col1<-plt.col[3]
        col2<-plt.col[2]
    }else{
        col1<-"black"
        col2<-"black"
    }

    x.data<<-WR[,"Zr"]/WR[,"Y"]
    y.data<<-WR[,"Th"]/WR[,"Yb"]

    temp1<-list(
        lines1=list("lines",x=c(1,20),y=c(2.11,0.011),col=col2),
        lines2=list("lines",x=c(1,20),y=c(11,0.059),col=col2),     
        GCDkit=list("NULL",plot.type="binary",plot.position=303,plot.name="Ross + Bedard (2009) Zr/Y-Th/Yb") # TO BE EDITED
    )  
    
    temp2<-list(      
        text1=list("text",x=10,y=0.4,text="\"Calc-alkaline\"",cex=1,col=col2,adj=0),
        text2=list("text",x=7.2,y=0.1,text="\"Transitional\"",cex=1,col=col2,adj=0),
        text3=list("text",x=5,y=0.03,text="\"Tholeiitic\"",cex=1,col=col2,adj=0)
    )

    if(getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    sheet<<-list(demo=list(fun="plot", 
                       call=list(xlim=c(1,20),
                                 ylim=c(0.01,20),
                                 log="xy",
                                 col="green",
                                 bg="transparent",
                                 fg="black",
                                 xlab=annotate("Zr/Y"),
                                 ylab=annotate("Th/Yb"),
                                 main=annotate("Zr/Y - Th/Yb (Ross and Bedard 2009)")),
                       template=temp))
}
