#####################################################################
#                 Pearce Nb/Yb - Th/Yb                              #
#####################################################################

PearceNbThYb<-function(reservoirs=TRUE,xmin=0.1,xmax=1000,ymin=0.01,ymax=100){
    if(!getOption("gcd.plot.bw")){
        col1<-plt.col[1]
        col2<-plt.col[2]
    }else{
        col1<-"black"
        col2<-"black"
    }

    #Plot WRCompositions
    x.data<<-WR[,"Nb"]/WR[,"Yb"]
    y.data<<-WR[,"Th"]/WR[,"Yb"]
    # slope for mantle field
    #b<-(log10(10)-log10(0.01))/(log10(80)-log10(0.1))
    b<-(log10(10)-log10(1.2))/(log10(0.8)-log10(0.1))
    
    # calculate ticks
    if( names(dev.cur())=="null device") new.win<-TRUE else new.win<-FALSE # axTicks opens an empty window if none is currently open
    at<-axTicks(2,axp = c(ymin,ymax,1), usr = c(floor(log10(ymin)),ceiling(log10(ymax))),log = TRUE)
    at.x<-as.vector(apply(t(at),2,function(i)i*1:10))

    at<-axTicks(1,axp = c(xmin,xmax,1), usr = c(floor(log10(xmin)),ceiling(log10(xmax))),log = TRUE)
    at.y<-as.vector(apply(t(at),2,function(i)i*1:10))
    
    if(new.win) dev.off(dev.cur()) # Close the empty window

    temp<-list(
        polygon1=list("polygon",x=c(0.1,0.3,1000,1000,800,0.1),y=c(0.01,0.01,48,100,100,0.01),col="khaki"),
        lines1=list("abline",a=log10(10)-log10(0.8),b=b,lty="dashed",col=col2),
        rug1=list("rug",x=at.x,ticksize=0.015,side=1,lwd=1,col="black"), 
        rug2=list("rug",x=at.y,ticksize=0.015,side=2,lwd=1,col="black"), 
        GCDkit=list("NULL",plot.type="binary",plot.position=301,plot.name="Pearce (2008) Nb-Th-Yb")
    )
    if(getOption("gcd.plot.text")){    
        temp<-c(temp,list(
            text1=list("text",x=0.15,y=1,text="Volcanic arc array",cex=1,col=col2,adj=0,srt=42),
            text2=list("text",x=70,y=5,text="MORB-OIB array",cex=1,col=col2,adj=0,srt=42)
        ))
    }


    #Plot Sun & McDonough mantle reservoirs, EMORB, NMORB, OIB
    if(reservoirs){
        temp<-c(temp,list(
            reservoirs=list("reservoirs",var.name="reservoirs.data",reserv.condition=c("NMORB|EMORB|OIB.* McDonough"),labs=c("NMORB","EMORB","OIB"),pch="*",col="darkgreen",cex=1)
        ))
    }        
           
    sheet<<-list(demo=list(fun="plot", 
                       call=list(xlim=c(xmin,xmax),
                                 ylim=c(ymin,ymax),
                                 log="xy",
                                 col="green",
                                 bg="transparent",
                                 fg="black",
                                 xlab=annotate("Nb/Yb"),
                                 ylab=annotate("Th/Yb"),
                                 main=annotate("Nb/Yb - Th/Yb (Pearce 2008)")),
                       template=temp))
}
