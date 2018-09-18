#####################################################################
#               Pearce Nb/Yb - TiO2/Yb                              #
#####################################################################

PearceNbTiYb<-function(reservoirs=TRUE,xmin=0.1,xmax=100,ymin=0.1,ymax=10){
    if(!getOption("gcd.plot.bw")){
        col1<-plt.col[1]
        col2<-plt.col[2]
    }else{
        col1<-"black"
        col2<-"black"
    }

    #Plot WRCompositions
    x.data<<-WR[,"Nb"]/WR[,"Yb"]
    y.data<<-WR[,"TiO2"]/WR[,"Yb"]
    
    # calculate ticks
    if( names(dev.cur())=="null device") new.win<-TRUE else new.win<-FALSE # axTicks opens an empty window  if none is currently open
    at<-axTicks(2,axp = c(ymin,ymax,1), usr = c(floor(log10(ymin)),ceiling(log10(ymax))),log = TRUE)
    at.x<-as.vector(apply(t(at),2,function(i)i*1:10))

    at<-axTicks(1,axp = c(xmin,xmax,1), usr = c(floor(log10(xmin)),ceiling(log10(xmax))),log = TRUE)
    at.y<-as.vector(apply(t(at),2,function(i)i*1:10))

    if(new.win) dev.off(dev.cur()) # Close the empty window
    
    temp<-list(
        polygon1=list("polygon",x=c(0.1,100,100,0.1,0.1),y=c(0.25,0.34,0.8,0.57,0.25),col="khaki"),
        lines1=list("abline",a=0.355496,b=0.046,lty="solid",col=col2),
        lines2=list("lines",x=c(1.591325,1.775364),y=c(0.4736188,0.1994768),lty="dashed",col=col2,lwd=2),
        lines3=list("lines",x=c(8.8,17.4),y=c(2.4588760,0.7297888),lty="dashed",col=col2,lwd=2),
        rug1=list("rug",x=at.x,ticksize=0.015,side=1,lwd=1,col="black"), 
        rug2=list("rug",x=at.y,ticksize=0.015,side=2,lwd=1,col="black"), 
        GCDkit=list("NULL",plot.type="binary",plot.position=301.5,plot.name="Pearce (2008) Nb-TiO2-Yb")
    )
    if(getOption("gcd.plot.text")){    
         temp<-c(temp,list(
            text1=list("text",x=7,y=1.8,text="Th",cex=1,col=col2,adj=0,srt=2),
            text2=list("text",x=12,y=1.8,text="Alk",cex=1,col=col2,adj=0,srt=2),
            text3=list("text",x=0.4,y=1.5,text="OIB array\n(deep melting)",cex=1,col=col2,adj=0.5,srt=5),
            text4=list("text",x=25,y=0.5,text="MORB array\n(shallow melting)",cex=1,col=col2,adj=0.5,hadj=0.5,srt=5),
            text5=list("text",x=1.5,y=0.25,text="N",cex=1,col=col2,adj=0.5,hadj=0.5,srt=5),
            text6=list("text",x=2.05,y=0.25,text="E",cex=1,col=col2,adj=0.5,hadj=0.5,srt=5)
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
                                 ylab=annotate("TiO2/Yb"),
                                 main=annotate("Nb/Yb - TiO2/Yb (Pearce 2008)")),
                       template=temp))
}
