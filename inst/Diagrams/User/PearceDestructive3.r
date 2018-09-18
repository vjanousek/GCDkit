###############################################################################
#                   Classification of volcanic rocks                          #
#  Pearce JA (1982) Trace element characteristics of lavas from destructive   #
#  plate boundaries. In: Thorpe RS (eds) Andesites; Orogenic Andesites and    #
#  Related Rocks. John Wiley & Sons, Chichester, pp 525-548                   #
###############################################################################

# Coordinates and graph layout from website of Kurt Hollocher, 
# http://minerva.union.edu/hollochk/c_petrology/discrim.html

# Fig. 9
PearceDestructive3<-function(plot.txt=getOption("gcd.plot.text")){
    if(!getOption("gcd.plot.bw")){
        col1<-plt.col[3]
        col2<-plt.col[2]
    }else{
        col1<-"black"
        col2<-"black"
    }
    cex.axis<-1
    cex.lab<-1
    
    x.data<<-WR[,"Nb"]/WR[,"Y"]
    y.data<<-WR[,"Ti"]/WR[,"Y"]

    temp1<-list(
        #axis1=list("axis",side=1,at=c(0.01,0.1,1,10,100),labels=c(0.01,0.1,1,10,100),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        #axis2=list("axis",side=2,at=c(100,200,500,1000,2000),labels=c(100,200,500,1000,2000),cex.axis=cex.axis,cex.lab=cex.lab,las=0,hadj=NA,padj=NA,lty="solid"),
        lines1=list("lines",x=c(0.04,0.04,0.05,0.42,1.55,0.21,0.05,0.04,0.04),y=c(187,183,181,181,386,398,384,380,370),col=col2),
        lines2=list("lines",x=c(2.17,10.60,0.92,0.17,0.40,2.17),y=c(397,1000,1000,416,397,397),col=col2),
        lines3=list("lines",x=c(0.04,0.05,0.16,0.57,0.34,0.28,0.23,0.18,0.14,0.12,0.10,0.08,0.07,0.04),y=c(175,173,173,379,379,384,397,420,449,468,481,491,495,497),col=col1,lwd=2),
        lines4=list("lines",x=c(0.62,0.76),y=c(1476,199),col=col1,lty="dashed"), 
        lines5=list("lines",x=c(1.32,1.61),y=c(1476,199),col=col1,lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.position=3,plot.name="Nb/Y - Ti/Y (Pearce 1982)") # TO BE EDITED
    )  
    
    temp2<-list(      
        text1=list("text",x=0.06,y=270,text="Arc volcanics incl.\nalkaline varieties",cex=1,col=col2,adj=0.5),
        text2=list("text",x=0.04,y=555,text="MORB",cex=1,col=col1,adj=0),
        text3=list("text",x=0.65,y=582,text="Within-plate\nvolcanics",cex=1,col=col2,adj=0),
        text4=list("text",x=0.40,y=1380,text="Th.",cex=0.8,col=col1,adj=0),
        text5=list("text",x=0.9,y=1380,text="Trans.",cex=0.8,col=col1,adj=0.5),
        text6=list("text",x=1.7,y=1380,text="Alk.",cex=0.8,col=col1,adj=0)
    )

    if(getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    sheet<<-list(demo=list(fun="plot", 
                       call=list(xlim=c(0.01,100),
                                 ylim=c(100,2000),
                                 log="xy",
                                 col="green",
                                 bg="transparent",
                                 fg="black",
                                 xlab=annotate("Nb/Y"),
                                 ylab=annotate("Ti/Y"),
                                 main=annotate("Nb/Y - Ti/Y (Pearce 1982)")),
                                 axes=FALSE, # does not work
                       template=temp))
}
