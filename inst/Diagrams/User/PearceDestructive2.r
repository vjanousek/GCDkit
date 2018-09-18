###############################################################################
#                   Classification of volcanic rocks                          #
#  Pearce JA (1982) Trace element characteristics of lavas from destructive   #
#  plate boundaries. In: Thorpe RS (eds) Andesites; Orogenic Andesites and    #
#  Related Rocks. John Wiley & Sons, Chichester, pp 525-548                   #
###############################################################################

# Coordinates and graph layout from website of Kurt Hollocher, 
# http://minerva.union.edu/hollochk/c_petrology/discrim.html

# Fig. 8
PearceDestructive2<-function(reservoirs=TRUE,plot.txt=getOption("gcd.plot.text")){
    if(!getOption("gcd.plot.bw")){
        col1<-plt.col[3]
        col2<-plt.col[2]
    }else{
        col1<-"black"
        col2<-"black"
    }

    x.data<<-WR[,"Ta"]/WR[,"Yb"]
    y.data<<-WR[,"Th"]/WR[,"Yb"]

    temp1<-list(
        lines1=list("lines",x=c(0.02,0.16,0.02,0.16,0.51,0.81,1.29,0.02,1.29,10.02,1.29,0.81,0.69,0.81,1.55,2.53,1.55,17.02,1.55,1.00,0.64,0.98,0.64,0.28,0.64,0.04),y=c(0.02,0.26,0.32,0.26,1.06,1.72,2.80,3.98,2.80,24.01,2.80,1.72,2.01,1.72,0.95,0.60,0.95,11.49,0.95,0.61,0.36,0.25,0.36,0.73,0.36,0.01),col=col2),
        GCDkit=list("NULL",plot.type="binary",plot.position=2,plot.name="Ta/Yb - Th/Yb (Pearce 1982)") # TO BE EDITED
    )  
    
    temp2<-list(      
        text1=list("text",x=0.02,y=0.15,text="Tholeiitic",cex=1,col=col2,adj=0),
        text2=list("text",x=0.04,y=1.24,text="Calc-alkaline",cex=1,col=col2,adj=0),
        text3=list("text",x=0.1,y=15,text="Shoshonitic",cex=1,col=col2,adj=0),
        text4=list("text",x=0.15,y=0.1,text="Th.",cex=1,col=col1,adj=0.5,srt=45),
        text5=list("text",x=0.75,y=0.77,text="Trans.",cex=1,col=col1,adj=0.5,srt=45),
        text6=list("text",x=4.5,y=5.1,text="Alk.",cex=1,col=col1,adj=0.5,srt=45),
        text7=list("text",x=0.05,y=0.2,text="V o l c a n i c   A r c s",cex=1.5,col="gray30",adj=0,srt=45)
    )

    if(getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1
    
    #Plot reservoirs
    
    if(reservoirs){
        temp<-c(temp,list(
            reservoirs=list("reservoirs",var.name="reservoirs.data",reserv.condition=c("Depleted"),labs=c("DM"),pch="*",col="black",cex=1)
        ))
    }       
        
    sheet<<-list(demo=list(fun="plot", 
                       call=list(xlim=c(0.01,100),
                                 ylim=c(0.01,100),
                                 log="xy",
                                 col="green",
                                 bg="transparent",
                                 fg="black",
                                 xlab=annotate("Ta/Yb"),
                                 ylab=annotate("Th/Yb"),
                                 main=annotate("Ta/Yb - Th/Yb (Pearce 1982)")),
                       template=temp))
}
