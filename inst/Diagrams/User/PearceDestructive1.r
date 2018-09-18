###############################################################################
#                   Classification of volcanic rocks                          #
#  Pearce JA (1982) Trace element characteristics of lavas from destructive   #
#  plate boundaries. In: Thorpe RS (eds) Andesites; Orogenic Andesites and    #
#  Related Rocks. John Wiley & Sons, Chichester, pp 525-548                   #
###############################################################################

# Coordinates and graph layout from website of Kurt Hollocher, 
# http://minerva.union.edu/hollochk/c_petrology/discrim.html

# Fig. 6A
PearceDestructive1<-function(reservoirs=TRUE,plot.txt=getOption("gcd.plot.text")){
    if(!getOption("gcd.plot.bw")){
        col1<-plt.col[3]
        col2<-plt.col[2]
    }else{
        col1<-"black"
        col2<-"black"
    }

    x.data<<-WR[,"Ta"]/WR[,"Yb"]
    y.data<<-WR[,"K2O"]/WR[,"Yb"]

    temp1<-list(
        lines1=list("lines",x=c(0.02,0.84,1.67,6.69,1.67,1.02,3.59,1.02,0.42,0.30,0.16,0.01,0.16,0.30,0.02,0.30,0.42,0.82,1.51,0.82,0.04,0.82,12.01),y=c(1.90,1.04,0.92,3.14,0.92,0.61,0.17,0.61,0.27,0.20,0.23,0.39,0.23,0.20,0.02,0.20,0.27,0.14,0.07,0.14,0.01,0.14,1.39),col=col2),
        GCDkit=list("NULL",plot.type="binary",plot.position=1,plot.name="Ta/Yb - K2O/Yb (Pearce 1982)") # TO BE EDITED
    )  
    
    temp2<-list(      
        text1=list("text",x=0.02,y=0.15,text="Tholeiitic",cex=1,col=col2,adj=0),
        text2=list("text",x=0.04,y=0.7,text="Calc-alkaline",cex=1,col=col2,adj=0),
        text3=list("text",x=0.1,y=2,text="Shoshonitic",cex=1,col=col2,adj=0),
        text4=list("text",x=0.14,y=0.056,text="Th.",cex=1,col=col1,adj=0.5,srt=45),
        text5=list("text",x=0.93,y=0.29,text="Trans.",cex=1,col=col1,adj=0.5,srt=45),
        text6=list("text",x=3.9,y=1.04,text="Alk.",cex=1,col=col1,adj=0.5,srt=45),
        text7=list("text",x=0.32,y=0.54,text="V o l c a n i c   A r c s",cex=1.5,col="gray30",adj=0.5,srt=48)
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
                                 ylim=c(0.01,10),
                                 log="xy",
                                 col="green",
                                 bg="transparent",
                                 fg="black",
                                 xlab=annotate("Ta/Yb"),
                                 ylab=annotate("K2O wt. %/Yb ppm"),
                                 main=annotate("Ta/Yb - K2O/Yb (Pearce 1982)")),
                       template=temp))
}
