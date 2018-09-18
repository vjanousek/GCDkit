###############################################################################
#                   Classification of subalkaline rocks                       #
#  Hollocher K, Robinson P, Walsh E, Roberts D (2012) Geochemistry of         #
# amphibolite-facies volcanics and gabbros of the Storen Nappe in extensions  #
# west and southwest of Trondheim, western gneiss region, Norway: A key to    #
# correlations and paleotectonic settings. Amer J Sci 312:357-416             #
###############################################################################

# Coordinates and graph layout from website of Kurt Hollocher, 
# http://minerva.union.edu/hollochk/c_petrology/discrim.html

Hollocher2<-function(plot.txt=getOption("gcd.plot.text")){
    if(!getOption("gcd.plot.bw")){
        col1<-plt.col[3]
        col2<-plt.col[2]
    }else{
        col1<-"black"
        col2<-"black"
    }

    x.data<<-WR[,"La"]/WR[,"Yb"]
    y.data<<-WR[,"Th"]/WR[,"Nb"]

    temp1<-list(
        lines1=list("lines",x=c(0.4,7.15,80,7.15,20,7.15,1.9,5.5,1.9,7.15,1.5),y=c(0.095,0.13,0.4,0.13,5,0.13,0.113,5,0.113,0.13,0.02),col=col2),
        GCDkit=list("NULL",plot.type="binary",plot.position=211.5,plot.name="Hollocher et al. (2012) La/Yb vs. Th/Nb") 
    )  
    
    temp2<-list(      
        text1=list("text",x=1.5,y=0.05,text="MORB",cex=1,col=col2,adj=0.5),
        text2=list("text",x=20,y=0.05,text="Oceanic\nislands",cex=1,col=col2,adj=0.5),
        text3=list("text",x=1.2,y=0.8,text="Oceanic\narcs",cex=1,col=col2,adj=0.5),
        text4=list("text",x=6.8,y=0.8,text="Continental\narcs",cex=1,col=col2,adj=0.5),
        text5=list("text",x=32,y=0.8,text="Alkaline\narcs",cex=1,col=col2,adj=0.5)
    )

    if(getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    sheet<<-list(demo=list(fun="plot", 
                       call=list(xlim=c(0.5,100),
                                 ylim=c(0.01,10),
                                 log="xy",
                                 col="green",
                                 bg="transparent",
                                 fg="black",
                                 xlab=annotate("La/Yb"),
                                 ylab=annotate("Th/Nb"),
                                 main=annotate("La/Yb - Th/Nb (Hollocher et al. 2012)")),
                       template=temp))
}
