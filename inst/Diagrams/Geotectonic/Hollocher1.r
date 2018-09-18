###############################################################################
#                   Classification of subalkaline rocks                       #
#  Hollocher K, Robinson P, Walsh E, Roberts D (2012) Geochemistry of         #
# amphibolite-facies volcanics and gabbros of the Storen Nappe in extensions  #
# west and southwest of Trondheim, western gneiss region, Norway: A key to    #
# correlations and paleotectonic settings. Amer J Sci 312:357-416             #
###############################################################################

# Coordinates and graph layout from website of Kurt Hollocher, 
# http://minerva.union.edu/hollochk/c_petrology/discrim.html

Hollocher1<-function(){
    if(!getOption("gcd.plot.bw")){
        col1<-plt.col[3]
        col2<-plt.col[2]
    }else{
        col1<-"black"
        col2<-"black"
    }

    x.data<<-WR[,"La"]/WR[,"Yb"]
    y.data<<-WR[,"Nb"]/WR[,"La"]

    temp1<-list(
        lines1=list("lines",x=c(0.4,80,10,10,22,10,10,1.75,7.5,1.75,2.44,6),y=c(0.5,0.95,0.739,0.25,0.09,0.25,0.739,0.598,0.09,0.598,0.622,3),col=col2),
        GCDkit=list("NULL",plot.type="binary",plot.position=211,plot.name="Hollocher et al. (2012) La/Yb vs. Nb/La") 
    )  
    
    temp2<-list(      
        text1=list("text",x=1.4,y=1.5,text="MORB",cex=1,col=col2,adj=0.5),
        text2=list("text",x=17.2,y=1.5,text="Oceanic\nislands",cex=1,col=col2,adj=0.5),
        text3=list("text",x=1.3,y=0.2,text="Oceanic\narcs",cex=1,col=col2,adj=0.5),
        text4=list("text",x=6.8,y=0.2,text="Continental\narcs",cex=1,col=col2,adj=0.5),
        text5=list("text",x=38.2,y=0.2,text="Alkaline\narcs",cex=1,col=col2,adj=0.5)
    )

    if(getOption("gcd.plot.text")) temp<-c(temp1,temp2) else temp<-temp1

    sheet<<-list(demo=list(fun="plot", 
                       call=list(xlim=c(0.5,100),
                                 ylim=c(0.05,5),
                                 log="xy",
                                 col="green",
                                 bg="transparent",
                                 fg="black",
                                 xlab=annotate("La/Yb"),
                                 ylab=annotate("Nb/La"),
                                 main=annotate("La/Yb - Nb/La (Hollocher et al. 2012)")),
                       template=temp))
}
