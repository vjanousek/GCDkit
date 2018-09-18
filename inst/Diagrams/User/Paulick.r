#####################################################################
#                  Al2O3/SiO2 vs MgO/SiO2 binary plot               #
#                   Fig. 3 in Paulick et al. (2006)                 #
#  Paulick, H., Bach, W., Godard, M., De Hoog, J.C.M., Suhr, G.,    #
#  Harvey, J., 2006. Geochemistry of abyssal peridotites (Mid-      #
#  Atlantic Ridge, 15o20'N, ODP Leg 209): implications for fluid/   #
#  rock interaction in slow spreading environments. Chemical Geo-   #
#  logy, 234: 179-210.                                              #
#####################################################################

Paulick<-function(){
    x.data<<-WR[,"Al2O3"]/WR[,"SiO2"]
    y.data<<-WR[,"MgO"]/WR[,"SiO2"]

    temp<-list(
        lines11=list("lines",x=c(0,0.1),y=c(1.1,0.842),col="darkgray", lwd=10),
        GCDkit=list("NULL",plot.type="binary",plot.position=12,plot.name="Al2O3/SiO2 - MgO/SiO2 (Paulick et al. 2006)")
    )

    temp2<-list(
        text1=list("text",x=0.095,y=1.008,text="Terrestrial Array\n(melting trend)",col="black",adj=c(1,0.5),srt=-10),
        text2=list("text",x=0.002,y=1.15,text="Depleted",col=plt.col[2],adj=c(0,0.5)),
        text3=list("text",x=0.099,y=0.83,text="Enriched",col=plt.col[2],adj=c(1,1))
    )

    if(getOption("gcd.plot.text")) temp<-c(temp,temp2)

    sheet<<-list(demo=list(fun="plot",call=list(xlim=c(0,0.1),ylim=c(0,1.4),main=annotate("Al2O3/SiO2 - MgO/SiO2 plot (Paulick et al. 2006)"),col="green",bg="transparent",fg="black",xlab=annotate("Al2O3/SiO2"),ylab=annotate("MgO/SiO2")),template=temp))
}
