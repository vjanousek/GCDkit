#####################################################################
#                    Winchester and Floyd 1977, sour. NewPet        #
#####################################################################

WinFloyd1<-function(){
template=list(
    clssf=list("NULL",use=2:12,rcname=c("Trachyandesite","Alk-Bas","Basanite/Nephelinite","Trachyte","Phonolite","Comendite/Pantellerite","Rhyolite","Rhyodacite/Dacite","Andesite","Andesite/Basalt","SubAlkaline Basalt")),
    lines1=list("NULL",x=log10(c(2.9,5.5,1.2,0.94,0.65,0.65,1.4,2.9)),y=log10(c(0.015,0.04,0.092,0.13,0.085,0.019,0.025,0.015)),col="red",lwd=3),
    lines2=list("NULL",x=log10(c(0.65,2.9,2.9,1.4,0.65,0.65)),y=log10(c(0.0016,0.004,0.015,0.025,0.019,0.0016)),col="red",lwd=3),
    lines3=list("NULL",x=log10(c(2.9,10,5.5,2.9,2.9)),y=log10(c(0.004,0.04,0.04,0.015,0.004)),col="red",lwd=3),
    lines4=list("NULL",x=log10(c(10,10,6,4,2.8,2.1,1.9,1.2,5.5,10)),y=log10(c(0.04,0.14,0.19,0.27,0.35,0.58,1.4,0.094,0.04,0.04)),col="red",lwd=3),
    lines5=list("NULL",x=log10(c(10,1.6,1.9,2.1,2.8,4,6,10)),y=log10(c(0.14,5,1.4,0.58,0.35,0.27,0.19,0.14)),col="red",lwd=3),
    lines6=list("NULL",x=log10(c(1.2,1.9,1.6,0.33,0.94,1.2)),y=log10(c(0.094,1.4,5,0.72,0.13,0.094)),col="red",lwd=3),
    lines7=list("NULL",x=log10(c(0.65,0.94,0.33,0.19,0.65)),y=log10(c(0.085,0.13,0.72,0.12,0.085)),col="red",lwd=3),
    lines8=list("NULL",x=log10(c(0.65,0.65,0.19,0.022,0.34,0.36,0.65)),y=log10(c(0.027,0.085,0.12,0.06,0.025,0.025,0.027)),col="red",lwd=3),
    lines9=list("NULL",x=log10(c(0.48,0.65,0.65,0.36,0.34,0.022,0.058,0.48)),y=log10(c(0.014,0.019,0.027,0.025,0.025,0.06,0.012,0.014)),col="red",lwd=3),
    lines10=list("NULL",x=log10(c(0.21,0.48,0.058,0.027,0.21)),y=log10(c(0.0055,0.014,0.012,0.0047,0.0055)),col="red",lwd=3),
    lines11=list("NULL",x=log10(c(0.65,0.65,0.48,0.21,0.027,0.65)),y=log10(c(0.0016,0.019,0.014,0.0055,0.0047,0.0016)),col="red",lwd=3),
    GCDkit=list("NULL",plot.type="binary",plot.position=26,plot.name="Nb/Y - Zr/TiO2 (Winchester + Floyd 1977)"),

    lines21=list("lines",x=c(0.33,0.94,1.2,1.9,1.6),y=c(0.72,0.13,0.094,1.4,5),col=plt.col[2]),
    lines22=list("lines",x=c(1.2,5.5,2.9,2.9),y=c(.094,0.04,.015,0.004),col=plt.col[2]),
    lines23=list("lines",x=c(2.9,1.4,.65,.48),y=c(.015,0.025,0.019,0.014),col=plt.col[2]),
    lines24=list("lines",x=c(.94,.65,.65),y=c(.13,.085,0.0016),col=plt.col[2]),
    lines25=list("lines",x=c(.36,.65),y=c(.025,0.027),col=plt.col[2]),
    lines26=list("lines",x=c(.027,.21,.48,.058),y=c(0.0047,0.0055,0.014,0.012),lty="dashed",col=plt.col[2]),
    lines27=list("lines",x=c(.022,0.34),y=c(0.06,0.025),lty="dashed",col=plt.col[2]),
    lines28=list("lines",x=c(.19,.65),y=c(.12,.085),lty="dashed",col=plt.col[2]),
    lines29=list("lines",x=c(5.5,10),y=c(0.04,0.04),lty="dashed",col=plt.col[2]),
    lines30=list("lines",x=c(1.9,2.1,2.8,4,6,10),y=c(1.4,.58,.35,.27,.19,.14),col=plt.col[2],lty="dashed")
)

  if(getOption("gcd.plot.text")){
    template<-c(template,list(
        text1=list("text",x=4,y=2,text="Phonolite",col=plt.col[2],cex=1,adj=0),
        text2=list("text",x=2,y=0.15,text="Trachyte",col=plt.col[2],cex=1,adj=0),
        text3=list("text",x=.2,y=.3,text="Rhyolite",col=plt.col[2],cex=1,adj=0),
        text4=list("text",x=0.02,y=0.008,text="Andesite/Basalt",col=plt.col[2],cex=1,adj=0),
        text5=list("text",x=0.02,y=.1,text="Rhyodacite/Dacite",col=plt.col[2],cex=1,adj=0),
        text6=list("text",x=0.04,y=.002,text="SubAlkaline Basalt",col=plt.col[2],cex=1,adj=0),
        text7=list("text",x=1,y=0.05,text="Trachy-\nandesite",col=plt.col[2],cex=1,adj=0),
        text8=list("text",x=1,y=0.003,text="Alk-Bas",col=plt.col[2],cex=1,adj=0),
        text9=list("text",x=0.5,y=2,text="Comendite\nPantellerite",col=plt.col[2],cex=1,adj=0),
        text10=list("text",x=3.5,y=0.01,text="Basanite\nNephelinite",col=plt.col[2],cex=1,adj=0),
        text11=list("text",x=.015,y=.04,text="Andesite",col=plt.col[2],cex=1,adj=0)))
}

#if (!(just.sheets)) {
#if (.check.colnames(colnames(WR),c("Nb","Y","Zr","TiO2"))) xx<<-"error" else {
    x.data<<-(WR[,"Nb"]/WR[,"Y"])
    y.data<<-(WR[,"Zr"]/WR[,"TiO2"]/1e4)
#}
#}
sheet<<-list(demo=list(fun="plot",call=list(xlim=c(0.01,10),ylim=c(0.001,5),main=annotate("Nb/Y - Zr/TiO2 plot (Winchester and Floyd 1977)"),bg="transparent",fg="black",asp=1,axes=TRUE,log="xy",xlab="Nb/Y",ylab=annotate("Zr/TiO2")),template=template))
}
