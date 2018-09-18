#####################################################################
#                    Winchester and Floyd 1977, sour. NewPet        #
#####################################################################
WinFloyd2<-function(){

template=list(
    clssf=list("NULL",use=2:11,rcname=c("TrAn","Bazanit/Trachybazanit/Nefelinit","Fonolit","Trachyt","Comendit/Pantellerit","Ryolit","Ryodacit/Dacit","Andezit","Sub-AB","AB")),
    lines1=list("NULL",x=log10(c(0.022,0.031,0.1,0.06,0.04,0.0155,0.022)),y=c(53,56,60,61,62,53,53),col=plt.col[2],lwd=3),
    lines2=list("NULL",x=log10(c(0.01,0.155,0.031,0.01)),y=c(45,49,53,45),col=plt.col[2],lwd=3),
    lines3=list("NULL",x=log10(c(0.155,5,0.1,0.031,0.022,0.031,0.155)),y=c(49,62,60,56,53,53,49),col=plt.col[2],lwd=3),
    lines4=list("NULL",x=log10(c(0.1,5,1.3,0.08,0.04,0.06,0.1)),y=c(60,62,66,66,62,61,60),col=plt.col[2],lwd=3),
    lines5=list("NULL",x=log10(c(1.3,0.7,0.25,0.08,1.3)),y=c(66,79,73,66,66),col=plt.col[2],lwd=3),
    lines6=list("NULL",x=log10(c(0.25,0.7,0.01,0.25)),y=c(73,79,73,73),col=plt.col[2],lwd=3),
    lines7=list("NULL",x=log10(c(0.04,0.08,0.25,0.01,0.006,0.027,0.04)),y=c(62,66,73,73,63,63,62),col=plt.col[2],lwd=3),
    lines8=list("NULL",x=log10(c(0.0155,0.04,0.027,0.006,0.002,0.0155)),y=c(53,62,63,63,53,53),col=plt.col[2],lwd=3),
    lines9=list("NULL",x=log10(c(0.009,0.0175,0.002,0.009)),y=c(47.5,53,53,47.5),col=plt.col[2],lwd=3),
    lines10=list("NULL",x=log10(c(0.01,0.031,0.0175,0.009,0.01)),y=c(45,53,53,47.5,45),col=plt.col[2],lwd=3),
    GCDkit=list("NULL",plot.type="binary",plot.position=28,plot.name="Zr/TiO2 - SiO2 (Winchester + Floyd 1977)"),


    lines21=list("lines",x=c(.7,.04,0.0155),y=c(79,62,53),col=plt.col[2]),
    lines22=list("lines",x=c(0.01,.25),y=c(73,73),col=plt.col[2]),
    lines23=list("lines",x=c(0.08,1.3),y=c(66,66),col=plt.col[2]),
    lines24=list("lines",x=c(0.006,0.027,0.04,0.06,.1,5),y=c(63,63,62,61,60,62),col=plt.col[2]),
    lines25=list("lines",x=c(.002,0.031,.155),y=c(53,53,49),col=plt.col[2]),
    lines26=list("lines",x=c(.1,.031,.022),y=c(60,56,53),col=plt.col[2]),
    lines27=list("lines",x=c(.01,.031),y=c(45,53),col=plt.col[2]),
    lines28=list("lines",x=c(.009,.0175),y=c(47.5,53),lty="dashed",col=plt.col[2])
)



if(getOption("gcd.plot.text")){
    template<-c(template,list(
    text1=list("text",x=1,y=55,text="Fonolit",col=plt.col[2],cex=1,adj=0),
    text1=list("text",x=1,y=65,text="Trachyt",col=plt.col[2],cex=1,adj=0),
    text1=list("text",x=0.002,y=75,text="Ryolit",col=plt.col[2],cex=1,adj=0),
   #text1=list("text",x=0.02,y=60,text="Andezit",col=plt.col[2],cex=1,adj=0),
    text1=list("text",x=0.002,y=70,text="Ryodacit\nDacit",col=plt.col[2],cex=1,adj=0),
    text1=list("text",x=0.002,y=50,text="Sub-AB",col=plt.col[2],cex=1,adj=0),
    text1=list("text",x=0.04,y=60.5,text="TrAn",col=plt.col[2],cex=1,adj=0),
    text1=list("text",x=0.007,y=46,text="AB",col=plt.col[2],cex=1,adj=0),
    text1=list("text",x=1,y=75,text="Comendit\nPantellerit",col=plt.col[2],cex=1,adj=0),
    text1=list("text",x=0.03,y=46,text="Bazanit\nTrachybazanit\nNefelinit",col=plt.col[2],cex=1,adj=0),
    text1=list("text",x=.002,y=60,text="Andezit",col=plt.col[2],cex=1,adj=0)))
}

#if (!(just.sheets)) {
#    if (.check.colnames(colnames(WR),"Zr")) xx<<-"error" else {
    x.data<<-WR[,"Zr"]/WR[,"TiO2"]/1e4
    y.data<<-WR[,"SiO2"]
#}
#}

sheet<<-list(demo=list(fun="plot",call=list(xlim=c(0.001,10),ylim=c(40,80),main=annotate("Zr/TiO2 - SiO2 diagram (Winchester a Floyd 1977)"),bg="transparent",fg="black",asp=1,axes=TRUE,log="x",xlab=annotate("Zr/TiO2"),ylab=annotate("SiO2")),template=template))

}
