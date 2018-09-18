#####################################################################
#                 P-Q plot of Debon and Le Fort (1988)              #
#                                                                   #
#####################################################################

DebonPQ<-function(){


temp1<-list(
        #classification
        clssf=list("NULL",use=2:13,rcname=c("go","mzgo","mz","s","dq","mzdq","mzq","sq","to","gd","ad","gr")),
        lines1=list("NULL",x=c(-377,-257,-205,-224,-311,-377),y=c(0,0,55,57,57,0)),
        lines2=list("NULL",x=c(-257,-188,-143,-205,-257),y=c(0,0,48,55,0)),
        lines3=list("NULL",x=c(-188,-117,-88,-143,-188),y=c(0,0,43,48,0)),
        lines4=list("NULL",x=c(-117,16,30,-40,-88,-117),y=c(0,0,26,38,43,0)),
        lines5=list("NULL",x=c(-311,-224,-205,-187,-172,-274,-311),y=c(57,57,55,79,104,100,57)),
        lines6=list("NULL",x=c(-205,-143,-125,-103,-172,-187,-205),y=c(55,48,70,107,104,79,55)),
        lines7=list("NULL",x=c(-143,-88,-69,-55,-103,-125,-143),y=c(48,43,77,109,107,70,48)),
        lines8=list("NULL",x=c(-88,-40,30,64,-30,-55,-69,-88),y=c(43,38,26,110,110,109,77,43)),
        lines9=list("NULL",x=c(-274,-172,-156,-134,-119,-216,-230,-256,-274),y=c(100,104,136,190,242,238,190,132,100)),
        lines10=list("NULL",x=c(-172,-103,-89,-71,-59,-119,-134,-156,-172),y=c(104,107,137,190,244,242,190,136,104)),
        lines11=list("NULL",x=c(-103,-55,-44,-29,-17,-59,-71,-89,-103),y=c(107,109,140,190,245,244,190,137,107)),
        lines12=list("NULL",x=c(-55,-30,64,75,90,105,-17,-29,-44,-55),y=c(109,110,110,140,190,250,245,190,140,109)),
        #plot
        lines20=list("abline",h=0,col=plt.col[2]),
        lines21=list("abline",v=0,col=plt.col[2]),
        lines22=list("lines",x=c(-216,-119,-59,-17,105),y=c(238,242,244,245,250),col=plt.col[2],lty="dashed"),
        lines23=list("lines",x=c(-274,-172,-103,-55,-30,64),y=c(100,104,107,109,110,110),col=plt.col[2],lty="dashed"),
        lines24=list("lines",x=c(-311,-224,-205,-143,-88,-40,30),y=c(57,57,55,48,43,38,26),col=plt.col[2],lty="dashed"),
        lines25=list("lines",x=c(-216,-230,-256,-274,-311,-377),y=c(238,190,132,100,57,0),col=plt.col[2],lty="dashed"),
        lines26=list("lines",x=c(-119,-134,-156,-172,-187,-205,-257),y=c(242,190,136,104,79,55,0),col=plt.col[2],lty="dashed"),
        lines27=list("lines",x=c(-59,-71,-89,-103,-125,-143,-188),y=c(244,190,137,107,70,48,0),col=plt.col[2],lty="dashed"),
        lines28=list("lines",x=c(-17,-29,-44,-55,-69,-88,-117),y=c(245,190,140,109,77,43,0),col=plt.col[2],lty="dashed"),
        lines29=list("lines",x=c(105,90,75,64,30,16),y=c(250,190,140,110,26,0),col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.position=34,plot.name="P-Q (Debon + Le Fort 1983)")
        )
temp2<-list(
        text1=list("text",x=25,y=175,text="gr",col=plt.col[2]),
        text2=list("text",x=-55,y=175,text="ad",col=plt.col[2]),
        text3=list("text",x=-110,y=175,text="gd",col=plt.col[2]),
        text4=list("text",x=-190,y=175,text="to",col=plt.col[2]),
        text5=list("text",x=-15,y=80,text="sq",col=plt.col[2]),
        text6=list("text",x=-90,y=80,text="mzq",col=plt.col[2]),
        text7=list("text",x=-150,y=80,text="mzdq",col=plt.col[2]),
        text8=list("text",x=-240,y=80,text="dq",col=plt.col[2]),
        text9=list("text",x=-35,y=20,text="s",col=plt.col[2]),
        text10=list("text",x=-140,y=20,text="mz",col=plt.col[2]),
        text11=list("text",x=-200,y=20,text="mzgo",col=plt.col[2]),
        text12=list("text",x=-290,y=20,text="go",col=plt.col[2])
        )

if(getOption("gcd.plot.text")){
    temp<-c(temp1,temp2)}
    else{
    temp<-temp1
}
debon<-DebonCalc(milli)
x.data<<-debon[,"P"]
y.data<<-debon[,"Q"]
sheet<<-list(demo=list(fun="plot", call=list(xlim=c(-400,120),ylim=c(-20,270),col="green",bg="transparent",fg="black",main=annotate("Diagramme P-Q (Debon et Le Fort)"),xlab="P = K - (Na + Ca)",ylab="Q = Si/3-(K+Na+2Ca/3)"),template=temp))
ee<-DebonCalc(milli)
return(ee)
}
