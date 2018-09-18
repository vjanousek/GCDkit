#####################################################################
#               R1-R2 diagram - De la Roche et al. 1980             #
#      + Batchelor and Bowden (1985): Chem.Geology, 48, 43-55       #
#                        boundaries: NewPet                         #
#####################################################################

Batchelor<-function(ideal=TRUE){
if(!getOption("gcd.plot.bw")){
    col1<-plt.col[3]
    col2<-plt.col[2]
}else{
    col1<-"black"
    col2<-"black"
}

#Plot WRCompositions
ee<-LaRocheCalc(WR)
results<<-LaRocheCalc(WR)
x.data<<-ee[,1]
y.data<<-ee[,2]

temp<-list(
        lines1=list("abline",h=0,col=col2),
        lines2=list("abline",v=0,col=col2),
        lines3=list("lines",x=c(525,2875),y=c(500,280),col=col2,lty="dashed"),
        lines4=list("lines",x=c(3000,2150),y=c(270,1720),col=col2,lty="dashed"),     
        lines5=list("lines",x=c(2310,1435),y=c(575,1310),col=col2,lty="dashed"),     
        lines6=list("lines",x=c(2250,1095),y=c(580,1000),col=col2,lty="dashed"),
        lines7=list("lines",x=c(0,2875.27),y=c(0,2875.84),col=col2,lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.position=101,plot.name="Batchelor + Bowden (1985)")
    )
if(getOption("gcd.plot.text")){    
    temp<-c(temp,list(      
        text1=list("text",x=2600,y=1700,text="Mantle\nFractionates",cex=0.7,col=col2),
        text2=list("text",x=2100,y=1250,text="Pre-plate\nCollision",cex=0.7,col=col2),
        text3=list("text",x=1600,y=1000,text="Post-\ncollision\nUplift",cex=0.7,col=col2),
        text4=list("text",x=1400,y=700,text="Late-\norogenic",cex=0.7,col=col2),
        text5=list("text",x=1100,y=350,text="Anorogenic",cex=0.7,col=col2),
        text6=list("text",x=2500,y=500,text="Syn-collision",cex=0.7,col=col2),
        text7=list("text",x=2900,y=250,text="Post-\norogenic",cex=0.7,col=col2)
    ))
}

if(ideal){
#Plot ideal minerals
    idealmin<-read.table(paste(gcdx.dir,"\\","idealmins.data",sep=""), sep="\t")
    idealmin<-data.matrix(idealmin)
    idealmin[is.na(idealmin)]<-0
    FeOt<-idealmin[,"FeO"]+idealmin[,"Fe2O3"]*0.89981
    idealmin<-cbind(idealmin,FeOt)
    
    RR<-LaRocheCalc(idealmin)
    temp3<-list(
        points1=list("points",x=RR[,1],y=RR[,2],col=col1,pch=20,bg="gray"),
        text8=list("text",x=RR[,1],y=RR[,2]+100,text=rownames(idealmin),col=col1,cex=0.7)
    )
    temp<-c(temp,temp3)
}

sheet<<-list(demo=list(fun="plot", 
                       call=list(xlim=c(-1000,4000),
                                 ylim=c(0,4000),
                                 col="green",
                                 bg="transparent",
                                 fg="black",
                                 xlab=expression(paste(plain(R[1]),plain("= 4Si - 11(Na + K) - 2(Fe + Ti)"))),
                                 ylab=expression(paste(plain(R[2]),plain("= 6Ca + 2Mg + Al"))),
                                 main=annotate("R1-R2 (Batchelor + Bowden 1985)")),
                       template=temp))
}
