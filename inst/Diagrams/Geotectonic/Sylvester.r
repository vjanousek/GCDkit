#####################################################################
#     Diagram to distinguish post-collisional alkaline granites     #
#               Sylvester(1989): J. Geol., 97, 261-280              #
#                                                                   #
#####################################################################

Sylvester<-function(){
if(!getOption("gcd.plot.bw")){
    col1<-plt.col[3]
    col2<-plt.col[2]
}else{
    col1<-"black"
    col2<-"black"
}

i<-rownames(subset(WR,WR[,"SiO2"]>68))
#i<-WR[,"SiO2"]>68
x.data<<-100*((WR[i,"MgO"]+WR[i,"FeOt"]+WR[i,"TiO2"])/WR[i,"SiO2"])
y.data<<-(WR[i,"Al2O3"]+WR[i,"CaO"])/(WR[i,"FeOt"]+WR[i,"Na2O"]+WR[i,"K2O"]) 

temp<-list(
        lines1=list("lines",x=c(1.6,4.4,9),y=c(1.7,1.44,1.44),col=col2,lty="solid"),
        lines2=list("lines",x=c(3,4.4),y=c(1.28,1.44),col=col2,lty="dashed"),     
        GCDkit=list("NULL",plot.type="binary",plot.position=107,plot.name="Sylvester (1989)")
    )
if(getOption("gcd.plot.text")){
    temp<-c(temp,list(      
        text1=list("text",x=1,y=1.85,text="Calc-alkaline & Strongly peraluminous",cex=1,adj=0,col=col2),
        text2=list("text",x=1,y=1.15,text="Alkaline",cex=1,adj=0,col=col2),
        text3=list("text",x=9,y=2.15,text=expression({SiO[2]} > 68*symbol(" ")*wt.*symbol("%")),cex=1,adj=0,col=col1)
    ))
}

#sheet<<-list(demo=list(fun="plot",call=list(xlim=c(10,1000),ylim=c(1,20),main="",col="green",bg="transparent",fg="black",log="xy",xlab="Zr",ylab="Zr/Y"),template=temp))

sheet<<-list(demo=list(fun="plot",
                       call=list(xlim=c(0,12),
                                 ylim=c(0.8,2.2), 
                                 xlab=expression(100*(MgO+FeOt+TiO[2])/SiO[2]),
                                 ylab=expression((Al[2]*O[3]+CaO)/(FeOt+Na[2]*O+K[2]*O)),
                                 main="Sylvester (1989)",
                                 col="green",
                                 bg="transparent",
                                 fg="black"),
                       template=temp))
}
