Villaseca<-function(){
debon<-DebonCalc(milli)
x.data<<-debon[,"B"]
y.data<<-debon[,"A"]

temp1<-list(
    clssf=list("NULL",use=2:7,rcname=c("undefined","l-P","m-P","h-P","f-P","peraluminous","metaluminous")),
    lines1=list("lines",x=c(0.41,6.86,14.91,21.21,25.63,28.85,30.81,32.91,35.68,38.32,42.13,46.35,47.82,51.56,55.09,58.23,61.74,65.57,71.92,78.88,0.46,0.41),y=c(101.16,101.1,100.9,100.32,99.56,98.87,105.35,112.51,120.94,128.46,138.97,150.73,154.59,162.18,168.2,173.1,178.18,183.42,191.57,199.97,199.97,101.16)),
    lines2=list("lines",x=c(132.6,126.2,119.44,113.45,110.53,94.58,79.54,71.49,62.5,56.84,47.34,38.6,33.37,33.24,33.09,32.84,32.1,29.71,132.6),y=c(0.18,2.13,4.09,5.84,6.68,11.17,15.03,16.94,18.92,20.06,22.04,23.75,24.74,22.88,20.88,18.87,13.16,0.23,0.18)),
    lines3=list("lines",x=c(200.3,193.8,189.97,187.86,173.18,172.04,165,161.4,157.77,153.09,149.84,146.59,143.49,139.99,137.14,133.64,130.54,127.44,124.37,106.92,101.21,90.21,80.15,74.34,68.57,59.17,53.97,48.74,43.73,38.25,37.38,36.93,36.34,35.61,34.72,34.57,33.6,33.57,33.52,33.37,38.6,47.34,56.84,62.5,71.49,79.54,94.58,110.53,113.45,119.44,126.2,132.77,200.13,200.3),
        y=c(52.77,49.96,48.2,47.31,42.82,42.54,40.89,40.15,39.49,38.81,38.35,38.15,38.12,38.1,38.43,38.65,38.86,39.06,39.52,42.18,43.25,45.43,47.44,48.58,49.68,51.15,51.96,52.7,53.18,53.74,50.34,46.53,42.54,38.37,33.57,32.58,25.75,25.45,25.17,24.74,23.75,22.04,20.06,18.92,16.94,15.03,11.17,6.68,5.84,4.09,2.13,0.23,0.23,52.77)),
    lines4=list("lines",x=c(37.94,43.73,48.74,53.97,59.17,68.57,74.34,80.15,90.21,101.21,106.92,124.37,127.44,130.54,133.64,137.14,139.99,143.49,146.59,149.84,153.09,157.77,161.4,165,172.04,173.18,187.86,189.97,193.8,200.23,200.13,78.88,71.92,65.57,61.74,58.23,55.09,51.56,47.82,46.35,42.13,38.32,35.68,32.91,30.81,28.85,30.78,32.38,34.57,36.01,36.85,37.74,38.45,38.7,38.81,37.94),
        y=c(53.74,53.18,52.7,51.96,51.15,49.68,48.58,47.44,45.43,43.25,42.18,39.52,39.06,38.86,38.65,38.43,38.1,38.12,38.15,38.35,38.81,39.49,40.15,40.89,42.54,42.82,47.31,48.2,49.96,53.33,199.97,199.97,191.57,183.42,178.18,173.1,168.2,162.18,154.59,150.73,138.97,128.46,120.94,112.51,105.35,98.87,97.37,95.87,93.66,91.3,89.27,85.69,80.2,76.44,66.31,53.74)),
    lines5=list("lines",x=c(0.00,6.86,14.91,21.21,25.63,28.52,30.78,32.38,34.57,36.01,36.85,37.74,38.45,38.7,38.81,37.69,37.38,36.93,36.34,35.61,34.72,34.57,33.6,33.3,33.12,32.84,32.1,29.71,0.46,0.00),
        y=c(101.16,101.1,100.9,100.32,99.56,98.95,97.37,95.87,93.66,91.3,89.27,85.69,80.2,76.44,66.31,53.82,50.34,46.53,42.54,38.37,33.57,32.58,25.75,23.47,21.16,18.87,13.16,0.23,0.23,101.16)),
    lines6=list("lines",x=c(0,600,600,0,0),y=c(-600,-60,0,0,-600)),
    lines7=list("lines",x=c(0.00,6.86,14.91,21.21,25.63,28.52,30.78,32.38,34.57,36.01,36.85,37.74,38.45,38.7,38.81,37.69,37.38,36.93,36.34,35.61,34.72,34.57,33.6,33.3,33.12,32.84,32.1,29.71),
        y=c(101.16,101.1,100.9,100.32,99.56,98.95,97.37,95.87,93.66,91.3,89.27,85.69,80.2,76.44,66.31,53.82,50.34,46.53,42.54,38.37,33.57,32.58,25.75,23.47,21.16,18.87,13.16,0.00),col=plt.col[2],lty="dashed"),
    lines8=list("lines",x=c(38.78,43.73,48.74,53.97,59.17,68.57,74.34,80.15,90.21,101.21,106.92,124.37,127.44,130.54,133.64,137.14,139.99,143.49,146.59,149.84,153.09,157.77,161.4,165,172.04,173.18,187.86,189.97,193.8,200.3),
        y=c(53.64,53.18,52.7,51.96,51.15,49.68,48.58,47.44,45.43,43.25,42.18,39.52,39.06,38.86,38.65,38.43,38.1,38.12,38.15,38.35,38.81,39.49,40.15,40.89,42.54,42.82,47.31,48.2,49.96,52.77),col=plt.col[2],lty="dashed"),
    lines9=list("lines",x=c(29.16,30.81,32.91,35.68,38.32,42.13,46.35,47.82,51.56,55.09,58.23,61.74,65.57,71.92,78.88),y=c(98.82,105.35,112.51,120.94,128.46,138.97,150.73,154.59,162.18,168.2,173.1,178.18,183.42,191.57,199.97),col=plt.col[2],lty="dashed"),
    lines10=list("lines",x=c(33.27,38.6,47.34,56.84,62.5,71.49,79.54,94.58,110.53,113.45,119.44,126.2,132.77),y=c(24.58,23.75,22.04,20.06,18.92,16.94,15.03,11.17,6.68,5.84,4.09,2.13,0.23),col=plt.col[2],lty="dashed"),
    lines11=list("abline",h=0,col=1),
    GCDkit=list("NULL",plot.type="binary",plot.position=36,plot.name="B-A plot (modified by Villaseca et al. 1998)")
    )
temp2<-list(
    text1=list("text",x=150,y=100,text="h-P",col=plt.col[2],adj=0),
    text2=list("text",x=150,y=25,text="m-P",col=plt.col[2],adj=0),
    text3=list("text",x=60,y=10,text="l-P",col=plt.col[2],adj=0),
    text4=list("text",x=10,y=50,text="f-P",col=plt.col[2],adj=0),
    text5=list("text",x=100,y=-70,text="Metaluminous",col=plt.col[2],adj=0,font=3),
    text6=list("text",x=100,y=70,text="Peraluminous",col=plt.col[2],adj=0,font=3)
    )
if(getOption("gcd.plot.text")){
    temp<-c(temp1,temp2)}
    else{
    temp<-temp1
}



sheet<<-list(demo=list(fun="plot", call=list(xlim=c(0,200),ylim=c(-100,200),col="green",bg="transparent",fg="black",main=annotate("B-A plot (modified by Villaseca et al. 1998)"),xlab="B = Fe + Mg +Ti",ylab="A = Al - (K + Na + 2Ca)"),template=temp))
result<-DebonCalc(milli)
}
