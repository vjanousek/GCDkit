#####################################################################
#              QAPF diagram - Streckeisen (1974)                    #
#####################################################################

QAPFPlut<-function(){
#triangle - which triangle: upper, lower, both or auto 
triangle<-"auto" 
y.data<-0
if (!just.sheets){
    bb <- selectColumnLabel(colnames(WR), message = "Select Q source column, press ENTER to pick from a list\nor enter zero for none", default = "", sample.names = FALSE, empty.ok = FALSE,silent=TRUE)
    if (!is.na(as.numeric(bb))) {
        if (bb!=0){
            bb <- colnames(WR)[bb]
            bb <- WR[, bb]
        }else{
            bb<-seq(0,0,length=nrow(WR))
        }
    }else{
        ee <- calcCore(bb)
        bb <- ee$results
    }

    bb[is.na(bb)]<-0
    
    aa <- selectColumnLabel(colnames(WR), message = "Select A source column\nor press ENTER to pick from a list", default = "", sample.names = FALSE, silent = TRUE, empty.ok = FALSE)
    if (!is.na(as.numeric(aa))) {
            aa <- colnames(WR)[aa]
            aa <- WR[, aa]
    }else {
        ee <- calcCore(aa)
        aa <- ee$results
    }
    
    aa[is.na(aa)]<-0
    
    cc <- selectColumnLabel(colnames(WR), message = "Select P source column\nor press ENTER to pick from a list", default = "", sample.names = FALSE, silent = TRUE, empty.ok = FALSE)
    if (!is.na(as.numeric(cc))) {
            cc <- colnames(WR)[cc]
            cc <- WR[, cc]
    }else{
        ee <- calcCore(cc)
        cc <- ee$results
    }
    
    cc[is.na(cc)]<-0
    
    dd <- selectColumnLabel(colnames(WR), message = "Select F source column, press ENTER to pick \nfrom a list or type 0 for none", default = "", sample.names = FALSE, silent = TRUE, empty.ok = FALSE)
    if (!is.na(as.numeric(dd))) {
        if (dd!=0){
            dd <- colnames(WR)[dd]
            dd <- WR[, dd]
        }else{
            dd<-seq(0,0,length=nrow(WR))
        }
    }
    else {
        ee <- calcCore(dd)
        dd <- ee$results
    }

    dd[is.na(dd)]<-0

suma<-aa+bb+cc+dd 
no.zero<-(suma!=0)
aa<-aa[no.zero]/suma[no.zero]*100
bb<-(bb[no.zero]-dd[no.zero])/suma[no.zero]*100
cc<-cc[no.zero]/suma[no.zero]*100
x.data<-x.data<<-cc+abs(bb)/2
y.data<-y.data<<-sqrt(3)*bb/2
results<-cbind(apply(as.matrix(bb),1,function(x){if (x<0) x<-0;return(x)}),
               100*cc/(aa+cc),
               abs(apply(as.matrix(bb),1,function(x){if (x>0) x<-0;return(x)})))
colnames(results)<-c("Q","100*P/(A+P)","F")
rownames(results)<-rownames(WR)[no.zero]
results<<-results

}   

template<-list(
clssf=list("NULL",use=2:28,rcname=c("quartzolite","quartz-rich granitoid","alk-fsp. granite","syenogranite","monzogranite","granodiorite","tonalite","q-alk-fsp syenite","quartzsyenite","quartzmonzonite","q-monzodiorite q-monzogabbro","q-diorite q-gabbro q-anorthosite","alkali-feldspar syenite","syenite","monzonite","monzodiorite monzogabbro","diorite gabbro anorthosite","foid-bearingalkali-feldspar-syenite","foid-bearing syenite","foid-bearing monzonite","foid-bearing-monzodiorite foid-bearing-monzogabbro","foid-bearing-diorite foid-bearing-gabbro foid-bearing-anorthosite","foid-syenite","foidmonzosyenite","foidmonzosyenite monzogabbro","foid-diorite/foid-gabbro","foidolite")),
polygon1=list("NULL",x=c(45,55,50),y=c(77.94229,77.94229,86.60254),col=plt.col[2]),
polygon2=list("NULL",x=c(30,70,55,45),y=c(51.96152,51.96152,77.94229,77.94229),col=plt.col[2]),
polygon3=list("NULL",x=c(10,18,34,30),y=c(17.32051,17.32051,51.96152,51.96152),col=plt.col[2]),
polygon4=list("NULL",x=c(18,38,44,34),y=c(17.32051,17.32051,51.96152,51.96152),col=plt.col[2]),
polygon5=list("NULL",x=c(38,62,56,44),y=c(17.32051,17.32051,51.96152,51.96152),col=plt.col[2]),
polygon6=list("NULL",x=c(62,82,66,56),y=c(17.32051,17.32051,51.96152,51.96152),col=plt.col[2]),
polygon7=list("NULL",x=c(82,90,70,66),y=c(17.32051,17.32051,51.96152,51.96152),col=plt.col[2]),
polygon8=list("NULL",x=c(2.5,12.0,18,10),y=c(4.330127,4.330127,17.32051,17.32051),col=plt.col[2]),
polygon9=list("NULL",x=c(12.00,35.75,38,18),y=c(4.330127,4.330127,17.32051,17.32051),col=plt.col[2]),
polygon10=list("NULL",x=c(35.75,64.25,62,38),y=c(4.330127,4.330127,17.32051,17.32051),col=plt.col[2]),
polygon11=list("NULL",x=c(64.25,88,82,62),y=c(4.330127,4.330127,17.32051,17.32051),col=plt.col[2]),
polygon12=list("NULL",x=c(88,97.5,90,82),y=c(4.330127,4.330127,17.32051,17.32051),col=plt.col[2]),
polygon13=list("NULL",x=c(0.,10.0,12.0,2.5),y=c(-0.000100,-0.000100,4.330127,4.330127),col=plt.col[2]),
polygon14=list("NULL",x=c(10.00,35.00,35.75,12.00),y=c(-0.00010,-0.0001000,4.330127,4.330127),col=plt.col[2]),
polygon15=list("NULL",x=c(35,65,64.25,35.75),y=c(-0.0001000,-0.0001000,4.330127,4.330127),col=plt.col[2]),
polygon16=list("NULL",x=c(65.00,90.00,88.00,64.25),y=c(-0.0001000,-0.0001000,4.330127,4.330127),col=plt.col[2]),
polygon17=list("NULL",x=c(90.0,100.0,97.5,88.0),y=c(-0.0001000,-0.0001000,4.330127,4.330127),col=plt.col[2]),
polygon18=list("NULL",x=c(5,14,10,0),y=c(-8.660254,-8.660254,-0.0001000,-0.000100),col=plt.col[2]),
polygon19=list("NULL",x=c(14.0,36.5,35,10),y=c(-8.660254,-8.660254,-0.0001000,-0.0001000),col=plt.col[2]),
polygon20=list("NULL",x=c(36.5,63.5,65.0,35.0),y=c(-8.660254,-8.660254,-0.0001000,-0.0001000),col=plt.col[2]),
polygon21=list("NULL",x=c(58.5,86.0,90.0,65.0),y=c(-8.660254,-8.660254,-0.0001000,-0.0001000),col=plt.col[2]),
polygon22=list("NULL",x=c(86,95,100,90),y=c(-8.660254,-8.660254,-0.0001000,-0.0001000),col=plt.col[2]),
polygon23=list("NULL",x=c(30,34,14,5),y=c(-51.961524,-51.961524,-8.660254,-8.660254),col=plt.col[2]),
polygon24=list("NULL",x=c(34,50,50,14),y=c(-51.961524,-51.961524,-8.660254,-8.660254),col=plt.col[2]),
polygon25=list("NULL",x=c(50,66,86,50),y=c(-51.961524,-51.961524,-8.660254,-8.660254),col=plt.col[2]),
polygon26=list("NULL",x=c(66,70,95,86),y=c(-51.961524,-51.961524,-8.660254,-8.660254),col=plt.col[2]),
polygon27=list("NULL",x=c(50,70,30),y=c(-86.60254,-51.96152,-51.96152),col=plt.col[2]),


A=list("text",x=-3,y=0,text="A",adj=0.5),
C=list("text",x=103,y=0,text="P",adj=0.5),
B=list("text",x=50,y=100*sqrt(3)/2+5,text=annotate("Q"),adj=0.5),
D=list("text",x=50,y=-100*sqrt(3)/2-5,text=annotate("F"),adj=0.5),
GCDkit=list("NULL",plot.type="ternary",plot.position=38,plot.name="QAPF diagram (Streckeisen 1974) ")
)

upper<-list(
lines1=list("lines",x=c(45,55),y=c(77.94229,77.94229),col=plt.col[2]),
lines2=list("lines",x=c(30,70),y=c(51.96152,51.96152),col=plt.col[2]),
lines3=list("lines",x=c(10,90),y=c(17.32051,17.32051),col=plt.col[2]),
lines4=list("lines",x=c(2.5,97.5),y=c(4.330127,4.330127),col=plt.col[2]),
lines5=list("lines",x=c(0,100),y=c(0,0),col=plt.col[2]),
lines8=list("lines",x=c(10,34),y=c(0,51.96152),col=plt.col[2]),
lines9=list("lines",x=c(35,38),y=c(0,17.32051),col=plt.col[2]),
lines9=list("lines",x=c(38,44),y=c(17.32051,51.96152),col=plt.col[2],lty="dashed"),
lines10=list("lines",x=c(65,56),y=c(0,51.96152),col=plt.col[2]),
lines11=list("lines",x=c(90,66),y=c(0,51.96152),col=plt.col[2]),
lines17=list("lines",x=c(0,50),y=c(0,86.60254),col=plt.col[2]),
lines18=list("lines",x=c(50,100),y=c(86.60254,0),col=plt.col[2])
)

lower<-list(
lines5=list("lines",x=c(0,100),y=c(0,0),col=plt.col[2]),
lines6=list("lines",x=c(5,95),y=c(-8.660254,-8.660254),col=plt.col[2]),
lines7=list("lines",x=c(30,70),y=c(-51.96152,-51.96152),col=plt.col[2]),
lines12=list("lines",x=c(10,34),y=c(0,-51.96152),col=plt.col[2]),
lines13=list("lines",x=c(35,36.5),y=c(0,-8.660254),col=plt.col[2]),
lines14=list("lines",x=c(65,63.5),y=c(0,-8.660254),col=plt.col[2]),
lines15=list("lines",x=c(90,66),y=c(0,-51.96152),col=plt.col[2]),
lines16=list("lines",x=c(50,50),y=c(-8.660254,-51.96152),col=plt.col[2]),
lines19=list("lines",x=c(0,50),y=c(0,-86.60254),col=plt.col[2]),
lines20=list("lines",x=c(50,100),y=c(-86.60254,0),col=plt.col[2])
)
 
uppertext<-list(    
text1=list("text",x=50,y=80,text="quartzolite",col=plt.col[2]), 
text2=list("text",x=50,y=60,text="quartz-rich\ngranitoid",col=plt.col[2]), 
text3=list("text",x=22,y=35,text="alk-fsp.granite",col=plt.col[2],srt=60),
text4=list("text",x=34,y=35,text="syeno-\ngranite",col=plt.col[2]),
text5=list("text",x=50,y=35,text="monzo-\ngranite",col=plt.col[2]),
text6=list("text",x=66,y=35,text="grano-\ndiorite",col=plt.col[2]),
text7=list("text",x=78,y=35,text="tonalite",col=plt.col[2],srt=-60),
text8=list("text",x=10,y=11,text="q-alk-fsp\nsyenite",col=plt.col[2],adj=1),
text9=list("text",x=27,y=11,text="quartz-\nsyenite",col=plt.col[2]),
text10=list("text",x=50,y=11,text="quartz-\nmonzonite",col=plt.col[2]),
text11=list("text",x=73,y=11,text="q-monzodiorite\nq-monzogabbro",col=plt.col[2]),
text12=list("text",x=88,y=11,text="q-diorite\nq-gabbro\nq-anorthosite",col=plt.col[2],adj=0),
text13=list("text",x=7,y=2,text="1",col=plt.col[2],adj=1),
text14=list("text",x=22,y=2,text="syenite",col=plt.col[2],adj=0.5),
text15=list("text",x=50,y=2,text="monzonite",col=plt.col[2],adj=0.5),
text16=list("text",x=78,y=2,text="2",col=plt.col[2],adj=0.5),
text17=list("text",x=93,y=2,text="3",col=plt.col[2],adj=0),
text18=list("text",x=0,y=80,text="1 - alkali-feldspar\nsyenite",col=plt.col[2],adj=0),
text19=list("text",x=0,y=68,text="2 - monzodiorite\nmonzogabbro",col=plt.col[2],adj=0),
text20=list("text",x=0,y=56,text="3 - diorite, gabbro",col=plt.col[2],adj=0)
)

lowertext<-list( 
text21=list("text",x=7,y=-4,text="4",col=plt.col[2],adj=0.5),
text22=list("text",x=22,y=-4,text="foid-bearing\nsyenite",col=plt.col[2],adj=0.5),
text23=list("text",x=50,y=-4,text="foid-bearing\nmonzonite",col=plt.col[2],adj=0.5),
text24=list("text",x=78,y=-4,text="5",col=plt.col[2],adj=0.5),
text25=list("text",x=93,y=-4,text="6",col=plt.col[2],adj=0.5),
text26=list("text",x=23,y=-35,text="foid-syenite",col=plt.col[2],adj=0.5,srt=-60),
text27=list("text",x=37,y=-20,text="foid-\nmonzosyenite",col=plt.col[2],adj=0.5),
text28=list("text",x=63,y=-20,text="foid-\nmonzodiorite\nmonzogabbro",col=plt.col[2],adj=0.5),
text29=list("text",x=79,y=-30,text="foid-diorite/foid-gabbro",col=plt.col[2],adj=0.5,srt=63),
text30=list("text",x=50,y=-60,text="foidolite",col=plt.col[2],adj=0.5),
text34=list("text",x=0,y=-60,text="4 - foid-bearing-\nalkali-feldspar-syenite",col=plt.col[2],adj=0),
text35=list("text",x=0,y=-72,text="5 - foid-bearing-monzodiorite\nfoid-bearing-monzogabbro",col=plt.col[2],adj=0),
text36=list("text",x=0,y=-84,text="6 - foid-bearing diorite\nfoid-bearing gabbro\nfoid-bearing anorthosite",col=plt.col[2],adj=0)
)

if (triangle=="auto"){
    if (all(y.data>=0,na.rm=TRUE)) triangle<-"upper"    #i is set within plotDiagram() by selectSamples()
    else if (all(y.data<=0,na.rm=TRUE)) triangle<-"lower" else{
        triangle<-"both"
        gcd.plot.text<-FALSE
    }
}

if (getOption("gcd.plot.text")){
    if (triangle=="upper") {template<-c(template,upper,uppertext);ylim<-c(-3,103);main<-"QAPF diagram - Si oversaturated"}
    if (triangle=="lower") {template<-c(template,lower,lowertext);ylim<-c(-103,3);main<-"QAPF diagram - Si undersaturated"}
    if (triangle=="both") {template<-c(template,upper,lower,uppertext,lowertext);ylim<-c(-103,103);main<-"QAPF diagram"}
}else{
    if (triangle=="upper") {template<-c(template,upper);ylim<-c(-3,103);main<-"QAPF diagram - Si oversaturated"}
    if (triangle=="lower") {template<-c(template,lower);ylim<-c(-103,3);main<-"QAPF diagram - Si undersaturated"}
    if (triangle=="both") {template<-c(template,upper,lower) ;ylim<-c(-103,103);main<-"QAPF diagram"}   
}

sheet<<-list(demo=list(fun="plot",call=list(xlim=c(-3,103),main=main,ylim=ylim,xlab="",ylab="",bg="transparent",fg="black",asp=1,axes=FALSE),template=template))
}
