#####################################################################
#                    #
#####################################################################

OhtaArai<-function(fixTi=F){
    usedox<-c("SiO2","TiO2","Al2O3","Fe2O3t","MgO","CaO_corr","Na2O","K2O")
    oxsubset<-c("SiO2","TiO2","Al2O3","FeOt","MgO","CaO","Na2O","K2O","P2O5","CO2")
    my.WR<-WR[,oxsubset]
    Fe2O3t<-my.WR[,"FeOt"]*1.111
    
    my.WR[is.na(my.WR)]<-0
    mCaO<-mw["Ca"]+mw["O"]
    mP2O5<-2*mw["P"]+5*mw["O"]
    mCO2<-mw["C"]+2*mw["O"]
    
    apatite<-(my.WR[,"P2O5"]/mP2O5)/3
    carb<-my.WR[,"CO2"]/mCO2
    CaO_corr<-my.WR[,"CaO"]-carb*mCaO-apatite*mCaO*10
    
    ### Empirical fix for missing TiO2 values: TiO2 = FeOT/7
    if(fixTi){
        fixme<-my.WR[,"TiO2"]==0
        my.WR[fixme,"TiO2"]<-my.WR[fixme,"FeOt"]/7
    }
    
    my.WR<-cbind(my.WR,Fe2O3t,CaO_corr)
    my.WR<-normalize2total(my.WR[,usedox],100)
    
    M_apex<- exp(-0.395*log(my.WR[,"SiO2"])+0.206*log(my.WR[,"TiO2"])-0.316*log(my.WR[,"Al2O3"])+0.160*log(my.WR[,"Fe2O3t"])
              +0.246*log(my.WR[,"MgO"])+0.368*log(my.WR[,"CaO_corr"])+0.073*log(my.WR[,"Na2O"])-0.342*log(my.WR[,"K2O"])+2.266)
    
    F_apex<- exp(0.191*log(my.WR[,"SiO2"])-0.397*log(my.WR[,"TiO2"])+0.020*log(my.WR[,"Al2O3"])-0.375*log(my.WR[,"Fe2O3t"])
            -0.243*log(my.WR[,"MgO"])+0.079*log(my.WR[,"CaO_corr"])+0.392*log(my.WR[,"Na2O"])+0.333*log(my.WR[,"K2O"])-0.892)

    W_apex<- exp(0.203*log(my.WR[,"SiO2"])+0.191*log(my.WR[,"TiO2"])+0.296*log(my.WR[,"Al2O3"])+0.215*log(my.WR[,"Fe2O3t"])
            -0.002*log(my.WR[,"MgO"])-0.448*log(my.WR[,"CaO_corr"])-0.464*log(my.WR[,"Na2O"])+0.008*log(my.WR[,"K2O"])-1.374)
            

    suma<-M_apex+F_apex+W_apex
    aa<-F_apex/suma
    bb<-M_apex/suma
    cc<-W_apex/suma
    x.data<<-cc+bb/2
    y.data<<-sqrt(3)*bb/2
  
    qq<-cbind(F_apex,M_apex,W_apex)
    colnames(qq)<-c("F","M","W")
    rownames(qq)<-rownames(my.WR)
    results<<-qq
    #print(results)
  
  # Frame
triang=list(     
        lines21=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        A=list("text",x=0,y=-0.03,text="F",adj=0.5),
        C=list("text",x=1,y=-0.03,text="W",adj=0.5),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text=annotate("M"),adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.position=305,plot.name="Ohta + Arai (2007) FMW weathering index")
        )

x1<-c(0.0194444444444444,
0.075,
0.136111111111111,
0.194444444444444,
0.241666666666667,
0.283333333333333,
0.313888888888889,
0.358333333333333,
0.391666666666667,
0.422222222222222,
0.452777777777778,
0.477777777777778,
0.497222222222222,
0.511111111111111,
0.516666666666667,
0.513888888888889
)

y1<-c(0,
0.0307299336826737,
0.0782216493740783,
0.13130062573506,
0.18717323243083,
0.2430458391266,
0.287743924483217,
0.354791052518141,
0.416250919883488,
0.474917156914047,
0.536377024279394,
0.60063052197953,
0.662090389344877,
0.720756626375436,
0.765454711732052,
0.840882730771342
)

x2<-c(0.161111111111111,
0.213888888888889,
0.291666666666667,
0.386111111111111,
0.458333333333333,
0.538888888888889,
0.619444444444444,
0.708333333333333,
0.813888888888889,
0.891666666666667
)

y2<-c(0.0977770617175979,
0.0810152797088668,
0.0670471280349243,
0.0530789763609817,
0.0446980853566162,
0.0363171943522507,
0.0279363033478851,
0.0195554123435196,
0.0139681516739425,
0.00838089100436551
)



x3<-c(0.455555555555555,
0.505555555555556,
0.541666666666667,
0.586111111111111,
0.636111111111111,
0.683333333333333,
0.752777777777778,
0.816666666666667,
0.880555555555556,
0.936111111111111
)

y3<-c(0.553138806288125,
0.539170654614183,
0.516821611935875,
0.486091678253201,
0.444187223231373,
0.396695507539969,
0.321267488500679,
0.245839469461389,
0.167617820087311,
0.0977770617175979
)

x4<-c(0.522222222222222,
0.583333333333333,
0.65,
0.736111111111111,
0.833333333333333
)

y4<-c(0.765454711732052,
0.667677650014454,
0.569900588296856,
0.427425441222642,
0.265394881804909
)



trends=list(igtrend=list("lines",x=x1,y=y1,col=plt.col[2],lty="dashed",lwd=2),
            grtrend=list("lines",x=x2,y=y2,col=plt.col[2],lty="dotted"),
            #foo=list("points",x=x2,y=y2,pch=15,col=1,cex=1),
            ditrend=list("lines",x=x3,y=y3,col=plt.col[2],lty="dotted"),
            bstrend=list("lines",x=x4,y=y4,col=plt.col[2],lty="dotted")
)

x5<-c(0.15,
0.222222222222222,
0.319444444444444,
0.472222222222222,
0.513888888888889,
0.525)

y5<-c(0.0838089100436554,
0.106157952721963,
0.315680227831102,
0.606217782649107,
0.776629233071206,
0.818533688093034)

mr_names<-c("Rhyo","Gra","Dac","And","Bas","Kom")


annot<-list(model_rocks=list("points",x=x5,y=y5,pch=15,col=plt.col[2],cex=1.5),
            mr_name=list("text",x=x5-0.015,y=y5,text=mr_names,adj=1,cex=0.7,col=plt.col[2])
            )


template<-c(triang,trends,annot)

sheet<<-list(demo=list(fun="plot",call=list(xlim=c(-.03,1.03),ylim=c(-0.05,1.03),main=annotate("FMW (Ohta and Arai 2007)"),bg="transparent",fg="black",asp=1,axes=FALSE,xlab="",ylab=""),template=template))
return(results)
}
