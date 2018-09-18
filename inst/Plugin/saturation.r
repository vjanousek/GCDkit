# Calculates Zr saturation following Watson and Harrison (1983) and  Boehnke et al. (2013)
zrSaturation<-function(cats=milli,T=0,Zr=filterOut(WR,"Zr",1)){
    on.exit(options("show.error.messages"=TRUE))
    if(T==0){
        T<-winDialogString("Temperature (degrees C)","750")
        if(is.null(T)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    }
    
    T<-as.numeric(T)+273.15
    oxides<-c("SiO2","TiO2","Al2O3","FeOt","MnO","MgO","CaO","Na2O","K2O","P2O5")
    oxides<-oxides[oxides%in%colnames(cats)]
    cats<-cats[,oxides,drop=FALSE]
    if(length(Zr)>1) cats[names(Zr),oxides]
    
    # NEW
    sums<-apply(cats,1,sum,na.rm=TRUE)
    ee<-sapply(1:nrow(cats),function(i){
        z<-cats[i,]/sums[i]
        return(z)
    })
    cats<-t(ee)
    
    #cats<-normalize2total(cats)/100
   
    M<-(cats[,"Na2O"]+cats[,"K2O"]+2*cats[,"CaO"])/(cats[,"Al2O3"]*cats[,"SiO2"])

    # Watson and Harrison (1983)
    DZr<-exp(-3.8-0.85*(M-1)+12900/T)
    Zr.sat<-497644/DZr
    
    DZr<-497644/Zr
    DZr<-as.vector(DZr)
    TZr.sat.C<-12900/(log(DZr)+3.8+0.85*(M-1))-273.15
    
    # Boehnke et al. (2013
    DZrB<-exp(10108/T-1.16*(M-1)-1.48)
    Zr.satB<-497644/DZrB
    
    DZrB<-497644/Zr
    DZrB<-as.vector(DZrB)
    TZr.satB.C<-10108/(log(DZrB)+1.16*(M-1)+1.48)-273.15
    
    
    y<-cbind(M,Zr,round(Zr.sat,1),round(TZr.sat.C,1),round(Zr.satB,1),round(TZr.satB.C,1))
    colnames(y)<-c("M","Zr.obs","Zr.sat","TZr.sat.C","Zr.sat (Boehnke)","TZr.sat.C (Boehnke)")
    if(nrow(y)>1) y<-formatResults(y) else rownames(y)<-rownames(cats)
    if(!getOption("gcd.shut.up"))print(y)
    assign("results",y,.GlobalEnv)
    invisible(y[,-2])
}    

# Calculates monazite saturation following Montel (1993) 
mzSaturation<-function(cats=milli,REE=filterOut(WR,c("La","Ce","Pr","Nd","Sm","Gd"),1),H2O=3,Xmz=0){ 
    on.exit(options("show.error.messages"=TRUE))
    if(H2O==0){
        H2O<-winDialogString("Water contents in the melt (%)","3")
        if(is.null(H2O)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
        H2O<-as.numeric(H2O)
    }
    
    if(Xmz==0){
        Xmz<-winDialogString("X mz","0.83")
        if(is.null(Xmz)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}    
    }
    Xmz<-as.numeric(Xmz)
    
    cats<-cats[rownames(REE),]
    MW.REE<-c(138.9055,140.12,140.9077,144.24,151.4,154.25)
    names(MW.REE)<-c("La","Ce","Pr","Nd","Sm","Gd")
    
    ree<-t(t(REE)/MW.REE)
    ree<-apply(ree,1,sum,na.rm=TRUE)
    reex<-ree/Xmz
    
    oxides<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","MnO","MgO","CaO","Na2O","K2O","P2O5") 
    x<-cats[,oxides]
    x<-normalize2total(x,100)
    
    D<-(x[,"Na2O"]+x[,"K2O"]+2*x[,"CaO"])/x[,"Al2O3"]*1/(x[,"Al2O3"]+x[,"SiO2"])*100

    T.calc<-13318/(9.5+2.34*D+0.3879*sqrt(H2O)-log(reex))-273.15
    
    y<-cbind(D,round(T.calc,3))
    colnames(y)<-c("Dmz","Tmz.sat.C")
    if(!getOption("gcd.shut.up"))print(y)
    y<-formatResults(y)
    assign("results",y,.GlobalEnv)
    invisible(y)
}


# Calculates apatite saturation following 
# HW: Harrison and Watson (1984)
# Bea: Bea et al. (1992)
# PV: Pichavant et al. (1992)
# WL: Wolf and London (1994)  

apSaturation<-function(Si=WR[,"SiO2"],ACNK=WR[,"A/CNK"],P2O5=WR[,"P2O5"],T=0){  
    on.exit(options("show.error.messages"=TRUE))
    ACNK[is.na(ACNK)]<-0
    ee<-cbind(Si,ACNK,P2O5)
    ee<-filterOut(ee,c("Si","ACNK","P2O5"),1)
    Si<-ee[,1]
    ACNK<-ee[,2]
    P2O5<-ee[,3]
    Si<-Si/100
    if(T==0){
        T<-winDialogString("Temperature (degrees C)","750")
        if(is.null(T)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    }
    T<-as.numeric(T)+273.15

    # Wolf and London (1994) at 750 degrees
    P2O5.WL<-NULL
    P2O5.WL[ACNK>1]<--3.4+3.1*ACNK[ACNK>1]
    
    # Harrison and Watson (1984)
    A<-8400+(Si-0.5)*26400
    B<-3.1+12.4*(Si-0.5) 
    D.HW<-exp(A/T-B)
    P2O5.HW<-42/D.HW
    T.HW<-A/(log(42/P2O5)+B)-273.15

    A<-8400+(Si-0.5)*26400
    B<-3.1+12.4*(Si-0.5) 
    D.HW<-exp(A/T-B)
    P2O5.HW<-42/D.HW
    T.HW<-A/(log(42/P2O5)+B)-273.15
      
    # A general routine that solves nonlinear equation for temperature (deg C) numerically 
    # (bisection method)
    solve.T<-function(fun,tmin=0,tmax=NULL){
        T.calc<-NULL
        if(.Platform$OS.type=="windows"){pb<-winProgressBar(title = "Iterating", label = "Processing sample",min = 1, max = length(Si), initial = 1, width = 300)}
        for(i in 1:length(Si)){
           if(.Platform$OS.type=="windows"){setWinProgressBar(pb, i, title = NULL, label = paste("Sample",i,"of",length(Si)))}
           if(ACNK[i]>1){
            ttold<-0
            tt<-1
            if(is.null(tmax)) tt.max<-T.HW[i]+273 else tt.max<-tmax 
                # H+W temperature is the only feasible maximum estimate
            tt.min<-tmin
            while(abs(ttold-tt)>0){
                ttold<-tt
                tt<-(tt.max-tt.min)/2+tt.min
                expr<-gsub("Si",Si[i],fun)
                expr<-gsub("ACNK",ACNK[i],expr)
                expr<-gsub("T",tt,expr)
                pp<-eval(parse(text=as.expression(expr)))
                if(pp>P2O5[i])tt.max<-tt else tt.min<-tt
            } 
                T.calc[i]<-tt
            }else{
                T.calc[i]<-NA
            }
        } 
        close(pb)
    return(T.calc-273.15)
    } 


# Pichavant et al. (1992)  
    P2O5.PV<-NULL
    C<--5900
    D<--3.22*Si+9.31  
    P2O5.PV[ACNK>1]<-P2O5.HW[ACNK>1]+(ACNK[ACNK>1]-1)*exp(C/T+D[ACNK>1])   
    
    T.PV<-solve.T("42/exp((8400+(Si-0.5)*26400)/T-3.1-12.4*(Si-0.5))+(ACNK-1)*exp(-5900/T-3.22*Si+9.31)")
   
# Bea et al. (1992)
    P2O5.Bea<-NULL
    E<-(ACNK-1)*6429
    P2O5.Bea[ACNK>1]<-P2O5.HW[ACNK>1]*exp(E[ACNK>1]/(T-273.15)) # CORRECTED K/C error
    T.Bea<-NULL 
    T.Bea<-solve.T("42*exp(((ACNK-1)*6429)/(T-273.15)-(8400+(Si-0.5)*26400)/T+3.1+12.4*(Si-0.5))")
    
    y<-cbind(T.HW,T.PV,T.Bea)
    colnames(y)<-c("Tap.sat.C.H&W","Tap.sat.C.Pich","Tap.sat.C.Bea")
    rownames(y)<-names(P2O5)
 
    if(is.null(P2O5.PV))P2O5.PV<-NA
    if(is.null(P2O5.Bea))P2O5.Bea<-NA 
    z<-cbind(ACNK,P2O5,P2O5.HW,P2O5.PV,P2O5.Bea)
    colnames(z)<-c("A/CNK","P2O5","P.H&W","P.Pich","P.Bea")
    if(!getOption("gcd.shut.up")){
        print(cbind(round(z,3),round(y,1)))
    }
    y<-cbind(z[,1],y)
    colnames(y)[1]<-"A/CNK"
    y<-formatResults(y)
    assign("results",y,.GlobalEnv)
    invisible(y)
 }
 
 # Calculates Rt saturation following Ryerson and Watson (1987) and Hayden and Watson (2007) 
rtSaturation<-function(cats=milli,T=0,P=0,Ti=filterOut(WR,"Ti",1)){ # Little tested so far
    on.exit(options("show.error.messages"=TRUE))
    if(T==0){
        T<-winDialogString("Temperature (degrees C)","750")
        if(is.null(T)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    }
    T<-as.numeric(T)+273.15
    
    if(P==0){
        P<-winDialogString("Pressure (kbar)","3")
        if(is.null(P)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    }
    P<-as.numeric(P)
        
    cats<-cats[rownames(Ti),c("SiO2","TiO2","Al2O3","FeOt","MnO","MgO","CaO","Na2O","K2O","P2O5")]

    cats<-normalize2total(cats)/100
    
    FM<-1/cats[,"SiO2"]*(cats[,"Na2O"]+cats[,"K2O"]+2*(cats[,"CaO"]+cats[,"MgO"]+cats[,"FeOt"]))/cats[,"Al2O3"]
    
    #Ryerson and Watson (1987) 
    DTiO2<-exp(-3.16+9373/T+0.026*P-0.152*FM)
    
    #Ti.sat.RW<-599342.9/DTi
    Ti.sat.RW<-599342.9/DTiO2
    TiO2<-Ti/0.59950*1e-4
    TRt.sat.C.RW<-9373/(3.16+log(100/TiO2)-0.026*P+0.152*FM)-273.15
    #TRt.sat.C.RW<-9373/(3.16+log(599342.9/Ti)-0.026*P+0.152*FM)-273.15
    aTi.RW<-Ti/Ti.sat.RW
    
    #Hayden and Watson (2007)
    Ti.sat.HW<-10^(7.95-5305/T+0.124*FM)
    
    TRt.sat.C.HW<-5305/(7.95-log10(Ti)+0.124*FM)-273.15
    aTi.HW<-Ti/Ti.sat.HW
    
    y<-cbind(FM,Ti,round(Ti.sat.RW,1),round(aTi.RW,2),round(TRt.sat.C.RW,1),round(Ti.sat.HW,1),round(aTi.HW,2),round(TRt.sat.C.HW,1))
    colnames(y)<-c("FM","Ti.obs","Ti.sat.RW","aTi.RW","TRt.sat.C.RW","Ti.sat.HW","aTi.HW","TRt.sat.C.HW")
    if(!getOption("gcd.shut.up"))print(y)
    y<-formatResults(y)
    results<<-y
    return(y[,-2])
}

.saturationMenu<-function(){
    on.exit(options("show.error.messages"=TRUE))
    #where<-c("Zircon saturation (Watson + Harrison 1983)","Monazite saturation (Montel 1993)","Apatite saturation")
    
    where<-c("Zircon saturation (Watson + Harrison 1983, Boehnke et al. 2013)","Monazite saturation (Montel 1993)","Apatite saturation","Rutile saturation (Ryerson + Watson 1987, Hayden + Watson 2007)")
    selected<-select.list(where, multiple = FALSE, title = "Select the accessory")
    if(selected==""){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    ee<-switch(which(where==selected),
        "zrSaturation()",
        "mzSaturation()",
        "apSaturation()",
        "rtSaturation()"
    )
    cat("GCDkit->",ee,"\n")
    .save2hist(ee)
    eval(parse(text=ee))
}    
    
if(getOption("gcd.menus")!=""){winMenuAddItem("Plugins","Saturation",".saturationMenu()")}
