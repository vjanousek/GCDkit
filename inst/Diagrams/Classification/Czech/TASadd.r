TASadd<-function(x,cutoff=TRUE){
     xx<-winDialog(type="yesno","Employ detailed subdivisions?\n (trachybasalt, b. trachyandesite, trachyandesite and high-Mg rocks)")
     yy<-winDialog(type="yesno","Exclude analyses with CO2 > 0.5 and H2O > 2.0 ?")
     for (f in seq(1,nrow(WR))){
         if (!(is.na(WRanh[f,"MgO"])) & !(is.na(WRanh[f,"Na2O"])) & !(is.na(WRanh[f,"K2O"])) & !(is.na(WRanh[f,"SiO2"])) & !(is.na(WRanh[f,"TiO2"]))){
             if (xx=="YES") {
                 if (x[f]=="trachybasalt") if (WRanh[f,"Na2O"]-2<WRanh[f,"K2O"]) (x[f]<-"K-rich trachybasalt") else (x[f]<-"hawaiite")
                 if (x[f]=="basaltic trachyandesite") if (WRanh[f,"Na2O"]-2<WRanh[f,"K2O"]) x[f]<-"shoshonite" else x[f]<-"mugearite"
                 if (x[f]=="trachyandesite") if (WRanh[f,"Na2O"]-2<WRanh[f,"K2O"]) x[f]<-"latite" else x[f]<-"benmoreite"
             }
              # conditions following LeBas (2000)
             if ((WRanh[f,"SiO2"] >= 52) & (WRanh[f,"MgO"] > 8) & (WRanh[f,"TiO2"] < 0.5)) x[f]<-"boninite"
             if (WRanh[f,"MgO"] >12){
                 if ((WRanh[f,"SiO2"]<52) & (WRanh[f,"SiO2"]>30)){
                     if ((WRanh[f,"Na2O"]+WRanh[f,"K2O"] < 3) & (WRanh[f,"Na2O"]+WRanh[f,"K2O"] < 2)) x[f]<-"picrite"
                     if ((WRanh[f,"Na2O"]+WRanh[f,"K2O"] < 2) & (WRanh[f,"TiO2"] < 1)) x[f]<-"komatiite"
                     if ((WRanh[f,"Na2O"]+WRanh[f,"K2O"] < 2) & (WRanh[f,"TiO2"] >= 1)) x[f]<-"meimechite"
                 }
             }
         }
         if (yy=="YES"){
             if (any(colnames(WR)=="CO2")) if (!is.na(WR[f,"CO2"])) if (WR[f,"CO2"]>0.5) x[f]<-"CO2 too high"
             if (any(colnames(WR)=="H2O.PLUS")) if (!is.na(WR[f,"H2O.PLUS"])) if (WR[f,"H2O.PLUS"]>2) if (x[f]=="CO2 too high") x[f]<-"CO2 & H2O too high" else x[f]<-"H2O too high"
         }
    }
return(x)
}
