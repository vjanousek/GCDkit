# Miscellaneous
Misc<-function(WR){
oxides<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","MnO","MgO","CaO","Na2O","K2O","H2O.PLUS","CO2","P2O5","F","S")   
x<-WR[,oxides]     
colnames(x)<-c("si","ti","al","fe3","fe2","mn","mg","ca","na","k","H2O","co2","p","fl","s")
x<-data.frame(x)
attach(x,warn.conflicts=FALSE)

results<-matrix(nrow=nrow(x),ncol=8)

results[,1]<-WR[,"FeOt"]*1.11135
results[,2]<- na / k
results[,3]<- k / na
results[,4]<-na + k
results[,5]<- fe3 / fe2

# Larsen's differentiation index; Larsen 1938
results[,6] <- 1 / 3 * si + k - (ca + mg + fe2)

# Solidification index (Kuno et al. 1957)
results[,7] <- 100 * mg / (mg + fe3 + fe2 + na + k)

# Agpaitic index (Ussing 1912)
results[,8] <-(WR[,"Na2O"]/MW["Na2O"] + WR[,"K2O"]/MW["K2O"])/(WR[,"Al2O3"]/MW["Al2O3"])

colnames(results)<-c("Fe2O3t","Na2O/K2O","K2O/Na2O","Na2O+K2O","Fe2O3/FeO","Larsen's DI","Kuno's SI","Agpaitic Index")
rownames(results)<-rownames(x)

ee<-apply(results,1,function(i)all(is.na(i)))
results<-subset(results,!ee)

if(!getOption("gcd.shut.up"))print(t(round(results,getOption("gcd.digits"))))

if(nrow(results)<nrow(WR)){
    cat("\nNot calculated: \n")
    print(rownames(WR)[is.na(match(rownames(WR),rownames(results)))])            
}
detach(x) 
return(results)
}
