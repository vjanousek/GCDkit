 
#####################################################################
#             "Eine bessere" Mesonorm for granitoid rocks           #
#                       (Mielke & Winkler 1979)                     #
#####################################################################


Mesonorm<-function(WR,GUI=FALSE,precision=getOption("gcd.digits")){
    result.names<-c("Orthoclase","Albite","Anorthite","Quartz","Apatite","Magnetite","Hematite","Ilmenite","Biotite","Amphibole","Calcite","Corundum","Rest","sum")
    results<-matrix(data = NA, ncol = length(result.names), nrow = nrow(WR), byrow = FALSE, dimnames = NULL)
    on.exit(options("show.error.messages"=TRUE))
    
######################################################################
#                              Main function                         #
######################################################################
Mesonorm.main<-function(x){
names(x)<-c("si","ti","al","fe3","fe2","mn","mg","ca","na","k","H2O","co2","p")
x<-as.list(x)
attach(x,warn.conflicts=FALSE)

bi <- 0
bife<-0
biti<-0
bimg<-0
HBB<-0
HBA<-0

fe2<-fe2 + mn

cc<-2.27 * co2
ca<-ca - 1.27 * co2
ap<-2.36 * p
ca<-ca - 1.32 * p
H2O<-H2O - 0.04 * p


   if (fe2 >= 0.45 * fe3){
            mt <- 1.45 * fe3
            fe2 <- fe2 - .45 * fe3
            hm<-0
            }
        else
            {
            mt<-0
            hm<-fe3
            }

    TI1 <- .5 * ti
    ti2 <-(1 - .5) * ti

    il<-1.9 * TI1
    fe2<-fe2 - .9 * TI1

    FMG<-mg / (mg + fe2 + ti2)
    FFE<-fe2 / (mg + fe2 + ti2)
    FTI<-ti2 / (mg + fe2 + ti2)
    
    ab<-8.47 * na
    si<-si - 5.82 * na
    al<-al - 1.65 * na
    
    OR1<-5.91 * k
    si<-si - 3.83 * k
    al<-al - 1.08 * k
    
   if (ca >= .55 * al){
            anort1<- 2.73 * al
            si<-si - 1.18 * al
            ca<-ca - .55 * al
            al<-0
#label3:
            HBAMG<-14.51 * FMG * ca
            anort2<-anort1 - 4.96 * ca
            HBAFE<-16.76 * FFE * ca
            H2O<-H2O - .31 * ca
            HBATI<-14.49 * FTI * ca
            si<-si-5.36 * ca
            mg<-mg - 2.87 * FMG * ca
            fe2<-fe2 - 5.12 * FFE * ca
            ti<-ti2 - 2.85 * FTI * ca
            HBA<-HBAMG + HBAFE + HBATI

            }
            else{
            anort1<-4.96 * ca
            si<-si - 2.14 * ca
            al<-al - 1.82 * ca
            anort2<-anort1
            ti<-ti2
            }

#label2:
   if (OR1 >= (2.3 * mg + 1.29 * fe2 + 2.32 * ti)){
            bimg<- 3.45 * mg
            ort<-OR1 - (2.3 * mg + 1.29 * fe2 + 2.32 * ti)
            bife<-2.37 * fe2
            H2O<-H2O - (.15 * mg + .08 * fe2 + .15 * ti)
            biti<-3.47 * ti
            bi<-bife + bimg + biti
            anort<-anort2
            }
        else{   
            bimg<-1.5 * FMG * OR1
            mg<-mg - .43 * FMG * OR1
            bife<-1.84 - FFE * OR1
            fe2<-fe2 - .78 * FFE * OR1
            biti<-1.5 * FTI * OR1
            ti<-ti - .43 * FTI * OR1
            H2O<-H2O - .06 * OR1
            ort<-0
            bi<-bimg + bife + biti
            HBBMG<-5.05 * mg
            anort<-anort2 - (3.45 * mg + 1.94 * fe2 + 3.48 * ti)
            HBBFE<-3.28 * fe2
            si<-si - (1.12 * mg + .63 * fe2 + 1.13 * ti)
            HBBTI<-5.08 * ti
            H2O<-H2O - (.11 * mg + .06 * fe2 + .11 * ti)
            al<-al + (.63 * mg + .35 * fe2 + .64 * ti)
            HBB<-HBBMG + HBBFE + HBBTI
            }

#label4:
    HB<-HBB + HBA
    qtz<-si
    cor<-al
    rest<-H2O
w<-as.numeric(13)
w[1]<-ort
w[2]<-ab
w[3]<-anort
w[4]<-qtz
w[5]<-ap
w[6]<-mt
w[7]<-hm
w[8]<-il
w[9]<-bi
w[10]<-HB
w[11]<-cc
w[12]<-cor
w[13]<-rest

return(w)
}


######################################################################
#                              Entry point                           #
######################################################################


oxides<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","MnO","MgO","CaO","Na2O","K2O","H2O.PLUS","CO2","P2O5")   

try.it<-function(x){
    Mesonorm.main(x)
}
#cat("Processing.....\n")
#flush.console()

if(.Platform$GUI=="Rgui"){
    pb<-winProgressBar(title = "Granite Mesonorm", label = "Processing sample",min = 1, max = nrow(WR), initial = 1, width = 300)
}else{
    if(!getOption("gcd.shut.up"))pb<-txtProgressBar(title = "Granite Mesonorm", label = "Processing sample",min = 1, max = nrow(WR), initial = 1,char = "=",width = NA, style = 3)
}

for (fff in 1:nrow(WR)){
    if(.Platform$GUI=="Rgui"){
        setWinProgressBar(pb, fff, title = NULL, label = paste("Sample",fff,"of",nrow(WR)))
    }else{
        if(!getOption("gcd.shut.up"))setTxtProgressBar(pb, fff, title = NULL, label = paste("Sample",fff,"of",nrow(WR)))
    }

    dataset<-WR[fff,oxides]
    dataset[is.na(dataset)]<-0
    options(show.error.messages = FALSE)
    res<-try(try.it(dataset))
    options(show.error.messages = TRUE)
    
    if((class(res))!="numeric"){
        #cat("Sample",rownames(WR)[fff],"- ")
        err<-res[1]
        #cat("Error in calculation\n")
        #cat(err)
        w<-NA
  }else{
        w<-res
  }
    results[fff,]<-c(w,sum(w))
}
colnames(results)<-result.names
rownames(results)<-rownames(WR)

results<-results[!is.na(results[,"sum"]),]
ee<-apply(results==0,2,sum,na.rm=TRUE)
results<-results[,ee!=nrow(results)]
if(.Platform$GUI!="RTerm"|!getOption("gcd.shut.up"))close(pb)
if(!getOption("gcd.shut.up"))print(t(round(results,precision)))

if(nrow(results)<nrow(WR)){
    cat("\nNot calculated: \n")
    print(rownames(WR)[is.na(match(rownames(WR),rownames(results)))])            
}
if(GUI){
    plot.qanor<-winDialog(type="yesno","Plot Q'-ANOR diagram?") 
    if(plot.qanor=="YES") Streckeisen(results)
}
return(results)
}


                    
