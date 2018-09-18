
#####################################################################
#                      Niggli's values                              #
#                                                                   #
#####################################################################

Niggli<-function(WR,precision=getOption("gcd.digits")){

result.names<-c("si","al","fm","c","alk","k","mg","c/fm","ti","p","qz") 

results<-matrix(data = NA, ncol = length(result.names), nrow = nrow(WR), byrow = FALSE, dimnames = NULL) 
colnames(results)<-result.names 
rownames(results)<-rownames(WR) 
on.exit(options("show.error.messages"=TRUE))

######################################################################
#                              Main function                         #
######################################################################
Niggli.main<-function(x){
names(x)<-c("si","ti","al","fe3","fe2","mn","mg","ca","na","k","H2O","co2","p","fl","s")
x<-as.list(x)
attach(x,warn.conflicts=FALSE)
fm <- 2 * fe3 + fe2 + mn + mg
alk <- k + na

suma <- al + fm + alk + ca

w<-as.numeric(11)
w[1] <- si / suma * 100
w[2] <- al / suma * 100
cfm <- ca / fm
w[3] <- fm / suma * 100
w[4] <-ca / suma * 100
w[5] <- alk / suma * 100
w[6] <- k / (k + na)
w[7] <- mg / (fe2 + 2 * fe3 + mn + mg)
w[8] <- ti / suma * 100
w[9] <- p / suma * 100
w[10] <- cfm

if (w[2] > w[5]){
        qz <- w[1] - 100 - 4 * w[5]
    }
    else{
        qz <- w[1] - 100 - 2 * w[2] - w[5]
    }

w[11] <- qz
detach(x)
return(w)
}


######################################################################
#                              Entry point                           #
######################################################################

oxides<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","MnO","MgO","CaO","Na2O","K2O","H2O.PLUS","CO2","P2O5","F","S")   

try.it<-function(x){
    Niggli.main(x)
}
#cat("Processing.....\n")
#flush.console()

if(.Platform$GUI=="Rgui"){
    pb<-winProgressBar(title = "Niggli", label = "Processing sample",min = 1, max = nrow(WR), initial = 1, width = 300)
}else{
    if(!getOption("gcd.shut.up"))pb<-txtProgressBar(title = "Niggli", label = "Processing sample",min = 1, max = nrow(WR), initial = 1,char = "=",width = NA, style = 3)
}

for (fff in 1:nrow(WR)){
    if(.Platform$GUI=="Rgui"){
        setWinProgressBar(pb, fff, title = NULL, label = paste("Sample",fff,"of",nrow(WR)))
    }else{
        if(!getOption("gcd.shut.up"))setTxtProgressBar(pb, fff, title = NULL, label = paste("Sample",fff,"of",nrow(WR)))
    }
    dataset<-WR[fff,oxides]/MW[oxides]
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
    
    results[fff,]<-w
}

ee<-apply(results,1,sum)
results<-results[!is.na(ee),]

if(!getOption("gcd.shut.up"))print(t(round(results,precision)))

if(nrow(results)<nrow(WR)){
    cat("\nNot calculated: \n")
    print(rownames(WR)[is.na(match(rownames(WR),rownames(results)))])            
}
if(.Platform$GUI!="RTerm"|!getOption("gcd.shut.up"))close(pb)
return(results)
}






                    
