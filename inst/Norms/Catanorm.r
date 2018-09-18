
#####################################################################
#           Barth-Niggli's molecular norm (Catanorm)                #
#                       (Hutchison, 1974)                           #
#####################################################################

Catanorm<-function(WR,precision=getOption("gcd.digits")){
    result.names<-c("Q","C","Or","Plag","   Ab","   An","Lc","Ne","Kp","Ac","Ns","Ks","Hy","Di","   Wo %","   En %","   Fs %","Ol","   Fo %","   Fa %","Cs","Mt","Hm","Il","Tn","Pf","Ru","Ap","Fr","Py","Cc","Sum")
    results<-matrix(data = NA, ncol = length(result.names), nrow = nrow(milli), byrow = FALSE, dimnames = NULL)
    colnames(results)<-result.names
    rownames(results)<-rownames(milli)
    on.exit(options("show.error.messages"=TRUE))

    ######################################################################
    #                  Main function                                      
    ######################################################################
    Catanorm.main<-function(x){
        names(x)<-c("si","ti","al","fe3","fe2","mn","mg","ca","na","k","co2","p","fl","s")
        x<-as.list(x)
        attach(x,warn.conflicts=FALSE)

        norm.names<-c("qtz","cor","ort","plg","ab","an","lc","ne","kp","ac","ns","ks","hy","di","wo","en","fs","ol","fo","fa","cs","mt","hm","il","tn","pf","ru","ap","fr","pr","cc","mgr")
        y<-rep(0,length(norm.names))
        names(y)<-norm.names
        y<-as.list(y)
        attach(y,warn.conflicts=FALSE)
        
    fe2 <- fe2 + mn
    y$cc <- 2 * co2
    ca <- ca - co2

if (p < 3 * fl){
    y$ap <- 3 * p; ca <- ca - 1.667 * p; fl <- fl - .33 * p
}else{
    y$ap <- 2.667 * p + fl; ca <- ca - 1.667 * p; fl <- 0
}

y$fr <- 1.5 * fl; ca <- ca - fl / 2; fl <- 0
y$pr <- 1.5 * s; fe2 <- fe2 - s / 2

if (ti <= fe2){
    y$il <- 2 * ti; fe2 <- fe2 - ti; ti <- 0
}else{
    y$il <- 2 * fe2; ti <- ti - fe2; fe2 <- 0
}

if (k <= al){
    y$ort <- 5 * k; al <- al - k; si <- si - 3 * k; k <- 0
}else{
    y$ort <- 5 * al; k <- k - al; si <- si - 3 * al; al <- 0
}

y$ks <- 1.5 * k; si <- si - .5 * k; k <- 0

if (na <= al){
    y$ab <- 5 * na; al <- al - na; si <- si - 3 * na; na <- 0
}else{
    y$ab <- 5 * al; na <- na - al; si <- si - 3 * al; al <- 0
}

if (na <= fe3){
    y$ac <- 4 * na; fe3 <- fe3 - na; si <- si - 2 * na; na <- 0
}else{
    y$ac <- 4 * fe3; si <- si - 2 * fe3; fe3 <- 0
}

y$ns <- 1.5 * na; si <- si - .5 * na; na <- 0

if (ca <= .5 * al){
    y$an <- 5 * ca; al <- al - 2 * ca; si <- si - 2 * ca; ca <- 0
}else{
    y$an <- 2.5 * al; ca <- ca - .5 * al; si <- si - al; al <- 0
}

if (ti <= ca){
    y$tn <- 3 * ti; ca <- ca - ti; si <- si - ti; ti <- 0
}else{
    y$tn <- 3 * ca; ti <- ti - ca; si <- si - ca; ca <- 0
}

y$ru <- ti; ti <- 0
y$cor <- al; al <- 0

if (fe3 <= 2 * fe2){
    y$mt <- 1.5 * fe3; fe2 <- fe2 - fe3 / 2; fe3 <- 0
}else{
    y$mt <- 3 * fe2; fe3 <- fe3 - 2 * fe2; fe2 <- 0
}

y$hm <- fe3; fe3 <- 0
y$wo <- 2 * ca; si <- si - ca; ca <- 0
y$en <- 2 * mg; si <- si - mg; mg <- 0
y$fs <- 2 * fe2; si <- si - fe2; fe2 <- 0
y$hy <- y$en + y$fs
y$mgr <- 100 * y$en / (y$en + y$fs)

if (y$hy < y$wo){
    y$di <- 2 * y$hy; y$wo <- y$wo - y$hy; y$hy <- 0
}else{
    y$di <- 2 * y$wo; y$hy <- y$hy - y$wo; y$wo <- 0
}

if (si >= 0){    y$qtz <- si
    w<-Ende(y);detach(x,y)
    return(w)
}
y$qtz <- -si

if (y$hy >= 4 * y$qtz){
    y$ol <- 3 * y$qtz; y$hy <- y$hy - 4 * y$qtz; y$qtz <- 0
    w<-Ende(y);detach(x,y)
    return(w)
}else{
    y$ol <- 3 / 4 * y$hy; y$qtz <- y$qtz - 1 / 4 * y$hy; y$hy <- 0
}

if (y$tn >= 3 * y$qtz){
    y$pf <- 2 * y$qtz; y$tn <- y$tn - 3 * y$qtz; y$qtz <- 0
    w<-Ende(y);detach(x,y)
    return(w)
}else{
    y$pf <- .667 * y$tn; y$qtz <- y$qtz - 1 / 3 * y$tn; y$tn <- 0
}

if (y$ab >= 2.5 * y$qtz){
    y$ne <- 1.5 * y$qtz; y$ab <- y$ab - 2.5 * y$qtz; y$qtz <- 0
    w<-Ende(y);detach(x,y)
    return(w)
}else{
    y$ne <- .6 * y$ab; y$qtz <- y$qtz - .4 * y$ab; y$ab <- 0
}

if (y$ort >= 5 * y$qtz){
    y$lc <- 4 * y$qtz; y$ort <- y$ort - 5 * y$qtz; y$qtz <- 0
    w<-Ende(y);detach(x,y)
    return(w)
}else{
    y$lc <- .8 * y$ort; y$qtz <- y$qtz - .2 * y$ort; y$ort <- 0
}

if (y$lc >= 4 * y$qtz){
    y$p <- 3 * y$qtz; y$lc <- y$lc - 4 * y$qtz; y$qtz <- 0
    w<-Ende(y);detach(x,y)
    return(w)
}else{
    y$kp <- .75 * y$lc; y$qtz <- y$qtz - .25 * y$lc; y$lc <- 0
}

if (y$wo >= 4 * y$qtz){
    y$cs <- 3 * y$qtz; y$wo <- y$wo - 4 * y$qtz; y$qtz <- 0
    w<-Ende(y);detach(x,y)
    return(w)

}else{
    y$cs <- .75 * y$wo; y$qtz <- y$qtz - .25 * y$wo; y$wo <- 0
}

if (y$di >= 4 * y$qtz){
    y$cs <- y$cs + 1.5 * y$qtz; y$ol <- y$ol + 1.5 * y$qtz; y$di <- y$di - 4 * y$qtz; y$qtz <- 0
    w<-Ende(y);detach(x,y)
    return(w)
}else{
    y$cs <- y$cs + .375 * y$di; y$ol <- y$ol + .375 * y$di; y$qtz <- y$qtz - .25 * y$di; y$di <- 0; y$qtz <- 0
    w<-Ende(y);detach(x,y)
    return(w)
}
}

Ende<-function(y){
attach(y,warn.conflicts=FALSE)
if(y$di>0){
    y$en <- y$mgr/2
    y$fs <- 50 - y$en
    y$wo<-50.00
}else{
    y$en <- 0
    y$fs <- 0
    y$wo<-0
}
if(y$ol>0){
    y$fo <- y$mgr
    y$fa <- 100 - y$fo
}else{
    y$fo<-0
    y$fa<-0
}

y$plg<-y$ab+y$an

w<-unlist(y)
w<-w[1:31]
suma<-sum(w[-c(5,6,15,16,17,19,20)])
w[32]<-suma
return(w)
}


######################################################################
#                   Entry point                                       
######################################################################
oxides<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","MnO","MgO","CaO","Na2O","K2O","CO2","P2O5","F","S")   


try.it<-function(x){
    Catanorm.main(x)
}
#cat("Processing.....\n")
#flush.console()

if(.Platform$GUI=="Rgui"){
    pb<-winProgressBar(title = "Catanorm", label = "Processing sample",min = 1, max = nrow(WR), initial = 1, width = 300)
}else{
    if(!getOption("gcd.shut.up"))pb<-txtProgressBar(title = "Catanorm", label = "Processing sample",min = 1, max = nrow(WR), initial = 1,char = "=",width = NA, style = 3)
}

for (fff in 1:nrow(WR)){
    if(.Platform$GUI=="Rgui"){
        setWinProgressBar(pb, fff, title = NULL, label = paste("Sample",fff,"of",nrow(WR)))
    }else{
        if(!getOption("gcd.shut.up"))setTxtProgressBar(pb, fff, title = NULL, label = paste("Sample",fff,"of",nrow(WR)))
    }
    
    suma<-sum(milli[fff,oxides],na.rm=TRUE)
    suma<-100/suma
    dataset<-milli[fff,oxides]*suma
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

results<-results[results[,"Sum"]!=0,]
ee<-apply(results==0,2,sum,na.rm=TRUE)
results<-results[,ee!=nrow(results)]

if(!getOption("gcd.shut.up"))print(t(round(results,precision)))

if(nrow(results)<nrow(WR)){
    cat("\nNot calculated: \n")
    print(rownames(WR)[is.na(match(rownames(WR),rownames(results)))])            
}
if(.Platform$GUI!="RTerm"|!getOption("gcd.shut.up"))close(pb)
return(results)
}
