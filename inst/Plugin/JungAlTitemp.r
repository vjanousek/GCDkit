#####################################################################
#             Al2O/TiO2 thermometer for granitic rocks              #
#                      Jung & Pfänder 2007                          #
#####################################################################

Jung<-function(model = NULL, plot = TRUE){
    on.exit(options("show.error.messages"=TRUE))
    ee<-c("Pelite melting","Psammite melting","Metaigneous rock melting","A-type granite melting","Amphibolite melting (Rapp & Watson, 1995)","Amphibolite melting (Patino Douce & Beard, 1995)")
    if(is.null(model)){    
        model<-select.list(ee, preselect = NULL, multiple = FALSE, title = "Select the melting mechanism")
        if(model==""){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    }else{
        model<-gsub("[ ]",".*",model)
        x<-grep(model,ee,value=FALSE,ignore.case=TRUE) #extended=TRUE,
        if(length(x)==0){
            cat("No matching normalizing scheme found.\n")
            options(show.error.messages=FALSE);stop()
        }
        if(length(x)>1){
            cat("Ambigious, several matching normalizing schemes found.\n")
            print(ee[x])
            options(show.error.messages=FALSE);stop()
        }
        model<-ee[x]
    }
    
    # Power Law
    switch(which(ee==model),
        {A1 <- 2.14e23; B1 <- 7.294}, #R2 = 0.85 for pelite melting
        {A1 <- 6.48e18; B1 <- 5.853}, #R2 = 0.91 for psammite melting
        {A1 <- 6.48e18; B1 <- 5.853}, #R2 = 0.91 for igneous rock melting
        {A1 <- 2.98e31; B1 <- 9.921}, #R2 = 0.76 for A-type granite melting; B1 CORRECTED
        {A1 <- 2.82e30; B1 <- 9.677}, #R2 = 0.80 for amphibolite melting (Rapp & Watson, 1995); A1 CORRECTED
        {A1 <- 1.81e27; B1 <- 8.689}  #R2 = 0.75 for amphibolite melting (Patino Douce & Beard, 1995)
    )

    T1<-(A1/(WR[,"Al2O3"]/WR[,"TiO2"]))^(1/B1)
    
    # Exponential Law
     switch(which(ee==model),
        {A2 <- 93183;  B2 <- 0.00813}, #R2 = 0.86 for pelite melting
        {A2 <- 23400;  B2 <- 0.00729}, #R2 = 0.92 for psammite melting
        {A2 <- 23400;  B2 <- 0.00729}, #R2 = 0.91 for igneous rock melting
        {A2 <- 14018;  B2 <- 0.01020}, #R2 = 0.75 for A-type granite melting
        {A2 <- 435247; B2 <- 0.00969}, #R2 = 0.80 for amphibolite melting (Rapp & Watson, 1995)
        {A2 <- 171928; B2 <- 0.00934}  #R2 = 0.75 for amphibolite melting (Patino & Beard, 1995)
    )
    
    T2<-(log(A2)-log(WR[,"Al2O3"]/WR[,"TiO2"]))/B2
    
     # Linear fit
     switch(which(ee==model),
        {A3<- 414134; B3 <- 391}, #R2 = 0.84 for pelite melting
        {A3<- 380090; B3 <- 388}, #R2 = 0.90 for psammite melting
        {A3<- 309901; B3 <- 309}, #R2 = 0.89 for igneous rock melting
        {A3<- 867604; B3 <- 809}, #R2 = 0.75 for A-type granite melting
        {A3<- 266664; B3 <- 233}, #R2 = 0.88 for amphibolite melting (Rapp & Watson, 1995)
        {A3<- 211213; B3 <- 197} #R2 = 0.74 for amphibolite melting (Patino Douce & Beard, 1995)
    )
    T3<-A3/((WR[,"Al2O3"]/WR[,"TiO2"]) + B3)

    z<-cbind(WR[,"Al2O3"]/WR[,"TiO2"],T1,T2,T3,apply(cbind(T1,T2,T3),1,mean))
    z<-round(z,1)
    colnames(z)<-c("Al2O3/TiO2","T_Al/Ti.power.C","T_Al/Ti.exp.C","T_Al/Ti.linear.C","T_Al/Ti.mean.C") 
    results<<-z
    
    if(plot){
        windows()
        old.par<-par()
        par(mar=c(6,5,10,5))
        plot(WR[,"Al2O3"]/WR[,"TiO2"],WR[,"CaO"]/WR[,"Na2O"],xlab=annotate("Al2O3/TiO2"),ylab=annotate("CaO/Na2O"),cex=labels$Size,pch=labels$Symbol,col=labels$Colour,log="xy",ylim=c(0.1,5))
        i<-list()
        at<-list()        
        Temp<-rev(seq(500,1200,20))
        
        at[[1]]<-A1*Temp^-B1 # 'Power'
        i[[1]]<-at[[1]]<=10^par("usr")[2]&at[[1]]>=10^par("usr")[1]
        
        at[[2]]<-exp(log(A2)-Temp*B2) # 'Exponential'
        i[[2]]<-at[[2]]<=10^par("usr")[2]&at[[2]]>=10^par("usr")[1]

        at[[3]]<-A3/Temp-B3 # 'Linear'
        i[[3]]<-at[[3]]<=10^par("usr")[2]&at[[3]]>=10^par("usr")[1]
        
        par(xpd=TRUE)
        step<-diff(par("usr")[3:4])/10 
        ee<-par("usr")[4]+(0:2)*step+step/2 # y-coordinates for the new x axes
        segments(10^par("usr")[1],10^ee,10^par("usr")[2],10^ee) # draw them
        text(10^par("usr")[1],10^ee,c("Linear ","Exponential ","Power" ),adj=c(1,0.5))
        
        j<-lapply(1:3,function(n){
            segments(at[[n]][i[[n]]],10^ee[n],at[[n]][i[[n]]],10^(ee[n]+step/7)) # ticks 
            label<-rev(pretty(Temp[i[[n]]])) # get the labels ready
            k<-which(Temp[i[[n]]]%in%label)
            #text(at[[n]][i[[n]]][k],ee[n]+diff(par("usr")[1:2])/10,Temp[i[[n]]][k])
            text(at[[n]][i[[n]]][k],10^(ee[n]+step/2),Temp[i[[n]]][k])
            segments(at[[n]][i[[n]]][k],10^ee[n],at[[n]][i[[n]]][k],10^(ee[n]+step/4))
        })
        mtext(expression(paste("Temperature (",degree,"C) after Jung and Pfander (2007)")),side=3,line=7)
        mtext(model,side=1,line=4)
        par(old.par)
    }

    return(results)
}

if(getOption("gcd.menus")!=""){winMenuAddItem("Plugins","Al-Ti thermometer (Jung + Pfander 2007)...","Jung()")}
