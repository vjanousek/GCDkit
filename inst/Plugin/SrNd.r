if(any(colnames(WR)=="87Sr/86Sr")|(any(colnames(WR)=="143Nd/144Nd"))){
options(show.error.messages=FALSE)
ee<-try(WR<<-addOn("87Sr/86Sr"))
ee<-try(WR<<-addOn("143Nd/144Nd"))

ee<-try(addOn("87Rb/86Sr",WR[,"Rb"]/WR[,"Sr"]*(2.6939+0.2832*WR[,"87Sr/86Sr"])))
if(class(ee)=="try-error") {
        ee<-try(addOn("87Rb/86Sr",WR[,"Rb.y"]/WR[,"Sr.y"]*(2.6939+0.2832*WR[,"87Sr/86Sr"])))
        if(class(ee)=="try-error") ee<-try(addOn("87Rb/86Sr"))
}
WR<<-ee

ee<-try(addOn("147Sm/144Nd",WR[,"Sm"]/WR[,"Nd"]*(0.53151+0.14252*WR[,"143Nd/144Nd"])))
if(class(ee)=="try-error"){ 
    ee<-try(addOn("147Sm/144Nd",WR[,"Sm.y"]/WR[,"Nd.y"]*(0.53151+0.14252*WR[,"143Nd/144Nd"])))
    if(class(ee)=="try-error") ee<-try(addOn("147Sm/144Nd"))
}
WR<<-ee
options(show.error.messages=TRUE)

# Constants ##########################
lambda<-c(1.42*10^-11,6.54*10^-12)
names(lambda)<-c("Sr","Nd")
e<-2.718281828

# CHUR - present-day Nd isotopic composition
CHUR<-c(0.1967,0.512638)
names(CHUR)<-c("147Sm/144Nd","143Nd/144Nd")
CHUR<-t(as.matrix(CHUR))
rownames(CHUR)<-"CHUR Jacobsen and Wasserburg 1980"

# UR - present-day Sr isotopic composition

UR<-c(0.0816,0.7045)
names(UR)<-c("87Rb/86Sr","87Sr/86Sr")
UR<-t(as.matrix(UR))
rownames(UR)<-"UR Faure 1986"

#Liew and Hofmann
DM<-c(0.219,0.513151)
names(DM)<-c("147Sm/144Nd","143Nd/144Nd")
DM<-t(as.matrix(DM))
rownames(DM)<-"DM Liew and Hofmann 1988"

# Important functions #
########################################################################

# Calculates Sr or Nd initials ****** 
initial<-function(x,age,system="Nd"){
    if(system=="Nd"){
        R<-x[,"147Sm/144Nd"]
        I<-x[,"143Nd/144Nd"]
    }
    if(system=="Sr"){
         R<-x[,"87Rb/86Sr"]
         I<-x[,"87Sr/86Sr"]
    }    
    y<-I-(R*(e^(lambda[system]*age*10^6)-1))
    age[is.na(age)]<--1
    y[age==0]<-I
    age[age==-1]<-NA
    return(y)
}

# Calculates initial Eps Nd values ****** 
epsilon<-function(WR,age){
    X<-round((initial(WR,age)/initial(CHUR,age)-1)*10^4,2)
    return(X)
}

# Calculates single-stage Nd model ages ****** 
DMage<-function(WR){
    R<-WR[,"147Sm/144Nd"]
    I<-WR[,"143Nd/144Nd"]
    X<-round(1/lambda["Nd"]*log(((I-DM[,"143Nd/144Nd"])/(R-DM[,"147Sm/144Nd"]))+1)/10^9,3)
    X
}

# Calculates TNd DM model ages after Goldstein et al. ****** 
DMGage<-function(WR){
    R<-WR[,"147Sm/144Nd"]
    I<-WR[,"143Nd/144Nd"]
    aa<--1/.456
    bb<-10
    Q<--CHUR[,"147Sm/144Nd"]*lambda["Nd"]/CHUR[,"143Nd/144Nd"]*1e4*1e9
    cc<-Q*(R-CHUR[,"147Sm/144Nd"])/CHUR[,"147Sm/144Nd"]
    dd<-(I/CHUR[,"143Nd/144Nd"]-1)*1e4
    X<-round((bb-dd)/(cc-aa),3)
    X
}

# Calculates TNd DM model ages after Liew and Hofmann (1988) ****** 
DMLHage<-function(WR,age,RCC=0.12){
    R<-WR[,"147Sm/144Nd"]
    I<-WR[,"143Nd/144Nd"]
    citatel<-I-(exp(lambda["Nd"]*age*1e6)-1)*(R-RCC)-DM[,"143Nd/144Nd"]
    jmenovatel<-RCC-DM[,"147Sm/144Nd"]
    X<-round(1/lambda["Nd"]*log(citatel/jmenovatel+1)/10^9,3)
    X
}


srnd<-function(age=NULL){
    on.exit(options("show.error.messages"=TRUE))
    if(!any(toupper(colnames(WR))=="AGE")){
        if(is.null(age)){
            age<-winDialogString("Age (Ma)",as.character(340))
            if(is.null(age)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
            age<-as.numeric(age)
            age<-rep(age,times=nrow(WR))
        }
    }else{
        age<-WR[,toupper(colnames(WR))=="AGE"]
    }        
    #}else{
    #    age<-rep(age,times=nrow(WR))
    #}  
    
    init<-initial(WR,age,"Sr")
    ndi<-initial(WR,age,"Nd")
    eps<-epsilon(WR,age)
    TDM<-DMage(WR)
    TDMG<-DMGage(WR)
    TDMLH<-DMLHage(WR,age)
    
    #if(all(age==0){
    #}else{
        init<-cbind(age,round(init,6),round(ndi,6),eps,TDM,TDMG,TDMLH)
    #}
    rownames(init)<-rownames(WR)
    colnames(init)<-c("Age (Ma)","87Sr/86Sri","143Nd/144Ndi","EpsNdi","TDM","TDM.Gold","TDM.2stg")
    
    ee<-filterOut(init,colnames(init),n=6)
    if(!getOption("gcd.shut.up")){cat("\n");print(ee)}
    # Set up labels for plotting with correct formatting
    ii<-unique(age)
    ii<-ii[!is.na(ii)]
    
    if(length(ii)==1){
        #b<-paste("epsilon*Nd[",ii,"]",sep="")
        b<-paste("epsilon[",ii,"]^Nd",sep="")
        b<-as.expression(b)
        epsNdlabi<<-parse(text=b)
        
        b<-paste("\" \"^87*Sr/\" \"^86*Sr[",ii,"]",sep="")
        b<-as.expression(b)
        srlab<<-parse(text=b)

        b<-paste("\" \"^143*Nd/\" \"^144*Nd[",ii,"]",sep="")
        b<-as.expression(b)
        ndlab<<-parse(text=b)
    }else{
        #epsNdlabi<<-expression(epsilon*Nd[i])
        epsNdlabi<<-expression(epsilon[i]^Nd)
        srlab<<-expression(" "^87*Sr/" "^86*Sr[i])
        ndlab<<-expression(" "^143*Nd/" "^144*Nd[i])
    }
    epsNdlab<<-expression(epsilon[Nd])

    izochron.Ndlab.x<<-expression(" "^{147}*Sm/" "^{144}*Nd)
    izochron.Ndlab.y<<-expression(" "^{143}*Nd/" "^{144}*Nd)
    izochron.Srlab.x<<-expression(" "^87*Rb/" "^86*Sr)
    izochron.Srlab.y<<-expression(" "^87*Sr/" "^86*Sr)

    assign("age",age,.GlobalEnv)
    assign("results",init,.GlobalEnv)
    return(init)
}

init<-srnd()

# Age vs EpsNdi ######################################################################
ageEps<-function(GUI=FALSE,...){    
    on.exit(options("show.error.messages"=TRUE))
    
    if(GUI){
        ee<-select.list(c("Rb-Sr","Sm-Nd single stage","Sm-Nd two stage"),preselect="Sm-Nd two stage",multiple=FALSE)
        if(nchar(ee)==0){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
        if(ee=="Rb-Sr"){
            ee<-ageSr(GUI=TRUE,...);return()
        }
    
        if(ee=="Sm-Nd two stage"){
            ee<-ageEps2(GUI=TRUE,...);return()
        }
    }
    
    eps0<-(WR[,"143Nd/144Nd"]/CHUR[,"143Nd/144Nd"]-1)*1e4
    x<-age/1000
    names(x)<-rownames(init)
    plotWithLimits(x,init[,"EpsNdi"],xmin=0,ymax=10,ymin=.round.min.down(eps0,1),xmax=.round.max.up(init[,"TDM"],1),xlab="Age (Ga)",ylab=epsNdlab,col=labels$Colour,pch=labels$Symbol,title="Nd isotopic growth diagram (single-stage)",...)
    
    sheet$demo$template$abline1<-list("abline",h=0,col="darkgrey",lwd=3)

    Q<--CHUR[,"147Sm/144Nd"]*lambda["Nd"]/CHUR[,"143Nd/144Nd"]*1e4*1e9
    
    # DM: Liew and Hofmann
    sheet$demo$template$abline2<-list("abline",a=(DM[,"143Nd/144Nd"]/CHUR[,"143Nd/144Nd"]-1)*1e4,b=Q*(DM[,"147Sm/144Nd"]-CHUR[,"147Sm/144Nd"])/CHUR[,"147Sm/144Nd"],col="darkgrey",lwd=3)
    
    # DM: Goldstein et al.
    sheet$demo$template$abline3<-list("abline",a=10,b=-1/.456,col="gray",lwd=3)
    
    xmax<-sheet$demo$call$xlim[2]
    sheet$demo$template$text1<-list("text",x=xmax-xmax/20,y=0.5,text="CHUR",size=1.5)
    sheet$demo$template$text2<-list("text",x=xmax-xmax/20,y=10-1/.456*xmax-1,text="DM",size=1.5)

    # Nd isotopic development lines for our samples
    temp.lines<-list()
    for (i in 1:nrow(WR)){
            if(!is.na(WR[i,"143Nd/144Nd"])){
                x<-c(0,init[i,"TDM"])
                y<-c(eps0[i],epsilon(DM,init[i,"TDM"]*10^3))
                ee<-eval(parse(text=paste("temp.lines$lines",i,"<-list(\"lines\",x=c(", paste(x,collapse=","),"),y=c(", paste(y,collapse=","),"),col= \"",labels$Colour[i],"\",lty=\"dashed\")",sep="")))
            }           
    }
    
    sheet$demo$call$col<-labels[names(x.data),"Colour"]
    sheet$demo$call$pch<-labels[names(x.data),"Symbol"]
    sheet$demo$call$cex<-labels[names(x.data),"Size"]
    sheet$demo$template<-c(sheet$demo$template,temp.lines)
    sheet$demo$template$rug1<-list("rug",x=init[,"TDM"],ticksize=0.03,side=1,lwd=1,col="darkred")
    sheet$demo$template$rug2<-list("rug",x=init[,"EpsNdi"],ticksize=0.03,side=2,lwd=1,col="darkred")

    assign("sheet",sheet,pos=parent.env(environment()))
    pp<<-figaro(demo,prefix="sheet")
    figRedraw()
    if(screen()) .saveCurPlotDef(screen()) else figaroOn()
    #if(!screen()) figaroOn()
    invisible()
}


# Age vs EpsNdi 2 stg ######################################################################
ageEps2<-function(GUI=FALSE,...){
    eps0<<-(WR[,"143Nd/144Nd"]/CHUR[,"143Nd/144Nd"]-1)*1e4
    x<-age/1000
    names(x)<-rownames(init)
    plotWithLimits(x,init[,"EpsNdi"],xmin=0,ymax=10,ymin=.round.min.down(c(eps0,init[,"EpsNdi"]),2),xmax=.round.max.up(init[,"TDM.2stg"],1),xlab="Age (Ga)",ylab=epsNdlab,col=labels$Colour,pch=labels$Symbol,title="Nd isotopic growth diagram (two-stage)",...)
    
    sheet$demo$template$abline1<-list("abline",h=0,col="darkgrey",lwd=3)
    
    Q<--CHUR[,"147Sm/144Nd"]*lambda["Nd"]/CHUR[,"143Nd/144Nd"]*1e4*1e9
    
    # DM: Liew and Hofmann
    sheet$demo$template$abline2<-list("abline",a=(DM[,"143Nd/144Nd"]/CHUR[,"143Nd/144Nd"]-1)*1e4,b=Q*(DM[,"147Sm/144Nd"]-CHUR[,"147Sm/144Nd"])/CHUR[,"147Sm/144Nd"],col="darkgrey",lwd=3)
   
    # DM: Goldstein et al.
    sheet$demo$template$abline3<-list("abline",a=10,b=-1/.456,col="gray",lwd=3)
    
    xmax<-sheet$demo$call$xlim[2]
    sheet$demo$template$text1<-list("text",x=xmax-xmax/20,y=0.5,text="CHUR",size=1.5)
    sheet$demo$template$text2<-list("text",x=xmax-xmax/20,y=10-1/.456*xmax-1,text="DM",size=1.5)

    # Nd isotopic development lines for our samples
    temp.lines<-list()
    for (i in 1:nrow(WR)){
            if(!is.na(WR[i,"143Nd/144Nd"])){
                x<-c(0,age[i]/1e3,init[i,"TDM.2stg"])
                y<-c(eps0[i],init[i,"EpsNdi"],epsilon(DM,init[i,"TDM.2stg"]*10^3))
                ee<-eval(parse(text=paste("temp.lines$lines",i,"<-list(\"lines\",x=c(", paste(x,collapse=","),"),y=c(", paste(y,collapse=","),"),col= \"",labels$Colour[i],"\",lty=\"solid\")",sep="")))
             }
    }
    col<-labels[names(x.data),"Colour"]
    sheet$demo$call$col<-col
    sheet$demo$call$pch<-labels[names(x.data),"Symbol"]
    sheet$demo$call$cex<-labels[names(x.data),"Size"]
    sheet$demo$template<-c(sheet$demo$template,temp.lines)
    #sheet$demo$template$rug1<-list("rug",x=init[,"TDM.2stg"],ticksize=0.03,side=1,lwd=1,col=col) # Does not work
    #sheet$demo$template$rug2<-list("rug",x=init[,"EpsNdi"],ticksize=0.03,side=2,lwd=1,col=col)   # Does not work
    sheet$demo$template$rug1<-list("rug",x=init[,"TDM.2stg"],ticksize=0.03,side=1,lwd=1,col="darkred")
    sheet$demo$template$rug2<-list("rug",x=init[,"EpsNdi"],ticksize=0.03,side=2,lwd=1,col="darkred")
    
    assign("sheet",sheet,pos=parent.env(environment()))
    pp<<-figaro(demo,prefix="sheet")
    figRedraw()
    if(screen()) .saveCurPlotDef(screen()) else figaroOn()
    #if(!screen()) figaroOn()
    invisible()
}

# Age vs Sr/Sri ######################################################################
ageSr<-function(GUI=FALSE,...){    
    x<-age/1000
    names(x)<-rownames(init)
    xmin<-0
    xmax<-1 # xmax is in Ga 
    plotWithLimits(x,initial(WR,age,"Sr"),xmin=xmin,ymin=0.700,ymax=.round.max.up(WR[,"87Sr/86Sr"],4),xmax=xmax,xlab="Age (Ga)",ylab=izochron.Srlab.y,col=labels$Colour,pch=labels$Symbol,title="Sr isotopic growth diagram (single-stage)",...)
    temp.lines<-list()
    which<-1

    for (i in rownames(init)){
            if(!is.na(WR[i,"87Sr/86Sr"])){
                x<-c(xmin,xmax)
                y<-c(WR[i,"87Sr/86Sr"]-WR[i,"87Rb/86Sr"]*(exp(lambda["Sr"]*xmin*1e9)-1),WR[i,"87Sr/86Sr"]-WR[i,"87Rb/86Sr"]*(exp(lambda["Sr"]*xmax*1e9)-1))
                ee<-eval(parse(text=paste("temp.lines$lines",which,"<-list(\"lines\",x=c(", paste(x,collapse=","),"),y=c(", paste(y,collapse=","),"),col= \"",labels[i,"Colour"],"\",lty=\"dashed\")",sep="")))
                which<-which+1
                #lines(x,y,col=labels[i,"Colour"],lty="solid")
             }
    }
    
    sheet$demo$template<-c(sheet$demo$template,temp.lines)
    # UR
    #polygon(c(0,xmax,xmax,0),c(0,0,UR[,"87Sr/86Sr"]-UR[,"87Rb/86Sr"]*(exp(lambda["Sr"]*xmax*1e9)-1),UR[,"87Sr/86Sr"]),col="white")
    
    ur<-UR[,"87Sr/86Sr"]-UR[,"87Rb/86Sr"]*(exp(lambda["Sr"]*5*1e9)-1)
    sheet$demo$template$lines0<-list("lines",x=c(0,5),y=c(UR[,"87Sr/86Sr"],ur),col="darkgrey",lwd=3)
    sheet$demo$template$text1<-list("text",x=0.1,y=0.705,text="UR",size=1.5)
    
    sheet$demo$call$col<-labels[names(x.data),"Colour"]
    sheet$demo$call$pch<-labels[names(x.data),"Symbol"]
    sheet$demo$call$cex<-labels[names(x.data),"Size"]

    assign("sheet",sheet,pos=parent.env(environment()))
    pp<<-figaro(demo,prefix="sheet")
    figRedraw()
    if(screen()) .saveCurPlotDef(screen()) else figaroOn()
    #if(!screen()) figaroOn()
    invisible()
}

epsEps<-function(GUI=FALSE,...){ # DONE
    where<-!is.na(init[,"87Sr/86Sri"])&!is.na(init[,"EpsNdi"])
    if(GUI) where<-selectSubset(where=cbind(labels[where,],WR[where,]),save=FALSE,GUI=TRUE)
    x<-init[where,"87Sr/86Sri"]
    y<-init[where,"EpsNdi"]
    ur<-UR[,"87Sr/86Sr"]-UR[,"87Rb/86Sr"]*(exp(lambda["Sr"]*age[!is.na(age)][1]*1e6)-1)
    plotWithLimits(x,y,xlab=srlab,ylab=epsNdlabi,digits.x=6,digits.y=2,title="87Sr/86Sri - EpsNdi plot",...)         
    
    sheet$demo$template$lines0<-list("abline",h=0,v=NULL,lty="solid",col="darkgrey",lwd=3)
    sheet$demo$template$lines1<-list("abline",h=NULL,v=ur,lty="solid",col="darkgrey",lwd=3)
    assign("sheet",sheet,pos=parent.env(environment()))
    pp<<-figaro(demo,prefix="sheet")
    figRedraw()
    if(screen()) .saveCurPlotDef(screen()) else figaroOn()
    #if(!screen()) figaroOn()
    invisible()
}

isochron<-function(){
   on.exit(options("show.error.messages"=TRUE))
   ee<-select.list(c("Rb-Sr","Sm-Nd"),preselect="Rb-Sr",multiple=FALSE)
   if(nchar(ee)==0){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
   if(ee=="Rb-Sr"){
        daughter<-"Sr"; x.axis<-"87Rb/86Sr"; y.axis<-"87Sr/86Sr"; error<-"2s.Sr"
        xlab<-izochron.Srlab.x;ylab<-izochron.Srlab.y
    }
    
    if(ee=="Sm-Nd"){
        daughter<-"Nd";x.axis<-"147Sm/144Nd";y.axis<-"143Nd/144Nd";error<-"2s.Nd"
        xlab<-izochron.Ndlab.x;ylab<-izochron.Ndlab.y
    }

    .isochronMain(daughter=daugter,x.axis=x.axis,y.axis=y.axis,error=error,xlab=xlab,ylab=ylab)

    ages<-winDialogString("Plot ages","650,550,450,350")
    if(is.null(ages)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    
    ages<-as.numeric(unlist(strsplit(ages,",")))        

    izoch<-lm(y.data~x.data)

    t<-1/lambda[daughter]*log(izoch$coefficients[2]+1)/1e6
    cat("Estimated age for the given samples is ",t," Ma\n")
    cat("Estimated initial ratio for the given samples is ",izoch$coeff[1],"\n")
     for (i in 1:length(ages)){
        slope<-e^(lambda[daughter]*ages[i]*10^6)-1
        abline(izoch$coefficients[1],slope,add=TRUE,col="black",lty=3)
        results<-c(t,izoch$coeff[1])
        names(results)<-c("Age (Ma)",paste(daughter," initial ratio",sep=""))
    }
    invisible(results)
}

.isochronMain<-function(daughter="Sr",x.axis="87Rb/86Sr",y.axis="87Sr/86Sr",error="2s.Sr",xlab=izochron.Srlab.x,ylab=izochron.Srlab.y){
    ee<-!is.na(WR[,x.axis])&!is.na(WR[,y.axis])
    where<-selectSubset(where=cbind(WR[ee,],labels[ee,]),save=FALSE,GUI=TRUE)
    x.data<<-WR[where,x.axis]
    y.data<<-WR[where,y.axis]   
 
    if(length(where)==length(ee[ee==TRUE])){
        windows(width = 5, height = 5, pointsize = 10)    
        plot(x.data,y.data,xlab=xlab,ylab=ylab,col=labels[where,"Colour"],pch=3)
        i<-identify(x.data,y.data,where,col="red")
        x.data<<-x.data[i]
        y.data<<-y.data[i]
    }
    
    pch<-rep(3,length(x.data))
    names(pch)<-names(x.data)
    tit<-gsub("(\\d{1,})([\\w\\w])","\\2",x.axis,perl=TRUE)
    plotWithLimits(x.data,y.data,digits.x=2,digits.y=5,xlab=xlab,ylab=ylab,fit=TRUE,IDlabels=names(x.data),pch=pch,title=paste(tit," isochron plot",sep=""))
}


.selectVariable<-function(what=NULL){
    on.exit(options("show.error.messages"=TRUE))
    vars<-c("87Sr/86Sri","143Nd/144Ndi","EpsNdi",
    "1 stg DM model ages (Goldstein et al. 1988)",
    "1 stg DM model ages (Liew & Hofmann 1988)",
    "2 stg DM model ages (Liew & Hofmann 1988)")
        
    deltas<-colnames(WR)[grep("^delta",colnames(WR))]
    vars<-c(vars,deltas)
    
    names(vars)<-c("87Sr/86Sri","143Nd/144Ndi","EpsNdi","TDM.Gold","TDM","TDM.2stg",deltas)

    if(is.null(what)){
        what<-select.list(vars,preselect="87Sr/86Sri",multiple=FALSE)
        if(nchar(what)==0){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    }else{
        what<-vars[what]
    }
    return(c(names(vars)[vars==what],what))
}

boxplotIso<-function(what=NULL){
    what<-.selectVariable(what)
    names(groups)<-rownames(WR)
    if(substr(what,1,5)!="delta"){
        x<-factor(groups[rownames(init)],ordered=TRUE)
    }else{
        x<-factor(groups[rownames(WR)],ordered=TRUE)
    }
    windows(width = 5, height = 5, pointsize = 10,title=paste("Boxplot of ",what[2]))
    par(las=1)
    par(oma=c(1,5,1,1))
    
    ee<-grep("model",what[2])
    if(length(ee)>0){
        xlab<-paste(what[2]," (Ga)",sep="")
    }else{
        xlab<-annotate(what[2])
    }
    
    if(substr(what,1,5)!="delta"){
        out<-boxplot(init[,what[1]]~x,ylab="",xlab=xlab,varwidth=TRUE,cex.axis=0.7,col="lightblue",horizontal=TRUE,xaxs="i")
        rownames(out$stats)<-c("Min","25%","50%","75%","Max")
        colnames(out$stats)<-out$names
        names(out$n)<-out$names
    }else{
        out<-boxplot(WR[,what[1]]~x,ylab="",xlab=xlab,varwidth=TRUE,cex.axis=0.7,col="lightblue",horizontal=TRUE,xaxs="i")
        rownames(out$stats)<-c("Min","25%","50%","75%","Max")
        colnames(out$stats)<-out$names
        names(out$n)<-out$names
    }
    ee<-ee[!is.na(ee)]
    i<-pretty(ee)
    
    abline(v=i,col="gray",lty="dotted")
    abline(v=0,lty="dashed")
    abline(h=(0.5:35.5),lty="dotted",col="gray")
    box()
    figaroOff()
    return(out)
}

stripplotIso<-function(what=NULL){
    strip.main<-function(elem,xlab="",title="Stripplot"){    
        names(groups)<-rownames(WR)
        if(substr(elem,1,5)!="delta"){
            ee<-init[,elem]
        }else{
            ee<-WR[,elem]
        }
        ee<-ee[!is.na(ee)]
        
        x<-factor(groups[names(ee)],ordered=TRUE)
        #trellis.device(bg = "white",new = TRUE,retain = FALSE,title=title)
        #trellis.par.set("background",list(col="white"))
        print(stripplot(x ~ ee,aspect = 1,jitter = TRUE, pch=labels[names(ee),"Symbol"],col=labels[names(ee),"Colour"],xlab=xlab,cex=labels[names(ee),"Size"]))
        
        # Nefunguji pch, col, cex
        #stripchart(ee~x,method="jitter",pch=labels[names(ee),"Symbol"],col=labels[names(ee),"Colour"],xlab=xlab,cex=labels[names(ee),"Size"])

        # Calculate statistical parameters
        out<-boxplot(ee~x,plot=FALSE)
        rownames(out$stats)<-c("Min","25%","50%","75%","Max")
        colnames(out$stats)<-out$names
        names(out$n)<-out$names
        return(out)
    }
    
    what<-.selectVariable(what)   
    ee<-grep("model",what[2])
    if(length(ee)>0){
        xlab<-paste(what[2]," (Ga)",sep="")
    }else{
        xlab<-annotate(what[2])
    }
    figaroOff()
    
    out<-strip.main(what[1],xlab=xlab,title=paste("Stripplot of ",what[2],sep=""))
    return(out)
}

elemIso<-function(xlab=NULL,what=NULL,GUI=FALSE,...){ # DONE
    if(is.null(xlab)) GUI<-TRUE
    if(GUI) xlab<-selectColumnLabel(colnames(WR),default="SiO2",empty.ok=FALSE,silent=TRUE)
    if(length(what)==1) what<-c(what,what)
   
    if(!is.na(as.numeric(xlab))){
        xlab<-colnames(WR)[xlab]
        x.data<-WR[,xlab]
    }else{
        ee<-calcCore(xlab)
        x.data<-ee$results
        xlab<-ee$equation
    }
    
    if(is.null(what)){
        what<-.selectVariable()
    }
     
    ee<-grep("model",what[2])
    
    if(length(ee)>0){
        ylab<-paste(what[2]," (Ga)",sep="")
        digits.y<-1
    }else{
        ylab<-annotate(what[2])
        digits.y<-6
    }
    
    y<-what[1]
    if(substr(y,1,5)!="delta"){
        ee<-!is.na(init[,y])
    }else{
        ee<-!is.na(WR[,y])
    }
    
    if(GUI) where<-selectSubset(where=cbind(labels[ee,],WR[ee,]),save=FALSE,GUI=TRUE) else where<-!is.na(ee)
    
    #y.data<-init[!is.na(init[,y]),y]
    if(substr(y,1,5)!="delta"){
        y.data<-init[where,y]
    }else{
        y.data<-WR[where,y]
    }
    x.data<-x.data[names(y.data)]
    
    tit=paste(xlab," vs. ",what[2]," plot",sep="")
    if(!screen(new=FALSE)){
        plotWithLimits(x.data,y.data,xlab=annotate(xlab),ylab=annotate(ylab),digits.y=digits.y,title=tit,...)
    }else{
        plotWithLimits(x.data,y.data,xlab=annotate(xlab),ylab=annotate(ylab),digits.y=digits.y,title=tit,new=FALSE,...)
    }
    # Plot error bars?
    if(what[1]=="87Sr/86Sri" & any(colnames(WR)=="Sr_2se")) fousy(x.data,y.data,WR[names(x.data),"Sr_2se"],col=labels[names(x.data),"Colour"])
    if(what[1]=="143Nd/144Ndi"& any(colnames(WR)=="Nd_2se")) fousy(x.data,y.data,WR[names(x.data),"Nd_2se"],col=labels[names(x.data),"Colour"])
    
    invisible()
}


reciprocalIso<-function(what=NULL,GUI=FALSE,...){ 
    on.exit(options("show.error.messages"=TRUE))
    if(is.null(what)) GUI<-TRUE
    if(GUI){
        what<-select.list(c("Rb-Sr","Sm-Nd"),preselect="Rb-Sr",multiple=FALSE)
        if(nchar(what)==0){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    }
    
    if(what=="Rb-Sr"){
        x<-"Sr.y"
        xlab="1/Sr"
        ylab<-"87Sr/86Sri"
        digits.y=5
    }else{
        x<-"Nd.y"
        xlab="1/Nd"
        ylab<-"EpsNdi"
        digits.y=1
    }
    
    ee<-!is.na(init[,ylab])
    
    if(GUI) where<-selectSubset(where=cbind(labels[ee,],WR[ee,]),save=FALSE,GUI=TRUE) else where<-!is.na(ee)
    
    options(show.error.messages=FALSE)
    x.data<-try(1/WR[where,x])
    if(class(x.data)=="try-error"){
        x<-strsplit(x,"[.]")[[1]][1]
        x.data<-try(1/WR[where,x])
        if(class(x.data)=="try-error"){
            winDialog(type="ok",paste("Element",x,"not found!"))
            return()
        }
    }
    y.data<-init[where,ylab]
    tit=paste(xlab," vs. ",ylab," plot",sep="")
    plotWithLimits(x.data,y.data,xlab=xlab,ylab=annotate(ylab),digits.x=3,digits.y=digits.y,title=tit,...)
    invisible()    
}


.Sr.error.envelope<-function(){
    x.data<-WR[,"87Rb/86Sr"]
    y.data<-init[,"87Sr/86Sri"]
    plotWithLimits(x.data,y.data,xlab=izochron.Srlab.x,ylab=annotate("87Sr/86Sri"),log="x",digits.y=3)
    curve(0.709+x*.1*(exp(age[1]*1e6*lambda["Sr"])-1),add=TRUE)
    curve(0.709-x*.1*(exp(age[1]*1e6*lambda["Sr"])-1),add=TRUE)
    abline(h=0.709)
}

addResultsIso<-function(){
    if(is.na(all( match(colnames(init),colnames(WR))))){
        cat("Appending...\n")
        addResults("init")
    }else{
        cat("Replacing...\n")
        ee<-WR
        ee[rownames(init),colnames(init)]<-init
        assign("WR",ee,.GlobalEnv)
    }    
}

saveResultsIso<-function(digits = 6){
    saveResults(init,digits=digits)
}

.Allegre<-function(){
    ee<-!is.na(WR[,"87Sr/86Sr"])&!is.na(WR[,"87Rb/86Sr"])&!is.na(WR[,"147Sm/144Nd"])&!is.na(WR[,"143Nd/144Nd"])
    where<-selectSubset(where=cbind(labels[ee,],WR[ee,]),save=FALSE,GUI=TRUE)
    x<-c(init[where,"87Sr/86Sri"],WR[where,"87Sr/86Sr"])
    y<-c(init[where,"143Nd/144Ndi"],WR[where,"143Nd/144Nd"])
    
    ur<-UR[,"87Sr/86Sr"]
    ur0<-UR[,"87Sr/86Sr"]-UR[,"87Rb/86Sr"]*(exp(lambda["Sr"]*age[1]*1e6)-1)
    chur<-CHUR[,"143Nd/144Nd"] 
    chur0<-CHUR[,"143Nd/144Nd"] -CHUR[,"147Sm/144Nd"]*(exp(lambda["Nd"]*age[1]*1e6)-1)
    
    xx<-pretty(c(x,ur0,ur))
    yy<-pretty(c(y,chur0,chur))
    
    plotWithLimits(x,y,xlab="87Sr/86Sr",ylab="143Nd/144Nd",xmin=min(xx),xmax=max(xx),ymin=min(yy),ymax=max(yy),title="87Sr/86Sr - 143Nd/144Nd growth plot after Allegre (2008)")     
    
    #plotWithLimits(x,y,xlab=srlab,ylab=epsNdlabi,digits.x=6,digits.y=6,title="87Sr/86Sr - 143Nd/144Nd growth plot after Allegre (2008)")     
    arrows(init[where,"87Sr/86Sri"],init[where,"143Nd/144Ndi"],WR[where,"87Sr/86Sr"],WR[where,"143Nd/144Nd"],col=labels[where,"Colour"],length=0.1)
    
    abline(h=chur,lty="solid") # Present-day
    abline(h=chur0,lty="dashed")# Initial  
    abline(v=ur,lty="solid") # Present-day
    abline(v=ur0,lty="dashed")# Initial
}

.srndMenu<-function(){
    on.exit(options("show.error.messages"=TRUE)) 
    where<-c("Recalculate to new age", "Copy Sr-Nd isotopic data to clipboard",    "Save Sr-Nd isotopic data",    "Append Sr-Nd isotopic data",    "------------------------------------    ",    "Isochron",    "Sr/Nd growth lines",    "------------------------------------        ",    "87Sr/86Sri vs EpsNdi",    "1/element vs isotopic ratio",    "Element vs isotopic ratio",    "------------------------------------      ",    "Boxplot isotopic ratios/model ages",    "Stripplot isotopic ratios/model ages")
    selected<-select.list(where, multiple = FALSE, title = "Sr-Nd isotopes")
    if(selected==""){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    ee<-switch(which(where==selected),
        "init<<-srnd()", 
        "r2clipboard(init)", 
        "ee<-saveResultsIso(digits=6)", 
        "ee<-addResultsIso()", 
        "", 
        "isochron()", 
        "ee<-ageEps(GUI=TRUE)", 
        "", 
        "epsEps(GUI=TRUE)", 
        "reciprocalIso()", 
        "elemIso(GUI=TRUE)", 
        "", 
        "boxplotIso()", 
        "stripplotIso()"
    )
    cat("GCDkit->",ee,"\n")
    .save2hist(ee)
    eval(parse(text=ee))
} 

if(getOption("gcd.menus")!=""){winMenuAddItem("Plugins","Sr-Nd isotopes",".srndMenu()")}

}else{
    if(!getOption("gcd.shut.up"))cat("WARNING: Skipping: No Sr-Nd isotopic data present!  ")
}
