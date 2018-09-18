#Max 255 chars
about<-function(){
    name<-as.character(packageDescription("GCDkit", fields = "Package"))
    codename<-"El Chupacabra"
    built<-as.character(packageDescription("GCDkit", fields = "Built"))
    date<-as.Date(strsplit(built,";")[[1]][3])
    date<-format(date, "%B %Y")

    ver<-paste(name," [",codename,", ",as.character(date),"]\n(c) 1999-2018\n",sep="")
    Rver<-paste("R for",.Platform$OS.type,paste(getRversion(),collapse="."),sep=" ")
    authors<-paste(
        "\n\nV. Janousek\nCzech Geol. Survey\nvojtech.janousek@geology.cz
        \nC. Farrow\nex-University of Glasgow
        \nV. Erban\nCzech Geol. Survey
        \nJ.-F. Moyen\nUniversite St-Etienne",sep="")
    
    message<-paste(ver,Rver,authors,sep="")
    winDialog(type = "ok",message)
}

#############################################################################
#                           Cluster analysis                                #
#############################################################################
cluster<-function(elems="SiO2,TiO2,Al2O3,FeOt,MnO,MgO,CaO,Na2O,K2O",method="average"){
    on.exit(options("show.error.messages"=TRUE))
    # Select elements
    elems<-selectColumnsLabels(default=elems)

    # Select samples
    where<-selectSamples(print=FALSE)

    # Select linkage
    method<-select.list(c("average", "ward", "single", "complete", "mcquitty", "median", "centroid"),preselect=method)
    if(method==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}

    x<-filterOut(WR[where,],elems)

    if(!getOption("gcd.shut.up")){
        cat("\nComplete cases:\n")
        print(rownames(x))
    }

    # Transform data to log ratios?
    #x<-.log.transform(x,elems)

    # Select labels for clusters
    labs<-selectColumnLabel(message="Label by?",default="1",sample.names=TRUE,empty.ok=FALSE)
    if(labs==1){
        labs<-rownames(x)
    }else{
        labs<-labels[rownames(x),labs-1]
    }

    x[is.na(x)]<-0
    windows()
    hc <- hclust(dist(x),method)
    if(!getOption("gcd.shut.up"))print(hc)
    plot(hc, hang=-1,cex=0.7,labels=labs,xlab="",ylab="")
    if(nrow(x)<20){
        n<-nrow(x)-3
    }else{
        n<-20
    }

    i<-identify(hc,N=n,MAXCLUSTER=n)
    if(length(i)==0) return()
    which<-selectColumnsLabels(colnames(labels),message="Labels to print")

    for (f in 1:length(i)){
        cat(paste("\n############\n"))
        cat(paste("# Group ",f,"#\n"))
        cat(paste("############\n"))
        print(labels[as.vector(i[[f]]),which])
    }
    invisible()
}


#############################################################################
#                       Principal components                                #
#############################################################################

gMean<-function(x){
    z<-sapply(rownames(x),function(i){
        ee<-exp(mean(log(x[i,]),na.rm=TRUE))
        return(ee)
    })
    return(z)
}

#.logGMean<-function(x){
#    gmeans<-gMean(x)
#    z<-sapply(rownames(x),function(i){
#        ee<-log10(x[i,]/gmeans[i])
#        return(ee)
#    })
#    z<-t(z)
#    colnames(z)<-paste(colnames(z),"gm",sep="")
#   return(z)
#}

prComp<-function(comp.data=NULL,use.cov=FALSE,scale=TRUE,GUI=FALSE){
    # Pick samples
    if(GUI){
        which.samples<-selectSamples(print=FALSE)
        # Pick variables
        comp.data<-selectColumnsLabels(default="SiO2,TiO2,Al2O3,FeOt,MnO,MgO,CaO,Na2O,K2O")
    }

    # Select the elements
    if(is.vector(comp.data)){
        comp.data<-comp.data[comp.data%in%colnames(WR)]
        # Filter out the samples with NAs
        comp.data <- filterOut(WR,comp.data,n=1)
    }
    
    # Select samples
    if(GUI){
        ee<-rownames(comp.data)%in%which.samples
        comp.data<-comp.data[ee,] 
    }
    
    # Select just the complete cases
    ee<-apply(comp.data,1,function(i)any(is.na(i)))
    comp.data <- comp.data[!ee,]
    if(!getOption("gcd.shut.up")){
        cat("Complete cases:\n")
        print(rownames(comp.data))
        cat("\n")
    }
    
    # Use correlation coefficients or covariances?
    if(GUI){
        use.cov<-(winDialog(type="yesno","Use covariance instead of correlation matrix?"))
        if(use.cov=="YES")use.cov<-TRUE else use.cov<-FALSE
    }
    
    if(use.cov){
        pc.cr<-princomp(comp.data, cor = FALSE,scores=TRUE,scale=scale)
    }else{
        pc.cr<-princomp(comp.data, cor = TRUE,scores=TRUE,scale=scale)
    }
    
    if(!getOption("gcd.shut.up"))print(summary(pc.cr))
    #if(!getOption("gcd.shut.up"))print(pc.cr$loadings)
    windows(width = 12, height = 6.5, pointsize = 10)
    par(mfrow=c(1,2))
    prd<-biplot(pc.cr,cex=0.7,pc.biplot=TRUE,col=c("darkgray","blue")) # expand
    #xlim<-par()$usr[1:2]
    #ylim<-par()$usr[3:4]
    
    #plot(pc.cr$scores[,1],pc.cr$scores[,2],xlim=xlim,ylim=ylim,xlab="Comp.1",ylab="Comp.2",pch=labels[rownames(comp.data),"Symbol"],col=labels[rownames(comp.data),"Colour"],cex=labels[rownames(comp.data),"Size"],xaxs="i",yaxs="i")
    plot(pc.cr$scores[,1],pc.cr$scores[,2],xlab="Comp.1",ylab="Comp.2",pch=labels[rownames(comp.data),"Symbol"],col=labels[rownames(comp.data),"Colour"],cex=labels[rownames(comp.data),"Size"])
    
    #results<<-summary(pc.cr)
    assign("results",pc.cr$scores,.GlobalEnv)
    invisible(pc.cr)
}

#############################################################################
#         Selects grouping for the data set by cluster analysis             #
#############################################################################


groupsByCluster<-function(elems="SiO2,TiO2,Al2O3,FeOt,MnO,MgO,CaO,Na2O,K2O",method="ave"){
    on.exit(options("show.error.messages"=TRUE))
    # Select elements
    elems<-selectColumnsLabels(default=elems)
    
    x<-filterOut(WR,elems)
    x[is.na(x)]<-0
    windows()
    hc <- hclust(dist(x),method)
    plot(hc, hang=-1,cex=0.5,xlab="",ylab="",sub="")


    x<-winDialogString("How many levels","3")
    if(is.null(x)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}


    if(x!=2){
        groups<<-cutree(hc, k=x)
        print(sort(groups))
    }else{
        groups<<-rep(1,times=nrow(WR))
        i<-identify.hclust(hc)
        print(i)
        for(e in(1:length(i))){
            groups[i[[e]]]<<-e
        }
    }
        xx<-vector()
    tmp<-0
    for (ii in 1:nrow(WR)){
        if (any(names(groups)==rownames(WR)[ii]))
        {xx<-c(xx,groups[ii-tmp])}
        else
        {tmp<-tmp+1;xx<-c(xx,"missing data")}
    }
    names(xx)<-rownames(WR)
    assign("groups",xx,.GlobalEnv)
    #groups<<-xx
    .assignWithNamespaceHard("grouping",0)
    #ee<-dev.off(dev.cur())
    x<-winDialog(type="yesno","Append current groups to labels?")
    if(x=="YES"){
        name<-winDialogString("Name of the new column","groups")
        if(is.null(name)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
        labels<-cbind(labels,groups)
        colnames(labels)[ncol(labels)]<-name
        .assignWithNamespaceHard("labels",labels)
    }
    assign("results",xx,.GlobalEnv)
}

#############################################################################
#                                                                           #
#                    AUXILIARY PETROLOGIC FUNCTIONS                         #
#                                                                           #
#############################################################################


##########################################################################
#  Calculates WRComposition from composition of minerals and the mode   #
##########################################################################
WRComp<-function(mins,f){
        y<-t(data.matrix(mins))%*%f
        WRc<-as.vector(y)
        names(WRc)<-rownames(y)
        return(WRc)
}


##########################################################################
#                        Calculate millications                          #
##########################################################################
millications<-function(x,print=FALSE,save=FALSE){
    x<-as.matrix(x)
    if(ncol(x)==1){
        x<-t(x)
        #rownames(x)<-NULL
    }
    oxides<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","FeOt","MnO","MgO","CaO","Na2O","K2O","H2O.PLUS","CO2","P2O5","F","S")
    oxides<-oxides[oxides%in%colnames(x)]
    ee<-sapply(1:nrow(x),function(f){
        z<-x[f,oxides]/MW[oxides]*x.atoms[oxides]*1000
        return(z)
    })
    milli<-t(ee)
    milli[is.na(milli)]<-0
    rownames(milli)<-rownames(x)
    
    # Get ready names of atoms from the oxide names
    .atoms.from.formula<-function(ox){
        z<-gsub("[0-9]","",ox)                        # Remove numbers                        
        z<-sapply((strsplit(z,"O")),paste,collapse="")    # Remove oxygen's "O" and anything beyond
    return(z)
    }
    
    if(save){
        oxides<-oxides[c(1:11,14)]   
    }
    results<-milli[,oxides,drop=FALSE]
    ee<-paste(.atoms.from.formula(oxides),".m",sep="")
    i<-grep("^Fe.m$",ee)
    if(length(i)==2) ee[i]<-c("Fe3.m","Fe2.m")
    ee<-gsub("^Fet.m$","Fe.m",ee)
    colnames(results)<-ee
    
    #colnames(results)<-c("Si.m","Ti.m","Al.m","Fe3.m","Fe2.m","Fe.m","Mn.m","Mg.m","Ca.m","Na.m","K.m","P.m")
    
    if(print)print(results)
    if(save){
        assign("milli",milli,.GlobalEnv) 
        assign("results",results,.GlobalEnv)   
        invisible()
    }
    return(milli)
}


##########################################################################
#                            De la Roche                                 #
##########################################################################
LaRocheCalc<-function(rock=WR){
   RR<-matrix(nrow=nrow(rock),ncol=2)
   rock[is.na(rock)]<-0

#oxides<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","MnO","MgO","CaO","Na2O","K2O","P2O5")

oxides<-c("SiO2","TiO2","Al2O3","FeOt","MnO","MgO","CaO","Na2O","K2O","P2O5")
   x<-t(t(rock[,oxides])/MW[oxides]*x.atoms[oxides])*1000
   colnames(x)<-c("si","ti","al","fe2","mn","mg","ca","na","k","p")
   RR[,1]<- 4 * x[,"si"] - 11 * (x[,"na"] + x[,"k"]) - 2 * (x[,"fe2"] + x[,"ti"])
   RR[,2]<- 6 * x[,"ca"] + 2 * x[,"mg"] + x[,"al"]
   rownames(RR)<-rownames(rock)
   colnames(RR)<-c("R1","R2")
   RR[RR==0]<-NA
   return(RR)
}

##########################################################################
#                               Debon                                    #
##########################################################################
DebonCalc<-function(millicats=milli){
    x<-matrix(nrow=nrow(millicats),ncol=11)
    #milli[is.na(milli)]<-0
    for (f in seq(1,nrow(millicats))){
        Si<-millicats[f,"SiO2"]
        Ti<-millicats[f,"TiO2"]
        Al<-millicats[f,"Al2O3"]
        Fe<-millicats[f,"FeOt"]
        #Fe3<-millicats[f,"Fe2O3"]
        #Fe2<-millicats[f,"FeO"]
        Mg<-millicats[f,"MgO"]
        Ca<-millicats[f,"CaO"]
        Na<-millicats[f,"Na2O"]
        K <-millicats[f,"K2O"]
        P<-millicats[f,"P2O5"]
    #REM Q
        x[f, 1]<- Si/3 - (K + Na + 2 * Ca / 3)
        q <- x[f, 1]
    #REM P
        x[f, 2]<- K - (Na + Ca)
    #REM A
        x[f, 3]<- Al - (K + Na + 2 * Ca)
    #REM B
        x[f, 4]<-Fe + Mg + Ti
        #x[f, 4]<-Fe2 + Fe3 + Mg + Ti
        b <-x[f, 4]
    #REM FALSE
        x[f, 5]<- 555 - (q + b)
        feld <-x[f, 5]
        x[f, 6]<- Na + K
        x[f, 7]<-  K / (Na + K)
        x[f, 8]<- Mg / (Fe + Mg)
        #x[f, 8]<- Mg / (Fe2 + Fe3 + Mg)
    #REM Recalc to 100%
        suma <- q + b + feld
        x[f, 9]<- q / suma * 100
        x[f, 10]<- b / suma * 100
        x[f, 11]<- feld / suma * 100
    }
    rownames(x)<-rownames(millicats)
    colnames(x)<-c("Q","P","A","B","F","(Na+K)","K/(Na+K)","Mg/(Fe+Mg)","Q(quartz)","B(dark m.)","F(feldsp)")
    assign("results",x,.GlobalEnv)
    #results<<-x
    return(x)
}


# Pouze pro ladeni - aspon zatim
.figaroOn.window<-function(){
    for(i in ((dev.cur()-1):2)){
        n<-paste("$Graph",i,"Main/Plot editing",sep="")
        try(winMenuDel(n),silent=TRUE)
    }
    n<-paste("$Graph",dev.cur(),"Main/Plot editing",sep="")
    winMenuAdd(n)
    winMenuAddItem(n,"Load a graph","figLoad()")
    winMenuAddItem(n,"Save a graph","figSave()")
    winMenuAddItem(n,"-----------------------------------      ","none")
    winMenuAddItem(n,"Edit","figEdit()")
    winMenuAddItem(n,"Edit","figEdit()")
    winMenuAddItem(n,"Colours","figColours()")
    winMenuAddItem(n,"Scale","figScale()")
    winMenuAddItem(n,"Redraw","pp$draw()")
    winMenuAddItem(n,"User defined parameter","figUser()")
    winMenuAddItem(n,"-----------------------------------  ","none")
    winMenuAddItem(n,"ADD          ","none")
    winMenuAddItem(n,"----------------------------------  ","none")
    winMenuAddItem(n,"Legend","figLegend()")
    ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    if(ee=="binary ") winMenuAddItem(n,"Reservoirs/minerals","figAddReservoirs()")
    winMenuAddItem(n,"Text","figAddText()")
    winMenuAddItem(n,"Arrow","figAddArrow()")
    winMenuAddItem(n,"Box","figAddBox()")
    winMenuAddItem(n,"Linear fit","figAddFit()")
    winMenuAddItem(n,"Curve","figAddCurve()")
    winMenuAddItem(n,"----------------------------------- ","none")
    winMenuAddItem(n,"ZOOMING          ","none")
    winMenuAddItem(n,"---------------------------------- ","none")
    winMenuAddItem(n,"Zoom in","figZoom()")
    winMenuAddItem(n,"Zoom out to original size","figUnzoom()")
    winMenuAddItem(n,"Scale x axis","figXlim()")
    winMenuAddItem(n,"Scale y axis","figYlim()")
    winMenuAddItem(n,"------------------------------------","none")
    winMenuAddItem(n,"Identify points","ee<-identify(x.data,y.data,names(x.data),offset=0.4,col=\"orange\",cex=0.5)")
    winMenuAddItem(n,"Highlight multiple points","highlightSelection()")
    winMenuAddItem(n,"------------------------------------ ","none")
    winMenuAddItem(n,"Multiple plot by groups","figMulti()")
    winMenuAddItem(n,"Groups by outline","figGbo()")
}




#############################################################################
#                                                                           #
#                             SELECT BY DIAGRAM                             #
#                                                                           #
#############################################################################
selectByDiagram<-function (diagram = select.list(claslist[, "menu"])){
    on.exit(options("show.error.messages"=TRUE))
    if (diagram == "") {
        cat("Cancelled.\n")
        options(show.error.messages = FALSE)
        stop()
    }
    diagram <- claslist[which(claslist[, 1] == diagram),2]
    
    if (length(diagram) == 0) {
        cat("Invalid diagram specification!\n")
        options(show.error.messages = FALSE)
        stop()
    }
    options(show.error.messages = FALSE)
    demo <- try(eval(call(diagram)))
    options(show.error.messages = TRUE)
    if (class(demo) == "error") {
        return()
    }
    
  #  options(show.error.messages = TRUE)
    windows(width = 6.5, height = 6.5, pointsize = 10)
    pp<<- figaro(demo, prefix = "sheet")
  
    pp$draw(x.data, y.data, col = labels[, "Colour"], pch = labels[, 
        "Symbol"], xlab = sheet$d$c$xlab, ylab = sheet$d$c$ylab, 
        main = sheet$d$c$main, sub = annotate(selected), xaxs = "i", 
        yaxs = "i", cex = labels[, "Size"])
    fields <- numeric(0)
#    classify(diagram, x = x.data, Y = y.data, silent = TRUE)
    classify(diagram, grp = FALSE, labs = length(i) == ii, source.sheet = FALSE, 
        X = x.data, Y = y.data, silent = TRUE)
    classified <- results
    while (length(ee <- locator(1)) != 0) {
        ee <- as.numeric(ee)
        i <- .classifyCore(diagram, x = ee[1], y = ee[2],source.sheet=FALSE)

        i <- which(sheet$d$t$clssf$rcname == as.character(i))
      
        if (length(i) != 0) {
            x.poly <- sheet$d$t[i + 1][[1]]$x
            
            y.poly <- sheet$d$t[i + 1][[1]]$y
            
            is.log <- paste(sheet$demo$call$log, " ")
            if (length(grep("x", is.log)) != 0) 
                x.poly <- 10^x.poly
            if (length(grep("y", is.log)) != 0) 
                y.poly <- 10^y.poly 
            if (any(fields == i)) {
                polygon(x.poly, y.poly, col = "white", border = 0)
                fields <- fields[-which(fields == i)]
                refreshFig()
            }
            else {
                polygon(x.poly, y.poly, col = "lightblue", border = 0)
                fields <- c(fields, i)
                refreshFig()
            }
        }
    }
    i <- match(classified, sheet$d$t$clssf$rcname[fields])
    if (all(is.na(i))) {
        winDialog(type = "ok", "Your query produced no matches!")
        xx <- rownames(WR)
        message <- "The old data set retained \n"
    }
    else {
        xx <- rownames(classified)[which(!is.na(i))]
        message <- paste(length(xx), " sample(s) selected \n", 
            "Default grouping on Symbol", sep = "")
    }
    winDialog(type = "ok", message)
    points(x.data[is.na(match(rownames(WR), xx))], y.data[is.na(match(rownames(WR), 
        xx))], col = "pink", pch = labels[is.na(match(rownames(WR), 
        xx)), "Symbol"])
        
    .saveVariables(xx)
    
    assign("groups",labels[, "Symbol"],.GlobalEnv)
    #groups <<- labels[, "Symbol"]
    .assignWithNamespaceHard("grouping",which(colnames(labels) == "Symbol"))
    #grouping<<- which(colnames(labels) == "Symbol")
    
    nn<-nrow(WRCube[[dataset.name]]$WR)
    cat("\nTotal number of samples is",nn)
    
    #cat("\nTotal number of samples is", nrow(WR.bak))
    cat(" of which are", nrow(WR), "selected", "\n")
    cat("\n")
    return(xx)
}



#############################################################################
#         Selects grouping for the data set on classification diagram       #
#############################################################################

groupsByDiagram<-function(fun=NULL,silent=TRUE){
    on.exit(options("show.error.messages"=TRUE))
    rownames(claslist) <- claslist[, 1]
    
    # Classification function name has been specified
    if(!is.null(fun)){
        x<-grep(fun,claslist[,"function"],value=TRUE,ignore.case=TRUE)
        if (length(x) == 0) {
            cat("Invalid diagram specification!\n")
            options(show.error.messages = FALSE)
            stop()
        }
        
        if (length(x) > 1) {
           cat("Ambigious, several matching classification schemes found.\n")
                print(x) 
                options(show.error.messages=FALSE);stop(call. = TRUE)
        }
        
        fun<-x
        
        options(show.error.messages = FALSE)
        demo <- try(eval(call(fun)))
        options(show.error.messages = TRUE)
        if (class(demo) == "error") {
            return()
        }
        
        pp <- names(x)
        #pp <- claslist[claslist[,"function"]==fun, 1]
    }
    
    # Classification function name has not been specified
    if(is.null(fun)){
        pp <- " "
        xxx <- 1
        while (any(pp == ignore)) {
            pp <- select.list(claslist[, 1])
            if (pp == "") {
                xxx <- 7
                break
            }
        }
        if(pp==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
    }
    
    cat("Classification by ", claslist[pp, 2],"...\n",sep="")
    
    classify(claslist[pp, 2], silent = TRUE)
    if(!silent) print(results)
    
    assign("groups",results,.GlobalEnv)
    #groups<<-results
    
    if(!getOption("gcd.shut.up")) {print(table(groups,dnn="Assigned groups:"))}
}

.classifyCore<-function (diagram,x,y,source.sheet=TRUE){
#input: 1 analysis, figaro template
#output: 1 rockname
if (source.sheet){
    xx <- 0
    eval(call(diagram))
    if (xx == "error") {
        options(show.error.messages = FALSE)
        stop("", call. = FALSE)
    }
    }
cls <- sheet$d$t
    ee <- paste(sheet$demo$call$log, " ")
    if (length(grep("x", ee)) != 0)
        x <- log10(x)
    if (length(grep("y", ee)) != 0)
        y <- log10(y)

rockname<-vector()
for (m in cls$clssf$use) {
    if ((min(cls[[m]]$x) <= x & x <= max(cls[[m]]$x)) &
      (min(cls[[m]]$y) <= y & y <= max(cls[[m]]$y))) {
      xp <- cls[[m]]$x
      yp <- cls[[m]]$y
      if (xp[1] == xp[length(xp)] & yp[1] == yp[length(yp)]) {
        xp <- c(xp, cls[[m]]$x[2])
        yp <- c(yp, cls[[m]]$y[2])
      }
      else {
        xp <- c(xp, cls[[m]]$x[1:2])
        yp <- c(yp, cls[[m]]$y[1:2])
      }
      prep <- FALSE
      for (k in 2:(length(yp) - 1)) {
        if (min(c(xp[k], xp[k + 1])) <= x & x <=
          max(c(xp[k], xp[k + 1]))) {
          if (xp[k] != xp[k + 1]) {
            if (x != xp[k] | (sign(xp[k] - xp[k -
              1]) == sign(xp[k + 1] - xp[k]))) {
              prus <- ((yp[k] - yp[k + 1]) * (x -
                xp[k + 1])/(xp[k] - xp[k + 1])) +
                yp[k + 1]
              prus <- floor(prus * 1e+12)/1e+12
              if (abs(prus - yp[k + 1]) > 1e-09 |
                abs(x - xp[k + 1]) > 1e-09) {
                if (prus == y) {
                  prep <- TRUE
                  break
                }
                if (prus >= y) {
                  prep <- !prep
                  
                }
              }
            }
          }
        }
      }
      if (prep) {
        
        rockname <- c(rockname, cls$c$rcname[m -
          1])
        }
    }
}
if (length(rockname) > 1) {
    rockname <- paste(sort(rockname), collapse = " & ")
    rockname <- paste("boundary between", rockname, sep = " ")
}

return(rockname)
}
