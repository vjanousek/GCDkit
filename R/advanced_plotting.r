
#############################################################################
#                                                                           #
#                      Modified filled contours                             #
#                                                                           #
#############################################################################
.fcm<-function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)),z, xlim,ylim,zlim = range(z, finite = TRUE), nlevels = 20,levels = pretty(zlim, nlevels), col = color.palette(length(levels) -1), plot.title, legend=TRUE,omit.axes, plot.axes,key.title, key.axes, asp = NA,xaxs = "i", yaxs = "i", las = 1, axes = TRUE,overplot=FALSE,...) {
        if(omit.axes)asp<-1
        if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq(0, 1, len = nrow(z))
            }
        }
        else stop("no `z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing x and y values expected")
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    w <- (3 + mar.orig[2]) * par("csi") * 2.54
    if(legend) layout(matrix(c(2, 1), nc = 2), widths = c(1,lcm(w)))
    par(las = las)
    mar <- mar.orig
    mar[4] <- mar[2]
    mar[2] <- 1


if(legend){
    par(mar = mar)
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
        yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1], col = col)
    if (missing(key.axes)) {
        if (axes) 
            axis(4)
    }
    else key.axes
    box()
    if (!missing(key.title)) 
        key.title
    mar <- mar.orig
    mar[4] <- 1
}
    par(mar = mar)
    plot.new()
    log<-paste("",sheet$demo$call$log,sep="")
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp,log = log)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
        stop("no proper `z' matrix specified")
    if (!is.double(z)) storage.mode(z) <- "double"
    #.Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels),col = col))
    graphics::.filled.contour(as.double(x), as.double(y), z, as.double(levels),col = col)

# For binary plots plot axes
    if (!(omit.axes)) {
        if (axes) {
           title(main = "", xlab = "", ylab = "")
            axis(1)
            axis(2)
            box()
        }
    }
# For ternary plots do not plot axes, mask the area surrounding the triangle
    else{
        polygon(x=c(-0.03,0,.5,1,1.05,1.05,-0.03,-0.03),y=c(0,0,sqrt(3)/2,0,0,1.03,1.03,0),col="white",border=NA)
        polygon(x=c(-0.03,1.03,1.03,-0.03,-0.03),y=c(-0.1,-0.1,0,0,-0.1),col="white",border=NA)
        rect(1.03,-0.1,1.05,1.05,col="white",fg="white",border=NA)
    }


# Plot Figaro template
    .my.template(sheet$demo$template)
        x<-winDialog("Overplot?",type="yesno")
        overplot<-(x=="YES")
    if(overplot){
                where<-selectSamples()
                x.data<-x.data[where]
                y.data<-y.data[where]
                points(x.data,y.data,pch=labels[where,"Symbol"],col=labels[where,"Colour"])
    }
            
    if (missing(plot.title)) 
        title(...)
    else plot.title 
    invisible()
    dev.set(dev.prev())
    figRedraw()
}

#############################################################################
#                                                                           #
#                             FILLED CONTOUR PLOT                           #
#                                                                           #
#############################################################################
filledContourFig<-function(xlab=sheet$demo$call$xlab,ylab=sheet$demo$call$ylab,xlim=sheet$demo$call$xlim,ylim=sheet$demo$call$ylim,annotate.fields=FALSE,...){
    on.exit(options("show.error.messages"=TRUE))
    xlim=sheet$demo$call$xlim
    ylim=sheet$demo$call$ylim
    omit.axes=(sheet$demo$call$axes==FALSE) # This is a ternary plot
    
    if(length(omit.axes)==0)omit.axes<-FALSE
    
    n<-winDialogString("Density of the grid (i.e., number of intervals per axis) ","20")
    if(is.null(n)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    
    n<-as.numeric(n)
    
    log<-paste("",sheet$demo$call$log,sep="")
    if(length(grep("x",log))==0){
        x.grid1<-seq(xlim[1],xlim[2],length=n) 
        x.step<-(x.grid1[2]-x.grid1[1])/2
        x.grid<-c(x.grid1,xlim[2]+2*x.step)
        abline(v=x.grid,col="gray")
        xx<-cut(x.data,x.grid)
    }else{
        x.grid1<-seq(log10(xlim[1]),log10(xlim[2]),length=n) 
        x.step<-(x.grid1[2]-x.grid1[1])/2
        x.grid1<-10^x.grid1
        x.grid<-c(x.grid1,xlim[2]+10^(2*x.step))
        abline(v=x.grid,col="gray")
        xx<-cut(x.data,x.grid)
    }
    
    if(length(grep("y",log))==0){
        y.grid1<-seq(ylim[1],ylim[2],length=n)  
        y.step<-(y.grid1[2]-y.grid1[1])/2
        y.grid<-c(y.grid1,ylim[2]+2*y.step)
        abline(h=y.grid,col="gray")
        yy<-cut(y.data,y.grid)
    }else{
        y.grid1<-seq(log10(ylim[1]),log10(ylim[2]),length=n) 
        y.step<-(y.grid1[2]-y.grid1[1])/2
        y.grid1<-10^y.grid1
        y.grid<-c(y.grid1,ylim[2]+10^(2*y.step))
        abline(h=y.grid,col="gray")
        yy<-cut(y.data,y.grid)
    }
    
    ee<-table(xx,yy)
    nlevels<-max(ee)
    
    number<-as.character(ee)
    number[number=="0"]<-""
        
    col<-selectPalette(nlevels)
    if(rownames(col)!="terrain.colors"&rownames(col)!="topo.colors"){
        col<-rev(col)
    }
    if(length(grep("x",log))==0){
        x<-(x.grid1+x.step)
         
    }else{
        x<-10^(log10(x.grid1)+x.step)    
    }
    
    if(length(grep("y",log))==0){
        y<-(y.grid1+y.step)
         
    }else{
        y<-10^(log10(y.grid1)+y.step)    
    }
    text(rep(x, n),rep(y,each=n),number,col="darkgray")
 
    windows(width = 8, height = 6.5, pointsize = 10)
    .fcm(x,y,ee,overplot=TRUE,col=col,xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,omit.axes=omit.axes,nlevels=nlevels)
if(annotate.fields){   
    abline(v=x.grid,lty="dashed",col="lightgreen")
    abline(h=y.grid,lty="dashed",col="lightgreen")
    
    ee[ee==0]<-""
    for(i in (1:length(y))){
        text(x,y[i],ee[,i],col="lightgreen")
    }
}
}

#############################################################################
#                                                                           #
#                                  ADD CONTOURS                             #
#                                                                           #
#############################################################################
.addContoursCore<-function(GUI=FALSE,bandwidth="auto",...){
    on.exit(options("show.error.messages"=TRUE))
    if(GUI){
        bandwidth<-winDialogString("Specify bandwidth - values 0.1-5 or 'auto'\n   (higher value corresponds to smoother result)","auto")
        if(is.null(bandwidth)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    }
    
    x<-x.data[!is.na(x.data)&!is.na(y.data)]
    y<-y.data[!is.na(x.data)&!is.na(y.data)]
    
    if (bandwidth=="auto"){
        out<-kde2d(x,y, n = 100,lims=c(sheet$d$c$xlim,sheet$d$c$ylim))
    }else{
        out<-kde2d(x,y, h= rep(as.numeric(bandwidth),2),n = 100,lims=c(sheet$d$c$xlim,sheet$d$c$ylim))
    }

    contour(out,add=TRUE,...)
    
    #if (bandwidth=="auto") out<-contour(kde2d(x,y, n = 100,lims=c(sheet$d$c$xlim,sheet$d$c$ylim)),add=TRUE,...)
    #else out<-contour(kde2d(x,y, h= rep(as.numeric(bandwidth),2),n = 100,lims=c(sheet$d$c$xlim,sheet$d$c$ylim)),add=TRUE,...)
    invisible(out)
}

addContours<-function(GUI=FALSE,bandwidth="auto",...){
    on.exit(options("show.error.messages"=TRUE))
    
    if(screen(new=FALSE)){
        out<-lapply(1:length(plate.data),function(i){
            .getSlot(i,FALSE)
            screen(i,new=FALSE)
            get("sheet",.GlobalEnv)
            
            #cat("Slot ",i,".\n",sep="")
            #cat("Slot:",i," - ",as.character(sheet$d$c$ylab),"\n")
            z<-.addContoursCore(GUI=GUI,bandwidth=bandwidth,...)
            return(z)
        })
    }else{
         out<-.addContoursCore(GUI=GUI,bandwidth=bandwidth,...)
    }
    invisible(out)
}





# Assigns to each level of x the most frequently used colour (character or numeric)
.getAvgColour<-function(x){
    which<-factor(x)
    ee<-tapply(factor(labels$Colour),which,table)
    col<-lapply(levels(factor(x)),function(i){
        names(which(ee[[i]]==max(ee[[i]])))
        })
    col<-unlist(col)
    if(is.numeric(labels$Colour)) col<-as.numeric(col)
    col<-col[1:length(levels(which))]
    names(col)<-levels(which)
    return(col)
}

#############################################################################
#                                                                           #
#                   Convex hull, by groups as default                       #
#                                                                           #
#############################################################################
 
chullGroups<-function(clusters=groups,border=NULL,fill=FALSE,...){
    on.exit(options("show.error.messages"=TRUE))
    if(is.numeric(clusters)){
        winDialog(type="ok",paste("clusters not defined!"))
        return()
    }
    
    # Setup the colours
    if(is.null(border)){
        border<-.getAvgColour(clusters)
    }else{
        if(length(border)!=length(levels(factor(clusters)))) border<-rep(border,times=length(levels(factor(clusters))))
        names(border)<-levels(factor(clusters))
    }
    
    if(fill) fill<-border else fill<-NA
    ee<-lapply(levels(factor(clusters)),function(i){
        xx<-x.data[clusters==i]
        yy<-y.data[clusters==i]
        xx<-xx[!is.na(xx)&!is.na(yy)]
        yy<-yy[!is.na(xx)&!is.na(yy)]
        ee<-chull(xx,yy)
        ee<-names(xx)[ee]
        which<-c(ee,ee[1])
        polygon(xx[which],yy[which],col=fill[i],border=border[i],...)
    })
    if(!is.na(fill)) points(x.data,y.data,pch=labels[names(x.data),"Symbol"],col="black",cex=labels[names(x.data),"Size"])
    invisible()
}

#############################################################################
#                                                                           #
#                     Contours, by groups as default                        #
#                                                                           #
#############################################################################

contourGroups<-function(clusters=groups,border=NULL,fill=FALSE,precision=50,...){
    on.exit(options("show.error.messages"=TRUE))
    if(is.numeric(clusters)){
        winDialog(type="ok",paste("clusters not defined!"))
        return()
    }
    
    #Setup the labels for clusters
    labs<-as.character(levels(factor(clusters)))
    labs<-abbreviate(labs,minlength = 5)
    
    # Setup the colours
    if(is.null(border)){
        border<-.getAvgColour(clusters)
    }
    names(border)<-labs
    gr<-abbreviate(clusters,minlength = 5)
    ee<-lapply(labs,function(i){
        xx<-x.data[gr==i]
        yy<-y.data[gr==i]
        xx<-xx[!is.na(xx)&!is.na(yy)]
        yy<-yy[!is.na(xx)&!is.na(yy)]
        #col<-col[i]
        if(length(xx)>0&length(yy)>0){
            picked<-intersect(names(xx),names(yy)) # Pick only points for which there are both x and y coordinates available
            grid<-kde2d(xx[picked],yy[picked], n = 500, lims = c(sheet$d$c$xlim, sheet$d$c$ylim))
            contour(grid, levels=max(unlist(grid$z)/precision),col=border[i], labels=i, drawlabels=TRUE, add = TRUE, ...)
        }
    })
   invisible()
}

#############################################################################
#                                                                           #
#                             Strip Boxplot                                 #
#                                                                           #
#############################################################################

stripBoxplot<-function(yaxis="",zaxis="0",ymin=NULL,ymax=NULL,pal="heat.colors",ident=FALSE,scaling.factor=NULL,boxplot.data=NULL,pch=NULL,col=NULL,cex=NULL,silent=TRUE,add=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    
    if(is.null(pch)){
        pch<-labels[,"Symbol"]
        names(pch)<-rownames(labels)
    }
    
    if(is.null(col)){
        col<-labels[,"Colour"]
        names(col)<-rownames(labels)
    }
    
     if(is.null(cex)){
        cex<-labels[,"Size"]
        names(cex)<-rownames(labels)
    }
    
    if(is.numeric(groups)){
        winDialog(type="ok",paste("Groups not defined!"))
        return()
    }
    
    if(!silent){
        if(nchar(yaxis)==0) yaxis<-selectColumnLabel(colnames(WR),message="Select variable... \n(press ENTER to pick from a list)",default="",sample.names=FALSE,silent=TRUE,print=FALSE,empty.ok=FALSE)
    }
        if(!is.na(as.numeric(yaxis))){
            yaxis<-colnames(WR)[yaxis]
            y.data<-WR[,yaxis]
        }else{
            ee<-calcCore(yaxis)
            y.data<-ee$results
            yaxis<-ee$equation
        }
    if(!silent){
        if(zaxis=="0") zaxis<-selectColumnLabel(colnames(WR),message="circles represent \n(0 - nothing, press ENTER to pick from a list)",default="0",sample.names=FALSE,silent=TRUE,print=FALSE,empty.ok=FALSE)
    }
    
    if(zaxis!="0"){
        if(!is.na(as.numeric(zaxis))){
            zaxis<-colnames(WR)[zaxis]
            z.data<-WR[,zaxis]
        }else{
            ee<-calcCore(zaxis)
            z.data<-ee$results
            zaxis<-ee$equation
        }
        ee<-cbind(y.data,z.data)

        #where<-selectSamples()
        #ee<-ee[where,]
        ii<-ee[apply(!is.na(ee),1,all),]


        y.data<-ii[,1]
        z.data<-ii[,2]

        #if(min(z.data)<0)stop("Cannot handle negative data. Quitting...")
    }
    
    if(is.null(ymin)|is.null(ymax)){   
        ymin<-min(y.data,na.rm=TRUE)
        ymax<-max(y.data,na.rm=TRUE)
    }
    x.data<-factor(groups[names(y.data)],ordered=TRUE)
    xmin<-1
    xmax<-length(levels(x.data))
    ee<-abs(xmax-xmin)

    if(zaxis!="0"){    
        ff<-max(z.data,na.rm=TRUE)-min(z.data,na.rm=TRUE)
        shift.z<-min(z.data,na.rm=TRUE)
        scaling.factor<-abs(round(ee/ff/40,7)) # Was commented off
        if(!silent|!is.null(scaling.factor)){
            scaling.factor<-abs(round(ee/ff/40,7))
        }
        
        if(!silent){
            scaling.factor<-as.numeric(winDialogString("scale circles by factor",as.character(scaling.factor)))
            if(length(scaling.factor)==0){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
        }
        
        leg<-pretty(z.data,n=10)

        nlevels<-length(leg)
             
        if(!silent){
            col<-selectPalette(nlevels,pal)
            #col<-selectPalette(nlevels)
        }else{
            col<-selectPalette(nlevels,pal)
        }
        # Plot legend for z
        windows(width = 9, height = 6, pointsize = 14)
        nf <- layout(matrix(c(1,1,2,2,2,2,1,1,2,2,2,2), 2,6,byrow=TRUE))
        layout.show(nf)

        ystep<-(ymax-ymin)/(nlevels+1)
       
        symbols(rep(1.5,nlevels-1),ymax-(1:(length(leg)-1)*ystep),circles=(leg[-1]-shift.z)*scaling.factor*2.25+0.05,axes=FALSE,xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab="",ylab="",main=annotate(zaxis),bg = col,fg="gray30",inches=FALSE,cex.main=1.5)
        #symbols(rep((xmin+xmax)/2,nlevels-1),ymax-(1:(length(leg)-1)*ystep),circles=(leg[-1]-shift.z)*scaling.factor*2.25+0.05,axes=FALSE,xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab="",ylab="",main=annotate(zaxis),bg = col,fg="gray30",inches=FALSE,cex.main=1.5)
        legenda<- paste(as.character(leg)[2:length(leg)-1],"-",as.character(leg)[2:length(leg)])
        box()
        text(rep((xmin+xmax)/2,length(legenda)),ymax-(1:length(legenda)*ystep),legenda,cex=1.3)
        #text(rep(xmax-(xmax-xmin)/5,length(legenda)),ymax-(1:length(legenda)*ystep),legenda,cex=1.3)
        
    }else{ # no second variable specified
        if(!add)windows(width = 9, height = 6, pointsize = 12)
    }
    
   
    # Plot the diagram
    
    # Boxplot
    if(add) minlength<-5 else minlength<-10
    if(is.null(boxplot.data)){
        boxplot(y.data~x.data,ylim=c(ymin,ymax),border="grey",col="lightgrey",boxwex=0.5,names=abbreviate(levels(x.data),minlength=minlength,dot=TRUE),ylab="",main=annotate(yaxis),cex.axis=1.3,cex.lab=1.1,outline=FALSE,las=1,horizontal=TRUE) 
    }else{
        names(boxplot.data)<-abbreviate(levels(x.data),minlength=minlength,dot=TRUE)
        boxplot(boxplot.data,ylim=c(ymin,ymax),border="grey",col="lightgrey",boxwex=0.5,ylab="",main=annotate(yaxis),cex.axis=1.3,cex.lab=1.1,outline=FALSE) 
    }
    abline(h=0)
    
    # Overlying stripplots
    y<-unlist(split(y.data,x.data))
    x.data<-sort(x.data)
    x<-jitter(as.numeric(x.data),0.5)
    if(zaxis!="0"){
        z<-z.data[names(x.data)]
        symbols(x, y, circles=(z-shift.z)*scaling.factor+0.05,ylim=c(ymin,ymax),inches=FALSE, bg = col[findInterval(z,leg,rightmost.closed=TRUE)],fg="gray30",ylab=annotate(yaxis),yaxs="i",add=TRUE)
        palette(as.vector(palette.gcdkit))
    }else{
        points(y, x, xlim=c(ymin,ymax),pch=pch[names(x.data)],col=col[names(x.data)],cex=cex[names(x.data)],ylab=annotate(yaxis))
        #points(x, y, ylim=c(ymin,ymax),pch=labels[names(x.data),"Symbol"],col=labels[names(x.data),"Colour"],cex=labels[names(x.data),"Size"],ylab=annotate(yaxis))
    }
    if(getOption("gcd.ident"))ee<-ID(x,y,labs=names(x.data),col="black")
    invisible()
}


# Still to do for integration to system - legend
# what is variable name or formula
# pal palette
# save: if true, labels$Colours are also superassigned

assignColVar<-function(what=NULL,pal="heat.colours",save=TRUE,n=15,quant=0,eq.classes=FALSE,alt.leg=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    options(show.error.messages=FALSE)
    
    # Batch mode or GUI?
    if(!is.null(what)) GUI<-FALSE else GUI<-TRUE
    
    # Check that quant is 0 to 50
    if(quant<0 | quant>50) {cat("Invalid quant value (must be 0-50)! Quitting...\n");stop(call. = TRUE)} # NEW BY VJ
    
    # Colours
    if(GUI){
        what<-selectColumnLabel(colnames(WR),message="Colours represent \n(press ENTER to pick from a list)",default="",sample.names=FALSE,silent=TRUE,print=FALSE,empty.ok=FALSE)
        if(is.numeric(what)) what<-colnames(WR)[what]
    }
    
    # Property
    if(!is.na(as.numeric(what))){
        zaxis<-colnames(WR)[what]
        z.data<-WR[,what]
    }else{
        ee<-calcCore(what)
        z.data<-ee$results
        zaxis<-ee$equation
    }
    
    if(GUI){
        n<-winDialogString("Desired approximate number of intervals",as.character(n))
        if(is.null(n)){cat("Cancelled.\n");stop(call. = TRUE)}
        if(is.na(as.numeric(n))){winDialog("ok","Non-numeric value!");stop(call. = TRUE)}
        n<-as.numeric(n)
    }
    
    # This block trims the interval to the (quant, 1-quant)-th quantile of the dataset
    # This helps to take care of outliers    
     if(GUI){
        quant<-winDialogString("Trim the data to n-th quantile?\nEnter a single value between 0 and 100...","0")
        if(is.null(quant)){cat("Cancelled.\n");stop(call. = TRUE)}
        if(is.na(as.numeric(quant))) {winDialog("ok","Non-numeric value!");stop(call. = TRUE)}
        quant<-as.numeric(quant)        
        if(quant<0 | quant>50) {winDialog("ok","Invalid value!");stop(call. = TRUE)} # NEW BY VJ
    }
    zmin<-quantile(z.data,quant/100,na.rm=TRUE)   
    zmax<-quantile(z.data,1-quant/100,na.rm=TRUE) 
    
    # Nothing left (only if quant=50 and we have even number of samples
    options(show.error.messages=FALSE)
    if(zmin==zmax) {cat("No matches! Quitting...\n");stop(call. = FALSE)} # NEW BY VJ
    
    zz<-range(subset(z.data,z.data>=zmin&z.data<=zmax)) 
    
    # There are two ways to cut the colour scale, by quantiles or in regular increments
    if(GUI){
        x<-winDialog("Make the intervals of the same width\n(rather than to contain equal numbers of values)?",type="yesno")
        if(is.null(x)){cat("Cancelled.\n");stop(call. = TRUE)}
        eq.classes<-(x=="NO")
    }
    
    if(eq.classes){
        brk<-pretty(c(0,1),n) # Pretty intervals containing the same numbers of observations (based on probabilitities, step by 20%)
        leg.levels<-unique(quantile(z.data[z.data>zmin&z.data<zmax],brk,na.rm=TRUE))
    }else{
        leg.levels <- pretty(zz, n = n + 1) # Pretty intervals of the same width in wt. % etc
    }
    
    # NEW BY VJ, PROBABLY JUNK
    #if(quant!=0){           
    #    dec.places<-nchar(gsub("^[0-9]{1,}[.]?", "",abs(max(leg.levels,na.rm=TRUE,finite=TRUE))))
    #    leg.levels[1]<-.round.min.down(zz[1],dec.places)
    #    leg.levels[length(leg.levels)]<-.round.max.up(zz[2],dec.places)
    #    leg.levels<-unique(leg.levels)
    #} # \VJ
       
    nlevels<-length(leg.levels)-1
    
    if(GUI){
        colour.palette<-NULL
    }
    
    if(GUI){
        pal<-selectPalette(nlevels)
    }else{
        pal<-selectPalette(nlevels,pal)
    }
    
    col<-as.character(cut(z.data,leg.levels,pal,include.lowest=TRUE))
    col[z.data<zmin]<-NA  
    col[z.data>zmax]<-NA
    
    col[is.na(col)]<-"gray"
    
    if(save){
        labels[,"Colour"]<-col
        .assignWithNamespaceHard("labels",labels)
    }
    
    leg<-pal
    colnames(leg)<-paste(as.character(leg.levels)[2:length(leg.levels)-1],"-",as.character(leg.levels)[2:length(leg.levels)],sep="")
    rownames(leg)<-zaxis
    
    assign("leg.col",leg,.GlobalEnv)
    assign("leg.pch",0,.GlobalEnv)
    
    if(GUI|alt.leg){
        showLegend(alt.leg=alt.leg,just.colours = TRUE,GUI=GUI) 
        options(show.error.messages=FALSE)
    }
    cat("Colours assigned successfully.\n")
    invisible(list(prop=what,col=col,leg=leg))
}

# Returns index of valid hexadecimal numbers within a character vector (# and 6-8 characters of 0-9A-F)
.whichHexaColour<-function(hexe){
    which<-sapply(hexe,function(i){
        z<-grep("^#[a-f0-9]{6,8}",i,ignore.case=TRUE)
        return(length(z)>0)
    },simplify=TRUE)
    return(which)
}

# Sets up the alpha channel (opacity attribute) within the given string of hexadecimal codes of colours [col]
.transparencyInternational<-function(col,alpha="66"){
        if(is.list(col)) col<-unlist(col)
        out<-sapply(1:length(col),function(i){
            nn<-nchar(col[i])
            z<-paste(col[i],paste(rep("0",9-nn),collapse=""),sep="")
            return(z)
        },simplify=TRUE)
        substr(out,8,9)<-alpha
        return(out)
}

# Sets opacity (alpha channel) of plotting symbols in GCDkit. 
# Opacity is a hexadecimal number, inverse of transparency

setTransparency<-function(which.samples=NULL,transp=NULL,alpha=NULL,col.in="black",save=TRUE,GUI=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    if(save){
        col.in<-labels[,"Colour"]
        names(col.in)<-rownames(labels)
    }

    # Translate any colours that are not hexadecimal already
    if(any(!.whichHexaColour(col.in))){
        i<-!.whichHexaColour(col.in)
        prd<-matrix(nrow=4,ncol=length(col.in),"ff",dimnames=list(c("red","gren","blue","alpha"),names(col.in)))
        prd[1:3,i]<-as.character(as.hexmode(col2rgb(col.in[i])))
        prd[prd=="0"]<-"00"
        mash<-paste("#",toupper(apply(prd,2,paste,collapse="")),sep="")
        col.in[i]<-mash[i] # those that were hex already are untouched
     }   
        
    col.out<-col.in

    # Select samples for which the transparencies are to be set
    if(save&is.null(which.samples)){
        if(GUI){
            which.samples<-selectSamples()
        }else{
            which.samples<-rownames(labels)
        }
    }
    
    if(!is.null(alpha)){
        # Test for valid alpha value
        if(nchar(alpha)!=2){winDialog(type="ok","Invalid input!\nAlpha has to be two characters long.");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        if(length(grep("[^a-f0-9]",alpha,ignore.case=TRUE))){winDialog(type="ok","Invalid input!\nAlpha has to be hexadecimal number.");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        transp<-1-as.numeric(as.hexmode(alpha))/255
    }

    # Choose the transp value
    if(is.null(transp)){
        #if(!is.na(as.numeric(transp)))
        transp<-winDialogString("Transparency (0-1)","0")
        if(length(transp)==0){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        transp<-as.numeric(transp)
        
        # Test for valid transparency value
        if(is.na(transp)){winDialog(type="ok","Invalid input!\nTransparency has to be numeric.");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        if(transp>1|transp<0){winDialog(type="ok","Out of bounds!\nTransparency has to be 0-1.");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    }
    
    if(!is.null(alpha)){
        # Test for valid alpha value
        if(nchar(alpha)!=2){winDialog(type="ok","Invalid input!\nAlpha has to be two characters long.");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        if(length(grep("[^a-f0-9]",alpha,ignore.case=TRUE))){winDialog(type="ok","Invalid input!\nAlpha has to be hexadecimal number.");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    }else{
        # Convert transparency to alpha value
        alpha1<-1-transp
        alpha1<-as.hexmode(floor(alpha1*255))
        alpha<-format(alpha1,width=2,upper.case=TRUE)
    }
          
    
    if(save){
        col.out[which.samples]<-.transparencyInternational(col.in[which.samples],alpha=toupper(alpha))
        labels[,"Colour"]<-col.out
        .assignWithNamespaceHard("labels",labels)
        message<-"Transparency successfully assigned"
        if(GUI){
            winDialog(type="ok",message)
            showLegend(new.plot=TRUE)
        }else{
            #cat(message)
            #cat("\n")
        }
    }else{
        col.out<-.transparencyInternational(col.in,alpha=toupper(alpha))
    }
    return(col.out)
}

.definedPalettes<-function(which,n){
    col.list<-c("standard","grays","reds","blues","greens","cyans","violets","yellows","cm.colors","heat.colors","terrain.colors","topo.colors","rainbow","jet.colors")  
    if(is.character(which)) which<-match(which,col.list)
    if(is.na(which)) return("Invalid palette")
    if(which>length(col.list)|which<=0) return("Invalid palette")
    
    # Standard colours (eight of them)
    std<-(c("black","red","green3","blue","cyan","magenta","yellow","white"))
    std2<-as.character(as.hexmode(col2rgb(std)))
    std2<-apply(std2,2,paste,collapse="")  
    
    pal<-switch(which,
            paste("#",toupper(std2),sep=""),
            rev(gray(seq(1-1/(n),0,by=-1/(n)))),
            rgb(r=(2:(n+1))/(n+1),g=0,b=0),
            rgb(b=(2:(n+1))/(n+1),g=0,r=0),
            rgb(g=(2:(n+1))/(n+1),r=0,b=0),
            rgb(g=(2:(n+1))/(n+1),r=0,b=(2:(n+1))/(n+1)),
            rgb(r=(2:(n+1))/(n+1),g=0,b=(2:(n+1))/(n+1)),
            rgb(r=(2:(n+1))/(n+1),g=(2:(n+1))/(n+1),b=0),
            rev(cm.colors(n)),
            heat.colors(n),
            terrain.colors(n+1)[1:n],
            topo.colors(n),
            rainbow(n, end = 0.9),
            jet.colors(n)
        )
    pal<-t(as.matrix(pal))
    rownames(pal)<-col.list[which]
    return(pal)
}

selectPalette<-function(n,colour.palette=NULL,GUI=TRUE){ 
    on.exit(options("show.error.messages"=TRUE))      
    if(n<=0){stop("Invalid number of colours! Quitting...",call. = FALSE)}
    col.list<-c("grays","reds","blues","greens","cyans","violets","yellows","cm.colors","heat.colors","terrain.colors","topo.colors","rainbow","jet.colors")  
       
    # GUI
    if(is.null(colour.palette)){       
        # show the table of available palettes
        if(GUI){    
            n.show<-15
            windows(width = n.show, height = n.show, pointsize = 10,rescale="fit")
            par(mai=c(0,0,0,0))
            ipch <- 0:(n.show-1)
            by<-n.show
            ix<-1:n.show
            plot(1, 1,type='n', xlim=c(-0.5,max(ix)+n.show/10),ylim=c(length(col.list)+0.5,-0.5),axes=FALSE, xlab='',ylab='',asp=1)
            ee<-lapply(1:(length(col.list)+1),function(j){
                col<-.definedPalettes(j,n.show)
                iy<-rep(j,n.show)
                points(ix,iy, pch=15, col=col, cex=by)        
                text (n.show, j, rownames(col),col="black",cex=3,adj=1)
            })
        }
        colour.palette<-select.list(col.list)
        if(nchar(colour.palette)==0){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        if(GUI)ee<-dev.off(dev.cur())
    
        }
        
        colour.palette<-gsub("colours","colors",colour.palette) # Fixes UK spelling to the US one
        ee<-which(col.list==colour.palette)
        
        if(length(ee)==1){ # Predefined palette in the system
             pal<-.definedPalettes(ee+1,n)
        }else{ # User defined palette function?
            pal<-try(eval(parse(text=paste(colour.palette,"(",n,")",sep=""))))
            if(class(pal)=="try-error"){
                stop("Invalid colour.palette scheme! Quitting...",call. = FALSE)
            }
            
            pal2<-substr(pal,2,length(pal))
            pal2<-try(strtoi(pal2,16L))
            if(class(pal2)=="try-error"){
                stop("Invalid colour.palette scheme! Quitting...",call. = FALSE)
            }
            pal<-t(as.matrix(pal))
            rownames(pal)<-colour.palette  
        }    
       return(pal)
}

# if in plate, erase the given slot
.eraseScreen<-function(){
        old<-par(usr = c(0, 1, 0, 1), mar = c(0,0, 0, 0), oma=c(0,0,4,0))
        par(new = TRUE)
        plot.new()
        #box(which="figure",col="pink",lwd=4)
        #rect(0,0,1,1,lty="blank",col="red")
        polygon(c(0, 1, 1, 0), c(0, 0, 1, 1), border = NA, col = "white")
        par(old)
}
