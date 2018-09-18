# Redraws a clean version of plate (for output)
plateRedraw<-function(device="windows",filename=NULL,colormodel="rgb"){
    on.exit(options("show.error.messages"=TRUE))
    
    if(length(plate.data)==1) {winDialog(type="ok","No plate defined yet!");return(invisible())}

    number<-length(plate.data)
   
    if(device=="windows" & !is.null(dev.list())) ee<-dev.off(dev.cur())
    .plateSetup(number,new=FALSE,device=device,filename=filename,title=plate$title,colormodel=colormodel)
    
    par(oma=c(0,0,4,0))
    
    if(colormodel=="gray") bw<-TRUE else bw<-FALSE
        ee<-lapply(1:number,function(i){
            screen(i)#,new=FALSE)
            if(mode(plate[[i]])=="list"){
                .loadCurPlotDef()
                if(is.list(sheet)){ # Otherwise is empty
                    typ<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
                    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
                        if(typ=="spider ") par(mar=c(5,4,4,1)) else {par(mar=c(4.5,5.5,2,1.5));par(pty="s")}
                        if(typ=="ternary ") par(mar=c(1,0.5,2,0.5));par(pty="s")
                    }else{ # New
                        if(typ=="spider ") par(mar=c(2,0.5,1,1)) else {par(mar=c(4.5,3,0.5,0.5));par(pty="s")}
                        if(typ=="ternary ") par(mar=c(1,0.5,1,0.5));par(pty="s")
                    }
                    
                    sheet$demo$call$bg<<-"transparent"
                    figRedraw(bw=bw)
                    #if(plate$slot.labels!="") legend(plate$slot.labels$pos,plate$slot.labels$labs[i],bty="n",cex=plate$slot.labels$cex,inset=0,adj=plate$slot.labels$adj)
                    if(plate$slot.labels!="") legend(plate$slot.labels$pos,plate$slot.labels$labs[i],bty="n",cex=plate$slot.labels$cex,inset=0,adj=plate$slot.labels$adj)
                }
            }
            invisible()
            })
     mtext(text=annotate(plate$title),side=3,line=0.25,outer=TRUE,cex=1.5)
   
     options(show.error.messages=FALSE)
     
     if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
        i<-grep("^[$]Graph[0-9]",winMenuNames())
        if(length(i)==0){    
            .menuPopUp()        
        }
     }
}

# Export a clean version of plate to PS
platePS<-function(colormodel="rgb"){
    on.exit(options("show.error.messages"=TRUE))
    if(length(plate.data)==1) {winDialog(type="ok","No plate defined yet!");return(invisible())}
    filename<-winDialogString("Output file","out")
    if(is.null(filename)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    filename<-paste(filename,".ps",sep="")
    plateRedraw(device="postscript",filename=filename,colormodel=colormodel)
    ee<-dev.off(dev.cur())
}

# Saves the currently selected plot definition, which is determined by screen()
.saveCurPlotDef<-function(i=screen(new=FALSE),which=names(x.data)){    
    if(is.null(sheet$demo$call$pch)) sheet$demo$call$pch<-labels[which,"Symbol"]
    
    if(is.null(sheet$demo$call$col)) sheet$demo$call$col<-labels[which,"Colour"]
    if(length(sheet$demo$call$col)==1) sheet$demo$call$col<-labels[which,"Colour"]
    
    if(is.null(sheet$demo$call$cex)) sheet$demo$call$cex<-labels[which,"Size"]
    if(length(sheet$demo$call$cex)==1) sheet$demo$call$cex<-labels[which,"Size"]
    plate[[i]]<-sheet
    if(is.matrix(y.data)){
        plate.data[[i]]$x<-x.data
        plate.data[[i]]$y<-y.data
    }else{
        plate.data[[i]]$x<-x.data[which]
        plate.data[[i]]$y<-y.data[which]
    }
    assign("plate",plate,.GlobalEnv)
    assign("plate.data",plate.data,.GlobalEnv)
    assign("sheet",sheet,.GlobalEnv)
    invisible()
}

# Loads the currently selected plot definition, which is determined by screen()
.loadCurPlotDef<-function(i=screen(new=FALSE)){
    on.exit(options("show.error.messages"=TRUE))
    assign("x.data",plate.data[[i]]$x,.GlobalEnv)
    assign("y.data",plate.data[[i]]$y,.GlobalEnv)
    options("show.error.messages"=FALSE)
    sheet<-plate[[i]]
    if(any(names(plate[[i]])=="demo")){
        assign("sheet",plate[[i]],.GlobalEnv)
        out<-TRUE
    }else{
        out<-FALSE
    }
    
    if(length(plate[[i]][[1]])>1){
        sheet$demo$call$new<<-FALSE
        typ<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
        #par(oma=c(0,0,4,0))
        if(typ!="spider ") par(pty="s")
        assign("sheet",sheet,.GlobalEnv)
        pp<<-figaro(demo,prefix="sheet")
    }
    invisible(out) # Returns TRUE upon success, FALSE if the current slot is empty
}

# Test whether the given parameter in sheet$demo$call, say xlab, is the same in all diagrams forming the plate
.allTheSame<-function(param){
    on.exit(options("show.error.messages"=TRUE))
    options("show.error.messages"=FALSE)
    ee<-try(sapply(2:length(plate.data),function(i){
        screen(i-1,new=FALSE)
        if(is.null(as.list(plate[[i]])$demo)) return(TRUE)# for empty slot
        .loadCurPlotDef()
        what.old<-as.character(sheet$demo$call[[param]])
        screen(i,new=FALSE)
        .loadCurPlotDef()
        what.new<-as.character(sheet$demo$call[[param]])
        return(identical(what.old,what.new))
    },simplify=TRUE))
    options("show.error.messages"=TRUE)
    z<-all(ee)
    if(is.na(z)) z<-FALSE
    invisible(z)
}

.getSlot<-function(scr,redraw.plate=TRUE){
    on.exit(options("show.error.messages"=TRUE))
    bg.bak<-par("bg")
    if(redraw.plate){
        par(bg="white")
        erase.screen(scr)
    }
    screen(scr,new=FALSE) # Important
    ee<-.loadCurPlotDef(scr)
    
    if(!ee){
        cat("Empty slot!\n")
        return(FALSE) # Slot is empty NEW!!
    }
    
    get("sheet",.GlobalEnv)
    
    options("show.error.messages"=FALSE)
    try(do.call("par",sheet$d$c))
    par("bg"=bg.bak)
    
    #if(any(names(plate[[scr]])=="demo") & .Platform$GUI=="Rgui") figRedraw() # Otherwise is empty
    
    if(any(names(plate[[scr]])=="demo")&redraw.plate) figRedraw() # Otherwise is empty
    screen(scr,new=FALSE)
    invisible(sheet)
}

# Front end for changing either x or y-axis limits in a single plot of a plate
.figZooming.plate<-function(){
    on.exit(options("show.error.messages"=TRUE))
    menus<-c("... xaxis","... yaxis")
    what<-select.list(menus,title="Scale")
    if(what==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
    
    i<-switch(which(menus==what),"figXlim()","figYlim()")
    cat("GCDkit->",i,"\n")
    .save2hist(i)
    eval(parse(text=i))
}

.plateLabelSlotsI<-function(){
    text<-select.list(c("letters: a, b, c,...","LETTERS: A, B, C,...","numbers: 1, 2, 3, 4",
        "roman: i, ii, iii, iv...","ROMAN: I, II, III, IV..."),title="Select the label type")
    text<-unlist(strsplit(text,":"))[1]
    style<-select.list(c("","()","[]","{}"),title="Select the label style")
    pos<-select.list(c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center"),title="Relative position") 
    plateLabelSlots(text=text,style=style,cex=1.5,pos=pos)
}   

# A wrapper function that applies the given function to all plots of the plate. The changes are saved.
.plateFig<-function(fun){
    on.exit(options("show.error.messages"=TRUE))
    if(!options()$gcd.shut.up)cat(fun,"\n")
    flush.console()
    dev.off(dev.cur())
    .plateSetup(length(grep("^Fig",names(plate))),nrow=plate$nrow,ncol=plate$ncol,title=plate$title,new=FALSE) # NEW
    par(oma=c(0,0,4,0))
    
    lapply(1:length(plate.data),function(i){
        screen(i) #,new=FALSE)
        if(mode(plate[[i]])=="list"){ # Otherwise is empty
            .loadCurPlotDef()
            if(is.list(sheet)){ 
                figRedraw()
                typ<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
                if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
                        if(typ=="spider ") par(mar=c(5,4,4,1)) else {par(mar=c(4.5,5.5,2,1.5));par(pty="s")}
                        if(typ=="ternary ") par(mar=c(1,0.5,2,0.5));par(pty="s")
                        
                        options("show.error.messages"=TRUE)
                        ii<-try(eval(parse(text=fun)),silent=FALSE)
                        if(class(ii)=="try-error"){
                            winDialog(type="ok","ERROR!")
                            return(ii)
                        }
                    }else{
                        if(typ=="spider ") par(mar=c(2,0.5,1,1)) else {par(mar=c(4.5,3,0.5,0.5));par(pty="s")}
                        if(typ=="ternary ") par(mar=c(1,0.5,2,0.5));par(pty="s")
                }
            .saveCurPlotDef(i)
            }
        }
    })
   
    mtext(text=annotate(plate$title),side=3,line=0.25,outer=TRUE,cex=1.5)
    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui") .menuPopUp()
    #if(.Platform$GUI!="Rgui") plateRedraw()
    invisible()
}

# Save/load a plate
plateSave<-function(){
    if(length(plate.data)==1) {winDialog(type="ok","No plate defined yet!");return(invisible())}
    filename<-winDialogString("Enter filename","My graph")
         pch<-labels[,"Symbol"]
         col<-labels[,"Colour"]
         eval(parse(text=paste("save(col,pch,plate.data,file = \"",filename,".mgr\", ascii = TRUE)",sep="")))
         cat("Plate stored in file ", filename, ".mgr\n",sep="")
}

plateLoad<-function(){
         on.exit(options("show.error.messages"=TRUE))
         #options("show.error.messages"=FALSE)
         filename<-choose.files(caption = "Select file",multi = FALSE, filters=matrix(ncol=2,byrow=TRUE,c("Figaro plate (*.mgr)","*.mgr")))
         if(filename==""){cat("Cancelled.\n");stop()}
         options("show.error.messages"=TRUE)
         
         load(filename)
         cat("Plate restored from file ", filename,"\n",sep="")
         plate<<-plate
         plate.data<<-plate.data
         
         options(show.error.messages=FALSE)
            ee<-try(assign("col",col,.GlobalEnv))
            ee1<-try(assign("pch",pch,.GlobalEnv))
         options(show.error.messages=TRUE)
         if(class(ee)=="try-error"|class(ee1)=="try-error"){
            assign("col",rep(1,length(x.data)),.GlobalEnv)
            assign("pch",rep(1,length(x.data)),.GlobalEnv)
         }
         windows()
         plateRedraw()
         scr.old<<-1
}

# Expands the selected plot
plateExpand<-function(scr=NULL){
    if(length(plate.data)==1) {winDialog(type="ok","No plate defined yet!");return(invisible())}
    
    if(is.null(scr)) scr<-screen(new=FALSE)
    plateRedraw()
    screen(scr,new=FALSE)
    .loadCurPlotDef()
    windows()
    figRedraw()
    #figaroOn(keep.plate=TRUE)   # TO DO WORKS WELL Jenom pridat co je od komentaru niz do nejake polozky v ramci Plot editing menu
    #.saveCurPlotDef(scr)
    #figaroOff
    .figOk()
    ee<-dev.off(dev.cur())
    plateRedraw()
    screen(scr,new=FALSE)
    assign("scr.old",scr,.GlobalEnv)
    invisible()
}

# Extracts a single plot from a pre-defined plate
plateExtract<-function(diagram,which=NULL,main=NULL,calc.only=FALSE,...){
    on.exit(options("show.error.messages"=TRUE))
    args<-list(...)
    if(!screen(new=FALSE)&!calc.only){
        tit<-paste(diagram,": ",which,sep="")
        windows(width = 6.5, height = 6.5, pointsize = 10,title=tit)
        new.plt<-TRUE
    }else{
        new.plt<-FALSE
    }
    
    if(length(ls(pattern=paste("^",diagram,"$",sep=""),envir=.GlobalEnv))==0){
        options(show.error.messages=FALSE)
        winDialog(type="ok","Invalid diagram name!")
        stop()
    }
    
    # Backup any existing plate
    plate.bak<-plate
    plate.data.bak<-plate.data
    scr.bak<-screen()
         
    xx<-try(eval(do.call(diagram,args)))
    if (class(xx) == "try-error") {
        options("warn"=-1)
        options(show.error.messages=FALSE)
        stop()
    }
    
    number<-length(plate.data)
    if(which>number|which<=0){
        options(show.error.messages=FALSE)
        winDialog(type="ok","Invalid diagram!")
        stop()
    }
    .loadCurPlotDef(which)
        # Calculate x data
        x.formula<<-x.data
        if (is.character(x.formula)) {
            x.data<-calcCore(x.formula,where="WR",redo=FALSE)$results
        }
        if(all(is.na(x.data))){
            figRedraw()
            #.dummyPlot()
            winDialog(type="ok",paste("Error in x-coordinate!",x.formula))
            return()
        }
        
        # Calculate y data
        y.formula<<-y.data
        if (is.character(y.formula)) {
            y.data<-calcCore(y.formula,where="WR",redo=FALSE)$results
        }
        
        if(all(is.na(y.data))){
            .dummyPlot()
            winDialog(type="ok",paste("Error in y-coordinate!",y.formula))
            return()
        }
       
        i<-names(x.data)
        sheet$demo$call$col<-labels[i,"Colour"]
        sheet$demo$call$pch<-labels[i,"Symbol"]
        sheet$demo$call$cex<-labels[i,"Size"]
       
        if(is.null(main)&sheet$demo$call$main==""){
            sheet$demo$call$main<-annotate(plate$title)
        }else{
            sheet$demo$call$main<-annotate(main)
        }    
        assign("x.data",x.data,.GlobalEnv)
        assign("y.data",y.data,.GlobalEnv)
        assign("sheet",sheet,.GlobalEnv)
        
        args<-c(args,list(which=which))
        sheet$demo$call$diagram<<-diagram # BRAND NEW
        sheet$demo$call$arguments<<-args  # BRAND NEW
        sheet$demo$call$plotting.function<<-"plateExtract" # BRAND NEW
   
        #if(!screen(new=FALSE))new.plt <- TRUE else new.plt<-FALSE NEWLY SWITCHED OFF
        
        # If plotting in a plate
        if(!new.plt){
            options(show.error.messages=FALSE)
            
            # Restore any preexisting plate
            assign("plate",plate.bak,.GlobalEnv)
            assign("plate.data",plate.data.bak,.GlobalEnv)
            
            ee<-screen(new=FALSE)
            
            if(!calc.only){
                .saveCurPlotDef(ee)
                if(ee<length(plate.data)) screen(ee+1,new=FALSE)
            }
            scr.old<<-ee
            options(show.error.messages=TRUE)
            get("sheet",.GlobalEnv)
            typ<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
            
            if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
                        if(typ=="spider ") par(mar=c(5,4,4,1)) else {par(mar=c(4.5,5.5,2,1.5));par(pty="s")}
                        if(typ=="ternary ") par(mar=c(1,0.5,2,0.5));par(pty="s") 
            }else{ # New
                        if(typ=="spider ") par(mar=c(2,0.5,1,1)) else {par(mar=c(4.5,3,0.5,0.5));par(pty="s")}
                        if(typ=="ternary ") par(mar=c(1,0.5,1,0.5));par(pty="s")
            }  
            
            screen(ee,new=FALSE)
            if(!calc.only)if(ee) plateRedraw() else figRedraw()
            
            scr.old<<-ee
            
            if(ee<length(plate.data.bak)) screen(ee+1,new=FALSE)
            options(show.error.messages=TRUE)
        }else{
            if(!calc.only)figRedraw()
            if(.Platform$GUI=="Rgui"&!screen(new=FALSE)) figaroOn()
        }
        
    # Restore any preexisting plate
    assign("plate",plate,.GlobalEnv)
    assign("plate.data",plate.data,.GlobalEnv)      
    invisible()
}

#Uniform colour of plotting symbols
plateCol<-function(col=NULL){
    on.exit(options("show.error.messages"=TRUE))
    if(length(plate.data)==1) {winDialog(type="ok","No plate defined yet!");return(invisible())}
    if(is.null(col)){
        showColours()
        col<-pickColour() 
        if(is.null(col)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
        dev.set(dev.next())
        dev.off(dev.cur())
    }    
    .plateFig(paste("figCol(\"",col,"\")",sep=""))
    invisible()
}

#All plots set to BW
plateBW<-function(){
    if(length(plate.data)==1) {winDialog(type="ok","No plate defined yet!");return(invisible())}
    .plateFig("figBw()")
    invisible()
}

#Uniform plotting symbols
platePch<-function(pch=NULL){
    on.exit(options("show.error.messages"=TRUE))
    if(length(plate.data)==1) {winDialog(type="ok","No plate defined yet!");return(invisible())}
    if(is.null(pch)){
        showSymbols()
        pch<-winDialogString("Select symbol","o")
        if(is.null(pch)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
        dev.off(dev.cur())
    }
    ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    if(ee=="spider " | ee=="profile "){
        if(is.na(as.numeric(pch))){ 
            .plateFig(paste("figReplace(\"points\",\"pch\",\"",pch,"\",exact=FALSE);figRedraw()",sep=""))
        }else{
            .plateFig(paste("figReplace(\"points\",\"pch\",",pch,",exact=FALSE);figRedraw()",sep="")) 
        }
    }else{
        if(is.na(as.numeric(pch))){     
            .plateFig(paste("figUser(\"pch=\\\"",pch,"\\\"\")",sep=""))
        }else{
            .plateFig(paste("figUser(\"pch=",pch,"\")",sep="")) 
        }
    }
    #plateRedraw()
    invisible()
}

#Uniform x axis for all plots
plateXLim<-function(xlim=NULL){
    on.exit(options("show.error.messages"=TRUE))
    if(length(plate.data)==1) {winDialog(type="ok","No plate defined yet!");return(invisible())}
    if(is.null(xlim)){
        xrange<-winDialogString("Enter range for the x axis",paste(sheet$d$c$xlim,collapse=","))
        if(is.null(xrange)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
        xlim<-as.numeric(unlist(strsplit(xrange,",")[[1]]))
    }
    
    if(length(xlim)!=2|any(is.na(xlim))){
        options("show.error.messages" = FALSE)
        winDialog("ok","Invalid range!")
        cat(yrange,"not found.\n")
        stop()
    }
    
    if(any(xlim<=0)& length(grep("x",sheet$d$c$log))!=0){ # not for logarithmic plots
                winDialog(type="ok","Out of bounds for logarithmic plots!")
                options("show.error.messages"=FALSE)
                stop()
    }
    
    lapply(1:length(plate.data),function(i){
        if(is.list(plate[[i]])){   
            screen(i,new=TRUE)
            .loadCurPlotDef()
            sheet$demo$call$xlim<<-xlim
            figRedraw()
            .saveCurPlotDef(i)
        }
    })
    plateRedraw()
    invisible()
}

#Uniform y axis for all plots
plateYLim<-function(ylim=NULL){
    on.exit(options("show.error.messages"=TRUE))
    if(length(plate.data)==1) {winDialog(type="ok","No plate defined yet!");return(invisible())}
    if(is.null(ylim)){
        yrange<-winDialogString("Enter range for the y axis",paste(sheet$d$c$ylim,collapse=","))
        if(is.null(yrange)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
        ylim<-as.numeric(unlist(strsplit(yrange,",")[[1]]))
    }
    
    if(length(ylim)!=2|any(is.na(ylim))){
        options("show.error.messages" = FALSE)
        winDialog("ok","Invalid range!")
        cat(yrange,"not found.\n")
        stop()
    }

    if(any(ylim<=0)& length(grep("y",sheet$d$c$log))!=0){ # not for logarithmic plots
                winDialog(type="ok","Out of bounds for logarithmic plots!")
                options("show.error.messages"=FALSE)
                stop()
    }
    
    lapply(1:length(plate.data),function(i){ 
        if(is.list(plate[[i]])){ 
            screen(i,new=TRUE)
            .loadCurPlotDef()
            sheet$demo$call$ylim<<-ylim
            figRedraw()
            .saveCurPlotDef(i)
        }
    })
    plateRedraw()
    invisible()
}

#all non-logarithmic y axes start from zero
plate0YLim<-function(){
    on.exit(options("show.error.messages"=TRUE))
    if(length(plate.data)==1) {winDialog(type="ok","No plate defined yet!");return(invisible())}
    
    lapply(1:length(plate.data),function(i){
        if(is.list(plate[[i]])){  
            screen(i,new=TRUE)
            .loadCurPlotDef()
            if(length(grep("y",sheet$d$c$log))==0){ # not for logarithmic plots
                ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
                if(ee!="ternary ")sheet$demo$call$ylim[[1]]<<-0
            }else{
                winDialog(type="ok","Not possible for logarithmic plots!");options("show.error.messages"=FALSE);stop()
            }
            figRedraw()
            .saveCurPlotDef(i)
        }
    })
    plateRedraw()
    invisible()
}

#Uniform size of plotting symbols
plateCex<-function(n=NULL){
     # TODO Error messages of no plotting symbols on spiders   
     on.exit(options("show.error.messages"=TRUE))
     if(length(plate.data)==1) {winDialog(type="ok","No plate defined yet!");return(invisible())}
     if(is.null(n)){
        n<-winDialogString("Scaling for symbols",paste("",sheet$d$c$cex[1],sep=""))
        if(is.null(n)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
        if(n=="NA") n<-NA
     }
     if(!is.na(n)&is.na(as.numeric(n))){
        winDialog(type="ok","Invalid size specification!")
        options("show.error.messages"=FALSE)
        stop("",call. = FALSE)
     }
    if(!is.na(n)){
        n<-as.numeric(n)   
    }
    .plateFig(paste("figCex(",n,")",sep=""))
    #plateRedraw()  
    invisible()
}

#Uniform size of axes labels
plateCexLab<-function(n=NULL){
    on.exit(options("show.error.messages"=TRUE))
    if(length(plate.data)==1) {winDialog(type="ok","No plate defined yet!");return(invisible())}
    if(is.null(n)){
        n<-winDialogString("Scaling for labels","1")
        if(is.null(n)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    }
    
    if(is.na(as.numeric(n))){
                winDialog(type="ok","Invalid size specification!")
                options("show.error.messages"=FALSE)
                stop("",call. = FALSE)
    }
    n<-as.numeric(n)
    options("show.error.messages"=FALSE)
    try(.plateFig(paste("figCexLab(",n,")",sep="")))
     options("show.error.messages"=TRUE)
    plateRedraw()   
    invisible()
}

#Uniform size of main title
plateCexMain<-function(n=NULL){
    on.exit(options("show.error.messages"=TRUE))
    if(length(plate.data)==1) {winDialog(type="ok","No plate defined yet!");return(invisible())}
    if(is.null(n)){
        n<-winDialogString("Scaling for main title","1")
        if(is.null(n)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    }
    
    if(is.na(as.numeric(n))){
                winDialog(type="ok","Invalid size specification!")
                options("show.error.messages"=FALSE)
                stop("",call. = FALSE)
    }
    n<-as.numeric(n)
    .plateFig(paste("figCexMain(",n,")",sep=""))
    plateRedraw()  
    invisible()
}
# Remove annotations in the whole plate
plateAnnotationsRemove<-function(){
    if(length(plate.data)==1) {winDialog(type="ok","No plate defined yet!");return(invisible())}
    .plateFig("figRemove()")  
    invisible()
}

# Setup the plate for "number" of graphs, split screen, get ready the list for the individual plot descriptions
.plateSetup<-function(number,nrow=NULL,ncol=NULL,title=NULL,new=TRUE,device="windows",filename=NULL,colormodel="rgb"){  
    if(new){
        .plateOff()
        if(is.null(nrow)){
            ncol<-n2mfrow(number)[1]
            nrow<-n2mfrow(number)[2]
        }
    }else{
        if(is.null(nrow))nrow<-plate$nrow
        if(is.null(ncol))ncol<-plate$ncol
        if(is.null(title))title<-plate$title
    }
    pointsize<-7
    if(ncol>6) pointsize<-6

    if(device=="windows"){ 
        tit<-gsub("(list)([(])([-a-zA-Z0-9,#*/+]{1,})([)])","\\3",title)
        windows(width = 4.5*ncol, height = 4*nrow+2, pointsize = pointsize,title=tit)
     }
     
     if(device=="windows"){    
        par(oma=c(0,0,4,0))
        #typ<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
        #if(typ=="spider ") par(mar=c(5,4,4,1)) else {par(mar=c(4.5,5.5,2,1.5));par(pty="s")}
        
        if(!new){
            par(oma=c(0,0,4,0))
            typ<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
            #if(typ=="spider ") par(mar=c(5,4,4,1)) else {par(mar=c(4.5,5.5,2,1.5));par(pty="s")}
            #if(typ=="ternary ") par(mar=c(1,0.5,2,0.5))
            ###if(typ=="ternary ") par(mar=c(1.5,0.5,2,0.5))
            par(pty="s")
        }else{
            par(mar=c(4.5,5.5,2,1.5))
            #par(mar=c(4.5,4,1,1))
            #par(mar=c(6,3.5,2,1.5)) # Ok, its is different?
            par(pty="s")
        }
    }
    
    if(toupper(device)=="SWORD"){    
        par(oma=c(0,0,0,0))
        #typ<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
        #if(typ=="spider ") par(mar=c(5,4,4,1)) else {par(mar=c(4.5,5.5,2,1.5));par(pty="s")}
        
        if(!new){
            #cat("SWORD NOT NEW!\n")
            #par(mar=c(3,4,1,1))
            typ<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
            if(typ=="spider ") par(mar=c(5,4,4,1)) else {par(mar=c(4.5,5.5,2,1.5));par(pty="s")}
            if(typ=="ternary ") par(mar=c(1,0.5,2,0.5));par(pty="s")
            ###if(typ=="ternary ") par(mar=c(1.5,0.5,2,0.5))
            par(pty="s")
        }else{
            #cat("SWORD NEW!\n")
            par(mar=c(3,4,1,1))
            #par(mar=c(4.5,4,1,1))
            #par(mar=c(6,3.5,2,1.5)) # Ok, its is different?
            par(pty="s")
        }
    } 
    
    if(device=="postscript"){
        pointsize=10
        postscript(pointsize = pointsize, horizontal=FALSE, file=filename, paper="a4", colormodel=colormodel, useKerning=FALSE)
        #postscript(width = 4.5*ncol, height = 4*nrow+2, pointsize = pointsize, horizontal=FALSE, file=filename, paper="special", colormodel=colormodel)
        par(oma=c(0,0,0,0))
        par(mar=c(3,4,1,1))
        par(pty="s")
    }
    close.screen(all = TRUE)
    split.screen(c(nrow,ncol))
    number<-ncol*nrow #NEEEEEEEW
    if(new){
        z<-as.list(1:number)
        names(z)<-paste("Fig",1:number,sep="")
        z$nrow<-nrow
        z$ncol<-ncol
        z$title<-title
        z$slot.labels<-""

        #z$xdata<-x.data
        #z$ydata<-y.data
        #z$sheet<-sheet
        return(z)
    }else{
        if(is.null(plate$slot.labels)) plate$slot.labels<<-""
        invisible()
    }
}

.reRunPlate<-function(){
    .saveCurPlotDef(scr.old)
    screen(scr.old,new=FALSE)
    Plate()
}

.PlateIntro<-function(){
    on.exit(options("show.error.messages"=TRUE))
    if(is.null(plate$title)) plate$title<<-""
    if(plate$title!=""){
        plate$title<<-""
        #mtext(text= paste(rep("X",each=10),collapse=""),side=3,line=0.25,outer=TRUE,cex=1.5)
        #plateRedraw()
    }
    plateRedraw()
    Plate()
}

.introMenu<-function(){
    on.exit(options("show.error.messages"=TRUE))
    plots<-c("Binary plot","Ternary plot","Spiderplot","Classification diagram","Load a saved graph")
    what<-select.list(plots, preselect = NULL, multiple = FALSE, title = "Introduce plot")
    pick<-which(plots==what)
    if(length(pick)==0){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    switch(pick,
        {binary(new=FALSE);.PlateIntro()},
        {ternary(new=FALSE);.PlateIntro()},
        {spider.individual(new=FALSE);.PlateIntro()},
        {.switch.classification("plate");.PlateIntro()},
        {figLoad();Plate()}
    )
}

.editPlotMenu<-function(){
    on.exit(options("show.error.messages"=TRUE))
    ee<-c("Redraw/Reset","Expand","Edit...","Colours...","Text/Symbols size...","Text annotations off","Add...","Scale axes...","Select samples","Classify")
    what<-select.list(ee, preselect = NULL, multiple = FALSE, title = "Introduce plot")
    pick<-which(ee==what)
    if(length(pick)==0){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    switch(pick,
        {figRedraw();Plate()},
        plateExpand(),
        {figEdit();.reRunPlate()},
        {figColours();.reRunPlate()},
        {figScale();.reRunPlate()},
        {figRemove();.reRunPlate()},
        {figAdd();.reRunPlate()},
        {.figZooming.plate();.reRunPlate()},
        {figSelectSamples();.reRunPlate()},
        {classify(grp = FALSE, labs = TRUE, source.sheet = FALSE);.reRunPlate()}
    )
}

    #winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"User defined parameter","figUser();.reRunPlate()")
    #winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Identify points","figIdentify();.reRunPlate()")
    #winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Highlight multiple points","highlightSelection();.reRunPlate()")
 
.editPlateMenu<-function(){
    on.exit(options("show.error.messages"=TRUE))
    # Disable Plot editing menu for dummy plots  
    if(any(x.data!=1)|length(x)>1){ 

    ee<-c("Redraw/Reset","Expand","Edit...","Colours...","Text/Symbols size...","Text annotations off","Add...","Scale axes...","Select samples","Classify")
    what<-select.list(ee, preselect = NULL, multiple = FALSE, title = "Introduce plot")
    pick<-which(ee==what)
    if(length(pick)==0){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    switch(pick,
        {figRedraw();Plate()},
        plateExpand(),
        {figEdit();.reRunPlate()},
        {figColours();.reRunPlate()},
        {figScale();.reRunPlate()},
        {figRemove();.reRunPlate()},
        {figAdd();.reRunPlate()},
        {.figZooming.plate();.reRunPlate()},
        {figSelectSamples();.reRunPlate()},
        {classify(grp = FALSE, labs = TRUE, source.sheet = FALSE);.reRunPlate()}
    )
    }else{
        winDialog(type="ok","Plot editing not available for dummy plots!")
        cat("\n");print(ee);options(show.error.messages=FALSE);stop()
    }
} 

    
.menuPopUp<-function(){
    on.exit(options("show.error.messages"=TRUE))
    if(.Platform$OS.type!="windows"|.Platform$GUI!="Rgui"){
        #cat("menuPopUp: Menus available only on Windows RGUI!\n")
        return() # Not Windows GUI, quit
    }
    if(length(plate.data)==1) {winDialog(type="ok","No plate defined yet!");return(invisible())}
    n<-dev.cur()
    options(show.error.messages=FALSE)
    i<-paste("$Graph",1:n,"Popup",sep="")
    lapply(i,function(g){
        try(winMenuDel(g))
    })
    
    options(show.error.messages=TRUE)
    if(!any(winMenuNames()==paste("$Graph",n,"Popup/GCDkit Plate",sep=""))){
        #Introduce plot
            winMenuAdd(paste("$Graph",n,"Popup/GCDkit Plate",sep=""))        
            winMenuAddItem(paste("$Graph",n,"Popup/GCDkit Plate",sep=""),"Select slot","Plate()")
            winMenuAddItem(paste("$Graph",n,"Popup/GCDkit Plate",sep=""),"Introduce plot",".introMenu()")
    }
        #winMenuAddItem(paste("$Graph",n,"Popup/GCDkit Plate",sep=""),"Plot editing",".editPlotMenu()")
       
        #Plot editing
        winMenuAdd(paste("$Graph",n,"Popup/Plot editing",sep=""))
            winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Redraw/Reset","figRedraw();Plate()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Expand","plateExpand()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"-----------------------------------      ",".reRunPlate()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Edit...","figEdit();.reRunPlate()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Colours...","figColours();.reRunPlate()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Text/Symbols size...","figScale();.reRunPlate()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Text annotations off","figRemove();.reRunPlate()")
            #winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"User defined parameter","figUser();.reRunPlate()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"-----------------------------------  ",".reRunPlate()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Add...","figAdd();.reRunPlate()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Scale axes...",".figZooming.plate();.reRunPlate()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"------------------------------------",".reRunPlate()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Select samples","figSelectSamples();.reRunPlate()")
            #winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Identify points","figIdentify();.reRunPlate()")
            #winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Highlight multiple points","highlightSelection();.reRunPlate()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Classify","classify(grp = FALSE, labs = TRUE, source.sheet = FALSE);.reRunPlate()")
         
        #Plate editing
        winMenuAdd(paste("$Graph",n,"Popup/Plate editing",sep=""))
            winMenuAddItem(paste("$Graph",n,"Popup/Plate editing",sep=""),"Redraw plate","plateRedraw()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plate editing",sep=""),"Add minerals","plateAddReservoirs(var.name=\"idealmins.data\")")
            winMenuAddItem(paste("$Graph",n,"Popup/Plate editing",sep=""),"Add reservoirs","plateAddReservoirs()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plate editing",sep=""),"Export to PS (colour)","platePS(\"rgb\")")
            winMenuAddItem(paste("$Graph",n,"Popup/Plate editing",sep=""),"Export to PS (black & white)","platePS(\"gray\")")
            winMenuAddItem(paste("$Graph",n,"Popup/Plate editing",sep=""),"Save plate","plateSave()")
            #Plotting symbols or colours
            winMenuAdd(paste("$Graph",n,"Popup/Plate editing/Plotting symbols or colours",sep=""))
            winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Plotting symbols or colours",sep=""),"Uniform plotting symbols","platePch()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Plotting symbols or colours",sep=""),"Uniform plotting colours","plateCol()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Plotting symbols or colours",sep=""),"Set to BW","plateBW()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Plotting symbols or colours",sep=""),"Plotting symbols size","plateCex()")
            #Axes
            winMenuAdd(paste("$Graph",n,"Popup/Plate editing/Axes",sep=""))
            winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Axes",sep=""),"[Common] scaling of x axes","plateXLim()")
            winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Axes",sep=""),"[Common] scaling of y axes","plateYLim()")
           
            # Test whether all plotted variables (x or y) are the same and thus can be changed
            if(.allTheSame("xlab")){
                winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Axes",sep=""),"[Common] scaling of x axes","enable")
            }else{
                winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Axes",sep=""),"[Common] scaling of x axes","disable")
            }
            
            if(.allTheSame("ylab")){
                winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Axes",sep=""),"[Common] scaling of y axes","enable")
            }else{
                winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Axes",sep=""),"[Common] scaling of y axes","disable")
            }
        
            winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Axes",sep=""),"All y axes start from 0","plate0YLim()")
            # Set popisky cisel na osach i popisky os
            winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Axes",sep=""),"Axis labels text size","plateCexLab()") 
            winMenuAddItem(paste("$Graph",n,"Popup/Plate editing",sep=""),"Label the slots by letters or numbers",".plateLabelSlotsI()")           
            winMenuAddItem(paste("$Graph",n,"Popup/Plate editing",sep=""),"Text annotations off","plateAnnotationsRemove()")
            if(length(grep("y",sheet$d$c$log))!=0)  winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Axes",sep=""),"All y axes start from 0","disable")    
 figaroOff()
} 

# Fills the predefined screen by plots
multiplePerPage<-function(which=NULL,nrow=NULL,ncol=NULL,title="Plate",dummy=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    .plateOff()
    sheet.bak<-sheet
    x.data.bak<-x.data
    y.data.bak<-y.data
    if(is.null(which)){
        which<-as.numeric(winDialogString("How many plots should the plate consist of?","6"))
        if(is.null(which)){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
    }
    
    if(is.numeric(which)){
            number<-which
            which<-NULL
    }else{
            number=length(which)
    }
            
    if(is.null(nrow)){
            ncol<-n2mfrow(number)[1]
            nrow<-n2mfrow(number)[2]
            repeat{
                ee<-winDialogString(paste("Number of rows, columns for",number,"plots"),paste(nrow,ncol,sep=","))
                if(is.null(ee)){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
                mat<-as.numeric(unlist(strsplit(ee,",")))
                if(all(!is.na(mat))){
                    nrow<-mat[1]
                    ncol<-mat[2]
                    if(nrow*ncol>=number)break
                }
            }
   }
    
    number<-nrow*ncol # NEEEEEW
    plate<-.plateSetup(number,nrow,ncol,title=title)
    #print(names(plate))
    plate.data<-as.list(1:number)
    plate.data<-lapply(1:number,function(i){
        plate.data[[i]]<-list(x=1,y=1)
    })
    names(plate.data)<-paste("Fig",1:number,sep="")
    assign("plate",plate,.GlobalEnv)
    assign("plate.data",plate.data,.GlobalEnv)
     
     #if(!new){
        #    typ<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
        #    if(typ=="spider ") par(mar=c(5,4,4,1)) else {par(mar=c(4.5,5.5,2,1.5));par(pty="s")}
        #    if(typ=="ternary ") par(mar=c(1.5,0.5,2,0.5))
        #}
        
    # Title for the whole plate
    par(oma=c(0,0,4,0))
    mtext(text=annotate(plate$title),side=3,line=0.25,outer=TRUE,cex=1.5)
    
    ee<-lapply(1:number,function(i){     
        screen(i,new=FALSE)
        # New
        if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
            par(mar=c(4.5,5.5,2,1.5))
            par(pty="s")
        }else{
            par(mar=c(2,0.5,1,1))
            par(pty="s")
        }
        
        
        if(!is.null(which)){
            ee<-length(grep("\\(",which[i]))
            if(ee==0){
                plotDiagram(which[i],FALSE,FALSE)
            }else{
                eval(parse(text=which[i]))
            }
        .saveCurPlotDef(i) 
        }else{ # Dummy empty plots
            if(dummy){
                template<-list(
                    lines1=list("box",which="plot",col="gray",lwd=3),
                    lines2=list("lines", x=c(-5,5), y=c(-5,5), lwd=2, col="gray")
                )
                x.data<-1
                names(x.data)<-"x"
                y.data<-1
                names(y.data)<-"x"
                
                sheet<-list(demo=list(fun="plot",call=list(axes=FALSE,type="n",new=FALSE,xlab="",ylab="",bg="white",pch=1,col=1),template=template))
                ee<-sheet$demo$call
                pp<-figaro(demo,prefix="sheet")
                sheet$demo$call<-ee
                
                assign("sheet",sheet,.GlobalEnv)
                assign("x.data",x.data,.GlobalEnv)
                assign("y.data",y.data,.GlobalEnv)
                sheet.bak<-sheet
                x.data.bak<-x.data
                y.data.bak<-y.data
                pp<-figaro(demo,prefix="sheet")
                pp$draw(x.data,y.data,main="",sub="",xlab="",ylab="")
                .saveCurPlotDef(i) 
            }
            
        }
   
            #if(is.null(which)) # Save the plot in either case
    })
    assign("sheet",sheet.bak,.GlobalEnv)
    assign("x.data",x.data.bak,.GlobalEnv)
    assign("y.data",y.data.bak,.GlobalEnv)
    assign("scr.old",1,.GlobalEnv)
    screen(1,new=FALSE)
    
    #figaroOff()
    #plateRedraw() # Newly removed
    
    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui") .menuPopUp()
    screen(1,new=FALSE)
}

.coord2scr<-function(coord){
    xx<-floor(coord[1]*plate$ncol+1)
    yy<-floor(coord[2]*plate$nrow+1)
    scr<-xx+plate$ncol*(plate$nrow-yy)
    return(scr)
}

.graphCursor<-function(which.screen,state="off"){
    if(state=="off")col<-"white" 
    if(state=="on") col<-"red" 
    if(state=="idle") col<-"khaki"
    screen(which.screen,new=FALSE)
    xmin<-0
    xmax<-1
    ymin<-0
    ymax<-1
    par(usr=c(0,1,0,1))
    if(par("xlog")) xmin<-1;xmax<-10
    if(par("ylog")) ymin<-1;ymax<-10
    rect(xmin,ymin,xmax,ymax,lty="blank")
    box(which="figure",col=col,lwd=4)
}

.graphCursorOff<-function(){
    #.graphCursor(scr.old,"off")
}

.graphCursorOn<-function(scr){
    .graphCursor(scr,"on")
}

.graphCursorIdle<-function(scr){
    .graphCursor(scr,"idle")
}

.graphCursorMove<-function(scr){
    on.exit(options("show.error.messages"=TRUE))
    .loadCurPlotDef()
    .graphCursor(scr.old,"off")
    .loadCurPlotDef()
    .graphCursor(scr,"on")
    assign("scr.old",scr,.GlobalEnv)
    invisible()  
}

# if buttons negative, it corresponds to a direct specification of the scr number  
.mousedownPlate <- function(buttons, x, y, interactive=TRUE) {
        on.exit(options("show.error.messages"=TRUE))
        if(buttons<0){
            scr<--buttons
            if(scr>length(plate.data)|!is.numeric(scr)) {
                winDialog(type="ok","Invalid screen specification!")
                return(NULL) 
            }
            screen(scr,new=FALSE)
        }else{
            if(buttons>3) scr<-screen() else scr<-.coord2scr(c(x,y))
            if(scr>length(plate.data)) return(NULL) 
            if(interactive).graphCursorOff()
        }
        .loadCurPlotDef()
        options("show.error.messages"=FALSE)
        try(sheet$demo$call$bg<<-"white")
        n<-dev.cur()
        typ<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
        if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
            if(typ!="spider "){            
                # Disable axes scaling for ternary plots
                ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
            
                if(ee=="ternary ") winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Scale axes...","disable")
            
                # Here comes the test whether all coordinates are the same
                if(.allTheSame("xlab")){
                    winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Axes",sep=""),"[Common] scaling of x axes","enable")
                }else{
                    winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Axes",sep=""),"[Common] scaling of x axes","disable")
                }
            
                if(.allTheSame("ylab")){
                    winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Axes",sep=""),"[Common] scaling of y axes","enable")
                }else{
                    winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Axes",sep=""),"[Common] scaling of y axes","disable")
                } 
            }else{
                # Some items are to be disabled for the spider plots
                winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Add...","disable") 
                winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Classify","disable") 
                winMenuAddItem(paste("$Graph",n,"Popup/Plot editing",sep=""),"Text annotations off","disable")
                winMenuAddItem(paste("$Graph",n,"Popup/Plate editing/Plate",sep=""),"[Common] scaling of x axes","disable") 
            }
        }
        assign("scr.old",scr,.GlobalEnv)
        screen(scr,new=FALSE)
        .loadCurPlotDef()
        #bringToTop(which = dev.cur())
        if(interactive) .graphCursorIdle(scr)
        invisible(scr)
}
    
.mousemovePlate <- function(buttons, x, y) {
         scr<-.coord2scr(c(x,y))
         if(scr!=scr.old & scr<=length(plate.data)){ #
            .graphCursorMove(scr)
         }
        NULL
}

.keyPlate <- function(key) {
        scr<-screen()
        scr.old<<-scr
        switch(key,
            "Left"={
                if(screen()>1){
                    scr<-scr-1 
                }else{
                    scr<-length(plate.data)
                }
            },
            "Right"={
                if(scr<length(plate.data)){
                    scr<-scr+1 
                }else{
                    scr<-1
                }
            },
            " "={
                if(scr<length(plate.data)){
                    scr<-scr+1 
                }else{
                    scr<-1
                }
            },
            "Up"={
                if(floor((scr-1)/plate$ncol)>0){
                    scr<-scr-plate$ncol 
                }
            },
            "Down"={
                if(floor((scr-1)/plate$ncol)<plate$nrow-1){
                    if(scr+plate$ncol<=length(plate.data)) scr<-scr+plate$ncol 
                }
            },
            "ctrl-J"={
                scr<-.mousedownPlate(4,0,0)
                return(scr)
            }
        )
        .graphCursorMove(scr)
        NULL
}

Plate<-function(scr=NULL){
    if(length(plate.data)==1|is.null(dev.list())) {winDialog(type="ok","No plate defined yet!");return(invisible())}
    interactive<-is.null(scr)
    if(interactive){
        bringToTop(which = dev.cur(),stay=TRUE)
        .graphCursorOn(scr.old)
        savepar <- par(ask=FALSE)
        setGraphicsEventEnv(which =  dev.cur(), .GlobalEnv)
        setGraphicsEventHandlers(prompt="Select the diagram to edit",consolePrompt = "Use mouse or arrows/Enter key",onMouseDown = .mousedownPlate, onMouseMove = .mousemovePlate,onMouseUp = NULL, onKeybd = .keyPlate)
        ee<-getGraphicsEvent()
        par(savepar)
        setGraphicsEventHandlers(prompt="",consolePrompt = "",onMouseDown = NULL, onMouseMove = NULL,onMouseUp = NULL, onKeybd = NULL)
        #ee<-getGraphicsEvent()
        bringToTop(which = dev.cur(),stay=FALSE)
    }else{
        .mousedownPlate(-scr,NULL,NULL,interactive=interactive)
    }
    invisible()
}

#############################################################################
#               Draws a plate of diagrams from Figaro sheets                #
#############################################################################
.dummyPlot<-function(){
     plot(0,0,type="n",axes=FALSE,xlim=c(-1,1),ylim=c(-1,1),xlab=sheet$demo$call$xlab,ylab=sheet$demo$call$ylab,cex.lab=sheet$demo$call$cex.lab)
     box("plot",col="gray",lwd=3)
     lines(x=c(-5,5), y=c(-5,5), lwd=2, col="gray")
}

plotPlate<-function(diagram,where="WR",...){    
    on.exit(options("show.error.messages"=TRUE))
    args<-list(...)
    
    figaroOff()
    .plateOff()
    #par(oma=c(1,1,1,1))
    #par(mar=c(4.5,5.5,2,1.5))
    
     if(length(ls(pattern=paste("^",diagram,"$",sep=""),envir=.GlobalEnv))==0){
        options(show.error.messages=FALSE)
        winDialog(type="ok","Invalid diagram name!")
        stop()
    }
    
    xx<-try(eval(do.call(diagram,args)))
    if (class(xx) == "try-error") {
        options("warn"=-1)
        options(show.error.messages=FALSE)
        stop()
    }
    
    number<-length(plate.data)
    .plateSetup(number,nrow=plate$nrow,ncol=plate$ncol,title=plate$title,new=FALSE)

    ee<-lapply(1:length(plate.data),function(i){
        screen(i,new=TRUE)
        .loadCurPlotDef()
        
        if(is.null(unlist(sheet$demo$call$cex.main))) { 
            sheet$demo$call$cex.main<<-par("cex.main")
        }
        
        if(is.null(unlist(sheet$demo$call$cex.lab))) { 
            sheet$demo$call$cex.main<<-par("cex.lab")
        }
        
        # Calculate x data
        x.formula<<-x.data
        if (is.character(x.formula)) {
            x.data<-calcCore(x.formula,where=where,redo=FALSE)$results
        }
        
        if(all(is.na(x.data))){
            figRedraw()
            #.dummyPlot()
            winDialog(type="ok",paste("Error in x-coordinate!",x.formula))
            sheet$demo$template<<-c(sheet$demo$template,list(lines10000=list("lines",x=sheet$demo$call$xlim,y=sheet$demo$call$ylim,col="red",lwd=2)))
            pp<<-figaro(demo,prefix="sheet")
            pp$draw(1,1,main=sheet$demo$call$main,xlab=sheet$demo$call$xlab,ylab=sheet$demo$call$ylab,type="n")
            .saveCurPlotDef()
            return()
        }
        
        # Calculate y data
        y.formula<<-y.data
        if (is.character(y.formula)) {
            y.data<-calcCore(y.formula,where=where,redo=FALSE)$results
        }
        
        if(all(is.na(y.data))){
            .dummyPlot()
            winDialog(type="ok",paste("Error in y-coordinate!",y.formula))
            sheet$demo$template<<-c(sheet$demo$template,list(lines10000=list("lines",x=sheet$demo$call$xlim,y=sheet$demo$call$ylim,col="red",lwd=2)))
            pp<<-figaro(demo,prefix="sheet")
            pp$draw(1,1,main=sheet$demo$call$main,xlab=sheet$demo$call$xlab,ylab=sheet$demo$call$ylab,type="n")
            .saveCurPlotDef()
            return()
        }
        assign("x.data",x.data,.GlobalEnv)
        assign("y.data",y.data,.GlobalEnv)
        .saveCurPlotDef()
        figRedraw()
    })
    assign("scr.old",1,.GlobalEnv)
    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui") .menuPopUp()
    par(oma=c(0,0,4,0))
    mtext(text=annotate(plate$title),side=3,line=0.25,outer=TRUE,cex=1.5)
    invisible()
    #Plate()
}


plateLabelSlots<-function(text="letters",style="()",cex=1.5,pos="topright"){
    if(length(text)==1){
        text<-switch(text,
            letters=letters,
            LETTERS=LETTERS,
            numbers=as.character(1:length(plate.data)),
            ROMAN=as.roman(1:length(plate.data)),
            roman=tolower(as.roman(1:length(plate.data))),
            NULL
        )
        if(is.null(text)){winDialog(type="ok","Invalid label text!")
             options(show.error.messages=FALSE);stop(call. = TRUE)
        }
    }
    brackets<-c(substr(style,1,1),substr(style,2,2))
    labs<-paste(brackets[1],text,brackets[2],sep="")[1:length(plate.data)]

    possible.pos<-c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center")
 
    if(!(pos%in%possible.pos)){winDialog(type="ok","Invalid label position!")
             options(show.error.messages=FALSE);stop(call. = TRUE)
    }

    if((length(grep("right",pos)))>0){
        adj<-0
    }else{
        adj<-1
    }
    
    ee<-list()
    ee$labs<-labs
    ee$pos<-pos
    ee$cex<-cex
    ee$adj<-adj
    plate$slot.labels<<-ee
    
    # NEW: Better not to redraw plate
    lapply(1:length(labs),function(i){
        .getSlot(i)
        if(any(names(plate[[i]])=="demo"))legend(pos,labs[i],bty="n",cex=cex,inset=0,adj=adj) # Otherwise is empty
    })
    if(.Platform$GUI!="Rgui") plateRedraw()
    invisible()
}
         
plateAddReservoirs<-function(autoscale=FALSE, var.name="reservoirs.data", sample.names=NULL, reserv.condition=NULL,labs=NULL, pch="*", col="darkblue", cex=1, type="p",just.draw=FALSE,...){

    #mat<-switch(var.name,
    #        reservoirs.data=.figAddReservoirsLoad(),
    #        idealmins.data=.figAddReservoirsMinsLoad(),    
    #        debon.ideal.data=.figAddReservoirsDebonLoad(),
    #        get(var.name,.GlobalEnv) # else, i.e. a global variable name was specified
    #)
    #if(is.null(sample.names)&is.null(reserv.condition)){
    #    sample.names<-rownames(mat) # All samples selected
        #sample.names<-select.list(rownames(mat),multiple=TRUE)
        #if(length(sample.names)==0){cat("Cancelled.\n");return()}
    #}
    out<-lapply(1:length(plate.data),function(i){
        if(!getOption("gcd.shut.up"))cat("\nSlot ",i,":\n",sep="")
        z<-.getSlot(i,FALSE) # Returns sheet, otherwise FALSE if the slot is empty
        if(!is.logical(z)){ # NEW
            z<-figAddReservoirs(autoscale=autoscale,var.name=var.name,sample.names=sample.names,reserv.condition=reserv.condition,labs=labs,pch=pch,col=col,cex=cex,type=type,...)
            if(!getOption("gcd.shut.up"))print(z)
            if(plate$slot.labels!="") legend(plate$slot.labels$pos,plate$slot.labels$labs[i],bty="n",cex=plate$slot.labels$cex,inset=0,adj=plate$slot.labels$adj)
            #if(plate$slot.labels!="") legend(plate$slot.labels$pos,plate$slot.labels$labs[i],bty="n",cex=plate$slot.labels$cex,inset=0,adj=plate$slot.labels$adj)
            if(!just.draw) .saveCurPlotDef()
        }
        #if(!is.null(plate$slot.labels)) legend(plate$slot.labels$pos,plate$slot.labels$labs[i],bty="n",cex=plate$slot.labels$cex,inset=0,adj=plate$slot.labels$adj)
        return(z)
    })
    names(out)<-paste("Slot",1:length(out))
    assign("results",out,.GlobalEnv)
    invisible(out) 
}
