# Put a new dataset into WRCube, save all the variables into the par.list
pokeDataset<-function(which.dataset=NULL,par.list="WR,WRanh,milli,labels,filename,groups,grouping,init,age",overwrite.warn=TRUE){ 
     on.exit(options("show.error.messages"=TRUE))
     if(package.name=="GCDkit.Mineral"){
        par.list=paste(par.list,",min",sep="")
     }
     # Dataset name not provided upon call, select from the list of already stored ones
     if(is.null(which.dataset)){
        xx<-winDialogString("Enter dataset name","")
        if(is.null(xx)){
            cat("Cancelled.\n")
            options("show.error.messages"=FALSE)
            stop(call. = TRUE)
        }
        if(xx==""){
            xx<-select.list(names(WRCube),preselect=dataset.name)
            if(is.null(xx)){
                cat("Cancelled.\n")
                options("show.error.messages"=FALSE);stop(call. = TRUE)
            }
        }
        which.dataset<-xx
     }     
     
     # Get the original WR Cube from the global environment
     assign("out",get("WRCube",.GlobalEnv))
     
     # Overwrite warning in GUI only
     if(any(names(out)==which.dataset)&overwrite.warn&.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
            x<-winDialog(type="yesno",paste("Dataset \'",which.dataset,"\' does exist! Overwrite?",sep=""))
            if(x!="YES"){
                cat("Cancelled.\n")
                options("show.error.messages"=FALSE)
                stop(call. = TRUE)
            }
     }
     
     # Which global variables are to be saved? Plus check whether they indeed do exist
     options(show.error.messages=FALSE)
     try(min<-get("min",.GlobalEnv))
     vars<-unlist(strsplit(par.list,","))
     i<-vars%in%ls(envir=.GlobalEnv)
     #if(!all(i)){
     #   winDialog(type="ok",paste("Variable(s) \'",paste(vars[!i],collapse=", "),"\' not found! ",sep=""))
     #   options(show.error.messages=FALSE)
     #   stop(call. = TRUE)
     #}
     
     # Store all of them
     z<-vector("list",0)
     z<-sapply(vars,function(i){
        options(show.error.messages=FALSE)
        #try(z<-eval(parse(text=i)))
        try(z<-get(i,.GlobalEnv)) # NEW
        return(z)
     })
     names(z)<-vars
     
     out[[which.dataset]]<-z
     assign("WRCube",out,.GlobalEnv)
     
     if(!getOption("gcd.shut.up")){
        cat("Available datasets:\n")
        print(names(out))
     }
     
     if(getOption("gcd.menus")!=""){
        #if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
            winMenuAddItem<-get("winMenuAddItem",.GlobalEnv)
        if(package.name=="GCDkitDevelop"){
            winMenuAddItem("GCDkit","Switch/restore a dataset","enable")
            winMenuAddItem("GCDkit","Purge stored datasets","enable")
        }else{
            winMenuAddItem(package.name,"Switch/restore a dataset","enable")
            winMenuAddItem(package.name,"Purge stored datasets","enable")
        }
     }
     if(getOption("gcd.menus")!=""){
        #if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
            winMenuAddItem<-get("winMenuAddItem",.GlobalEnv)
        if(length(WRCube)==1){
            winMenuAddItem("GCDkit","Overplot another dataset","disable")
            winMenuAddItem("GCDkit","Underplot another dataset","disable")
            winMenuAddItem("GCDkit","Switch/restore a dataset","disable")
        }else{
            #if(length(ls(pattern="^pp$",env=.GlobalEnv))==1){
                winMenuAddItem("GCDkit","Overplot another dataset","enable")
                winMenuAddItem("GCDkit","Underplot another dataset","enable")
            #}
            winMenuAddItem("GCDkit","Switch/restore a dataset","enable")
            # dev.list() is not NULL
        }
     }
     invisible() 
}

# Retrieve an old dataset from WRCube, read all the variables in the par.list
peekDataset<-function(which.dataset=NULL){
    on.exit(options("show.error.messages"=TRUE))
    
    #  Dataset name not provided upon call, select from the already stored ones
    if(is.null(which.dataset)){
        selectDataset()
        options("show.error.messages"=FALSE)
        stop(call. = TRUE)
    }
    
    # Get the original WR Cube from the global environment
    assign("cube",get("WRCube",.GlobalEnv))
    
    # Dataset specified by sequence number
    if(is.numeric(which.dataset)){
        if(which.dataset>0&which.dataset<=length(WRCube)){
            which.dataset<-names(cube)[which.dataset]
        }else{
            print(names(cube))
            if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
                winDialog(type="ok",paste("Dataset \'",which.dataset,"\' does not exist! ",sep=""))
            }else{
                cat("Dataset",which.dataset,"does not exist. Quitting.\n")
                options("show.error.messages"=FALSE);stop(call. = TRUE)
            }
        }
    }
    
    
    # Dataset name does not exist
    if(!(which.dataset%in%names(cube))){
        print(names(cube))
        if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
            winDialog(type="ok",paste("Dataset \'",which.dataset,"\' does not exist! ",sep=""))
        }else{
            cat("Dataset",which.dataset,"does not exist. Quitting.\n")
            options("show.error.messages"=FALSE);stop(call. = TRUE)
        }
        options(show.error.messages=FALSE)
        stop(call. = TRUE)
    }
    
    # GO!
    vars<-names(cube[[which.dataset]])
    out<-sapply(vars,function(i){
        #z<-eval(parse(text=i))
        z<-eval(parse(text=paste("assign(\"",i,"\",cube[[\"",which.dataset,"\"]]$",i,",.GlobalEnv)",sep="")))
        return(z)
    })
    names(out)<-vars
    .assignWithNamespaceHard("labels",cube[[which.dataset]]$labels)
    if(!getOption("gcd.shut.up"))cat("Current dataset: ",which.dataset,"\n",sep="")
    
    assign("dataset.name",which.dataset,.GlobalEnv)
    assign("results",numeric(0),.GlobalEnv)
    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui")setWindowTitle(paste("//","GCDkit - ",which.dataset,sep=""))
    figaroOff()
    #dev.set(which=1)
    invisible()
}

purgeDatasets<-function(GUI=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    if(!GUI) x<-"YES" # To be deleted without warning in batch mode
    
    assign("cube",get("WRCube",.GlobalEnv))
    
    # No alterantive datasets stored
    if(length(cube)<=1){
        cat("Nothing to purge.\n")
        invisible()
        x<-"NO"
        #options(show.error.messages=FALSE)
        #stop(call. = TRUE)
    }
    
    # Warning in GUI only
    if(GUI){
        x<-winDialog("Purge all the stored datasets\n except the current one?",type="yesno")
    }
    
    if(x=="YES"){
        i<-which(names(cube)==dataset.name)
        which.dataset<-names(cube)[i]
        cube<-cube[i]
        assign("WRCube",cube,.GlobalEnv)
        assign("dataset.name",which.dataset,.GlobalEnv)
        
        if(getOption("gcd.menus")!=""){
        #if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
            winMenuAddItem<-get("winMenuAddItem",.GlobalEnv)
            if(package.name=="GCDkitDevelop"){
                winMenuAddItem("GCDkit","Switch/restore a dataset","disable")
                winMenuAddItem("GCDkit","Purge stored datasets","disable")
            }else{
                winMenuAddItem(package.name,"Switch/restore a dataset","disable")
                winMenuAddItem(package.name,"Purge stored datasets","disable")
            }
        }
        if(!getOption("gcd.shut.up")){
            cat("Available datasets:\n")
            items<-names(get("WRCube",.GlobalEnv))
            print(items)
            cat("Current dataset: ",dataset.name,"\n",sep="")
        }
    }  
    invisible()
}

.pickOtherDataset<-function(){
    on.exit(options("show.error.messages"=TRUE))
    items<-names(get("WRCube",.GlobalEnv))
    if(is.null(items)){
        cat("No datasets available.\n")
        options(show.error.messages=FALSE)
        stop(call. = TRUE)
    }
    if(length(items)==1){
        cat("No alternative datasets available.\n")
        options(show.error.messages=FALSE)
        stop(call. = TRUE)
    }
    
    items<-items[items!=dataset.name]
    i<-select.list(items)
    
    if(i==""){
        cat("Cancelled.\n")
        options(show.error.messages=FALSE)
        stop(call. = TRUE)
    }
    return(i)
}

# Select dataset from list box
selectDataset<-function(){
    on.exit(options("show.error.messages"=TRUE))
    i<-.pickOtherDataset()
    
    peekDataset(i)
    invisible(i)
}

#############################################################################
#              Overplots classification diagram from Figaro sheet           #
#               the background dataset is shown just temporarilly           #
#############################################################################

#figOverplotDiagram<-function(overplot.dataset,bg.dataset=NULL,diagram=NULL,which=NULL,xlim=NULL,ylim=NULL,pch=labels$Symbol,col=labels$Colour,cex=labels$Size,labs=NULL,type="p",lwd=1,lty="solid",transp=0,just.draw=TRUE,...){
figOverplotDiagram<-function(overplot.dataset,bg.dataset=NULL,diagram=NULL,which=NULL,xlim=NULL,ylim=NULL,pch="*",col="darkred",cex=1,labs=NULL,type="p",lwd=1,lty="solid",transp=0,just.draw=TRUE,...){
    on.exit(options("show.error.messages"=TRUE))
    # Options
    shut.bak<-getOption("gcd.shut.up") # Make less verbose temporarilly
    options("gcd.shut.up"=TRUE)
    options(show.error.messages=FALSE)
    
    # Keep the Figaro stuff (sheet with plot data) safe
    if(is.null(sheet$demo$call$pch)) sheet$demo$call$pch<-labels[names(x.data),"Symbol"]
    if(is.null(sheet$demo$call$col)) sheet$demo$call$col<-labels[names(x.data),"Colour"]
    if(is.null(sheet$demo$call$cex)) sheet$demo$call$cex<-labels[names(x.data),"Size"]
    
    sheet.bak<-get("sheet",.GlobalEnv)
    xd<-x.data
    yd<-y.data
    
    # Keep the current dataset safe as @@@temp2
    which.dataset<-dataset.name
    pokeDataset("@@@temp2",overwrite.warn=FALSE)
    
    # Is underplotting rqd?
    under<-FALSE
    
    # If not specified, retrieve the current diagram name
    if(is.null(diagram)){
        if(!is.null(bg.dataset)) under<-TRUE
        diagram<-sheet$demo$call$diagram
    }
    
    # As soon as the name of diagram is provided, check for its existence
    if(!is.null(diagram)){
        if(length(ls(pattern=paste("^",diagram,"$",sep=""),envir=.GlobalEnv))==0){
            winDialog(type="ok","Invalid diagram name!")
            stop()
        }
    }
    
    # Get the list of ... arguments from the call
    args<-list(...)
    
    # Set transparency if no background dataset specified
    if(is.null(bg.dataset) & transp!=0){
        setTransparency(transp=transp)
        sheet$demo$call$col<-labels[names(x.data),"Colour"]
        assign("sheet",sheet,.GlobalEnv)
        figRedraw()
        peekDataset("@@@temp2")
    }

    ###############################################################################################################
    # BACKGROUND dataset (if any)
    if(!is.null(bg.dataset)){
        # Peek the background dataset and make it transparent if necessary
        peekDataset(bg.dataset)
        setTransparency(transp=transp)
        
        # Source the diagram code, calculate x.data, y.data
        if(is.null(which)){ # Standard single Figaro diagram => plotDiagram
                xx<-try(eval(do.call(diagram,args))) 
        }else{              # Extracting diagram from a plate => plateExtract
                xx<-try(plateExtract(diagram,which=which,main=sheet$d$c$main,calc.only=TRUE))
                dev.off(dev.cur())
        }
        # ....Failed
        if (class(xx)=="try-error") {
            msg<-paste("Error in ",diagram,"()! Quitting.\n",sep="")
            cat(msg)
            stop(call. = FALSE)
        }        
        
        if(under){
            sheet<-sheet.bak
        }else{
            sheet<-get("sheet",.GlobalEnv)
        }
 
        # Replace the backup copy of the background data
        xd<-x.data
        yd<-y.data 
        
        i<-screen(new=FALSE)
        if(i){ # if the diagram is a part of a plate...
             bg.bak<-par("bg") # ...then wipe the plotting panel clean
             par(bg="white")
             erase.screen(i)
             par("bg"=bg.bak)
             sheet<-plate[[i]] # and get the correct sheet
        }
        
        iii<-names(x.data) #rownames(WR)
        sheet$demo$call$pch<-labels[iii,"Symbol"]
        sheet$demo$call$col<-labels[iii,"Colour"]   
        sheet$demo$call$cex<-labels[iii,"Size"]
        sheet$demo$call$diagram<-diagram # BRAND NEW
        sheet$demo$call$arguments<-args  # BRAND NEW
        
        scr<-screen(new=FALSE)
        if(!is.null(which)){ # NEW this plate extract    
            sheet$demo$call$plotting.function<-"plateExtract" # BRAND NEW
            assign("sheet",sheet,.GlobalEnv)
            #if(!scr) figRedraw(title=paste(diagram,": ",which,sep="")) # Open a new graphical window
            if(scr) figRedraw() else figRedraw(title=paste(diagram,": ",which,sep=""))
        }else{
            sheet$demo$call$plotting.function<-"plotDiagram" # BRAND NEW
            assign("sheet",sheet,.GlobalEnv)
            #if(!scr) figRedraw(title=paste(diagram,": ",which,sep="")) # Open a new graphical window
            if(scr) figRedraw() else figRedraw(title=sheet$demo$template$GCDkit$plot.name)
        }
        
        # Backup  the underlying Figaro stuff, if plotted with both bg and fg datasets 
        
         if(!under) sheet.bak<-sheet # Zlobil

        # Restore the previous colour (with original transparency or lack thereof)
        peekDataset(bg.dataset)
        get("labels",.GlobalEnv)
        sheet$demo$call$col<-labels[iii,"Colour"]
        assign("sheet",sheet,.GlobalEnv)
        #sheet.bak<-sheet
    }
    
    # Scale the plot if necessary
    if(!is.null(xlim)) figXlim(xlim)
    if(!is.null(ylim)) figYlim(ylim)
    
    ###############################################################################################################
    # FOREGROUND dataset (always there)
    ###############################################################################################################
    # Backup any plate data if they exist (= if plotting on a plate)
    if(screen(new=FALSE)){
        plate.bak<-plate
        plate.data.bak<-plate.data
    }
    
    # Name of a stored dataset, or variable name - the content is taken as is
    if(is.character(overplot.dataset)){ 
        if(any(names(WRCube)==overplot.dataset)){
            peekDataset(overplot.dataset)
        }else{  
            WR<-try(get(overplot.dataset,envir=.GlobalEnv))
            if (class(WR)=="try-error") {
                msg<-paste("Variable not found ",overplot.dataset,"()! Quitting.\n",sep="")
                cat(msg)
                stop(call. = FALSE)
            }
        } 
        
    # Otherwise - GCDkit is called and everything must be calculated
    }else{ 
            sheetd<-sheet
            if(!getOption("gcd.shut.up")) print("Calling GCDkit")
            .loadData.process(overplot.dataset, merging = FALSE, GUI = FALSE, pokeDataset = FALSE)
            sheet<-sheetd
            assign("sheet",sheet,.GlobalEnv)
    }
    
    # Source the diagram code, calculate x.data, y.data
    if(is.null(which)) which<-sheet$demo$call$arguments$which
         
    if(is.null(which)){ # Standard single Figaro diagram => plotDiagram
        xx<-try(eval(do.call(diagram,args))) 
    }else{              # Extracting diagram from a plate => plateExtract
        xx<-try(plateExtract(diagram,which=which,main=sheet$d$c$main,calc.only=TRUE))
    }
        
    # ... failed
    if (class(xx)=="try-error") {
        msg<-paste("Error in ",diagram,"()! Quitting.\n",sep="")
        cat(msg)
        stop(call. = FALSE)
    }
    
    # Setting the plotting parameters
    iii<-which(rownames(WR)%in%names(x.data))
    i<-screen(new=FALSE)
    if(i){ # the diagram can be a part of a plate...
        assign("plate",plate.bak,.GlobalEnv)
        assign("plate.data",plate.data.bak,.GlobalEnv)
        sht<-plate[[i]]$demo$call
    }else{ # or not
        sht<-sheet.bak$demo$call
    }
    
    if(under){
        # NEW
        #pch<-sheet.bak$demo$call$pch
        #col<-sheet.bak$demo$call$col
        #cex<-sheet.bak$demo$call$cex
        if(length(sht$pch)>1) pch<-sht$pch[iii]
        if(length(sht$col)>1) col<-sht$col[iii]
        if(length(sht$cex)>1) cex<-sht$cex[iii]
    }
    
    if(is.null(pch)) pch<-labels[iii,"Symbol"]
    if(is.null(col)) col<-labels[iii,"Colour"]   
    if(is.null(cex)) cex<-labels[iii,"Size"]
    
    # Draw temporarilly on the top
    points(x.data[iii],y.data[iii],pch=pch,col=col,cex=cex,type=type,lwd=lwd,lty=lty)
    
    text(x.data[iii],y.data[iii],labs,cex=cex,col=col,font=2,adj=c(0.5,0),pos=3)
    
    # Restore the underlying Figaro stuff
    assign("sheet",sheet.bak,.GlobalEnv)
    assign("x.data",xd,.GlobalEnv)
    assign("y.data",yd,.GlobalEnv)       
    
    # Restore the original dataset, clean up
    peekDataset("@@@temp2")
    WRCube["@@@temp2"]<<-NULL
    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui") setWindowTitle(paste("//","GCDkit - ",which.dataset,sep="")) # Set the RConsole title, dataset name is to be restored, too
    assign("dataset.name",which.dataset,.GlobalEnv)
    
    # Restore the global options
    options("gcd.shut.up"=shut.bak) # Restore the verbose option
    if(!screen(new=FALSE)) figaroOn()
    invisible()
}



#############################################################################
#                     Universal over- and underplotting                     #
#               the background dataset is shown just temporarilly           #
#############################################################################

overplotDataset<-function(reference.dataset=NULL,underplotting=FALSE,transp=0,pch=NULL,col=NULL,cex=NULL,...){
    on.exit(options("show.error.messages"=TRUE))
    
    shut.bak<-getOption("gcd.shut.up") # Make less verbose temporarilly
    options("gcd.shut.up"=TRUE)
    options("show.error.messages"=FALSE)
    
    # Select a dataset interactively if none specified (GUI)
    if(is.null(reference.dataset)){ 
        reference.dataset<-.pickOtherDataset()
    }
        
    # Keep the data safe
    xd<-x.data
    yd<-y.data
    which.dataset<-dataset.name
    pokeDataset("@@@temp",overwrite.warn=FALSE)
    no.panels<-screen(new=FALSE)
    
    # NEW If reference.dataset contains a global variable name, retrieve its contents 
    if(is.character(reference.dataset)){
            if(all(names(WRCube)!=reference.dataset)){
                ee<-try(get(reference.dataset,envir=.GlobalEnv))
                if (class(ee)=="try-error") {
                    msg<-paste("Variable not found: ",reference.dataset,"! Quitting.\n",sep="")
                    cat(msg)
                    stop(call. = FALSE)
                }
                reference.dataset<-ee
            }    
    }
  
    # If reference dataset is a list - not implemented, unless there is a single item only
    if(is.list(reference.dataset)){
        if(length(reference.dataset)!=1){
                cat("Lists are not implemented so far. Quitting..\n")
                options("show.error.messages"=FALSE)
                stop(call. = TRUE)
        }else{
            reference.dataset<-reference.dataset[[1]]
        }
    }
    
    # If reference.dataset contains a matrix, GCDkit is called and everything calculated
    if(!is.character(reference.dataset)){
        sheetd<-sheet
        .loadData.process(reference.dataset, merging = FALSE, GUI = FALSE, pokeDataset = FALSE)
        sheet<-sheetd
        assign("sheet",sheet,.GlobalEnv)    
        pokeDataset("@@@overplot.dataset",overwrite.warn=FALSE)
        peekDataset("@@@temp")
        reference.dataset<-"@@@overplot.dataset"
    }
    
    # CORE
    if(!screen(new=FALSE)){                     # Not a plate
        if(!is.null(sheet$demo$call$diagram)){  # Single Figaro-compatible template => figOverplotDiagram
            if(!underplotting){                 # Overplotting
                figOverplotDiagram(overplot.dataset=reference.dataset,which=sheet$demo$call$arguments$which,pch=pch,col=col,cex=cex,...)  
            }else{                              # Underplotting
                figOverplotDiagram(overplot.dataset="@@@temp",bg.dataset=reference.dataset,which=sheet$demo$call$arguments$which,transp=transp,...) 
            }
        }else{                  # Not a diagram template => figOverplot
            peekDataset(reference.dataset)
            if(is.null(pch)) pch<-labels$Symbol
            if(is.null(col)) col<-labels$Colour
            if(is.null(cex)) cex<-labels$Size
            if(length(pch)==nrow(WR))names(pch)<-rownames(WR)
            if(length(col)==nrow(WR)) names(col)<-rownames(WR)
            if(length(cex)==1) cex<-rep(cex,nrow(WR))
            if(length(cex)==nrow(WR)) names(cex)<-rownames(WR) 
            
            if(!underplotting){ # Overplotting
                figOverplot(var.name="WR",pch=pch,col=col,cex=cex,just.draw=TRUE,overplotDataset=TRUE,...)
            }else{              # Underplotting
                if(length(col)>1){
                    setTransparency(transp=transp)
                }else{
                    col<-setTransparency(col=col,transp=transp,save=F)
                }
                
                typ<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
               
                # Spiders
                if(typ=="spider "){
                    # Backup the underlying Figaro stuff
                    sheet.bak<-sheet
                    xd<-x.data
                    yd<-y.data
                    
                    #.fig.spider.axes(main=sheet$demo$call$main,sub=sheet$demo$call$sub)
                    chon<-strsplit(.fig.deeval(sheet$demo$call$main)," - ")[[1]][2]
                    dev.off(dev.cur())
                    ee<-spider(WR,chon,pch=pch,col=col,cex=cex,add=FALSE,ymin=sheet$d$c$ylim[1],ymax=sheet$d$c$ylim[2],...)
                    
                    peekDataset("@@@temp")
                    sheet$demo$template<-c(sheet$demo$template,sheet.bak$demo$template)
                    sheet$demo$bg<-"khaki"
                    assign("sheet",sheet,.GlobalEnv)
                    assign("y.data",rbind(y.data,yd),.GlobalEnv)
                    figRedraw()
                    
                    # Restore the underlying Figaro stuff
                    assign("sheet",sheet.bak,.GlobalEnv)
                    assign("x.data",xd,.GlobalEnv)
                    assign("y.data",yd,.GlobalEnv)
                    #spider(WR,chon,add=TRUE)
                #The rest of the World, i.e., single binary, ternary etc.
                }else{
                    cex.bak<-sheet$demo$call$cex
                    figCex(0) # Clean plotting window
                    ee<-figOverplot(var.name="WR",pch=pch,col=col,cex=cex,just.draw=TRUE,overplotDataset=TRUE,...) # First pass plots nothing
                    points(ee[,1],ee[,2],pch=pch,col=col,cex=cex)
                    peekDataset("@@@temp")
                    points(x.data,y.data,pch=sheet$demo$call$pch,col=sheet$demo$call$col,cex=cex.bak)
                    sheet$demo$call$cex<-cex.bak
                    assign("sheet",sheet,.GlobalEnv)
                }
            }
        }
    }else{                  # A plate
            #peekDataset(reference.dataset)
            lapply(1:length(plate.data),function(i){
                .getSlot(i,FALSE) # Get the slot definition
                if(!is.list(plate[[i]][1]))return() # Slot is empty
                
                if(!is.null(sheet$demo$call$diagram)){ # It is a diagram!
                    if(!underplotting){ # Overplotting
                        figOverplotDiagram(overplot.dataset=reference.dataset,which=sheet$demo$call$arguments$which,pch=pch,col=col,cex=cex,...)
                    }else{              # Underploting
                        figOverplotDiagram(overplot.dataset="@@@temp",bg.dataset=reference.dataset,which=sheet$demo$call$arguments$which,transp=transp,...)
                    }
                
                }else{
                    sht<-plate[[i]]$demo$call
                    if(!underplotting){ # Overplotting
                        peekDataset(reference.dataset)
                        # NEW
                        if(is.null(pch)) pch<-labels$Symbol
                        if(is.null(col)) col<-labels$Colour
                        if(is.null(cex)) cex<-labels$Size
                        if(length(pch)==nrow(WR))names(pch)<-rownames(WR)
                        if(length(col)==nrow(WR)) names(col)<-rownames(WR)
                        if(length(cex)==1) cex<-rep(cex,nrow(WR))
                        if(length(cex)==nrow(WR)) names(cex)<-rownames(WR) 
                        # /NEW
                        out<-figOverplot(var.name="WR",pch=pch,col=col,cex=cex,just.draw=TRUE,overplotDataset=TRUE,...)
                    }else{              # Underplotting
                        # Background dataset
                        peekDataset(reference.dataset)
                        if(is.null(pch)) pch<-labels$Symbol
                        if(is.null(col)) col<-labels$Colour
                        if(is.null(cex)) cex<-labels$Size
                        setTransparency(transp=transp)
                        out<-figOverplot(var.name="WR",pch=pch,col=col,cex=cex,just.draw=TRUE,overplotDataset=TRUE,...)
                        xd<-x.data
                        yd<-y.data
                        assign("x.data",out[,1],.GlobalEnv)
                        assign("y.data",out[,2],.GlobalEnv)
                        figRedraw()
                        
                        # Foreground dataset
                        peekDataset("@@@temp")
                        points(xd,yd,pch=sht$pch,col=sht$col,cex=sht$cex)
                        assign("x.data",xd,.GlobalEnv)
                        assign("y.data",yd,.GlobalEnv)
                    }
                }
                #if(plate$slot.labels!="") legend(plate$slot.labels$pos,plate$slot.labels$labs[i],bty="n",cex=plate$slot.labels$cex,inset=0,adj=plate$slot.labels$adj)
                if(plate$slot.labels!="") legend(plate$slot.labels$pos,plate$slot.labels$labs[i],bty="n",cex=plate$slot.labels$cex,inset=0,adj=plate$slot.labels$adj)
             })        
    }
    
    if(reference.dataset!="@@@overplot.dataset"){
        if(!underplotting){
            cat("Overplotted dataset: ",reference.dataset,"\n",sep="")
        }else{
            cat("Underplotted dataset: ",reference.dataset,"\n",sep="")
        }
    }else{
        cat("Success.\n",sep="")
    }
    # Restore the original values, clean up
    peekDataset("@@@temp")
    assign("x.data",xd,.GlobalEnv)
    assign("y.data",yd,.GlobalEnv)
    if(any(names(WRCube)=="@@@overplot.dataset")) WRCube["@@@overplot.dataset"]<<-NULL           
    WRCube["@@@temp"]<<-NULL
    
    options("gcd.shut.up"=shut.bak) # Restore the verbose option
    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui")setWindowTitle(paste("//","GCDkit - ",which.dataset,sep="")) # Set the RConsole title, dataset name to be restored too
    assign("dataset.name",which.dataset,.GlobalEnv)
    if(!screen(new=FALSE)) figaroOn() 
    invisible(reference.dataset)
}

# For underplotting, no pch, col, and cex may be specified
underplotDataset<-function(reference.dataset = NULL,transp = 0,...){
    overplotDataset(reference.dataset=reference.dataset,underplotting=TRUE,transp=transp,...)
    invisible(reference.dataset)
}
