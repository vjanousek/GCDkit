.odbc.export.core<-function(what,tablename,channel,transpose=FALSE,dec.places=NULL){ 
    on.exit(options("show.error.messages"=TRUE))
    options(warn=-1)
    #ee<-require(RODBC)
    #options(warn=0)
    #if(!ee){
    #    cat("WARNING: library RODBC is not installed! Quitting...\n")
    #    return()
    #}
    if(length(what)==0){cat("Skipping empty table",tablename,"\n");return()}
    if(length(tablename)==0)tablename<-winDialogString("Enter table name","")
    dest.tablenames<-sqlTables(channel)
    dest.tablenames<-gsub("[$]","",dest.tablenames[,"TABLE_NAME"])
    if(any(dest.tablenames==tablename)){
        winDialog(type="ok",paste("Table ",tablename," already exists!",sep=""))
        return(-1)
    }
    
    x<-data.frame(what,check.names=FALSE)
    x<-data.frame(x[,colnames(x)!="Mg#"],check.names=FALSE)
    if(ncol(x)==1){
        colnames(x)<-tablename
        rownames(x)<-rownames(what)
    }
    n<-gsub("[[:punct:]]","_",colnames(x))
    #n<-gsub("[!$%&'()*+,-.:;<=>?@^`|~]","_",colnames(x)) # Nenahradi lomitko
    n<-gsub(" ","_",n)
    i<-which(duplicated(toupper(n)))
    if(any(i))n[i]<-paste(n[i],1:length(i),sep="_")
    colnames(x)<-n
    
    n <-gsub("[[:punct:]]","",rownames(x))
    rownames(x)<-n
    
    cat(paste("Writing a table ",tablename,"....",sep=" "))
    
    # Round the numeric columns if required    
    if(!is.null(dec.places)){
        i<-unlist(lapply(colnames(x),function(i) is.numeric(x[,i])))
        x[,i]<-round(x[,i],dec.places)
    }

    # Transpose samples to columns if required
    if(transpose){
        ee<-dimnames(x)
        x<-data.frame(t(x))
        colnames(x)<-ee[[1]]
        rownames(x)<-ee[[2]]
    }
    
    #file.access(filename, mode = 2)
    options(show.error.messages=FALSE)
    ee<-try(sqlSave(channel,x,tablename=tablename,rownames=TRUE,colnames=FALSE,safer = TRUE))
    
    if(class(ee)=="try-error"){ 
        print(ee)
        cat("..FAILED!\n")
        winDialog(type="ok","Write error, possibly file already open")
        return(-1)
    }
    cat("..ok\n")
    invisible()
}

.tkfile.choose<-function(caption="",filters=matrix(ncol=2,byrow=TRUE,c("All files (*.*)","*.*"))){
    #require(tcltk) || stop("tcltk support is absent")
    ee<-paste(t(filters[,1]),collapse="}} {{")
    ee<-paste("{{",ee,"}}",sep="")
    ee<-gsub(" \\(","} {",ee)
    ee<-gsub("\\)","",ee)
    ee<-gsub("\\*\\.",".",ee)

    
    # "{{Excel files} {.xls}} {{All files} *}"
    ReturnVal <- tclvalue(tkgetSaveFile(title=caption,filetypes=ee))
    if (!nchar(ReturnVal)){
        tkmessageBox(message="No file was selected!")
    }else{
        ee<-as.character(ReturnVal)
        return(ee)
    }
}

excelExport<-function(what=cbind(labels,WR),tablename=NULL,transpose=FALSE,dec.places=NULL){
    on.exit(options("show.error.messages"=TRUE))
    
    if(length(grep("_64-pc",R.version$platform))>0){
        cat("WARNING: Excel output not available on 64bit systems! Quitting...\n")
        options("show.error.messages"=FALSE)
        stop()
    } 

    if(is.null(what)){
        winDialog(type = "ok", "No data available")
        options("show.error.messages"=FALSE)
        stop()
    }  
    
    #ee<-require(RODBC)
    #options(warn=0)
    #if(!ee){
    #    cat("WARNING: library RODBC is not installed! Quitting...\n")
    #    return()
    #}
    
    #filename<-choose.files(caption = "Select file",multi = FALSE, filters=matrix(ncol=2,byrow=TRUE,c("Excel files (*.xls)","*.xls")))
    #filename<-.tkfile.choose(caption = "Select file",filters=matrix(ncol=2,byrow=TRUE,c("Excel files (*.xls)","*.xls","All files (*.*)","*.*")))
    
    
    #if(!file.exists(filename)){
    #    ee<-file.copy(paste(gcdx.dir,"tmp.xls",sep="/"),filename)
    #    cat("New file\n")
    #}
    
    #dirt<-dirname(filename)
    #dirt<- chartr("/", "\\", dirt)

    #filename<-basename(filename)
    #con<-paste("Driver={Microsoft Excel Driver (*.xls)};DriverId=790;Dbq=",filename, ";DefaultDir=",dirt, ";", sep = "")
    #odbcCloseAll()
    #channel<-odbcDriverConnect(con)
    #print(channel)

    channel<-suppressWarnings(odbcConnectExcel(readOnly = FALSE))
    tablename<-NULL
    
    if(channel==-1){
        #cat("Cancelled.\n")
        options("show.error.messages"=FALSE)
        winDialog(type="ok","NB that currently only adding new worksheets to a pre-existing Excel file is possible.")
        stop("",call. = FALSE)
    }
    
    #print(odbcGetInfo(channel))
    if(is.data.frame(what)|is.numeric(what)){
            out<-.odbc.export.core(what,tablename,channel,transpose,dec.places)
    }else{
        for(i in attributes(what)$names){
            x<-what[[i]]
            y<-as.data.frame(x)
            if(ncol(y)==1)x<-y
            out<-.odbc.export.core(x,i,channel,transpose,dec.places)
        }
    }
    odbcCloseAll()
    if(is.null(out))ee<-winDialog(type="ok","Writing successful")
    flush.console()
    invisible()
}


excel2007Export<-function(what=cbind(labels,WR),tablename=NULL,transpose=FALSE,dec.places=NULL){
    on.exit(options("show.error.messages"=TRUE))
    
    if(length(grep("_64-pc",R.version$platform))>0){
        cat("WARNING: Excel output not available on 64bit systems! Quitting...\n")
        options("show.error.messages"=FALSE)
        stop()
    } 
    
    if(is.null(what)){
        winDialog(type = "ok", "No data available")
        options("show.error.messages"=FALSE)
        stop()
    }  
    
    #ee<-require(RODBC)
    #options(warn=0)
    #if(!ee){
    #    cat("WARNING: library RODBC is not installed! Quitting...\n")
    #    return()
    #}
    
    channel<-suppressWarnings(odbcConnectExcel2007(readOnly = FALSE))
    tablename<-NULL
    
    if(channel==-1){
        #cat("Cancelled.\n")
        options("show.error.messages"=FALSE)
        winDialog(type="ok","NB that currently only adding new worksheets to a pre-existing Excel file is possible.")
        stop("",call. = FALSE)
    }
    
    #print(odbcGetInfo(channel))
    if(is.data.frame(what)|is.numeric(what)){
            out<-.odbc.export.core(what,tablename,channel,transpose,dec.places)
    }else{
        for(i in attributes(what)$names){
            x<-what[[i]]
            y<-as.data.frame(x)
            if(ncol(y)==1)x<-y
            out<-.odbc.export.core(x,i,channel,transpose,dec.places)
        }
    }
    odbcCloseAll()
    if(is.null(out))ee<-winDialog(type="ok","Writing successful")
    flush.console()
    invisible()
}

accessExport<-function(what=cbind(labels,WR),tablename=NULL,transpose=FALSE,dec.places=NULL){
    on.exit(options("show.error.messages"=TRUE))
    
     if(length(grep("_64-pc",R.version$platform))>0){
        cat("WARNING: Access output not available on 64bit systems! Quitting...\n")
        options("show.error.messages"=FALSE)
        stop()
    } 
    
    if(is.null(what)){
        winDialog(type = "ok", "No data available")
        options("show.error.messages"=FALSE)
        stop()
    }  
    channel <- odbcConnectAccess()
    if(channel==-1){
            cat("Cancelled.\n")
            options(show.ercor.messages=FALSE)
            stop()
        }
    #print(odbcGetInfo(channel))
    if(is.data.frame(what)|is.numeric(what)){
            out<-.odbc.export.core(what,tablename,channel,transpose,dec.places)
    }else{
        for(i in attributes(what)$names){
            x<-what[[i]]
            y<-as.data.frame(x)
            if(ncol(y)==1)x<-y
            out<-.odbc.export.core(x,i,channel,transpose,dec.places)
        }
    }
    ee<-odbcCloseAll()
    if(is.null(out))ee<-winDialog(type="ok","Writing successful")
    flush.console()
    invisible()
}

dbfExport<-function(what=cbind(labels,WR),transpose=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    options(warn=-1)
    
    #ee<-require(foreign)
    #options(warn=0)
    #if(!ee){
    #    cat("WARNING: library foreign is not installed! Quitting...\n")
    #    return()
    #}
    
    if(is.null(what)){
        winDialog(type = "ok", "No data available")
        options("show.error.messages"=FALSE)
        stop()
    }
    
    if(length(what)==1){
        names(what)<-NULL
        what<-as.data.frame(what)
    }
    
    if(!is.data.frame(what)&!is.matrix(what)){
        winDialog(type = "ok", "Cannot handle lists, sorry")
        options("show.error.messages"=FALSE)
        stop()
    }
    
    colnames(what)<-gsub("[[:punct:]]","_",colnames(what))
    rownames(what)<-gsub("[[:punct:]]","_",rownames(what))
    if(transpose)what<-t(what)
    what<-cbind(row.names(what),what)
    colnames(what)[1]<-"SAMPLE"
          
    filename<-choose.files(caption = "Select file",multi = FALSE, filters=matrix(ncol=2,byrow=TRUE,c("dBase files (*.dbf)","*.dbf")))
    
    # Check for file overwrite
    ee<-file.access(filename, mode = 0)
    if(ee==0){
        eee<-winDialog(type="yesno","File exists! Overwrite?")
        if(eee=="NO"){options("show.error.messages"=FALSE);cat("Cancelled.\n");stop(call. = FALSE)}
    }
    
    options(warn=-1)
    write.dbf(what, filename, factor2char = TRUE)
    options(warn=0)
    flush.console()
    invisible()
}

  
