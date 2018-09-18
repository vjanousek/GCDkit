   
#############################################################################
#                                                                           #
#                      Tcl/Tk interface to GCDkit options                   #
#                                                                           #
#############################################################################

gcdOptions<-function(permanent.only=FALSE){  
    #require(tcltk) || stop("tcltk support is absent")
    fontHeading<<-tkfont.create(family="arial",size=9,weight="bold") #slant="italic") 
    
    # Initialize Tcl variables   
    textDigits<-tclVar(getOption("gcd.digits"))
    quiet<-tclVar(getOption("gcd.shut.up"))
    if(getOption("gcd.plot.text")) rbAnnotate <- tclVar("Yes") else rbAnnotate <- tclVar("No")
    bw<-tclVar(0)
    
    listIdentifyVar <-tclVar("")
    
    ee1<-getOption("gcd.ident")     
    if(ee1==0){
        rbIdentify <- tclVar("No")
        
    }else{
        ee2<-getOption("gcd.ident.each")
        if(ee2) rbIdentify <- tclVar("Individually") else rbIdentify <- tclVar("All automatically")     
    }
    
    textCex<-tclVar(getOption("gcd.cex"))
    listLang <- tclVar(getOption("gcd.language"))
            
    #Open Tcl/Tk window 
    tt<<-tktoplevel()
    tkconfigure(tt,padx=10,pady=5,width=60)
    tkconfigure(tt,cursor="left_ptr")
    tkwm.title(tt,"GCDkit options")

# Core function                                                                    
.GCDOptions.Main<-function(){
    main.frame <<- tkframe(tt)
    tkfocus(tt)  
        # Working dir
            frame.dir <- tkframe(tt,  borderwidth=1, relief="groove")
                tkconfigure(frame.dir,padx=5,pady=5)
                .tkDirectory(frame.dir,on.select=OnLeaveFilename)
            tkgrid(frame.dir,sticky="nwe",rowspan=10)
            
          # Shut up, digits
         frame.first<-tkframe(tt, borderwidth=1, relief="groove")
            frame.shutup<-tkframe(frame.first, borderwidth=1, relief="groove")
                tkconfigure(frame.shutup,padx=5,pady=5)
                .tkCheckBoxMy(frame.shutup,quiet,text="Minimize output\non screen")
            frame.digits <- tkframe(frame.first, borderwidth=1, relief="groove")
            tkconfigure(frame.digits,padx=5,pady=5)
            .tkEntryTexttMy(frame.digits,textDigits,width=3,text="Precision of results",background="gray95")
            
            
            # Entry box cex
            frame.cex <- tkframe(frame.first, borderwidth=1, relief="groove")
                tkconfigure(frame.cex,padx=10,pady=5)
                .tkEntryTexttMy(frame.cex,textCex,width=3,text="Plotting symbols magnification",background="gray95",font=fontHeading)
            tkgrid(frame.shutup,frame.digits,frame.cex,sticky="w")
        tkgrid(frame.first,sticky="we")
        
        # Annotate?
        frame.anno <- tkframe(tt, borderwidth=1, relief="groove")
        tkconfigure(frame.anno,padx=5,pady=5)   
            #Radio button Annotate fields by text labels?
            OnLeave2 <- function(){
                        ee<-as.numeric(tkcurselection(listbox2))
                        options("gcd.language"=languages[ee+1])
                        #if(length(ee)==0) options("gcd.language"="English")
            }
            
            .AnnoSave<-function(){
                if(length(tclvalue(listLang))==0) listLang <<- tclVar("English")          
                #options("gcd.plot.text"=tclvalue(rbAnnotate)=="Yes")
                if(tclvalue(rbAnnotate)=="Yes"){
                    state="normal" 
                }else{
                    state="disabled"
                }
                tkconfigure(listbox2,state=state)
            }
            
            frame.anno.butt <- tkframe(frame.anno, borderwidth=0)
                tkconfigure(frame.anno.butt,padx=5,pady=5)
                #tkgrid(tklabel(frame.anno.butt,text="Annotate fields in\ndiscrimination plots?",font=fontHeading,background="gray95"),sticky="w")
                what<-c("Yes","No")
                .tkRadioButtonMy(frame.anno.butt,what,rbAnnotate,text="Annotate fields in\ndiscrimination plots?",sticky="w",on.leave=.AnnoSave)
            tkgrid(frame.anno.butt,sticky="w",row=2,column=1)
            
            # Language List box
            frame.gcd.lang <- tkframe(frame.anno, borderwidth=0)
            tkconfigure(frame.gcd.lang,padx=5,pady=5)
                frame.gcd.lang.txt <- tkframe(frame.gcd.lang, borderwidth=0, relief="groove")
                    tkgrid(tklabel(frame.gcd.lang.txt,text="Language",font=fontHeading,background="gray95"),sticky="w")
                frame.gcd.lang.list <- tkframe(frame.gcd.lang, borderwidth=0, relief="groove")

                    pathClas<-paste(gcdx.dir,"Diagrams","Classification",sep="/")
                    languages<-.files.only(pathClas,"*[^.r]$",full.names=FALSE)
                    
                    if(getOption("gcd.plot.text")) state="normal" else state="disabled"
                    listbox2<-.tkListBoxMy(frame.gcd.lang.list,languages,listLang,text="",selectmode="single",height=2,width=10,preselect=getOption("gcd.language"),on.leave=OnLeave2,row=2,column=2)    
                    tkconfigure(listbox2,state=state)
                tkgrid(frame.gcd.lang.txt,frame.gcd.lang.list,sticky="w")
            tkgrid(frame.gcd.lang,sticky="w",row=2,column=2)
        tkgrid(frame.anno,sticky="we",columnspan=2)
        
        # Colours for field boundaries etc
        colours<-unique(c(plt.col,"black",colours()))
        #colours<-c(col.no,colours()[colours()!=c(col.no,"gray95")])
        frame.col.main <- tkframe(tt, borderwidth=1, relief="groove")
        tkconfigure(frame.col.main,padx=5,pady=5)
            #tkgrid(tklabel(frame.col.main,text="Colours for:",font=fontHeading,background="gray95"),sticky="w")
            ptcol2<-plt.col[2]
            ptcol3<-plt.col[3]
            .col.menus<-function(state="normal"){
                tkSelectVariable(frame.field,where=colours,preselect=plt.col[2],pack=FALSE,background="gray95",message="Colours for boundaries/labs",variable="pltcol2",on.leave=function(){},row=3,column=2,height=2,width=40,buttons=FALSE,state=state)
                tkSelectVariable(frame.txt,where=colours,preselect=plt.col[3],pack=FALSE,background="gray95",message="Colours for extra texts",variable="pltcol3",on.leave=function(){},row=3,column=3,height=2,width=40,buttons=FALSE,state=state)
            }
            
            .ColsSave<-function(){
                if(length(!is.na(pltcol2))!=0){if(!is.na(pltcol2))  plt.col[2]<<-pltcol2}
                if(length(!is.na(pltcol3))!=0){if(!is.na(pltcol3))  plt.col[3]<<-pltcol3}
            }    
                   
            frame.col <- tkframe(frame.col.main, borderwidth=0)
            tkconfigure(frame.col,padx=5,pady=5)
                frame.field <- tkframe(frame.col, borderwidth=0, relief="groove") 
                frame.txt <- tkframe(frame.col, borderwidth=0, relief="groove") 
                    .col.menus("normal")
                tkgrid(frame.field,frame.txt,sticky="w")
                tkconfigure(frame.col,padx=5,pady=5)
                tkbind(frame.col,"<Leave>",.ColsSave)
           
            
            frame.col.bw <- tkframe(frame.col.main, borderwidth=0)
            tkconfigure(frame.col.bw,padx=5,pady=5)
                   Click <- function(){
                   ee<-as.numeric(tclvalue(bw))
                   if(ee){
                        .col.menus("disable")  
                   }else{
                       .col.menus("normal")
                    }
                }  
                .tkCheckBoxMy(frame.col.bw,bw,text="Set to BW",command=Click)
            tkgrid(frame.col,frame.col.bw,sticky="w")
        tkgrid(frame.col.main,sticky="ew") 
        tkgrid(frame.col,sticky="n")
        if(!permanent.only){ 
        
            # Identify Options
            frame.id <- tkframe(tt, borderwidth=1, relief="groove")
            tkconfigure(frame.id,padx=5,pady=5)
            .IdentSave<-function(){
                if(tclvalue(rbIdentify)=="No"){
                    state<-"disabled"
                    options("gcd.ident"=0) 
                }else{
                    state<-"normal" 
                }
                tkconfigure(listbox1,state=state)
            }
            # Identify RB
            frame.id.rb <- tkframe(frame.id, borderwidth=0, relief="groove")
                #tkgrid(tklabel(frame.id.rb,text="Identify points?",font=fontHeading,background="gray95"),sticky="w")
                what<-c("No","All automatically","Individually")
                .tkRadioButtonMy(frame.id.rb,what,rbIdentify,text="Identify points?",sticky="w",on.leave=.IdentSave)
            tkgrid(frame.id.rb,sticky="w",row=4,column=1)
            
            # Identify List box
            frame.id.which <- tkframe(frame.id, borderwidth=0, relief="groove")
                OnLeave1<- function(){
                    options("gcd.ident"=as.numeric(tkcurselection(listbox1))+1)
                    if(length(getOption("gcd.ident"))==0) options("gcd.ident"=0)
                }
                if(tclvalue(rbIdentify)=="No") state="disabled" else state="normal"
                ids<-c("Sample names",colnames(labels))
                listbox1<<-.tkListBoxMy(frame.id.which,ids,listIdentifyVar,text="",selectmode="single",height=4,width=20,preselect=ids[getOption("gcd.ident")],on.leave=OnLeave1)  
                tkconfigure(listbox1,state=state)
            tkgrid(frame.id.which,sticky="w",row=4,column=2)
            tkgrid(frame.id,sticky="we",columnspan=3)
        }
    
    # Save button  
    OnSave <- function(){
            # Quiet?
            options("gcd.shut.up"=(tclvalue(quiet)==1))
            
            # Digits
            options("gcd.digits"=as.numeric(tclvalue(textDigits)))
            
            # Plot text
            options("gcd.plot.text"=(tclvalue(rbAnnotate)=="Yes"))
            #Language
            #options("gcd.language"=tclvalue(listLang))
           
            #Plotting colours, field boundaries, text labels for the fields
            options("gcd.plot.bw"=(tclvalue(bw)=="1"))
             if(!is.null(pltcol2)){    
                if(tclvalue(bw)!="1"){
                    .ColsSave()
                }else{
                    plt.col[[2]]<<-"black"
                    plt.col[[3]]<<-"black"
                }
            }
            # Identify - various combinations
            if(tclvalue(rbIdentify)=="No"){
                options("gcd.ident"=0)
            }else{
                options("gcd.ident.each"=tclvalue(rbIdentify)=="Individually")
            }
            
            # Cex
            options("gcd.cex"=as.numeric(tclvalue(textCex)))
            
            # Set data dir
            setwd(data.dir)           
            
            # Source correct language version of diagrams
            sourceDir(paste("Diagrams/Classification",getOption("gcd.language"),sep="/"),"classification diagram",recursive=TRUE)
            sourceDir("Diagrams/Geotectonic","geotectonic diagram",recursive=TRUE) 
            .optionsSave(permanent.only=permanent.only)             
            if(!permanent.only) .tkmessageBoxMy(type="ok",message="Saving successful")
            tkdestroy(tt)
    }
    
    # Cancel button  
    OnCancel <- function(){
        cat("Cancelled....\n")
        flush.console()
        tkdestroy(tt)
    }
    
    # Restore defaults button
    OnDef<- function(){
        x<-winDialog("Are you sure?",type="yesno")
        if(x=="NO") return()
        .setDefault()
        flush.console()
        tkdestroy(tt)
        gcdOptions()
    }
    
    # Help button
    OnHelp<-function(){
        browseURL(paste("file:/",gcdx.dir,"/html/gcdOptions.html",sep=""))
    }
    
    frame.but<- tkframe(tt, borderwidth=0)
        Save.but <-tkbutton(frame.but,text="   Ok/Save   ",command=OnSave,fg="darkgreen")
        Cancel.but <-tkbutton(frame.but,text="   Cancel   ",command=OnCancel,fg="darkred")
        Defaults.but<-tkbutton(frame.but,text="   Restore defaults   ",command=OnDef)
        Help.but <-tkbutton(frame.but,text="   Help   ",command=OnHelp)
    tkgrid(Save.but,Cancel.but,Defaults.but,Help.but,sticky="w")
    tkgrid(frame.but,sticky="n")
    
    #yscr <- tkscrollbar(tt, repeatinterval=5,
    #                   command=function(...)tkyview(tt,...))
    #tkgrid(yscr) # Draw scrollbar nefunguje
    #tkgrid.configure(yscr,sticky="ns")
    tkfocus(tt)
    tkwait.window(tt)
 }
  ee<-.GCDOptions.Main()
}

   
.optionsSave<-function(permanent.only=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    filename<-paste(gcdx.dir,"gcdkit.xxx",sep="/")
    which<-c("prompt","windowsBuffered","locatorBell","scipen","max.print","gcd.language","gcd.plot.text","gcd.plot.bw","gcd.digits","gcd.shut.up","gcd.cex","gcd.menus")
    what<-cbind(options()[which])
    colnames(what)<-""
    dir<-paste("\nWorking dir = ",data.dir,sep="")
    cat(dir,"\n")
    cat("Plotting cols\t",plt.col)
    
    if(!permanent.only){
        cat("\nOptions not to be saved to the config file:\n")
        cat("gcd.ident \t",getOption("gcd.ident"),"\n")
        cat("gcd.ident.each \t",getOption("gcd.ident.each"),"\n")
    }
    
    write.table(dir,file=filename,quote=FALSE,col.names=FALSE,row.names=FALSE,append=FALSE,sep=" = ")
    write.table(plt.col,file=filename,quote=FALSE,col.names=FALSE,row.names=TRUE,append=TRUE,sep=" = ")
    write.table(what,file=filename,quote=FALSE,col.names=FALSE,row.names=TRUE,append=TRUE,sep=" = ")
    options(show.error.messages=FALSE)
    ee<-try(labels[,"Size"]<-labels[,"Size"]*getOption("gcd.cex"))
    .assignWithNamespaceHard("labels",labels)
}

.optionsLoad<-function(){
    options("warn"=-1)
    filename<-paste(gcdx.dir,"gcdkit.xxx",sep="/")
    #ee<-read.table(file=filename,sep="=")
    ee<-try(read.table(file=filename,sep="="))
        if(class(ee)=="try-error"){
        winDialog(type="ok","Error reading the GCDkit config file!\nSaving a new file with default values....") # TO BE FIXED
        .setDefault()
        .optionsSave()
        invisible()
        return()
    }
    # Set data dir
    dir<-as.character(ee[1,2])
    dir<-gsub("^[ ]{1,}","",dir)
    assign("data.dir",dir,.GlobalEnv)
    setwd(dir)
    
    ee<-apply(ee,2,gsub,pattern="[ ]",replacement="")   
    prc<-as.list(as.character(ee[,2]))
    prc[prc=="FALSE"]<-FALSE
    prc[prc=="TRUE"]<-TRUE
    ow <- options("warn")
    options("warn"=-1)
    out<-lapply(1:length(prc),function(i){
        y<-as.numeric(prc[[i]])
        if(is.na(y)){
            return(prc[[i]])
        }else{
            if(!is.logical(prc[[i]])) return(y) else return(prc[[i]])
        }
    }
    )
    options(ow)
    
    # Default is no identification
    options("gcd.ident"=0)
    options("gcd.ident.each"=TRUE)            
    
    assign("plt.col",unlist(out[2:4]),.GlobalEnv)
    out<-out[-(1:4)]
    names(out)<-ee[-(1:4),1]
    #print(out)
    options(out)
    flush.console()
}

.files.only<-function(directory,pattern="*.*",full.names=TRUE){
    ee<-list.files(path=directory,pattern=pattern,full.names=full.names)
    if(is.vector(ee)) return(ee)
    return(ee[!(file.info(ee))[2]])    
}
