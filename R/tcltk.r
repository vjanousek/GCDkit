#############################################################################
#                                                                           #
#                                Tcl/Tk core routines                       #
#                                                                           #
#############################################################################
#require(tcltk)
#fontHeading<-tkfont.create(family="times",size=12,weight="bold",slant="italic")




#############################################################################
#                                                                           #
#                         Selects a single variable                         #
#                                                                           #
#############################################################################

tkSelectVariable<-function(top.frame=NULL,where=colnames(WR),preselect=2,pack=FALSE,message="Select a variable",background="wheat",variable="x",on.leave=function(){},row=0,column=0,height=15,width=50,buttons=FALSE,state="normal"){    
    if(is.null(top.frame)){
        pack<-TRUE
    }
    ee<-.selectSingle.tk(top.frame,where,preselect=preselect,message=message,print=FALSE,empty.ok=FALSE,pack=pack,variable=variable,background=background,on.leave=on.leave,row=row,column=column,height=height,width=width,buttons=buttons,state=state)
    #eval(parse(text=paste("print(",variable,")",sep="")))
}

###############################################################3#############
#                                                                           #
#                         Selects a single variable                         #
#                                                                           #
#############################################################################

.selectSingle.tk<-function(top.frame=tt,where=rownames(WR),message="Select sample",preselect=2,default="",silent=FALSE,print=TRUE,empty.ok=TRUE,pack=TRUE,row=0,column=0,variable="xx",selectmode="single",background="wheat",on.leave=NULL,show=NULL,height=15,width=30,buttons=TRUE,state="normal"){
    names(where)<-1:length(where)
    assign(variable,where[preselect],env = .GlobalEnv)
    if(print&!getOption("gcd.shut.up")){
        cat("\n")
        print(where)
        cat("\n")
    }

    # Core function !!!!!!
    .tkSelectColumnMain<-function(where,where.bak,preselect=0,top.frame,background="wheat"){
        on.exit(options("show.error.messages"=TRUE))
        # Initialize Tcl variables
        textEntry<-tclVar(selected)
        listBoxEntry<-tclVar("")
        lim1<-tclVar("")
        lim2<-tclVar("")
        tkfocus(top.frame)
        
        main.frame <- tkframe(top.frame,borderwidth=0, relief="groove")
        tkconfigure(main.frame,padx=5,pady=5)
        #fontHeading<-tkfont.create(family="times",size=11,weight="bold",slant="italic")
        fontBody<-tkfont.create(family="times",size=9,weight="bold")
    
        frame.textbox <- tkframe(main.frame, borderwidth=1, relief="groove")
            tkconfigure(frame.textbox,padx=5,pady=5)
            
            # Pressing OK in textbox
            OnReturn <- function(){
                selected<-tclvalue(textEntry)
                assign("selected",selected,env = .GlobalEnv)
                if(selected=="" & length(where)>1){
                    ee<-1:length(where.bak)
                }else{ 
                    ee<-.selectColumnLabel.core(selected,where.bak,empty.ok)
                }
                .tkSelectColumnMain(where.bak[ee],where.bak,preselect=0,top.frame=top.frame,background=background)
            }      
            
                # Entry box
                .tkEntryTexttMy(frame.textbox,textEntry,width=width-nchar(message)+8,text=message,on.return=OnReturn,background=background)
        tkgrid(frame.textbox,row=row,sticky="ew")
        
        frame.listbox <- tkframe(main.frame, borderwidth=0, relief="groove")        
            tkfocus(frame.listbox)
            
            #Listbox
            listbox<-.tkListBoxMy(frame.listbox,where,listBoxEntry,"",selectmode=selectmode,height=height,width=width,preselect=preselect,show=show)
            tkconfigure(listbox,state=state)
            tkfocus(frame.listbox)
            # leaving the whole window, equivalent to Ok button
            OnLeave2 <- function(){
                selected<<-""
                xx<-where[as.numeric(tkcurselection(listbox)) + 1]
                names(xx)<-NULL       
                assign(variable,xx,env = .GlobalEnv)
                on.leave()
            }
        
        tkbind(frame.listbox,"<Leave>",OnLeave2)
        tkgrid(frame.listbox,sticky="we")
            
            # Reset button - lists again all the variables in 'where'
            OnReset <- function(){
                .tkSelectColumnMain(where.bak,where.bak,preselect=0,top.frame=top.frame)
                #selected<<-""
            }
            
            
            # selecting.....
            OnOK <- function(){
                xx<-where[as.numeric(tkcurselection(listbox)) + 1]
                names(xx)<-NULL
                #print("OK")
                #print(xx)
                #print(variable)
                assign(variable,xx,env = .GlobalEnv)
                #if(length(xx)!=0){
                #    .tkSelectColumnMain(xx,where.bak,preselect=0,top.frame=top.frame)
                #}else{ 
                #    .tkSelectColumnMain(where,where.bak,preselect=NULL,top.frame=top.frame)
                #}
                if(pack)tkdestroy(top.frame)
            }
            
            # Cancel button  
            OnCancel <- function(){
                cat("Cancelled....\n")
                flush.console()
                xx<<-""
                selected<<-""
                if(pack)tkdestroy(top.frame)
            }
            
            # Sort button  
            OnSortUp <- function(){
                where2<-names(sort(WR[where,show],na.last=TRUE))
                .tkSelectColumnMain(where2,where.bak,preselect=0,top.frame=top.frame)
            }    
                        
            # Sort button  
            OnSortDown <- function(){
                where2<-names(rev(sort(WR[where,show],na.last=TRUE)))
                .tkSelectColumnMain(where2,where.bak,preselect=0,top.frame=top.frame)
            }
            
            if(buttons){
            frame.butt <- tkframe(main.frame, borderwidth=1, relief="groove") 
                Plot.but<-tkbutton(frame.butt,text=" ... ",command=function(){
                    SelectFromPlot("MgO")
                    textEntry<-tclVar(selected)
                    ee<-.selectColumnLabel.core(selected,where.bak,empty.ok)
                    OnOK()
                    .tkSelectColumnMain(where.bak[ee],where.bak,preselect=0,top.frame=top.frame,background=background)                   
                },fg="red")
                Reset.but<-tkbutton(frame.butt,text="   Reset   ",command=OnReset)
                SortUp.but<-tkbutton(frame.butt,text=" Sort Up",command=OnSortUp)
                SortDown.but<-tkbutton(frame.butt,text=" Sort Dn ",command=OnSortDown)
                Select.but <-tkbutton(frame.butt,text="   OK   ",command=OnOK,fg="darkgreen")
                Cancel.but <-tkbutton(frame.butt,text="   Cancel   ",command=OnCancel,fg="darkred")
                #if(!pack){
                if(is.null(show)){
                    tkgrid(Plot.but,Reset.but,Select.but,sticky="w")
                }else{
                    tkgrid(Plot.but,Reset.but,SortUp.but,SortDown.but,Select.but,sticky="w")
                }
                #}else{
                #    tkgrid(Plot.but,Reset.but,SortUp.but,SortDown.but,Select.but,Cancel.but,sticky="w")
                #}
            tkgrid(frame.butt)
            }else{
                tkbind(main.frame,"<Leave>",OnOK)
            }
        tkgrid(main.frame,row=row,column=column,sticky="ns") # Draw the whole damn thing
        if(pack & length(where)==1) return()
        tkfocus(top.frame)
        if(pack)tkwait.window(top.frame)
        return()    
    }
    
   if(pack){
        #require(tcltk) || stop("tcltk support is absent")
        top.frame<<-tktoplevel()
        tkwm.title(top.frame,"Select a variable")
    }
    xx<<-""
    ee<-.tkSelectColumnMain(where,where,top.frame=top.frame,preselect=preselect,background=background)
    return()
}
        
.selectColumnLabel.core<-function(selected,where=colnames(labels),empty.ok=TRUE,background="wheat",silent=FALSE){ 
    on.exit(options("show.error.messages"=TRUE))
    x<-selected
    #x<-winDialogString(message,default)
    
    if(x==""&empty.ok)return(NULL)
    xx<-gsub("\\*","\\\\*",x)

    if(!is.na(as.numeric(xx))){
        selected<-as.numeric(xx)
        if (selected>length(where)|selected<=0){
            selected<-x
            if(!silent){
                .tkmessageBoxMy(type="ok","Your query produced no matches!")
                options(show.error.messages=FALSE)
                stop("",call. = FALSE)
            }
        }
        return(selected)
    }
            options(show.error.messages=FALSE)
            selected<-try(grep(xx,where))
            if(class(selected)=="try-error"){
                .tkmessageBoxMy(type="ok","Syntax error in search pattern!")
                stop("",call. = FALSE)
            }

    options(show.error.messages=TRUE)
    if(length(selected)==0){
        selected<-x
        if(!silent){
            .tkmessageBoxMy(type="ok","Your query produced no matches!")
            options(show.error.messages=FALSE)
            stop("",call. = FALSE)
            #return(selected)
        }
        options(show.error.messages=TRUE)
        selected.old<-selected
        selected<-gsub("major",paste(major,collapse=","),selected)
        selected<-gsub("REE",paste(REE,collapse=","),selected)
        selected<-gsub("HFSE",paste(HFSE,collapse=","),selected)
        selected<-gsub("LILE",paste(LILE,collapse=","),selected)
        if(selected.old!=selected)selected<-selectColumnLabel(default=selected,silent=TRUE,print=FALSE)
        return(selected)
    }

    if((length(selected)==length(where))&empty.ok){
        selected<-NULL
        return(selected)
    }
    
    
    SelectFromPlot<-function(variable){
    tt<<-tktoplevel()
    tkwm.title(tt,"Binary plot")
    x<<-colnames(WR)[1]
    y<<-colnames(WR)[3]

    frame.xy <- tkframe(tt, borderwidth=3, relief="groove")
                tkconfigure(frame.xy,padx=1,pady=1) 
                #fontHeading<-tkfont.create(family="times",size=11,weight="bold",slant="italic")
                tkgrid(tklabel(frame.xy,text="Elements for plotting:",font=fontHeading,background=background),sticky="n")            
                
                cbLogX<<-tclVar(0)
                cbLogY<<-tclVar(0)
                
                # Select x axis for plot
                frame.x <- tkframe(frame.xy, borderwidth=0, relief="groove") 
                    tkSelectVariable(frame.x,where=colnames(WR),preselect=0,message="x:",variable="x",pack=FALSE,row=0,column=0,width=10,height=5)  
                    .tkCheckBoxMy(frame.x,cbLogX,text="log")
                    print(x)
                # Select y axis for plot
                frame.y <- tkframe(frame.xy, borderwidth=0, relief="groove") 
                    tkSelectVariable(frame.y,where=colnames(WR),preselect=2,message="y:",variable="y",pack=FALSE,row=0,column=1,width=10,height=5)  
                    .tkCheckBoxMy(frame.y,cbLogY,text="log")
                    print(y)
                tkgrid(frame.x,frame.y,sticky="w")
    tkgrid(frame.xy,row=0,column=0)
                
            
    # Buttons
    tkfocus(tt)
    tkwm.deiconify(tt) 
   
    
    # Cancel button  
    OnCancel <- function(){
        cat("Cancelled....\n")
        flush.console()
        tkdestroy(tt)
    }
    
    # OK button
    OnOK<-function(variable){
        print(x)
        print(y)
        windows(width = 6.5, height = 6.5, pointsize = 10)
        plot(WR[,x],WR[,y],xlab=annotate(x),ylab=annotate(y),col=labels$Colour,pch=labels$Symbol)
        #text(WR[,x],WR[,y]+0.15,rownames(WR))
        i<-identify(WR[,x],WR[,y],labels=rownames(WR),n=1,offset=0.1,col="red")
        xx<-rownames(WR)[i]
        assign(variable,xx,env = .GlobalEnv)
        if(length(xx)!=0) .tkSelectColumnMain(xx,where.bak,preselect=0,top.frame=top.frame) else .tkSelectColumnMain(where,where.bak,preselect=NULL,top.frame=top.frame)
        ee<-locator(1)
        ee<-dev.off(dev.cur())
        if(pack)tkdestroy(top.frame)
    }
      
    frame.but<- tkframe(tt, borderwidth=3, relief="groove")
        Cancel.but <-tkbutton(frame.but,text="   Cancel   ",command=OnCancel)
        OK.but <-tkbutton(frame.but,text="   OK   ",command=OnOK)
        tkgrid(Cancel.but,OK.but)
    tkgrid(frame.but,row=1,column=0)
    
    tkfocus(tt)
    ee<-tkwait.window(tt)          
                        
}
    
return(selected)
}

#############################################################################
#                                                                           #
#                              Select directory                             #
#                                                                           #
#############################################################################
#fontHeading<-tkfont.create(family="times",size=12,weight="bold",slant="italic")
.tkDirectory<-function(top.frame=NULL,pack=FALSE,width=60,background="gray95",on.leave=function(){},on.select=function(){}){    
    textDir<-tclVar(data.dir)
    frame <- tkframe(top.frame, borderwidth=0, relief="groove")   
    tkconfigure(frame,padx=0,pady=5)
    
    abbrev<-function(width,what){
       x<-nchar(what)
            if (x>width) txt<-paste("...",substring(what,x-width+5,x)) else txt<-substring(paste(what,"                                                                                                        "),1,width)    
        return(txt)
    }
    
    # Successfully selected
    OnFilenameBrowse<-function(dir){
            if(dir=="") return()
            assign("data.dir",dir,env = .GlobalEnv)
            textFilename<-tclVar(dir)
            tkgrid(tklabel(frame,text=abbrev(width,dir),font=fontFileName),sticky="w",row=0,column=2)
    }
    
    # Clicked button for selecting a directory
    OnSelectDir<-function(){
        OnFilenameBrowse(tclvalue(tkchooseDirectory(initialdir=data.dir,title="Select the GCDkit data directory")))
    }
    
    fontFileName<-tkfont.create(family="courier",size=9)#,slant="italic")
    
    #frame2 <- tkframe(top.frame, borderwidth=0, relief="groove") 
        SelectDir.but <-tkbutton(frame,text=" ... ",command=OnSelectDir)
        #fontHeading<-tkfont.create(family="times",size=11,weight="bold",slant="italic")       
        tkgrid(tklabel(frame,text="Working directory ",font=fontHeading,background=background),SelectDir.but,sticky="w")  
    #tkgrid(frame2,sticky="w")
    text<-tclvalue(textDir)
    tkgrid(tklabel(frame,text=abbrev(width,text),font=fontFileName),sticky="w",row=0,column=2)     
    tkbind(frame,"<Leave>",on.leave)     
    tkgrid(frame)
}


# Text entry
.tkEntryTexttMy<-function(top.frame,tcl.variable,text="Enter variable",width=3,background="wheat",on.leave=NULL,on.return=NULL,on.keyrelease=NULL,...){    
    frame <- tkframe(top.frame)
    tkconfigure(frame,padx=1,pady=1)
    #fontHeading<-tkfont.create(family="times",size=11,weight="bold",slant="italic")
    
    ee<-tkentry(frame,textvariable=tcl.variable,width=width,bg="white",...)
    tkbind(ee,"<Return>",on.return)
    tkbind(frame,"<Leave>",on.leave)
    tkbind(frame,"<KeyRelease>",on.keyrelease)

    if(text!=""){
        tcl.variable<-tkgrid(tklabel(frame,text=paste(text," "),font=fontHeading,background=background),ee,sticky="w")
    }else{
        tcl.variable<-tkgrid(ee)
    }
    tkgrid(frame)
    return(tcl.variable)
}


# Text entries [multiple]
.tkEntryTextsMy <- function(top.frame,varlist,tcl.variable.name="cb",text="",on.return=function(){},on.keyrelease=function(){}){
    frame <- tkframe(top.frame, borderwidth=0)
    tkconfigure(frame,padx=1,pady=1)
    if(text!=""){
        #fontHeading<-tkfont.create(family="times",size=11,weight="bold",slant="italic")
        tkgrid(tklabel(frame,text=text,font=fontHeading,background="wheat"),sticky="we",row=0,column=0)
    }
    
    frame2 <- tkframe(frame, borderwidth=3)
        for(i in 1:length(varlist)){
            eval(parse(text=(paste(tcl.variable.name,i,"<<- tclVar(\"",varlist[i],"\")",sep=""))))
            #eval(parse(text=(paste("cb",i,"<<- tclVar(\"\")",sep=""))))
            eval(parse(text=(paste("ee",i,"<-tkentry(frame2,textvariable=",tcl.variable.name,i,",width=10)",sep=""))))
            eval(parse(text=(paste("tkbind(ee",i,",\"<Return>\",on.return)",sep=""))))
            eval(parse(text=(paste("tkbind(ee",i,",\"<KeyRelease>\",on.keyrelease)",sep=""))))
            tkgrid(tklabel(frame2,text=names(varlist)[i]),eval(parse(text=(paste("ee",i,sep="")))),sticky="w",row=i-1)
            #tkgrid(tklabel(frame2,text=varlist[i]),eval(parse(text=(paste("ee",i,sep="")))),sticky="w",row=i-1)
        }
    tkgrid(frame2,row=1,column=0,sticky="w")
    tkgrid(frame,sticky="nswe")
}


# Select list
.tkListBoxMy<-function(top.frame,varlist,tcl.variable,text="",selectmode="single",height=5,width=30,preselect=NULL,on.leave=NULL,show=NULL,state="normal",row=0,column=0,pack=TRUE){    
    frame <- tkframe(top.frame)
    tkconfigure(frame,padx=1,pady=1)
    if(is.character(preselect)) preselect<-match(preselect,varlist)-1
    
    fontList<-tkfont.create(family="courier",size=9)
    if(length(varlist)>height){
        scrollbar <- tkscrollbar(frame, command=function(...)tkyview(listbox,...)) # get the scrollbar
        listbox<-tklistbox(frame,height=height,width=width,selectmode=selectmode,font=fontList,exportselection=FALSE,yscrollcommand=function(...)tkset(scrollbar,...),fg="darkblue",bg="white",disabledforeground="",state=state)
    }else{
        listbox<-tklistbox(frame,height=height,width=width,selectmode=selectmode,font=fontList,exportselection=FALSE,fg="darkblue",bg="white",disabledforeground="",state=state)
    } 
           
    if (is.null(show)){
        for (var in varlist){ 
            tkinsert(listbox,"end",var)
        }  
    }else{
        ee<-sapply(varlist,function(i){
                n<-10-nchar(i)
                n[n<0]<-0
                what<-paste(rep(" ",each=n),collapse="")
                what<-strtrim(paste(i,what,sep=""),10)
                z<-paste(" ",what,"|",WR[i,show])
            })
        for (var in ee){ 
            tkinsert(listbox,"end",var)
        }
        text<-paste("              ",show)
    }
    
    if (!is.null(preselect)){
        sapply(preselect,function(i){
            tkselection.set(listbox, i)
            }          
        )
    }
    
    if(text!="")tkgrid(tklabel(frame,text=text,font=fontList),sticky="w")
    
    if(length(varlist)>height){
        tkgrid(listbox, scrollbar, sticky="nw")
        tkgrid.configure(scrollbar, sticky="wns")
    }else{
        tkgrid(listbox,sticky="nw")
    }
    tkgrid.configure(listbox, sticky="ew")
    
    tkbind(frame,"<Leave>",on.leave)
    if(pack) tkgrid(frame,row=row,column=column)
    return(listbox)
}

    
# Radio button
.tkRadioButtonMy <- function(top.frame,varlist,tcl.variable,sticky="e",text="",on.leave=NULL){
    uff<-tklabel(top.frame,text=text,font=fontHeading,background="gray95")
    frame <- tkframe(top.frame, borderwidth=3, relief="flat")
    for(i in 1:length(varlist)){
        rb <- tkradiobutton(frame)
        tkconfigure(rb,variable=tcl.variable,value=varlist[i])
        tkgrid(rb,tklabel(frame,text=varlist[i]),sticky="w")
    }  
    tkbind(frame,"<Leave>",on.leave)
    tkgrid(uff,frame,sticky="e")
    #tkbind(frame,"<Button-1>",on.mouse.button)
}


# CheckBox
.tkCheckBoxMy <- function(top.frame,tcl.variable,text="",command=NULL){
    frame <- tkframe(top.frame, borderwidth=0, relief="raised")
    #if(text!=""){
    #    fontHeading<-tkfont.create(family="times",size=11,weight="bold",slant="italic")
        uf<-tklabel(frame,text=text,font=fontHeading)
    cb<-tkcheckbutton(frame)      
    tkconfigure(cb,variable=tcl.variable)
    tkconfigure(cb,command=command)
    tkgrid(uf,cb,sticky="w")
    tkgrid(frame)
}


# CheckBoxes
.tkCheckBoxesMy <- function(top.frame,varlist,tcl.variable,text="",ticks=rep(0,length(varlist))){
    frame <- tkframe(top.frame, borderwidth=0, relief="raised")
    tkconfigure(frame,padx=1,pady=1)
    if(text!=""){
        #fontHeading<-tkfont.create(family="times",size=11,weight="bold",slant="italic")
        tkgrid(tklabel(frame,text=text,font=fontHeading,background="wheat"),sticky="ew",row=0,column=0)
    }
    
    #scrollbar <- tkscrollbar(frame, command=function(...)tkyview(prd,...)) # get the scrollbar
    frame2 <- tkframe(frame, borderwidth=0, relief="raised")

    for(i in 1:length(varlist)){
        eval(parse(text=(paste("cb",i," <- tkcheckbutton(frame2)",sep=""))))
        eval(parse(text=(paste("cbValue",i," <<- tclVar(\"",ticks[i],"\")",sep=""))))
        ee<-parse(text=paste("cb",i,sep=""))
        ee1<-parse(text=paste("cbValue",i,sep=""))
        tkconfigure(eval(ee),variable=eval(ee1))
        tkgrid(tklabel(frame2,text=varlist[i]),eval(ee),sticky="w",row=i-1)
    }  
    tkgrid(frame2,row=1,column=0,sticky="ew")
    tkgrid(frame,sticky="ew")
}


# Message box
.tkmessageBoxMy<-function(type="ok",message=""){
    ReturnVal <- tkmessageBox(title="",message=message,icon="info",type=type)
    #tkfocus(tt)
    ee<-toupper(as.character(ReturnVal))
    return(ee)
}
