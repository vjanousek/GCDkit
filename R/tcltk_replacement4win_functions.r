#############################################################################
#                                                                           #
# winDialog # (type = c("ok", "okcancel", "yesno", "yesnocancel"), message) #
#                                                                           #
#############################################################################

tk_winDialog<-function(type="ok",message=""){
    ReturnVal <- tkmessageBox(title="",message=message,icon="info",type=type)
    ee<-toupper(as.character(ReturnVal))
    return(ee)
}

# tk_messageBox Perhaps useful
# tk_messageBox(type = c("ok", "okcancel", "yesno", "yesnocancel","retrycancel", "aburtretrycancel"),message, caption = "", default = "", ...)
                
#############################################################################
#                                                                           #
#                    winDialogString (message, default)                     #
#                                                                           #
#############################################################################
              
tk_winDialogString<-function(message="Enter variable",default="",returnValOnCancel=NULL){
    tt<-tktoplevel()
        tkconfigure(tt,borderwidth=0, relief="groove")
        tkwm.deiconify(tt)
        tkwm.title(tt,"")
        tkgrid(tklabel(tt,text=message))
        tkgrab.set(tt)
        tkfocus(tt)
        
        textEntryVarTcl <- tclVar(paste(default))
        text.butt <- tkframe(tt, borderwidth=0)
            tkconfigure(text.butt,padx=10,pady=10)
            textEntryWidget <- tkentry(text.butt,width=paste(50),textvariable=textEntryVarTcl)
            tkgrid(textEntryWidget)
        tkgrid(text.butt,sticky="w")
        ReturnVal <- returnValOnCancel
                
        onOK <- function(){
            ReturnVal <<- tclvalue(textEntryVarTcl)
            tkgrab.release(tt)
            tkdestroy(tt)
        }
    
        onCancel <- function(){
            ReturnVal <<- returnValOnCancel
            tkgrab.release(tt)
            tkdestroy(tt)
        }
                
        frame.butt <- tkframe(tt, borderwidth=1, relief="groove")
            OK.but     <-tkbutton(frame.butt,text="   OK   ",command=onOK)
            Cancel.but <-tkbutton(frame.butt,text=" Cancel ",command=onCancel)
        
            tkgrid(OK.but,Cancel.but,sticky="s")
        tkgrid(frame.butt,sticky="s")
                
    
    tkbind(tt, "<Destroy>", function() {tkgrab.release(tt)})
    tkbind(textEntryWidget, "<Return>", onOK)
    
    tkfocus(tt)
    tkraise(tt)
    tkwait.window(tt)
    return(ReturnVal)
}
            
#############################################################################
#                                                                           #
#                          select.list                                    #
#                                                                           #
#############################################################################
            
# Syntax of the two is: 
# select.list(choices, preselect = NULL, multiple = FALSE,title = NULL, graphics = getOption("menu.graphics"))
# tk_select.list(choices, preselect = NULL, multiple = FALSE,title = NULL)
            
#############################################################################
#                                                                           #
#                          Progress bars                                    #
#                                                                           #
#############################################################################

# Syntax of the two is:
#winProgressBar(title = "R progress bar", label = "", min = 0, max = 1, initial = 0, width = 300)
#tkProgressBar(title = "R progress bar", label = "", min = 0, max = 1, initial = 0, width = 300)
#setWinProgressBar(pb, value, title = NULL, label = NULL)
#setTkProgressBar(pb, value, title = NULL, label = NULL)

#############################################################################
#                                                                           #
#                          choose.files                                     #
#                                                                           #
#############################################################################
 
#choose.files(default = "", caption = "Select files", multi = TRUE, filters = Filters, index = nrow(Filters)) 
#tk_choose.files(default = "", caption = "Select files", multi = TRUE, filters = NULL, index = 1)

#############################################################################
#                                                                           #
#                            choose.dir                                     #
#                                                                           #
#############################################################################

#choose.dir(default = "", caption = "Select folder")  
#tk_choose.dir(default = "", caption = "Select directory")

#############################################################################
#                                                                           #
#                         Slightly modified tkProgressBar                   #
#                                                                           #
#############################################################################
.tk_ProgressBar<-function (title = "R progress bar", label = "", min = 0, max = 1,initial = 0, width = 300){
    useText <- FALSE
    have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
    if (!have_ttk && as.character(tclRequire("PBar")) == "FALSE") 
        useText <- TRUE
    .win <- tktoplevel()
    .val <- initial
    .killed <- FALSE
    tkwm.geometry(.win, sprintf("%dx80", width+50)) # Modified
    tkwm.title(.win, title)
    fn <- tkfont.create(family = "helvetica", size = 11) # Modified
    if (useText) {
        .lab <- tklabel(.win, text = label, font = fn, padx = 20)
        tkpack(.lab, side = "left")
        fn2 <- tkfont.create(family = "helvetica", size = 13) # Modified
        .vlab <- tklabel(.win, text = "0%", font = fn2, padx = 20)
        tkpack(.vlab, side = "right")
        up <- function(value) {
            if (!is.finite(value) || value < min || value > max) 
                return()
            .val <<- value
            tkconfigure(.vlab, text = sprintf("%d%%", round(100 * 
                (value - min)/(max - min))))
        }
    }else {
        .lab <- tklabel(.win, text = label, font = fn, pady = 10)
        .tkval <- tclVar(0)
        tkpack(.lab, side = "top")
        tkpack(tklabel(.win, text = "", font = fn), side = "bottom")
        pBar <- if (have_ttk) 
            ttkprogressbar(.win, length = width, variable = .tkval)
        else tkwidget(.win, "ProgressBar", width = width, variable = .tkval)
        tkpack(pBar, side = "bottom")
        up <- function(value) {
            if (!is.finite(value) || value < min || value > max) 
                return()
            .val <<- value
            tclvalue(.tkval) <<- 100 * (value - min)/(max - min)
        }
    }
    getVal <- function() .val
    kill <- function() if (!.killed) {
        tkdestroy(.win)
        .killed <<- TRUE
    }
    title <- function(title) tkwm.title(.win, title)
    lab <- function(label) tkconfigure(.lab, text = label)
    tkbind(.win, "<Destroy>", kill)
    up(initial)
    structure(list(getVal = getVal, up = up, title = title, label = lab, 
        kill = kill), class = "tkProgressBar")
}


#################################################################################################################

#############################################################################
###                                                                         #
###                            Menus                                        #
###                                                                         #
#############################################################################

#############################################################################
#                                                                           #
#                         AUXILIARY MENU FUNCTIONS                          #
#                                                                           #
#############################################################################
.tk_winMenuNamesLoad<-function(){
    on.exit(options("show.error.messages"=TRUE))
    menu.file<-paste(gcdx.dir,"menu_GCDkit.r",sep="/")
    ee<-scan(menu.file,what=list(x=""),sep="\n",quiet=TRUE,comment.char="#")
    ee<-unlist(ee)
    x<-ee[grep("winMenuAdd[(]",ee)]
    x<-gsub("winMenuAdd[(]","",x)
    x<-gsub("[)]","",x)
    x<-gsub("^ ","",x)
    x<-gsub("\\\"","",x)
    i<-grep("[/]",x)
    return(x[-i])
}

.tk_winMenuItemsLoad<-function(menuname=""){
   on.exit(options("show.error.messages"=TRUE))
   menu.file<-paste(gcdx.dir,"menu_GCDkit.r",sep="/")
   ee<-scan(menu.file,what=list(x=""),sep="\n",quiet=TRUE,comment.char="#")
   ee<-unlist(ee)
   prd<-strsplit(ee,"\",\"")
   i<- grep(paste("winMenuAddItem[(].",menuname,sep=""),ee)
   if(length(i)==0) return("")
   ee<-matrix(unlist(prd[i]),ncol=3,byrow=TRUE)
   x<-ee[,3]
   x<-gsub("\\\")$","",x)
   x<-gsub("\\","",x,fixed=TRUE) # NEW
   x<-gsub(" $","",x) # NEW
   names(x)<-ee[,2]
   return(x)
}

#############################################################################
#                                                                           #
#                           winMenuNames                                    #
#                                                                           #
#############################################################################
.tk_winMenuNames<-function(){
    on.exit(options("show.error.messages"=TRUE))
    end<-as.numeric(tkindex(topMenu,"last"))
    out<-sapply(1:end,function(i){
        z<-tclvalue(try(tkentrycget(topMenu,i,"-label")))        
        return(z)
    },simplify=TRUE)
    return(out)
}

#############################################################################
#                                                                           #
#                           winMenuItems                                    #
#                                                                           #
#############################################################################
.tk_winMenuItems<-function(menuname){
    on.exit(options("show.error.messages"=TRUE))
    menunameTk<-paste(gsub(" ","",menuname),"Menu",sep="")
    which.menu<-eval(parse(text=menunameTk))
    end<-as.numeric(tkindex(which.menu,"last"))
    if(!is.na(end)){
        out<-sapply(0:end,function(i){
            if(as.character(tktype(which.menu,i))=="command"){
                z<-tclvalue(try(tkentrycget(which.menu,i,"-label")))
            }else{
                z<-"---------------------"
            }
        return(z)
        },simplify=TRUE)
        names(out)<-out
    }else{
        out<-character(0)
    }
    return(out)
}

#############################################################################
#                                                                           #
#                            winMenuAdd                                     #
#                                                                           #
#############################################################################

.tk_winMenuAdd<-function(menuname){
    on.exit(options("show.error.messages"=TRUE))
    menunameTk<-paste(gsub(" ","",menuname),"Menu",sep="")
    if(any(winMenuNames()==menuname)){
        #cat("Menu already exists\n")
        invisible(FALSE)
    }
    
    options("show.error.messages"=FALSE)
    eval(parse(text=paste(menunameTk," <- tkmenu(topMenu,tearoff=TRUE)",sep="")))
    eval(parse(text=paste("tkadd(topMenu,\"cascade\",label=\"",menuname,"\",menu=",menunameTk,")",sep="")))
    invisible(TRUE)
}

#############################################################################
#                                                                           #
#                         winMenuAddItem                                    #
#                                                                           #
#############################################################################
.tk_winMenuAddItem<-function(menuname, itemname, action){
    on.exit(options("show.error.messages"=TRUE))
    if(substr(itemname,1,10)=="----------") return(FALSE) # This is a separator line, nothing to do
    
    menunameTk<-paste(gsub(" ","",menuname),"Menu",sep="")
    if(action=="enable") action<-"active" # In TclTk jargon
    
    # If the menuname parameter of winMenuAddItem does exist already, modify its state
    if(any(names(.tk_winMenuItems(menuname))==itemname)){ 
        # Get the current definition
        #txt<-paste("get(\"",menunameTk,"\",.GlobalEnv)",sep="")
        txt<-menunameTk
        which.menu<-eval(parse(text=txt))
        tkentryconfigure(which.menu,itemname,state=action)
    
    # If the menuname parameter does not exist, new item will be created automatically
    }else{
        cmd<-paste("tkadd(",menunameTk,",\"cascade\",label=\"",itemname,"\",command=function(){",action,"})",sep="")
        eval(parse(text=cmd))
    }
    #assign(menunameTk,which.menu,.GlobalEnv)
    return(TRUE)
}

#############################################################################
#                                                                           #
#                           winMenuDel                                      #
#                                                                           #
#############################################################################

.tk_winMenuDel<-function(menuname){
    on.exit(options("show.error.messages"=TRUE))
    menunameTk<-paste(gsub(" ","",menuname),"Menu",sep="")
    which.menu<-eval(parse(text=menunameTk))
    #tkdelete(tt.menu,whichmenu)
    options("show.error.messages"=FALSE)
    try(tkdelete(which.menu,0,"last"))   # Delete all items in the menu, leave it empty
    invisible(TRUE)
}

#############################################################################
#                                                                           #
#                           winMenuDelItem                                  #
#                                                                           #
#############################################################################
.tk_winMenuDelItem<-function(menuname,itemname){
    on.exit(options("show.error.messages"=TRUE))
    menunameTk<-paste(gsub(" ","",menuname),"Menu",sep="")
    which.menu<-eval(parse(text=menunameTk))
    
    tkdelete(which.menu,itemname)
    invisible(TRUE) 
}
           
#############################################################################
#                                                                           #
#                           MAIN MENU FUNCTION                              #
#                                                                           #
#############################################################################
menuet<-function(){
    on.exit(options("show.error.messages"=TRUE))
    if(.Platform$OS.type=="windows"){
        hist.file <<- tempfile(".Rhistory")
        utils::savehistory(hist.file)
        #savehistory(file = ".Rhistory")
    }
    
    tt.menu <- tktoplevel()
    tkwm.title(tt.menu,"GCDkit: experimental Tcl/Tk (platform-independent) menu")
    tkconfigure(tt.menu,padx=20,pady=20,width=600,height=40)
    
    topMenu <- tkmenu(tt.menu)
    tkconfigure(tt.menu,menu=topMenu)
    
    tkmenu.main<-function(which="",menu=""){    
        eval(parse(text=paste(menu," <- tkmenu(topMenu,tearoff=TRUE)",sep="")))
        ee<-.tk_winMenuItemsLoad(which)
        
        if(length(ee)!=1|ee[1]!=""){
            ee<-as.matrix(ee,ncol=2)
            for(i in 1:nrow(ee)){
                item.name<-rownames(ee)[i]
                fun<-ee[i,1] 
                if(fun=="none") fun<-""
                if(substr(item.name,1,10)=="----------"){
                    cmd<-paste("tkadd(",menu,",\"separator\")",sep="")
                }else{
                    cmd<-paste("tkadd(",menu,", \"command\",label=\"",item.name,"\",command=function(){",fun,"})",sep="")
                }
                eval(parse(text=cmd))
            }       
        }
        cmd<-paste("tkadd(topMenu,\"cascade\",label=\"",which,"\",menu=",menu,")",sep="")
        eval(parse(text=cmd))
        assign(menu,get(menu),.GlobalEnv)
    }
    menus<-.tk_winMenuNamesLoad()
    for(j in 1:length(menus)){
        tkmenu.main(menus[j],paste(gsub(" ","",menus[j]),"Menu",sep=""))  
    }
    
    # If TclTk menus are desired, replace Windows-specific menu functions too
    .assignWithNamespaceHard("winMenuNames",.tk_winMenuNames) 
    .assignWithNamespaceHard("winMenuItems",.tk_winMenuItems) 
    .assignWithNamespaceHard("winMenuAdd",.tk_winMenuAdd)
    .assignWithNamespaceHard("winMenuAddItem",.tk_winMenuAddItem)
    .assignWithNamespaceHard("winMenuDelItem",.tk_winMenuDelItem)
    .assignWithNamespaceHard("winMenuDel",.tk_winMenuDel)
    
    # If on R Gui, remove Win RGui nmenus from Console [if any]
    options("show.error.messages"=FALSE)
    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
        #try(.menu.del())
    }
    assign("topMenu",topMenu,.GlobalEnv)
    assign("tt.menu",tt.menu,.GlobalEnv)
    
    options("gcd.menus"="tcltk")
    options("gcd.shut.up"=TRUE)
    
    tkfocus(topMenu)
    tkraise(tt.menu)
    menuOff()
    invisible(TRUE)
}                     
            
