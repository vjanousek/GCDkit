.onLoad<-function(lib,pkg) {
    on.exit(options("show.error.messages"=TRUE))
    options("warn"=-1)
    fontHeading<<-tkfont.create(family="times",size=12,weight="bold",slant="italic")    
    grDevices::pdf.options(useDingbats=FALSE)
    
    assign("x.data",numeric(0),.GlobalEnv)
    assign("y.data",numeric(0),.GlobalEnv)
    assign(".conflicts.OK",TRUE,.GlobalEnv)
    assign("package.name",pkg,.GlobalEnv)
    assign("selected"," ",.GlobalEnv)
    
    # Safety measure, switch off the buffered output if not in Windows RGUI
    if(.Platform$OS.type=="windows"&.Platform$GUI=="RTerm"){
        options("gcd.menus"="")
        windows.options("buffered"=FALSE)
    }
    
    # Setup the gcdx.dir
    dirpaths<-paste(.libPaths(),"/",package.name,sep="")
    for(ee in dirpaths){
        if(length(dir(ee))!=0) assign("gcdx.dir",ee,.GlobalEnv)
    }

    # Read GCDkit options from a file
    options("show.error.messages"=FALSE)
    ee<-try(.optionsLoad())
    if(class(ee)=="try-error"){
        options("warn"=-1)
        if(.Platform$OS.type=="windows"){
            winDialog(type="ok","Error reading the GCDkit config file!\nSticking to defaults....")
            .setDefault()
            .optionsSave()
            gcdOptions()
        }else{
            packageStartupMessage("=====================================",appendLF = TRUE)
            packageStartupMessage("Error reading the GCDkit config file!\nSticking to defaults....",appendLF = TRUE)
            packageStartupMessage("=====================================\n",appendLF = TRUE)
            .setDefault()
        }
    }
    
    options("demo.ask"=FALSE)
    options("example.ask"=FALSE)  
    
    # Read Rconsole if on Windows RGUI
    if(.Platform$GUI=="Rgui"){
        options("show.error.messages"=FALSE)
        try(loadRconsole(paste(.libPaths(),"/",package.name,"/Rconsole",sep="")))
    }

    # Read in all spiderplots and classification diagrams
    sourceDir("Diagrams","spiderplot",recursive=FALSE)
    sourceDir(paste("Diagrams/Classification",getOption("gcd.language"),sep="/"),"classification diagram",recursive=TRUE)
    
    # If in GCDkit proper, read also geotectonic diagrams and norms
    if(package.name=="GCDkit"|package.name=="GCDkitDevelop"){
        sourceDir("Diagrams/Geotectonic","geotectonic diagram",recursive=TRUE) # Still to do languages in these
        sourceDir("Diagrams/User","user-defined diagram",recursive=TRUE) 
        sourceDir("Norms","normative algorithm")
    }

    # GCDkit Mineral setup
    if(package.name=="GCDkit.Mineral"){
        source(paste(gcdx.dir,"mineral_class.r",sep="/"))
        
        # Standard database
        source(paste(gcdx.dir,"mineral_db.r",sep="/"))
        packageStartupMessage("\nChecking the internal consistency of the database...",appendLF = FALSE)
 
        ClassDef<-getClass("mineral")
        x<-names(ClassDef@subclasses)
        x<-x[x!="USER"]
        ee<-lapply(x,function(i){validObject(new(i),test=TRUE)})
        names(ee)<-x
        if(mode(unlist(ee))!="logical"){
             winDialog(type="ok","ERRORS FOUND! Please edit 'mineral_db.r'!")
             packageStartupMessage("",appendLF = TRUE)
             packageStartupMessage(ee)
             options(show.error.messages=FALSE)
             stop(call. = TRUE)
        }
        packageStartupMessage("...ok\n",appendLF = FALSE)
        
        # User database
        
        # Build a user database if necessary
        #require("XML")
        ee<-dir(data.dir,"user_db.xml")
        if(length(ee)==0){ 
            packageStartupMessage("\nUser database with recalculation options not found! \nCreating a new one on the basis of the standard database...please wait...\n\n")
            minSaveDefaultUserDb()
        # Try to load it
        }else{ 
            ee<-try(.minLoadUserDb())
            if(class(ee)=="try-error"){
                winDialog(type="ok","ERRORS FOUND IN USER DATABASE; sticking to defaults!")
                source(paste(gcdx.dir,"mineral_db.r",sep="/"))
            }
        }
    }
    
    # If in Windows GUI, read menus
    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
        if(length(utils::winMenuNames())>0){
            if(utils::winMenuNames()[1]!=package.name&utils::winMenuNames()[1]!="GCDkit"){
                try(.menu.del()) 
            }else{
                try(utils::winMenuDel("Plugins"))
            }
        }
    }
    #link.html.help()
    
    # Read atomic weights
    x<-data.matrix(read.table(paste(gcdx.dir,"/","MW.data",sep=""),sep="\t"))
    mw<-as.vector(x)
    names(mw)<-rownames(x)
    assign("mw",mw,.GlobalEnv)
    
    assign("sheet",as.list(1),.GlobalEnv)
    assign("plate",as.list(1),.GlobalEnv)
    assign("plate.data",as.list(1),.GlobalEnv)
    assign("WRCube",vector("list",0),.GlobalEnv)
    
    REE<-c("La","Ce","Pr","Nd","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu")
    #REE<-c("La","Ce","Pr","Nd","Pm","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu")
    assign("REE",REE,.GlobalEnv)
    
    major<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","MnO","MgO","CaO","Na2O","K2O","P2O5")
    assign("major",major,.GlobalEnv)
    
#############################################################################
#                                                                           #
#                           SYSTEM OPTIONS                                  #
#                                                                           #
#############################################################################
    # Set palette
    n<-7
    reds<-rgb(r=(2:(n+1))/(n+1), g=0,b=0)
    blues<-rgb(b=(2:(n+1))/(n+1), g=0,r=0)
    greens <- rgb(g=(2:(n+1))/(n+1), r=0,b=0)
    cyans<- rgb(g=(2:(n+1))/(n+1), r=0,b=(2:(n+1))/(n+1))
    violets<- rgb(r=(2:(n+1))/(n+1), g=0,b=(2:(n+1))/(n+1))
    yellows<- rgb(r=(2:(n+1))/(n+1), g=(2:(n+1))/(n+1),b=0)
    std<-(c("black","red","green3","blue","cyan","magenta","yellow"))
    additional<-matrix(nrow=7,ncol=6,byrow=TRUE,c(paste("gray",seq(40,90,10),sep=""),reds[-1],greens[-1],blues[-1],cyans[-1],violets[-1],yellows[-1]))
    
    eee<-as.vector(cbind(std,additional))
    palette.gcdkit<-adjustcolor(eee)
    #palette(col2rgb(palette.gcdkit)) # opens unwanted graphical window but I cannot fix it
    assign("palette.gcdkit",palette.gcdkit,.GlobalEnv)
    
    jet.colors<-colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F","yellow", "#FF7F00", "red", "#7F0000")) 
    assign("jet.colors",jet.colors,.GlobalEnv)
      
    # Patching
    packageStartupMessage("Patching.... please wait",appendLF = TRUE)
    try(.loadPatch())
    invisible() 
}
# END OF .ONLOAD

.onAttach<-function(lib, pkg) {
    on.exit(options("show.error.messages"=TRUE))
    options("show.error.messages"=FALSE)
    

    
    if(.Platform$OS.type=="windows"&.Platform$GUI=="RTerm"){
        options("gcd.menus"="")
    }
     # Load the EarthChem interface
    .EarthChemStart()
    
    # Startup messages - OS type (windows or linux)
    if(.Platform$OS.type=="windows"){
        packageStartupMessage("\nWindows version: ",win.version(),appendLF = TRUE)
    }else{
        packageStartupMessage("\nPlatform: ",.Platform$OS.type,appendLF = TRUE)
    }
    
    packageStartupMessage("R version: ",R.version$platform,paste(getRversion(),collapse="."),appendLF = TRUE)
    ee<-as.character(packageDescription("GCDkit", fields = c("Version","Built"))) ##### GCDkit or GCDkitDevelop TO BE FIXED IN FINAL VERSION, DO NOT FORGET TO AMEND .RProfile!!!!!
    
    packageStartupMessage("\nGeochemical Data Toolkit (GCDkit) ",ee[1],",\nbuilt ",ee[2], appendLF = TRUE)
   
    packageStartupMessage("\nPlease support our efforts and cite the package 'GCDkit' in publications\nusing the reference below. Type 'citation(\"GCDkit\")' for BibTex version.", appendLF = TRUE)  
    packageStartupMessage("=======================================================================",appendLF = TRUE)
    packageStartupMessage("Vojtech Janousek, Colin M. Farrow and Vojtech Erban (2006).",appendLF = TRUE)
    packageStartupMessage("Interpretation of whole-rock geochemical data in igneous geochemistry:",appendLF = TRUE)
    packageStartupMessage("introducing Geochemical Data Toolkit (GCDkit).",appendLF = TRUE)
    packageStartupMessage("Journal of Petrology 47(6): 1255-1259.",appendLF = TRUE)
    packageStartupMessage("doi: 10.1093/petrology/egl013",appendLF = TRUE)
    packageStartupMessage("=======================================================================",appendLF = TRUE)
    
    # Mackintosh and other non-Windows systems, Windows RTerm
    if(.Platform$OS.type!="windows"|.Platform$GUI=="RTerm"){
            packageStartupMessage("=======================================================================",appendLF = TRUE)
            packageStartupMessage("A platform different from RGUI!\nStill highly experimental... Expect troubles.",appendLF = TRUE)
            packageStartupMessage("=======================================================================\n",appendLF = TRUE)
            
            #if(.Platform$GUI=="AQUA"){assign("windows",quartz,.GlobalEnv)}
            #if(.Platform$GUI=="X11"){assign("windows",x11,.GlobalEnv)}
            
            # rescale is not available in quartz. Perhaps filter our some other paremters, too? 
            if(.Platform$GUI=="AQUA"){
                fun<-function(){
                    args<-list(...)
                    args$rescale<-NULL
                    do.call("quartz",args)   
                }
                assign("windows",fun,.GlobalEnv)
            }
           
            # Analogous safety measure for x11
            if(.Platform$GUI=="X11"){
                fun<-function(){
                    args<-list(...)
                    args$rescale<-NULL
                    do.call("x11",args)   
                }
                assign("windows",fun,.GlobalEnv)
            }
           
            # Just working:
            # sessionInfo()
            # system("defaults write org.R-project.R force.LANG en_US.UTF-8") # Set UTF-8 locale on Mac, https://stat.ethz.ch/pipermail/r-sig-mac/2010-February/007106.html
    }
    # End of Mackintosh and other non-Windows systems
    
    # TclTk interface, possible also on Windows systems if specifically desired
     if(.Platform$OS.type!="windows"|.Platform$GUI=="RTerm"|getOption("gcd.menus")=="tcltk"){
        #My Tcl/Tk replacement for Windows-specific dialogue boxes, see file tcltk_replacement4win_functions.r
        .assignWithNamespaceHard("winDialog",tk_winDialog)
        .assignWithNamespaceHard("winDialogString",tk_winDialogString)
        .assignWithNamespaceHard("select.list",tk_select.list)
        .assignWithNamespaceHard("winProgressBar",.tk_ProgressBar)
        .assignWithNamespaceHard("setWinProgressBar",setTkProgressBar)
        .assignWithNamespaceHard("choose.files",tk_choose.files)
        .assignWithNamespaceHard("choose.dir",tk_choose.dir)
        packageStartupMessage("\nTcl/Tk interface initialized.",appendLF = TRUE)
    }
    
    # PDF options
    pdf.options(useDingbats=FALSE) # IMPORTANT
    pdf.options(pointsize=14)
    
    windows()
    palette(palette.gcdkit) # opens unnecessary window
    graphics.off() # switch it off

    # If TclTk menus are desired, run the function "menuet" that will replace Windows-specific menu functions too
    if(getOption("gcd.menus")=="tcltk"){ 
        # Start Tcl/Tk menus 
        menuet()
    }else{
 
    # If we are either using Windows RGui menus, show them and switch most items off
    #if(getOption("gcd.menus")=="win"){
        if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
            menu.file<-paste(gcdx.dir,"menu_GCDkit.r",sep="/")
            ee<-source(menu.file)
            #assign(".Menu",.Menu,.GlobalEnv)
            .Menu()
            menuOff()
            options("gcd.menus"="win")
        }  
    }
       
    packageStartupMessage("Ready 2 Go - Enjoy!\n", appendLF = FALSE)
    
    # Bring the Tcl/Tk menus to front, if present
    if(getOption("gcd.menus")=="tcltk"){
    #    tkfocus(topMenu)
        if(.Platform$OS.type=="windows"){
            utils::loadhistory(file = hist.file)
            #loadhistory(file = ".Rhistory")
        }
        tkraise(tt.menu)
    }
    menuOff()
    invisible()
}
# END OF .ONATTACH

#.onDetach<-function(){ # DOES NOT WORK
#    # If on R Gui, remove Win RGui nmenus from Console [if any]
#    options("show.error.messages"=FALSE)
#    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
#        try(.menu.del())
#    }
#    options("show.error.messages"=TRUE)
#    invisible()
#}

# SOLVING THE PROBLEM OF LOCKED labels AND SUCH ALIKE IN THE base LIBRARY 
# VERY VERY DIRTY BUT VERY VERY IMPORTANT
.assignWithNamespaceHard<-function(x,value,ns=asNamespace("base")){
        assign(x,value,.GlobalEnv)
        unlockBinding(x,ns)
        assign(x,value, envir = ns, inherits = TRUE)
}
### End of the dirty hack

.setDefault<-function(){
        # Normal R options
        options(prompt=paste(package.name,"-> ",sep=""))
        options(windowsBuffered=FALSE)
        options(locatorBell=FALSE)
        options(scipen=20)
        options(max.print=99999999)
        options(warn=-1)
        
        # GCDkit-specific options   
        options("gcd.language"="English")
        options("gcd.plot.text"=TRUE)
        options("gcd.plot.bw"=FALSE)
        options("gcd.digits"=3)
        options("gcd.ident"=0)
        options("gcd.ident.each"=TRUE)
        options("gcd.cex"=1)
        options("gcd.shut.up"=FALSE)
        # GCDkit menus, possible values are "" "tcltk", "win"
        options("gcd.menus"="")
        
        # These are to stay
        assign("data.dir",getwd(),.GlobalEnv)
        assign("plt.col",c("blue","brown","navyblue"),.GlobalEnv)
}

#.setPSDefault<-function(){
#        #ps.options(paper="special",horizontal=TRUE,bg="transparent",width=170/25.4,useKerning=FALSE,encoding="native.enc",pointsize=10,colormodel="rgb")
#        #ps.options(paper="special",horizontal=TRUE,bg="transparent",width=170/25.4,useKerning=FALSE,encoding="CP1250.enc",pointsize=10,colormodel="rgb")
#        #pdf.options(useKerning=FALSE,encoding="native.enc")
#        pdf.options(useDingbats=FALSE)
#        pdf.options(pointsize=14)
#}        

setCex<-function(x=NULL){
    on.exit(options("show.error.messages"=TRUE))
    if(is.null(x)) x<-"prd"
    if(is.na(as.numeric(x))){
        x<-winDialogString("Scaling for symbols",as.character(labels$Size)[1])
        if(is.null(x)){cat("Cancelled.\n");options(show.error.messages=FALSE);stop(call. = TRUE)} 
    }
    x<-rep(as.numeric(x),times=nrow(WR))
   labels[,"Size"]<-x
   .assignWithNamespaceHard("labels",labels)
}

setShutUp<-function(){
    x<-winDialog("Minimize printed output?",type="yesno")
    options("gcd.shut.up"=(x=="YES"))
    invisible()
}

################################################################################################
# Add missing columns ("what") to a matrix "where", fill with "value" 
    addOn<-function(what,value=NA,where=WR){
        on.exit(options("show.error.messages"=TRUE))
        options(show.error.messages = FALSE)
        do.it<-function(where,what,value){
            i<-is.na(where[,what])
            #try(where[,what][i]<-value[i])
            #return(where)
            try(where[,what][i]<<-value[i]) # TRY to replace NA's by a sensible value
        }
        for(f in what){
            if(any(colnames(where)==f)){
                res<-try(do.it(where,what,value))
            }else{
                where<-try(cbind(where,value))
                try(colnames(where)[length(colnames(where))]<-f)
            }
        }
    return(where)
}

quitGCDkit<-function(){
    if(winDialog(type = "yesno","Really quit GCDkit?" )=="YES")quit(save="no")
    invisible()     
}


#############################################################################
#           Turns numbers in names of oxides to subscripts                  #
#############################################################################
.annotate.main<-function(a){
    on.exit(options("show.error.messages"=TRUE))
    #a<- c("K2O","P2O5")
    #a<-"SiO2 wt%"
    # Isotopic ratios
    if(substring(a,1,10)=="87Sr/86Sri")return(srlab)
    if(substring(a,1,12)=="143Nd/144Ndi")return(ndlab)
    if(substring(a,1,6)=="EpsNdi")return(epsNdlabi)
    if(substring(a,1,6)=="EpsHfi")return(epsHflabi)

    if(substring(a,1,11)=="147Sm/144Nd")return(expression(" "^147*Sm/" "^144*Nd))
    if(substring(a,1,11)=="143Nd/144Nd")return(expression(" "^143*Nd/" "^144*Nd))
    
    if(substring(a,1,9)=="87Rb/86Sr")return(expression(" "^87*Rb/" "^86*Sr))
    if(substring(a,1,9)=="87Sr/86Sr")return(expression(" "^87*Sr/" "^86*Sr))
    
    if(substring(a,1,11)=="206Pb/204Pb")return(expression(" "^206*Pb/" "^204*Pb))
    if(substring(a,1,11)=="207Pb/204Pb")return(expression(" "^207*Pb/" "^204*Pb))
    if(substring(a,1,11)=="208Pb/204Pb")return(expression(" "^208*Pb/" "^204*Pb))
    if(substring(a,1,11)=="207Pb/206Pb")return(expression(" "^207*Pb/" "^206*Pb))
    
    if(substring(a,1,10)=="206Pb/238U")return(expression(" "^206*Pb/" "^238*U))
    if(substring(a,1,10)=="207Pb/235U")return(expression(" "^207*Pb/" "^235*U))
    if(substring(a,1,11)=="208Pb/232Th")return(expression(" "^208*Pb/" "^232*Th))
    
    #a<-gsub("Eu/Eu[*]","Eu/Eu\u002A",a) 
    
    # Saturation temperatures
    if(substring(a,1,18)=="TZr.sat.C(Boehnke)")return(expression("Zircon saturation T (Boehnke et al. 2013) (\u00B0C)"))
    if(substring(a,1,9)=="TZr.sat.C")return(expression("Zircon saturation T (Watson and Harrison 1983) (\u00B0C)"))
    if(substring(a,1,9)=="Tmz.sat.C")return(expression("Monazite saturation T (\u00B0C)"))
    
    if(substring(a,1,13)=="Tap.sat.C.H&W")return(expression("Apatite saturation T (Harrison & Watson 1984) (\u00B0C)"))
    if(substring(a,1,14)=="Tap.sat.C.Pich")return(expression("Apatite saturation T (Pichavant et al. 1992) (\u00B0C)"))
    if(substring(a,1,13)=="Tap.sat.C.Bea")return(expression("Apatite saturation T (Bea et al. 1992) (\u00B0C)"))

    # delta values (stable isotopes)
    a<-gsub("(delta)([0-9]{1,})([A-Z][a-z]?)","delta^\\2\\3",a)
    
    # normalized elemental ratios
    a<-gsub("([A-Z])([a-z]?)N/([A-Z])([a-z]?)N","\\1\\2[N]/\\3\\4[N]",a)
    
    #a<-gsub("(LaN/YbN)","La[N]/Yb[N]",a)
    a<-gsub("Sum_REE","Sigma*REE",a)
    
    # Legal search pattern
    #options("show.error.messages"=FALSE)
    #    ee<-try(selectSubset(a,save=FALSE))
    #    if(class(ee)!="try-error"){
    #        options("warn"=-1) 
    #        a<-paste("Search pattern:",a)          
    #        return(a)
    #    }
    
    #if(length(grep("([:,])",a))>0) a<-paste("Samples~~","list(",a,")",sep="")
    #if(length(as.numeric(a))>0 & !is.na(as.numeric(a))) a<-paste("Sample~~",a,sep="")
    
    a<-gsub(" ","~",a) # Replace space by ~
    a<-gsub("#","",a)  # by nil
    
    #.100
     a<-gsub("([.])([0-9]{1,}$)","(\\2\\3)",a)
    
    # Logical operators
    a<-gsub("[.]OR[.]","~or~",a) 
    a<-gsub("[.]AND[.]","~and~",a)
    a<-gsub("([|])","~or~",a)
    a<-gsub(" & ","~and~",a)
    a<-gsub("&","~and~",a)
    #a<-gsub("([,])","~",a)
    a<-gsub("([0-9a-zA-Z()]])([=])","\\1==",a) # = by ==
    a<-gsub("([0-9][*][0-9])","@",a) # * by @
    
    a<-gsub("([a-zA-Z])([0-9])","\\1[\\2",a) # left [  bracket after letter and before digit
    a<-gsub("([[])([0-9])([-a-zA-Z@+(~])","\\1\\2]*\\3",a) # right ]  bracket after left [, digit, and before next letter
    a<-gsub("([[])([0-9])([,)])","\\1\\2]\\3",a) # right ]  bracket after left [, digit, and before next comma or right bracket
    a<-gsub("([[])([0-9])([<>=])","\\1\\2]\\3",a) # right ]  bracket after left [, digit and [<>=]
    #a<-gsub("([0-9])([a-zA-Z])","\\1*\\2\\",a) 
    #a<-gsub("([0-9])([a-zA-Z.])","\\1*\\2\\",a)
    a<-gsub("([0-9])([a-zA-Z$])","\\1*\\2\\",a)
    a<-gsub("([[])([0-9])([)/])","\\1\\2]\\3",a)
   
    a<-gsub("[*][@]","@",a)
    a<-gsub("([@])","\\%*\\%",a)
    a<-gsub("[*][*]","*",a) # ** by *
    
    # Add * between up to 3. digits and text character
    a<-gsub("([0-9])([0-9])([0-9])([a-zA-Z])","\" \"^\\1\\2\\3*\\4",a)
    a<-gsub("([0-9])([0-9])([a-zA-Z])","\" \"^\\1\\2*\\3",a)
    a<-gsub("([0-9])([a-zA-Z])","\" \"^\\1*\\2",a)    
    
    # space after subscript
    a<-gsub("([]])(*~~)","\\1*{  }*",a) # Brand new
    
    left<-length(unlist(strsplit(paste(" ",a),"\\[")))-1
    right<-length(unlist(strsplit(paste(a," "),"\\]")))-1
        if (left>right){a<-paste(a,"]",sep="")}
    
    # Percent symbol
    a<-gsub("([%])","\"\u0025\"",a) # Brand new
    
    a<-gsub("([*][~]{1,}$)","",a) # Brand new, remove trailing spaces in the end
    
    a<-gsub("Eu/Eu[*],","Eu/Eu,",a)  
    a<-gsub("Eu/Eu[*]","Eu/Eu[*]",a)  
    options("show.error.messages"=FALSE)
    out<-try(parse(text=as.expression(a)))
    if(class(out)=="try-error") stop("",call. = TRUE)
    return(out)
}

annotate<-function(text){
    try.annotate<-function(x){.annotate.main(x)}

    if(is.expression(text))return(text)
    
    out<-sapply(text,function(j){
        res<-try(try.annotate(j),silent=TRUE)
        if((class(res))!="expression"){
            err<-res[1]
            res<-j
        }else{
            res<-res[1]
        }
        return(res)
    },simplify=FALSE)
    names(out)<-NULL
    #out<- unlist(out)
    if(length(text)==1){
        return(out[[1]])
    }else{
        out<- as.expression(unlist(out))
    }
    return(out)
}

#############################################################################
#       Finds all columns in [where] whose name appears in [what]           #
#          eliminates all rows that have more at least [n] NAs              #
#                   (by default completely empty rows)                      #
#############################################################################


filterOut<-function(where,what,n=length(what)){
    ii<-as.vector(pmatch(what,colnames(where)))
    names(ii)<-what
    where<-subset(where,TRUE,ii,drop=FALSE)
    #where<-where[,ii]

    if(length(ii)==1){
        ee<-apply(where,1,is.na)
        yy<-where
        yy<-subset(yy,!ee)
    }else{
        ee<-apply(where,1,is.na)
        ee<-apply(as.matrix(ee),2,sum)
        yy<-subset(where,ee<n,drop=FALSE)
        #yy<-where[ee<n,]
    }
   
    colnames(yy)<-what
    return(yy)
}


#############################################################################
#                        Choose a column of the labels                      #
#############################################################################


selectColumnLabel<-function(where=colnames(labels),message="Select the variable\nor press ENTER to pick from a list",default="",sample.names=FALSE,silent=FALSE,print=TRUE,empty.ok=TRUE){
    on.exit(options("show.error.messages"=TRUE))
    if(sample.names)where<-c("Sample name",where)

    names(where)<-1:length(where)

    if(print&!getOption("gcd.shut.up")){
        cat("Variable names:\n")
        print(where)
        cat("\n")
    }
    x<-winDialogString(message,default)
    if(is.null(x)){cat("Cancelled.\n");options(show.error.messages=FALSE);stop(call. = TRUE)}
    if(x==""&empty.ok)return(NULL)
    xx<-gsub("\\*","\\\\*",x)
    
    if(!is.na(as.numeric(xx))){
        selected<-as.numeric(xx)
        if (selected>length(where)|selected<0){ # bylo <=0
            selected<-x
            if(!silent){
                winDialog(type="ok","Your query produced no matches!")
                options(show.error.messages=FALSE)
                stop("",call. = FALSE)
            }
        }
        return(selected)
    }
    
            options(show.error.messages=FALSE)
            selected<-try(grep(xx,where,fixed=TRUE))
            if(class(selected)=="try-error"){
                winDialog(type="ok","Syntax error in search pattern!")
                stop("",call. = FALSE)
            }

    options(show.error.messages=TRUE)
    if(length(selected)==0){
        selected<-x
        if(!silent){
            winDialog(type="ok","Your query produced no matches!")
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


    if(length(selected)==1&x!=where[selected]){
        selectColumnLabel(where,message,default=where[selected],sample.names,silent,print=FALSE)
    }


    if((length(selected)==length(where))&empty.ok){
        selected<-NULL
        return(selected)
    }


    if(length(selected)>1){
        selected.old<-where[selected]
               
        selected<-select.list(where[selected],preselect=x)
        if(selected==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop(call. = TRUE)}

        #selected<-gsub("[*]","\\\\*",selected)
        #selected<-gsub("[+]","\\\\+",selected)
        #selected<-as.numeric(names(selected.old)[grep(selected,selected.old,value=FALSE,extended=TRUE)[1]])
        #selected<-as.numeric(names(selected.old)[grep(paste("^",selected,"$",sep=""),selected.old)[1]])
        selected<-as.numeric(names(selected.old)[grep(paste("\\A\\Q",selected,"\\E\\Z",sep=""),selected.old,perl=TRUE)[1]])

        if(length(selected)==0){
            selected<-x
            if(!silent){
                winDialog(type="ok","Your query produced no matches!")
                stop("",call. = FALSE)
            }
        }
    }
return(selected)
}

selectColumnsLabels<-function(where=colnames(WR),message="Select variable(s), e.g. 'SiO2,TiO2,MgO'\nor press ENTER to pick from a list",default="",print=TRUE,exact.only=TRUE){
    on.exit(options("show.error.messages"=TRUE))
    if(exact.only& nchar(default)>0){ 
        if(length(default)==1){
            #default<-where[!is.na(match(where,unlist(strsplit(default,","))))]
        }else{
            #default<-where[!is.na(match(where,default))]
        }
    }

    if(length(default)>1) default<-paste(text=default,collapse=",")

    selected<-selectColumnLabel(where,message=message,default=default,sample.names=FALSE,print=print,silent=TRUE)
    
     if(is.numeric(selected)){ # is a single variable and exact match
        ii<-where[selected]
     }else{
        if(is.null(selected)){ # empty input line
                ii<-select.list(where,multiple=TRUE)
                if(length(ii)==0) {
                    winDialog(type = "ok", "No data available")
                    options("show.error.messages"=FALSE)
                stop(call. = TRUE)
                }
        }else{
                selected<-gsub("[ ]{1,}","",selected)
                ii<-unlist(strsplit(selected,","))
                ee<-grep("[:]",ii)
                if(!is.na(as.numeric(ii))|length(ee)>0) { # numeric range
                    options("show.error.messages"=FALSE)
                    ii<-try(eval(parse(text=paste("c(",paste(ii,collapse=","),")",sep=""))),silent=TRUE)
                    if(class(ii)=="try-error"){
                        ii<-selectColumnsLabels(where,message="Variable(s) not found! Please correct....",default=selected,print=FALSE,exact.only)
                        return(ii)
                    }
                    options("show.error.messages"=TRUE)
                    ii<-where[ii]
                    ii<-ii[!is.na(ii)]
                    ii<-selectColumnsLabels(where=where,default=ii)
                }
        }
    }

    options(show.error.messages=FALSE)
    if(length(ii)==1 & !any(where==ii)){ # single variable, to be calculated
        ee<-gsub("[*]","\\*",ii)
        ee<-try(eval(parse(text=ii)))
        if(class(ee)== "try-error" & exact.only==TRUE){
            ii<-selectColumnsLabels(where,message="Variable(s) not found! Please correct....",default=selected,print=FALSE,exact.only)
            return(ii)
       }else{
            return(ii)
       }
    }
    
    #More variables
    if(is.na(all(match(ii,where))) & exact.only==TRUE){
            ii<-selectColumnsLabels(where,message=paste("Variable(s) not found! Please correct....\nMissing: ",paste(ii[!ii%in%where],collapse=", ")),default=selected,print=FALSE,exact.only)
            return(ii)
    }
    return(unique(ii))
}

#############################################################################
#                                                                           #
#                             RESULTS                                       #
#                                                                           #
#############################################################################


saveResults<-function(what=results,sep="\t",digits=2){
    on.exit(options("show.error.messages"=TRUE))
    if(is.null(what)) {winDialog(type = "ok", "No data available");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    options(show.error.messages=FALSE)
    filename<-choose.files(caption = "Select file",multi = FALSE, filters=matrix(ncol=2,byrow=TRUE,c("GCDkit text files (*.txt)","*.txt")))
    
    # Check for file overwrite
    ee<-file.access(filename, mode = 0)
    if(ee==0){
        eee<-winDialog(type="yesno","File exists! Overwrite?")
        if(eee=="NO"){cat("Cancelled.\n");stop(call. = FALSE)}
    }
    
    if(filename==""){cat("Cancelled.\n");stop(call. = TRUE)}
    options(show.error.messages=TRUE)

    if(is.table(what)){
            if(length(attributes(results)$dim)!=2){
                write.ftable(ftable(what),file=filename,quote=TRUE,digits=digits)
                return()
            }else{
            what<-as.matrix(what)
            }
    }

    if(is.list(what)){
        for(i in 1:length(what)){
            write.table(t(c(names(what)[i],colnames(what[[i]]))),row.names=FALSE,filename,sep="\t",quote=FALSE,col.names=FALSE,na="",append=TRUE)
            write.table(data.frame(what[i]),filename,sep="\t",quote=FALSE,col.names=FALSE,na="",append=TRUE)
         }
         return()
    }
    write.matrix(cbind(rownames(what),round(what,digits)),file=filename, sep=sep)
    winDialog("ok","File saved successfully")
}


addResults<-function(what="results",save=TRUE,overwrite=TRUE,GUI=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    if(is.null(eval(parse(text=what)))) {winDialog(type = "ok", "No data available");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        
    # FILTRER OUT everything that is not numeric
    xx<-eval(parse(text=what))
    if(is.vector(xx)){ # New
        xx<-as.matrix(xx,ncol=1)
        colnames(xx)<-what
    }
    xxx<-xx
    xxx[is.na(xxx)]<-1
    # Store safely colnames, even a single one
    ee<-colnames(xx)
    
    which.num<-!is.na(apply(xxx,2,function(i){all(as.numeric(i))}))
    x<-as.matrix(xx[,which.num])
    
    #if(ncol(x)==1){
    #    colnames(x)<-what
    #}else{
    #    ee<-colnames(x)
    #    ee<-gsub("[ ]?","",ee)
    #    colnames(x)<-ee
    #}
    
    ee<-gsub("[ ]?","",ee) # NEW
    colnames(x)<-ee        # NEW
    
    ii<-which(colnames(WR)%in%colnames(x))
    if(length(ii)>0 & GUI){ 
            ee<-winDialog(type = "okcancel",paste("Variables ",paste(colnames(WR)[ii],collapse=", "),"\n\nwill be overwritten by a new version!",sep=""))
            if(ee=="OK"){
                WR<-WR[,-ii]
            }else{
                options("show.error.messages"=FALSE)
                stop(call. = TRUE)
            }
    }else{
        if(length(ii)>0 & overwrite) WR<-WR[,-ii] 
    }
        ee<-merge(WR,x,by=0,all.x=TRUE)
        rownames(ee)<-ee[,1]
        ee<-ee[,-1]
        ee<-ee[rownames(WR),]
        if(save){
            assign("WR",data.matrix(ee),.GlobalEnv)
            #WR<<-data.matrix(ee)
            txt<-paste("Column(s) ",paste(colnames(x),collapse=", ")," were successfuly added to the data.",sep="")
            if(GUI){
                winDialog(type = "ok",txt)
            }else{
                if(!getOption("gcd.shut.up"))cat(txt,"\n")
            }
        }   
    invisible(data.matrix(ee))
}



#############################################################################
#                                                                           #
#                           PLOT FUNCTIONS                                  #
#                                                                           #
#############################################################################


#############################################################################
#                      Calculates and plots triplot                         #
#############################################################################


#    TERNARY DIAGRAM
#          B
#        /   \
#       A-----C
# Parameters:
#   analyses to be plotted [aa,bb,cc] - the function returns these recast to sum = 1
#   labels for apices [alab,blab,clab]
#   title of the whole diagram [title]
#   interval of grid lines [grid.int], if set to zero, no grid is drawn
#   interval of ticks [tick.int], if set to zero, no ticks are drawn
#   shall the axes be labelled by % of A, B, C? [label.axes], the values are chosen according to tick.int or grid.int, if NA by 10%
#   plotting symbols [pch]
#     their colour [col]
#   shall be the individual points identified by their names?  [identify]

triplot<-function(aa,bb,cc,alab,blab,clab,title="",grid.int=0,tick.int=0,label.axes=FALSE,line=FALSE,pch=labels[names(aa),"Symbol"],col=labels[names(aa),"Colour"],cex = labels[names(aa),"Size"],identify=getOption("gcd.ident"),new=TRUE,...){
    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma

    temp1<-list(
        lines1=list("lines",x=c(0,1,.5,0),y=c(0,0,sqrt(3)/2,0),col="black"),
        A=list("text",x=0,y=-0.03,text=annotate(alab),adj=0.5,cex=1),
        B=list("text",x=0.5,y=sqrt(3)/2+.03,text=annotate(blab),adj=0.5,cex=1), #NEW
        C=list("text",x=1,y=-0.03,text=annotate(clab),adj=0.5,cex=1),           # NEW
        #B=list("text",x=1,y=-0.03,text=annotate(clab),adj=0.5,cex=1),
        #C=list("text",x=0.5,y=sqrt(3)/2+.03,text=annotate(blab),adj=0.5,cex=1),
        GCDkit=list("NULL",plot.type="ternary",plot.name=paste("Ternary plot",alab, "-", blab, "-", clab))
    )

    ######### Plot grid ####################
    temp.grid<-list()
    if(grid.int>0){
        inter<-seq(grid.int,1-grid.int,by=grid.int)
        for(i in seq(1,1/grid.int-1,by=1)){
            AA<-c(inter[i],inter[i])
            CC<-c(0,inter[1/grid.int-i])
            BB<-1-AA
            ee<-list(
                eval(parse(text=paste("temp.grid$gridlines",4*(i-1)+1,"<-list(\"lines\",x=c(",CC[1]+AA[1]/2,",",CC[2]+AA[2]/2,"),y=c(",sqrt(3)*AA[1]/2,",",sqrt(3)*AA[2]/2,"),col=\"black\",lwd=1,lty=3)",sep=""))),
                eval(parse(text=paste("temp.grid$gridlines",4*(i-1)+2,"<-list(\"lines\",x=c(",AA[1]+CC[1]/2,",",AA[2]+CC[2]/2,"),y=c(",sqrt(3)*CC[1]/2,",",sqrt(3)*CC[2]/2,"),col=\"black\",lwd=1,lty=3)",sep=""))),
                eval(parse(text=paste("temp.grid$gridlines",4*(i-1)+3,"<-list(\"lines\",x=c(",CC[1]+(BB[1]-CC[1])/2,",",CC[2]+(BB[2]-CC[2])/2,"),y=c(",sqrt(3)*(BB[1]-CC[1])/2,",",sqrt(3)*(BB[2]-CC[2])/2,"),col=\"black\",lwd=1,lty=3)",sep="")))
            )
        }
    }


    ######### Plot text ####################
    temp.text<-list()
    if(label.axes){
        if(tick.int==0) lab.int<-.1 else lab.int<-tick.int
        inter<-seq(lab.int,1-lab.int,by=lab.int)
        for(i in seq(1,1/lab.int-1,by=1)){
            ee<-list(
               eval(parse(text=paste("temp.text$textlabels",4*(i-1)+1,"<-list(\"text\",x=",inter[i]/2-sqrt(3)/2*0.03,",y=",sqrt(3)*inter[i]/2+0.015,",text= \"",100*inter[i],"\",cex=0.7,srt=60,adj=c(1,0))",sep=""))),
               eval(parse(text=paste("temp.text$textlabels",4*(i-1)+2,"<-list(\"text\",x=",1-inter[i]+inter[i]/2+sqrt(3)/2*0.03,",y=",sqrt(3)*inter[i]/2+0.015,",text= \"",ceiling(100*(1-inter[i])),"\",cex=0.7,srt=-60,adj=c(0,0))",sep=""))),
               eval(parse(text=paste("temp.text$textlabels",4*(i-1)+3,"<-list(\"text\",x=",inter[i],",y=-0.03,text= \"",ceiling(100*(1-inter[i])),"\",cex=0.7)",sep="")))
            )
        }
    }

    ######### Plot ticks ####################
    temp.tick<-list()
    if(tick.int>0){
        inter<-seq(tick.int,1-tick.int,by=tick.int)
        for(i in seq(1,1/tick.int-1,by=1)){
            ee<-list(
            eval(parse(text=paste("temp.tick$ticklines",7*(i-1)+1,"<-list(\"lines\",x=c(",inter[i]/2,",",inter[i]/2+0.030,"),y=c(",sqrt(3)*inter[i]/2,",",sqrt(3)*inter[i]/2,"),col=\"black\",lwd=1,lty=1)",sep=""))),
            eval(parse(text=paste("temp.tick$ticklines",7*(i-1)+2,"<-list(\"lines\",x=c(",inter[i]/2,",",inter[i]/2+0.015,"),y=c(",sqrt(3)*inter[i]/2,",",sqrt(3)*inter[i]/2-sqrt(3)*0.015,"),col=\"black\",lwd=1,lty=1)",sep=""))),
            eval(parse(text=paste("temp.tick$ticklines",7*(i-1)+3,"<-list(\"lines\",x=c(",1-inter[i]+inter[i]/2,",",1-inter[i]+inter[i]/2-0.030,"),y=c(",sqrt(3)*inter[i]/2,",",sqrt(3)*inter[i]/2,"),col=\"black\",lwd=1,lty=1)",sep=""))),
            eval(parse(text=paste("temp.tick$ticklines",7*(i-1)+4,"<-list(\"lines\",x=c(",1-inter[i]+inter[i]/2,",",1-inter[i]+inter[i]/2-0.015,"),y=c(",sqrt(3)*inter[i]/2,",",sqrt(3)*inter[i]/2-sqrt(3)*0.015,"),col=\"black\",lwd=1,lty=1)",sep=""))),
            eval(parse(text=paste("temp.tick$ticklines",7*(i-1)+5,"<-list(\"lines\",x=c(",inter[i],",",inter[i]+0.015,"),y=c(0,",sqrt(3)*0.015,"),col=\"black\",lwd=1,lty=1)",sep=""))),
            eval(parse(text=paste("temp.tick$ticklines",7*(i-1)+6,"<-list(\"lines\",x=c(",inter[i],",",inter[i]-0.015,"),y=c(0,",sqrt(3)*0.015,"),col=\"black\",lwd=1,lty=1)",sep="")))
            )
        }
    }

    temp<-c(temp1,temp.grid,temp.tick,temp.text)
    #main<-paste("Ternary plot",alab,"-",blab,"-",clab)
    main<-NULL
    
    if(!new) bg<-"white" else bg<-"transparent"
    
    sheet<<-list(demo=list(fun="plot",call=list(pch=pch,col=col,cex=cex,xlim=c(-.03,1.03),ylim=c(-0.08,1.03),bg=bg,fg="black",asp=1,axes=FALSE,xlab="",ylab="",main=main,new=new),template=temp))

    x.data<<-cc+bb/2
    y.data<<-sqrt(3)*bb/2
    
    pp<<-figaro(demo,prefix="sheet")
    sheet$demo$call$new<-new
    if(!new) sheet$demo$call$bg<-"white"
    pp$draw(x.data,y.data,main=main,xlab="",ylab="",xaxs="i",yaxs="i",new=new)
    
    #NEW
        sheet$demo$call$pch<-rep(pch,length.out=length(x.data))
        sheet$demo$call$col<-rep(col,length.out=length(x.data))
        sheet$demo$call$cex<-rep(cex,length.out=length(x.data))
    #/NEW
    assign("sheet",sheet,.GlobalEnv)
    
    if(screen()){
        plate[[screen()]]<<-sheet
        plate.data[[screen()]]$x<<-x.data
        plate.data[[screen()]]$y<<-y.data
    }else{
         figaroOn()
    }
    
    if(getOption("gcd.ident")){
        ID(x.data,y.data)
    }
   
    ee<-cbind(aa,bb,cc)
    return(ee)
}


#############################################################################
#                  Adds points to already existing triplot                  #
#############################################################################


# Parameters:
#   analyses to be plotted [aa,bb,cc] - the function returns these recast to sum = 1
#   plotting symbols [pch]
#     their colour [col]
#   shall be the individual points identified intercatively by their names?  [ident]

triplotadd<-function(aa,bb,cc,pch=labels[names(aa),"Symbol"],col=labels[names(aa),"Colour"],cex=labels[names(aa),"Size"],labs=NULL,identify=FALSE,lines=FALSE,lty="solid",type="p"){
    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma
    if(lines){
        lines(cc+bb/2,sqrt(3)*bb/2,col=col,bg="black",pch=pch,lty=lty)
    }else{
        points(cc+bb/2,sqrt(3)*bb/2,col=col,bg="black",pch=pch,cex=cex,type=type)
    }
    
    if(!is.null(labs)) text(cc+bb/2,sqrt(3)*bb/2,labs,cex=0.7,pos=3)

    if(identify){
        cat("Identification in progress\n")
        x<-identify(cc+bb/2,sqrt(3)*bb/2,rownames(WR),offset=0.1,cex=0.2,col="darkred")
        print(labels[x,])
    }
    return(cbind(aa,bb,cc))
}



#############################################################################
#       Read the normalizing file and select the normalization scheme       #
#############################################################################
selectNorm<-function(ref=NULL,elems="Rb,Sr,Ba,Cr,Ni,La,Ce,Y,Zr",REE.only=FALSE,multiple=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    normal<<-scan(paste(gcdx.dir,"/spider.data",sep=""),skip=1,what=list(mname="",elements="",nvalues=""),sep="\n",quiet=TRUE)
    if(!is.null(ref)){
        #x<-grep(ref,normal$mname)
        ee<-gsub("[ ]",".*",ref)
        x<-grep(ee,normal$mname,value=FALSE,ignore.case=TRUE) #extended=TRUE,
        
        if(length(x)==0){
            # Failed? Try sample names! NEW
            selected<-grep(ref,rownames(WR),value=TRUE,ignore.case=FALSE)

            # General failure
            if(length(selected)==0){
                cat("No matching normalizing scheme found.\n")
                options(show.error.messages=FALSE);stop(call. = TRUE)
            }
            
            elems<-unlist(strsplit(elems,","))
            elems<-elems[elems%in%colnames(WR)]
            temp<-WR[selected,elems,drop=FALSE]
            chondrit<-as.matrix(t(apply(temp,2,mean,na.rm=TRUE)))
            
            if(nrow(temp)>1){    
                rownames(chondrit)<-paste("average of ",paste(selected,collapse=", "),sep="")
                x<-2
            }else{
                rownames(chondrit)<-selected
                x<-1
            }
            
        }else{
            x<-x+2
        }
        
        # end of NEW
        if(length(x)>1&!multiple){
                cat("Ambigious, several matching normalizing schemes found.\n")
                print(normal$mname[x-2]) # Fixed
                options(show.error.messages=FALSE);stop(call. = TRUE)
        }
        
    # ref not provided, select from list 
    }else{
        ee1<-c(">> SAMPLE",">> MEAN OF (SPECIFIED) SAMPLES",normal$mname)
    
        if(REE.only){
            ee<-normal$mname[grep("^REE",normal$mname)]
        }else{
            ee<-ee1 
        }
        x<-select.list(ee)
        if(x==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop(call. = TRUE)}
    
        x<-which(ee1==x)
    
        # By sample
        if(x==1){
            cat("Sample names:\n")
            print(rownames(WR))
            norm<-""
            while(is.na(match(norm,rownames(WR)))){
                norm<-selectColumnLabel(rownames(WR),message="Select a sample to be used for normalization",default="",sample.names=FALSE,silent=TRUE,empty.ok=FALSE,print=FALSE)
                if(is.numeric(norm))norm<-rownames(WR)[norm]
            }
              
            if(length(norm)==0){
                norm<-select.list(rownames(WR),multiple=FALSE)
                if(norm==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop(call. = TRUE)}
            }
            if(is.numeric(norm))norm<-rownames(WR)[norm]
        }

        # By sample or average - select the elements
        if(x<=2){
            elems<-unlist(strsplit(elems,","))
            elems<-elems[elems%in%colnames(WR)]
            elems<-paste(elems,collapse=", ")
            elems<-selectColumnsLabels(colnames(WR),default=elems)
            if(is.null(elems)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
            elems<-unlist(strsplit(elems,","))
        }

        # By average
        if(x==2){
            where<-selectSubset(text="Average of which samples?\n(enter a search pattern)",save=FALSE,GUI=TRUE)
            temp<-WR[where,elems]
            chondrit<-as.matrix(t(apply(temp,2,mean,na.rm=TRUE)))
            rownames(chondrit)<-paste("average of ",selected,sep="")
            colnames(chondrit)<-elems
            print(chondrit)
            cat("\n")
            selected<<-""
        }

        if(x==1){
            chondrit<-t(as.matrix(WR[norm,elems]))
            rownames(chondrit)<-norm
            colnames(chondrit)<-elems
        }
    }
    # end of select from list
       
        if((all(x>2))){ #|!is.null(ref)){ NEW
            x<-x-2
            aa<-strsplit(normal$elements[x],",")
            bb<-strsplit(normal$nvalues[x],",")
            model<-normal$mname[x]
            
            if(!multiple){
                aa<-aa[[1]]
                bb<-bb[[1]]
                bb<-as.numeric(bb)
                names(bb)<-aa
                chondrit<-as.matrix(bb)
                chondrit<-t(chondrit)
                rownames(chondrit)<-model
            }else{
                chondrit<-lapply(1:length(bb),function(i){
                    z<-as.numeric(bb[[i]])
                    names(z)<-as.character(aa[[i]])    
                    return(z)
                })
                names(chondrit)<-model
            }
            
            
        }
        assign("chondrit",chondrit,.GlobalEnv)
        return(chondrit)
}


#############################################################################
#                  Fills an area with cross-hatched pattern                  #
#############################################################################


.srafuj<-function(x,y,density=0.05,angle=0,degrees=FALSE,lty=1,lwd=1,col="black"){

# Working only for spiderplots
# x,y - vertices of the polygon
# density - density of fill pattern
# angle - angle of fill pattern
# degrees - is the angle in degrees (TRUE) or radians (FALSE)?

    x<-x[!is.na(y)]
    y<-y[!is.na(y)]
    lines(x,y,col=col)
    old.coord<-par("usr")
    y<-log10(y)/(old.coord[4]-old.coord[3])-old.coord[3]/(old.coord[4]-old.coord[3])
    x<-x+(1-old.coord[1]) #VI aby to pracovalo i s xaxs="r"
    x<-x/(old.coord[2]-old.coord[1])-1/(old.coord[2]-old.coord[1])
    par(ylog=FALSE)
    par(usr=c(0,1,0,1))
    if (degrees) angle<-(angle*pi/180)
        mat<-rbind(x,y)
        matice<-matrix(c(cos(-angle),sin(-angle),-sin(-angle),cos(-angle)),2,2)
        rotmat<-matice%*%mat
        x<-rotmat[1,]
        y<-rotmat[2,]
        y<-c(y,y[2])
        srafypoc<-matrix(nrow=2)
        srafykon<-matrix(nrow=2)
        for (a in seq(min(y)+density,max(y),by=density)){
            meze<-vector()
            for (k in 1:(length(x)-1)){
                if (y[k]!=y[k+1]){
                    if ((y[k+1]!=a)|(sign(y[k+1]-y[k])!=(sign(y[k+2]-y[k+1])))){
                    z<-approx(c(y[k],y[k+1]),c(x[k],x[k+1]),a)
                        if (is.na(z$y)==FALSE) meze<-c(meze,z$y)
                    }
                }
            }
            meze<-sort(meze)
            for (k in 1:(length(meze)/2)){
                srafypoc<-cbind(srafypoc,c(a,meze[2*k-1]))
                srafykon<-cbind(srafykon,c(a,meze[2*k]))
            }
        }
    srafypoc<-(matice)%*%srafypoc
    srafykon<-(matice)%*%srafykon
        for (k in 1:ncol(srafypoc)){
            lines(c(srafypoc[2,k],srafykon[2,k]),c(srafypoc[1,k],srafykon[1,k]),lwd=lwd,lty=lty,col=col)
        }
    par(ylog=TRUE)
    par(usr=old.coord)
}




#############################################################################
#                      Multiple plots (Harkers by default)                  #
#############################################################################
# Plot multiple binary plots with the same x axis
#
# Parameters:
#   common x axis [x]
#   all y axes [y]
#   plotting symbols [pch]
#     their colour [col]


multiple<-function(x,y=paste(colnames(WR),sep=","),samples=rownames(WR),pch=labels$Symbol,col=labels$Colour,xmin=NULL,xmax=NULL,GUI=FALSE,nrow=NULL,ncol=NULL,...){
   on.exit(options("show.error.messages"=TRUE))
   if(is.numeric(samples)) samples<-rownames(WR)[samples]
   if(GUI){ 
        xlab<-selectColumnLabel(colnames(WR),message="x-axis, e.g. (SiO2+2.1)*TiO2/3\nor press ENTER to pick from a list",empty.ok=FALSE,silent=TRUE,default=x)    
        if(is.null(xlab)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        if(!is.na(as.numeric(xlab)))xlab<-colnames(WR)[as.numeric(xlab)]
    }else{
        xlab<-x
    }
    
    if(any(colnames(WR)==xlab)){
        x<-WR[,xlab]
    }else{
        ee<-calcCore(xlab)
        x<-ee$results
        xlab<-ee$equation
    }
    if(GUI){
        ylab<-selectColumnsLabels(colnames(WR),message="y-axes",default=y,exact.only=FALSE)
    }else{
        ylab<-unlist(strsplit(y,","))
    }
    ylab<-ylab[ylab!=xlab]
    
    if(is.na(pmatch(xlab,ylab))){
        delka<-length(ylab)
    }else{
        delka<-length(ylab)-1
    }

    if(GUI){
        where<-selectSamples(print=FALSE)
        #if(!getOption("gcd.shut.up")) print(where)
    }else{
        where<-samples
    }
    if(is.numeric(where))where<-rownames(WR)[where]
    
    
    # Set limits for x axis
    if(is.null(xmin)){
        digits.x<--as.integer(log10(max(x[where],na.rm=TRUE))-1)
        xmin=.round.min.down(x[where],digits.x)
    }
    
    if(is.null(xmax)){
        digits.x<--as.integer(log10(max(x[where],na.rm=TRUE))-1)
        xmax=.round.max.up(x[where],digits.x)
    }
    
    if(GUI){
        xx<-winDialogString("Enter minimum and maximum for x-axis, separated by commas",paste(xmin,xmax,sep=","))
        if(is.null(xx)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        xmin<-as.numeric(unlist(strsplit(xx,","))[[1]])
        xmax<-as.numeric(unlist(strsplit(xx,","))[[2]])
    }else{
        if(is.null(ncol)) ncol<-n2mfrow(delka)[1]
        if(is.null(nrow)) nrow<-n2mfrow(delka)[2]
    }
    
    if(length(pch)==1) pch<-rep(pch,times=length(samples))
    if(length(col)==1) col<-rep(col,times=length(samples))    
    names(col)<-samples
    names(pch)<-samples
    # GO
    title<-paste("Multiple plot of ",xlab," vs. ","list(",paste(ylab,collapse=","),")",sep="")
    multiplePerPage(delka,title=title,dummy=FALSE,nrow=nrow,ncol=ncol)

    for (f in 1:delka){
        screen(f)
        #par(mar=c(6,3.5,2,1.5))
        par(mar=c(4.5,4.5,1,1))
        par(pty="s")
        if (xlab!=ylab[f]){
            if(any(colnames(WR)==ylab[f])){
                y<-WR[,ylab[f]]
            }else{
                ee<-calcCore(ylab[f])
                y<-ee$results
                #f<-ee$equation
            }
            
            ylim<-pretty(range(y[where],na.rm=TRUE))
            ylim<-c(ylim[1],ylim[length(ylim)])            

            if(any(!is.na(x[where]))&any(!is.na(y[where]))){
                delta<-0.05*abs(xmax-xmin)
                plotWithLimits(x[where],y[where],xlab=annotate(xlab),ylab=annotate(ylab[f]),xmin=xmin-delta,xmax=xmax+delta,pch=pch[where],ymin=ylim[1],ymax=ylim[2],col=col[where],new=FALSE,...)
            }
        }
    }
    scr.old<<-screen()
    # Title for the whole plate
    mtext(text=annotate(plate$title),side=3,line=0.25,outer=TRUE,cex=1.5)
    
    # If in Windows GUI, read menus
    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
        figaroOff()
        .menuPopUp()
    }
    invisible()
}


multipleMjr<-function(x = "", y = "SiO2,TiO2,Al2O3,FeOt,MgO,CaO,Na2O,K2O,P2O5", pch = labels$Symbol, col = labels$Colour, ...){
    GUI<-x==""
    if(x=="" & package.name=="GCDkit" & any(colnames(WR)=="SiO2")) x<-"SiO2"
    y<-colnames(WR)[!is.na(match(colnames(WR),unlist(strsplit(y,","))))]
    if(length(y)>1) y<-paste(text=y,collapse=",") else y<-""
    multiple(x,y,pch=pch,col=col,GUI=GUI)
}


multipleTrc<-function(x = "", y = "Rb,Sr,Ba,Cr,Ni,La,Ce,Y,Zr,mg#,A/CNK,K2O/Na2O", pch = labels$Symbol, col = labels$Colour, ...){
    GUI<-x==""
    if(x=="" & package.name=="GCDkit"&any(colnames(WR)=="SiO2")) x<-"SiO2"
    y<-colnames(WR)[!is.na(match(colnames(WR),unlist(strsplit(y,","))))]
    if(length(y)>1) y<-paste(text=y,collapse=",") else y<-""
    multiple(x,y,pch=pch,col=col,GUI=GUI)
}




#############################################################################
#                          Custom binary plot                               #
#############################################################################
binary<-function(x=NULL,y=NULL,log="",samples=rownames(WR),new=TRUE,...){
    on.exit(options("show.error.messages"=TRUE))
    #if(screen())new<-FALSE
    # xaxis
    if(is.null(x)){
        log<-NULL
        samples<-NULL
        xlab<-selectColumnLabel(colnames(WR),message="x-axis, e.g. (SiO2+2.1)*TiO2/3\nor press ENTER to pick from a list",default="",sample.names=FALSE,silent=TRUE,empty.ok=FALSE)
    }else{
        xlab<-x
    }
    
    if(!is.na(as.numeric(xlab))){
        xlab<-colnames(WR)[xlab]
        x<-WR[,xlab]
    }else{
        ee<-calcCore(xlab)
        x<-ee$results
        xlab<-ee$equation
    }
    if(all(is.na(x))) {z<-winDialog("ok",paste("The data",xlab,"is empty!")); return()}
    
    # yaxis
    if(is.null(y)){
        ylab<-selectColumnLabel(colnames(WR),message="y-axis \n(press ENTER to pick from a list)",default="",sample.names=FALSE,silent=TRUE,print=FALSE,empty.ok=FALSE)
    }else{
        ylab<-y
    }
    if(!is.na(as.numeric(ylab))){
        ylab<-colnames(WR)[ylab]
        y<-WR[,ylab]
    }else{
        ee<-calcCore(ylab)
        y<-ee$results
        ylab<-ee$equation
    }
    if(all(is.na(y))) {z<-winDialog("ok",paste("The data",ylab,"is empty!")); return()}
    
    # log
    if(is.null(log)){
        z<-" "
        while(z!=""&z!="x"&z!="y"&z!="xy"){
            z<-winDialogString("logarithmic are, eg. x, or xy","")
            if(is.null(z)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        }
        log<-z
    }
    
    # select subset for plotting
    if(is.null(samples)){
        samples<-selectSamples(print=FALSE)
        #if(!getOption("gcd.shut.up")) print(samples)
    }
    x<-x[samples]
    y<-y[samples]
    
    # scaling
    #rangex<-log10(diff(range(x,na.rm=TRUE)))
    #if(rangex<0) digits.x<-round(abs(rangex),0) else digits.x=0
    xlim<-pretty(range(x,na.rm=TRUE))
    xlim<-c(xlim[1],xlim[length(xlim)])

    #rangey<-log10(diff(range(y,na.rm=TRUE)))
    #if(rangey<0) digits.y<-round(abs(rangey),0) else digits.y=0
    args<-list(...)
    
    ylim<-pretty(range(y,na.rm=TRUE))
    ylim<-c(ylim[1],ylim[length(ylim)])
    
    args$x.data<-x
    args$y.data<-y
    if(is.null(args$xlab)) args$xlab<-xlab
    if(is.null(args$ylab)) args$ylab<-ylab
    if(is.null(args$log)) args$log<-log
    if(is.null(args$new)) args$new<-new
    if(is.null(args$xmin)) args$xmin<-xlim[1]
    if(is.null(args$xmax)) args$xmax<-xlim[2]
    if(is.null(args$ymin)) args$ymin<-ylim[1]
    if(is.null(args$ymax)) args$ymax<-ylim[2]
    
    xx<-try(eval(do.call("plotWithLimits",args)))
    
    # If plotting in a plate #NEW BY VJ
    if(screen()){
        options(show.error.messages=FALSE)
        ee<-screen()
        scr.old<<-ee
        if(ee<length(plate.data)) screen(ee+1,new=FALSE)
    }
    
    if(class(xx)=="try-error"){
        #options("show.error.messages"=FALSE)
        #stop(call. = FALSE)
        invisible()
     }
}


#############################################################################
#                          Custom ternary plot                              #
#############################################################################
ternary<-function(x=NULL,y=NULL,z=NULL,samples=rownames(WR),new=TRUE,grid=FALSE,ticks=TRUE,...){
    #if(screen())new<-FALSE
    # Bottom left apex
    if(is.null(x)){
        samples<-NULL
        grid<-NULL
        ticks<-NULL
        xlab<-selectColumnLabel(colnames(WR),message="X (bottom left apex), \ne.g. 1000*Ba or press ENTER to pick from a list",default="",sample.names=FALSE,silent=TRUE,empty.ok=FALSE)    
    }else{
        xlab<-x
    }
    
    if(!is.na(as.numeric(xlab))){
        xlab<-colnames(WR)[xlab]
        x<-WR[,xlab]
    }else{
        ee<-calcCore(xlab)
        x<-ee$results
        xlab<-ee$equation
    }

    # Top apex
    if(is.null(y)){
        ylab<-selectColumnLabel(colnames(WR),message="Y (top apex), \ne.g. 1000*Ba or press ENTER to pick from a list",default="",sample.names=FALSE,silent=TRUE,print=FALSE,empty.ok=FALSE)
    }else{
        ylab<-y
    }
    
    if(!is.na(as.numeric(ylab))){
        ylab<-colnames(WR)[ylab]
        y<-WR[,ylab]
    }else{
        ee<-calcCore(ylab)
        y<-ee$results
        ylab<-ee$equation
    }
    
    # Bottom right apex
    if(is.null(z)){
        zlab<-selectColumnLabel(colnames(WR),message="Z (bottom right apex), \ne.g. 1000*Ba or press ENTER to pick from a list",default="",sample.names=FALSE,silent=TRUE,print=FALSE,empty.ok=FALSE)
    }else{
        zlab<-z
    }
    if(!is.na(as.numeric(zlab))){
        zlab<-colnames(WR)[zlab]
        z<-WR[,zlab]
    }else{
        ee<-calcCore(zlab)
        z<-ee$results
        zlab<-ee$equation
    }

    # Plot grid?
    if(is.null(grid)){
        ee<-winDialog("Plot grid?",type="yesno")
        grid<-(ee=="YES")
    }
    
    # Plot ticks?
    if(is.null(ticks)){
        ticks<-FALSE
        if(!grid){
            ee<-winDialog("Plot ticks?",type="yesno")
            ticks<-(ee=="YES")
        }
    }

    # select subset for plotting
    if(is.null(samples)){
        samples<-selectSamples(print=FALSE)
        #if(!getOption("gcd.shut.up")) print(samples)
    }
    x<-x[samples]
    y<-y[samples]
    z<-z[samples]


    if(grid){
        grid.int<-0.1
    }else{
        grid.int<-0
    }

    if(ticks){
        tick.int<-0.1
    }else{
        tick.int<-0
    }
    
    #main<-paste("Ternary plot",xlab,"-",ylab,"-",zlab)
    #windows(width = 6.5, height = 6.5, pointsize = 10,title=main)
    ee<-triplot(x,y,z,xlab,ylab,zlab,new=new,grid.int=grid.int,label.axes=FALSE,tick.int=tick.int,...)
    
    # If plotting in a plate #NEW BY VJ
    if(screen()){
        options(show.error.messages=FALSE)
        ee<-screen() 
        scr.old<<-ee
        if(ee<length(plate.data)) screen(ee+1,new=FALSE)
    }
    invisible()
}


#############################################################################
#                        Custom coplotByGroup                             #
#############################################################################
coplotByGroup<-function(xaxis="",yaxis="",show.leg=""){
    if(nchar(xaxis)==0) xaxis<-selectColumnLabel(colnames(WR),message="x-axis, e.g. (SiO2+2.1)*TiO2/3\nor press ENTER to pick from a list",default="",sample.names=FALSE,silent=TRUE,empty.ok=FALSE)
    if(!is.na(as.numeric(xaxis))){
        xaxis<-colnames(WR)[xaxis]
        x.data<-WR[,xaxis]
    }else{
        ee<-calcCore(xaxis)
        x.data<-ee$results
        xaxis<-ee$equation
    }


    if(nchar(yaxis)==0) yaxis<-selectColumnLabel(colnames(WR),message="y-axis \n(press ENTER to pick from a list)",default="",sample.names=FALSE,silent=TRUE,print=FALSE,empty.ok=FALSE)
    if(!is.na(as.numeric(yaxis))){
        yaxis<-colnames(WR)[yaxis]
        y.data<-WR[,yaxis]
    }else{
        ee<-calcCore(yaxis)
        y.data<-ee$results
        yaxis<-ee$equation
    }


    if(nchar(show.leg)==0) {show.leg<-winDialog("Show legend?",type="yesno");show.leg<-(show.leg=="YES")}


    windows(width = 6.5, height = 6.5, pointsize = 10)
    data<-data.frame(cbind(x.data,y.data))
    colnames(data)<-c("x","y")

    ee<-parse(text=paste(parse(text="y"),"~",parse(text="x"),"|",parse(text="group"),sep=""))
    ee<-eval(ee)

    group<-factor(groups)
    coplot(ee,data=data,xlab=annotate(xaxis),ylab=annotate(yaxis),col=labels$Colour,pch=labels$Symbol,show.given=show.leg)
}


#############################################################################
#                 Custom coplot with third element                          #
#############################################################################


coplotTri<-function(xaxis="",yaxis="",zaxis="",int=""){
    on.exit(options("show.error.messages"=TRUE))
    if(nchar(xaxis)==0) xaxis<-selectColumnLabel(colnames(WR),message="x-axis, e.g. (SiO2+2.1)*TiO2/3\nor press ENTER to pick from a list",default="",sample.names=FALSE,silent=TRUE,empty.ok=FALSE)
    if(!is.na(as.numeric(xaxis))){
        xaxis<-colnames(WR)[xaxis]
        x.data<-WR[,xaxis]
    }else{
        ee<-calcCore(xaxis)
        x.data<-ee$results
        xaxis<-ee$equation
    }


    if(nchar(yaxis)==0) yaxis<-selectColumnLabel(colnames(WR),message="y-axis \n(press ENTER to pick from a list)",default="",sample.names=FALSE,silent=TRUE,print=FALSE,empty.ok=FALSE)
    if(!is.na(as.numeric(yaxis))){
        yaxis<-colnames(WR)[yaxis]
        y.data<-WR[,yaxis]
    }else{
        ee<-calcCore(yaxis)
        y.data<-ee$results
        yaxis<-ee$equation
    }


    if(nchar(zaxis)==0) zaxis<-selectColumnLabel(colnames(WR),message="split by",default="",sample.names=FALSE,silent=TRUE,print=FALSE,empty.ok=FALSE)
    if(!is.na(as.numeric(zaxis))){
        zaxis<-colnames(WR)[zaxis]
        z.data<-WR[,zaxis]
    }else{
        ee<-calcCore(zaxis)
        z.data<-ee$results
        zaxis<-"Calculated" #ee$equation
    }


    if(nchar(int)==0){
        int<-winDialogString("specify cutoff values (separated by commas)","auto")
        if(is.null(int)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    }


    if(int!="auto"){
        int<-as.numeric(strsplit(int,",")[[1]])
        if(any(is.na(int))){
                winDialog(type="ok","Error in intervals!")
                options(show.error.messages=FALSE)
                stop(call. = TRUE)
        }
        if(min(z.data,na.rm=TRUE)<min(int))int<-c(min(z.data,na.rm=TRUE),int)
        if(max(z.data,na.rm=TRUE)>max(int,na.rm=TRUE))int<-c(int,max(z.data,na.rm=TRUE))


        ind<-matrix(nrow=length(int)-1,ncol=2)
        for(i in 1:length(int)-1){
            ind[i,]<-c(int[i],int[i+1])
        }
    }

    windows(width = 6.5, height = 6.5, pointsize = 10)

    data<-data.frame(cbind(x.data,y.data,z.data))
    colnames(data)<-c("x","y",zaxis)
    ee<-parse(text=paste(parse(text="y"),"~",parse(text="x"),"|",parse(text=zaxis),sep=""))
    ee<-eval(ee)

    if(int=="auto"){
         coplot(ee,data=data,xlab=annotate(xaxis),ylab=annotate(yaxis),col=labels$Colour,pch=labels$Symbol,show.given=TRUE,main="Title",cex=labels$Size)
    }else{
        coplot(ee,data=data,xlab=annotate(xaxis),ylab=annotate(yaxis),given.values=ind,col=labels$Colour,pch=labels$Symbol,show.given=TRUE,main="Title",cex=labels$Size)
    }
}


#############################################################################
#                      Plots binary xy diagrams                             #
#   with the size of the plotting symbols proportional to third variable (z)#
#############################################################################
plotWithCircles<-function(xaxis="",yaxis="",zaxis="",colour="heat.colors",scaling.factor=NULL,bins=NULL,ident=getOption("gcd.ident")){
    on.exit(options("show.error.messages"=TRUE))
    
    # If GUI, select samples and x axis
    if(nchar(xaxis)==0) {
        where<-selectSamples(print=!getOption("gcd.shut.up"))
        xaxis<-selectColumnLabel(colnames(WR),message="x-axis, e.g. (SiO2+2.1)*TiO2/3\nor press ENTER to pick from a list",default="",sample.names=FALSE,silent=TRUE,empty.ok=FALSE)
        GUI<-TRUE
    }else{
        where<-rownames(WR)
        GUI<-FALSE
    }
    if(!getOption("gcd.shut.up")) print(where)
        
    # x data
    if(!is.na(as.numeric(xaxis))){
        xaxis<-colnames(WR)[xaxis]
        x.data<-WR[,xaxis]
    }else{
        ee<-calcCore(xaxis)
        x.data<-ee$results
        xaxis<-ee$equation
    }

    # y axis
    if(nchar(yaxis)==0) yaxis<-selectColumnLabel(colnames(WR),message="y-axis \n(press ENTER to pick from a list)",default="",sample.names=FALSE,silent=TRUE,print=FALSE,empty.ok=FALSE)
    if(!is.na(as.numeric(yaxis))){
        yaxis<-colnames(WR)[yaxis]
        y.data<-WR[,yaxis]
    }else{
        ee<-calcCore(yaxis)
        y.data<-ee$results
        yaxis<-ee$equation
    }

     # z axis
    if(nchar(zaxis)==0) zaxis<-selectColumnLabel(colnames(WR),message="circles represent \n(press ENTER to pick from a list)",default="",sample.names=FALSE,silent=TRUE,print=FALSE,empty.ok=FALSE)
    if(!is.na(as.numeric(zaxis))){
        zaxis<-colnames(WR)[zaxis]
        z.data<-WR[,zaxis]
    }else{
        ee<-calcCore(zaxis)
        z.data<-ee$results
        zaxis<-ee$equation
    }

    #Get the data ready for selected samples
    ee<-cbind(x.data,y.data,z.data)
    ee<-ee[where,]
    ii<-ee[apply(!is.na(ee),1,all),]   
    x.data<-ii[,1]
    y.data<-ii[,2]
    z.data<-ii[,3]

    #if(min(z.data)<0)stop("Cannot handle negative data. Quitting...",call. = FALSE)

    xmin<-min(x.data,na.rm=TRUE)
    xmax<-max(x.data,na.rm=TRUE)
    ymin<-min(y.data,na.rm=TRUE)
    ymax<-max(y.data,na.rm=TRUE)

    ee<-abs(xmax-xmin)

    ff<-max(z.data,na.rm=TRUE)-min(z.data,na.rm=TRUE)
    shift.z<-min(z.data,na.rm=TRUE)
    
    #Scaling factor
    if(GUI){
        scaling.factor<-abs(round(ee/ff/30,7))
        scaling.factor<-as.numeric(winDialogString("scale circles by factor",as.character(scaling.factor)))
        if(length(scaling.factor)==0){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    }else{
        if(is.null(scaling.factor)) scaling.factor<-abs(round(ee/ff/30,7))
    }
   
    if(!is.null(bins)){    
        leg<-pretty(z.data,n=bins-1)
    }else{
        leg<-pretty(z.data)
    }
    
    if(GUI){
        colour<-NULL
    }
    
    nlevels<-length(leg)   
    col<-selectPalette(nlevels,colour)
    
    # Plot legend for z
    windows(width = 9, height = 6, pointsize = 10,title=paste("Plot of",xaxis,"vs.",yaxis,"with circles representing",zaxis))
    nf <- layout(matrix(c(1,1,2,2,2,2,1,1,2,2,2,2), 2,6,byrow=TRUE))
    layout.show(nf)

    ystep<-(ymax-ymin)/(nlevels+1)
    symbols(rep((xmin+xmax)/2,nlevels-1),ymax-(1:(length(leg)-1)*ystep),circles=(leg[-1]-shift.z)*scaling.factor*2.25,axes=FALSE,xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab="",ylab="",main=annotate(zaxis),bg = col,fg="gray30",inches=FALSE,cex.main=1.8)
    legenda<- paste(as.character(leg)[2:length(leg)-1],"-",as.character(leg)[2:length(leg)])
    box()
    text(rep(xmax-(xmax-xmin)/5,length(legenda)),ymax-(1:length(legenda)*ystep),legenda,cex=1.5)

    # Plot the diagram
    symbols(x.data, y.data, circles=(z.data-shift.z)*scaling.factor,xlim=c(xmin,xmax),ylim=c(ymin,ymax),inches=FALSE, bg = col[findInterval(z.data,leg,rightmost.closed=TRUE)],fg="gray30",xlab=annotate(xaxis),ylab=annotate(yaxis),cex.axis=1.3,cex.lab=1.3)
    if(ident)ee<-ID(x.data,y.data,col="black")
    palette(as.vector(palette.gcdkit))
    figaroOff()
}



#############################################################################
#            Pairs with correlation coeffs in upper panel                   #
#############################################################################


# put (absolute) correlations on the upper panels,
# with size proportional to the correlations.
panel.cor <- function(x, y, digits=2, prefix="",cex.cor,...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y,use="pairwise.complete.obs"))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r)
}


panel.cov<-function(x,y,...){
    usr<-par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    text(0.5, 0.5,round(cov(x,y,use="pairwise.complete.obs"),3))
}


panel.hist <- function(x,col="khaki",...){
         usr <- par("usr")
         on.exit(par(usr))
         par(usr = c(usr[1:2], 0, 1.5) )
         h <- hist(x, plot = FALSE)
         breaks <- h$breaks; nB <- length(breaks)
         y <- h$counts; y <- y/max(y)
         rect(breaks[-nB], 0, breaks[-1], y, col=col,density=NULL)
}


.panel.my<-function(x,y,labs,...){
    text(x,y,annotate(labs),cex=1.2)
}


pairsCorr<-function(elems=major){
    on.exit(options("show.error.messages"=TRUE))
    # Select samples
    where<-selectSamples(print=FALSE)
    if(!getOption("gcd.shut.up")) print(where)

    # Select elements
    y<-selectColumnsLabels(default=elems)
    x<-filterOut(WR[where,],y,n=1)
    # Transform data to log ratios?
    #x<-.log.transform(x,y)


    # Select function for upper panel
    y<-select.list(c("panel.cor","panel.cov","panel.smooth","panel.hist"),multiple=FALSE)
     if(y==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop(call. = TRUE)}
    fun<-parse(text=y)

    windows(width = 7, height = 7, pointsize = 10)
    par(oma=c(1,1,1,1))
    pairs(x,pch=labels[rownames(x),"Symbol"],col=labels[rownames(x),"Colour"],upper.panel=eval(fun),text.panel=.panel.my)
}


pairsMjr<-function(){
    pairsCorr("SiO2,TiO2,Al2O3,FeOt,MgO,CaO,Na2O,K2O")
}


pairsTrc<-function(){
    pairsCorr("Rb,Sr,Ba,Cr,La,Zr,mg#,A/CNK")
}


# Transform data to log ratios? DOES NOT WORK!!!!!!
.log.transform<-function(x,elems){
    log.ratio<-(winDialog(type="yesno","Use log ratios with common denominator?"))
    if(log.ratio=="YES"){
        elems<-y
        denom<-selectColumnLabel(where=elems,message="Select denominator",empty.ok=FALSE)
        denominator<-elems[denom]
        elems<-elems[-denom]
        x<-(x/x[,denominator])
        x<-x[,elems]
    }
        return(x)
}


#############################################################################
#                     Correlation coefficient patterns                      #
#                 (Rollinson 1993 and references therein)                   #
#############################################################################
correlationCoefPlot<-function(elems=NULL){
    # Select elements
    if(is.null(elems)){
        elems<-selectColumnsLabels(default=c(LILE,HFSE))
        #intersect(names(mw),colnames(WR)))
    }else{
        elems<-unlist(strsplit(elems,","))
    }
    
    # Cycle for groups
    z<-levels(factor(groups))
    for(f in 1:length(z)){
        x<-WR[labels[,grouping]==z[f],elems]
        if(nrow(rbind(x,x))>4){
            ee<-cor(x,use="pairwise.complete.obs")
            cols<-n2mfrow(length(elems))[1]
            rows<-n2mfrow(length(elems))[2]
            windows(width = 2.5*cols, height = 2.5*rows, pointsize = 10)
            par(mfrow=c(rows,cols))
            par(pty="s")
            par(oma=c(1,1,1,1))
            par(mar=c(5,4,1,1))
            for (i in 1:length(elems)){
                ee[i,i]<-NA
                col<-labels$Colour[labels[,grouping]==z[f]]
                plot(ee[i,],type="o",ylab=elems[i],ylim=c(-1,1),col=col,axes=FALSE,xlab="",main=z[f])
                axis(1, 1:length(colnames(ee)),labels=colnames(ee))
                axis(2,)
                if(i>1 & i<length(elems))lines(c(i-1,i+1),c(ee[i,i-1],ee[i,i+1]),lty="dashed",col=col)
            }
        }
    }
}


#############################################################################
#                                                                           #
#                              STATISTCS                                    #
#                                                                           #
#############################################################################

.summary.my<-function(where,elems){
    on.exit(options("show.error.messages"=TRUE))
    where<-as.matrix(where)
    colnames(where)<-elems
    desc<-function(x){
            y<-as.numeric(9)
            y[3]<-mean(x,na.rm=TRUE)
            options(show.error.messages=FALSE)
            ee<-try(sd(x,na.rm=TRUE))
            if(class(ee)=="try-error"){
               y[4]<-NA
            }else{ 
                y[4]<-ee
            }
            options(show.error.messages=TRUE)
            y<-c(y,quantile(x,na.rm=TRUE))
    return(y)
    }
    
    descript<-t(sapply(elems,function(f){
        what<-where[,f]
        n<-length(what[!is.na(what)])
        y<-desc(what)
        y[1]<-n
        y[2]<-length(what)-n
        return(y)
    }))
    
    #descript<-descript[-1,]

    dig<-.round.digits(descript) 
    descript<-round(descript,dig) 
    if(is.vector(descript)){descript<-as.matrix(t(descript))}
    ee<-log10(min(descript[,4],na.rm=TRUE))

    if(ee==Inf) ee<--4
    if(ee>0){
        digits<-0
    }else{
        digits<-floor(abs(ee)+2)
    }
    #descript<-round(descript,digits) #Predelat
    rownames(descript)<-elems
    colnames(descript)<-c("n","NA","Mean","Std","Min","25%","50%","75%","Max")
    return(descript)
}


#############################################################################
#            Calculates simple descriptive statistics by group              #
#                     plots boxplots and histograms                         #
#############################################################################
#
# Parameters:
#   which elements/oxides [elems]
#   each analysis has assigned a group in a vector [groups]
#   which groups are to be shown [which]
#   plot boxplots? [boxplots]
#   plot histograms? [histograms]
#   print table of results? [print.table]


statistics<-function(where=WR,elems,groups=rep("",nrow(where)),which="",boxplots=TRUE,histograms=TRUE,print.table=TRUE,sub=""){
    results<-by(where[,elems],factor(groups),.summary.my,elems=elems)
    for (f in elems){
            what<-where[groups==which,f]
            n<-length(what[!is.na(what)])

            if (boxplots){
                if(n>1)boxplot(what,names=f,main=annotate(f),cex=0.7,sub=sub)
            }


            if (histograms){
                if(n>1){
                    hist(what,xlab=annotate(f),main=which,col="darkred",breaks=8,cex=1.5,sub=sub,prob=TRUE)
                    lines(density(what,na.rm=TRUE),col="darkblue")
                }
            }
    }
    if((boxplots|histograms)&n<2&any(groups!="")){
                    dev.off(dev.cur())
    }

    if(print.table){cat("\n");print(results);cat("\n")}
    return(results)
}


#############################################################################
statsByGroup<-function(data=WR,groups=groups){
    if(is.numeric(groups)){
        winDialog(type="ok",paste("Groups not defined!"))
        return()
    }
    
    cat("\nProcessing..")
    flush.console()
    #data<-data[!is.na(data)]
    #groups<-groups[names(data)]

    results<-list(9)
    results[[1]]<-aggregate(data,list(Groups=factor(groups)),FUN=function(i){length(i[!is.na(i)])})
    cat(".");flush.console()
    results[[2]]<-aggregate(data,list(Groups=factor(groups)),FUN=function(i){length(i[is.na(i)])})
    cat(".");flush.console()
    results[[3]]<-aggregate(data,list(Groups=factor(groups)),FUN=function(i){mean(i,na.rm=TRUE)})
    cat(".");flush.console()
    results[[4]]<-aggregate(data,list(Groups=factor(groups)),FUN=function(i){sd(i,na.rm=TRUE)})
    cat(".");flush.console()
    results[[5]]<-aggregate(data,list(Groups=factor(groups)),FUN=function(i){min(i,na.rm=TRUE)})
    cat(".");flush.console()
    results[[6]]<-aggregate(data,list(Groups=factor(groups)),FUN=function(i){quantile(i,probs=0.25,na.rm=TRUE)})
    cat(".");flush.console()
    results[[7]]<-aggregate(data,list(Groups=factor(groups)),FUN=function(i){quantile(i,probs=0.5,na.rm=TRUE)})
    cat(".");flush.console()
    results[[8]]<-aggregate(data,list(Groups=factor(groups)),FUN=function(i){quantile(i,probs=0.75,na.rm=TRUE)})
    cat(".");flush.console()
    results[[9]]<-aggregate(data,list(Groups=factor(groups)),FUN=function(i){max(i,na.rm=TRUE)})

    names(results)<-c("n","NA","Mean","Std","Min","25%","50%","75%","Max")

    cat("... done!\n")
    assign("results",results,.GlobalEnv)
    return(results)
}



#############################################################################
summaryAll<-function(elems=major,where=NULL,show.boxplot=FALSE,show.hist=FALSE,silent=TRUE){
    on.exit(options("show.error.messages"=TRUE))
    if(!silent){
        where<-selectSamples(print=FALSE)
        elems<-selectColumnsLabels(default=elems,exact.only = FALSE)
        if(is.null(elems)){
            winDialog(type="ok","Your query produced no matches!")
            options(show.error.messages=FALSE);stop("",call. = FALSE)
        }

        x<-winDialog("Plot summary boxplot?",type="yesno")
        show.boxplot<-(x=="YES")

        x<-winDialog("Plot individual histograms?",type="yesno")
        show.hist<-(x=="YES")
    }
   
    elems<-unlist(strsplit(elems,","))
    if(is.null(where)) where<-rownames(WR)
    
    temp<-printSamples(elems,which=where,print=FALSE)
    
    delka<-length(elems)
    cols<-n2mfrow(delka)[1]
    rows<-n2mfrow(delka)[2]


    if(show.hist){
    windows(width = 2.5*cols, height = 2.5*rows, pointsize = 10,title=paste("Summary of histograms for ",paste(elems,collapse=", "),sep=""))
    par(mfrow=c(rows,cols))
    }
    ee<-statistics(temp,elems,boxplots=FALSE,histograms=show.hist,print.table=FALSE,sub=annotate(selected))
    #ee<-statistics(WR[where,],elems,boxplots=FALSE,histograms=show.hist,print.table=FALSE,sub=annotate(selected))

    if(show.boxplot){
        windows(width = 2.5*cols, height = 2.5*rows, pointsize = 10,title=paste("Summary of boxplots for ",paste(elems,collapse=", "),sep=""))
        par(mfrow=c(rows,cols))
    }
    results<<-statistics(temp,elems,boxplots=show.boxplot,histograms=FALSE,print.table=FALSE,sub=annotate(selected))
    #results<<-statistics(WR[where,],elems,boxplots=show.boxplot,histograms=FALSE,print.table=FALSE,sub=annotate(selected))
    figaroOff()
    results<<-unlist(results[[1]])
    return(results)
}


#############################################################################
summaryByGroup<-function(elems=major,where=NULL,show.boxplot=FALSE,show.hist=FALSE,silent=TRUE){
    on.exit(options("show.error.messages"=TRUE))
    if(is.numeric(groups)){
        winDialog(type="ok",paste("Groups not defined!"))
        return()
    }
    if(!silent){    
        if(is.null(where)) where<-selectSamples(print=FALSE)
        elems<-selectColumnsLabels(default=elems,exact.only = FALSE)
        if(is.null(elems)){
            winDialog(type="ok","Your query produced no matches!")
            options(show.error.messages=FALSE);stop("",call. = FALSE)
        }
    
        x<-winDialog("Plot summary boxplot?",type="yesno")
        show.boxplot<-(x=="YES")
        x<-winDialog("Plot individual histograms?",type="yesno")
        show.hist<-(x=="YES")
    }

    elems<-unlist(strsplit(elems,","))
    if(is.null(where)) where<-rownames(WR)
    
    temp<-printSamples(elems,which=where,print=FALSE)
        
    delka<-length(elems)
    cols<-n2mfrow(delka)[1]
    rows<-n2mfrow(delka)[2]
    names(groups)<-rownames(WR)
    fact<-factor(groups[where])

    if(show.boxplot){
        windows(width = 2.5*cols, height = 2.5*rows, pointsize = 10,title=paste("Summary of boxplots for ",paste(elems,collapse=", ")," (by groups)",sep=""))
        par(mfrow=c(rows,cols))
        par(pty="s")
        par(oma=c(1,1,1,1))
        par(mar=c(5,4,1,1))
        par(las=1)
        for (f in 1:length(elems)){
            boxplot(temp[,f]~fact,names=abbreviate(levels(fact),9),main=annotate(elems[f]),varwidth=TRUE,cex.axis=0.7,horizontal=TRUE,xaxs="i",yaxs="i",sub=annotate(selected))
            #boxplot(WR[where,elems[f]]~fact,names=abbreviate(levels(fact),9),main=annotate(elems[f]),varwidth=TRUE,cex.axis=0.7,horizontal=TRUE,xaxs="i",yaxs="i",sub=annotate(selected))
        }
    }

    if(show.hist){
            for (f in levels(fact)){
                windows(width = 2.5*cols, height = 2.5*rows, pointsize = 8,title=paste("Summary of histograms for ",paste(elems,collapse=", ")," by groups (",f,")",sep=""))
                par(mfrow=c(rows,cols))
                results<-statistics(temp,elems,groups[where],f,histograms=TRUE,boxplots=FALSE,print.table=FALSE,sub=annotate(selected))
                #results<-statistics(WR[where,],elems,groups[where],f,histograms=TRUE,boxplots=FALSE,print.table=FALSE,sub=annotate(selected))
            }
    }
    results<-statistics(temp,elems,groups[where],histograms=FALSE,boxplots=FALSE,print.table=FALSE)
    #results<-statistics(WR[where,],elems,groups[where],histograms=FALSE,boxplots=FALSE,print.table=FALSE)
    figaroOff()
    assign("results",results,.GlobalEnv)
    return(results)
}


#############################################################################
#                        SINGLE VARIABLE ALL/SELECTION                      #
#            Calculates basic statistics for a single variable              #
#                           and the whole data set                          #
#############################################################################

summarySingle<-function(xlab=""){
    if(xlab=="") {
        xlab<-selectColumnLabel(colnames(WR),message="Which variable?",default="",silent=TRUE,empty.ok=FALSE)    
        where<-selectSamples(print=FALSE)
    }else{
        where<-rownames(WR)
    }

    if(!is.na(as.numeric(xlab))){
            xlab<-colnames(WR)[xlab]
            what<-WR[,xlab]
    }else{
            ee<-calcCore(xlab)
            what<-ee$results
            xlab<-ee$equation
    }
        
    if(length(where)==nrow(WR)){
            group.label<-"All data"
    }else{
            group.label<-"Selection"
    }

    x<-statsByGroup(what[where],groups=rep(group.label,length(where)))
    results<-cbind(x$n[2],x$"NA"[2],x$Mean[2],x$Std[2],x$Min[2],x$"25%"[2],x$"50%"[2],x$"75%"[2],x$Max[2])

    dig<-.round.digits(what[where])
    results<-as.matrix(round(results,dig))

    colnames(results)<-c("n","NA","Mean","Std","Min","25%","50%","75%","Max")
    rownames(results)<-group.label
    cat("\n#####################################################################\n")
    cat("Variable:  ",xlab,"\n")
    cat("#####################################################################\n")
    results<<-results
    cat("\n")

    windows(width = 5, height = 5, pointsize = 10, title=paste("Statistical summary of",xlab))
    par(mfrow=c(2,2))
    par(pty="s")
    par(oma=c(1,1,1,1))
    par(mar=c(4,4,1,1))
    
    boxplot(what[where],ylab="",xlab=annotate(xlab),varwidth=TRUE,cex.axis=0.7,col="lightgreen",horizontal=TRUE,xaxs="i",sub=annotate(selected))
    bpplot2(what[where],main="",ylab=annotate(xlab),xlab="",log="",col="lightgreen",sub=annotate(selected),horizontal=TRUE)
    hist(what[where],col="lightgreen",main="",xlab=annotate(xlab),border="darkgreen",sub=annotate(selected),prob=FALSE)
    hist(what[where],col="lightgreen",main="",xlab=annotate(xlab),border="darkgreen",sub=annotate(selected),prob=TRUE)
    lines(density(what[where],na.rm=TRUE),col="darkblue")
    box()
    figaroOff()
    return(results)
}



#############################################################################
#                         SINGLE VARIABLE BY GROUPS                         #
#    Calculates basic statistics for a single variable, individual groups   #
#                           and the whole data set                          #
#############################################################################
summarySingleByGroup<-function(xlab=""){
    if(xlab=="") GUI<-TRUE else GUI<-FALSE
    if(is.numeric(groups)){
        winDialog(type="ok",paste("Groups not defined!"))
        return()
    }
    
    if(xlab=="") {
        xlab<-selectColumnLabel(colnames(WR),message="Which variable?",default="",silent=TRUE,empty.ok=FALSE)    
        #where<-selectSamples(print=FALSE)
    }

    if(!is.na(as.numeric(xlab))){
            xlab<-colnames(WR)[xlab]
            what<-WR[,xlab]
    }else{
            ee<-calcCore(xlab)
            what<-ee$results
            xlab<-ee$equation
    }
    
    x<-statsByGroup(what,groups)
    results<-cbind(x$n[2],x$"NA"[2],x$Mean[2],x$Std[2],x$Min[2],x$"25%"[2],x$"50%"[2],x$"75%"[2],x$Max[2])

    dig<-.round.digits(what)

    results<-as.matrix(round(results,dig))
    colnames(results)<-c("n","NA","Mean","Std","Min","25%","50%","75%","Max")
    what<-what[!is.na(what)]
    
    rownames(results)<-levels(factor(groups[names(what)]))
    cat("\n#####################################################################\n")
    cat("Variable:  ",xlab,"\n")
    cat("#####################################################################\n")
    results<<-results
    cat("\n")

    if(GUI){
        windows(width = 12, height = 6.5, pointsize = 10,title=paste("Statistical summary of",xlab,"(by groups)"))
        par(mfrow=c(1,2))
        par(las=1)
        par(oma=c(1,5,1,1))
        boxplot(what~factor(groups[names(what)]),ylab="",xlab=annotate(xlab),varwidth=TRUE,cex.axis=0.7,col="lightgreen",horizontal=TRUE,xaxs="i",yaxs="i",sub=annotate(selected))
        hist(what,main="",col="lightgreen",xlab=annotate(xlab),border="darkgreen",sub=annotate(selected),prob=TRUE)
        lines(density(what,na.rm=TRUE),col="darkblue")
        box()
        figaroOff()
    }
    return(results)
}


#############################################################################
#                       MAJOR SUMMARY ALL/SELECTION                         #
#  Calculates basic statistics for major elements, individual groups        #
#############################################################################
summaryMajor<-function(){
    summaryAll("SiO2,TiO2,Al2O3,FeOt,MnO,MgO,CaO,Na2O,K2O",silent=FALSE)
}


#############################################################################
#                           MAJOR SUMMARY BY GROUPS                         #
#  Calculates basic statistics for major elements, individual groups        #
#############################################################################
summaryByGroupMjr<-function(){
    if(is.numeric(groups)){
        winDialog(type="ok",paste("Groups not defined!"))
        return()
    }
    summaryByGroup("SiO2,TiO2,Al2O3,FeOt,MnO,MgO,CaO,Na2O,K2O",silent=FALSE)
}


#############################################################################
#                       TRACE SUMMARY ALL/SELECTION                         #
#  Calculates basic statistics for major elements, individual groups        #
#############################################################################
summaryTrace<-function(){
    summaryAll("Rb,Sr,Ba,Cr,Ni,La,Eu,Y,Zr",silent=FALSE)
}


#############################################################################
#                           TRACE SUMMARY BY GROUPS                         #
#  Calculates basic statistics for trace elements, individual groups        #
#############################################################################
summaryByGroupTrc<-function(){
    if(is.numeric(groups)){
        winDialog(type="ok",paste("Groups not defined!"))
        return()
    }
    summaryByGroup("Rb,Sr,Ba,Cr,Ni,La,Eu,Y,Zr",silent=FALSE)
}



#############################################################################
#            Plots averages and their std deviations calculated by          #
#                       stat.groups for individual groups                   #
#############################################################################
#
# Parameters:
#   calculated arithmetic means for given groups [averages]
#   calculated std deviations for given groups [deviations]
#   which two elements/oxides [xaxis, yaxis]


statsByGroupPlot<-function(){
    xlab<-selectColumnLabel(colnames(WR),message="x-axis, e.g. (SiO2+2.1)*TiO2/3\nor press ENTER to pick from a list",default="",sample.names=FALSE,silent=TRUE,empty.ok=FALSE)
    if(!is.na(as.numeric(xlab))){
            xlab<-colnames(WR)[xlab]
            x<-WR[,xlab]
    }else{
            ee<-calcCore(xlab)
            x<-ee$results
            xlab<-ee$equation
    }
    
    ylab<-selectColumnLabel(colnames(WR),message="y-axis \n(press ENTER to pick from a list)",default="",sample.names=FALSE,silent=TRUE,print=TRUE,empty.ok=FALSE)
    if(!is.na(as.numeric(ylab))){
            ylab<-colnames(WR)[ylab]
            y<-WR[,ylab]
    }else{
            ee<-calcCore(ylab)
            y<-ee$results
            ylab<-ee$equation
    }
    
    cat("Processing..")
    flush.console()
    results<-list(2)
    results[[1]]<-aggregate(cbind(x,y),list(Groups=factor(groups)),FUN=function(i){mean(i,na.rm=TRUE)})  
    cat(".");flush.console()
    results[[2]]<-aggregate(cbind(x,y),list(Groups=factor(groups)),FUN=function(i){sd(i,na.rm=TRUE)})
    cat(".");flush.console()
    avg<-data.frame(results[[1]],check.names=FALSE)
    deviat<-data.frame(results[[2]],check.names=FALSE)
    cat(".....Done.\n")
    names(results)<-c("Mean","Standard deviation")
    z<-.statsByGroupPlot2(avg,deviat,xlab,ylab)
    return(z)
}


.statsByGroupPlot2<-function(averages,deviations,xaxis,yaxis){
    fousy<-function(x,sx,y,sy,boxes=FALSE){
        xmin<-min(x-sx,na.rm=TRUE)
        xmax<-max(x+sx,na.rm=TRUE)
        ymin<-min(y-sy,na.rm=TRUE)
        ymax<-max(y+sy,na.rm=TRUE)
        plot(x,y,type="p",pch="o",xlab="",ylab=annotate(yaxis),xlim=c(xmin,xmax),ylim=c(ymin,ymax),main=expression("Crosses are " %+-% "1"* sigma))
        mtext(annotate(xaxis),1,3)
        for (i in 1:length(x)){
            aa1<-c(x[i]-sx[i],x[i]+sx[i])
            bb1<-c(y[i],y[i])
            bb2<-c(y[i]-sy[i],y[i]+sy[i])
            aa2<-c(x[i],x[i])


            if(!boxes){
                lines(aa1,bb1,col=rep(barvy,times=500)[i])
                lines(aa2,bb2,col=rep(barvy,times=500)[i])
            }else{
                lines(aa1,bb2,col=rep(barvy,times=500)[i])
                rect(x[i]-sx[i],y[i]-sy[i],x[i]+sx[i],y[i]+sy[i],border=rep(barvy,times=500)[i])
            }


        }
    }


    ee<-cbind(averages[,"x"],deviations[,"x"],averages[,"y"],deviations[,"y"])
    rownames(ee)<-as.character(averages$Groups)
    colnames(ee)<-c(xaxis,"1s",yaxis,"1s ")
    print(ee)

    rect<-cbind(deviations[,"x"],deviations[,"y"],0.5)

    x<-averages[,"x"]
    y<-averages[,"y"]

    windows(width = 6.5, height = 6.5, pointsize = 10)
    fousy(x,deviations[,"x"],y,deviations[,"y"],boxes=FALSE)
    symbols(x,y,thermometers=rect,add=TRUE,fg=rep(barvy,times=500)[1:nrow(averages)])
    #points(x,y,"o",type="p")
    text(x,y,as.character(averages$Groups),col="black")
    return(ee)
}


#############################################################################
#                                                                           #
#                         SUBSETS/SELECTIONS                                #
#                                                                           #
#############################################################################

.saveVariables<-function(xx){
            on.exit(options("show.error.messages"=TRUE))
            options(show.error.messages=FALSE)
            
            labels<-labels[xx,,drop=FALSE]
            .assignWithNamespaceHard("labels",labels)
            
            try(assign("WR",WR[xx,,drop=FALSE],.GlobalEnv))
            try(assign("WRanh",WRanh[xx,,drop=FALSE],.GlobalEnv))
            try(assign("milli",milli[xx,,drop=FALSE],.GlobalEnv))
            
            # Isotopes    
            options("show.error.messages"=FALSE)
            ee<-try(assign("init",srnd(),.GlobalEnv))
            options("show.error.messages"=FALSE)
            ee<-try(assign("Hfinit",hfizo(),.GlobalEnv))
            invisible()
}

#############################################################################
#           Selects subsets of the original data set by 1 label             #
#############################################################################


selectByLabel<-function(){
    on.exit(options("show.error.messages"=TRUE))
    nn<-nrow(WRCube[[dataset.name]]$WR)
    cat("\nTotal number of samples is",nn)
    #cat("\nTotal number of samples is",nrow(WR.bak))
    cat(" of which are",nrow(WR),"selected","\n")
    cat("\n")

    x<-selectColumnLabel(sample.names=TRUE,message="Select the item to be searched in",empty.ok=FALSE)
    if(!is.numeric(x)){options(show.error.messages = FALSE);stop(call. = TRUE)}
    if(x!=1){
            where=labels[,x-1]
            what=colnames(labels)[x-1]
    }else{
            where=rownames(labels)
            what="sample names"
    }

    xfact<-factor(where)

    if(!getOption("gcd.shut.up")){
        if(what!="sample names"){
            print(table(xfact),quote=FALSE)
        }else{
            print(rownames(labels))
        }
    }

    yy<-winDialogString(paste("Enter the search pattern '",what,"'",sep=""),"")
    if(is.null(yy)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}

    xx<-grep(yy,as.vector(where))
    
    if(length(xx)==0){
        winDialog(type="ok","Your query produced no matches!")
    }else{
        names(where)<-where
        ee<-levels(factor(where[xx]))
        ee1<-winDialogString(paste("Enter the search pattern '",what,"'",sep=""),paste(ee,collapse=","))
            
        ee1<-unlist(strsplit(ee1,","))
        ee1<-paste("^",ee1,"$",sep="")
        xx<-grep(paste(ee1,collapse="|"),as.vector(where))
    
       .saveVariables(xx)
    }
    
    cat("\n")
    result<-as.matrix(where[xx])
    rownames(result)<-rownames(labels)
    colnames(result)<-what

    groups<<-labels[,"Symbol"]
    #grouping<<-which(colnames(labels)=="Symbol")
    .assignWithNamespaceHard("grouping",which(colnames(labels)=="Symbol"))
    
    message<-paste(nrow(result)," sample(s) selected \n","Default grouping on Symbol",sep="")
    winDialog(type="ok",message)
    cat("\n")
    
    #cat("\nTotal number of samples is",nrow(WR.bak))
    nn<-nrow(WRCube[[dataset.name]]$WR)
    cat("\nTotal number of samples is",nn)
    cat(" of which are",nrow(WR),"selected","\n")

    #if(colnames(result)!="sample names")print(t(result))
    cat("\n")
    return(rownames(result))
}



#############################################################################
#                     Selects subsets of the original data set              #
#                               recognizes automatically                    #
#     grep in sample ID, sample range or Boolean search in labels+data      #
#############################################################################


selectSubset<-function(what=NULL,where=cbind(labels,WR),save=TRUE,multiple=TRUE,text="Press ENTER for all samples, or specify search pattern \n by sample name, range or Boolean condition",range=FALSE,GUI=FALSE,all.nomatch=TRUE){
    on.exit(options("show.error.messages"=TRUE))
    options(show.error.messages=TRUE)
    if(is.null(what))GUI<-TRUE 
    if(length(selected)==0) selected<<-""

    if(range){
        where<-vector(length=nrow(labels))
        where<-data.frame(where)
        rownames(where)<-rownames(labels)
    } 
    if(!getOption("gcd.shut.up")&GUI){
        cat("Sample names to select from: \n")
        print(rownames(where))
        if(all(colnames(where)!="where")){
            cat("\nVariable names: \n")
            xx<-colnames(where)
            names(xx)<-1:length(xx)
            print(xx)
            cat("\n")
        }
    }
    
    if(GUI){
        selected<<-winDialogString(text,selected)
        if(is.null(selected)){cat("Cancelled.\n");options("show.error.messages"=FALSE);selected<<-"";stop(call. = FALSE)}        
    }else{
        selected<-what
    }
    
    xx<-NULL   
            ee<-unlist(strsplit(selected,","))
            options(show.error.messages=FALSE)
            xx<-try(WR[ee,])
            if(class(xx)!="try-error"){
                xx<-pmatch(ee,rownames(where))
            }else{
                xx<-NULL
                if(!range){
                    xx<-try(grep(selected,rownames(where)))
                    if(class(xx)=="try-error"){
                    winDialog(type="ok","Syntax error in search pattern!")
                    stop("",call. = TRUE)
                    }
                }
            }

    ee<-try(WR[xx,])
    
    #if(class(ee)!="try-error")xx<-NULL # commented off
    if(length(xx)>0){
        xx<-rownames(where)[xx]
    }else{
        x<-grep("[><&|=\"]",selected) # Is this a Boolean condition?
        if(length(x)==0){
            x<-paste("c(",selected,")",sep="")
            x<-try(eval(parse(text=as.expression(x))))
            if(is.null(x)&!range){
                x<-1:nrow(where)
            }
            if(class(x)=="try-error"|all(is.na(rownames(where)[x]))){
                if(!range){
                    if(GUI) winDialog(type="ok","Your query produced no matches!")
                    stop("",call. = TRUE)
                }else{
                    xx<-select.list(rownames(where),multiple=multiple)
                    if(length(xx)==0){
                        winDialog(type="ok","Your query produced no matches!")
                        stop("",call. = TRUE)
                    }
                    x<-pmatch(xx,rownames(where))
                }
            }

            x1<-rownames(where)[x][!is.na(rownames(where)[x])]
           
            if(GUI){
                y1<-winDialogString("Selected samples",paste(x1,collapse=","))
                if(is.null(y1)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = FALSE)}
                xx<-unlist(strsplit(y1,","))
            }else{
                xx<-x1   
            }
            if(!getOption("gcd.shut.up"))print(xx)
        }else{
            yy<-selected
            yy<-gsub("[ ]*","",yy)  # remove all spaces # TODO not if quoted in a string
            yy<-gsub("==","=",yy)
            yy<-gsub("\\.AND\\.","&",yy,ignore.case=TRUE)
            yy<-gsub("\\.OR\\.","|",yy,ignore.case=TRUE)

            fields<-gsub("([><&|=\\])","@",yy) # fields<-gsub("([><&|=\])","@",yy) TROUBLEMAKER
            fields<-gsub("\\.and\\.","@",fields)
            fields<-gsub("\\.or\\.","@",fields)
            fields<-gsub(("[()=!]"),"",fields)
            fields<-strsplit(fields,"@")
            fields<-unlist(fields)

            # Added
            pole<-matrix(fields,ncol=2,nrow=length(fields)/2,byrow=TRUE)
            pole[!is.na(as.numeric(pole))]<-""
       
            for(i in 1:nrow(pole)){
               if(pole[i,2]!=""){
                    options("show.error.messages"=FALSE)
                    prd<-try(levels(where[,pole[i,1]])) # PODEZRELY
                     if(class(prd)=="try-error"){
                        winDialog(type="ok","Unknown variable in search pattern!")
                        xx<-NULL
                        stop("",call. = TRUE)
                    }
                    
                    ee<-try(grep(eval(parse(text=pole[i,2])),prd))
                    if(class(ee)=="try-error"){
                        winDialog(type="ok","Syntax error in search pattern!")
                        xx<-NULL
                        stop("",call. = TRUE)
                    }
                   j<-deparse(paste(prd[ee],collapse=paste("\"|",pole[i,1],"=\"",sep=""))) # Troublemaker with accented characters
                   k<-gsub("[\\^]","\\\\^",pole[i,2])
                   k<-gsub("[$]","\\\\$",k)
                   k<-gsub("[[]","\\\\[",k)
                   k<-gsub("[]]","\\\\]",k)
                   k<-gsub("[{]","\\\\{",k)
                   k<-gsub("[}]","\\\\}",k)
                   yy<-sub(paste(pole[i,1],"=",k,sep=""),paste("(",pole[i,1],"=",j,")",sep=""),yy) # Troublemaker with accented characters
                   #yy<-sub(paste(pole[i,1],"=",k,sep=""),paste("\(",pole[i,1],"=",j,"\)",sep=""),yy) # Troublemaker with accented characters
               }
            }
            
            fields<-unique(fields[is.na(as.numeric(fields))])
            #x<-match(gsub("\/",".",fields),colnames(where))
            x<-match(gsub("/",".",fields),colnames(where))
            
            fields<-fields[!is.na(x)]
            names.preserved<-fields
            #fields<-gsub("\/",".",fields)
            fields<-gsub("/",".",fields)

            yy<-gsub(">=",">@",yy)
            yy<-gsub("<=","<@",yy)
            yy<-gsub("!=","!@",yy)
            yy<-gsub("=","==",yy)
            yy<-gsub(">@",">=",yy)
            yy<-gsub("<@","<=",yy)
            yy<-gsub("!@","!=",yy)
            #yy<-gsub("\/",".",yy)
            yy<-gsub("/",".",yy)
            
            # added due to mg#
            colnames(where)<-gsub("[#]","XXXXX",colnames(where))
            yy<-gsub("[#]","XXXXX",yy)
            fields1<-gsub("[#]","XXXXX",fields)
            
            xx<-try(subset(where,eval(parse(text=yy)),select=fields1)) # mg# does not work here
            colnames(xx)<-fields
            
            # added due to mg#
            colnames(where)<-gsub("XXXXX","#",colnames(where))
            yy<-gsub("XXXXX","#",yy)
            
            if(class(xx)=="try-error"){
                if(GUI) winDialog(type="ok","Syntax error!")
                stop("",call. = TRUE)
            }
 
            colnames(xx)<-names.preserved
            if(!getOption("gcd.shut.up") & nrow(xx)!=0){
                cat(selected,"\n")
                if(ncol(xx)==1) print(t(xx)) else print(xx)
            }
            xx<-as.matrix(xx)
            xx<-rownames(xx)
        }
    }
    ee<-try(WR[xx,])
    
    if(length(xx)==0|class(ee)=="try-error"){
        if(GUI) winDialog(type="ok","Your query produced no matches!")
        if(all.nomatch){
            xx<-rownames(where)
            if(save) message<-paste(length(xx)," sample(s) retained \n",sep="") else message<-"The whole data set used \n"
        }else{
            return("")
        }
    }else{
         message<-paste(length(xx)," sample(s) selected",sep="")
         cat(message,"\n")
         if(save){
            .saveVariables(xx)
         }
    }
        if(GUI)winDialog(type="ok",message)
        cat("\n")
    invisible(xx)
}


#############################################################################
#           Selects samples on the basis of the Boolean search              #
#                 global variables WR and labels unaffected                 #
#############################################################################
selectSamples<-function(what=NULL,print=TRUE,multiple=TRUE,text=NULL){
    if(is.null(text)){
        xx<-selectSubset(what,save=FALSE, multiple=multiple,GUI=TRUE)
    }else{
        xx<-selectSubset(what,save=FALSE, multiple=multiple,text=text)
    } 
    if(is.null(xx)){cat("Cancelled.\n");options("gcd.ident"=0);return()}
    if(!getOption("gcd.shut.up") & print){cat("Selected samples:","\n");print(xx)}
    return(unique(xx))
}


#############################################################################
#                    Selects the whole original data set                    #
#############################################################################
selectAll<-function(GUI=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    options(show.error.messages=FALSE)
    
    #.assignWithNamespaceHard("labels",labels.bak)
    #assign("WR",WR.bak,.GlobalEnv)
    #assign("WRanh",WRanh.bak,.GlobalEnv)
    #assign("milli",milli.bak,.GlobalEnv)
    #grouping<<-which(colnames(labels)=="Symbol")
    #groups<<-labels[,"Symbol"]
    
    peekDataset(dataset.name)    
    assign("groups",labels[,"Symbol"],.GlobalEnv)
    .assignWithNamespaceHard("grouping",which(colnames(labels)=="Symbol"))
    
    # Isotopes    
    options("show.error.messages"=FALSE)   
    ee<-try(assign("init",srnd(),.GlobalEnv))
    options("show.error.messages"=FALSE)
    ee<-try(assign("Hfinit",hfizo(),.GlobalEnv))
                
    if(GUI){
        message<-paste("All ",nrow(WR)," samples selected \n","Default grouping on Symbol",sep="")
        winDialog(type="ok",message)
    }
    invisible()
}


#############################################################################
#                                                                           #
#                              GROUPING                                     #
#                                                                           #
#############################################################################


#############################################################################
#         Selects grouping for the data set on a single label               #
#############################################################################
groupsByLabel<-function(lab=NULL){
    on.exit(options("show.error.messages"=TRUE))
    if(is.null(lab)){
        lab<-selectColumnLabel(message="The analyses should be grouped according to",empty.ok=FALSE)
    }else{
        if(is.character(lab)){
            i<-which(colnames(labels)==lab)
            if(length(i)==0){
                winDialog(type="ok","Label not found - groups not set!")
                options(show.error.messages=FALSE)
                stop(call. = TRUE)
            }
        }
    }
    x<-as.vector(labels[,lab])
    groups<-x
    names(groups)<-rownames(labels)
    groups[nchar(groups)==0]<-"(Undefined)" #NEW
    
    if(!getOption("gcd.shut.up")){print(groups);cat("\n");print(table(groups,dnn="Assigned groups:"))}
    
    assign("groups",groups,.GlobalEnv)
    .assignWithNamespaceHard("grouping",lab)
    #assign("grouping",lab,.GlobalEnv)
    invisible()
}


#############################################################################
#                                                                           #
#                     PLOTTING SYMBOLS AND COLOURS                          #
#                                                                           #
#############################################################################


#############################################################################
#          Assigns a single plotting colour to all analyses                 #
#############################################################################
assign1col<-function(col=-1){
    on.exit(options("show.error.messages"=TRUE))
    if(col==-1){
        ee<-showColours()
        col<-pickColour()
        dev.off(ee)
        GUI<-TRUE
    }else{
        GUI<-FALSE
    }
    labels$Colour<-rep(col,nrow(labels))
    
    if(all(!is.na(as.numeric(labels$Colour)))){
        labels$Colour<-as.numeric(labels$Colour)
    }
    .assignWithNamespaceHard("labels",labels)
    message<-"Colours for all samples successfully assigned"
    if(GUI){
        winDialog(type="ok",message)
        showLegend(new.plot=TRUE)
    }else{
        cat(message)
        cat("\n")
    }
}


#############################################################################
#          Assigns a single plotting symbol to all analyses                 #
#############################################################################
assign1symb<-function(pch=-1){
    on.exit(options("show.error.messages"=TRUE))
    if(pch==-1){
        ee<-showSymbols()
        y<-winDialogString("Which symbol","1")
        if(is.null(y)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        pch<-y
        dev.off(ee)
        GUI<-TRUE
    }else{
        GUI<-FALSE
    }
    labels$Symbol<-rep(pch,nrow(labels))
    if(all(!is.na(as.numeric(labels$Symbol)))){
        labels$Symbol<-as.numeric(labels$Symbol)
    }
    .assignWithNamespaceHard("labels",labels)
    message<-"Symbols for all samples successfully assigned"
    if(GUI){
        winDialog(type="ok",message)
        showLegend(new.plot=TRUE)
    }else{
        cat(message)
        cat("\n")
    }
}


#############################################################################
#                           Shows available symbols                         #
#############################################################################
showSymbols<-function(){
    if(!is.null(dev.list())) ee<-dev.set(dev.cur()) else ee<-NULL
    windows(width = 2.5, height = 2.5, pointsize = 10,rescale="fit")
    par(mai=c(0.1,0.1,0.1,0.1))
    ipch <- 0:19
    iy <- 3 + 4-(ipch) %% 5
    ix <- (ipch) %/% 5
    plot(ix, iy, xlim=c(-0.5,3.5),type='n', axes = FALSE, xlab = '', ylab = '',asp=1)
    for(i in ipch+1) {
        points(ix[i],iy[i], pch=i-1, col=1, bg="yellow", cex = 2)
        text  (ix[i] - .4, iy[i], i-1, col="black")
    }
    out<-dev.cur()
    if(!is.null(ee)) dev.set(ee)
    invisible(out)
}


#############################################################################
#                           Shows available colours                         #
#############################################################################
showColours<-function(n=49){
    if(!is.null(dev.list())) ee<-dev.set(dev.cur()) else ee<-NULL
    windows(width = n/12, height = n/12, pointsize = 10,rescale="fit")
    par(mai=c(0,0,0,0))
    ipch <- 0:(n-1)
    by<-round(sqrt(n),0)
    iy <- by + by+1-(ipch) %% by
    ix <- ipch %/% by


    plot(ix, iy,type='n', xlim=c(-0.5,max(ix)+0.5),axes = FALSE, xlab = '', ylab = '',asp=1)
    for(i in ipch+1) {
        points(ix[i],iy[i], pch=15, col=i, cex = by+1)
        text (ix[i], iy[i], i, col="black")
    }
    out<-dev.cur()
    if(!is.null(ee)) dev.set(ee)
    invisible(out)
}

showColours2<-function(n=64){
    for (j in 0:round(length(colours())/n,0)){
        windows(width = n/12, height = n/12, pointsize = 8,rescale="fit")
        par(mai=c(0,0,0,0))
        ipch <- (j*n):(j*n+(n-1))
        step.by<-round(sqrt(n),0)
        iy <- step.by + step.by+1-(ipch-j*n) %% step.by
        ix <- (ipch-j*n) %/% step.by
        plot(ix, iy,type='n', xlim=c(-0.5,max(ix)+0.5),axes = FALSE, xlab = '', ylab = '',asp=1)
    for(i in ipch+1) {
        points(ix[i-j*n],iy[i-j*n], pch=19, col=colours()[i], cex = step.by+1)
        text (ix[i-j*n], iy[i-j*n], colours()[i], col="black",srt=45,adj=0.5)
    }
}
}


#############################################################################
#                                                                           #
#                                 LEGEND                                    #
#                                                                           #
#############################################################################


#############################################################################
#                                Builds legend                              #
#############################################################################


.build.legend<-function(labs,chars=TRUE,cols=TRUE,pch=labels$Symbol,col=labels$Colour){
    if(chars)ee<-cbind(labels$Symbol,1)
    if(cols){
            if(!is.character(leg.col)){
                ee<-cbind(1,col)
                #ee<-cbind(1,labels$Colour)
            }else{
                ee<-cbind(1,t(leg.col))
            }
    }
   
    if(chars & cols)ee<-cbind(pch,col)
    #if(chars & cols)ee<-cbind(labels$Symbol,labels$Colour)
    
    rownames(ee)<-as.character(labs)
    ee<-ee[unique(rownames(ee)),]
    if(is.vector(ee)){
            ee<-t(as.matrix(ee))
            rownames(ee)<-as.vector(labs[1])
    }
    ee<-cbind(rownames(ee),ee)
    
    #ord<-sort.list(ee[,1])
    #ee<-ee[ord,,drop=FALSE]
    return(ee)
}


#############################################################################
#                                 Shows legend                              #
#############################################################################

showLegend<-function(pch=labels$Symbol,col=labels$Colour,new.plot=TRUE,alt.leg=FALSE,just.colours=FALSE,GUI=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    #leg.pch<-get("leg.pch",.GlobalEnv)
    #leg.col<-get("leg.col",.GlobalEnv)
   
    labels$Symbol[labels$Symbol==NA]<-11
    labels$Colour[labels$Colour==NA]<-8
  
    # Legend for both symbols and colours is based on the same criterion
    if(leg.col==leg.pch){
       if(leg.pch==-1) {
            labs<-rownames(WR)
            x<-"Symbols & colours by sample names"
       }
       
       if(leg.pch==0) {
            x<-"Symbols & colours by group"
            gr<-get("grouping",.GlobalEnv)
            if(gr<=0)labs<-groups else labs<-labels[,gr]
            labs<-as.character(labs)
            labs[is.na(labs)]<-"Unspecified"
       }
       
       if(leg.pch>0) {
            x<-paste("Symbols & colours by",colnames(labels)[leg.col])
            labs<-labels[,leg.col]
            labs<-as.character(labs)
            labs[is.na(labs)]<-"Unspecified"
       }

       ee<- unlist(strsplit(x," by "))[[2]]
            if(length(unique(col))==1){
                if(length(unique(pch))==1){
                    x<-"Uniform symbols and colours"
                }else{
                    x<-paste("Symbols by",ee)
                }
            }else{
                if(length(unique(pch))==1){
                    x<-paste("Colours by",ee)
                }
            }
            .legend.main(labs=labs,main=x,new.plot=new.plot)
            
    # Legend for both symbols and colours is based on distinct criteria, two legends are to be built
    # Unless just.colours = TRUE
    }else{
       #par(mfcol=c(2,1))
       if(leg.col!=0){
            if(length(leg.col)==1){ # Single value,i.e. colours by sample names or labels
                if(leg.col==-1){
                    labs.col<-rownames(WR)
                    typ<-"sample names"
                }else{
                    labs.col<-labels[,leg.col]
                    typ<-colnames(labels)[leg.col]
                }
                labs.col<-as.character(labs.col)
                labs.col[is.na(labs.col)]<-"Unspecified"
            
                x2<-paste("Colours by",typ)
            
            }else{                  # Colours by variable
                labs.col<-colnames(leg.col)
                labs.col<-as.character(labs.col)
                
                x2<-paste("Colours by",rownames(leg.col))
                cols<-leg.col
                chars<-rep(15,length(leg.col))
                labs<-colnames(leg.col)
                
                if(GUI){
                    eee<-winDialog(type="yesno","Use alternative (continuous spectrum) legend?")
                    if(eee=="YES") alt.leg<-TRUE
                }
            }
            
       }else{                       # Colours by groups
            labs.col<-groups
            x2<-"Colours by group"
       }
       
       if(!just.colours){ 
            if(leg.pch!=0){ # Symbols by sample names or labels
                if(leg.pch==-1){
                    labs.pch<-rownames(WR)
                    typ<-"sample names"
                }else{
                    labs.pch<-labels[,leg.pch]
                    typ<-colnames(labels)[leg.pch]
                }
                labs.pch<-as.character(labs.pch)
                labs.pch[is.na(labs.pch)]<-"Unspecified"
            
                x1<-paste("Symbols by",typ)
            }else{ # Symbols by Groups
                labs.pch<-groups
                x1<-"Symbols by group"
            }
       }
       .assignWithNamespaceHard("labels",labels)
       if(!just.colours) .legend.main(labs.pch,chars=TRUE,cols=FALSE,main=x1,new.plot=new.plot)
       .legend.main(labs.col,chars=FALSE,cols=TRUE,main=x2,new.plot=new.plot,alt.leg=alt.leg)
    }
    
    invisible()
}

# JF - alternative legends, so far in a separate plotting windo only
.altLegJFM<-function(new.plt){
        leg.levels<-matrix(as.numeric(unlist(strsplit(colnames(leg.col),"-"))),ncol=2,byrow=TRUE) # NEW by VJ
        lb<-leg.levels[,1] # NEW by VJ
        ub<-leg.levels[,2] # NEW by VJ
        #leg.levels<-as.numeric(gsub("([-.0-9]{1,})([-][-.0-9]{1,})","\\1",colnames(leg.col)))
        #lb<-leg.levels[1:length(leg.levels)-1]
        #ub<-leg.levels[2:length(leg.levels)]
        plot(1,1,xlim=c(0,1),ylim=c(min(lb),max(ub)),axes=FALSE,pty="n",main=annotate(rownames(leg.col)),adj=0) # ADDING DOES NOT WORK ,new=!new.plt)
        axis(side=2)
        rect(0,lb,0.1,ub,density=-1,col=leg.col)
}

.legend.main<-function(labs,main="",chars=TRUE,cols=TRUE,pch=labels$Symbol,col=labels$Colour,new.plot=TRUE,alt.leg=FALSE){
    if(alt.leg) new.plot<-TRUE
    if(new.plot){
        if(!is.null(dev.list())) dev.bak<-dev.cur() else dev.bak<-NULL
        windows(width = 3.5, height = 8.5,points=10)
        #par(mar=c(1,1,1,1))
    }
    get("leg.pch",.GlobalEnv)
    get("leg.col",.GlobalEnv)
    
    labs<-as.character(labs)
    labs[is.na(labs)]<-"Unspecified"

    xx<-.build.legend(labs=labs,chars=chars,cols=cols)
    
    if(length(unique(xx[,2]))==1 & length(unique(xx[,3]))==1){
        xx<-xx[1,,drop=FALSE]
        xx[1,1]<-"All the same"
    }   
    
    if(is.vector(xx))xx<-t(xx);rownames(xx)<-xx[,1]
    pch<-xx[,2]
    if(all(!is.na(as.numeric(pch)))){
    pch<-as.numeric(pch)
    }
    
    #col<-as.numeric(xx[,3])
    col<-xx[,3]

    n<-length(labs)
    if(new.plot){
        plot(1,1,xlim=c(-1,1),ylim=c(-1,1),type="n",xlab="",ylab="",axes=FALSE)
        text(0,1,annotate(main),cex=1,adj=0.5)
    }
    labs<-rownames(xx)
    if(chars){
        go<- as.go(points=as.pgo(rep(1,n),1:n,pch=pch,col=col,label=labs))
    }else{
        go<- as.go(points=as.pgo(rep(1,n),1:n,pch=15,col=col,label=labs))
    }
    if(length(leg.col)>1 & alt.leg){
        .altLegJFM(new.plot)
    }else{
        if(new.plot){
            ee<-legend.go(go,xy=list(x=-1,y=0.9)) 
            if(!is.null(dev.bak)) dev.set(dev.bak)
        }else{
            ee<-legend.go(go)
        }
    }
    invisible()
}

#############################################################################
#                                                                           #
#                               FILES                                       #
#                                                                           #
#############################################################################

#############################################################################
#                             Source R files in a dir                       #
#############################################################################

sourceDir<-function(dir="Norms",text="external file",recursive=FALSE){
        if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui")verbose<-TRUE else verbose<-FALSE 
        on.exit(options("show.error.messages"=TRUE))
        source.path<-paste(gcdx.dir,dir,sep="/")
        source.names<-dir(source.path,pattern = "[.][Rr]$",recursive=recursive)
        options(show.error.messages=TRUE)
        for(i in source.names){
            if(!getOption("gcd.shut.up")&verbose){
                if(!is.null(text) & substr(i,1,1)!="_") cat("Loading ",text,": ",i,"...",sep="")
            }
            ee<-try(source(paste(source.path,"/",i,sep="")[1]))
            if(class(ee)!="try-error"){
                if(!is.null(text)& substr(i,1,1)!="_"){
                    if(!getOption("gcd.shut.up")&verbose) cat("...Syntax ok\n")
                }
            }else{
                if(!verbose) cat("Loading ",text,": ",i,"...",sep="")
                if(!is.null(text)& substr(i,1,1)!="_")cat("...Syntax failed!\n")
                print(paste(source.path,"/",i,sep=""))
                cat(geterrmessage())
            }
            
         flush.console()    
    }
    #cat(dir,"is ok.\n")
    #flush.console()
    invisible()
}

#############################################################################
#     Initialize all the variables (clear all the data in memory)           #
#############################################################################
.clear.data<-function(){
    on.exit(options("show.error.messages"=TRUE))
    options(show.error.messages=FALSE)
    assign("leg.col",0,.GlobalEnv) 
    assign("leg.pch",0,.GlobalEnv) 
    assign("selected","",.GlobalEnv)
    assign("barvy",1:49,.GlobalEnv)
    assign("WR",numeric(0),.GlobalEnv) 
    assign("labels",character(0),.GlobalEnv)
    assign("WRanh",numeric(0),.GlobalEnv)
    assign("milli",numeric(0),.GlobalEnv)
    assign("equation","",.GlobalEnv)
    assign("results",numeric(0),.GlobalEnv)
    try(assign("init",numeric(0),.GlobalEnv))
    try(assign("Hfinit",numeric(0),.GlobalEnv))
    try(assign("age",NULL,.GlobalEnv))
    #rm(init,envir=.GlobalEnv)
    #rm(list=ls())
}

 # Auxiliary function that prints elements of a data matrix corresponding 
    # to the search "pattern" (regular expression) and replaces them by 
    # "replacement" (if specified); returns amended matrix "where"
    
.replace.regex<-function(kde,pattern,replacement=NULL,txt=NULL,silent=FALSE,prompt=FALSE){
        if(is.vector(kde)){
            kde<-(as.matrix(kde,ncol=1))
            colnames(kde)<-"aa"
        }
        i<-sapply(colnames(kde),function(f){grep(pattern,as.vector(kde[,f]))},USE.NAMES=FALSE)
        names(i)<-1:length(i)
        i<-i[sapply(names(i),function(f){length(i[[f]])>0})]
        ee<-cbind(as.numeric(unlist(i)),as.numeric(unlist(sapply(names(i),function(f){rep(f,length(i[[f]]))}))))
        old<-kde[ee]
        if(!is.null(replacement)&length(i)>0){ 
            if(is.numeric(replacement)){
                original<-as.numeric(unlist(lapply(old,substring,first=2)))
                kde[ee]<-replacement*original
                replacement<-paste(replacement,"x the detection limit",sep="")
            }else{
                kde[ee]<-replacement
            }
            message<-paste("Warning! Some items will be replaced by ",replacement,"!!!",sep="")
            if(!silent)winDialog(type="ok",message)
        }  
          
        if(length(i)>0){
            p<-cbind(rownames(kde)[ee[,1]],colnames(kde)[ee[,2]],old,kde[ee])
            p<-data.frame(p)
            colnames(p)<-c("Sample","Variable","Value","Replacement")
         #if(!silent){   
            cat("\nRemoving",txt,"- replacing by",replacement,":\n")
            print(p)
            if(prompt)cat("...Press any key\n")
            flush.console()
            if(prompt)readline()
         #}
        }
        flush.console()
        return(kde)
}
    
#############################################################################
#     Process loaded data (Separate labels from chemical data..)            #
#############################################################################
.loadData.process<-function(x,merging=FALSE,clipboard=FALSE,GUI=TRUE,source.first=TRUE,pokeDataset=TRUE){
   options(show.error.messages=FALSE)
   #ee<-try(get(filename,.GlobalEnv))
   if(!getOption("gcd.shut.up")){
        ee<-try(cat(filename,"\n"))
        if(class(ee)=="try-error")filename<<-"from memory" 
   }
   on.exit(options("show.error.messages"=TRUE))
   
   #if(length(ls(pat="^WR$"))==1){
   #     if(!merging){   
   #         winMenuDel("Plugins") # this is not the first load       
            #.menu.del() 
   #         source(paste(gcdx.dir,"menu.r",sep="/"))
        #}else{
        #    menuOff()
        #}
   #}
   
    .clear.data() # Clear the memory
    options("show.error.messages"=FALSE)
    try(detach("package:XML"))
       
    x<-as.matrix(x) # Convert all data to character   

    if(nrow(x)>1){
        x<-x[,apply(x,2,function(i)!all(is.na(i)))] # all items in the column empty
        x<-x[apply(x,1,function(i)!all(is.na(i))),] # all items in the row empty
    }else{
        i<-colnames(x)[!is.na(x)]
        x<-x[,i,drop=FALSE]
    }
   
    # Filter out all rows starting with "#" = comments, should be out already anyway
    i<-grep("^#",rownames(x))
    if(length(i)>0) x<-x[-i,]
        
    # NEW Replace all decimal commas by decimal points [JF bug]
    xx<-apply(x,2,function(f){
       z<-gsub("(^[<]?)([-0-9]*)(,)([-0-9]*)","\\1\\2.\\4",f)
       return(z)
    })
    
    if(is.vector(xx))xx<-matrix(xx,nrow=1,ncol=ncol(x))
    dimnames(xx)<-dimnames(x)
    x<-xx
    options(show.error.messages=TRUE)
    
    # NEW 
    # Protect the hexadecimal numbers (# and 6-8 characters 0-1A-F), symbol # to be temporarilly replaced by @
    
    x<-apply(x,2,gsub,pattern="(^#)([a-fA-F0-9]{6,8})",replacement="@\\2",x[1,])
   
    # Replace all warnings from Excel (starting with "#", i.e. #REF and such)
    x<-.replace.regex(x,pattern="^#[a-zA-Z_/0-9].{1,}",txt="Excel errors",replacement=NA,silent=!GUI)
    
    # Get the hexadecimal numbers back, replacing @ by #
    x<-apply(x,2,gsub,pattern="(^@)([a-fA-F0-9]{6,8})",replacement="#\\2",x[1,])
    
    # Add FF as alpha channel to 6-digit hexadecimal numbers
    x<-apply(x,2,gsub,pattern="(^#)([a-fA-F0-9]{6})$",replacement="#\\2FF",x[1,])
    
    if(GUI){
            # Replace all values smaller than detection limit - expressed as negative values or as values "lesser than"       
            if(length(grep("<[0-9.,E]",x))>0|length(grep("[-][0-9.,E]",x))){ # NEW
                ee<-winDialog("Replace the values below detection limit with half of d.l.?",type="yesno")
                    if(ee=="YES"){
                        # Replace all values "less than"        
                        x<-.replace.regex(x,pattern="<[0-9.,E]{1,}",txt="< bd",replacement=0.5,silent=FALSE)
                        # Replace all values starting with minus sign 
                        x<-.replace.regex(x,pattern="^[-][0-9.,E]{1,}$",replacement=0.5,silent=FALSE) #NEW
                    }else{
                        # Replace all values "less than" by NA        
                        x<-.replace.regex(x,pattern="^[ ]*<[0-9.,E]{1,}",txt="< bd",replacement=NA,silent=TRUE)        
                    }
            }
   }
    
    ee<-colnames(x)
    ee<-gsub("^[ ]{1,}","",ee) # Filter out trailing spaces at beginning of variable names
    ee<-gsub("^[_]{1,}","",ee) # Filter out trailing spaces at beginning of variable names
    ee<-gsub("[ ]{1,}$","",ee) # Filter out trailing spaces in the end of variable names
    ee<-gsub("[#].{1,}$",".",ee) # replace by not if not at the end (mg#, Mg#)
    colnames(x)<-ee
    
    # NEW FUNCTION TO CONVERT HEXDECIMAL NUMBERS TO DECIMAL
    xx<-apply(x,2,function(f){
        # Hex numbers
        ee<-grep("^(-0x[0-9a-fA-F]{4})(L{,1}$)",f)
        f[ee]<-as.numeric(gsub("(^-0x[0-9a-fA-F]{4})(L{,1}$)","\\1",f[ee])) # Hexadecimal numbers end by L, drop it as as.numeric cannot handle that
        return(f)
    })
    if(is.vector(xx))xx<-matrix(xx,nrow=1,ncol=ncol(x))
    dimnames(xx)<-dimnames(x)
    x<-xx
    
    dict<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","MnO","MgO","CaO","Na2O","K2O","FeOt",
    "Fe2O3t","Li2O","P2O5","Cr2O3","NiO","LOI","H2O","H2O_MINUS",
    "H2O_PLUS","CO2","F","B","S","mg#",
    "Ac","Ag","Al","As","At","Au","Ba","Be","Bi","Br","Ca","Cd","Ce","Cl",
    "Co","Cr","Cs","Cu","Dy","Er","Eu","Fe","Ga","Gd","Ge","Hf","Hg","Ho",
    "In","Ir","La","Li","Lu","Mg","Mn","Mo","Na","Nb","Nd","Ne","Ni",
    "Np","Os","Pa","Pb","Pd","Pm","Pr","Pt","Pu","Rb","Re","Rh","Ru","Sb","Sc",
    "Se","Si","Sm","Sn","Sr","Ta","Tb","Te","Th","Ti","Tl","Tm","U","V","W","Y","Yb","Zn","Zr")
    
    # Replace typos in numeric columns found in dictionary NEW
    i<-toupper(colnames(x))%in%toupper(dict)
    
    if(any(i)){
        x[,i]<-.replace.regex(x[,i],pattern="[a-df-zA-DF-Z]{1,}",txt="non-numeric values in numeric fields", 
            replacement=NA,silent=FALSE)  
    }
    
    # NEW for reading Melts files
    ee<-colnames(x)
    uff<-grep("^wt% [A-Z]",ee)
    if(length(uff)>0){
        cat("Reading Melts output....\n")
        ee<-gsub("(^wt% )([A-Z])","\\2",ee)
        colnames(x)<-ee
    }
    
    # Get all the completely numeric variables, put them to a numeric matrix "WR"
    if(nrow(x)>1){
        if(.Platform$OS.type=="windows"){
            i<-apply(x,2,function(f){length(grep("[#\u00b0'a-df-zA-DF-Z]",f))==0}) # \u00b0 is degree DEGREE AND PERHAPS APOSTROPHE MAKES PROBLEMS ON MAC
        }else{
            prdik<-x # BUG: IT CANNOT DEAL WITH VALUES LESS THAN (<5) FOR BDL !!!!!
            prdik[prdik==""]<-0
            prdik[is.na(prdik)]<-0
            i<-apply(prdik,2,function(f){
                z<-!any(is.na(as.numeric(f)))
                return(z)
            })
        }
    }else{
         i<-!is.na(as.numeric(x))
         names(i)<-colnames(x)
    }
    if(!getOption("gcd.shut.up")){
        cat("...")
        flush.console()
    }
    
    #WR<-data.frame(x[,i,drop=FALSE])
    options(show.error.messages=TRUE)
    WR<-matrix(as.numeric(x[,i,drop=FALSE]),byrow=FALSE,ncol=length(which(i)))
    #WR<-matrix(as.numeric(x[,i]),byrow=FALSE,ncol=length(which(i)))
    rownames(WR)<-rownames(x)
    colnames(WR)<-names(i)[i]
    
     # Replace all values "less than"
     if(GUI){
       if(any(WR<0,na.rm=TRUE)){ 
            ee<-winDialog("Do negative values mean that the given item is under detection limit?",type="yesno")
            if(ee=="YES"){    
                # Replace all numeric values starting with minus sign 
                #x<-.replace.regex(x,pattern="^[ ]*[-][0-9.,E]{1,}$",replacement=NA,silent=TRUE) # NEW
                # Replace all zeros or negative values
                options(warn=-1)
                WR[as.numeric(WR)<=0]<-NA
                options(warn=0)
            }
        }
    }
    
    # Gather the rest to data frame "labels"
    labels<-data.frame(x[,!i,drop=FALSE],check.names = FALSE)
    colnames(labels)<-colnames(x)[!i]
    rownames(labels)<-rownames(x)
    
    if(!getOption("gcd.shut.up")){
        cat("...\n")
        flush.console()
    }
    
    # Get symbols, if specified
    i1<-which(toupper(colnames(WR))=="SYMBOL")
    i2<-try(which(toupper(colnames(labels))=="SYMBOL"),silent=TRUE)
    if(class(i2)=="try-error") i2<-character(0)
    if(length(i2)==0){                  # Character codes do not exist
        if(length(i1)>0 ){              # Numeric codes do exist 
            labels<-cbind(labels,WR[,i1,drop=FALSE]) 
            colnames(labels)[ncol(labels)]<-"Symbol"
            WR<-WR[,-i1,drop=FALSE]
        }else{
            labels<-cbind(labels,rep(1,nrow(WR)))
            colnames(labels)[ncol(labels)]<-"Symbol"
        }
    }
    
    # Default symbol where not specified
    labels[is.na(labels[,"Symbol"]),"Symbol"]<-1
    
    # Get colours, if specified
    i1<-grep("^COLO[U]?R$",toupper(colnames(WR)))
    i2<-grep("^COLO[U]?R$",toupper(colnames(labels)))
  
    if(length(i2)==0){ # Character codes do not exist
        if(length(i1)>0){ # Numeric codes do exist
            labels<-cbind(labels,WR[,i1])
            colnames(labels)[ncol(labels)]<-"Colour"
            WR<-WR[,-i1,drop=FALSE]
        }else{
            labels<-cbind(labels,rep(1,nrow(WR)))
            colnames(labels)[ncol(labels)]<-"Colour"
        }
    }else{ # Character codes do exist
            colnames(labels)[i2]<-"Colour"
            labels[,"Colour"]<-as.character(labels[,"Colour"])
    }
    
    # Default colour where not specified
    labels[is.na(labels[,"Colour"]),"Colour"]<-1
    
    # Get character expansion (cex), if specified
    i1<-c(which(toupper(colnames(WR))=="CEX"),which(toupper(colnames(WR))=="SIZE"))
    if(length(i1)>0){ # Numeric codes do exist
            labels<-cbind(labels,WR[,i1,drop=FALSE])
            colnames(labels)[ncol(labels)]<-"Size"
            WR<-WR[,-i1,drop=FALSE]
    }else{
        labels<-cbind(labels,rep(1,nrow(WR)))
        colnames(labels)[ncol(labels)]<-"Size"
    }
    
    # Default character expansion where not specified
    labels[is.na(labels[,"Size"]),"Size"]<-1
    
    # Multiple by overall factor
    labels[,"Size"]<-labels[,"Size"]*getOption("gcd.cex")
    
    # Get the default precision of the results
    getOption("gcd.digits")
    options(show.error.messages=TRUE)
    
    # Autoassign symbols and colours (only in GUI)
    ###############################################
    if(GUI){
        if(all(labels[,"Symbol"]==1)&all(labels[,"Colour"]==1)){
            if(ncol(labels)>3 | (ncol(labels)>4 & any(toupper(colnames(labels))=="MINERAL"))){
                if(.Platform$GUI=="Rgui"){
                    ee<-winDialog("Autoassign plotting symbols & colours",type="yesno")
                    if(ee=="YES"){
                        .assignWithNamespaceHard("labels",labels)
                        #assign("labels",labels[,-x],.GlobalEnv)
                        #assignInNamespace("labels",labels,"base",pos=2)
                        symb<-.autoassign.pch(legend=TRUE)
                        labels[,"Colour"]<-symb$col
                        labels[,"Symbol"]<-symb$pch
                    }
                }
            }
        }
    }
    #cat("... \n")
    #flush.console()

    #Print summary info about imported file
    if(!getOption("gcd.shut.up")){
    for (f in 1:ncol(labels)){
        if(length(levels(labels[,f]))!=0) {
            cat(colnames(labels)[f],":")
            print(table(factor(labels[,f])))
            cat("\n\n")
            flush.console()
        }
        flush.console()
    }
    }
    
    ee<-colnames(WR)
    ee<-gsub("\"","",ee) # Remove extra "
    
    # Translate the upper case according to dictionary  
    for(i in 1:length(dict)){
      ee<-gsub(paste("(^",toupper(dict[i]),"$)",sep=""),paste(dict[i],sep=""),ee) # exact matches
      ee<-gsub(paste("(^",toupper(dict[i]),")([0-9]?O[0-9]?)",sep=""),paste(dict[i],"\\2",sep=""),ee) #oxides
      ee<-gsub(paste("^",toupper(dict[i]),"[.]",sep=""),paste(dict[i],".",sep=""),ee) #ending with dot?
    }
    ee<-gsub("^Co2$","CO2",ee) # error in CO2
    
    # Translate the lower case according to dictionary (EarthChem org)
    for(i in 1:length(dict)){
      ee<-gsub(paste("(^",tolower(dict[i]),"$)",sep=""),paste(dict[i],sep=""),ee) # exact matches
    } 
        
    colnames(WR)<-ee
    
    colnames(WR)[colnames(WR)=="H2O_PLUS"]<-"H2O.PLUS"
    colnames(WR)[colnames(WR)=="H2OPLUS"]<-"H2O.PLUS"
    colnames(WR)[colnames(WR)=="H2O_PLUS"]<-"H2O.PLUS"
    colnames(WR)[colnames(WR)=="H2OP"]<-"H2O.PLUS"
    colnames(WR)[colnames(WR)=="H2O+"]<-"H2O.PLUS"
    colnames(WR)[colnames(WR)=="H2O_MINUS"]<-"H2O.MINUS"
    colnames(WR)[colnames(WR)=="FeOT"]<-"FeOt"
    colnames(WR)[toupper(colnames(WR))=="FEOTOT"]<-"FeOt"
    colnames(WR)[colnames(WR)=="FeO*"]<-"FeOt"
    colnames(WR)[colnames(WR)=="Fe2O3T"]<-"Fe2O3t"
    colnames(WR)[colnames(WR)=="Fe2O3*"]<-"Fe2O3t"
    colnames(WR)[toupper(colnames(WR))=="FE2O3TOT"]<-"Fe2O3t"
    
    # Translate isotopes from EarthChem
    colnames(WR)[colnames(WR)=="sr87_sr86"]<-"87Sr/86Sr"
    colnames(WR)[colnames(WR)=="nd143_nd144"]<-"143Nd/144Nd"
    colnames(WR)[colnames(WR)=="pb206_pb204"]<-"206Pb/204Pb"
    colnames(WR)[colnames(WR)=="pb207_pb204"]<-"207Pb/204Pb"
    colnames(WR)[colnames(WR)=="pb208_pb204"]<-"208Pb/204Pb"
    colnames(WR)[colnames(WR)=="hf176_hf177"]<-"176Hf/177Hf"
    colnames(WR)[colnames(WR)=="lu176_hf177"]<-"176Lu/177Hf"
    colnames(WR)[colnames(WR)=="os187_os188"]<-"187Os/188Os"
    colnames(WR)[colnames(WR)=="he3_he4"]<-"3He/4He" 
    
    rownames(WR)<-rownames(labels)
    
    .assignWithNamespaceHard("labels",labels)
  
    assign("WR",WR,.GlobalEnv)
    #.assignWithNamespaceHard("WR",WR) # NEW SAFETY MEASURE
    
    #if(!merging & .Platform$GUI=="Rgui"){
    #    menuOff()
    #    options(show.error.messages=FALSE)
    #    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
    #        winMenuDel("Plugins")
    #        ee<-winMenuAdd("Plugins")
    #    }
    #    sourceDir("Plugin","plugin")
    #}else{
    #    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
    #        winMenuDel("Plugins")
    #        ee<-winMenuAdd("Plugins")
    #    }
    #    #menuOff()
    #}
    
    if(getOption("gcd.menus")!=""){
    #if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
        menuOff()
        options(show.error.messages=FALSE)
        #winMenuDel<-get("winMenuDel",.GlobalEnv)
        #winMenuAdd<-get("winMenuAdd",.GlobalEnv)
        #winMenuNames<-get("winMenuNames",.GlobalEnv)
        
        try(winMenuDel("Plugins"))
        if(getOption("gcd.menus")!="tcltk") ee<-try(winMenuAdd("Plugins"))
    }
    
    if(!merging){
        sourceDir("Plugin","plugin")
    }
    
    if(GUI){
        message<-paste(nrow(WR)," samples loaded successfully \n","Default grouping on Symbol",sep="")
        winDialog(type="ok",message)
    }
    
    if(.Platform$OS.type=="windows" & .Platform$GUI=="Rgui" & source.first){ # Windows GUI only
        if(!clipboard){
            #print(dirname(filename))
            filename<-get("filename",.GlobalEnv) #NEW
            setwd(dirname(filename)) 
        }
        if(!merging){
            if(package.name=="GCDkitDevelop"){
                source(paste(gcdx.dir,"/GCDkit.r",sep=""))
            }else{
                source(paste(gcdx.dir,"/",package.name,".r",sep=""))
            }
        }
        if(package.name=="GCDkitDevelop"|package.name=="GCDkit").generateUserHTMLHelp()
        menuOn()
    }
    
    if(.Platform$OS.type=="windows" & (.Platform$GUI=="RTerm"|.Platform$GUI=="RStudio") & source.first){ # Windows non GUI, including RStudio
        if(!merging){
            if(package.name=="GCDkitDevelop"){
                source(paste(gcdx.dir,"/GCDkit.r",sep=""))
            }else{
                source(paste(gcdx.dir,"/",package.name,".r",sep=""))
            }
        }
    }
    
    if(.Platform$OS.type!="windows" & source.first){ # Non-windows
        if(!merging){
            if(package.name=="GCDkitDevelop"){
                source(paste(gcdx.dir,"/GCDkit.r",sep=""))
            }else{
                source(paste(gcdx.dir,"/",package.name,".r",sep=""))
            }
        }
    }

   if(getOption("gcd.menus")!=""& length(get("WRCube",.GlobalEnv))==0){
   #if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui" & length(get("WRCube",.GlobalEnv))==0){
        winMenuAddItem<-get("winMenuAddItem",.GlobalEnv)
        if(package.name=="GCDkitDevelop"){
            winMenuAddItem("GCDkit","Switch/restore a dataset","disable")
            winMenuAddItem("GCDkit","Purge stored datasets","enable")
        }else{
            winMenuAddItem(package.name,"Switch/restore a dataset","disable")
            winMenuAddItem(package.name,"Purge stored datasets","enable")
        }
   }
   
   # Backup into the cube NEW
   if(filename!="R data"&pokeDataset){
        krychle<-basename(filename)
        i<-grep(krychle,names(WRCube))
        if(length(i)>0){ # If the name already exists, add a time stamp 
            time<-strsplit(as.character(Sys.time())," ")[[1]][2]
            krychle<-paste(krychle,time)
        }
        pokeDataset(krychle)  
        assign("dataset.name",krychle,.GlobalEnv)
        if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui") setWindowTitle(paste("//","GCDkit - ",dataset.name,sep=""))
   }
   assign("results",numeric(0),.GlobalEnv)

   flush(stdout())
   if(getOption("gcd.menus")=="tcltk"){
        tkfocus(topMenu)
        if(.Platform$OS.type=="windows"){
            utils::loadhistory(file = hist.file)
            #loadhistory(file = ".Rhistory")
        }
        tkraise(tt.menu)
   }
   figaroOff()
   invisible()
}

.fixColNames<-function(txt){
   dupl<-unique(txt[duplicated(txt)])
   if(length(dupl)>0){
        if(.Platform$GUI=="Rgui") winDialog(type="ok","These column names will be altered!")
        cat("\nNew names of duplicated columns:\n")
        return(txt)
   }
   suf<-c("x","y","z",letters[1:23])
   ee<-lapply(dupl,function(i){
        which<-grep(paste("^",i,"$",sep=""),txt)
        n<-length(which)
        z<-paste(rep(i,n),suf[1:n],sep=".")
        cat(paste(z,collapse=", "),"\n")
        txt[which]<-z
        assign("txt",txt,envir=parent.env(environment()))
        return(z)
   })
   return(txt)
}

############################################################################
#                           Load new data via ODBC                         #
############################################################################

loadDataOdbc<-function(filename=NULL,na.strings = c("NA","-", "bd", "b.d.", "bdl", "b.d.l.", "N.A.","n.d."), merging= FALSE,ODBC.choose=TRUE){
    on.exit(options("show.error.messages"=TRUE))
    #ee<-require(RODBC)
    ee<-requireNamespace("RODBC",quietly=TRUE)
    if(!ee){cat("RODBC not installed...\n");return(-1)}
  
    if(is.null(filename)){
        filename<-choose.files(multi = FALSE)
        if(paste(filename," ",sep="")==" "){
            cat("Cancelled.\n")
            options(show.error.messages=FALSE)
            stop(call. = TRUE)
        }
    }
    
    suffix<-strsplit(basename(filename),"\\.")[[1]]
    if(length(suffix)==2){
        suffix<-toupper(suffix[2])
    }else{
        suffix<-""
    }
    options(show.error.messages=TRUE)
    xx<-try(switch(suffix,
    "ROC"={
     x<-read.csv(filename,header=FALSE)
       y<-as.numeric(as.vector(x[1,1]))
       if(is.na(y))y<--1
       if(y==nrow(x)-1){
            cat("NewPet ROC file...")
            flush.console()
            cat("loading...\n")
            flush.console()
            ee<-x[,1]
            x<-x[,-1]
            rownames(x)<-ee
            x<-x[-1,]
            colnames(x)<-c("Locality 1","Locality 2","Symbol","Colour","Rock Type",
                "Anhydrous","SiO2","TiO2","Al2O3","Fe2O3","FeO","MnO","MgO","CaO","Na2O",
                "K2O","P2O5","H2O","CO2","LOI","Cr","Ni","Co","Sc","V","Cu","Pb","Zn",
                "Bi","Cd","In","Sn","W","Mo","S","As","Se","Sb","Te","Ru","Rh","Pd","Ag",
                "Re","Os","Ir","Pt","Au","Hg","K","Rb","Cs","Ba","Sr","Tl","Ga","Li","Ta",
                "Nb","Hf","Zr","Ti","Y","Th","U","La","Ce","Pr","Nd","Sm","Eu","Gd","Tb",
                "Dy","Ho","Er","Tm","Yb","Lu","F","Cl","Br","I","B","Be","C","N","D",
                "13C","18O","34S","87Sr/86Sri","143Nd/144Ndi","EpsNdi","206Pb/204Pbi",
                "207Pb/204Pbi","208Pb/204Pbi","176Hf/177Hf","EpsHfi","187Os/186Osi",
                "87Sr/86Sr","87Rb/86Sr","143Nd/144Nd","147Sm/144Nd","206Pb/204Pb",
                "238U/204Pb","207Pb/204Pb","235U/204Pb","208Pb/204Pb","232Th/204Pb",
                "176Hf/177Hf","176Lu/177Hf","187Os/186Os","187Re/186Os")
            i<-apply(x,2,function(f){!all(is.na(f))}) # Filter out completely empty columns
            x<-x[,i]
            i<-apply(x,2,function(f){!all(f==0)}) # Filter out columns only with zeros
            x<-x[,i]
            x<-x[,colnames(x)!="Anhydrous"]
            cat("...ok\n")
            .loadData.process(x,merging)
            return(1)
       }else{
            cat("IgPet ROC file...")
            flush.console()
            cat("loading...\n")
            flush.console()
            # Getting the names ready 
            x<-scan(filename,what="character",sep="\n")
            i<-grep("END$",toupper(x))
            header<-as.vector(strsplit(x[1],",")[[1]])
            ee<-x[2:i]         
            ee<-paste(ee,collapse=",")
            ee<-gsub(",,",",",ee)         
            
            ee<-gsub("(\")([-a-zA-z{}0-9 ]*)(,)([-a-zA-z{}0-9 ]*)(\")","\\2\\4",ee)
            ee<-unlist(strsplit(ee,",")[[1]])
            
            ee<-ee[-length(ee)]          
            ee<-gsub("[{}]","",ee)
            ee<-gsub("[[]","",ee)
            ylab<-gsub("[]]","",ee)
            # Uffff... done.
            
            x<-x[(i+1):length(x)]
            ii<-grep("[a-zA-Z]",x)
            int<-ii[2]-ii[1]
            
            xxx<-sapply(ii,function(f) paste(x[f:(f+int-1)],collapse=","))
            x<-t(sapply(1:length(xxx), function(f) as.vector(strsplit(xxx[f],",")[[1]])))         
            
            i<-apply(x,2,function(f)!all(sapply(f,nchar)=="0"))
            x<-x[,i]
            xlab<-gsub("\"","",x[,length(header)])
            
            x<-x[,-(1:length(header))]
            colnames(x)<-ylab
            if(any(duplicated(xlab))){  # duplicated sample IDs
                message<-"WARNING! Duplicated rownames found! Replaced by sequence numbers...."
                if(.Platform$GUI=="Rgui") winDialog(type="ok",message)
                xlab<-1:nrow(x)
            }
            rownames(x)<-xlab
            #cat("...ok\n")
            
            .loadData.process(x,merging)
            return(1)
        }
        },      
    "PEG"={
        cat("PetroGraph file...")
        flush.console()
        cat("loading...\n")
        flush.console()
        x<-read.csv(filename,header=TRUE,skip=1,row.names=1)
        cat("...ok\n")
        .loadData.process(x,merging)
        return(1)
        },
    "RES"={
        cat("Glitter results file...")
        flush.console()
        cat("loading...\n")
        flush.console()
        prd<-loadGlitterData(filename,append=FALSE)
        .GlitterDataProcess(prd,append=FALSE)
        return(1)
        },
        
    "GLT"={
        cat("Glitter data imported to GCDkit...")
        flush.console()
        cat("loading...\n")
        flush.console()
        glitterLoad(filename)
        return(1)
        },     
            
    "PHM"={
        cat("Perplex (WhereAmI) file...")
        flush.console()
        cat("loading...\n")
        flush.console()
        
        # New
        #uf<-require(compiler) 
        #if(!uf){winDialog(type = "ok","Package compiler not found");stop()}
        
        .readPerplexFile<-function(filename){
            # Read the header of 12 lines
            head<-scan(file = filename,character(),n=12)
            ncol<-as.numeric(head[12])
        
            #Scan all but the header
            ee<-scan(file = filename,character(),skip=12,sep="\n")
            ee<-gsub("^[ ]{1,}","",ee)
            ee<-gsub("[ ]{1,}","\t",ee)
                
            if(.Platform$GUI=="Rgui"){
                pb<-winProgressBar(title = "Perplex import", label = "Processing line",min = 1, max = length(ee), initial = 1, width = 300)
            }else{
                pb<-txtProgressBar(title = "Perplex import", label = "Processing line",min = 1, max = nrow(WR), initial = 1,char = "=",width = NA, style = 3)
            }
            
            mat<-sapply(1:length(ee),function(i){
                z<-unlist(strsplit(ee[i],"\t"))
                if(.Platform$GUI=="Rgui"){
                    setWinProgressBar(pb, i, title = NULL, label = paste("Line",i,"of",length(ee)))
                }else{
                    setTxtProgressBar(pb, i, title = NULL, label = paste("Line",i,"of",length(ee)))
                }
                return(z)
            })      
            colnames(mat)<-NULL
       
            mat<-t(mat)
            prd<-mat[1,] # To preserve the colnames
            mat<-mat[-1,]
            colnames(mat)<-prd

            cat("...ok\n")
            #labels<<-mat[,1]
            close(pb)
            out<-list()
            out$mat<-mat
            out$head<-head
            return(out)
        }
        
        #.readPerplexFileC<-cmpfun(.readPerplexFile)
        #out<-.readPerplexFileC(filename)
        out<-.readPerplexFile(filename)
        
        mat<-out$mat
        head<-out$head
        
        # PT 
        if(any(head=="P(bar)")){
            #col.names<-c("Name","T(K)","P(bar)","vol,%","rho,kg/m3",grep("wt%",colnames(mat),value=TRUE)) # To fix the problem with wt. % phases in the current version of whereami
            col.names<-c("Name","T(K)","P(bar)","wt,%",grep("wt%",colnames(mat),value=TRUE))         
        }else{
        # TX
            col.names<-c("Name","X(C1)","T(K)","wt,%",grep("wt%",colnames(mat),value=TRUE)) # To fix the problem with wt. % phases in the current version of whereami
       }
        mat<-mat[,col.names]    
        
        # Post-processing variable names
        col.names<-gsub(",wt%","",col.names)

        dict<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","MnO","MgO","CaO","Na2O","NiO","ZrO2","Cl2")
        
        # Replace typos in numeric columns found in dictionary NEW
        for(i in dict){        
            col.names[match(toupper(i),col.names)]<-i
        }
        colnames(mat)<-col.names
        #x<-apply(mat,2,as.numeric)
        rownames(mat)<-1:nrow(mat)
        .loadData.process(mat,merging=FALSE,GUI=FALSE)
        #library(GCDPerplex) # Remove to activate
        .onPlugin()
        return(1)
        },
    
    
    "CSV"={
        cat("CSV file...")
        flush.console()
        cat("loading...\n")
        flush.console()
        x<-read.csv(filename,header=FALSE)
        
        ee<-as.character(as.matrix(x[1,])) # COLNAMES  

        # because of GeoRoc
        refs<-grep("^References:",x[-1,1])
        if(length(refs)!=0){
            cat("References used:\n") 
            results<-x[(refs+2):nrow(x),1] # show the references, save into references.txt
            cat(as.character(results),file=paste(getwd(),"references.txt",sep="/"),sep="\n")
            file.show("references.txt", title = "References")
            x<-x[1:refs,] # drop references in the end
        }
        
        ee<-gsub("(\\(WT%\\))$","",ee) # because of GeoRoc
        ee<-gsub("(\\(PPM\\))$","",ee) # because of GeoRoc
        ee<-gsub("(\\(PPT\\))$","",ee) # because of GeoRoc
        x<-x[-1,]
        colnames(x)<-ee
        
        ee<-as.character(as.matrix(x[,1])) # ROWNAMES 
        i<-grep("PETDB petdb.ldeo.columbia.edu",ee)-1
        if(length(i)!=0){
            ee<-ee[1:i]
            x<-x[1:i,]
        }
        if(any(duplicated(ee))){  # duplicated sample IDs
            message<-"WARNING! Duplicated rownames found! Replaced by sequence numbers...."
            if(.Platform$GUI=="Rgui") winDialog(type="ok",message)
            ee<-1:nrow(x)
        }else{
            x<-x[,-1]
        }         
        rownames(x)<-ee
        
        ee<-colnames(x) # COLNAMES new 
        if(any(duplicated(ee))){
            message<-paste("WARNING! duplicated column(s) found:\n",paste(ee[duplicated(ee)],collapse=", "),sep="")
            cat("...error!\n")
            if(.Platform$GUI=="Rgui") winDialog(type="ok",message)
            colnames(x)<-.fixColNames(ee)
            #options("show.error.messages"=FALSE)
            #stop("Quitting...Sorry")
        }
      
        cat("...ok\n")
        .loadData.process(x,merging)
        return(1)
    },
    
    "TBL"={
        cat("Melts output file...")
        flush.console()
        cat("loading...\n")
        flush.console()
        x<-read.csv(filename,header=FALSE)
        
        ee<-as.character(as.matrix(x[1,])) # COLNAMES  
  
        ee<-gsub("(\\(WT%\\))$","",ee) # because of GeoRoc
        ee<-gsub("(\\(PPM\\))$","",ee) # because of GeoRoc
        ee<-gsub("(\\(PPT\\))$","",ee) # because of GeoRoc
        x<-x[-1,]
        colnames(x)<-ee
        
        ee<-as.character(as.matrix(x[,1])) # ROWNAMES 
        i<-grep("PETDB petdb.ldeo.columbia.edu",ee)-1
        if(length(i)!=0){
            ee<-ee[1:i]
            x<-x[1:i,]
        }
        if(any(duplicated(ee))){  # duplicated sample IDs
            message<-"WARNING! Duplicated rownames found! Replaced by sequence numbers...."
            if(.Platform$GUI=="Rgui") winDialog(type="ok",message)
            ee<-1:nrow(x)
        }else{
            x<-x[,-1]
        }         
        rownames(x)<-ee
      
        cat("...ok\n")
        .loadData.process(x,merging)
        return(1)
    },
        
    "XLS"={
        cat("Excel file...")
        flush.console()
            
        #if(length(grep("_64-pc",R.version$platform))>0){
        if(R.version$arch=="x86_64"){
            cat("WARNING: MS Excel import is not available on 64bit systems! Quitting...\n")
            options("show.error.messages"=FALSE)
            stop()
        } 

        #channel<-odbcDriverConnect(paste("Driver={Microsoft Excel Driver (*.xls)};DriverId=790;Dbq=",filename, sep = ""))
        channel<-odbcConnectExcel(filename)
        cat("loading...\n")
        flush.console()
        ee<-sqlTables(channel)[,"TABLE_NAME"]
        ee1<-gsub("(^[']?)(.{1,})(\\$[']?$)","\\2",ee)
        i<-select.list(ee1)
        if(i==""){
            cat("Cancelled.\n")
            options(show.error.messages=FALSE)
            stop(call. = TRUE)
        }
        which.table<-ee[ee1==i]
        },

     "XLSX"={
        cat("Excel 2007 file...")
        flush.console()

        #if(length(grep("_64-pc",R.version$platform))>0){
        if(R.version$arch=="x86_64"){
            cat("WARNING: MS Excel import is not available on 64bit systems! Quitting...\n")
            options("show.error.messages"=FALSE)
            #stop()
        } 
        channel<-odbcConnectExcel2007(filename)
        cat("loading...\n")
        flush.console()
        ee<-sqlTables(channel)[,"TABLE_NAME"]
        gsub("(^[']?)(.{1,})(\\$[']?$)","\\2",ee)
        ee1<-gsub("(^[']?)(.{1,})(\\$[']?$)","\\2",ee)
        i<-select.list(ee1)
        if(i==""){
            cat("Cancelled.\n")
            options(show.error.messages=FALSE)
            stop(call. = TRUE)
        }
        which.table<-ee[ee1==i]
     },
        
    "DBF"={
        cat("dBase file...")
        flush.console()

        #if(length(grep("_64-pc",R.version$platform))>0){
        if(R.version$arch=="x86_64"){
            cat("WARNING: dBase (DBF) import is not available on 64bit systems! Quitting...\n")
            options("show.error.messages"=FALSE)
            stop()
        } 

        #channel<-odbcDriverConnect(paste("Driver={Microsoft dBASE Driver (*.dbf)};DriverID=277;Dbq=",dirname(filename), sep = ""))
        channel<-odbcDriverConnect(paste("Driver={Microsoft dBASE Driver (*.dbf)};DriverID=21;Dbq=",dirname(filename), sep = "")) #for dBase III
        #channel<-odbcConnectDbase(filename)
        cat("loading...\n")
        flush.console()
        which.table<-strsplit(basename(filename),"\\.")[[1]][1]
    },
    
     "SHP"={
        cat("Arcview shapefile...")
        flush.console()
        options(show.error.messages=TRUE)
        
        rgdal<-requireNamespace("rgdal",quietly=TRUE) # Remove to activate
        if(!rgdal){
        #if(!require(rgdal)){
            cat("WARNING: Library rgdal is missing! Quitting...\n")
            options("show.error.messages"=FALSE)
            stop()
        } 
        
        sp<-requireNamespace("rgdal",quietly=TRUE) # Remove to activate
        if(!sp){
        #if(!require(sp)){
            cat("WARNING: Library sp is missing! Quitting...\n")
            options("show.error.messages"=FALSE)
            stop()
        }
        
        dsn<<-dirname(filename)
        setwd(dsn)
        
        #ee<-ogrListLayers(dsn)
        #if(length(ee)==1) layer<-ee else layer<-select.list(ee)
        
        layer<-gsub("[.][a-zA-Z]{1,}$","",basename(filename))
        cat("loading: ",layer,"...\n")
        flush.console()
        #ogrInfo(dsn=dsn,layer=layer)
        #OGRSpatialRef(dsn=dsn,layer=layer)
        gis<-readOGR(dsn,layer)
        #plot(readOGR(dsn,layer))
        summary(gis)
        
        # No previous map layer exists in the memory
        if(length(ls(pattern="^GCDmap$",env=.GlobalEnv))==0){
            GCDmap<-list(0)
            GCDmap[[1]]<-gis
            i<-1
        
        # If exists a previous map layer, put the new one on the top
        }else{
            GCDmap<-c(GCDmap,gis)
            i<-length(GCDmap)
        }
        names(GCDmap)[i]<-layer
                      
        cat("Projection string: ",proj4string(GCDmap[[i]]),"\n")
        longlat<-!is.na(proj4string(GCDmap[[i]]))
        
        #windows(width = 6.5, height = 6.5,title="ArcView-based map") #Commented off
        
        # make the Figaro object
        palette<-"terrain.colors"
        map.col<-selectPalette(length(GCDmap[[i]]),palette)

        ArcMapSetup(GCDmap,map.palette=palette,labels.txt=FALSE,col.txt="darkred",axes=TRUE,longlat=longlat)
        GCDmap[[i]]$map.palette<-palette[1]
        GCDmap[[i]]$map.col<-as.vector(map.col)        
        assign("GCDmap",GCDmap,.GlobalEnv)
        sourceDir("plugin/GIS","GIS plugin")
        figaroOn()
        return(1) 
    },
    
    "MDB"={
        cat("Access file...")
        flush.console()

        #if(length(grep(" x64 ",win.version()))>0){
        if(R.version$arch=="x86_64"){
            cat("WARNING: MS Access import is not available on 64bit systems! Quitting...\n")
            options("show.error.messages"=FALSE)
            #stop()
        }

        channel<-odbcDriverConnect(paste("Driver={Microsoft Access Driver (*.mdb)};Dbq=",filename, sep = ""))
        #channel<-odbcConnectAccess(filename)
        cat("loading...\n")
        flush.console()
        #which.table<-strsplit(basename(filename),"\\.")[[1]][1]
        ee<-sqlTables(channel)[,"TABLE_NAME"]
        i<-substring(ee,1,4)!="MSys"
        which.table<-select.list(ee[i])
    },
        {
        options(show.error.messages=TRUE)
        if(!ODBC.choose) return(-1) # Unrecognized type - possibly a text file?
        cat("Unrecognised type... Please select an ODBC driver!\n")
        channel <- odbcDriverConnect()
        if(channel==-1){
            cat("Cancelled.\n")
            options(show.error.messages=FALSE)
            stop(call. = TRUE)
        }
        ee<-sqlTables(channel)[,"TABLE_NAME"]
        which.table<-select.list(ee)
        }
    ))
    if(class(xx)=="try-error")cat("File reading error\n")
    #x<-sqlFetch(channel,which.table,na.strings=na.strings,as.is=TRUE,rownames=FALSE,dec=",")
    x<-sqlQuery(channel, paste("SELECT * FROM [",which.table,"]",sep=""),na.strings=na.strings,as.is=TRUE,dec=",")
    #x<-sqlQuery(channel, paste("SELECT * FROM [",which.table,"]","WHERE Intrusion='Sazava'",sep=""),na.strings=na.strings,as.is=TRUE,dec=",")
    #x<-sqlQuery(channel, paste("SELECT * FROM [",which.table,"]","WHERE SiO2>73",sep=""),na.strings=na.strings,as.is=TRUE,dec=",")
    
    if(all(is.na(x))){  
        winDialog(type="ok","Empty file/table")
            cat("...error!\n")
            odbcClose(channel)
            options(show.error.messages=FALSE)
            stop(call. = TRUE)
    }
    #xx<-sqlQuery(channel, paste("SELECT * FROM [",which.table,"]",sep=""),na.strings=na.strings,as.is=TRUE,dec=",")
    
    odbcClose(channel)
    
    ee<-!is.na(x[,1]) # NEW filter out all rows with empty sample names (NA), good for EPMA files with a lot of text in the end
    x<-x[ee,] # NEW
    
    options(show.error.messages=TRUE)
    colnames(x)<-gsub("#",".",colnames(x))
    rownames(x)<-gsub("#",".",rownames(x))
    #x<-x[!is.na(x[,1]),]
    ee<-as.character(x[,1])
    ee[which(is.na(ee))]<-paste("NA", 1:length(which(is.na(ee))),sep="")
    if(any(duplicated(ee))){  # duplicated sample IDs
            message<-"WARNING! Duplicated rownames found! Replaced by sequence numbers...."
            if(.Platform$GUI=="Rgui") winDialog(type="ok",message)
            ee<-1:nrow(x)
        }else{
            x<-x[,-1]
        }         
    rownames(x)<-ee

    cat("...ok\n")
    .loadData.process(x,merging)
    return(1)
}

#############################################################################
#                       Load/paste new data set MAIN                        #
#############################################################################
loadData<-function(filename=NULL,separators=c("\t",",",";"," "),na.strings=c("NA","-","bd","b.d.","bdl","b.d.l.","N.A.","n.d."),clipboard=FALSE,merging=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    if(getOption("gcd.menus")=="tcltk"){
    #    tkfocus(topMenu)
        if(.Platform$OS.type=="windows"){
            hist.file <<- tempfile(".Rhistory")
            utils::savehistory(hist.file)
            #savehistory(file = ".Rhistory")
        }
    #    tkraise(tt.menu)
    }
    
    if(is.null(filename)) GUI<-TRUE else GUI<-FALSE
    graphicsOff()

    #odbc<-require(RODBC)
    #if(!odbc)cat("RODBC not installed...\n")
    
    odbc<-requireNamespace("RODBC",quietly=TRUE)
    if(!odbc){cat("RODBC not installed...\n");return(-1)}
  
    if(clipboard){
            .clear.data() # Clear the memory
            filename<-file("clipboard",open="r")
            sep<-"\t"
            xx<-readLines(filename,1)
            if(is.null(xx)){
                options(show.error.messages=FALSE)
                if(.Platform$GUI=="Rgui") winDialog(type="ok","Empty clipboard")
                stop(call. = TRUE)
            }
            close(filename)
        }else{
            if(is.null(filename)){
            options(show.error.messages=FALSE)
                if(R.version$arch=="x86_64"){ # ODBC import not available on 64 bit Windows
                #if(length(grep("_64-w64",R.version$platform))>0){  # ODBC import not available on 64 bit Windows
                    ee<-matrix(ncol=2,byrow=TRUE,c(
                    "Text files (*.data,*.txt,*.asc)","*.data;*.txt;*.asc",
                    "NewPet, IgPet (*.roc)","*.roc",
                    "PetroGraph (*.peg)","*.peg",
                    "GEOROC,PETDB... (*.csv)","*.csv",
                    #"Glitter single results table (*.res)","*.res",
                    #"Glitter data imported to GCDkit (*.glt)","*.glt",
                    #"ArcView shapefiles (*.shp)","*.shp",
                    #"Perplex (WhereAmI 6.6.7) (*.phm)","*.phm", 
                    "All files (*.*)","*.*"),
                    dimnames=list(c("txt","roc","peg","csv",#"shp","phm",#"res","glt",
                    "All"),NULL))
                }else{
                    ee<-matrix(ncol=2,byrow=TRUE,c(
                    "Text files (*.data,*.txt,*.asc)","*.data;*.txt;*.asc",
                    "Excel (*.xls, *xlsx)","*.xls;*.xlsx",
                    "DBase, IgPet, MinPet, NAVDAT... (*.dbf)","*.dbf",
                    "Access (*.mdb)", "*.mdb",
                    "NewPet, IgPet (*.roc)","*.roc",
                    "PetroGraph (*.peg)","*.peg",
                    "GEOROC,PETDB... (*.csv)","*.csv",
                    #"Glitter results (*.res)","*.res",
                    #"Glitter data imported to GCDkit (*.glt)","*.glt",
                    #"Melts output (*.tbl)","*.tbl",
                    #"ArcView shapefiles (*.shp)","*.shp",
                    #"Perplex (WhereAmI 6.6.7) (*.phm)","*.phm", 
                    "All files (*.*)","*.*"),
                    dimnames=list(c("txt","xls","dbf","mdb","roc","peg","csv",#"tbl","shp","phm",#"res","glt",
                    "All"),NULL))
                } 
                def<-getwd()
                if(substr(def,nchar(def),nchar(def))!="/") def<-paste(def,"/",sep="")
               
                #if(substr(def,nchar(def),nchar(def))!="/") def<-paste(getwd(),"*.*",sep="/") else def<-paste(getwd(),"*.*",sep="")
                if(odbc){
                    filename<-choose.files(default=paste(def,"*.data",sep=""),caption = "Select a data file",multi = FALSE, filters = ee, index = 1)
                }else{
                    filename<-choose.files(default=paste(def,"*.data",sep=""),caption = "Select a data file",multi = FALSE, filters = ee)
                }
        }  

        #filename<<-filename
        if(nchar(filename)<=1){
            cat("Cancelled.\n")
            options(show.error.messages=FALSE)
            stop(call. = TRUE)
        }
        
        # Get the data type
        suffix<-strsplit(basename(filename),"\\.")[[1]]
        if(length(suffix)==2){
            suffix<-toupper(suffix[2])
        }else{
            suffix<-""
        }
        
        if(suffix!="SHP"){ 
            .clear.data() # Clear the memory
            if(.Platform$GUI=="Rgui") menuOff()
        }
        
        if(suffix!="SHP") assign("filename",filename,.GlobalEnv)
            
        # Test whether the data are of recognized ODBC type (XLS, DBF....) and RODBC is available        
        if(suffix=="XLS"|suffix=="XLSX"|suffix=="MDB"|suffix=="DBF"){
            if(length(grep("_64-pc",R.version$platform))>0){
                if(.Platform$GUI=="Rgui") winDialog(type="ok",paste("Filetype *.",suffix," not available in 64 bit R\n(without ODBC support)",sep=""))
                options("show.error.messages"=FALSE)
                stop(call. = TRUE)
            }
        }
        #if(odbc){
            i<-loadDataOdbc(filename,na.strings=na.strings,merging=merging,ODBC.choose=FALSE)
            if(i==1){
                return(invisible())
            } # Successfully read by loadDataODBC
        #}
        
        #i<-loadDataNonOdbc(filename,na.strings=na.strings,merging=merging,ODBC.choose=FALSE)
        #if(i==1){filename<<-filename;return(invisible())} # Successfully read nonODBC, special type
        
        cat("Text file...")
        flush.console()
        
        # On the basis of the first row, find out what the separator is
        xx<-scan(filename,what=character(0),sep="\n",quiet=TRUE,comment.char="#")
        xx<-xx[1]
        sep<-NULL
        
        for (i in 1:length(separators)){
            x<-strsplit(xx,separators[i])[[1]]
            if (length(x)>1){sep<-separators[i];break}
        }
       
        options(show.error.messages=TRUE)
        if(is.null(sep)){
            cat("...error!\n")
            message<-paste("Unrecognized file type: *.",rev(unlist(strsplit(filename,"[.]")))[1])
            if(.Platform$GUI=="Rgui") winDialog(type="ok",message)
            options("show.error.messages"=FALSE)
            stop("Quitting...Sorry")
        }
    }
    
    ee<-strsplit(xx,sep)[[1]]
    ee<-gsub("[ ]{1,}$","",ee)
    if(any(duplicated(ee))){
        message<-paste("WARNING! duplicated column(s) found:\n",paste(ee[duplicated(ee)],collapse=", "),sep="")
        cat("...error!\n")
        if(.Platform$GUI=="Rgui") winDialog(type="ok",message)
        #options("show.error.messages"=FALSE)
        #stop("Quitting...Sorry")
    }
   
    if(clipboard)filename<-file("clipboard",open="r")
    ee1<-try(read.table(filename,quote="",sep=sep,skip=1,comment.char="",header=FALSE,as.is=TRUE,fill=TRUE)[,1],silent=TRUE)
    
    if(class(ee1)=="try-error"){
            if(.Platform$GUI=="Rgui"){
                if(clipboard) winDialog(type="ok","Empty clipboard")
                if(!clipboard) winDialog(type="ok","Error in data file")
            }
            options(show.error.messages=FALSE)
            stop(call. = TRUE)
     }

    if(clipboard)close(filename)
   
    if(any(duplicated(ee1))){
        message<-paste("WARNING! duplicated sample(s) found:\n",paste(ee1[duplicated(ee1)],collapse=","),sep="")
        if(.Platform$GUI=="Rgui") winDialog(type="ok",message)
        stop("Quitting...Sorry")
    }
    cat("loading...\n")
    flush.console()

    if(clipboard)filename<-file("clipboard",open="r")
     #options(show.error.messages=FALSE)
      
     ee2<-try(read.table(filename,quote="",sep=sep,skip=0,comment.char="",header=FALSE,as.is=TRUE,fill=TRUE)[2,],silent=TRUE)
     if(class(ee2)=="try-error"){
            if(.Platform$GUI=="Rgui"){
                if(clipboard) winDialog(type="ok","Empty clipboard")
                if(!clipboard) winDialog(type="ok","Error in data file")
            }
            options(show.error.messages=FALSE)
            stop(call. = TRUE)
     }
   
    # On the basis of the first data row, find out what the decimal point is
    dec<-"."
    temp<-grep("[0-9][,][0-9]",as.character(ee2))
    if(length(temp)!=0 & sep!=",")dec<-","
    if(clipboard)close(filename)
    if(clipboard)filename<-file("clipboard",open="r")
    
    # Check whether there is a colnames row with one item less than the other rows
    if(length(ee)==length(ee2)){
        #NEW
        x<-try(read.table(filename,quote="",sep=sep,na.strings=na.strings,header=FALSE,comment.char="",check.names = FALSE,strip.white=TRUE,fill=TRUE,dec=dec),silent=TRUE)
        x.names<-as.vector(x[,1])
        x.names[1]<-"prd"
        rownames(x)<-x.names
        colnames(x)<-.fixColNames(ee) # NEW
        # Filter out all rows starting with "#" = comments
        i<-grep("^#",x[,1])
        if(length(i)>0) x<-x[-i,]
        x<-x[,-1]
        x<-x[-1,]
    }else{
        #NEW
        x<-try(read.table(filename,quote="",sep=sep,na.strings=na.strings,comment.char="",check.names=FALSE,strip.white=TRUE,fill=TRUE,dec=dec),silent=TRUE)
        # Filter out all rows starting with "#" = comments
        i<-grep("^#",x[,1])
        if(length(i)>0) x<-x[-i,]
        if(all(colnames(x)==paste("V",1:ncol(x),sep=""))){
            rownames(x)<-x[,1]
            x<-x[,-1]
            x<-x[-1,]
        }
        colnames(x)<-.fixColNames(ee) # NEW
    }
    
    if(class(x)=="try-error"){
            if(file=="clipboard") print("Empty clipboard")
            if(file!="clipboard") print("Error in data file")
            stop(call. = TRUE)
    }
   
    if(clipboard){
        close(filename)
        writeClipboard("")
        filename<<-"clipboard"
    }
    .loadData.process(x,merging,clipboard,GUI)
    invisible()
}

#############################################################################
#                           Edit the current data set                       #
#############################################################################

editData<-function(x=WR){
    
    if(sys.call()=="editData(WR)") sourcing<-TRUE else sourcing<-FALSE
    x<-edit(x)
    if(sourcing){
        assign("WR",x,.GlobalEnv)
            if(package.name=="GCDkitDevelop"){
                source(paste(gcdx.dir,"/GCDkit.r",sep=""))
            }else{
                source(paste(gcdx.dir,"/",package.name,".r",sep=""))
            }
        assign("WRanh",WRanh,.GlobalEnv)
        assign("milli",milli,.GlobalEnv)
    }
    
    if(sys.call()=="editData(labels)") .assignWithNamespaceHard("labels",x)
    invisible()
}

editLabels<-function(){
    editData(labels)
    invisible()
}



#############################################################################
#                           Save the current data set                       #
#############################################################################
saveData<-function(sep="\t"){
    on.exit(options("show.error.messages"=TRUE))
    x<-WR
    x<-x[,colnames(x)!="mg#"]
    x<-x[,colnames(x)!="Mg#"]
    x<-cbind(labels,x)
    #Compress, remove all columns fully NA
    cols<-sapply(colnames(x),function(i){
        z<-all(is.na(x[,i]))
        return(z)
    })
    cols<-which(cols)
    #cols<-names(cols)[cols]
    if(length(cols)>0){
        x<-x[,-cols]
    }
    options(show.error.messages=FALSE)
    filename<-choose.files(caption = "Select file",multi = FALSE, filters=matrix(ncol=2,byrow=TRUE,c("GCDkit text files (*.data)","*.data")))
    
    # Check for file overwrite
    ee<-file.access(filename, mode = 0)
    if(ee==0){
        eee<-winDialog(type="yesno","File exists! Overwrite?")
        if(eee=="NO"){cat("Cancelled.\n");stop(call. = TRUE)}
    }
    
    x[x==Inf]<-NA # NEW
    if(filename==""){cat("Cancelled.\n");stop(call. = TRUE)}
    write.table(x,file=filename,sep=sep,quote=FALSE,col.names=TRUE,row.names=rownames(WR),na="")
}


#############################################################################
#                    Merge new file to the current data set                 #
#############################################################################
mergeDataCols<-function(all.rows=NULL){
    on.exit(options("show.error.messages"=TRUE))
    WRold<-WR[,!apply(is.na(WR),2,all)]
    labelsold<-labels[,!apply(is.na(labels),2,all)]
    
    loadData(merging=TRUE)
    ee<-length(is.na(match(rownames(WRold),rownames(WR))))
    if(is.null(all.rows)){
        if(ee!=0){
            all.rows<-winDialog(paste("There are ",ee,"unmatched samples\nin the two datasets. Preserve all?"),type="yesno")
            all.rows<-all.rows=="YES"
        }else{
            all.rows<-all.rows==TRUE
        }
    }
    
    cat("Merging......")
    flush.console()
    
    # Merge WR data
    WRnew<-WR[,!apply(is.na(WR),2,all)]
    WR<-merge(WRold,WRnew,by.x=0,by.y=0,sort=FALSE,,all=all.rows)
    rownames(WR)<-WR[,"Row.names"]
    WR<-data.matrix(WR[,colnames(WR)!="Row.names"])
    assign("WR",WR,.GlobalEnv)  
    
    # Merge labels
    labelsnew<-labels[rownames(WRnew),]
    labels<-merge(labelsold,labelsnew,by.x=0,by.y=0,sort=FALSE,all=all.rows)
    rownames(labels)<-labels[,"Row.names"]
    labels<-labels[,colnames(labels)!="Row.names"]
    
    
    # CEX
    ii<-grep("^Size",colnames(labels),ignore.case=TRUE)
    if(length(ii)==1) colnames(labels)[ii]<-"Size"
    if(length(ii)>1){
        labels<-cbind(labels[,ii[1]],labels)
        colnames(labels)[1]<-"Size"
    }
      
    # Set the NA to default size
    labels[is.na(labels$Size),"Size"]<-1
    
    if(length(ii)>1)labels<-labels[,-(ii+1)]
    
    
    # COLOUR
    ii<-grep("^Colo[u]?r",colnames(labels),ignore.case=TRUE)
    if(length(ii)==1) colnames(labels)[ii]<-"Colour"
    if(length(ii)>1){
        labels<-cbind(labels[,ii[1]],labels)
        colnames(labels)[1]<-"Colour"
    }
    
    # Import colours from the second file, if matching numeric-numeric, character-character
    # Set the remaining NA to default (black) colour 
    if(is.numeric(labels$Colour)){
        if(length(ii)>1 & is.numeric(labelsnew$Colour)){
           labels[rownames(labelsnew),"Colour"]<-labelsnew$Colour 
        }
        labels[is.na(labels$Colour),"Colour"]<-1
    }else{
        labels[,"Colour"]<-as.character(labels[,"Colour"])
        if(length(ii)>1 & is.character(labelsnew$Colour)){
           labels[rownames(labelsnew),"Colour"]<-labelsnew$Colour 
        }
        labels[is.na(labels$Colour),"Colour"]<-"black"
    }
    
    if(length(ii)>1)labels<-labels[,-(ii+1)]
    
    #SYMBOL
    ii<-grep("^Symbol",colnames(labels),ignore.case=TRUE)
    if(length(ii)==1) colnames(labels)[ii]<-"Symbol"
    if(length(ii)>1){
        labels<-cbind(labels[,ii[1]],labels)
        colnames(labels)[1]<-"Symbol"
    }
   
    # Set the NA to default symbol (1 or o)
    if(is.numeric(labels$Symbol)){
         if(length(ii)>1 & is.numeric(labelsnew$Symbol)){
           i<-rownames(labelsnew)[labelsnew$Symbol!=1]
           if(length(i)>0)labels[i,"Symbol"]<-labelsnew[i,"Symbol"] 
        }
        labels[is.na(labels$Symbol),ii]<-1
    }else{
        labels[,"Symbol"]<-as.character(labels[,"Symbol"])
         if(length(ii)>1 & is.character(labelsnew$Symbol)){
           labels[rownames(labelsnew),"Symbol"]<-labelsnew$Symbol 
        }
        labels[is.na(labels$Symbol),"Symbol"]<-"o"
    }
    
    if(length(ii)>1)labels<-labels[,-(ii+1)]
    
    .assignWithNamespaceHard("labels",labels)
    #assign("labels",labels,.GlobalEnv)
    #assignInNamespace("labels",labels,"base",pos=2)

    cat("......... done!\n")

    sourceDir("Plugin","plugin")
    assign("groups",labels[,"Symbol"],.GlobalEnv)
    #assign("grouping",which(colnames(labels)=="Symbol"),.GlobalEnv)
    .assignWithNamespaceHard("grouping",which(colnames(labels)=="Symbol"))
    #assign("cex",labels$cex,.GlobalEnv)

    message<-paste(nrow(WR)," samples merged successfully \n","Default grouping on Symbol",sep="")
    winDialog(type="ok",message)
    #if(!merging){
            if(package.name=="GCDkitDevelop"){
                source(paste(gcdx.dir,"/GCDkit.r",sep=""))
            }else{
                source(paste(gcdx.dir,"/",package.name,".r",sep=""))
            }
    #}
}


mergeDataRows<-function(){
    on.exit(options("show.error.messages"=TRUE))
    # BACKUP OLD DATA, OMIT ONLY FULLY EMPTY COLUMNS
    WR.old<-WR[,!apply(is.na(WR),2,all)] 
    labels.old<-labels[,!apply(is.na(labels),2,all)]
    labels.old<-addOn("file",basename(filename),labels.old)

    #cex.old<-cex
    loadData(merging=TRUE)
    cat("Merging......")
    flush.console()
    WR.new<-WR[,!apply(is.na(WR),2,all)]
    
    # Labels
    labels.new<-labels[,!apply(is.na(labels),2,all)]
    labels.new<-addOn("file",basename(filename),labels.new)
    
    ee<-.merge.my(labels.old,labels.new)
    if(is.factor(ee[,"Colour"])) ee[,"Colour"]<-as.character(ee[,"Colour"])
    .assignWithNamespaceHard("labels",ee)
    
    # Whole-rock data
    ee<-data.matrix(.merge.my(WR.old,WR.new,rownames=FALSE))
    assign("WR",ee,.GlobalEnv)

    cat("......... done!\n")
    sourceDir("Plugin","plugin")

    .assignWithNamespaceHard("grouping",which(colnames(labels)=="Symbol"))
    #grouping<<-which(colnames(labels)=="Symbol")
    
    #if(!merging){
            if(package.name=="GCDkitDevelop"){
                source(paste(gcdx.dir,"/GCDkit.r",sep=""))
            }else{
                source(paste(gcdx.dir,"/",package.name,".r",sep=""))
            }
    #}
}


.merge.my<-function(matrix1,matrix2,rownames=TRUE){
    on.exit(options("show.error.messages"=TRUE)) 
    hledej<-function(x,y){
        rough<-data.frame(matrix(nrow=nrow(x),ncol=1))
        for (f in seq(1,length(column))){
            found<-as.numeric(1)
            found<-x[,colnames(x)==y[f]]
                if (length(found)>1){rough<-cbind(rough,found)}
                else{rough<-cbind(rough,rep(NA,nrow(x)))}
        }
        rough<-rough[,-1]
        colnames(rough)<-y
        return(rough)
    }
   
    if(rownames){
        xlab<-c(rownames(matrix1),rownames(matrix2))
        print(xlab)
        if(any(duplicated(xlab))){  # duplicated sample IDs
            message<-"WARNING! Duplicated rownames found! Replaced by sequence numbers...."
            winDialog(type="ok",message)
            cat("Duplicated are:\n")
            print(unique(xlab[duplicated(xlab)]))
            matrix1<-addOn("old.ID",rownames(matrix1),matrix1)
            matrix2<-addOn("old.ID",rownames(matrix2),matrix2)
            xlab<-1:(nrow(matrix1)+nrow(matrix2))
        }
        options(warn=-1)
        if(all(!is.na(as.numeric(rownames(matrix1))))){
           matrix2<-addOn("old.ID",rownames(matrix2),matrix2)
           xlab<-1:(nrow(matrix1)+nrow(matrix2))
        }
        
    }
    column<-unique(c(colnames(matrix1),colnames(matrix2)))

    rough1<-hledej(matrix1,column)
    rough2<-hledej(matrix2,column)
    rough<-rbind(rough1,rough2)
    if(rownames)rownames(rough)<-xlab
return(rough)
}

# switches all the menus associated with plates, deletes list plate and plate.data
.plateOff<-function(){
    options(show.error.messages=FALSE)
    try(close.screen(all = TRUE))
    assign("plate",NULL,.GlobalEnv)
    assign("plate.data",NULL,.GlobalEnv)
    
    if(.Platform$OS.type!="windows"|.Platform$GUI!="Rgui") return() # Menu attached to Console not implemented apart from RGUI
    
    n<-dev.cur()
    i<-paste("$Graph",1:(n-1),"Popup",sep="")
    
    if(getOption("gcd.menus")!=""){
    #if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
        lapply(i,function(g){
            ee<-try(winMenuDel(g))
        })
    }
}

# Closes all graphic windows
graphicsOff<-function(){
    on.exit(options("show.error.messages"=TRUE))
    options(show.error.messages=FALSE)
    x<-dev.list()
    if(!is.null(x)){
        for(i in x){
            try(ee<-dev.off(i))
        }
        try(.plateOff())
        close.screen(all.screens=TRUE)
     }
     invisible()    
}
    
#############################################################################
#              Save all the graphic windows to PS                           #
#############################################################################
psAll<-function(filename=NULL){
    on.exit(options("show.error.messages"=TRUE))
    if(is.null(filename)){
    #filename<-winDialogString("Output file","out")
    #if(is.null(filename)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}

    options(show.error.messages=FALSE)
        filename<-choose.files(caption = "Select file",multi = FALSE,filters = Filters[c("ps", "All"),],index=1)
        if(is.null(filename)){cat("Cancelled.\n");stop(call. = TRUE)}
    }
    
    x<-dev.list()
    for(i in 1:length(x)){
        dev.print(postscript,fg="black",horizontal=FALSE,file=paste(filename,dev.cur()-1,".ps",sep=""),paper="a4",onefile=FALSE)
        dev.set(dev.prev())
    }
    dev.set(i)
    message<-paste(length(x)," graphs saved successfully",sep="")
    winDialog(type="ok",message)
}


#############################################################################
#              Save all the graphic windows to PDF                          #
#############################################################################
pdfAll<-function(filename=NULL){
    on.exit(options("show.error.messages"=TRUE))
    pdf.options(useDingbats=FALSE)
    options(show.error.messages=FALSE)
    if(is.null(filename)){
        filename<-choose.files(caption = "Select file",multi = FALSE,filters = Filters[c("pdf", "All"),],index=1)
        if(is.null(filename)){cat("Cancelled.\n");stop(call. = TRUE)}
    }
    
    # Test for file overwrite
    ee<-file.access(filename, mode = 0)
    if(ee==0){
        eee<-winDialog(type="yesno","File exists! Overwrite?")
        if(eee=="NO"){cat("Cancelled.\n");stop(call. = FALSE)}
    }
    if(filename==""){cat("Cancelled.\n");stop(call. = TRUE)}
   
    x<-dev.list()
    pdf(filename,paper="a4")
    pedef<-dev.cur()

    for(i in 1:length(x)){
        dev.set(x[i])
        dev.copy(which=pedef)
    }
    dev.off(which=pedef)
    message<-paste(length(x)," graphs saved successfully",sep="")
    winDialog(type="ok",message)
}


#############################################################################
#                Prints summary about the current dataset                   #
#############################################################################
info<-function(){
    cat("##########################################\n")
    cat("# SUMMARY INFORMATION ABOUT THE DATAFILE #\n")
    cat("##########################################\n")
    cat("Filename: ",filename,"\n")
    cat("\n\n")


    for (f in 1:ncol(labels)){
        if(length(levels(factor(labels[,f])))!=0) {
            cat(colnames(labels)[f],":")
            print(table(factor(labels[,f])))
            cat("\n\n")
        }
    }


    cat("Number of variables: ",ncol(WR),"\n")
    print(colnames(WR))
    x<-apply(WR,2,is.na)
    cases<-apply(!x,2,sum)
    cat("\nAvailable data: \n")
    print(cases)

    nn<-nrow(WRCube[[dataset.name]]$WR)
    cat("\nTotal number of samples: ",nn)
    #cat("\nTotal number of samples: ",nrow(WR.bak))
    cat("\nof which are ",nrow(WR)," selected:","\n")
    print(rownames(WR))
    cat("\n")
    cat("\nCurrent grouping: ")
        if(grouping>0){
            cat(colnames(labels)[grouping],"\n")
            cat("with the following levels and frequencies:\n")
            table(factor(groups))
        }else{
            if(grouping==-1)cat("based on diagram/cluster analysis\n\n")
            if(grouping==-100)cat("based on user parameter\n\n")
        }
}


#############################################################################
#                             Print a single parameter                      #
#############################################################################
printSingle<-function(default=""){
    where<-selectColumnLabel(colnames(WR),message="Select a variable to print, e.g. (SiO2+2.1)*TiO2/3\nor press ENTER to pick from a list",default=default,sample.names=FALSE,silent=TRUE,empty.ok=FALSE)
    if(!is.na(as.numeric(where))){
        where<-colnames(WR)[where]
        x.data<-WR[,where]
    }else{
        ee<-calcCore(where)
        x.data<-ee$results
        where<-ee$equation
    }
    cat("Variable:",where,"\n")
    print(x.data)
    results<<-x.data
}


#############################################################################
#                                Print samples                              #
#############################################################################

printSamples<-function(elems=NULL,which=NULL,select.samples=FALSE,print=TRUE){
    if(select.samples){
        which<-selectSamples(print=FALSE) 
    }
    
    if(is.null(elems)){
        where<-c(colnames(WR),colnames(labels))
        elems<-selectColumnsLabels(where=where,exact.only=FALSE)
    }
    if(is.null(which)) which<-rownames(WR)
    lab<-unlist(strsplit(elems,","))
    i<-lab[!is.na(match(lab,colnames(labels)))]  # these are labels
    ii<-lab[is.na(match(lab,colnames(labels)))]  # this is everything else (numeric data, formulae, mistakes)
    ee<-labels[i] # select the labels for printing 
    tmp<-cbind(labels,WR)
    if(length(ii)!=0){
        ee<-cbind(ee,sapply(ii,function(j){
                out<-calcCore(j,where="tmp")
                lab[lab==j]<-out$equation
                assign("lab",lab,pos=parent.env(environment()))
                return(out$results)
            },simplify=TRUE) 
        )
        # get the numeric data ready, calculate them if necessary  
    }
    #ee<-subset(ee,rownames(ee)==which)
    ee<-ee[which,,drop=FALSE]
    colnames(ee)<-lab
    if(print){
        if(ncol(ee)==1){
            print(t(ee))
        }else{
            print(ee)
        }
    }
    results<<-as.matrix(ee)
    return(results)
}

#############################################################################
#                             Append a single parameter                      #
#############################################################################
appendSingle<-function(){
    on.exit(options("show.error.messages"=TRUE))
    x<-winDialog(type="yesno","Append to numeric data?")
    if (x=="YES") what<-winDialogString("Enter the name for the new variable","")
    else what<-winDialogString("Enter the name for the new label","")
    if(is.null(what)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}


    if(any(c(colnames(WR),colnames(labels))==what)){
        ee<-(winDialog(type="ok",paste("Variable or label \'",what,"\' already exists! ",sep="")))
    }else{
        if(x=="YES"){
            WR<<-cbind(WR,NA)
            colnames(WR)[ncol(WR)]<<-what
        }else{
            labels<-cbind(labels,"")
            colnames(labels)[ncol(labels)]<-what
            .assignWithNamespaceHard("labels",labels)
        }
    }
}


#############################################################################
#                            Deletes a single parameter                     #
#############################################################################
deleteSingle<-function(){
    on.exit(options("show.error.messages"=TRUE))
    where<-c(colnames(labels),colnames(WR))
    x<-selectColumnLabel(where,message="Select a variable or label to DELETE \n(Some are mandatory and cannot be deleted!)",default="",sample.names=FALSE,silent=FALSE,empty.ok=FALSE)
    if(is.character(x))return()

    what<-where[x]

    # Labels
    if(x<=ncol(labels)){
        if(x!=grouping & what!="Symbol" & what !="Colour"){
            sure<-(winDialog(type="yesno",paste("Delete label ",what,"?",sep="")))
                if(sure=="YES"){
                    .assignWithNamespaceHard("labels",labels[,-x])
                    #assign("labels",labels[,-x],.GlobalEnv)
                }
        }else{
            ee<-(winDialog(type="ok",paste("Cannot delete label ",what,"!",sep="")))
        }

    # WR
    }else{
        if(!any(what==major)){
            sure<-(winDialog(type="yesno",paste("Delete numerical column ",what,"?",sep="")))
                if(sure=="YES"){
                    assign("WR",WR[,-(x-ncol(labels))],.GlobalEnv)
                    #WR<<-WR[,-(x-ncol(labels))]
                }
        }else{
            ee<-(winDialog(type="ok",paste("Cannot delete data column ",what,"!",sep="")))
        }
    }
}


#############################################################################
#            Prints cross table of labels for the current dataset           #
#############################################################################
crosstab<-function(plot=TRUE){
    x<-selectColumnLabel(message="Select the first variable\nor press ENTER to pick from a list",empty.ok=FALSE)
    xfact<-factor(labels[,x])

    y<-selectColumnLabel(message="Select the second variable\nor press ENTER to proceed",empty.ok=TRUE,print=FALSE)

    if(length(y)==0){
        results<<-table(xfact,dnn=c(colnames(labels)[x]))
        if(plot){
            windows(width = 9, height = 6, pointsize = 12)
            plot(xfact,density=20,horiz=FALSE,cex.names=0.7,cex.axis=0.7,las=2,names.arg=abbreviate(levels(xfact),12))
        }
    }else{
        yfact<-factor(labels[,y])
        z<-selectColumnLabel(message="Select the third variable\nor press ENTER to proceed",empty.ok=TRUE,print=FALSE)
            if(length(z)==0){
                results<<-table(xfact,yfact,dnn=c(colnames(labels)[x],colnames(labels)[y]))
                if(plot){
                        windows(width = 9, height = 6, pointsize = 12)
                        nf <- layout(matrix(c(1,1,1,1,2,2,1,1,1,1,2,2), 2,6,byrow=TRUE))
                        layout.show(nf)
                    plot(xfact,yfact,col=rep(barvy,100),xlab="",ylab="",angle=45,horiz=FALSE,cex.names=0.7,cex.axis=0.7,las=2,legend=FALSE,names.arg=abbreviate(levels(xfact),12))
                    plot(1,1,type="n",axes=FALSE,xlab="",ylab="",xlim=c(-1,1),ylim=c(-1,1))
                    legend(-1.1,1,abbreviate(levels(yfact),25),fill=rep(barvy,100),bty="n")
                }
            }else{
                zfact<-factor(labels[,z])
                results<<-table(xfact,yfact,zfact,dnn=c(colnames(labels)[x],colnames(labels)[y],colnames(labels)[z]))
            }
    }
    print(results)
}


#############################################################################
#     Classifies the current dataset on the basis of a single parameter     #
#############################################################################
cutMy<-function(where=NULL,int=NULL,int.lab=NULL,na.lab="Unclassified"){
    on.exit(options("show.error.messages"=TRUE))
    if(is.null(where)){
        GUI<-TRUE
        where<-selectColumnLabel(colnames(WR),message="Select a variable, e.g. (SiO2+2.1)*TiO2/3\nor press ENTER to pick from a list",default="",sample.names=FALSE,silent=TRUE,empty.ok=FALSE)
    }else{
        GUI<-FALSE
    }
    
    
    # Calculate the data if necesssary
    if(!is.na(as.numeric(where))){
            where<-colnames(WR)[where]
            x.data<-WR[,where]
    }else{
            ee<-calcCore(where)
            x.data<-ee[[2]]
            where<-ee[[1]]
    }
    
    if(GUI){
        int<-winDialogString("specify cutoff values (separated by commas)",paste(round(mean(x.data,na.rm=TRUE),2),collapse=","))
        if(is.null(int)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        int<-as.character(int)
        int<-as.numeric(strsplit(int,",")[[1]])
        if(any(is.na(int))){
                winDialog(type="ok","Error in intervals!")
                return()
        }
    }
    
    if(min(x.data,na.rm=TRUE)<min(int))int<-c(0,int)
    if(max(x.data,na.rm=TRUE)>max(int,na.rm=TRUE))int<-c(int,max(x.data,na.rm=TRUE))
        
    if(is.null(int.lab)){
        int.lab<-winDialogString(paste("Specify label names (separated by commas) for intervals\n",paste(int,collapse="-"),sep=" "),paste(LETTERS[1:(length(int)-1)],collapse=","))
        if(is.null(int.lab)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        int.lab<-strsplit(int.lab,",")[[1]]
    }
              
    groups<-cut(x.data,int,int.lab)
    groups<-as.character(groups)
    groups[is.na(groups)]<-na.lab
    
    x<-(data.frame(cbind(x.data,groups)))
    colnames(x)<-c(where,"Interval")
    if(!getOption("gcd.shut.up"))print(x)
    groups<<-groups
    .assignWithNamespaceHard("grouping",-100)
    #grouping<<--100
    if(GUI){
        x<-winDialog(type="yesno","Append current groups to labels?")
        if(x=="YES"){
            name<-winDialogString("Name of the new column",where)
            if(is.null(name)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
            labels<-cbind(labels,groups)
            colnames(labels)[ncol(labels)]<-name
            .assignWithNamespaceHard("labels",labels)
            .assignWithNamespaceHard("grouping",ncol(labels))
            #grouping<<-ncol(labels)
        }
    }
}

#############################################################################
#                                                                           #
#                    AUXILIARY GRAPHICAL FUNCTIONS                          #
#                                                                           #
#############################################################################


.round.min.down<-function(x,dec.places=NULL,expand=FALSE){
    if(is.null(dec.places)) dec.places=nchar(gsub("^[0-9]{1,}[.]?","",abs(min(x,na.rm=TRUE,finite=TRUE))))
    #x<-as.integer(min(x,na.rm=TRUE)*10^dec.places)
    if(expand)x<-x-0.04*abs(range(x,na.rm=TRUE,finite=TRUE)[1]-range(x,na.rm=TRUE,finite=TRUE)[2])
    x<-floor(range(x,na.rm=TRUE,finite=TRUE)[1]*10^dec.places)
    x<-x/10^dec.places
    return(x)
}


.round.max.up<-function(x,dec.places=NULL,expand=FALSE){
    if(is.null(dec.places)) dec.places=nchar(gsub("^[0-9]{1,}[.]?","",abs(max(x,na.rm=TRUE,finite=TRUE))))
    if(expand)x<-x+0.04*abs(range(x,na.rm=TRUE,finite=TRUE)[1]-range(x,na.rm=TRUE,finite=TRUE)[2])
    #x<-as.integer(max(x,na.rm=TRUE)*10^dec.places+1)
    x<-ceiling(range(x,na.rm=TRUE,finite=TRUE)[2]*10^dec.places)
    x<-x/10^dec.places
    return(x)
}


#############################################################################
#              Draws classification diagram from Figaro sheet               #
#############################################################################

plotDiagram<-function(diagram,select.samples=TRUE,new=TRUE,main=NULL,width=6.5,height=6.5,pointsize=10,bg="transparent",interactive=FALSE,...){
    on.exit(options("show.error.messages"=TRUE))
    args<-list(...)
    
    if(length(ls(pattern=paste("^",diagram,"$",sep=""),envir=.GlobalEnv))==0){
        options(show.error.messages=FALSE)
        winDialog(type="ok","Invalid diagram name!")
        stop()
    }
   
    if(!interactive){
        if(!screen())new<-TRUE
    }
    
    xx <- 0
    if(select.samples) i <- selectSamples(print=FALSE) else i<-rownames(WR)

    flush.console()

    if (!getOption("gcd.plot.bw")) {
        #plt.col <<- c("blue", "brown", "navyblue")
    }else {
        plt.col <<- c("black", "black", "black")
    }
    
    xx<-try(eval(do.call(diagram,args))) # Source the diagram code
    
    if (class(xx)=="try-error") {
       msg<-paste("Error in ",diagram,"()! Quitting.\n",sep="")
       cat(msg)
       stop(call. = FALSE)
    }
    
    pp <- figaro(demo, prefix = "sheet")
     
    i<-i[i%in%names(x.data)]
    x.data <- x.data[i]
    y.data <- y.data[i]
    sheet$demo$call$new<-new
    sheet$demo$call$diagram<-diagram # BRAND NEW
    sheet$demo$call$arguments<-args  # BRAND NEW
    sheet$demo$call$plotting.function<-"plotDiagram" # BRAND NEW

    if(!is.null(main)) sheet$demo$call$main<-main
    sheet$demo$call$cex<-subset(labels,rownames(WR)%in%names(x.data),"Size",drop=TRUE) # NEEW
    
    if(!new) bg<-"white"
    
    assign("sheet",sheet,.GlobalEnv)
     
    # If plotting in a plate
    if(!new){
        typ<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
        if(typ=="spider ") par(mar=c(5,4,4,1)) else {par(mar=c(4.5,5.5,2,1.5));par(pty="s")}
        if(typ=="ternary ") par(mar=c(0.5,0.5,1,0.5));par(pty="s")      
    } 
    assign("x.data",x.data,.GlobalEnv)
    assign("y.data",y.data,.GlobalEnv)
    
    pp$draw(x.data, y.data, xlab = sheet$demo$call$xlab, ylab = sheet$demo$call$ylab, 
        main = sheet$demo$call$main, sub = annotate(selected), xaxs = "i", 
        yaxs = "i", bg=bg,col=subset(labels,rownames(WR)%in%names(x.data),"Colour",drop=TRUE),pch=subset(labels,rownames(WR)%in%names(x.data),"Symbol",drop=TRUE),cex=subset(labels,rownames(WR)%in%names(x.data),"Size",drop=TRUE),new=new)
    #    yaxs = "i", col=labels[names(x.data),"Colour"],pch=labels[names(x.data),"Symbol"],cex=labels[names(x.data),"Size"],new=new)
    assign("pp",pp,.GlobalEnv)
   
    if (getOption("gcd.ident")) ee <- ID(x.data, y.data)
    
    # Warning if some analyses fall outside the plot limits
    out<-names(which(x.data[!is.na(x.data)]<min(sheet$demo$call$xlim)))
    out<-c(out,names(which(x.data[!is.na(x.data)&x.data!="Inf"]>max(sheet$demo$call$xlim))))
    out<-c(out,names(which(y.data[!is.na(y.data)]<min(sheet$demo$call$ylim))))
    out<-c(out,names(which(y.data[!is.na(y.data)&y.data!="Inf"]>max(sheet$demo$call$ylim))))
    out<-unique(out)
    if(length(out)>0){
       cat("WARNING! - in",.fig.deeval(sheet$demo$call$main),"\nsome analyses plot out of the limits of the current plot.....\n")
       
       ee<-cbind(x.data[out],y.data[out])
       rownames(ee)<-out
       colnames(ee)<-c("x","y")
       ii<-apply(ee,1,function(i){all(!is.na(i))})
       ii<-names(ii)[ii]
       ee<-subset(ee,rownames(ee)%in%ii,drop=FALSE)
       cat("Samples not plotted:\n")
       print(ee)
       cat("\n")
       flush.console()
       
       # New
       if(getOption("gcd.menus")!=""){
            figFixLim(no.action.warn=FALSE)
       }else{
            xx<-paste(round(sheet$demo$call$xlim,2),collapse=" to ")
            yy<-paste(round(sheet$demo$call$ylim,2),collapse=" to ")
            cat("Actual plot ranges for x-axis: [", xx,"] and for y-axis: [",yy,"]\n",sep="")
       
            xx<-paste(round(range(x.data[x.data!="Inf"],na.rm=TRUE),2),collapse=" to ")
            yy<-paste(round(range(y.data[y.data!="Inf"],na.rm=TRUE),2),collapse=" to ")
            cat("Actual data ranges for x-axis: [", xx,"] and for y-axis: [",yy,"]\n",sep="")
            flush.console()       
       }
    }
    
    # If plotting in a plate
    if(!new){
        options(show.error.messages=FALSE)
        ee<-screen()
        .saveCurPlotDef(ee)      
        scr.old<<-ee
        if(ee<length(plate.data)) screen(ee+1,new=FALSE)
    }else{
        if(.Platform$GUI=="Rgui") figaroOn()
    }
    invisible()
}

.add.missing.cols<-function(what,where=WR,fill=0){
for (i in what){
    if (all(colnames(where) != i)){ 
        where<-cbind(where,fill)
        colnames(where)[ncol(where)] <- i
    }
}
return(where)#returned data matrix intended for temporary use only
}

.anydiagramlist<-function(dir,headings=NULL,heading.pos=numeric(0)){
    just.sheets<<-TRUE
    source.path <- paste(gcdx.dir,"Diagrams",dir,sep="/")
    source.names <- dir(source.path, pattern = "[.][Rr]$")
    eelist<-matrix(ncol=5)
    
    for (i in source.names) {
        i<-gsub("[.][Rr]$","", i)
        if (substr(i,1,1)!="_"){
                assign("plate",NULL,.GlobalEnv)
                ee <- try(eval(call(i)),silent=TRUE)
            if (class(ee) != "try-error"){
                get("plate",envir=.GlobalEnv)
                if(is.null(plate)){
                    x1<-c(sheet$d$t$GCDkit$plot.name,i,FALSE,sheet$d$t$GCDkit$plot.position)
                    x2<-1
                }else{
                    x1<-c(plate$plot.name,i,TRUE,plate$plot.position)
                    x2<-length(plate.data)
                }
                eelist<-rbind(eelist,c(x1,x2))
            }else{
                #cat(paste("Module",i,"failed!"),"\n") # debugging only - syntax failed or some data not present
            }
        }
    }
   
    eelist<-eelist[-1,,drop=FALSE]
    if(!is.null(headings)){
        z<-matrix(NA,nrow=length(headings)*2,ncol=5)
        prd<-sapply(1:length(headings),function(i){
            z[i*2-1,]<<-c(" ","NULL",FALSE,heading.pos[i],NA)
            z[i*2,]<<-c(headings[i],"NULL",FALSE,heading.pos[i]+0.001,NA)
            return()
        })
        
        eelist<-rbind(eelist,z)
    }
    # Sort the list according to the last column
    x<-matrix(ncol=5)
    if(nrow(eelist)>1){
        for (i in sort(as.numeric(eelist[,4]))) x<-rbind(x,eelist[eelist[,4]==i,])
        eelist<-x    
        eelist<-eelist[-1,,drop=FALSE]
    }
    colnames(eelist)<-c("menu","function","plate?","seq.number","no_panels")
    ignore<<-c(" ","===VOLCANITES===","===PLUTONITES===","===EXTRAS===","===GRANITOIDS===","===BASALTOIDS===","===UNIVERSAL===")
    just.sheets<<-FALSE
    return(eelist)
}

.claslist<-function(){
    ee<-.anydiagramlist(dir=paste("Classification",getOption("gcd.language"),sep="/"),c("===VOLCANITES===","===PLUTONITES==="),heading.pos=c(20,30))       
    return(ee)
}

.tectlist<-function(){
    ee<-.anydiagramlist(dir="Geotectonic",headings=c("===GRANITOIDS===","===BASALTOIDS===","===UNIVERSAL==="),heading.pos=c(100,200,300))       
    return(ee)
}

.userlist<-function(){
    ee<-.anydiagramlist(dir="User")       
    return(ee)
}

.generateUserHTMLHelp<-function(){
    in.dir<-paste(gcdx.dir,"Diagrams","User","doc",sep="/")
    out.dir<-paste(gcdx.dir,"GCDkit_help","Plots",sep="/")
    direct<-dir(in.dir,pattern="*htm")
    target<-HTMLInitFile(outdir=out.dir, filename="switch.user",Title = "R output", CSSFile="../gcd.css")
    
    HTML("<base target=\"main\">\n
          <h2>User-defined plots...</h2>\n
          <div class=\"OkCancel\">\n
          <div class=\"menu-envelope\">\n
          <div class=\"menu\">\n
          <div class=\"title\">Select diagram</div>
          <div class=\"central-field\">\n"
    )
    
    dialist<-.userlist()
    
    if(nrow(dialist)==0){
        out<-paste("<div class=\"item\"\n><p>No user-defined diagram templates found.</p>\n</div>\n")
        HTML(out)
        html.block<-out
    }else{
        html.block<-sapply(1:nrow(dialist),function(i){
            help.name<-paste(dialist[i,2],".htm",sep="")
            if(help.name%in%direct) help.link<-paste(in.dir,"/",help.name,sep="") else help.link<-""
                out<-paste("<div class=\"item\"\n><a href=\"",
                    help.link,"\">",dialist[i,1],"</a>\n</div>\n")
            HTML(out)
        return(out)
        },simplify=TRUE)
   }
    
    HTML("</div>\n
          <div style=\"text-align: right; margin:7px\">\n
          <img src=\"../OkCancel.gif\" align=\"bottom\" border=\"0\" height=\"25\" width=\"150\">\n
          </div>\n
          </div>\n
          </div>\n
          </div>\n
          <p>\n
          <center><a href=\"00index.html\" target=\"_self\">Back to 'Plots' menu</a></center></p>\n"
    )
    invisible(html.block)
}

fousy<-function(x,y,sy,sx=0,col=rep("black",length(x))){
    for (i in 1:length(x)){
            aa<-c(x[i]-sx[i],x[i]+sx[i])
            bb<-c(y[i],y[i])
            lines(aa,bb,col=col[i])
            eval(parse(text=paste("sheet$demo$template$lines",i,"a<<-list(\"lines\",x=aa,y=bb,col=col[",i,"])",sep="")))
            bb<-c(y[i]-sy[i],y[i]+sy[i])
            aa<-c(x[i],x[i])
            eval(parse(text=paste("sheet$demo$template$lines",i,"b<<-list(\"lines\",x=aa,y=bb,col=col[",i,"])",sep="")))
    }
    #print(sheet$d$t)
    #pp<<-figaro(demo,prefix="sheet")
    #assign("sheet",sheet,envir=.GlobalEnv)
    #assign("pp",pp,envir=.GlobalEnv)
    #assign("sheet",sheet,envir=parent.env(environment())) #.GlobalEnv
    #assign("pp",pp,envir=parent.env(environment())) #.GlobalEnv
    figRedraw()
    invisible(sheet)
}
   


#############################################################################
#                           Plot with limits                                #
#############################################################################
plotWithLimits<-function(x.data,y.data,digits.x=NULL,digits.y=NULL,log="",new=TRUE,xmin=.round.min.down(x.data,dec.places=digits.x,expand=TRUE),xmax=.round.max.up(x.data,dec.places=digits.x,expand=TRUE),ymin=.round.min.down(y.data,dec.places=digits.y,expand=TRUE),ymax=.round.max.up(y.data,dec.places=digits.y,expand=TRUE),xlab="",ylab="",fousy="",IDlabels=getOption("gcd.ident"),fit=FALSE,main="",pch=labels[names(x.data),"Symbol"],col=labels[names(x.data),"Colour"],cex=labels[names(x.data),"Size"],title=NULL,xaxs="i",yaxs="i",interactive=FALSE){
    # NEW
    on.exit(options("show.error.messages"=TRUE))
    if(length(pch)<length(x.data)) pch<-rep(pch,length.out=length(x.data))
    if(length(col)<length(x.data)) col<-rep(col,length.out=length(x.data))
    if(length(cex)<length(x.data)) cex<-rep(cex,length.out=length(x.data))
    
    #if(length(pch)==nrow(WR)) names(pch)<-rownames(WR) 
    #if(length(col)==nrow(WR)) names(col)<-rownames(WR)
    #if(length(cex)==1) cex<-rep(cex,nrow(WR)) 
    #if(length(cex)==nrow(WR)) names(cex)<-rownames(WR)
    
    ii<-!is.na(x.data)&!is.na(y.data)
    x.data<-x.data[ii]    
    y.data<-y.data[ii]
    pch<-pch[ii]
    col<-col[ii]
    
    # /NEW

    # Plotting window title
    if(is.null(title)){
        tit<-"Binary plot of "
        if(log=="x" | log=="xy") tit<-paste(tit,"log",sep="")
        tit<-paste(tit,xlab,"vs.")
        if(log=="y" | log=="xy") tit<-paste(tit,"log",sep="")
        tit<-paste(tit,ylab)
    }else{
        tit<-title
    }

    #windows(width = 6.5, height = 6.5, pointsize = 10,title=tit)
    if(length(grep("x",log)!=0)){
        if(any(x.data<=0,na.rm=TRUE)){winDialog(type="ok",paste(xlab,"- negative or zero data not allowed on log plots!"));options("show.error.messages"=FALSE);stop("",call. = FALSE)}
        if(xmin<=0) xmin<-min(x.data,na.rm=TRUE)
    }
    
    if(length(grep("y",log)!=0)){
        if(any(y.data<=0,na.rm=TRUE)){winDialog(type="ok",paste(ylab,"- negative or zero data not allowed on log plots!"));options("show.error.messages"=FALSE);stop("",call. = FALSE)}
        if(ymin<=0) ymin<-min(y.data,na.rm=TRUE)
    }
    
    x.data<<-x.data
    y.data<<-y.data

    temp<-list(
        GCDkit=list("NULL",plot.type="binary",plot.name=tit)
    )
    
    sheet<-list(demo=list(fun="plot",call=list(xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="green",bg="transparent",fg="black",xlab=annotate(xlab),ylab=annotate(ylab),log=log,main=annotate(main)),template=temp))
    sheet$demo$call$new<-new
    
    # NEW
    #ii<-!is.na(x.data)&!is.na(y.data)
    #options("show.error.messages"=FALSE)
    #x.data<-x.data[ii]
    #y.data<-y.data[ii]
    #pch<-try(pch[ii])
    #pch<-col[!is.na(pch)]
    
    #col<-try(col[ii])
    #col<-col[!is.na(col)]
   
    #cex<-try(cex[ii])
    #cex<-cex[!is.na(cex)]
    #options("show.error.messages"=TRUE)
    # /NEW
    
    if(!new & !interactive) sheet$demo$call$bg<-"white"
    assign("sheet",sheet,.GlobalEnv)
    if(is.numeric(fousy)){
        sheet<-fousy(x.data,y.data,fousy,col=labels$Colour)
        print(sheet$d$t)
        #assign("pp",pp,.GlobalEnv)
    }else{
        pp<<-figaro(demo,prefix="sheet")
        pp$draw(x.data,y.data,xlab=annotate(xlab),ylab=annotate(ylab),col=col,pch=pch,cex=cex,xaxs=xaxs,yaxs=yaxs,main=annotate(main),new=new)                     

        #NEW
        sheet$demo$call$pch<-rep(pch,length.out=length(x.data))
        names(sheet$demo$call$pch)<-names(x.data)
        sheet$demo$call$col<-rep(col,length.out=length(x.data))
        names(sheet$demo$call$col)<-names(x.data)
        sheet$demo$call$cex<-rep(cex,length.out=length(x.data))
        names(sheet$demo$call$cex)<-names(x.data)
        #/NEW
        sheet$demo$call$xaxs<-xaxs
        sheet$demo$call$yaxs<-yaxs
    }
    assign("sheet",sheet,.GlobalEnv)
    
    if(fit){
        if(length(grep("y",log))>0)ss<-paste("lm(log10(y.data)~",sep="") else ss<-paste("lm(y.data~",sep="")
        if(length(grep("x",log))>0)ss<-paste(ss,"log10(x.data))",sep="") else ss<-paste(ss,"x.data)",sep="")
        izoch<-eval(parse(text=ss))
        abline(izoch,lty=2, col="blue")
    }
    
    if(new)figaroOn()
    
    if(screen()){
        plate[[screen()]]<-sheet
        plate.data[[screen()]]$x<-x.data
        plate.data[[screen()]]$y<-y.data
        
        assign("plate",plate,.GlobalEnv)
        assign("plate.data",plate.data,.GlobalEnv)
        .saveCurPlotDef(screen()) # NEWLY ADDED
    }
    if(IDlabels!=0)x<-ID(x.data,y.data,IDlabels)
    invisible()
}


#############################################################################
#                                 Classify                                  #
#############################################################################

classify<-function (diagram = NULL, grp = TRUE, labs = FALSE, source.sheet = TRUE, 
    overlap = FALSE, X = x.data, Y = y.data, silent = FALSE, 
    clas = sheet$d$t,...) {
    on.exit(options("show.error.messages"=TRUE))
    if(is.null(diagram)){
        GUI<-TRUE 
        diagram<-"NULL"
    }else{
        GUI<-FALSE
    }
    options(show.error.messages=FALSE)
    if (source.sheet) {
        xx <- 0
        xx<-eval(call(diagram))
        
        if (xx == "error") {
            options(show.error.messages = FALSE)
            stop("", call. = FALSE)
        }
        
        assign("sheet", sheet, inherits = TRUE)        
        assign("X", x.data, inherits = TRUE)
        assign("Y", y.data, inherits = TRUE)

    }
    ee <- paste(sheet$demo$call$log, " ")
    if (length(grep("x", ee)) != 0) 
        X <- log10(X)
    if (length(grep("y", ee)) != 0) 
        Y <- log10(Y)
    name <- list(1)
    name <- as.list(character(length(X)))
    for (i in 1:length(X)) {
        xx <- X[i]
        yy <- Y[i]
        if (!(is.na(xx) | is.na(yy))) {
            for (m in clas$clssf$use) {
                if ((min(clas[[m]]$x) <= xx & xx <= max(clas[[m]]$x)) & 
                  (min(clas[[m]]$y) <= yy & yy <= max(clas[[m]]$y))) {
                  xp <- clas[[m]]$x
                  yp <- clas[[m]]$y
                  if (xp[1] == xp[length(xp)] & yp[1] == yp[length(yp)]) {
                    xp <- c(xp, clas[[m]]$x[2])
                    yp <- c(yp, clas[[m]]$y[2])
                  }
                  else {
                    xp <- c(xp, clas[[m]]$x[1:2])
                    yp <- c(yp, clas[[m]]$y[1:2])
                  }
                  prep <- FALSE
                  for (k in 2:(length(yp) - 1)) {
                    if (min(c(xp[k], xp[k + 1])) <= xx & xx <= 
                      max(c(xp[k], xp[k + 1]))) {
                      if (xp[k] != xp[k + 1]) {
                        if (xx != xp[k] | (sign(xp[k] - xp[k - 
                          1]) == sign(xp[k + 1] - xp[k]))) {
                          prus <- ((yp[k] - yp[k + 1]) * (xx - 
                            xp[k + 1])/(xp[k] - xp[k + 1])) + 
                            yp[k + 1]
                          prus <- floor(prus * 1e+12)/1e+12
                          if (abs(prus - yp[k + 1]) > 0.000000001 | 
                            abs(xx - xp[k + 1]) > 0.000000001) {
                            if (prus == yy) {
                              prep <- TRUE
                              break
                            }
                            if (prus >= yy) {
                              prep <- !prep
                            }
                          }
                        }
                      }
                    }
                  }
                  if (prep) {
                    name[[i]] <- c(name[[i]], clas$c$rcname[m - 
                      1])
                  }
                }
            }
        }
        if (length(name[[i]]) == 1) 
            name[[i]] <- c(name[[i]], "unclassified")
        name[[i]] <- name[[i]][-1]
        if (overlap) 
            xx <- "intersection between"
        else xx <- "boundary between"
        if (length(name[[i]]) > 1) {
            name[[i]] <- paste(sort(name[[i]]), collapse = " & ")
            name[[i]] <- paste(xx, name[[i]], sep = " ")
        }
    }
    name <- as.character(name)
    if (GUI & diagram == "TAS" & !silent) {
        source(paste(gcdx.dir,"Diagrams/Classification",options("gcd.language"),"TASadd.r",sep="/"))
        name <- TASadd(name)
    }
    
    rownames(claslist) <- claslist[, 2]
    if (!silent) {
        cat("Samples classified by: ", claslist[diagram, 1], "\n")
        print(tapply(name, factor(name), length))
        flush.console()
    }
    if (grp) {
        groups <<- name
        .assignWithNamespaceHard("grouping",-1)
        #grouping <<- -1
    }
    results <<- matrix(name, length(name), 1)
    rownames(results) <<- names(X)
    if(source.sheet) colnames(results) <<- diagram else colnames(results)<<-""
    if (!silent) {
        cat("to view classification of individual samples type 'results'\n")
        if (labs) 
            x <- winDialog(type = "yesno", "Append classification to labels?")
        else x <- "NO"
        if (x == "YES") {
            name <- winDialogString("Name of the new column",diagram)
            while (any(c(colnames(labels), colnames(WR)) == name)) name <- winDialogString(paste("The name '", 
                name, "' already in use\nPlease try another", 
                collapse = ""), "")
            if (is.null(name)) {
                cat("Cancelled.\n")
                options(show.error.messages = FALSE)
                stop(call. = TRUE)
            }
            labels <- cbind(labels, results)
            colnames(labels)[ncol(labels)] <- name
            .assignWithNamespaceHard("labels",labels)
        }
    }
    return(results)
}

.check.colnames<-function(clnms,to.check){
    x<-vector()
    for (ii in to.check) x<-c(x,any(clnms==ii))
    if (any(x==FALSE)) winDialog(type="ok","Invalid data")
    return(any(x==FALSE))
}

##########################################################################
#                             Identify                                   #
##########################################################################
ID<-function(x,y,labs=getOption("gcd.ident"),offset=0.4,col="gray30",cex=1){
        if(is.numeric(labs)){
            if(labs>1){
                labs<-as.character(labels[names(x),(labs-1)])
            }else{
                labs<-names(x)
            }
        }
        if(!getOption("gcd.shut.up"))print(labs)
        if(getOption("gcd.ident.each")){
            ee<-identify(x,y,labs,offset=offset,col=col,cex=cex)
            if(length(ee)){
                ee<-names(x)[ee]
                if(!getOption("gcd.shut.up"))print(labels[ee,])
            }
        }else{
                text(x,y,labs,offset=offset,pos=3,col=col,cex=cex)
        }
}

#############################################################################
#                                                                           #
#                               CALCULATOR                                  #
#                                                                           #
#############################################################################
calcCore<-function(equation,where="WR",redo=TRUE){
    on.exit(options("show.error.messages"=TRUE))
    if(equation==""){
        out<-NULL
        eval(parse(text=paste("equation<-selectColumnLabel(colnames(",where,"),message=\"Variable not found!Select a new one\nor press ENTER to pick from a list\",empty.ok=FALSE,print=FALSE,silent=TRUE)",sep="")))
        eval(parse(text=paste("equation<-colnames(",where,")[",equation,"]",sep="")))
        eval(parse(text=paste("out<-",where,"[,\"",equation,"\"]",sep="")))
        z<-list(equation,out,equation)
        names(z)<-c("equation","results","formula")
        return(z)
    }
    yy<-equation
    #ee<-eval(parse(text=where)) # OLD VERSION
    ee <- eval(parse(text = where), envir = parent.frame()) # From JFM
    if(any(colnames(ee)==equation)){
        z<-list(equation,ee[,equation],equation)
        z[[3]]<-paste(where,"[,\"",equation,"\"]",sep="")
    }else{
        fields<-strsplit(yy,"[-+**//()^^~~,,]")
        fields<-unlist(fields)
        x<-match(fields,eval(parse(text=paste("colnames(",where,")",sep="")),envir = parent.frame())) #NEW
        #x<-match(fields,eval(parse(text=paste("colnames(",where,")",sep="")))) #OLD VERSION
        fields<-fields[!is.na(x)]
        x<-x[!is.na(x)]

        if(length(fields)!=0){
            for(i in 1:length(fields)){
                hovno<-eval(paste("ee","[,",paste("\"","@",i,"\"",sep=""),"]",sep="")) #NEW
                yy<-sub(fields[i],hovno,yy) #NEW
                #yy<-sub(fields[i],paste(where,"[,",paste("\"","@",i,"\"",sep=""),"]",sep=""),yy) #OLD VERSION
            }
            for(i in 1:length(fields)){
                yy<-(sub(paste("@",i,sep=""),fields[i],yy))
            }
        }
        options(show.error.messages = FALSE)
            out<-try(eval(parse(text=yy)))
        if(class(out)=="try-error"){
            #winDialog("ok","Error in calculation")
            if(!redo){
                z<-list(equation,NA,yy)
                names(z)<-c("equation","results","formula")
                return(z)
            }
            cat(yy,"not found.\n")
            yy<-winDialogString("Error in calculation! Please correct the formula",equation)
            if(is.null(yy)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
            out<-calcCore(yy,where)
            return(out)
        }
        out[out==Inf]<-NA # NEW
        z<-list(equation,out,yy)
    }    
        z[[3]]<-gsub("ee",where,z[[3]])
        names(z)<-c("equation","results","formula")
    return(z)
}


calc<-function(){
    on.exit(options("show.error.messages"=TRUE))
    where<-selectColumnLabel(colnames(WR),message="Select a variable to calculate, e.g. (SiO2+2.1)*TiO2/3",default="",sample.names=FALSE,silent=TRUE,empty.ok=TRUE)
    if(!is.na(as.numeric(where))){
        where<-colnames(WR)[where]
        x.data<-WR[,where]
    }else{
        ee<-calcCore(where)
        x.data<-ee$results
        where<-ee$equation
    }
    
    if(!getOption("gcd.shut.up")){cat("Calculated results: ",equation,"\n");print(x.data)}
    if(length(x.data)==nrow(WR)){
        storage<-winDialogString("Store as (variable name)",where)
        if(is.null(storage)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        if(storage!=""){
            if(any(colnames(WR)==storage)){
                winDialog(type="ok","Variable already exists, nothing stored")
                return()
            }
            WR<<-cbind(WR,x.data)
            colnames(WR)[ncol(WR)]<<-storage
            message<-paste("Parameter ",storage,"\n","was appended to your data!",sep="")
            winDialog(type="ok",message)
        }
        cat("\n")
    }
    return(results)
}

printSingle<-function(default=""){
    where<-selectColumnLabel(colnames(WR),message="Select a variable to print, e.g. (SiO2+2.1)*TiO2/3\nor press ENTER to pick from a list",default=default,sample.names=FALSE,silent=TRUE,empty.ok=FALSE)
    if(!is.na(as.numeric(where))){
        where<-colnames(WR)[where]
        x.data<-WR[,where]
    }else{
        ee<-calcCore(where)
        x.data<-ee$results
        where<-ee$equation
    }
    cat("Variable:",where,"\n")
    print(x.data)
    results<<-x.data
}


#############################################################################
#                                                                           #
#                             SCRIPTING LANGUAGE                            #
#                                                                           #
#############################################################################


##########################################################################
#                              Scripts                                   #
##########################################################################


customScript<-function(){
    on.exit(options("show.error.messages"=TRUE))
    if(length(equation)==0)equation<-""
    equation<-selectColumnLabel(colnames(WR),message="Enter formula, e.g. (SiO2*54.5+FeO*1.5)/TiO2",default=equation,sample.names=FALSE,silent=TRUE)

    if(is.null(equation)){
        winDialog(type="ok","Invalid formula")
        return()
    }

    equation<<-equation
    ee<-calcCore(equation)

    storage<-winDialogString("Store as (variable name)","new")
    if(is.null(storage)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}


    description<-winDialogString("Enter a comment","Script")
    if(is.null(description)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}

    options(show.error.messages=FALSE)
    filename<-choose.files(caption = "Select file",multi = FALSE, filters=matrix(ncol=2,byrow=TRUE,c("GCDkit calculation script (*.r)","*.r")))
    if(filename==""){cat("Cancelled.\n");stop(call. = TRUE)}
    options(show.error.messages=TRUE)
    
    results<-calcCore(equation)
    scr<-paste(eval(storage),"<-",results[[3]],sep="")
    scr<-parse(text=scr)

    x<-c(paste("#",description,sep=""),as.character(scr))
    x<-c(x,paste("addResults(\"",eval(storage),"\")",sep=""),paste("colnames(WR)[ncol(WR)]<-\"",eval(storage),"\"",sep=""))
        
    write.table(x,file=filename,quote=FALSE,col.names=FALSE,row.names=FALSE,append=TRUE)
    message<-paste("Parameter ",storage," was appended\n","to your script ",filename,sep="")
    winDialog(type="ok",message)
   
    ee<-winDialog("Append to the numeric data (matrix WR)",type="yesno")
    app<-(ee=="YES")
    if(app){
        ee<-as.numeric(results[[2]])
        names(ee)<-rownames(WR)
        eval(parse(text=paste(eval(storage),"<<-ee",sep="")))
        addResults(eval(storage))
        colnames(WR)[ncol(WR)]<<-eval(storage)
        cat("...Done")
    }
    cat("\n")
}

#############################################################################
#                                                                           #
#                             INTERFACE TO FIGARO                           #
#                                                                           #
#############################################################################
#which=NULL;pch=NULL;zoom=NULL;shaded.col=NULL;cex.in=NULL;new=sheet$demo$call$new
.spiderPlotRedraw<-function(which=NULL,pch=NULL,zoom=NULL,shaded.col=NULL,cex.in=NULL,new=sheet$demo$call$new,...){ # 
    on.exit(options("show.error.messages"=TRUE))
    bg.bk<-sheet$d$call$bg
    if(new) if(!is.null(dev.list()))dev.off(dev.cur())
    if(new){
        windows(width = 10, height = 6.5, pointsize = 12,title=sheet$demo$template$GCDkit$plot.name)
    }else{
        #sheet$demo$call$bg<<-"white"
    }
    if(!is.null(sheet$demo$legend)&!is.null(sheet$demo$legend$y)){  # Big margin only if single window (not for figMulti/plates)
        par(omd=c(0,1,0,1))
        par(mar=c(6,3,6,11))
    }
                  
    .fig.spider.axes(main=sheet$demo$call$main,sub=sheet$demo$call$sub)
    ee<-grep("points",names(sheet$demo$template))        
    if(is.null(which)){
        which<-1:nrow(y.data)
        which2<-ee
    }else{
        which2<-ee[which]  
    }
    
    for(ii in which2){
        eval(parse(text=(paste("param<-(sheet$demo$template[[",ii+1,"]])",sep=""))))
        if(is.null(cex.in)){
            cex<-eval(parse(text=(paste("param2<-(sheet$demo$template[[",ii,"]]$cex)",sep=""))))
        }
        
        pch<-eval(parse(text=(paste("param2<-(sheet$demo$template[[",ii,"]]$pch)",sep=""))))
        if(!is.null(zoom)){
            cex<-cex.in*zoom
        }
            lines(param$x,param$y,lwd=1,pch=pch,col=param$col,cex=cex,type="o")
    }  
    
    # Draw all added boxes and grid lines
    my<-sheet$demo$template
    ee<-grep("grid|box|reservoirs",names(my))
    blb<-lapply(ee,function(i){
        fun<-as.character(my[[i]][1])
        if(fun=="reservoirs") fun<-"figAddReservoirs"
        call<-my[[i]][-1]
        do.call(fun,call)
    })
    
    # Draw a legend
    if(!is.null(sheet$demo$legend)){
        args<-sheet$demo$legend
        #which<-rownames(y.data)%in%args$legend #TODO
        
        if(is.null(args$title)){ # Proper sample names in the legend
            args$legend<-args$legend[which]
            args$col<-args$col[which]
            args$pch<-args$pch[which]
        } 
        legend(x=args$x,y=args$y,legend=args$legend,inset=args$inset,col=args$col,pch=args$pch,lty=args$lty,lwd=args$lwd,cex=args$cex,bg=args$bg,bty=args$bty,ncol=args$ncol,title=args$title,xpd=args$xpd,yjust=0.5)
    }
    
    sheet$demo$call$bg<<-bg.bk # Backup of background colour
    invisible()
}

# Replots a Figaro graph
figRedraw<-function(x=x.data,y=y.data,zoom=NULL,bw=FALSE,title=NULL){  
    ee<-dev.list()
    if(is.null(ee)|!is.null(title)){
        windows(width = 6.5, height = 6.5, pointsize = 10,title=title)
    }      
    scr<-screen(new=FALSE)
    if(scr){
        #scr<-scr.old # NEW POTENTIALLY V. DANGEROUS
        .eraseScreen()
        #.saveCurPlotDef(screen())
        screen(scr,new=FALSE)
    }
    
    #if(length(x)==0) x<-0
    #if(length(y)==0) y<-0
    
    ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    
    sheet.bak<-sheet
    
    #par("mfg")
    if(ee!="GIS "|mode(labels)!="function"){ 
        if(is.null(sheet$demo$call$cex)){
            sheet$demo$call$cex<<-rep(1,length(x.data)) # NEW because of plotting when labels are not ready (e.g. spiderplots)
            #sheet$demo$call$cex<<-labels[names(x.data),"Size"]
        }
        cex<-sheet$demo$call$cex[x.data%in%x]
       
        #windows(width = 6.5, height = 6.5, pointsize = 10)
        
        if(ee!="spider "){
            if(length(sheet$demo$call$col)==length(x)){
                pch<-sheet$demo$call$pch
                col<-sheet$demo$call$col
            }else{
                #if(all(!is.na(labels[names(x),"Symbol"]))){
                    pch<-labels[names(x),"Symbol"]
                    col<-labels[names(x),"Colour"]
                #}
            }
        }
    }else{
        if(is.null(sheet$demo$call$cex)){
            sheet$demo$call$cex<<-1
            cex<-1
        }else{
            cex<-sheet$demo$call$cex
        }
        if(is.null(sheet$demo$call$pch)){
            sheet$demo$call$pch<<-19
            pch<-19
        }else{
            pch<-sheet$demo$call$pch
        }
        
        if(is.null(sheet$demo$call$col)){
            sheet$demo$call$col<<-"black"
            col<-1
        }
    }
    
        if(is.null(unlist(sheet$demo$call$xaxs))) { 
            sheet$demo$call$xaxs<<-"i"
        } 
    
        if(is.null(unlist(sheet$demo$call$yaxs))) { 
            sheet$demo$call$yaxs<<-"i"
        }
    
        if(is.null(unlist(sheet$demo$call$cex.main))) { 
            sheet$demo$call$cex.main<<-par("cex.main")
        }
    
        if(is.null(unlist(sheet$demo$call$cex.sub))) { 
            sheet$demo$call$cex.sub<<-par("cex.sub")
        }
    
    if(bw==TRUE) col<-rep("black",nrow(labels))
        if(ee=="spider " | ee=="profile "){ # Spider plots 
             .spiderPlotRedraw(new=FALSE)
        }else{
            if(is.null(zoom)){
                        xlim<-sheet$demo$call$xlim
                        ylim<-sheet$demo$call$ylim
            }else{
                        xlim<-zoom$x
                        ylim<-zoom$y
            }
                
                if(bw){
                    figReplace(what="lines",param="col",value="black")
                    figReplace(what="text",param="col",value="black")
                }
         
                if(ee=="ternary "){ # Ternary plots
                    sheet$demo$call$new<<-FALSE
                    pp<<-figaro(demo,prefix="sheet")
                    pp$draw(x,y,cex=cex,
                    pch=pch,col=col,
                    xaxs="i",yaxs="i",
                    main=annotate(sheet$demo$call$main),sub=sheet$demo$call$sub,
                    xlab=sheet$demo$call$xlab,ylab=sheet$demo$call$ylab,
                    xlim=xlim,ylim=ylim,#cex.lab=sheet$demo$call$cex.lab,
                    cex.main=sheet$demo$call$cex.main,cex.sub=sheet$demo$call$cex.sub,
                    #cex.axis=sheet$demo$call$cex.axis,
                    bg=sheet$demo$call$bg,new=FALSE) 
                }else{
                    if(ee=="GIS "){   
                        plot(GCDmap[[1]],bg="transparent",col="white",axes=sheet$demo$call$axes,xaxs="i",yaxs="i",xlim=xlim,ylim=ylim,xlab=sheet$demo$call$xlab,ylab=sheet$demo$call$ylab,cex.lab=sheet$demo$call$cex.lab,cex.main=sheet$demo$call$cex.main,cex.sub=sheet$demo$call$cex.sub,cex.axis=sheet$demo$call$cex.axis,add=FALSE,new=FALSE,lty="blank")
                        xlim<-round(par("usr")[1:2],2)
                        sheet$demo$call$xlim<<-xlim
                        ylim<-round(par("usr")[3:4],2)
                        sheet$demo$call$ylim<<-ylim
                        
                        pp$draw(x,y,cex=cex, # Arc View maps
                        pch=pch,col=col,
                        xaxs=sheet$demo$call$xaxs,
                        yaxs=sheet$demo$call$yaxs,
                        main=annotate(sheet$demo$call$main),
                        col.main=sheet$demo$call$col.main,
                        col.sub=sheet$demo$call$col.sub,
                        sub=sheet$demo$call$sub,
                        xlab=sheet$demo$call$xlab,
                        ylab=sheet$demo$call$ylab,
                        xlim=xlim,ylim=ylim,
                        axes=sheet$demo$call$axes,
                        cex.lab=sheet$demo$call$cex.lab,
                        cex.main=sheet$demo$call$cex.main,
                        cex.sub=sheet$demo$call$cex.sub,
                        cex.axis=sheet$demo$call$cex.axis,
                        new=FALSE)
                    }else{
                         if(ee=="PTmap "){ # PT maps (Perplex and suchlike)
                            pp$draw(x,axes=FALSE,frame.plot=FALSE,cex=cex,
                            pch=pch,col=col, 
                            xaxs=sheet$demo$call$xaxs,
                            yaxs=sheet$demo$call$yaxs,
                            main=annotate(sheet$demo$call$main),
                            col.main=sheet$demo$call$col.main,
                            col.sub=sheet$demo$call$col.sub,
                            sub=sheet$demo$call$sub,
                            xlab=sheet$demo$call$xlab,
                            ylab=sheet$demo$call$ylab,
                            #xlim=xlim,ylim=ylim,
                            cex.lab=sheet$demo$call$cex.lab,
                            cex.main=sheet$demo$call$cex.main,
                            cex.sub=sheet$demo$call$cex.sub,
                            cex.axis=sheet$demo$call$cex.axis,
                            bg=sheet$demo$call$bg,new=FALSE)
                
                        }else{         # Binary plots and all the rest
                            sheet$demo$call$new<<-FALSE
                            pp<<-figaro(demo,prefix="sheet")
                            pp$draw(x,y,cex=cex,
                            pch=pch,col=col, 
                            xaxs=sheet$demo$call$xaxs,
                            yaxs=sheet$demo$call$yaxs,
                            main=annotate(sheet$demo$call$main),
                            col.main=sheet$demo$call$col.main,
                            col.sub=sheet$demo$call$col.sub,
                            sub=sheet$demo$call$sub,
                            xlab=sheet$demo$call$xlab,
                            ylab=sheet$demo$call$ylab,
                            xlim=xlim,ylim=ylim,
                            cex.lab=sheet$demo$call$cex.lab,
                            cex.main=sheet$demo$call$cex.main,
                            cex.sub=sheet$demo$call$cex.sub,
                            cex.axis=sheet$demo$call$cex.axis,
                            bg=sheet$demo$call$bg,new=FALSE)
                        }
                    }
                }       
        }
        assign("sheet",sheet.bak,.GlobalEnv)
        #sheet<<-sheet.bak
        #if(scr){
        #    screen(scr,new=FALSE)
        #}
        
         if(getOption("gcd.ident")){
                if(!getOption("gcd.ident.each")){
                    IDlabels<-getOption("gcd.ident")
                    x<-ID(x.data,y.data,IDlabels)
                }
            }
        invisible()
}

# Removes formatting from an expression (inverse of annotate)
.fig.deeval<-function(def){
    if(length(def)==0) def<-""
    if(!is.expression(def))return(def)
    def<-as.character(def)
    def<-gsub("(epsilon\\[[0-9]{3}\\].)([a-zA-Z]*)","Eps\\2i",def) # Translate Eps[340]Nd etc to Eps[i]Nd NEW
    def<-gsub("(\" \"\\^)([0-9]{2,3})( \\* )([A-Z][a-z])","\\2\\4",def) # isotopic ratios NEW
    def<-gsub("([a-zA-Z0-9/])(\\[)([0-9]{3,})(\\])","\\1i",def) 
    #def<-gsub("(\" \"\\^)(\\d*)( \\* )()","\\2",def,perl=TRUE) # isotopic ratios AULD
    def<-gsub("([[])","",def)       # Left square bracket
    def<-gsub("([\"])","",def)      # Quotation mark
    #def<-gsub(" ~","",def)         # Tilde
    def<-gsub("[%][*][%]","@",def)  # %*% replace by @
    def<-gsub("(^[-0-9. ]{1,})[*]([ a-zA-Z])","\\1@\\2",def)  # ^number*text replace by @
    def<-gsub(" * ","",def)         # Remove the rest of asterisks
    def<-gsub(" * ","",def)
    def<-gsub("([]*])","",def)
    def<-gsub("~~"," ",def)
    def<-gsub("([~])"," ",def)
    def<-gsub("@","*",def)
    def<-gsub("^mg$","mg#",def)
    def<-gsub("^Mg$","Mg#",def)
    
    return(def)
}

# Fundamental function to modify Figaro templates; it looks for specified parameters (param)
# in the objects of the given type (what) and assigns them a new value (value)

figReplace<-function(what="lines",param="col",value=1,where="sheet$demo$template",exact=TRUE){
      
        if(!is.null(what)){
            ee<-grep(what,names(eval(parse(text=where))))
            for(i in ee){
                if(is.character(value)){
                    eval(parse(text=paste(where,"[[",i,"]][\"",param,"\"]<<-\"",value,"\"",sep="")))
                }else{
                    eval(parse(text=paste(where,"[[",i,"]][\"",param,"\"]<<-",value,sep="")))
                }
            }
            bak<-sheet$demo$call
            pp<<-figaro(demo,prefix="sheet")
            sheet$demo$call<-bak
        }else{
            if(exact){
                ee<-grep(paste("^",param,"$",sep=""),names(eval(parse(text=where))))#,extended=TRUE)
            }else{
                ee<-grep(param,names(eval(parse(text=where))))#,extended=TRUE)   
            }
            for(i in ee){
                if(is.character(value)){
                    eval(parse(text=paste(where,"[[",i,"]]<<-\"",value,"\"",sep="")))
                }else{
                    eval(parse(text=paste(where,"[[",i,"]]<<-",value,sep="")))
                }
            }
        }
}

figSelectSamples<-function(pattern=NULL){
    if(!is.null(pattern)){GUI<-FALSE}else{GUI<-TRUE}
    ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    
    if(ee=="spider "){
        xx<-selectSubset(pattern,where=cbind(labels[rownames(y.data),],WR[rownames(y.data),]),save=FALSE, multiple=TRUE,GUI=GUI)
    }else{
        xx<-selectSubset(pattern,where=cbind(labels[names(y.data),],WR[names(y.data),]),save=FALSE,multiple=TRUE,GUI=GUI)
    }
    
    if(is.null(xx)){cat("Cancelled.\n");options("gcd.ident"=0);return()}
    if(!getOption("gcd.shut.up")){cat("Selected samples:","\n");print(xx)}
    flush.console()
    which<-unique(xx)

    if(ee=="spider "){
        which<-match(which,rownames(y.data))
        #which<-which[!is.na(which)]
        .spiderPlotRedraw(which=which)
     }else{
        figRedraw(x.data[which],y.data[which])
     }
     mtext(side=1,line=2,annotate(selected))
     if(is.null(pattern)){  # Called from within GUI
        .figOk()
        figRedraw()
     }
}

.figOk<-function(){ 
    if(screen()) {locator(1);return()}
    x <- grconvertX(0.92, "ndc", "user")
    y <- grconvertY(0.995, "ndc", "user")
    legend(x,y,"OK",box.lwd=3,bg="khaki",xpd=TRUE,xjust=0,yjust=1,adj=0.5,cex=0.7)
    bringToTop(which = dev.cur(),stay=TRUE)
    
    .mousedownOK <- function(buttons, x, y) {
        plx <- grconvertX(x, "ndc", "user")
        ply <- grconvertY(y, "ndc", "user")    
        if (x > 0.9 && y > 0.9) "Done" else NULL
    }
    
    .keybdOK<-function(key){
        return(NULL)
    } 
    
    .onMouseMoveOK <- function(buttons, x, y) {
        bringToTop(which = dev.cur(),stay=FALSE)
        return(NULL)
    }
    
    setGraphicsEventEnv(which = dev.cur(), environment())
    setGraphicsEventHandlers(prompt="Click in upper right corner to quit", onMouseDown = .mousedownOK, onKeybd = .keybdOK, onMouseMove = .onMouseMoveOK, onMouseUp = NULL)
    #eventEnv <- getGraphicsEventEnv()
    getGraphicsEvent()
}

figRemove<-function(what="text",where="sheet$demo$template"){
            ee<-grep(what,names(eval(parse(text=where))))
            eval(parse(text=paste(where,"<<-",where,"[!(1:length(",where,"))%in%ee]",sep="")))
            #if(redraw){
                pp<<-figaro(demo,prefix="sheet")
                figRedraw()
            #}
}

.figCol.tmp<-function(what="lines",col="gray",where="sheet$demo$template"){
        x<-grep(what,names(eval(parse(text=where))))
        for(i in x){
            eval(parse(text=(paste("param<-(sheet$demo$template[[",i,"]])",sep=""))))
            eval(parse(text=(paste(what,"(param$x,param$y,col=\"",col,"\")",sep=""))))
        }

}

pickColour<-function(prompt="Enter plotting colour \n(or press Enter to pick from a list)",default=""){
    on.exit(options("show.error.messages"=TRUE))
    x<-winDialogString(prompt,default)
    if(is.null(x)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    if(x=="")x<-select.list(colours(),title="Select colour")
    if(x==""){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    if(is.na(as.numeric(x))){
        if(!any(colors()==x)){
                winDialog(type="ok","Invalid colour specification!")
                options(show.error.messages=FALSE)
                stop("",call. = FALSE)
        }
    }else{
        x<-as.numeric(x)
        if(x<0|x>656){
             winDialog(type="ok","Invalid colour specification!")
             options(show.error.messages=FALSE)
             stop("",call. = FALSE)
        }
    }
    return(x)
}

# Menus
figEdit<-function(){
    on.exit(options("show.error.messages"=TRUE))
    ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    menus<-c("title","subtitle","x label", "y label")
    if(ee=="ternary ") menus<-c("title","subtitle","a label (left)", "b label (top)", "c label (right)")
    if(ee=="spider ") menus<-c("title","subtitle","y label")

    what<-select.list(menus,title="Edit")
    if(what==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop(call. = TRUE)}
    if(ee=="ternary "){
        i<-switch(which(menus==what),"figMain()","figSub()","figAlab()","figBlab()","figClab()")
    }
    
    if(ee=="spider "){    
        i<-switch(which(menus==what),"figMain()","figSub()","figYlab()")
    }
    
    if(ee!="spider " & ee!="ternary "){    
        i<-switch(which(menus==what),"figMain()","figSub()","figXlab()","figYlab()")
    }
    cat("GCDkit->",i,"\n")
    .save2hist(i)
    eval(parse(text=i))
}


figColours<-function(){
    on.exit(options("show.error.messages"=TRUE))
    menus<-c("symbols","title","subtitle","set to B&W")
    what<-select.list(menus,title="Colours")
     if(what==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop(call. = TRUE)}
    ee<-switch(which(menus==what),"figCol()","figColMain()","figColSub()","figBw()")
    cat("GCDkit->",ee,"\n")
    .save2hist(ee)
    eval(parse(text=ee))
}

figScale<-function(){
    on.exit(options("show.error.messages"=TRUE))
    menus<-c("symbols","axis labels","title","subtitle")
    what<-select.list(menus,title="Text/symbols size")
    if(what==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop(call. = TRUE)}
    ee<-switch(which(menus==what),"figCex()","figCexLab()","figCexMain()","figCexSub()")
    cat("GCDkit->",ee,"\n")
    .save2hist(ee)
    eval(parse(text=ee))
}

figAdd<-function(){
    on.exit(options("show.error.messages"=TRUE))
    ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    menus<-c("ticks","grid lines","legend","reservoirs/minerals","text","arrow","box","linear fit","curve","contours by groups","convex hulls by groups")
    if(ee=="ternary ") menus<-c("legend","reservoirs/minerals","text","arrow","box") 
    if(ee=="spider " | ee=="profile ") menus<-c("text","arrow","box","reservoirs") 
  
    what<-select.list(menus,title="Add (temporarilly)")
    if(what==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop(call. = TRUE)}
        
    if(ee!="spider " & ee!="ternary " & ee!="profile "){ # Binary and suchalike
        i<-switch(which(menus==what),"figTicks()","figGrid(GUI=TRUE)","figLegend()","figAddReservoirs()","figAddText()","figAddArrow()","figAddBox()","figAddFit()","figAddCurve()","contourGroups()","chullGroups()")
    }else{ 
        if(ee=="spider " |  ee=="profile "){
            i<-switch(which(menus==what),"figAddText()","figAddArrow()","figAddBox()","figAddReservoirs()")
        }else{ # ternary
            i<-switch(which(menus==what),"figLegend()","figAddReservoirs()","figAddText()","figAddArrow()","figAddBox()","contourGroups()","chullGroups()")
        }
    }
    cat("GCDkit->",i,"\n")
    .save2hist(i)
    eval(parse(text=i))
}

figZooming<-function(){
    on.exit(options("show.error.messages"=TRUE))
    ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    if(ee=="ternary ") menus<-c("... in") else menus<-c("... in","Scale x axis","Scale y axis","Autoscale both axes")
    what<-select.list(menus,title="Zoom")
    if(what==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop(call. = TRUE)}
    if(ee=="ternary "){
        i<-switch(which(menus==what),"figZoom()")
    }else{
        i<-switch(which(menus==what),"figZoom()","figXlim()","figYlim()","figFixLim()")
    }
    cat("GCDkit->",i,"\n")
    .save2hist(i)
    eval(parse(text=i))
}

# Menu Edit
figMain<-function(txt=NULL){
    if(is.null(txt)){
        txt<-.figCharacter(txt,default=.fig.deeval(sheet$demo$call$main),message="Enter main title for the graph")
    }
    sheet$demo$call$main<<-annotate(txt)
    figRedraw()
}

figSub<-function(txt=NULL){
    if(is.null(txt)){
        txt<-.figCharacter(txt,default=.fig.deeval(sheet$demo$call$sub),message="Enter sub title for the graph")
    }
    sheet$demo$call$sub<<-annotate(txt)
    figRedraw()
}

figXlab<-function(txt=NULL){
    if(is.null(txt)){
        txt<-.figCharacter(txt,default=.fig.deeval(sheet$demo$call$xlab),message="Enter label for the x axis")
    }  
    sheet$demo$call$xlab<<-annotate(txt)
    figRedraw()
}

figYlab<-function(txt=NULL){
    if(is.null(txt)){
        txt<-.figCharacter(txt,default=.fig.deeval(sheet$demo$call$ylab),message="Enter label for the y axis")
    }
    sheet$demo$call$ylab<<-annotate(txt)
    figRedraw()
}

figAlab<-function(txt=NULL){
    if(is.null(txt)){
        txt<-.figCharacter(txt,default=.fig.deeval(sheet$demo$template$A$text),message="Enter label for a bottom left apex")
    }
    sheet$demo$template$A$text<<-annotate(txt)
    ee<-sheet$demo$call
    pp<<-figaro(demo,prefix="sheet")
    sheet$demo$call<-ee
    figRedraw()
}

figBlab<-function(txt=NULL){
    if(is.null(txt)){
        txt<-.figCharacter(txt,default=.fig.deeval(sheet$demo$template$B$text),message="Enter label for a top apex")
    }
    sheet$demo$template$B$text<<-annotate(txt)
    ee<-sheet$demo$call
    pp<<-figaro(demo,prefix="sheet")
    sheet$demo$call<-ee
    figRedraw()
}

figClab<-function(txt=NULL){
    if(is.null(txt)){
        txt<-.figCharacter(txt,default=.fig.deeval(sheet$demo$template$C$text),message="Enter label for a bottom right apex")
    }
    sheet$demo$template$C$text<<-annotate(txt)
    ee<-sheet$demo$call
    pp<<-figaro(demo,prefix="sheet")
    sheet$demo$call<-ee
    figRedraw()
}


    
# Menu Zooming
figXlim<-function(range=NULL){
    on.exit(options("show.error.messages"=TRUE))
    scr<-screen()
    #if(scr)screen(scr.old,new=FALSE) #NEWLY COMMENTED OFF

    if(is.null(range)){
        xrange<-winDialogString("Enter range for the x axis",paste(sheet$demo$call$xlim,collapse=","))
        if(is.null(xrange)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        x<-as.numeric(unlist(strsplit(xrange,",")[[1]]))
    }else{
        x<-range
    }
    if(length(x)!=2|any(is.na(x))){
        options("show.error.messages" = FALSE)
        winDialog("ok","Invalid range!")
        cat(xrange,"not found.\n")
        stop(call. = TRUE)
    }

    ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    
    if(ee=="ternary ") return() # Not applicable to ternary plots
    
    if(ee=="spider "){
        x<-as.integer(x)
        if(any(x<1) | any(x> length(sheet$demo$template$axis1$at))){
            options("show.error.messages" = FALSE)
            winDialog("ok","Out of range!")
            stop(call. = TRUE)
        }
    }
    
    if(any(names(sheet$demo$call)=="log") & any(x<0) & length(grep("x",sheet$demo$call$log)!=0)){
        options("show.error.messages" = FALSE)
        winDialog("ok","Negative values not alowed for log axes!")
        stop(call. = TRUE)
    }
    sheet$demo$call$xlim<<-x #sort(x)
    ee<-sheet$demo$call
    pp<<-figaro(demo,prefix="sheet")
    sheet$demo$call<-ee
   
    if(scr){
        .saveCurPlotDef(screen())
        screen(scr,new=FALSE)
    }
    figRedraw()
}


figYlim<-function(range=NULL){
    on.exit(options("show.error.messages"=TRUE))
    scr<-screen()
    #if(scr)screen(scr.old,new=FALSE) #NEWLY COMMENTED OFF
    if(is.null(range)){
        yrange<-winDialogString("Enter range for the y axis",paste(sheet$demo$call$ylim,collapse=","))
        if(is.null(yrange)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        x<-as.numeric(unlist(strsplit(yrange,",")[[1]]))
    }else{
        x<-range
    }
    
    if(length(x)!=2|any(is.na(x))){
        options("show.error.messages" = FALSE)
        winDialog("ok","Invalid range!")
        stop(call. = TRUE)
    }

   if(any(names(sheet$demo$call)=="log") & any(x<0) & length(grep("y",sheet$demo$call$log)!=0)){
        options("show.error.messages" = FALSE)
        winDialog("ok","Negative values not alowed for log axes!")
        cat(xrange,"not found.\n")
        stop(call. = TRUE)
    }
    sheet$demo$call$ylim<<-x #sort(x)
    ee<-sheet$demo$call
    pp<<-figaro(demo,prefix="sheet")
    sheet$demo$call<-ee
    if(scr){
       .saveCurPlotDef(screen())
       screen(scr,new=FALSE)
    }
    figRedraw()
}

# Autoscale if some analyses fall outside the plot limits
figFixLim<-function(no.action.warn=TRUE){
    on.exit(options("show.error.messages"=TRUE))
    
    if(length(grep("x",sheet$demo$call$log))==0){
        ee.x<-x.data[!is.na(x.data)&x.data!="Inf"]  
    }else{
        ee.x<-x.data[!is.na(x.data)&x.data!="Inf"&x.data>0]   # Logarithmic >0 
    }
    
    out<-which(ee.x<min(sheet$demo$call$xlim)|ee.x>max(sheet$demo$call$xlim))
    
    if(length(grep("x",sheet$demo$call$log))==0){
        ee.y<-y.data[!is.na(y.data)&y.data!="Inf"]    
    }else{
        ee.y<-y.data[!is.na(y.data)&y.data!="Inf"&y.data>0] # Logarithmic >0 
    }
    
    out<-c(out,which(ee.y<min(sheet$demo$call$ylim)|ee.y>max(sheet$demo$call$ylim)))
    out<-unique(out)
    
    if(length(out)==0&!no.action.warn){options("show.error.messages" = FALSE);stop(call. = FALSE) }
    
    if(length(out)==0&no.action.warn){
        options("show.error.messages" = FALSE)
        if(.fig.deeval(sheet$demo$call$main)!="NULL") msg<-paste(.fig.deeval(sheet$demo$call$main),":\n",sep="") else msg<-""
        msg<-paste(msg,"No analyses plot outside the limits... Nothing to do.",sep="")
        
        if(getOption("gcd.menus")!=""){
            x<-winDialog(msg,type="ok") 
        }else{
            cat(msg,"\n")
        }
        stop(call. = FALSE) 
    }

    xx1<-paste(round(sheet$demo$call$xlim,5),collapse=" to ")
    yy1<-paste(round(sheet$demo$call$ylim,5),collapse=" to ")
    cat("Actual plot ranges for x-axis: [", xx1,"] and for y-axis: [",yy1,"]\n",sep="")
        
    xx<-round(c(sheet$demo$call$xlim,range(ee.x,na.rm=TRUE)),5)
    xx<-range(xx,na.rm=TRUE)
    
    yy<-round(c(sheet$demo$call$ylim,range(ee.y,na.rm=TRUE)),5)
    yy<-range(yy,na.rm=TRUE)
    
    cat("Actual data ranges for x-axis: [", paste(xx,collapse=" to "),"] and for y-axis: [",paste(yy,collapse=" to "),"]\n",sep="") 
    flush.console()
    
    if(getOption("gcd.menus")!="") x<-winDialog("Fix coordinates?",type="yesno") else x<-"YES"
    if(x=="YES"){
        
        # Check also for logarithmic axes
        if(any(is.na(match(round(sheet$demo$call$xlim,5),xx)))){
            if(length(grep("x",sheet$demo$call$log))==0){
                xrange<-range(pretty(xx))
            }else{
                xrange<-10^(c(.round.min.down(log10(xx),1),.round.max.up(log10(xx),1)))
            }
            xrange<-round(xrange,3)
            figXlim(xrange)
        }else{
             xrange<-range(xx)
        }
        
        if(any(is.na(match(round(sheet$demo$call$ylim,5),yy)))){
            if(length(grep("y",sheet$demo$call$log))==0){
                yrange<-range(pretty(yy))
            }else{
                yrange<-10^(c(.round.min.down(log10(yy),1),.round.max.up(log10(yy),1))) 
            }
            yrange<-round(yrange,3)
            figYlim(yrange)
        }else{
             yrange<-range(yy)
        }
        
        cat("Fixed plot limits for x-axis:  [", paste(xrange,collapse=" to "),"] and for y-axis: [",paste(yrange,collapse=" to "),"]\n",sep="") 
    }
    invisible()   
}

# Menu Colour
figCol<-function(col=NULL){
    if(is.null(col)) col<-pickColour()
    ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    if(ee=="spider " | ee=="profile "){
        figReplace("points","col",col,exact=FALSE)
        figReplace("lines","col",col,exact=FALSE)
        figRedraw()
     }else{
        figRedraw()
        figUser(paste("col=\"",col,"\"",sep=""))
     }
}

figColMain<-function(col=NULL){
    if(is.null(col)) col<-pickColour("Enter main title colour \n(or press Enter to pick from a list)")
    sheet$demo$call$col.main<<-col
    figRedraw()
}


figColSub<-function(col=NULL){
    if(is.null(col)) col<-pickColour("Enter sub title colour \n(or press Enter to pick from a list)")
    #ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    #if(ee=="spider " | ee=="profile "){
        sheet$demo$call$col.sub<<-col
        figRedraw()
    #}else{
    #    pp$set(col.sub=col)
    #}
}


figBw<-function(){
     figColMain("black")
     figColSub("black")
     figReplace("text","col","black",exact=FALSE)
     figReplace("points","col","black",exact=FALSE)
     figReplace("lines","col","black",exact=FALSE)
     figReplace("polygon","col","gray",exact=FALSE)
     figReplace("reservoirs","col","black",exact=FALSE)
     
     ee<-sheet$demo$call
     pp<<-figaro(demo,prefix="sheet")
     sheet$demo$call<-ee
     if(!is.null(sheet$demo$legend)){
        sheet$demo$legend$col<<-rep("black",times=length(sheet$demo$legend$col))
     }
     
     if(any(names(sheet$demo$template)=="field")){
        sheet$demo$template$field$col<<-"gray"
        sheet$demo$template$field$border<<-"gray"
     }
    
     figRedraw()
     ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
     if(ee!="spider " & ee!="profile "){
        figUser("col=\"black\"")
    }
}

# Menu Text/Symbols size
.figNumber<-function(x,default=1,message="Scaling for symbols"){
    on.exit(options("show.error.messages"=TRUE))
    default<-as.character(default)
    if(is.null(x)) x<-"a"
    if(is.na(as.numeric(x))){
        repeat{
            x<-winDialogString("Scaling for sub title",default)
            if(is.null(x)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
            x<-as.numeric(x)
            if(is.na(x)){
                winDialog(type="ok","Invalid size specification!")
            }else{
                break
            }
        }
    } 
    return(x)
}

.figCharacter<-function(x,default="",message="Scaling for symbols"){
    on.exit(options("show.error.messages"=TRUE))
    x<-winDialogString("Enter sub title for the graph",default)
    if(is.null(x)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    return(x)
}

figCex<-function(x=NULL){
    on.exit(options("show.error.messages"=TRUE))
    if(is.null(x)){
        ee<-sheet$demo$call$cex[1]
        if(is.null(ee)) ee<-"1" else ee<-as.character(ee)
        x<-winDialogString("Scaling for symbols",ee)
        if(is.null(x)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        if(x=="NA") x<-NA
        if(!is.na(x)&is.na(as.numeric(x))){
                winDialog(type="ok","Invalid size specification!")
                options("show.error.messages"=FALSE)
                stop("",call. = FALSE)
        }
    }
    if(!is.na(x)){
        x<-as.numeric(x)
        if(is.na(x))return()
    }
    ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    if(ee!="spider " & ee!="profile "){
        sheet$demo$call$cex<-rep(x,length(x.data))
        #sheet$demo$call$cex<-rep(x,length(sheet$demo$call$cex))
    }else{
        figReplace("points","cex",x)
    }
    ee<-sheet$demo$call
    assign("sheet",sheet,.GlobalEnv)
    pp<<-figaro(demo,prefix="sheet")
    sheet$demo$call<-ee
    figRedraw()
}


figCexLab<-function(x=NULL){
    on.exit(options("show.error.messages"=TRUE))
    ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    if(is.null(x)){    
        if(ee=="ternary "){
            x<-winDialogString("Scaling for axis labels",as.character(.fig.deeval(sheet$demo$template$A$size)))
        }else{
            x<-winDialogString("Scaling for axis labels",paste("",sheet$demo$call$cex.lab,sep=""))
        }
        if(is.null(x)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    }

    if(is.na(as.numeric(x))){
                winDialog(type="ok","Invalid size specification!")
                options("show.error.messages"=FALSE)
                stop("",call. = FALSE)
    }
    x<-as.numeric(x)
    if(is.na(x))return()
   
    if(ee!="spider " & ee!="profile "){
        sheet$demo$call$cex.lab<<-x
        sheet$demo$call$cex.axis<<-x
        if(ee=="ternary "){
            sheet$demo$template$A$cex<<-x
            sheet$demo$template$B$cex<<-x
            sheet$demo$template$C$cex<<-x
        }
        #figRedraw()
    }else{
        sheet$demo$call$cex.lab<<-x*1.2
        figReplace(what="axis",param="cex.axis",value=x)
        figReplace(what="mtext",param="cex",value=x)
    }
    
    ee<-sheet$demo$call
    pp<<-figaro(demo,prefix="sheet")
    sheet$demo$call<-ee
    figRedraw()
}


figCexMain<-function(x=NULL){
    x<-.figNumber(x,paste("",sheet$demo$call$cex.main,sep=""),"Scaling for main title")
    sheet$demo$call$cex.main<<-x
    figRedraw()
}

figCexSub<-function(x=NULL){
    x<-.figNumber(x,paste("",sheet$demo$call$cex.sub,sep=""),"Scaling for sub title")
    sheet$demo$call$cex.sub<<-x
    figRedraw()
}

# Menu Add
figLegend<-function(){
    showLegend(new.plot=FALSE)
}

figTicks<-function(major=-0.5, minor=0.25,xmjr=NULL,xmin=NULL,ymjr=NULL,ymin=NULL){
    on.exit(options("show.error.messages"=TRUE))
    if(is.null(xmjr)){
        ee<-winDialogString("Enter intervals of major/minor ticks for x and y axes, separated by commas, e.g. \"xmjr, xmin, ymjr, ymin\"","")
        if(is.null(ee)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        x<-strsplit(ee,",")[[1]]
        if(any(is.na(as.numeric(x))|length(x)!=4)|length(x)==0){
                winDialog(type="ok","Invalid ticks specification!")
                options("show.error.messages"=FALSE)
                stop("",call. = FALSE)
        }
    }else{
        x<-c(xmjr,xmin,ymjr,ymin)
    }
    
    x<-as.numeric(x)
    if(length(grep("x", sheet$demo$call$log))==0){
        axis(1,at=seq(0,par("usr")[2],x[1]),labels=FALSE,tcl=major)
        axis(1,at=seq(0,par("usr")[2],x[2]),labels=FALSE,tcl=minor)
    }
    if(length(grep("y", sheet$demo$call$log))==0){   
        axis(2,at=seq(0,par("usr")[4],x[3]),labels=FALSE,tcl=major)   
        axis(2,at=seq(0,par("usr")[4],x[4]),labels=FALSE,tcl=minor)
    }
    
    # Spider
    # axis(2,at= axTicks(2, axp = c(10^par("usr")[4],10^par("usr")[3],3), usr = NULL, log = TRUE),labels=FALSE,tcl=minor)
}

figGrid<-function(x.int=NULL,y.int=NULL,lty="dotted",col="gray30",GUI=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    if(GUI){
        if(is.null(x.int)) x.int<-winDialogString("Enter interval of grid lines for x axis or ENTER for none","")
        if(is.null(x.int)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        if(is.na(as.numeric(x.int))&x.int!=""){
                winDialog(type="ok","Invalid ticks specification!")
                options("show.error.messages"=FALSE)
                stop("",call. = FALSE)
        }
    }else{
        if(is.null(x.int)) x.int<-""
    }
    if(x.int!=""){
        x.int<-as.numeric(x.int)
        x.int<-seq(par("usr")[1],par("usr")[2],x.int)
        x.int<-x.int[-c(1,length(x.int))]
        axis(1,at=x.int,labels=FALSE,tck=1,lty=lty,col=col)
    }
    
    if(GUI){ 
        if(is.null(y.int)) y.int<-winDialogString("Enter interval of grid lines for y axis or ENTER for none","")
        if(is.null(y.int)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
        if(is.na(as.numeric(y.int))&y.int!=""){
                winDialog(type="ok","Invalid ticks specification!")
                options("show.error.messages"=FALSE)
                stop("",call. = FALSE)
        }
    }else{
        if(is.null(y.int)) y.int<-""
    }
    if(y.int!=""){
        y.int<-as.numeric(y.int)
        y.int<-seq(par("usr")[3],par("usr")[4],y.int)
        y.int<-y.int[-c(1,length(y.int))]
        axis(2,at=y.int,labels=FALSE,tck=1,lty=lty,col=col)
    }
}


figUser<-function(expression=NULL,redraw=TRUE){
    on.exit(options("show.error.messages"=TRUE))
    if(is.null(expression)){
        GUI<-TRUE
        expression<-winDialogString("Enter expression eg. bg=\"khaki\"; col=\"darkblue\"","")
        if(is.null(expression)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    }else{
        GUI<-FALSE
    }
    
    scr<-screen()
    if(scr){
        .eraseScreen()
        screen(scr,new=FALSE)
    }
    
    if(nchar(expression)<=1){
        variables<-list()
        where[1]<-"sheet$demo$call"
        variables$call<-names(eval(parse(text=where[1])))

        where[2]<-"sheet$demo$template"
        variables$template<-names(eval(parse(text=where[2])))
        names(where)<-c("call","template")

        expression<-c(paste(rep("call",length(variables$call)),variables$call,sep=": "),"-----",paste(rep("template",length(variables$template)),variables$template,sep=": "))
        expression<-unique(gsub("[0-9]","",expression))

        what<-expression[expression!="template: GCDkit" & expression!="template: clssf"]
        what<-select.list(what)
        if(what==""){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}

        x<-strsplit(what,": ")[[1]]
        variables<-unlist(variables[x[1]])
        what<-x[2]
        where<-where[x[1]]
        if(x[1]=="template"){
            expression<-variables[grep(what,variables)]
            res<-""
            for(j in length(expression)){
                res<-c(res,names(eval(parse(text=paste(where,"$",expression[j],sep="")))))
            }
            res<-unique(res)
            res<-res[nchar(res)>0]
            res<-res[res!="x"&res!="y"]
            param<-select.list(res)
            default=""
        }else{
            param<-what
            what<-NULL
            default=as.character(eval(parse(text=paste(where,param,sep="$"))))
        }
        if(length(grep(param,c("col","bg","fg")))>0){
            value<-pickColour()
        }else{
            options("show.error.messages"=FALSE)
            value<-try(winDialogString("Enter the new value",default))
            if(class(value)=="try-error") {print(default);value<-winDialogString("Enter the new value","")}
            
            i<-try(expression<-eval(parse(text=value)))
            if(class(i)!="try-error") value<-parse(text=value)
        }      
        if(param=="col" & is.null(what)) pp$set(col=value) else figReplace(what=what,param=param,value=value,where=where,exact=TRUE)
        figRedraw()
        return()
    }
    
    x<-as.list(eval(paste(text=unlist(strsplit(expression,";")[[1]]))))      
    options("show.error.messages"=TRUE)
    
    if(substr(x,2,4)!="lim"){
        out<-try(eval(parse(text=paste("pp$set(",text=x,")",sep=""))))
    }else{
        prd<-(substr(x,1,4))
        sheet$demo$call[[prd]]<<-eval(parse(text=substr(x,6,nchar(x))))
        pp<<-figaro(demo,prefix="sheet")
        if(redraw) out<-figRedraw()
    }
    if(class(out)=="try-error"){
        winDialog("ok",paste("Syntax error! ", x,"not found."))
        #cat(as.character(x),"not found.\n")
        stop(call. = TRUE)
    }
    
    #pp<-get("pp",envir=.GlobalEnv) #NEW
    call<-pp$return()$call
    
    ee<-names(call)
    pars<-call[ee[nchar(ee)>0]]
    
    if(length(pars$pch)==1) pars$pch<-rep(call$pch,length(call[[1]]))
    if(length(pars$col)==1) pars$col<-rep(call$col,length(call[[1]]))
    if(length(pars$cex)==1) pars$cex<-rep(call$cex,length(call[[1]]))
    
    sheet$demo$call<<-pars 
    
    if(scr){ # NEW
        .saveCurPlotDef(screen())
        screen(scr,new=FALSE)
    }
   
    assign("sheet",sheet,.GlobalEnv)
    assign("pp",pp,.GlobalEnv)
}


figZoom<-function(){
    cat("Click bottom left and top right corners of the area to be zoomed\n\n")
    flush.console()
    z<-pp$zoom()
    #dev.set(dev.prev())
    #new<-par("usr")
    #rect(z$x[1],z$y[1],z$x[2],z$y[2],density=5,angle=45,col="darkkhaki")
    #cat(z$x[1],z$y[1],z$x[2],z$y[2],"\n")
    #ee<-dev.set(dev.next())
         
    i<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    if(i=="ternary "){
        new<-par("usr")
        if(new[1]>0)lines(c(new[1],new[1]),c(0,sqrt(3)*new[1]),col="red",lty="dashed")
        if(new[3]>0)lines(c(new[3]/sqrt(3),1-new[3]/sqrt(3)),c(new[3],new[3]),col="red",lty="dashed")
        if(new[4]<sqrt(3)/2)lines(c(new[4]/sqrt(3),1-new[4]/sqrt(3)),c(new[4],new[4]),col="red",lty="dashed")
        if(new[2]<1)lines(c(new[2],new[2]),c(0,sqrt(3)*(1-new[2])),col="red",lty="dashed")
    }
    if(i=="spider "){
        #args<-sheet$demo$template[[2]]
        #axis(2,at=axTicks(2, axp = c(10^par("usr")[4],10^par("usr")[3],3), usr = NULL, log = TRUE),labels=FALSE,tcl=0.25,cex.axis=args$cex.axis*1.2)
        if(!is.null(sheet$demo$legend)){
                old.par <- par("mai")
                #par(mai=c(0.1,0.1,0.1,0.1))
                par(xpd=TRUE)
                args<-sheet$demo$legend
                legend(x=args$x,y=args$y,legend=args$legend,inset=args$inset,bty=1,col=args$col,lty="solid",bg="white",pch=args$pch,cex=args$cex)
                par(xpd=FALSE)
                par(mai=old.par)
        }
    }
    bringToTop(which = dev.cur())
    .figOk()
    figUnzoom()
}


figUnzoom<-function(){
    new<-dev.cur()
    z<-pp$unzoom()
    figRedraw()
    #ee<-dev.off(dev.prev())
    ee<-dev.set(new)
}

figIdentify<-function(){
    ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    x<-x.data
    y<-y.data
    if(ee=="profile "){
        x<-rep(x.data,nrow(y.data))
        y<-as.numeric(t(y.data))
    }
    if(ee!="spider "){
        ee<-identify(x,y,names(x),offset=0.4,col="gray30",cex=0.8)
    }else{
        x<-rep(x.data,nrow(y.data))
        y<-as.numeric(t(y.data))

        xlab<-sheet$demo$call$xlab
        ylab<-sheet$demo$call$ylab
        main<-sheet$demo$call$main
        ymin<-sheet$demo$call$ylim[1]
        ymax<-sheet$demo$call$ylim[2]
        xmax<-sheet$demo$call$xlim[2]
        labs<-filterOut(WR[rownames(y.data),],colnames(y.data))
        labs=as.numeric(t(labs))
        flush.console()
        figRedraw()

        repeat{
            ee<-identify(x,y,labs,plot=FALSE,n=1,tolerance=0.5)
            if(length(ee)==0)break
            figRedraw()
            #pp$draw(1,1,xlab=xlab,ylab=ylab,main=main)
            i<-floor(ee/ncol(y.data))+1
            which<-rownames(y.data)[i]
            j<-names(sheet$demo$template)
            what<- grep("lines",j,value=TRUE)[i]
            eval(parse(text=(paste("param<-(sheet$demo$template$",what,")",sep=""))))
            lines(param$x,param$y,col="red",lwd=3,pch=1,cex=2,type="b")
            text(xmax/2,ymax/2,which,cex=1.5,col=param$col)
            text(x[ee],y[ee],labs[ee],pos=4,ofset=0.5)
        }
        figRedraw()
    }
}

figAddText<-function(){
    on.exit(options("show.error.messages"=TRUE))
    x<-winDialogString("Enter text","")
    if(is.null(x)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    font<-winDialogString("Text style (n/b/i/bi)","n")
    if(is.null(font)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    font<-switch(font,n=1,b=2,i=3,bi=4,1)

    col<-pickColour("Text colour","black")

    cex<-as.numeric(winDialogString("Text magnification","1"))
    if(is.null(cex)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}

    if(is.na(as.numeric(cex))){
                winDialog(type="ok","Invalid size specification!")
                options("show.error.messages"=FALSE)
                stop("",call. = FALSE)
    }

    ee<-locator(n=1)
    text(ee,x,col=col,cex=cex,font=font)
}


figAddArrow<-function(){
    on.exit(options("show.error.messages"=TRUE))
    ee<-locator(n=2,type="l",col="pink",lty="dashed")
    col<-pickColour("Arrow colour","black")
    lty<-select.list(c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),title="Line type")
    if(lty==""){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    lines(ee$x,ee$y,col="white")
    arrows(ee$x[1],ee$y[1],ee$x[2],ee$y[2],col=col,lty=lty)
}


figAddBox<-function(){
    cat("Click bottom left and top right corners of the box\n\n")
    ee<-locator(n=2)
    col<-pickColour("Box colour","black")
    rect(ee$x[1],ee$y[1],ee$x[2],ee$y[2],border=col)
}

figAddCurve<-function(equation=NULL){
    if(is.null(equation)) GUI<-TRUE else GUI<-FALSE
    on.exit(options("show.error.messages"=TRUE))
    if(GUI){
        equation<-""
        equation<-winDialogString("Enter a formula, e.g. (x-5)^2+5",equation)
        if(is.null(equation)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    }
    selected<-try(parse(text=equation))
    if(class(selected)=="try-error"){
                winDialog(type="ok","Syntax error in formula!")
                stop("",call. = FALSE)
    }
    options("show.error.messages"=TRUE)
    equation<<-equation

    col<-"darkred"
    lty<-"solid"
    
    if(GUI){
        col<-pickColour("Curve colour",col)

        lty<-select.list(c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),title="Line type")
        if(lty==""){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    }
    options("show.error.messages"=FALSE)
    selected<-try(eval(parse(text=paste("curve(",as.expression(equation),",col=\"",col,"\",lty=\"",lty,"\",add=TRUE)",sep=""))))
    #selected<-try(eval(parse(text=paste("curve\(",as.expression(equation),",col=\"",col,"\",lty=\"",lty,"\",add=TRUE\)",sep=""))))
    if(class(selected)=="try-error"){
                winDialog(type="ok","Syntax error in formula!")
                stop("",call. = FALSE)
    }
    options("show.error.messages"=TRUE)
}

figAddFit<-function(){
    on.exit(options("show.error.messages"=TRUE))
    
    lty<-select.list(c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),title="Line type")
    if(lty==""){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    x<-winDialog("Fit lines by groups?",type="yesno")
    if(x=="YES"){
        out<-.figAddFit.groups(lty)
        return(out)
    }
    col<-pickColour("Line colour","darkred")
    fitted<-lsfit(x.data,y.data)
    abline(fitted,col=col,lty=lty)
    ee<-locator(n=1)
    text(ee,paste(round(fitted$coef[2],5)," * x + ",round(fitted$coef[1],5),sep=""))
    results<<-fitted$coef
    invisible(fitted)
}


.figAddFit.groups<-function(lty){
    gr<-factor(groups)
    out<-sapply(1:length(levels(gr)),function(i){
        cat(levels(gr)[i],"\n")
        flush.console()
        x<-x.data[gr==levels(gr)[i]]
        y<-y.data[gr==levels(gr)[i]]
        ee<-sum(!apply(is.na(cbind(x,y)),1,any))
        if(ee>1){
            fitted<-lsfit(x,y)
            abline(fitted,col=i,lty=lty)
            ee<-locator(n=1)
            text(ee,levels(gr)[i],col=i)
            z<-fitted$coeff
            prd<<-fitted
        }else{
            z<-NULL
        }
        return(z)
    },simplify=TRUE)
    colnames(out)<-levels(gr)
    results<<-out
    invisible(out)
}


.fig.spider.axes<-function(main="",sub="",offset=FALSE,centered=FALSE,xrotate=FALSE,xaxs=sheet$demo$call$xaxs,ylab=sheet$demo$call$ylab){  
    plot(1,1,type="n",main=main,sub=sub,cex.lab=sheet$demo$call$cex.lab,cex.main=sheet$demo$call$cex.main,cex.sub=sheet$demo$call$cex.sub,col.main=sheet$demo$call$col.main,col.sub=sheet$demo$call$col.sub,xlim=sheet$demo$call$xlim,axes=FALSE,xaxs=xaxs,yaxs=sheet$demo$call$yaxs,ylim=sheet$demo$call$ylim,xlab=sheet$demo$call$xlab,ylab=ylab,log=sheet$demo$call$log,bg=sheet$demo$call$bg)
    my<-sheet$demo$template
    ee<-grep("axis|rug|mtext|field|box",names(my))#,value=TRUE)
    lapply(ee,function(i){
        fun<-as.character(my[[i]][1])
        call<-my[[i]][-1]
        if(fun=="mtext"){
            if(call$side==1|call$side==3){
                i<-call$at>=sheet$demo$call$xlim[1]&call$at<=sheet$demo$call$xlim[2]
                call$at<-call$at[i]
                call$text<-call$text[i]
            }
            
            if(call$side==2|call$side==4){
                i<-call$at>=sheet$demo$call$ylim[1]&call$at<=sheet$demo$call$ylim[2]
                call$at<-call$at[i]
                call$text<-call$text[i]
            }
        } 
        do.call(fun,call)
    })
    #sheet$demo$template$mtext1$text[sheet$demo$call$xlim[1]:sheet$demo$call$xlim[2]]
    return()
}
    

.fig.spider.selected<-function(index,x,y,pch=NULL,main="",shaded.col=NULL,cex=NULL,offset=FALSE,xrotate=FALSE,centered=FALSE,xaxs=sheet$demo$call$xaxs,ylab=sheet$demo$call$ylab,col=NULL,...){
            on.exit(options("show.error.messages"=TRUE))
            # Setup the axes
            .fig.spider.axes(main=main,sub="",xaxs=xaxs)#,shaded.col=shaded.col)      
            
            # Plot behind field?
            ee<-("field"%in%names(sheet$d$t))
            if(ee){
                polygon(sheet$d$t$field[[2]],sheet$d$t$field[[3]],col=sheet$d$t$field$col,border=sheet$d$t$field$border)
            }
            
            # Plot individual patterns
            for(ii in index){
                    eval(parse(text=(paste("param<-(sheet$demo$template$lines",ii,")",sep=""))))
                    eval(parse(text=(paste("param2<-(sheet$demo$template$points",ii,")",sep=""))))
                    
                    if(is.null(pch)){
                        pch1<-param2$pch
                    }else{
                        pch1<-pch
                    }                    
                    
                    if(is.null(col)){
                        col1<-param$col
                    }else{
                        col1<-col
                    }
                    
                    if(is.null(cex)){
                        cex1<-param2$cex
                    }else{
                        cex1<-cex
                    }
                    lwd<-1
                    if(!is.null(cex)) {if(cex==0)lwd<-2} # No plotting symbols desired
                    lines(param$x,param$y,pch=pch1,col=col1,cex=cex1,type="o",lwd=lwd)
            }
            
            # Plot grid
            options(show.error.messages=FALSE)
                ee<-try(eval(parse(text="param<-sheet$demo$template$grid")))
                if(class(ee)!="try-error"){
                    abline(h=param$h,lty=param$lty,col=param$col)
                }
                ee<-try(eval(parse(text="param<-sheet$demo$template$grid1")))
                if(class(ee)!="try-error"){
                    abline(h=param$h,lty=param$lty,col=param$col)
                }
            options(show.error.messages=TRUE)
            
            # y axis
            axis(2,at= axTicks(2, axp = c(10^par("usr")[4],10^par("usr")[3],3), usr = NULL, log = TRUE),labels=FALSE,tcl=0.25)  
}

figMulti<-function(x=x.data,y=y.data,nrow=NULL,ncol=NULL,xlab=sheet$demo$call$xlab,ylab=sheet$demo$call$ylab,pch=NULL,col=NULL,cex=NULL,plot.symb=NULL,shaded.col="gray",rotate.xlab=TRUE,offset=TRUE,centered=FALSE,title=NULL,...){    
     on.exit(options("show.error.messages"=TRUE))
     sheet.tmp<-NULL
     figaroOff()
     try(.plateOff())
     x.data.bak<-x.data
     y.data.bak<-y.data
     
     # Plotting symbols    
     if(!is.null(pch)){         
              if(length(pch)!=nrow(labels)) pch<-rep(pch[1],nrow(labels))
              names(pch)<-rownames(labels)
     }
     
     #Plotting colours
     if(!is.null(col)){
              if(length(col)!=nrow(labels)) col<-rep(col[1],nrow(labels))
              names(col)<-rownames(labels)
    }
    
    #Plot symbols size
    #if(is.null(cex)){
    #    if(length(sheet$demo$call$cex)>0){
    #        cex<-sheet$demo$call$cex
    #    }else{
    #        if(is.vector(y.data)){
    #            browser()
    #            cex<-labels[names(y.data),"Size"]
    #            sheet$demo$call$cex<-cex[rownames(WR)%in%names(y.data)] 
    #            #names(cex)<-names(y.data)
    #        }else{
    #            cex<-labels[rownames(y.data),"Size"]
    #            sheet$demo$call$cex<-cex[rownames(WR)%in%rownames(y.data)] 
    #        }
    #        
    #        #sheet$demo$call$cex<-cex[rownames(WR)%in%names(x.data)]     
    #        #sheet$demo$call$cex<-cex #cex[rownames(WR)%in%names(x.data)]     
    #    }
    #}
    
    #Plot symbols size
    if(is.null(cex)){
        if(length(sheet$demo$call$cex)>0){
            cex<-sheet$demo$call$cex
        }else{
            #cex<-get("cex",.GlobalEnv)
            #cex<-labels[rownames(y.data),"Size"]
            cex<-labels[names(x.data),"Size"]
            sheet$demo$call$cex<-cex[rownames(WR)%in%names(x.data)]     
        }
    }
     
    if(length(cex)<nrow(labels)) cex<-rep(cex[1],nrow(labels))
    names(cex)<-rownames(labels)        
    
    # Setup the plate
    group<-factor(groups)
    delka<-length(levels(group))
    if(!is.null(sheet$demo$legend)){
        #if(all(sheet$demo$legend$pch==15)) #delka<-delka+1
    }
    
    # Check dimensions
    if(!is.null(nrow)|!is.null(nrow)){
        ee<-as.numeric(nrow)*as.numeric(ncol)
        if(length(ee)==0){
            winDialog(type="ok","Invalid dimensions for the plate!");
            nrow<-NULL
            ncol<-NULL
            options("show.error.messages"=FALSE)
            try(.plateOff())
            stop("",call. = FALSE)
        }
        if(ee<delka){
            winDialog(type="ok",paste("Wrong dimensions for \nthe plate of ",delka," groups !",sep=""))
            nrow<-NULL
            ncol<-NULL
            options("show.error.messages"=FALSE)
            try(.plateOff())
            stop("",call. = FALSE)
        }
    }
    
    #title<-paste(.fig.deeval(sheet$demo$call$main),"- multiple by groups")
    title<-sheet$demo$call$main
    
    if(is.null(title)){
        title<-sheet$demo$template$GCDkit$plot.name
        if(!is.null(title)){
            title<-paste(sheet$demo$template$GCDkit$plot.name,"- multiple by groups",sep="")
        }else{
            title<-"Multiple plot by groups"
        }    
    }
    
    typ<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
   
    if(!is.null(names(plate))&delka==length(grep("^Fig",names(plate)))){
        ee<-sapply(1:delka,function(i){
        z<-plate[[i]]$demo$call$main==levels(group)[i]
        return(z)
        })
        if(!all(ee)) return()
        plateRedraw()
        options(show.error.messages=FALSE)
        stop(call. = TRUE)
    }
    
    if(!is.null(names(plate))){
        x.data<-plate$xdata
        y.data<-plate$ydata
        sheet<-plate$sheet
    }
    
    if(!is.null(sheet$demo$legend)){
        #if(all(sheet$demo$legend$pch==15)) delka<-nrow*ncol #NEWLY COMMENTED OFF
    }
    
    multiplePerPage(delka,nrow=nrow,ncol=ncol,title=title,dummy=FALSE)
    # For spiderplots offer the possibility to plot the lines only   
    if(typ=="spider "){
        if(is.null(plot.symb)){ 
            xx<-winDialog("Plot symbols in diagrams?",type="yesno")
            if(xx=="NO"){
                plot.symb<-FALSE
            }else{
                plot.symb<-TRUE
            }
        }
        
        if(!plot.symb){        
            ee<-grep("points",names(sheet$demo$template))
            for(i in ee){
                eval(parse(text=paste("sheet$demo$template[[",i,"]][\"","cex","\"]<-0",sep="")))
            }
        }

    }else{
        if(is.null(pch)) {
            pch<-labels[names(x),"Symbol"]
            names(pch)<-names(x)
        }
        if(is.null(col)){
            col<-labels[names(x),"Colour"]
            names(col)<-names(x)
        }
    }
    
    sheet$demo$call$new<-FALSE
    sheet$demo$call$bg<-"white"
    #sheet$demo$call$cex.lab<-1.5
    #sheet$demo$call$cex.axis<-1.3
    sheet.bak<-sheet
    
    # Main LOOP
    for (f in 1:(length(levels(group)))){
        screen(f,new=FALSE) 
        what<-group==levels(group)[f]
        which2<-rownames(WR)[what] # Sample names to be plotted (all, taken from WR, nevermind whether relevant data are available
        
        if(typ=="spider "){
            par(mar=c(5,4,4,1))
            par(pty="m")
            which<-which(rownames(y)%in%which2) # Sample names in y, i.e. in  available relevant data only
            
            # Remove from template all irrelevant patterns (lines and points) for samples that do not belong to the given group
            sheet.tmp<-sheet.bak # Restore fresh copy of the sheet definition
            which<-which(rownames(y)%in%which2) # Sequence nos of the samples to be plotted (in the matrix y, originally y.data) 
            where="sheet.tmp$demo$template"
            which.rm<-which(!rownames(y)%in%which2)
            
            .rmTemplate<-function(what,which,where="sheet.tmp$demo$template"){
                ee<-lapply(which,function(i){
                        what=paste(paste("^",what,sep=""),i,"$",sep="")            
                        x<-grep(what,names(eval(parse(text=where))))
                        eval(parse(text=paste(where,"<<-",where,"[!(1:length(",where,"))%in%x]",sep="")))
                })
            }
            .rmTemplate("lines",which.rm)
            .rmTemplate("points",which.rm)
                
            #Plot field to portray the overall variability, if shaded.col is not NULL
            ee<-.shaded(y,x,plot=FALSE) # Get the coordinates but plot nothing 
            
            # Add the polygon as the first in the template, so that everything is plotted on the top
            sheet.tmp$demo$template<-c(list(field=list("polygon",x=ee$x,y=ee$y,col=shaded.col,border=shaded.col)),sheet.tmp$demo$template) 
                
            if(length(which)>0){ # Are there any relevant data?
                #Adjust legend (if present) just to show analyses plotted in this frame
                if(!is.null(sheet.tmp$demo$legend)){
                    args<-sheet.tmp$demo$legend               
                    cex.leg<-1
                    #cex.leg<-cex[1]*1.2 # Zoom up the legend textual labels?
                    
                    #if(length(leg.col)>1){ # DOES MOT WORK YET: These are spider contours, all pch are filled squares
                    if(all(args$pch==15)){ # These are spider contours, all pch are filled squares
                        if(f==1){
                            gero<-length(split.screen())
                            if(gero>length(levels(group))){                            
                                screen(gero,new=FALSE)
                                 .altLegJFM()
                                screen(1,new=FALSE)
                                sheet.tmp$demo$legend<-NULL
                            }else{
                                sheet.tmp$demo$legend<-list("legend",x="topright",y=NULL,legend=args$legend,col=args$col,pch=15,
                                    cex=cex.leg,pt.cex=args$pt.cex,lty="solid",bg="#FFFFFF66",ncol=2,bty="o",xpd=args$xpd,title=args$title,inset=0)                         
                            }
                        }else{
                                sheet.tmp$demo$legend<-NULL
                        }
                    }else{
                        leg.which<-args$legend%in%rownames(y)[which]
                        options(show.errormessages=TRUE)
                        if(!plot.symb){ # No symbols, just lines
                            sheet.tmp$demo$legend<-list(x="topright",y=NULL,legend=args$legend[leg.which],col=args$col[leg.which],pch="",
                                cex=cex.leg,pt.cex=args$pt.cex,lty="solid",bg="#FFFFFFCC",ncol=2,bty="o",xpd=FALSE,inset=0)
                        }else{ # normal plotting symbols desired
                            #if(f==2) browser()
                            sheet.tmp$demo$legend<-list(x="topright",y=NULL,legend=args$legend[leg.which],col=args$col[leg.which],pch=args$pch[leg.which],
                                cex=cex.leg,pt.cex=args$pt.cex,lty="solid",bg="#FFFFFFCC",ncol=2,bty="o",xpd=FALSE,inset=0) 
                        }
                   }
                    
                    # If plotting only lines, no symbols are desired
                    if(!plot.symb){
                        args$pt.cex<-rep(0,length(which))
                        args$pch<-rep(" ",length(which))
                    }
                }
                #pp$draw(1,1,ylab="",xlab="",main=levels(group)[f],axes=FALSE)
                
            }else{ # Nothing to plot in the given group
                sheet.tmp$demo$template<-c(sheet.tmp$demo$template,list(
                    points1=list("points", x=sheet.tmp$demo$call$xlim, y=sheet.tmp$demo$call$ylim,pch=NULL),
                    lines1=list("lines", x=sheet.tmp$demo$call$xlim, y=sheet.tmp$demo$call$ylim, lwd=5, col=2, lty="solid",pch=NULL),
                    points2=list("points", x=sheet.tmp$demo$call$xlim, y=rev(sheet.tmp$demo$call$ylim),pch=NULL),
                    lines2=list("lines", x=sheet.tmp$demo$call$xlim, y=rev(sheet.tmp$demo$call$ylim), lwd=5, col=2, lty="solid",pch=NULL)
                     #x<-1000
                ))
                sheet.tmp$demo$legend<-list(x="topright",y=NULL,lty="solid",bg="transparent",ncol=2,bty="o",xpd=TRUE)    
            }
            
            # Assign sheet to the global environment, plot the lot
            sheet.tmp$demo$call$main<-levels(group)[f]
            sheet.tmp$demo$call$ylab<-""
                
            # Assign globally and (re)draw
            assign("sheet",sheet.tmp,.GlobalEnv)
            assign("x.data",x,.GlobalEnv)

            if(length(which)>0){
                assign("y.data",y[which,,drop=FALSE],.GlobalEnv)
            }else{
                assign("y.data",matrix(nrow=1,ncol=1),.GlobalEnv)
            }
           
            pp<<-figaro(demo,prefix="sheet")
            figRedraw()
            #assign("x.data",x.data.bak,.GlobalEnv)
            #assign("y.data",y.data.bak,.GlobalEnv)
        }else{
            # Everything but the spiders
            if(typ=="ternary ") par(mar=c(1.5,0.5,2,0.5)) else {par(mar=c(4.5,5.5,2,1.5));par(pty="s")}  
            which<-which(names(y)%in%which2)
            if(!all(is.na(x[which]))){ 
                sheet$demo$call$main<-levels(group)[f]
                ee<-sheet$demo$call
                assign("sheet",sheet,.GlobalEnv)
                pp<<-figaro(demo,prefix="sheet")
                sheet$demo$call<-ee
                pp$draw(x[which],y[which],xlab=xlab,ylab=ylab,pch=pch[which],col=col[which],cex=cex[which], xaxs = "i",  yaxs = "i", bg="white",main=levels(group)[f])
            }else{
                pp$draw(NULL,NULL,xlab=xlab,ylab=ylab,pch=pch[which],col=col[which],main=levels(group)[f])
                lines(par("usr")[1:2], par("usr")[3:4],lwd=2,col="red")
                lines(par("usr")[1:2], par("usr")[4:3],lwd=2,col="red")
            }
            #box(which="figure")
            assign("x.data",x,.GlobalEnv)
            assign("y.data",y,.GlobalEnv)
        }
       .saveCurPlotDef(f,which)
    }
    
    # Title for the whole plate
    mtext(text=annotate(plate$title),side=3,line=0.25,outer=TRUE,cex=1.5)
    
    # Export the sheet
    assign("sheet",sheet.bak,.GlobalEnv)
    
    # Clean up
    rm(sheet.tmp)
    rm(sheet.bak)
    invisible()
}

figSave<-function(){
    filename<-winDialogString("Enter filename","My graph")
         pch<-labels[,"Symbol"]
         col<-labels[,"Colour"]
         eval(parse(text=paste("save(col,pch,pp,x.data,y.data,sheet,file = \"",filename,".fgr\", ascii = TRUE)",sep="")))
         cat("Graph stored in file ", filename, ".fgr\n",sep="")
}

figLoad<-function(){
         on.exit(options("show.error.messages"=TRUE))
         options("show.error.messages"=FALSE)
         filename<-choose.files(caption = "Select file",multi = FALSE, filters=matrix(ncol=2,byrow=TRUE,c("Figaro graph (*.fgr)","*.fgr")))
         if(filename==""){cat("Cancelled.\n");stop(call. = TRUE)}
         options("show.error.messages"=TRUE)
         
         load(filename)
         cat("Graph restored from file ", filename,"\n",sep="")
         pp<<-pp
         sheet<<-sheet
         x.data<<-x.data
         y.data<<-y.data
         
         options(show.error.messages=FALSE)
            ee<-try(assign("col",col,.GlobalEnv))
            ee1<-try(assign("pch",pch,.GlobalEnv))
         options(show.error.messages=TRUE)
         if(class(ee)=="try-error"|class(ee1)=="try-error"){
            assign("col",rep(1,length(x.data)),.GlobalEnv)
            assign("pch",rep(1,length(x.data)),.GlobalEnv)
         }

         if(!screen()){
            windows(width = 6.5, height = 6.5, pointsize = 10)
            figaroOn()
            if(getOption("gcd.menus")!=""){
            #if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
                winMenuAddItem("Plot editing","Highlight multiple points","disable")
                winMenuAddItem("Plots","Multiple plots by groups","disable")
                #winMenuAddItem("Plot editing","Groups by outline","disable")         
            }
         }else{
            sheet$demo$call$new<<-FALSE
            sheet$demo$call$bg<<-"white"
            .saveCurPlotDef()
         }
         figRedraw()
}

figaroOff<-function(){
    on.exit(options("show.error.messages"=TRUE))
    if(getOption("gcd.menus")==""){
    #if(.Platform$OS.type!="windows"|.Platform$GUI!="Rgui"){
        return() # No menus, quit
    } 
    
    options(show.error.messages=FALSE)
    try(menuOff("Plot editing"))
    winMenuAddItem<-get("winMenuAddItem",.GlobalEnv)
    
    if(package.name=="GCDkit"|package.name=="GCDkitDevelop"){
        try(winMenuAddItem("Plots","Multiple plots by groups","disable"))
    }
    #winMenuAddItem("Data handling","Classify","disable")
    if(screen())try(winMenuAddItem("Plot editing","Edit a plot from a plate","enable"))
}


figaroOn<-function(keep.plate=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    if(getOption("gcd.menus")==""){
    #if(.Platform$OS.type!="windows"|.Platform$GUI!="Rgui"){
        return() # No menus, quit
    }
    
    winMenuAddItem<-get("winMenuAddItem",.GlobalEnv)
    winMenuNames<-get("winMenuNames",.GlobalEnv)
    winMenuItems<-get("winMenuItems",.GlobalEnv)
    
    items <- winMenuItems("Plot editing") 
    #items.orig<-utils::winMenuItems("Plot editing") # Original R version 
    
    plot.type <- paste(sheet$demo$template$GCDkit$plot.type, 
        " ", sep = "")
    if (plot.type == "ternary ") {
        figaroOff()
    }
    if (plot.type == "spider ") {
        figaroOff()
        items <- items[c(1:7,9:12, 14:16)]
        items <- items[names(items) != "Linear fit" & names(items) != 
            "Curve"]
    }
    
     if (plot.type == "GIS ") {
        figaroOff()
        items <- items[c(1:7, 11:16,19)]
        items <- items[names(items) != "Linear fit" & names(items) != 
            "Curve"]
    }
    
    if (plot.type == "profile ") {
        figaroOff()
        items <- items[c(1:7, 9:12,15)]
        items <- items[names(items) != "Linear fit" & names(items) != 
            "Curve"]
    }
    
    for (i in names(items)) {
        winMenuAddItem("Plot editing", i, "enable")
    }
    
    if(package.name=="GCDkit"|package.name=="GCDkitDevelop") winMenuAddItem("Plots", "Multiple plots by groups", "enable")
    if(!screen())winMenuAddItem("Plot editing","Edit a plot from a plate","disable") 
    if(!keep.plate).plateOff()
    invisible()
}

.blink<-function(i,col1="red",col2="white"){
  #cat("Press Esc to stop\n")
  #on.exit(.do.this(i))
  pp$set(col="gray")
  x<-x.data[i]
  y<-y.data[i]
  points(x,y,col=labels[i,"Colour"],pch=labels[i,"Symbol"])
  points(x,y,col=col1,pch=1,cex=2.5,lwd=1.5)
     #repeat{
     #   points(x,y,col=col1,pch=1,cex=2)
     #   points(x,y,col=col1,pch=1,cex=2)
     #   points(x,y,col=col2,pch=1,cex=2)
     #   points(x,y,col=col1,pch=1,cex=2)
     #   points(x,y,col=col1,pch=1,cex=2)
     # }
}


.do.this<- function(i) {
  pp$draw()
  return()
}


highlightSelection<-function(){
    ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    if(ee=="spider "){
        which<-selectSubset(where=cbind(labels[rownames(y.data),],WR[rownames(y.data),]),save=FALSE, multiple=multiple,GUI=TRUE)
    }else{
        which<-selectSamples(print=FALSE)
    }
    
    if(is.null(which)){cat("Cancelled.\n");options("gcd.ident"=0);return()}
    if(!getOption("gcd.shut.up")){cat("Selected samples:","\n");print(which)}
    flush.console()
    
    ee<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    if(ee!="spider "){
        which<-match(which,names(y.data))
        #which<-which[!is.na(which)]
        ee<-.blink(which)
        .figOk()
        #ee<-locator(1)
    }else{
        which<-match(which,rownames(y.data))
        #Polygon for overall variation; add it as the first in the template, so that everything is plotted on the top
        ee<-.shaded(y.data,x.data,plot=FALSE) # Get the coordinates but plot nothing 
        sheet$demo$template<<-c(list(field=list("polygon",x=ee$x,y=ee$y,col="gray",border="gray")),sheet$demo$template) 
        # Draw and wait
        .spiderPlotRedraw(which=which)
        #.fig.spider.selected(which,x.data,y.data,main=sheet$demo$call$main,shaded.col="gray",col="red",lwd=1.5,pch=1,type="b") 
        .figOk()
        # Remove polygon and redraw the original
        sheet$demo$template<<-sheet$demo$template[-1]
    }  
        figRedraw()
}


.highlight.pick<-function(text=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    i<-select.list(rownames(WR),multiple=TRUE)
    if(length(i)==0){cat("Cancelled.\n");options(show.error.messages=FALSE);stop(call. = TRUE)}
    print(i)
    if(text){
        text(x.data[i],y.data[i],i,col="pink")
    }else{
        ee<-.blink(i)
    }
}


# Redundant, for debugging only
.tri<-function(aa,bb){
    cc<-100-aa-bb
    suma<-aa+bb+cc
    aa<-aa/suma
    bb<-bb/suma
    cc<-cc/suma
    x<-cc+bb/2
    y<-sqrt(3)*bb/2
    print(x)
    print(y)
}


formatResults<-function(x){
    ee<-matrix(nrow=nrow(WR),ncol=ncol(x))
    rownames(ee)<-rownames(WR)
    colnames(ee)<-colnames(x)
    ee[rownames(x),]<-x[rownames(x),]
    return(ee)
}


recast<-function(total=100){
    on.exit(options("show.error.messages"=TRUE))
    total<-winDialogString("The desired total",as.character(total))
    if(is.null(total)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    total<-as.numeric(total)
    if(is.na(total)){
                winDialog(type="ok","Non-numeric sum!")
                options(show.error.messages=FALSE)
                stop("",call. = FALSE)
            }
    results<<-normalize2total(total=total)
    colnames(results)<<-paste(colnames(results),".",total,sep="")
    if(!getOption("gcd.shut.up"))print(results)
}


#.extract.menu<-function(){ # NOT WORKING, CURRENTLY UNUSED
#    ee<-read.table(paste(gcdx.dir,"\\menu.r",sep=""),sep=",",fill=TRUE)
#    i<-grep("winMenuAddItem",as.character(ee[,1]))
#    return(ee[i,1:2])
#}

# Removes all user menus (Windows only!!!)
.menu.del<-function(which=utils::winMenuNames()){
    on.exit(options("show.error.messages"=TRUE))
    if(getOption("gcd.menus")=="") return() # No TclTk nor Windows menus available in the system, quit
    
    #if(.Platform$OS.type!="windows"|.Platform$GUI!="Rgui")return() # Not Windows GUI, quit
    options("show.error.messages"=FALSE)
    i<-which(substr(which,1,1)!="$")
    which<-which[i]
    for(k in which){
        print(k)
        ee<-try(utils::winMenuDel(k))
    }
    invisible()
}

menuOff<-function(which=winMenuNames()){
    on.exit(options("show.error.messages"=TRUE))
    
    if(getOption("gcd.menus")=="") return() # Not TclTk nor Windows menus, quit
    options(show.error.messages=FALSE)
    
    if(getOption("gcd.menus")=="tcltk"){ 
        try(winMenuAddItem<-get("winMenuAddItem",.GlobalEnv))
        try(winMenuNames<-get("winMenuNames",.GlobalEnv))
        try(winMenuItems<-get("winMenuItems",.GlobalEnv))
        tkfocus(topMenu)
        tkraise(tt.menu)
    }    

    prd<-sapply(which,function(k){
            ee<-try(sapply(names(winMenuItems(k)),function(l){
                if(substr(l,1,10)!="----------"){
                    try(winMenuAddItem(menuname=k,itemname=l,action="disable"))
                }
            invisible()
            }))
    })
    options(show.error.messages=TRUE)
    
    x<-package.name
    if(x=="GCDkitDevelop") x<-"GCDkit"
    if(any(which==x)){ 
        eval(parse(text=paste("winMenuAddItem(\"",x,"\",\"Load data file\",\"enable\")",sep="")))
        eval(parse(text=paste("winMenuAddItem(\"",x,"\",\"Paste data from clipboard\",\"enable\")",sep=""))) 
        if(is.list(schema.db)){
            eval(parse(text=paste("winMenuAddItem(\"",x,"\",\"Online search EarthChem.org\",\"enable\")",sep=""))) # Internet connection ok
        } 
        eval(parse(text=paste("winMenuAddItem(\"",x,"\",\"Quiet mode?\",\"enable\")",sep="")))
        eval(parse(text=paste("winMenuAddItem(\"",x,"\",\"Options...\",\"enable\")",sep="")))
        eval(parse(text=paste("winMenuAddItem(\"",x,"\",\"Help\",\"enable\")",sep="")))
        eval(parse(text=paste("winMenuAddItem(\"",x,"\",\"About ",x,"\", \"enable\")",sep="")))
        eval(parse(text=paste("winMenuAddItem(\"",x,"\",\"Exit ",x,"\", \"enable\")",sep="")))
    }
    invisible()
}

menuOn<-function(which=winMenuNames()){
    on.exit(options("show.error.messages"=TRUE))
    
    if(getOption("gcd.menus")=="") return() # Not TclTk nor Windows menus, quit
    
    
    if(getOption("gcd.menus")=="tcltk"){ 
        try(winMenuAddItem<-get("winMenuAddItem",.GlobalEnv))
        try(winMenuNames<-get("winMenuNames",.GlobalEnv))
        try(winMenuItems<-get("winMenuItems",.GlobalEnv))
        tkfocus(topMenu)
        tkraise(tt.menu)
    }
    
    options(show.error.messages=FALSE)
    prd<-sapply(which,function(k){
        ee<-try(sapply(names(winMenuItems(k)),function(l){
            if(substr(l,1,10)!="----------"){
                try(winMenuAddItem(menuname=k,itemname=l,action="enable"))
            }
            invisible()
        }))
    })
    
    if(!is.list(schema.db)){
            if(schema.db==-1)try(winMenuAddItem(menuname="GCDkit",itemname="Online search EarthChem.org",action="disable")) # No Internet connection, no online search
    } 
    
    invisible()
}

strip<-function(xlab="", ...){
    on.exit(options("show.error.messages"=TRUE))
    if(is.numeric(groups)){
        winDialog(type="ok",paste("Groups not defined!"))
        return()
    }
    
    if(xlab=="") {
        xlab<-selectColumnLabel(colnames(WR),message="Which variable?",default="",silent=TRUE,empty.ok=FALSE)    
        where<-selectSamples(print=FALSE)
    }else{
        where<-rownames(WR)
    }

    if(!is.na(as.numeric(xlab))){
            xlab<-colnames(WR)[xlab]
            what<-WR[,xlab]
    }else{
            ee<-calcCore(xlab)
            what<-ee$results
            xlab<-ee$equation
    }
    
    temp<-printSamples(xlab,which=where,print=FALSE)
    temp<-temp[!is.na(temp),]
    ii<-names(temp)
    
    names(groups)<-rownames(WR)
    gr<-factor(groups[ii])
    trellis.device(new = TRUE, retain = FALSE)
    trellis.par.set("background", list(col = "white"))
    print(stripplot(gr~temp,aspect = 1,jitter = TRUE,xlab=annotate(xlab),pch=labels[ii,"Symbol"],col=labels[ii,"Colour"]))
    invisible()
}


threeD<-function(xlab="",ylab="",zlab=""){
    on.exit(options("show.error.messages"=TRUE))
    GUI<-xlab=="" 
    # x axis
    if(nchar(xlab)==0) xlab<-selectColumnLabel(where=colnames(WR),message="x axis \n(ENTER to pick from a list)",default="",silent=TRUE,print=TRUE,empty.ok=FALSE)
    if(!is.na(as.numeric(xlab))){
        xlab<-colnames(WR)[xlab]
        x<-WR[,xlab]
    }else{
         ee<-calcCore(xlab)
         x<-ee$results
         xlab<-ee$equation
    }
    
    # y axis
    if(nchar(ylab)==0) ylab<-selectColumnLabel(where=colnames(WR),message="y axis \n(ENTER to pick from a list)",default="",silent=TRUE,print=FALSE,empty.ok=FALSE)
    if(!is.na(as.numeric(ylab))){
        ylab<-colnames(WR)[ylab]
        y<-WR[,ylab]
    }else{
        ee<-calcCore(ylab)
        y<-ee$results
        ylab<-ee$equation
    }

    # z axis
    if(nchar(zlab)==0)zlab<-selectColumnLabel(where=colnames(WR),message="z axis \n(ENTER to pick from a list)",default="",silent=TRUE,print=FALSE,empty.ok=FALSE)
    if(!is.na(as.numeric(zlab))){
        zlab<-colnames(WR)[zlab]
        z<-WR[,zlab]
    }else{
        ee<-calcCore(zlab)
        z<-ee$results
        zlab<-ee$equation
    }
    
    if(GUI){
        where<-selectSamples(print=FALSE)
        if(!getOption("gcd.shut.up")) print(where)
    }else{
        where<-names(x)
    } 
    
    i<-cbind(x,y,z)
    i<-i[where,]
    ee<-c("X","Y","Z")
    colnames(i)<-ee
    i<-as.data.frame(filterOut(i,ee,1))
    trellis.device(new = TRUE,retain = FALSE,title=paste("Three-D plot of ",xlab,", ",ylab," and ",zlab,sep=""))
    trellis.par.set("background",list(col="white"))

    print(cloud(i[,3]~i[,1]*i[,2],xlab=annotate(xlab),ylab=annotate(ylab),zlab=annotate(zlab),scales=list(arrows=FALSE,relation="sliced"),pch=labels[rownames(i),"Symbol"],col=labels[rownames(i),"Colour"],screen = list(z = 20, x = -70, y = 3)))

    if(GUI){
        pp<-winDialog("yesno","Rotate?")
        rot<-list(z = 20, x = -70, y = 3)
        while (pp=="YES") {
            rot$x<-as.numeric(winDialogString(paste("Rotate around",xlab,"axis:  (if [0,0,0], ",xlab," is horizontal,",ylab,"vertical and ",zlab,"perpendicular to the screen)"),as.character(rot$x)))
            if(length(rot$x)==0){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}

            rot$y<-as.numeric(winDialogString(paste("Rotate around",ylab,"axis:  (if [0,0,0], ",xlab," is horizontal,",ylab,"vertical and ",zlab,"perpendicular to the screen)"),as.character(rot$y)))
            if(length(rot$y)==0){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}

            rot$z<-as.numeric(winDialogString(paste("Rotate around",zlab,"axis:  (if [0,0,0], ",xlab," is horizontal,",ylab,"vertical and ",zlab,"perpendicular to the screen)"),as.character(rot$z)))
            if(length(rot$z)==0){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}

            print(cloud(i[,3]~i[,1]*i[,2],xlab=annotate(xlab),ylab=annotate(ylab),zlab=annotate(zlab),scales=list(arrows=FALSE),pch=labels[rownames(i),"Symbol"],col=labels[rownames(i),"Colour"],screen = rot))
            pp<-winDialog("yesno","Rotate again?")
        }
    }
    figaroOff()
    invisible()
}

.round.digits<-function(x){
    x<-x[!is.na(x)]
    x<-x[x!=Inf]
    x<<-x[x!=-Inf]


    for(i in (0:6)){
        ee<-round(x,i)
        if(all(x==ee)){return(i)}
        }
    return(i)
}


normalize2total<-function(what=NULL,total=100){
    if(is.null(what))what<-selectColumnsLabels(default=anhydrous)
    if(length(what)==1)what<-unlist(strsplit(what,","))
    if(is.vector(what))what<-WR[,what]
    if(length(total)==1) total<-rep(total,nrow(what))
    names(total)<-rownames(what)
    y<-t(sapply(rownames(what),function(f) what[f,]/sum(what[f,],na.rm=TRUE)*total[f]))
    return(y)
}

#############################################################################
#               Assigns plotting colours on a single label                  #
#############################################################################


assignColLab<-function(lab=NULL,pal=NULL,colours=NULL,display.legend=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    
    # Select the variable and specify the colours
    if(!is.null(lab)){
        GUI<-FALSE
        if(is.character(lab)){
            ee<-which(colnames(labels)==lab)
            if(length(ee)==0){
               cat("Invalid label: ",lab,"\n",sep="")
               options(show.error.messages=FALSE)
               stop(call. = TRUE) 
            }
            lab<-ee+1
        }
            lab<-floor(lab)
            
            if(lab<=0|lab>ncol(labels)+1){
               cat("Response out of range: ",lab,"\n",sep="")
               options(show.error.messages=FALSE)
               stop(call. = TRUE) 
            }
        what<-lab
       
        if(is.null(colours)){
            if(is.null(pal)){
               cat("Either colours or palette have to be specified! Quitting...\n")
               options(show.error.messages=FALSE)
               stop(call. = TRUE) 
            }
            colours<-selectPalette(nrow(labels),pal)
            colours<-as.vector(colours)
        }else{
            colours<-rep(colours,100)[1:nrow(labels)]
        }
    }else{
        GUI<-TRUE
        colours=labels$Colour # Take what is already there if in GUI
        what<-selectColumnLabel(where=c("Sample name",colnames(labels)),empty.ok=FALSE)
    }
    
    
    # Store the information on variable used for a later use
    leg.col<-what-1
    if(leg.col==0) leg.col<--1 # -1 for sample names, otherwise which column in labels?
    assign("leg.col",leg.col,.GlobalEnv)
    
    if(leg.col>0){
        where.leg<-as.character(labels[,what-1])
        where.leg[is.na(where.leg)]<-"Unspecified"
    }else{
        where.leg<-rownames(WR)
    }    
    
    # Display available colours
    if(GUI)showColours()

    # Build legend
    xx<-.build.legend(where.leg,col=colours,chars=FALSE)
    if(is.vector(xx))xx<-t(xx)
    
    if(!GUI){
        if(!is.null(pal)){
            colours<-as.vector(selectPalette(nrow(xx),pal))
        }
        xx[,3]<-colours[1:nrow(xx)]
    }
    
    xx<-xx[,-(1:2)]
    xx<-as.matrix(xx)
    colnames(xx)<-"Colour"
    if(nrow(xx)==1)rownames(xx)<-where.leg[1]
    
    # Invoke editor if in GUI
    if(GUI){
        xx<-edit(xx)
        if(is.vector(xx))xx<-t(xx)
    }
    
    # Assign the labels
    for (f in 1:nrow(xx)){
        labels[where.leg==rownames(xx)[f],"Colour"]<-xx[f,1]
    }        
    .assignWithNamespaceHard("labels",labels)
    
    if(GUI){
        options(show.error.messages=FALSE)
        try(dev.off(dev.cur()))
    }
    
    if(GUI|display.legend)showLegend(new.plot=TRUE)
    
    if(GUI){
        message<-paste(nrow(xx)," group(s) successfully assigned",sep="")
        winDialog(type="ok",message)
    }
    cat("\n")
}

#############################################################################
#          Assigns the initial letter as symbol to groups                   #
#############################################################################

assignSymbLett<-function(lab=NULL,display.legend=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    # Select the variable to specify the symbols
    if(is.null(lab)){
        lab<-selectColumnLabel(message="Select the label whose 1st letters will be used as plotting symbols",empty.ok=FALSE)
        GUI<-TRUE
    }else{
        if(is.character(lab)){
            ee<-which(colnames(labels)==lab)
            if(length(ee)==0){
               cat("Invalid label: ",lab,"\n",sep="")
               options(show.error.messages=FALSE)
               stop(call. = TRUE) 
            }
            lab<-ee
        }
            lab<-floor(lab)

            if(lab<1|lab>ncol(labels)){
               cat("Response out of range: ",lab,"\n",sep="")
               options(show.error.messages=FALSE)
               stop(call. = TRUE) 
            }
        GUI<-FALSE
    }
    
    x<-as.vector(labels[,lab])
    leg.pch<<-lab
    
    #Build legend
    xx<-.build.legend(labels[,lab],cols=FALSE)
    if(is.vector(xx))xx<-t(xx)
    xx<-xx[,1:2]
    if(is.vector(xx))xx<-t(xx)
    
    # Assign symbols
    labels$Symbol<-substring(paste(as.character(x)," "),1,1)
    labels$Symbol<-as.character(labels$Symbol)
    xx<-length(levels(factor(labels$Symbol)))
    .assignWithNamespaceHard("labels",labels)
    
    # Display legend
    if(GUI|display.legend)showLegend(new.plot=TRUE)
    if(GUI){
        message<-paste(xx," group(s) successfully assigned",sep="")
        winDialog(type="ok",message)
    }
    cat("\n")
}



#############################################################################
#               Assigns plotting symbols on a single label                  #
#############################################################################
assignSymbLab<-function(lab=NULL,symbols=NULL,display.legend=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    
    # Select the variable and specify the symbols
    if(!is.null(lab)){
        GUI<-FALSE
        if(is.character(lab)){
            ee<-which(colnames(labels)==lab)
            if(length(ee)==0){
               cat("Invalid label: ",lab,"\n",sep="")
               options(show.error.messages=FALSE)
               stop(call. = TRUE) 
            }
            lab<-ee+1
        }
            lab<-floor(lab)

            if(lab<=0|lab>ncol(labels)+1){
               cat("Response out of range: ",lab,"\n",sep="")
               options(show.error.messages=FALSE)
               stop(call. = TRUE) 
            }
        if(is.null(symbols)){
               cat("Plotting symbols have to be specified! Quitting...\n")
               options(show.error.messages=FALSE)
               stop(call. = TRUE) 
        }else{
            symbols<-rep(symbols,100)[1:nrow(labels)]
        }
        what<-lab
    }else{
        GUI<-TRUE
        what<-selectColumnLabel(where=c("Sample name",colnames(labels)),empty.ok=FALSE)
    }
    
    leg.pch<-what-1
    if(leg.pch==0) leg.pch<--1 # -1 for sample names, otherwise which column in labels?
    assign("leg.pch",leg.pch,.GlobalEnv)
    
    if(leg.pch>0){
        where.leg<-as.character(labels[,what-1])
        where.leg[is.na(where.leg)]<-"Unspecified"
    }else{
        where.leg<-rownames(WR)
    }
    
    # Display available symbols
    if(GUI) showSymbols()
    
    # Build the legend
    xx<-.build.legend(where.leg,cols=FALSE)
    if(is.vector(xx))xx<-t(xx)
    
    if(!GUI){
        xx[,2]<-symbols[1:nrow(xx)]
    }
    
    xx<-xx[,2]
    xx<-as.matrix(xx)
    colnames(xx)<-"Symbol"
    if(nrow(xx)==1)rownames(xx)<-labels[1,what]
    
    # Invoke editor if in GUI
    if(GUI){
        xx<-edit(xx)
    }
    
    if(is.vector(xx))xx<-t(xx)
    
    # Assign the labels
    for (f in 1:nrow(xx)){
        labels[where.leg==rownames(xx)[f],"Symbol"]<-xx[f,1]
    }
    if(all(!is.na(as.numeric(labels$Symbol)))){
        labels$Symbol<-as.numeric(labels$Symbol)
    }
    .assignWithNamespaceHard("labels",labels)
    
    if(GUI){
        options(show.error.messages=FALSE)
        try(dev.off(dev.cur()))
    }
    
    # Display legend
    if(GUI|display.legend)showLegend(new.plot=TRUE)
    if(GUI){
        message<-paste(nrow(xx)," group(s) successfully assigned",sep="")
        winDialog(type="ok",message)
    }
    cat("\n")
    
}

#############################################################################
#             Assigns plotting symbols and colours by groups                #
#############################################################################
assignSymbGroup<-function(){
    on.exit(options("show.error.messages"=TRUE))
    labels$Symbol[labels$Symbol==NA]<-11
    labels$Colour[labels$Colour==NA]<-8
    leg.col<<-0
    leg.pch<<-0
    
    where<-as.character(groups)
    where[is.na(where)]<-"Unspecified"
    
    .assignWithNamespaceHard("labels",labels)
    xx<-.build.legend(where)
    xx<-xx[,-1,drop=FALSE]
    xx<-as.matrix(xx)
    colnames(xx)<-c("Symbol","Colour")
    if(nrow(xx)==1)rownames(xx)<-where[1]

    showSymbols()
    showColours()
    xx<-edit(xx)

    for (f in 1:nrow(xx)){
        labels[where==rownames(xx)[f],"Symbol"]<-xx[f,1]
        labels[where==rownames(xx)[f],"Colour"]<-xx[f,2]
    }
    
    if(all(!is.na(as.numeric(labels$Colour)))){  
        labels$Colour<-as.numeric(labels$Colour)
    }
    
    labels$Colour<-labels$Colour
    
    if(all(!is.na(as.numeric(labels$Symbol)))){
        labels$Symbol<-as.numeric(labels$Symbol)
    }

    labels$Symbol<-labels$Symbol
    rownames(xx)<-as.character(xx[,1])
    
    .assignWithNamespaceHard("labels",labels)
    
    options(show.error.messages=FALSE)
    try(dev.off(dev.cur()))
    try(dev.off(dev.cur()))
    
    showLegend(new.plot=TRUE)

    message<-paste(nrow(xx)," groups successfully assigned",sep="")
    winDialog(type="ok",message)
    cat("\n")
}

#############################################################################
#                                Merges groups                              #
#############################################################################

joinGroups<-function(){
    leg<-factor(groups)
    names.suites<-levels(leg)
    xx<-cbind(names.suites,names.suites)
    xx<-as.matrix(xx)
    colnames(xx)<-c("Old group","New group")
    if(nrow(xx)==1)rownames(xx)<-groups[1]

    xx<-edit(xx)

    for (f in 1:nrow(xx)){
        cat(names.suites[f],"--> ")
        groups[groups==xx[f,1]]<<-xx[f,2]
        cat(xx[f,2],"\n")
    }
    leg<-factor(groups)

    message<-paste(length(levels(leg))," groups successfully assigned",sep="")
    winDialog(type="ok",message)

    x<-winDialog(type="yesno","Append current groups to labels?")
    if(x=="YES"){
        name<-winDialogString("Name of the new column","groups")
        labels<-cbind(labels,groups)
        colnames(labels)[ncol(labels)]<-name
        .assignWithNamespaceHard("labels",labels)
        .assignWithNamespaceHard("grouping",ncol(labels))
        #grouping<<-ncol(labels)
    }else{
        .assignWithNamespaceHard("grouping",-100)
        #grouping<<--100
    }
    cat("\n")
}


#############################################################################
#                Edits a single label as if it was a factor                 #
#############################################################################
editLabFactor<-function(){
    what<-selectColumnLabel(empty.ok=FALSE)
    new.labs<-as.vector(labels[,what])
    xx<-as.matrix(levels(as.factor(new.labs)))
    colnames(xx)<-colnames(labels)[what]
    rownames(xx)<-NULL

    xx.old<-as.character(as.vector(xx))

    xx<-edit(xx)
    xx<-as.character(as.vector(xx))

    for (f in 1:length(xx)){
        new.labs<-sub(paste("^",xx.old[f],"$",sep=""),xx[f],as.character(new.labs))
    }
    labels[,what]<-new.labs
    .assignWithNamespaceHard("labels",labels)
    if(grouping==what) groups<<-new.labs
    cat("Replacement successful!\n")
}


#############################################################################
#      Makes accessible data frames/matrices already present in system      #
#############################################################################

accessVar<-function(var=NULL,GUI=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    x<-character()
    vars<-objects(name=globalenv())

    for(i in vars){
         ee<-mode(eval(parse(text=i)))
         if(ee=="list"|ee=="numeric")x<-c(x,i)
   }
    
    if(is.null(var)){
        var<-select.list(x,multiple=FALSE)
    }
  
    selected<-exists(var)
    if(!selected|length(selected)==0){
                print(var)
                winDialog(type="ok","Variable does not exist!")
                options("show.error.messages"=FALSE)
                stop("",call. = FALSE)
    }
    what<-get(var,.GlobalEnv)
    rownames(what)<-gsub("[']","",rownames(what))
    colnames(what)<-gsub("[']","",colnames(what))
    
    #filename<<-"R data"
    .assignWithNamespaceHard("filename","R data")
    #if(GUI) filename<<-NULL # If not called from command line, be silent
    if(GUI) .assignWithNamespaceHard("filename",NULL) # If not called from command line, be silent
    
    # Fix the extra X in the beginning and dot instead of slash in isotope names generated by "data" due to syntax chekcing
    coln<-colnames(what)
    coln<-gsub("^[X]87Rb.86Sr","87Rb/86Sr",coln)
    coln<-gsub("^[X]87Sr.86Sr","87Sr/86Sr",coln)
    coln<-gsub("^[X]147Sm.144Nd","147Sm/144Nd",coln)
    coln<-gsub("^[X]143Nd.144Nd","143Nd/144Nd",coln)
    # Perhaps add more here
    colnames(what)<-coln
    
    .loadData.process(what,merging=FALSE,GUI=GUI)
    
    # Store in the cube
    i<-grep(var,names(WRCube))
    if(length(i)>0){ # If the name already exists, add a time stamp 
        time<-strsplit(as.character(Sys.time())," ")[[1]][2]
        var<-paste(var,time)
        }
    pokeDataset(var)  
    assign("dataset.name",var,.GlobalEnv)
    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui") setWindowTitle(paste("//","GCDkit - ",var,sep=""))
    assign("results",numeric(0),.GlobalEnv)
    figaroOff()
    .save2hist("loadData()")
    invisible()
}


#############################################################################
#                      Copies results to clipboard                          #
#############################################################################
r2clipboard<-function(what=results) {
    on.exit(options("show.error.messages"=TRUE))
    if(is.null(what)) {winDialog(type = "ok", "No data available");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    filename<-file("clipboard",open="w")
    cat("Please wait....")
    flush.console()
    
    if(is.list(what) & package.name=="GCDkit.Mineral" & length(what)==1) what<-t(what[[1]])
    
    if(is.vector(what)){
        what<-as.matrix(what)
        colnames(what)<-NULL
    }

    if(is.table(what)){
        if(length(attributes(results)$dim)!=2){
                write.ftable(ftable(what),file=filename,quote=TRUE,digits=getOption("gcd.digits"))
                cat("done\n")
                flush.console()
                return()
            }else{
            what<-as.matrix(what)
        }
    }

    if(is.list(what)){
         lapply(1:length(what),function(i){
            ee<-data.frame(what[[i]],check.names=FALSE)
            x<-cbind(rownames(ee),data.frame(ee))
            colnames(x)<-c(dimnames(what)[[1]][i],colnames(ee))
            if(is.vector(what[[i]]))colnames(x)[2]<-""
            cat(".")
            flush.console()
            write.matrix(x,filename,sep="\t")
            write("\n",filename,append=TRUE)
         })
    }else{
        write.matrix(cbind(rownames(what),what),filename,sep="\t")
    }
        close(filename)
        cat("done.\n")
    flush.console()
    invisible()
}

.curveMy<-function(equation,col="darkred",lty="dashed",...){
     #ee<-eval(parse(text=paste("curve\(",as.expression(equation),",col=\"",col,"\",lty=\"",lty,"\",add=TRUE,...\)",sep="")))
     ee<-eval(parse(text=paste("curve(",as.expression(equation),",col=\"",col,"\",lty=\"",lty,"\",add=TRUE,...)",sep="")))
}

figGbo<-function(x.tol=0,y.tol=0,max.points=100,max.polygons=25){
    on.exit(options("show.error.messages"=TRUE))
    ee<-try(i<-pp,silent=TRUE)
     if(class(ee)=="try-error"){winDialog(type = "ok", "No graph template available");options("show.error.messages"=FALSE);stop(call. = TRUE)}
    
    cara<-function(){
        ee<-locator(1)
        if(is.null(ee)){
            return(ee)
        }
        x<-as.numeric(ee[1])
        y<-as.numeric(ee[2])
        points(x,y,col="blue")
        return(list(x=x,y=y))
    }
    if(x.tol==0 | y.tol==0){
        xlim<-sheet$demo$call$xlim
        x.tol<-abs(xlim[1]-xlim[2])/100
        ylim<-sheet$demo$call$ylim
        y.tol<-abs(ylim[1]-ylim[2])/100
    }
    temp<-list(clssf=list("NULL"))
#    xx<-matrix(NA,max.polygons,max.points)
#    yy<-matrix(NA,max.polygons,max.points)
    pp$draw()
    j<-1
    ee<-cara()
    while(!is.null(ee)){                                                        # czklus
        xx<-ee$x
        yy<-ee$y
        i<-1
        while((abs(xx[i]-xx[1])>x.tol | abs(yy[i]-yy[1])>y.tol) | i==1){        # cyklus polygonu krom 1. bodu
            i<-i+1
            ee<-cara()
            if (is.null(ee)) stop("Error - last polygon not closed",call. = FALSE)
            points(xx[i-1],yy[i-1],pch=10,col="white")                          # smaze kolecko z predch. kroku
            xx[i]<-ee$x
            yy[i]<-ee$y
            lines(c(xx[i-1],xx[i]),c(yy[i-1],yy[i]),col="blue",lty="dashed")    # nakresli caru
        }
        points(xx[i],yy[i],pch=10,col="white")                                  # smaze posledni kolecko
        polygon(xx,yy,col="wheat",border=barvy[j],lwd=2)                               # vybarvi polygon
        text(xx[1],yy[1],j,cex=2)
        points(x.data,y.data,pch=labels[names(x.data),"Symbol"],col=labels[names(x.data),"Colour"]) # nakresli body pres polygon
        ee<-cara()
        temp<-c(temp,eval(parse(text=paste("list(lines",j,"=list(\"NULL\",x=xx[!is.na(xx)],y=yy[!is.na(yy)]))",sep=""))))
        j<-j+1
    }
    j<-j-1
    temp$clssf<-list("NULL",use=1:j+1,rcname=as.character(1:j))
    #x<-winDialog(type="yesno","Save polygons' parameters?")
    #if (x=="YES") {
    #     description<-winDialogString("Enter filename","My polygon")
    #     eval(parse(text=paste("save(temp,sheet, file = \"",description,".Rdata\", ascii = TRUE)",sep="")))
    #     cat("Polygons stored in file ", description, ".Rdata\n",sep="")
    #}
    classify(source.sheet=FALSE,clas=temp,overlap=TRUE)
}

.my.template<-function(cmdlist) {
    for (i in 1:length(cmdlist)) {
      args <- cmdlist[[i]]
      switch(args[[1]],
        lines=lines(x=args$x,y=args$y,col=args$col, lty=args$lty, # modified by VJ
        lwd=args$lwd,pch=args$pch),
            polygon=polygon(args$x, args$y, col=args$col, border=FALSE),
        text=text(args$x, args$y, args$text, col=args$col,
            cex=args$cex, font=args$font,adj=args$adj,srt=args$srt),# modified by VJ cex=args$size
        etext=text(args$x, args$y, parse(text=args$text),
            col=args$col, cex=args$size, font=args$font),
        abline=abline(v=args$v,h=args$h,a=args$a,b=args$b,col=args$col,lty=args$lty), # modified by VJ
        points=points(x=args$x,y=args$y,col=args$col,pch=args$pch,cex=args$cex), # added by VJ
        axis=axis(side=args$side,at=args$at,labels=args$labels,cex.axis=args$cex.axis) # added by VJ
      )
    }   
}

refreshFig<-function(){
    omit.axes=(sheet$demo$call$axes==FALSE) # This is a ternary plot
    if(length(omit.axes)==0)omit.axes<-FALSE
    if (!(omit.axes)) {
        #if (axes) {
           title(main = "", xlab = "", ylab = "")
            axis(1)
            axis(2)
            box()
        #}
    }
    # For ternary plots do not plot axes, mask the area surrounding the triangle
    else{
        polygon(x=c(-0.03,0,.5,1,1.05,1.05,-0.03,-0.03),y=c(0,0,sqrt(3)/2,0,0,1.03,1.03,0),col="white",border=NA)
        polygon(x=c(-0.03,1.03,1.03,-0.03,-0.03),y=c(-0.1,-0.1,0,0,-0.1),col="white",border=NA)
        rect(1.03,-0.1,1.05,1.05,col="white",fg="white",border=NA)
    }
    .my.template(sheet$demo$template)
    points(x.data,y.data,pch=labels[,"Symbol"],col=labels[,"Colour"])
}

.autoassign.pch<-function(what=NULL,edit=TRUE,legend=FALSE){
    if(is.null(what)){
        ee<-colnames(labels)[which(colnames(labels)!=("Colour")&colnames(labels)!="Symbol"&colnames(labels)!="Size")]
        i<-selectColumnLabel(where=ee,empty.ok=FALSE)
        what<-labels[,ee[i]]
        leg.pch<<-which(colnames(labels)==ee[i])
        leg.col<<-which(colnames(labels)==ee[i])
    }

    fact.what<-factor(what)
    col.avail<-rep(barvy,length.out=length(levels(fact.what)))
    pch.avail<-rep(1:18,length.out=length(levels(fact.what)))
    col<-rep(NA,length(what))
    pch<-rep(NA,length(what))
    if(edit){
        x<-cbind(levels(fact.what),pch.avail,col.avail)
        col.chart<-showColours()
        symb.chart<-showSymbols()
        colnames(x)<-c("Name","Symbol","Colour")
        x<-edit(x)
        pch.avail<-x[,2]
        col.avail<-x[,3]
    }

    for(i in 1:length(levels(fact.what))){
            ee<-what==levels(fact.what)[i]
            col[ee]<-col.avail[i]
            pch[ee]<-pch.avail[i]
    }

    if(all(!is.na(as.numeric(pch)))){
        pch<-as.numeric(pch)
    }

    if(all(!is.na(as.numeric(col)))){
        col<-as.numeric(col)
    }

    pch<-as.numeric(pch)
    dev.off(symb.chart)
    dev.off(col.chart)
    #dev.off(dev.prev())
    #dev.off(dev.cur())
    message<-paste(i," group(s) successfully assigned",sep="")
    winDialog(type="ok",message)
    cat("\n")
    x<-list(pch=pch,col=col)
    return(x)
}

#############################################################################
#                                                                           #
#                         ANOMALY PLOT (PETERPLOT)                          #
#                                                                           #
#############################################################################
peterplot<-function(xaxis="",yaxis="",zaxis="",ident=FALSE,scaling.small=labels[1,"Size"],scaling.big=2*scaling.small,assign.symbols=FALSE){
    # x-axis
    if(nchar(xaxis)==0){
        GUI<-TRUE
        xaxis<-selectColumnLabel(colnames(WR),message="x-axis, e.g. (SiO2+2.1)*TiO2/3\nor press ENTER to pick from a list",default="",sample.names=FALSE,silent=TRUE,empty.ok=FALSE)
    }else{
        GUI<-FALSE
    }
    if(!is.na(as.numeric(xaxis))){
        xaxis<-colnames(WR)[xaxis]
        x.data<-WR[,xaxis]
    }else{
        ee<-calcCore(xaxis)
        x.data<-ee$results
        xaxis<-ee$equation
    }
    
    # y-axis
    if(nchar(yaxis)==0) yaxis<-selectColumnLabel(colnames(WR),message="y-axis\n(press ENTER to pick from a list)",default="",sample.names=FALSE,silent=TRUE,print=FALSE,empty.ok=FALSE)
    if(!is.na(as.numeric(yaxis))){
        yaxis<-colnames(WR)[yaxis]
        y.data<-WR[,yaxis]
    }else{
        ee<-calcCore(yaxis)
        y.data<-ee$results
        yaxis<-ee$equation
    }

    # z-axis
    if(nchar(zaxis)==0) zaxis<-selectColumnLabel(colnames(WR),message="symbols represent distribution of\n(press ENTER to pick from a list)",default="",sample.names=FALSE,silent=TRUE,print=FALSE,empty.ok=FALSE)
    if(!is.na(as.numeric(zaxis))){
        zaxis<-colnames(WR)[zaxis]
        z.data<-WR[,zaxis]
    }else{
        ee<-calcCore(zaxis)
        z.data<-ee$results
        zaxis<-ee$equation
    }

    ee<-cbind(x.data,y.data,z.data)

    if(GUI){
        where<-selectSamples(print=FALSE)
        if(!getOption("gcd.shut.up")) print(where)
        ee<-ee[where,]
    }
    ii<-ee[apply(!is.na(ee),1,all),]

    x.data<-ii[,1]
    y.data<-ii[,2]
    z.data<-ii[,3]

    ee<-boxplot(z.data,plot=FALSE)
    ee<-as.vector(ee$stats)
    med<-ee[3]
    zmin<-min(z.data,na.rm=TRUE)
    zmax<-max(z.data,na.rm=TRUE)

    #if(min(z.data)<0)stop("Cannot handle negative data. Quitting...")

    # Plot legend for z
    windows(width = 9, height = 6, pointsize = 14,title=paste("Anomaly plot of ",xaxis," vs. ", yaxis," with symbols reflecting the distribution of ",zaxis,sep=""))
    nf <- layout(matrix(c(1,1,2,2,2,2,1,1,2,2,2,2), 2,6,byrow=TRUE))
    layout.show(nf)
    boxplot(z.data,notch=FALSE,col="lightgreen",main=annotate(zaxis),cex.lab=1.5,cex.main=1.5,cex.axis=1.2)
    #boxplot(log(z.data),notch=FALSE,col="lightgreen",main=annotate(zaxis))
    
    text(1.3,ee,round(ee,3))
    #text(1.3,log(ee),ee)
    
    text(1.4,zmin+(ee[1]-zmin)/2,"o",cex=1.2*scaling.big)
    text(1.4,ee[1]+(ee[2]-ee[1])/2,"o",cex=1.2*scaling.small)
    text(1.4,med,".",cex=5)
    text(1.4,ee[4]+(ee[5]-ee[4])/2,"+",cex=1.2*scaling.small)
    text(1.4,zmax-(zmax-ee[5])/2,"+",cex=1.2*scaling.big)

    
    #text(1.4,log(zmin+(ee[1]-zmin)/2),"o",cex=1.2*scaling.big)
    #text(1.4,log(ee[1]+(ee[2]-ee[1])/2),"o",cex=1.2*scaling.small)
    #text(1.4,log(med),".",cex=5)
    #text(1.4,log(ee[4]+(ee[5]-ee[4])/2),"+",cex=1.2*scaling.small)
    #text(1.4,log(zmax-(zmax-ee[5])/2),"+",cex=1.2*scaling.big)


    # Classify the data according to boxplot
    int<-c(-Inf,ee[-3],Inf)
    int[2]<-int[2]-1e-5
    int.lab<-c("oo","o",".","+","++")

    z<-cut(z.data,int,int.lab)
    #z<-cut(z.data,int)
    z<-as.character(z)
    z[is.na(z)]<-"?"

    cex<-rep(scaling.small,length(z.data))
    cex[z=="++"|z=="oo"]<-scaling.big

    pch<-z
    pch[pch=="++"]<-"+"
    pch[pch=="oo"]<-"o"
    pch[pch=="o"]<-1
    pch[pch=="+"]<-3
    pch[pch=="."]<-20
    pch<-as.numeric(pch)

    xmin<-min(x.data,na.rm=TRUE)
    xmax<-max(x.data,na.rm=TRUE)
    ymin<-min(y.data,na.rm=TRUE)
    ymax<-max(y.data,na.rm=TRUE)

    # Plot the diagram
    plot(x.data, y.data, xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab=annotate(xaxis),ylab=annotate(yaxis),cex=1.5*cex,pch=pch,col=labels[names(x.data),"Colour"],cex.lab=1.5,cex.axis=1.2)
    
    # Identify?
    if(getOption("gcd.ident"))ee<-ID(x.data,y.data)
    
    # Assign symbols?
    if(length(x.data)==nrow(WR)&GUI){
        x<-winDialog("Assign plotting symbols?",type="yesno")
        x<-(x=="YES")
    }else{
        x<-assign.symbols
    }

    if(x){
            labels$Symbol<-4
            labels[names(x.data),"Symbol"]<-pch
            ee<-rep(1,length(x.data))
            names(ee)<-names(x.data)
            ee[names(x.data)]<-cex
            labels[,"Size"]<-ee
            .assignWithNamespaceHard("labels",labels)
    }
    figaroOff()
    invisible()
}

#############################################################################
#                                                                           #
#                      BINARY COMBINED WITH BOXPLOTS                        #
#                                                                           #
#############################################################################

binaryBoxplot<-function(xaxis="",yaxis=""){
    if(xaxis=="") GUI<-TRUE else GUI<-FALSE
    if(nchar(xaxis)==0) xaxis<-selectColumnLabel(colnames(WR),message="x-axis, e.g. (SiO2+2.1)*TiO2/3\nor press ENTER to pick from a list",default="",sample.names=FALSE,silent=TRUE,empty.ok=FALSE)
    if(!is.na(as.numeric(xaxis))){
        xaxis<-colnames(WR)[xaxis]
        x.data<-WR[,xaxis]
    }else{
        ee<-calcCore(xaxis)
        x.data<-ee$results
        xaxis<-ee$equation
    }

    if(nchar(yaxis)==0) yaxis<-selectColumnLabel(colnames(WR),message="y-axis \n(press ENTER to pick from a list)",default="",sample.names=FALSE,silent=TRUE,print=FALSE,empty.ok=FALSE)
    if(!is.na(as.numeric(yaxis))){
        yaxis<-colnames(WR)[yaxis]
        y.data<-WR[,yaxis]
    }else{
        ee<-calcCore(yaxis)
        y.data<-ee$results
        yaxis<-ee$equation
    }

    ee<-cbind(x.data,y.data)
    if(GUI) where<-selectSamples(print=FALSE) else where<-rownames(WR)
    if(!getOption("gcd.shut.up")) print(where)
    
    ee<-ee[where,]
    ii<-ee[apply(!is.na(ee),1,all),]

    x.data<-ii[,1]
    y.data<-ii[,2]
    
    windows(width = 9, height = 6, pointsize = 10,title=paste("Binary boxplot of",xaxis,"vs.",yaxis))
    nf <- layout(matrix(c(1,2,2,2,2,2,1,2,2,2,2,2,1,2,2,2,2,2,1,2,2,2,2,2,3,4,4,4,4,4),5,6,byrow=TRUE))
    layout.show(nf)
    par.old<-par()$mar
    par(mar=c(1,1,1,1))
    boxplot(y.data,notch=TRUE,col="darkgreen",axes=FALSE)

    ee<-boxplot(y.data,plot=FALSE)
    ee<-as.vector(ee$stats)
    text(1.4,ee,round(ee,4))
    text(1,ee[3],annotate(yaxis),cex=2,srt=90,font=2,col="white")

    plot(x.data, y.data,xlab="",ylab="",pch=labels[names(x.data),"Symbol"],col=labels[names(x.data),"Colour"],cex=1.5*labels[names(x.data),"Size"])
    box()
    rug(ee,-0.02,side=2)
    ee<-boxplot(x.data,plot=FALSE)
    ee<-as.vector(ee$stats)
    rug(ee,-0.02,side=1)

    plot(1,1,xlab="",ylab="",type="n",axes=FALSE)

    boxplot(x.data,notch=TRUE,col="darkgreen",main="",horizontal=TRUE,axes=FALSE)

    text(ee,1.4,round(ee,4),srt=90)
    text(ee[3],1,annotate(xaxis),cex=2,font=2,col="white")
    par("mar"=par.old)
    figaroOff()
}

    # Normalizes data for a spiderdiagram
    # Parameters:
    #   sample analyses [A]
    #   normalizing values [B]

    .normalization<-function(A,B){
        C<-matrix(rep(NA,nrow(A)*length(B)),ncol=length(B))
        for (i in 1:nrow(A)){
            C[i,]<-A[i,]/B
        }
        dimnames(C)<-dimnames(A)
        return(C)
    }



#############################################################################
#                      Calculates and plots spiderdiagram                   #
#############################################################################


# spider (rock,chondrit,ymin=0,ymax=0,plot=TRUE,join=TRUE, field=FALSE, legend=TRUE,add=FALSE,density=-1,angle=0)
# Normalizes data and plots spiderdiagram
#
# Parameters:
#   analyses to be normalized [rock]
#   normalizing values [chondrit]
#   range of the diagram [ymin, ymax]
#   plot the lines + symbols [plot], if set to false, only fields can be plotted
#   shall the patterns be unbroken (even for NAs) [join]
#   shall be also shown a shaded field denoting the overall span [field]
#   plot the legend? [legend]
#     start a new plot or overplot [add]


    # shaded (A,B,col)
    # Shades an area between max and min values of the given array
    #
    # Parameters:
    #   data array [A]
    #   x axis [B]
    #   colour of the fill [colour]
    #
    .shaded<-function(A,B,col="black",density=NULL,angle=0,shaded.col="gray",plot=TRUE){
            if(nrow(A)==1){lines(1:ncol(A),A,col=col);return()}
            minimum<-apply(A,2,min,na.rm=TRUE)
            maximum<-apply(A,2,max,na.rm=TRUE)
            minimum[is.infinite(minimum)]<-NA
            maximum[is.infinite(maximum)]<-NA
            tempy1<-minimum[!is.na(minimum)]
            tempx1<-(1:length(B))[!is.na(minimum)]
            #ee<-seq(par("usr")[1],par("usr")[2],by=1)[-1]
            #tempx1<-ee[!is.na(minimum)]
            tempy2<-maximum[!is.na(maximum)]
            tempx2<-(1:length(B))[!is.na(maximum)]
            #tempx2<- ee[!is.na(maximum)]
            x<-c(tempx1,rev(tempx2),tempx1[1])
            y<-c(tempy1,rev(tempy2),tempy1[1])

            if(plot){
                if(!is.null(density)){
                    polygon(x,y,col=NULL,border=col)
                    .srafuj(x,y,col=col,angle=angle,density=density,degrees=TRUE)
                }else{
                    polygon(x,y,col=shaded.col,border=col)
                }
            return()    
            }else{
                return(list(x=x,y=y))
            }
    }


spider<-function(rock,chondrit=selectNorm(),ymin=0,ymax=0,cex=NULL,plot=TRUE,join=TRUE,field=FALSE,legend=FALSE,add=FALSE,pch=NULL,col=NULL,shaded.col="gray",density=0.02,angle=0,main="",sub="",offset=FALSE,centered=FALSE,xrotate=FALSE,xaxs="r",fill.col=TRUE,log="y",new=TRUE,...){
#spider<-function(rock,chondrit=selectNorm(),ymin=0,ymax=0,cex=1,plot=TRUE,join=TRUE,field=FALSE,legend=FALSE,add=FALSE,pch=1,col="black",shaded.col="gray",density=0.02,angle=0,main="",sub="",offset=FALSE,centered=FALSE,xrotate=FALSE,xaxs="r",fill.col=TRUE,log="y",new=TRUE,...){
#spider<-function(rock,chondrit=selectNorm(),ymin=0,ymax=0,cex=1,plot=TRUE,join=TRUE,field=FALSE,legend=FALSE,add=FALSE,pch=rep(0,times=length(rock)+1),col=rep("black",times=length(rock)+1),shaded.col="gray",density=-1,angle=0,main="",sub="",offset=FALSE,centered=FALSE,xrotate=FALSE,xaxs="r",fill.col=TRUE,log="y",new=TRUE,...){
    on.exit(options("show.error.messages"=TRUE))
    rockName<-as.character(match.call()$rock)
    # If plotting from standard GCDkit

    if(rockName=="WR"){
        if(is.null(pch))pch<-labels[,"Symbol"]
        if(is.null(col))col<-labels[,"Colour"]
        if(is.null(cex))cex<-labels[,"Size"] 
    }else{
        if(is.null(pch))pch<-0
        if(is.null(col))col<-"black"
        if(is.null(cex))cex<-1
    }
    
    if(length(pch)==1 & !is.vector(rock)) pch<-rep(pch,nrow(rock)) 
    if(length(col)==1 & !is.vector(rock)) col<-rep(col,nrow(rock))
    if(length(cex)==1 & !is.vector(rock)) cex<-rep(cex,nrow(rock))
    
    # Truncating extra pch, col and cex information
    if(is.vector(rock)){
        nn<-1
    }else{
        nn<-nrow(rock) # NEW
        names(pch)<-rownames(rock) # NEW
        names(col)<-rownames(rock) # NEW
        names(cex)<-rownames(rock) # NEW
    }
    if(length(pch)> length(y.data)) pch<-pch[1:nn] # NEW
    if(length(col)> length(y.data)) col<-col[1:nn] # NEW
    if(length(cex)> length(y.data)) cex<-cex[1:nn] # NEW
       
    #if(is.character(rock)&length(rock)==1)rock<-WR[,unlist(strsplit(rock,","))]
    #if(is.character(rock))rock<-WR[,rock]

    # spiderplot (A,B,model,ymin,ymax,add)
    # Plots spiderdiagram
    #
    # Parameters:
    #   normalized values [A]
    #   normalizing values [B]
    #   range of the diagram [ymin, ymax]
    #
    spiderplot<-function(A,B,ymin,ymax,add,main="",xaxs="r",new=TRUE,log="y",pty="p",...){
         on.exit(options("show.error.messages"=TRUE))
         #ee<-sys.call()
         #cat("GCDkit->",ee,"\n")
         #.save2hist(ee)
         
         model<-rownames(B)
         if(main==""){
            main<-"Spider plot"
            if(!is.null(model)) main<-paste(main,"-",model)
         } 
         
         options(show.error.messages=FALSE)
         model1<-try(strsplit(model,"\\(")[[1]][1])
         options(show.error.messages=TRUE)
             if (add==FALSE){
                if(ymin==0&ymax==0){
                    ymin<-min(A[A>0],na.rm=TRUE) #NEW
                    ymax<-max(A[A>0],na.rm=TRUE) #NEW
                    if(log=="y"){
                        ymin<-10^floor(log(ymin,10))
                        ymax<-10^ceiling(log(ymax,10))
                    }
             
                    yy<-winDialogString("Enter min and max for y-axis, separated by commas",paste(ymin,ymax,sep=","))
                    if(is.null(yy)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop(call. = TRUE)}
                    ee<-unlist(strsplit(yy,","))
                    if(length(ee)<=1) {winDialog("ok","Invalid limits!");options("show.error.messages"=FALSE);stop(call. = TRUE)}
                    ymin<-as.numeric(ee[[1]])
                    ymax<-as.numeric(ee[[2]])
                    if(ymin<=0 | ymax<=0 | ymin>=ymax) {winDialog("ok","Invalid limits!");options("show.error.messages"=FALSE);stop(call. = TRUE)}
                }
                
                if(new){
                    windows(width = 8, height = 6.5, pointsize = 12, title=model)
                }
                
                if(legend){
                    par(omd=c(0,1,0,1))
                    par(mar=c(6,4,6,11))
                    xcex<-0.8
                }else{
                    xcex<-1-0.015*length(colnames(A))
                }
                #ymin<-10^floor(log(ymin,10))
                #ymax<-10^ceiling(log(ymax,10))
                
                lbs<-sapply(colnames(A),annotate)
                
                if(screen()){  # For plates
                    xcex<-1.1*xcex
                    #cex<-1.2*cex
                }     
                # Axes being formed
                    # Various styles of x axis
                    if(xrotate){
                        las<-2
                        adj=1
                        padj=0.5
                    }else{
                        las<-0
                        adj=0.5
                        padj=1
                    }
                    at<-1:length(B)                    
                    if(centered){
                        at<-c(at,length(at)+1)
                        lbs<-c(lbs,"")
                    } 
                    delta<-centered*0.5
                    # Shift even and odd labels?
                    if(offset){ # Yes
                        odd<-seq(1,length(at),by=2)
                        even<-seq(2,length(at),by=2)
                        axis1<-list("axis",side=1,at=at-delta,labels=FALSE,cex.axis=xcex,padj=1,hadj=0.5,xaxs=xaxs,lty="blank")
                        if(centered){
                            rug1<-list("rug",x=at-delta,ticksize=-0.02,side=1,lwd=1,col="black")
                            if(xrotate){
                                mtext1<-list("mtext",side=1,at=at[odd],text=lbs[odd],cex=xcex,line=-0.7,las=las,adj=0,padj=padj)
                                mtext2<-list("mtext",side=1,at=at[even],text=lbs[even],cex=xcex,line=1,las=las,adj=1,padj=padj)
                            }else{
                                mtext1<-list("mtext",side=1,at=at[odd],text=lbs[odd],cex=xcex,line=-1.5,las=las,adj=adj,padj=0)
                                mtext2<-list("mtext",side=1,at=at[even],text=lbs[even],cex=xcex,line=-0.3,las=las,adj=adj,padj=1)
                            }
                            temp1<-list(axis1=axis1,mtext1=mtext1,mtext2=mtext2,rug1=rug1)
                        }else{
                            rug1<-list("rug",x=odd,ticksize=0.02,side=1,lwd=1,col="black")
                            rug2<-list("rug",x=even,ticksize=-0.02,side=1,lwd=1,col="black")
                            if(xrotate){
                                mtext1<-list("mtext",side=1,at=at[odd],text=lbs[odd],cex=xcex,line=-0.7,las=las,adj=0,padj=padj)
                                mtext2<-list("mtext",side=1,at=at[even],text=lbs[even],cex=xcex,line=1,las=las,adj=1,padj=padj)
                            }else{
                                mtext1<-list("mtext",side=1,at=at[odd],text=lbs[odd],cex=xcex,line=-1.5,las=las,adj=adj,padj=0)
                                mtext2<-list("mtext",side=1,at=at[even],text=lbs[even],cex=xcex,line=-0.3,las=las,adj=adj,padj=1)
                            }
                            temp1<-list(axis1=axis1,mtext1=mtext1,mtext2=mtext2,rug1=rug1,rug2=rug2) 
                        }
                       
                    # No offset
                    }else{
                        axis1=list("axis",side=1,at=at-delta,labels=FALSE,cex.axis=xcex,padj=1,hadj=0.5,xaxs=xaxs,lty="solid")
                        mtext1<-list("mtext",side=1,at=at,text=lbs,cex=xcex,line=1.2*xrotate-delta,las=las,adj=adj,padj=padj)
                        temp1<-list(axis1=axis1,mtext1=mtext1)
                    }
                        
                    # y axis is relatively straightforward
                    if(log=="y"){
                        #Axis
                        at<-10^(ceiling(log10(ymin)):floor(log10(ymax)))
                        axis2<-list("axis",side=2,at=at,labels=at,cex.axis=xcex,padj=0.5,hadj=0.5,lty="solid",log="y")
                        
                        # Minor ticks
                        at<-axTicks(2,axp = c(10^floor(log10(ymin)),10^ceiling(log10(ymax)),1), usr = c(floor(log10(ymin)),ceiling(log10(ymax))),log = TRUE, nintLog =Inf)
                        #at<-axTicks(2,axp = c(ymin,ymax,1), usr = c(floor(log10(ymin)),ceiling(log10(ymax))),log = TRUE) # FIXED BY VJ
                        at<-as.vector(apply(t(at),2,function(i)i*1:10))
                        rug3<-list("rug",x=at,ticksize=0.015,side=2,lwd=1,col="black")
                    }else{
                        at<-pretty(A)
                        axis2<-list("axis",side=2,cex.axis=xcex,padj=0.5,hadj=0.5,lty="solid",log="")
                        at<-pretty(c(ymin,ymax),50)
                        rug3<-list("rug",x=at,ticksize=0.015,side=2,lwd=1,col="black")          
                    }
                    temp2<-list(axis2=axis2,rug3=rug3)
                    temp<-c(temp1,temp2)                
                    
                    # Setup the Figaro template
                    
                    if(!new){
                    #if(screen()|!new){ # Plate has to have opaque background
                        new<-FALSE
                        bg<-"white"
                    }else{
                        new<-TRUE
                        try(close.screen(screen(n), all.screens = TRUE)) # VERY NEW
                        bg<-"transparent"
                    }
                    
                    sheet<-list(demo=list(fun="plot",call=list(xlim=c(1,length(colnames(A))),ylim=c(ymin,ymax),xlab=" ",ylab=paste("Sample/",model1),type="n",axes=FALSE,log=log,xaxs=xaxs,yaxs="i",main=annotate(main),col.main="black",col.sub="black",cex.main=par("cex.main"),cex.sub=par("cex.sub"),new=new),template=temp))             
                    sheet$demo$template$GCDkit$plot.type<-"spider"
                    sheet$demo$template$GCDkit$plot.name<-model
                    sheet$demo$call$new<-TRUE

                    # Gridlines
                    if(log=="y"){
                        at<-10^(ceiling(log10(ymin)):floor(log10(ymax)))
                        #at<-axTicks(2,axp = c(ymin,ymax,1), usr = c(floor(log10(ymin)),ceiling(log10(ymax))),log = TRUE)
                        ee<-which(at%in%ymin)
                        if(length(ee)>0) at<-at[-ee]
                        
                        ee<-which(at%in%ymax)
                        if(length(ee)>0) at<-at[-ee]
                        
                        at<-at[at!=1]
                        sheet$demo$template$grid<-list("abline",h=at,col="black",lty="dotted")
                        sheet$demo$template$grid1<-list("abline",h=1,col="black")  
                    }
                    #Box around
                    sheet$demo$template$box<-list("box",which = "plot",col="black",lwd=1)
                    assign("sheet",sheet,.GlobalEnv) # NEW
           }
       
             if (field){
                    if(add & screen()){  
                     if(is.null(names(plate[[screen()]]))) screen(screen()-1)     
                    }
             
                   if (fill.col){
                        #Plot field to portray the overall variability, if shaded.col is not NULL
                        ee<-.shaded(A,B,plot=FALSE) # Get the coordinates but plot nothing 
                        # Add the polygon as the first in the template, so that everything is plotted on the top
                        nn<-length(grep("field",names(sheet$demo$template)))+1
                        eval(parse(text=paste("sheet$demo$template<-c(list(field",nn,"=list(\"polygon\",x=ee$x,y=ee$y,col=shaded.col,border=shaded.col)),sheet$demo$template)",sep=""))) 
                        assign("sheet",sheet,.GlobalEnv) # NEW
                        pp<-figaro(demo,prefix="sheet")
                        assign("pp",pp,.GlobalEnv) # NEW
                        #x.data<<-A
                        #y.data<<-B
                        x.data<-1:ncol(A)
                        assign("x.data",x.data,.GlobalEnv) # NEW
                        y.data<-A
                        assign("y.data",y.data,.GlobalEnv) # NEW
                        pp$draw(1,1,xlab=" ",ylab=paste("Sample/",model1),main=annotate(main),sub=annotate(sub),cex=cex,axes=FALSE,log=log)            
                   }
                    
                    if (!fill.col){
                        if(!add){
                            pp<-figaro(demo,prefix="sheet")
                            assign("pp",pp,.GlobalEnv) # NEW
                            #x.data<<-A
                            #y.data<<-B
                            pp$draw(1,1,xlab=" ",ylab=paste("Sample/",model1),main=annotate(main),sub=annotate(sub),cex=cex,axes=FALSE,log=log)            
                            figaroOff()
                            x.data<-1:ncol(A)
                            assign("x.data",x.data,.GlobalEnv) # NEW
                            y.data<-A
                            assign("y.data",y.data,.GlobalEnv) # NEW
                        }
                    ee<-.shaded(A,B,shaded.col=shaded.col,density=density,col=col,angle=angle) #shading so far is not Figaro compatible
             }              
             plot<-FALSE
             
            }

            if(plot){
                if(!add){
                    for (i in 1:nrow(A)){
                        eval(parse(text=(paste("sheet$demo$template$points",i,"<-list(\"points\",x=1:ncol(A),y=A[",i,",],col=col[",i,"],pch=pch[",i,"],cex=cex[",i,"])",sep=""))))
                        tempy<-A[i,][!is.na(A[i,])]
                        tempx<-(1:length(B))[!is.na(A[i,])]
                        if (join){
                          eval(parse(text=(paste("sheet$demo$template$lines",i,"<-list(\"lines\",x=tempx,y=tempy,col=col[",i,"])",sep=""))))
                        }else{
                          eval(parse(text=(paste("sheet$demo$template$lines",i,"<-list(\"lines\",x=A[",i,",],col=col[",i,"])",sep=""))))
                        }
                    }
    
                    # Plot it
                    pp<-figaro(demo,prefix="sheet")
                    assign("pp",pp,.GlobalEnv) # NEW
                    x.data<-1:ncol(A)
                    assign("x.data",x.data,.GlobalEnv) # NEW
                    y.data<-A
                    assign("y.data",y.data,.GlobalEnv) # NEW
                    # NEW
                    pp$draw(1,1,xlab=" ",ylab=paste("Sample/",model1),main=annotate(main),sub=annotate(sub),cex=cex,axes=FALSE,log=log,bg=bg)
                    sheet$demo$call$sub<-annotate(sub)
                    
                    
                    # Legend
                    if(legend){
                        x<-par("usr")[2]+0.5
                        y <- 10^(diff(par("usr")[3:4])/2+par("usr")[3]) # Fixed by Jeff
                        #y<-10^(diff(par("usr")[3:4])/2)
                        which<-apply(y.data,1,function(i)!all(is.na(i)))
                        sheet$demo$legend<-list(x=x,y=y,legend=rownames(y.data)[which],col=col[which],pch=pch[which],cex=0.7,pt.cex=cex[which],lty="solid",bg="transparent",ncol=2,bty="o",xpd=TRUE,yjust=0.5)
                    }
                    assign("sheet",sheet,.GlobalEnv) 
                    #assign("leg.col",NA,.GlobalEnv)  # NEW
                    figRedraw()
                }else{ #adding
                    for (i in 1:nrow(A)){
                        points(A[i,],col=col[i],pch=pch[i],type="p",cex=cex)
                        tempy<-A[i,][!is.na(A[i,])]
                        tempx<-(1:length(B))[!is.na(A[i,])]
                        if(join){
                            lines(tempx,tempy,col=col[i])
                        }else{
                            lines(A[i,],col=col[i],pch=pch[i])
                        }
                    }
                }

         }
         # If plotting in a plate
                    if(screen()){  
                        ee<-screen()
                        options(show.error.messages=FALSE)    
                        .saveCurPlotDef(ee)
                        scr.old<<-ee
                        if(ee<length(plate.data)) screen(ee+1,new=FALSE)
                        options(show.error.messages=TRUE)  
                    }else{
                        if(.Platform$GUI=="Rgui") figaroOn()
                    }
    }
    ###############################
    # Main spider function - body #
    ###############################
    if(is.character(chondrit)) chondrit<-selectNorm(chondrit)
    
    if (is.vector(rock))rock<-t(as.matrix(rock))
        rock<-rbind(rock[1,],rock)
        model<-rownames(chondrit)
        rough<-matrix(nrow=nrow(rock),ncol=1)
        for (f in seq(1,length(chondrit))){
            found<-as.numeric(1)
            found<-rock[,colnames(rock)==colnames(chondrit)[f]]
                if (length(found)>1){
                    rough<-cbind(rough,found)
                }else{
                    rough<-cbind(rough,rep(NA,nrow(rock)))
                }
        }
        rough<-rough[,-1]
        rough<-rough[-1,]
    if (is.vector(rough)){
        rough<-t(as.matrix(rough))
    }
    colnames(rough)<-colnames(chondrit)
    rownames(rough)<-rownames(rock)[-1]
    
    rough[rough==0]<-NA
    if(!getOption("gcd.shut.up")){
        #cat("Original data:","\n")
        #print(rough)
    }
    
    Y<-.normalization(rough,chondrit)
    if(plot)spiderplot(Y,chondrit,ymin,ymax,add=add,main=main,cex=cex,field=field,plot=plot,new=new,offset=offset, xrotate=xrotate, centered=centered, xaxs=xaxs, log=log, sub=sub)
    colnames(Y)<-paste(colnames(Y),"N",sep="")
    
    #Specific for REE
    if(!is.null(model)){
        if(substr(model,1,3)=="REE"){
            Eu<-Y[,"EuN"]/sqrt(Y[,"SmN"]*Y[,"GdN"])
            Y<-cbind(Y,Eu,
                Y[,"LaN"]/Y[,"YbN"],
                Y[,"LaN"]/Y[,"SmN"],
                Y[,"CeN"]/Y[,"YbN"],
                Y[,"CeN"]/Y[,"SmN"],
                Y[,"EuN"]/Y[,"YbN"]
            )   
            rock<-rock[-1,] # REE
            
            # Check whether all REE are present, otherwise sum of REE makes no sense
            if(!is.vector(rock)){
                if(all(REE%in%colnames(rock))){
                    sum.ree<-apply(rock[,REE],1,sum,na.rm=TRUE)
                }else{
                    sum.ree<-rep(NA,times=nrow(rock))
                }
            }else{
                if(all(REE%in%names(rock))){
                    sum.ree<-sum(rock[REE],na.rm=TRUE)
                }else{
                    sum.ree<-NA
                }
            }
            sum.ree[sum.ree==0]<-NA
            Y<-cbind(Y,sum.ree)
            colnames(Y)[(ncol(chondrit)+1):(ncol(Y))]<-c("Eu/Eu*","LaN/YbN","LaN/SmN","CeN/YbN","CeN/SmN","EuN/YbN","Sum_REE")
        }
    }
    
    if(getOption("gcd.menus")!=""& plot & !screen()) figaroOn()
    #if(.Platform$GUI=="Rgui" & plot & !screen()) figaroOn()
    assign("results",Y,.GlobalEnv) # NEW
    invisible(Y)
}


bpplot2<-function (x, main = "Box-Percentile Plot", sub="",xlab = "",ylab = "", log="y",col="lightgray",horizontal=FALSE,ylim=NULL,axes=TRUE,...){
    on.exit(options("show.error.messages"=TRUE))
    bpx<-function (y, offset){
        on.exit(options("show.error.messages"=TRUE))
        y <- y[!is.na(y)]
        n <- length(y)
        delta <- 1/(n + 1)
        prob <- seq(delta, 1 - delta, delta)
        quan <- sort(y)
        med <- median(y,na.rm=TRUE)
        q1 <- median(y[y < med],na.rm=TRUE)
        q3 <- median(y[y > med],na.rm=TRUE)
        if(is.na(q1)) q1<-med #VJ
        if(is.na(q3)) q3<-med #VJ
        first.half.p <- prob[quan <= med]
        second.half.p <- 1 - prob[quan > med]
        plotx <- c(first.half.p, second.half.p)
        
        options(show.error.messages=FALSE)
        qx <- try(approx(quan, plotx, xout = q1)$y)
        if(class(qx)=="try-error") qx<-0.25
        q1.x <- c(-qx, qx) + offset
        qx <- try(approx(quan, plotx, xout = q3)$y)
        if(class(qx)=="try-error") qx<-0.25
        options(show.error.messages=TRUE)
        
        q3.x <- c(-qx, qx) + offset
        q1.y <- c(q1, q1)
        q3.y <- c(q3, q3)
        med.x <- c(-max(first.half.p), max(first.half.p)) + offset
        med.y <- c(med, med)
        return(list(x1 = (-plotx) + offset, y1 = quan, x2 = plotx + 
            offset, y2 = quan, q1.y = q1.y, q1.x = q1.x, q3.y = q3.y, 
            q3.x = q3.x, med.y = med.y, med.x = med.x))
    }
   
    all.x <<- list(x)
    nam <- character(0)
    if (is.list(all.x[[1]])) {
        all.x <- all.x[[1]]
        #if (is.logical(name) && name) 
        #    name <- names(x)
    }
    n <- length(all.x)
    centers <<- seq(from = 0, by = 1.2, length = n)
    
    if(is.null(ylim)){
        ymax <- max(sapply(all.x, max, na.rm = TRUE))
        ymin <- min(sapply(all.x, min, na.rm = TRUE))
    }else{
        ymin<-ylim[1]
        ymax<-ylim[2]
    }
    xmax <- max(centers) + 0.7
    xmin <- -0.7
    if(horizontal){
        plot( c(ymin, ymax),c(xmin, xmax), type = "n", main = annotate(main), sub=sub,  
            xlab = ylab, ylab = xlab, yaxt = "n",log=log,ylim=c(xmin,xmax),xaxs="i",yaxs="i",axes=axes)
    }else{
        plot(c(xmin, xmax), c(ymin, ymax), type = "n", main = annotate(main), sub=sub,  # Replace ymin ymax with desired y axis range
            xlab = xlab, ylab = ylab, xaxt = "n",log=log,xlim=c(xmin,xmax),xaxs="i",yaxs="i",axes=axes)
    }
    
    med.x<-NULL
    med.y<-NULL
    for (i in 1:n) {
       if(!all(is.na(all.x[[i]]))){ 
        plot.values <- bpx(all.x[[i]], centers[i])
        if(horizontal){
            polygon(c(plot.values$y1,rev(plot.values$y2)),c(plot.values$x1,rev(plot.values$x2)),col=col)
            lines(plot.values$q1.y,plot.values$q1.x)
            lines(plot.values$q3.y,plot.values$q3.x)
            lines(plot.values$med.y,plot.values$med.x)
        }else{
            polygon(c(plot.values$x1,rev(plot.values$x2)),c(plot.values$y1,rev(plot.values$y2)),col=col)
            lines(plot.values$q1.x, plot.values$q1.y)
            lines(plot.values$q3.x, plot.values$q3.y)
            lines(plot.values$med.x, plot.values$med.y)
        }
        med.x<-c(med.x,mean(plot.values$med.x))
        med.y<-c(med.y,mean(plot.values$med.y))
        }
    }  
     if(n!=1){ # TODO ALSO FOR HORIZONTAL
        axis(1,seq(-0.2,xmax-0.7,(xmax-xmin)/(ncol(x))),colnames(x))
        lines(med.x,med.y,col="darkblue",lwd=2)
     }
}


spiderBoxplot<-function(norm=NULL,which=rep(TRUE,nrow(WR)),doublenorm=FALSE,norm2="",ymin=NULL,ymax=NULL,bpplot=TRUE,col="lightgray",log=TRUE){
    on.exit(options("show.error.messages"=TRUE))
    GUI<-is.null(norm)
    if(log) log<-"y" else log<-""
    # Select the normalization scheme
    chondrit<-selectNorm(norm)    
    model<-rownames(chondrit)
    temp<-filterOut(WR,colnames(chondrit))
    
    # Select the samples to be plotted
    if(GUI){
        # Select the samples to be plotted
        i<-rownames(WR)[which]%in%rownames(temp)
        y1<-selectSubset(where=cbind(labels[i,],WR[i,]),save=FALSE,GUI=TRUE)
        if(is.null(y1)){cat("Cancelled.\n");options("gcd.ident"=0);return()}
    }else{
        i<-rownames(temp)%in%rownames(WR)[which]
        y1<-rownames(temp)[i]
    }
    
    methods<-c("boxplot","box and percentile plot")
    if(GUI){
        # Select the plotted diagram
        ee<-select.list(methods,preselect="boxplot",title="Select the diagram to be plotted")
        bpplot<-ee=="box and percentile plot"
    }else{
        if(bpplot) ee<-"box and percentile plot" else ee<-"boxplot"
    }
    
    A<-temp[y1,]
    A[A==0]<-NA
    print(y1)

    normalized<-data.frame(.normalization(A,chondrit))
    rownames(normalized)<-y1
    
    if(doublenorm){
        if(all(colnames(temp)!=norm2)){
            norm2<-selectColumnLabel(colnames(normalized), message = "Select a variable for double normalization",default = colnames(temp)[ncol(temp)], sample.names = FALSE, silent = TRUE, empty.ok = FALSE)
            norm2<-colnames(normalized)[norm2]
        }
    
        normalized<-normalized[!is.na(normalized[,norm2]),]
        normalized<-normalized/rep(normalized[,norm2],ncol(normalized))
    
        chondrit<-rep(1,length(chondrit))
        chondrit<-t(as.matrix(chondrit))
        colnames(chondrit)<-colnames(normalized)    
        rownames(chondrit)<-paste(model, " and ", norm2, "[N]=1",sep="")
    }
    
    model<-rownames(chondrit)
    which<-y1%in%rownames(normalized)    
    if(bpplot){
        main<-paste("Normalized by",model)
    }else{
        main<-paste("Normalized by",model)
    }
    
    #Specific for REE
    if(substr(model,1,3)=="REE"){
        Eu<-normalized[,"Eu"]/sqrt(normalized[,"Sm"]*normalized[,"Gd"])
        ratio1<-(normalized[,"Ce"]/normalized[,"Yb"])
        ratio2<-(normalized[,"Ce"]/normalized[,"Sm"])
        ratio3<-(normalized[,"Eu"]/normalized[,"Yb"])
        sum.ree<-apply(A,1,sum,na.rm=TRUE)
        names<-c("Eu/Eu*",parse(text="Ce[N]/Yb[N]"),parse(text="Ce[N]/Sm[N]"),parse(text="Eu[N]/YbN"))
        windows(width = 8, height = 6.5, pointsize = 10)
        boxplot(data.frame(cbind(Eu,ratio1,ratio2,ratio3)),names=names,sub=annotate(selected),notch=TRUE,col="lightgray",main=annotate(main))
    }
   if(!doublenorm){
        model<-paste("Normalized by",model)
    }
    if(!getOption("gcd.shut.up")) cat("\n",model,":","\n",sep="")
   
   #Range
   if(is.null(ymin)) ymin<-min(normalized[normalized>0],na.rm=TRUE) #NEW
   if(is.null(ymax)) ymax<-max(normalized[normalized>0],na.rm=TRUE) #NEW
   if(ymin<=0 | ymax<=0 | ymin>=ymax) {winDialog("ok","Invalid limits!");options("show.error.messages"=FALSE);stop(call. = TRUE)}
   
   tit<-paste(toupper(substr(ee,1,1)),substr(ee,2,nchar(ee))," ",tolower(substr(main,1,1)),substr(main,2,nchar(main)),sep="")
          
   windows(width = 8, height = 6.5, pointsize = 10, title=tit)   
    if(bpplot){
        bpplot2(normalized,sub=annotate(selected),main=annotate(main),xlab="", ylab="", log=log,col=col,ylim=c(ymin,ymax),axes=FALSE)
    }else{
        boxplot(normalized,sub=annotate(selected),main=annotate(main),notch=FALSE,col=col,sub=model,log=log,ylim=c(ymin,ymax),yaxs="i",axes=FALSE)
    }
    
    # X axis
    if(!bpplot){
        axis(1,1:ncol(normalized),labels=colnames(normalized))
    }
    if(doublenorm){
        colnames(normalized)<-paste(colnames(normalized),"N",sep="")
    }
   
    res<-statistics(normalized,colnames(normalized),boxplots=FALSE,histograms=FALSE,print.table=FALSE)
    res<-unlist(res[[1]])
    ee<-res[,"50%"]
    y<-ee[!is.na(ee)]
    if(!bpplot) lines((1:length(chondrit))[!is.na(ee)],y,lwd=1.5,col="darkblue") 
    
    # Major ticks
    #at<-axTicks(2,axp = c(ymin,ymax,1), usr = c(floor(log10(ymin)),ceiling(log10(ymax))),log = TRUE)
    #ee<-which(at%in%ymin)
    #if(length(ee)>0) at<-at[-ee]
    #ee<-which(at%in%ymax)
    #if(length(ee)>0) at<-at[-ee]
                        
    # Y axis
    at<-10^(-10:10)
    axis(side=2,at=at,labels=at,padj=0.5,hadj=0.5,lty="solid",log="y")
    
    #Gridlines
    at<-at[at!=1]
    abline(h=at,col="black",lty="dotted")
    abline(h=1,col="black")
    
    # Minor ticks
    at<-axTicks(2,axp = c(10^floor(log10(ymin)),10^ceiling(log10(ymax)),1), usr = c(floor(log10(ymin)),ceiling(log10(ymax))),log = TRUE, nintLog =Inf)
    
    #at<-axTicks(2,axp = c(ymin,ymax,1), usr = c(floor(log10(ymin)),ceiling(log10(ymax))),log = TRUE)3 Fixed by VJ
    at<-as.vector(apply(t(at),2,function(i)i*1:10))
    rug(x=at,ticksize=0.015,side=2,lwd=1,col="black")
    box()         
    figaroOff()
    if(!getOption("gcd.shut.up")) print(res) else (cat("Type 'results' to see the overall statistics\n"))
    assign("results",res,.GlobalEnv)
    invisible()
}



#############################################################################
#                                                                           #
#                   Reservoirs & Figure overplotting                        #
#                                                                           #
#############################################################################

# Get ready names of atoms from the oxide names
.atoms.from.formula<-function(oxides,valency=FALSE){
    z<-gsub("[0-9]","",oxides)                        # Remove numbers                        
    z<-sapply((strsplit(z,"O")),paste,collapse="")    # Remove oxygen's "O" and anything beyond
    if(valency){
        v<-.valency(oxides)
        v[is.na(v)]<-""
        z<-paste(z,v,sep="")
    }
    return(z)
}

.formula.reader<-function(formula){
    ee<-strsplit(paste(formula," ",sep=""),"O")
    n<-as.numeric(gsub("[a-zA-Z]","",ee[[1]]))
    n[is.na(n)]<-1
    if(length(n)==1)n[2]<-0   
    atom<-gsub("[0-9 ]","",ee[[1]][1])
    names(n)<-c(atom,"O")
    out<-list(atoms.n=n,atom=atom)
    return(out)
}

oxide2ppm<-function(formula,where="WR"){
    where<-get(where,envir = parent.frame())
    
    form.read<-.formula.reader(formula)  
    n<-form.read$atoms.n
    atom<-form.read$atom
    
    # Recalculation factor
    fact<-n[1]*mw[atom]/(n[1]*mw[atom]+n[2]*mw["O"])*1e4
    #cat("Factor is:",fact,"\n")
    
    # Do the calculation
    z<-matrix(where[,formula]*fact,ncol=1)
    rownames(z)<-rownames(where)
    colnames(z)<-atom
    return(z)
}

ppm2oxide<-function(formula,where="WR"){
    where<-get(where,envir = parent.frame())
    
    form.read<-.formula.reader(formula)  
    n<-form.read$atoms.n
    atom<-form.read$atom
    
    # Recalculation factor
    fact<-(n[1]*mw[atom]+n[2]*mw["O"])/(n[1]*mw[atom])/1e4
    #cat("Factor is:",fact,"\n") 
    
    # Do the calculation
    z<-matrix(where[,atom]*fact,ncol=1)
    rownames(z)<-rownames(where)
    colnames(z)<-formula
    #print(cbind(where[,c(atom,formula)],round(z,3))) # Testing
    return(z)
}

# Returns a recalculation factor for recalculation between two different oxides of the same element
oxide2oxide<-function(formula1,formula2){
    #where<-get(where,envir = parent.frame()) # NEW
    mw1<-molecularWeight(formula1)
    mw2<-molecularWeight(formula2)
    fact<-mw2["MW"]/mw1["MW"]*mw1["x.atoms"]/mw2["x.atoms"]
    names(fact)<-NULL
    return(fact)
}

# Get data ready from "reservoirs.data"
.figAddReservoirsLoad<-function(){ 
    on.exit(options("show.error.messages"=TRUE))
    normal<-scan(paste(gcdx.dir,"/reservoirs.data",sep=""),skip=1,what=list(mname="",elements="",nvalues=""),sep="\n",quiet=TRUE)
    elems<-sapply(normal$elements,strsplit,split=",")
    vals<-sapply(normal$nvalues,strsplit,split=",")
    vals<-sapply(vals,as.numeric)
    nams<-normal$mname
     
    data<-sapply(1:length(nams),function(i){
        ee<-vals[[i]]
        names(ee)<-elems[[i]]
        ee<-as.matrix(ee)
        colnames(ee)<-nams[[i]]
        return(ee)
    })
    
    z<-data[[1]]
    for(i in 2:length(nams)){
        z<-merge(z,data[[i]],by.x=0,by.y=0,sort=FALSE,all=TRUE)
        rownames(z)<-z[,1]
        z<-z[,-1]
    }
    z<-t(z)
    
    # Recalc all  atoms in ppm to oxides in wt. %
    options(show.error.messages=FALSE)
    ox.list<-c("SiO2","TiO2","Al2O3","FeOt","FeO","MnO","MgO","CaO","Na2O","K2O","P2O5")
    ox<-sapply(ox.list,function(i){
        out<-ppm2oxide(i,"z")
        ee<-addOn(i,out,where=z)
        assign("z",ee,envir=parent.env(environment()))
        return(out)
    })
    return(z)
}

# Get data ready from "idealmins.data"
.figAddReservoirsMinsLoad<-function(){ 
    on.exit(options("show.error.messages"=TRUE))
    z<-read.table(paste(gcdx.dir,"\\","idealmins.data",sep=""), sep="\t")
    z<-data.matrix(z)
    z[is.na(z)]<-0
    
    # Append FeOt
    options(show.error.messages=FALSE)
    try(z<-addOn("FeOt",z[,"FeO"]+z[,"Fe2O3"]*0.89981,where=z))
    
    # Recalc all oxides in wt. % to atoms in ppm
    ox.list<-c("SiO2","TiO2","Al2O3","FeOt","MnO","MgO","CaO","Na2O","K2O","P2O5")
    ppm<-sapply(ox.list,function(i){
        out<-oxide2ppm(i,"z")
        return(out)
    })
    
    # Names of atoms, Fet becomes Fe
    if(length(ppm)!=0){
            atom.names<-.atoms.from.formula(ox.list)
            atom.names[atom.names=="Fet"]<-"Fe"
            colnames(ppm)<-atom.names
            rownames(ppm)<-rownames(z)
    }
    return(cbind(z,ppm))
}

# Still TODO
# Get data ready from "debon.ideal.data" - idealized average compositions of various rcok types
.figAddReservoirsDebonLoad<-function(){ 
    on.exit(options("show.error.messages"=TRUE))
    normal<-read.table(paste(gcdx.dir,"/debon.ideal.data",sep=""),sep="\t")
    FeOt<-normal[,"Fe2O3"]*0.89981
    P2O5<-NA
    z<-cbind(normal,FeOt,P2O5)
    #mil<-millications(normal)
    #z<-DebonCalc(mil)
    return(z)
}

# Calculate data using several formulae, return only samples for which all values are not NA
.getRelevantReservoirs<-function(formulae,data){
        on.exit(options("show.error.messages"=TRUE))
        
        options("show.error.messages"=FALSE)
        # Calculate the data using these formulae
        z<-try(sapply(formulae,function(i){          
            z<-calcCore(paste(text=i),where="data",redo=FALSE)$results
            return(z)
        },simplify=TRUE))
        if(class(z)=="try-error"|length(z)==0) return(NULL)
        
        # Select only reservoirs where both axes are available
        options("show.error.messages"=FALSE)
        ee<-try(apply(z,1,function(i)all(!is.na(i))))
        if(class(ee)=="try-error"|length(ee)==0) return(NULL)
        
        z<-z[ee,,drop=FALSE]
        if(length(z)==0)return(NULL)
        return(z)
}

#############################################################################
#                                                                           #
#       Overplot geochemical data for geochemical reservoirs/minerals       #
#                var.name = text string (embedded into Figaro)              #
#                                                                           #
#############################################################################
figAddReservoirs<-function(autoscale=FALSE, var.name=NULL, sample.names=NULL, reserv.condition=NULL, labs=NULL, pch="*", col="darkred", cex=1, type="p",...){
   on.exit(options("show.error.messages"=TRUE))
   arguments<-list(...)
   # If no variable name is specified, select from list either the reservoirs or ideal mins data

   if(is.null(var.name)){
        where<-c("reservoirs.data","idealmins.data")
        #where<-c("reservoirs.data","idealmins.data","debon.ideal.data") # Debon not to be offered to public yet
        var.name<-select.list(where,multiple=FALSE)
        if(length(var.name)==0){cat("Cancelled.\n");return()}
    }
    
    # Test whether variable specified by var.name exists in the GlobEnv, stop if not
    # Auxiliary function for switch below
    test.fun<-function(){
                ee<-ls(.GlobalEnv,pattern=paste("^",var.name,"$",sep=""))
                if(length(ee)==0){
                    #if(var.name=="")return(NULL)
                    cat("Unknown variable name with data for overplotting! Quitting....\n",sep="")
                    options(show.error.messages=FALSE)
                    stop(call. = TRUE)
                }else{
                    z<-get(var.name,.GlobalEnv)
                    return(z)
                }
    }
      
    # Call appropriate function to provide the data matrix
    #if(var.name!=""){
        mat<-switch(var.name,
            reservoirs.data=.figAddReservoirsLoad(),
            idealmins.data=.figAddReservoirsMinsLoad(),
            debon.ideal.data=.figAddReservoirsDebonLoad(),
            get(var.name,.GlobalEnv) # else, i.e. a global variable name was specified
        )
    #}
    
    # Select samples
    if(is.null(sample.names)&is.null(reserv.condition)){
        sample.names<-rownames(mat)
        #sample.names<-select.list(rownames(mat),multiple=TRUE)
        #if(length(sample.names)==0){cat("Cancelled.\n");return()}
    }

    # mat is a matrix with data for all reservoirs available for overplotting
    if(var.name=="debon.ideal.data"){    
        arguments<-list(overplot.dataset=mat[sample.names,],labs=labs,pch=pch,col=col,cex=cex,type=type)
        # STILL TODO, NOT WORKING
        out<-do.call("figOverplotDiagram",arguments)
    }else{
        arguments<-c(list(autoscale=autoscale,var.name=var.name,mat=mat,sample.names=sample.names,overplotDataset=TRUE,
            condition=reserv.condition,labs=labs,pch=pch,col=col,cex=cex,type=type),arguments)
        out<-do.call("figOverplot",arguments)
    }
    invisible(out)
}

# Overplot additional data onto Figaro compatible binary, ternary and spider plots
figOverplot<-function(var.name,mat=NULL,sample.names=NULL,condition=NULL,labs=NULL,autoscale=FALSE,pch="*",col="darkred",cex=1,type="p",just.draw=FALSE,overplotDataset=FALSE,...){    
    on.exit(options("show.error.messages"=TRUE))
    arguments<-list(...)
    
    if(length(plate.data)>0&!overplotDataset){
        cat("Function figOverplot is not designed for plotting on plates! Quitting!\n",sep="")
        options("show.error.messages"=FALSE)
        stop("",call. = FALSE)
    }
    
    # If mat not specified, it is created using the variable name in the global environment
    options(show.error.messages=FALSE)
    if(is.null(mat)){
        if(!is.character(var.name)){
            mat<-var.name
        }else{
            mat<-try(get(var.name,.GlobalEnv))
            if(class(mat)=="try-error"){
                cat("Variable not found: ",var.name,". Quitting!\n",sep="")
                stop("",call. = TRUE)
            }
        }
    }else{
    #    var.name<-""
    }
 
    # If neither sample names nor regex therein are defined, all samples are to be used
    if(is.null(sample.names) & is.null(condition)){
        sample.names<-rownames(mat)
        condition<-"."
        if(is.null(labs))labs<-"" # NEW TO BE TESTED
    }
    
    # Get the correct symbols, colours and sizes
    # NEW
    if(length(pch)>1 & !is.null(names(pch))){
        pch<-pch[sample.names]
    }
    if(length(col)>1& !is.null(names(col))){
        col<-col[sample.names]
    }
    if(length(cex)>1& !is.null(names(cex))){
        cex<-cex[sample.names]
    }
    # /NEW
    
    # Call the correct underlying function for binary, ternary or spiderplot
    plotType<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
    out<-paste("Not implemented for",plotType) # To be returned for unrecognized plot types

    if(plotType=="binary "){
        arguments<-c(list(mat,var.name,sample.names=sample.names,samples.condition=condition,labs=labs,autoscale=autoscale,pch=pch,col=col,cex=cex,type=type),arguments)
        out<-do.call(".figAddReservoirsBinary",arguments)
    }
    
    if(plotType=="ternary "){
        arguments<-c(list(mat,var.name,sample.names=sample.names,samples.condition=condition,labs=labs,pch=pch,col=col,cex=cex,type=type),arguments)
        out<-do.call(".figAddReservoirsTernary",arguments)
    }
    if(plotType=="spider "){
        #xd<-x.data # Keep the data safe
        #yd<-y.data 
        arguments<-c(list(overdata=mat,var.name=var.name,sample.names=sample.names,samples.condition=condition,labs=labs,autoscale=autoscale,pch=pch,col=col,cex=cex),arguments)
        ee<-paste("^",sheet$demo$template$GCDkit$plot.name,"$",sep="")
        ee<-gsub("[ ()]",".",ee)
        chon<-selectNorm(ee) # NEW Restore the normalizing values from the Figaro template
        assign("chondrit",chon,.GlobalEnv) # NEW
        out<-do.call(".figAddReservoirsSpider",arguments)
        #assign("x.data",xd,.GlobalEnv) # Restore the data from the safe copy
        #assign("y.data",yd,.GlobalEnv)
    }
    
    # Modify the Figaro template, add a new item each time it is called
    if(!just.draw){
        i<-grep("^reservoirs",names(sheet$demo$template))
        res.name<-paste("reservoirs",length(i)+1,sep="")
        sheet$demo$template[[res.name]]<-list("reservoirs", autoscale=FALSE, var.name=var.name, sample.names=out$sample.names, 
            reserv.condition = out$samples.condition, labs = out$labs, pch=out$pch, col=out$col, cex=out$cex, type=out$type)
        assign("sheet",sheet,.GlobalEnv)
    }
    if(is.list(out)) out<-out$out
    return(out)
}


# Binary plot
.figAddReservoirsBinary<-function(overdata,var.name,sample.names=NULL,samples.condition=NULL,labs=NULL,autoscale=FALSE,pch="*",col="darkgreen",cex=1.5,type="p",...){ 
    on.exit(options("show.error.messages"=TRUE))
    if(all(is.na(sample.names))) sample.names<-NULL
       
    # Get the formulae for the two axes from the Figaro template
    apices<-sapply(c("xlab","ylab"),function(i){ 
        z<-.fig.deeval(sheet$demo$call[[i]])
        return(z)
    },simplify=TRUE)

    # Calculate the data using these formulae and select only reservoirs where both axes are available
    coords<-.getRelevantReservoirs(apices,overdata)
    if(is.null(coords)){cat("No samples available with sufficient data!\n");return()}
    colnames(coords)<-apices
        
    # If sample condition specified, apply it now and generate sample names
    if(is.null(sample.names) & !is.null(samples.condition)){
        txt<-paste(samples.condition,collapse="|")
        txt<-gsub("[ ]",".*",txt)
        sample.names<-grep(txt,rownames(coords),value=TRUE,ignore.case=TRUE)
    }
       
    # If both sample.names and samples.condition missing, invoke the select.list 
    if(is.null(sample.names) & is.null(samples.condition)){
        sample.names<-select.list(rownames(coords),multiple=TRUE)
        if(length(sample.names)==0){cat("Cancelled.\n");return()}
    }
    
    # Name labs if necessary
    #if(is.null(names(labs))) names(labs)<-sample.names
    
    # Sample names specified now, but are them not empty?
    if(!is.null(sample.names)){
        sample.names<-intersect(sample.names,rownames(coords))
        if(length(sample.names)==0){cat("No samples available with sufficient data!\n");return()}
    }
        
    coords<-coords[sample.names,,drop=FALSE]
    if(length(overdata)==0){cat("Nothing to plot...\n");return()} 
    if(is.null(labs)) labs<-sample.names 
    # NEW
    if(labs!="") labs<-labs[!is.na(x)&!is.na(y)]
    # /NEW

    x<-coords[,1,drop=FALSE]
    y<-coords[,2,drop=FALSE]
         
    if(autoscale){
        x.range<-sheet$demo$call$xlim
        y.range<-sheet$demo$call$ylim
        rangex<-range(c(x,x.range),na.rm=TRUE)
        rangey<-range(c(y,y.range),na.rm=TRUE)
        #range<-rangey[2]-rangey[1]
        sheet$demo$call$xlim<-rangex
        sheet$demo$call$ylim<-rangey
        sheet$demo$call$ylim[2]<-sheet$demo$call$ylim[2]+diff(rangey)*0.1 # NEW TO ACCOMMODATE THE TEXTUAL LABELS 
        sheet$demo$call$xaxs<-"r"
        sheet$demo$call$yaxs<-"r"
        assign("sheet",sheet,.GlobalEnv)
        figRedraw()
    }
    # NEW
    if(length(pch)>1|length(col)>1|length(cex)>1){
        ii<- rownames(!is.na(x)&!is.na(y))
    }else{
        ii<-1
    }
    points(x,y,pch=pch[ii],cex=cex[ii],col=col[ii],type=type,log=sheet$demo$call$log,...)
    text(x,y,labs,cex=cex[ii]*0.75,col=col[ii],font=2,adj=c(0.5,0),pos=3)
    # /NEW
    #if(!is.null(samples.condition)) sample.names<-NULL # Fix of Pearce problem    
    invisible(list(out=coords, var.name=var.name, sample.names=sample.names, reserv.condition=samples.condition, 
        labs=labs, pch=pch, col=col, cex=cex, type=type))
}

# Ternary plot
.figAddReservoirsTernary<-function(overdata,var.name,sample.names=NULL,samples.condition=NULL,labs=NULL,pch="*",col="darkgreen",cex=1.5,type="p",...){
    on.exit(options("show.error.messages"=TRUE))
    arguments<-list(...)
     
    # Get the formulae for the three apices from the Figaro template
    apices<-sapply(c("A","B","C"),function(i){ 
        z<-.fig.deeval(sheet$demo$temp[[i]]$text)
        return(z)
    },simplify=TRUE)
        
    # Calculate the data using these formulae and select only reservoirs where all three coordinates are available
    coords<-.getRelevantReservoirs(apices,overdata)
    if(is.null(coords)){cat("No samples available with sufficient data!\n");return()}
    colnames(coords)<-apices
        
    # If sample condition specified, apply it now and generate sample names
    if(is.null(sample.names) & !is.null(samples.condition)){
        txt<-paste(samples.condition,collapse="|")
        txt<-gsub("[ ]",".*",txt)
        sample.names<-grep(txt,rownames(coords),value=TRUE,ignore.case=TRUE)
    }
       
    # If both sample.names and samples.condition missing, invoke the select.list 
    if(is.null(sample.names) & is.null(samples.condition)){
        sample.names<-select.list(rownames(coords),multiple=TRUE)
        if(length(sample.names)==0){cat("Cancelled.\n");return()}
    }
        
        
    # Sample names specified now, but are them not empty?
    if(!is.null(sample.names)){
        sample.names<-intersect(sample.names,rownames(coords))
        if(length(sample.names)==0){cat("No samples available with sufficient data!\n");return()}
    }
        
    # Select the corresponding analyses
    coords<-coords[sample.names,,drop=F]
    if(length(overdata)==0){cat("Nothing to plot...\n");return()} 
    if(is.null(labs)) labs<-sample.names     
    # NEW
    if(length(pch)>1|length(col)>1|length(cex)>1){
        ii<- rownames(coords)
    }else{
        ii<-1
    }
    out<-triplotadd(coords[,1],coords[,2],coords[,3],pch=pch[ii],cex=cex[ii],col=col[ii],labs=labs,type=type,...)
    # /NEW
    out<-cbind(coords,out)
    colnames(out)[1:3]<-apices
    invisible(list(out=out, var.name=var.name, sample.names=sample.names, reserv.condition=samples.condition, 
            labs=labs, pch=pch, col=col, cex=cex, type=type))
}

# Spiderplot
.figAddReservoirsSpider<-function(overdata,var.name,sample.names=NULL,samples.condition=NULL,labs=NULL,autoscale=FALSE,pch="*",col="darkred",cex=1.5,...){
        on.exit(options("show.error.messages"=TRUE))
        #ee<-as.list(match.call())
        arguments<-list(...)
        
        if(all(is.na(sample.names))) sample.names<-NULL
        
        # Select only reservoirs for which enough data are available
        temp<-filterOut(overdata,colnames(chondrit))
        if(length(temp)==0){cat("No samples available with sufficient data!\n");return()}
        
        # Select samples according to regex
        if(is.null(sample.names) & !is.null(samples.condition)){
            txt<-paste(samples.condition,collapse="|")
            txt<-gsub("[ ]",".*",txt)
            item<-grep(txt,rownames(overdata),value=TRUE,ignore.case=TRUE) 
            sample.names<-intersect(item,rownames(temp))
        }
        
        # Sample names specified
        if(!is.null(sample.names)){
            sample.names<-sample.names[sample.names%in%rownames(temp)]
            if(length(sample.names)==0){cat("No samples available with sufficient data!\n");return()}
        }else{
        
        # If sample condition specified, apply it now and generate sample names
            if(!is.null(samples.condition)){
                 txt<-paste(samples.condition,collapse="|")
                 txt<-gsub("[ ]",".*",txt)
                 item<-grep(txt,rownames(overdata),value=FALSE,ignore.case=TRUE) 
                 sample.names<-rownames(temp)[item]
            }
        
            # Otherwise,invoke the select.list 
            if(is.null(samples.condition)){
                sample.names<-select.list(rownames(temp),multiple=TRUE)
                if(length(sample.names)==0){cat("Cancelled.\n");return()}
            }
        }
        
        if(!is.null(sample.names)){
            A<-subset(temp,rownames(temp)%in%sample.names,drop=FALSE)
        }else{
            A<-temp
        }
        A[A==0]<-NA
        
        if(is.null(labs)) labs<-rownames(A)
        #if(length(labs)!=nrow(A)) labs<-rownames(A)
        
        # If field, col = NA and no labels
        if(!is.null(arguments$field)){
            if(arguments$field){
                col<-NA
                labs=" "
            }        
        }
        
        # Autoscaling
        if(autoscale){
            y<-spider(A,chondrit,plot=FALSE)
            rangey<-range(c(y,y.data[y.data>0]),na.rm=TRUE)
            ymin<-floor(100*rangey[1])/100
            if(ymin==0) ymin<-rangey[1]
            ymax<-ceiling(rangey[2]/10)*10
            sheet$demo$call$ylim<-c(ymin,ymax)
            sheet$demo$call$xaxs<-"i"
            sheet$demo$call$yaxs<-"i"
            assign("sheet",sheet,.GlobalEnv)
            pp<<-figaro(demo,prefix="sheet")
            figRedraw()
        }
        
        if(length(pch)>nrow(A)|length(col)>nrow(A)|length(cex)>nrow(A)){
            j<-which(rownames(overdata)%in%rownames(A)) # NEW
            pch<-pch[j]
            col=col[j]
            cex=cex[j]
            #normalized<-spider(A,chondrit,pch=pch[j],col=col[j],cex=cex[j],join=TRUE,add=TRUE,legend=FALSE,...)
        }else{
            pch<-rep(pch,length.out=nrow(A))
            col<-rep(col,length.out=nrow(A))
            cex<-rep(cex,length.out=nrow(A))
            #normalized<-spider(A,chondrit,pch=rep(pch,length.out=nrow(A)),col=rep(col,length.out=nrow(A)),cex=rep(cex,length.out=nrow(A)),join=TRUE,add=TRUE,legend=FALSE,...)
        }
        normalized<-spider(A,chondrit,pch=pch,col=col,cex=cex,join=TRUE,add=TRUE,legend=FALSE,...)
        
        yy<-apply(normalized[,1:3,drop=FALSE],1,max,na.rm=TRUE)
        text(1.2,yy,cex=0.75,col=col,font=2,labs,adj=c(0,0))
        
        invisible(list(out=normalized, var.name=var.name, sample.names=sample.names, reserv.condition=samples.condition, 
            labs=labs, pch=pch, col=col, cex=cex, type="p")) # Type not implemented yet, left here for compatibility only
}

# Overplot additional data onto Figaro compatible classification or geotectonic plots (obtained by plotDiagram or plateExtract)


#############################################################################
#                                Auxiliary functions                        #
#############################################################################

# Calculates a molecularWeight of a single oxide given by its formula, 
# returns also number of atoms and oxygens 
molecularWeight<-function(formula){
    on.exit(options("show.error.messages"=TRUE))
    ee<-strsplit(paste(formula," ",sep=""),"O")
    n<-as.numeric(gsub("[a-zA-Z]","",ee[[1]]))
    n[is.na(n)]<-1
    if(length(n)==1)n[2]<-0   
    atom<-gsub("[0-9 ]","",ee[[1]][1])
    z<-n[1]*mw[atom]+n[2]*mw["O"]
    z<-c(z,n)
    names(z)<-c("MW","x.atoms","x.oxygen")
    return(z)
}


.save2hist<-function(what){
    on.exit(options("show.error.messages"=TRUE))
    options("show.error.messages"=TRUE)   
    if(.Platform$OS.type!="windows"|.Platform$GUI!="Rgui")return()
    
    file1 <- tempfile("Rrawhist")
    utils::savehistory(file1)
    rawhist <- readLines(file1)
    rawhist[length(rawhist)]<-what
    file2 <- tempfile("hist")
    writeLines(rawhist, file2)
    utils::loadhistory(file2)
    unlink(file1)
    unlink(file2)
    invisible()
}

summaryRangesByGroup<-function(elems=major,where=NULL,silent=TRUE){
  on.exit(options("show.error.messages"=TRUE))
   if(!silent){     
        if(is.null(where)) where<-selectSamples(print=FALSE)
        elems<-selectColumnsLabels(default=elems,exact.only = FALSE)
        if(is.null(elems)){
            winDialog(type="ok","Your query produced no matches!")
            options(show.error.messages=FALSE);stop("",call. = FALSE)
        }
    }
    
    elems<-unlist(strsplit(elems,","))
    
    if(is.null(where)) where<-rownames(WR)
    if(is.null(names(groups))) names(groups)<-rownames(WR) # NEW
    
    temp<-printSamples(elems,which=where,print=!silent)
    
    results<-aggregate(
        temp,
        list(groups=groups[where]),function(i){
        paste(round(min(i,na.rm=TRUE),3),round(max(i,na.rm=TRUE),3),sep="-")}
    )
    results<-as.matrix(results)
    
    rownames(results)<-results[,1]
    results<-subset(results,select=2:ncol(results))
    colnames(results)<-elems
    assign("results",results,.GlobalEnv)
    return(results)
}

# Calculates any anomaly on a spiderplot
calcAnomaly<-function(which.elem="Eu",dataset=WR,ref="^REE Boynton",left="Sm",right="Gd"){
    spider(dataset,ref,plot=FALSE) # Calculation only
    
    which.left<-grep(paste("^",left,"N$",sep=""),colnames(results))
    which.anom<-grep(paste("^",which.elem,"N$",sep=""),colnames(results))
    which.right<-grep(paste("^",right,"N$",sep=""),colnames(results))
    delka<-abs(which.right-which.left)
    
    eq<-paste(which.elem,"N/(",left,"N^",abs(which.right-which.anom)/delka,"*",right,"N^",abs(which.left-which.anom)/delka,")",sep="")
    print(eq)
    
    z<-calcCore(eq,where="results")$results
    
    varname<-paste(which.elem,"/",which.elem,"*",sep="")
    z<-matrix(z,nrow=1,ncol=length(z),dimnames=list(varname,names(z)))
    return(z)
}

#Testing on Eu anomaly
# calcAnomaly()
