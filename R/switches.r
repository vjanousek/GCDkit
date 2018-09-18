###### Menu definitions
.switch.data.export.format<-function(){
    on.exit(options("show.error.messages"=TRUE))
    where<-c("HTML","Excel (XLS)","Excel 2007 (XLSX)","Access (MDB)","dBase (DBF)")
    selected<-select.list(where,title="Select output format",multiple=FALSE)
    if(selected==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
    ee<-switch(which(where==selected),
        "HTMLTableWR()",
        "excelExport()",
        "excel2007Export()",
        "accessExport()",
        "dbfExport()"    
    )
    cat("GCDkit->",ee,"\n")
    .save2hist(ee)
    eval(parse(text=ee)) 
}

.switch.results.export.format<-function(){
    on.exit(options("show.error.messages"=TRUE))
    where<-c("HTML","Excel (XLS)","Excel 2007 (XLSX)","Access (MDB)","dBase (DBF)")
    selected<-select.list(where,title="Select output format",multiple=FALSE)
    if(selected==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
    ee<-switch(which(where==selected),
        "HTMLTableResults()",
        "excelExport(results,transpose=FALSE,dec.places=3)",
        "excel2007Export(results,transpose=FALSE,dec.places=3)",
        "accessExport(results,transpose=FALSE,dec.places=3)",
        "dbfExport(results,transpose=FALSE)"
    )
    cat("GCDkit->",ee,"\n")
    .save2hist(ee)
    eval(parse(text=ee))
}

.switch.display<-function(){
    on.exit(options("show.error.messages"=TRUE))
    where<-c("... variable","... samples","... info on data file","... cross table of labels")
    selected<-select.list(where,title="Display...",multiple=FALSE)
    if(selected==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
    ee<-switch(which(where==selected),
        "printSingle()",
        "printSamples(select.samples=TRUE)",
        "info()",
        "crosstab()"
    )
    cat("GCDkit->",ee,"\n")
    .save2hist(ee)
    eval(parse(text=ee))
}

.switch.edit<-function(){
    on.exit(options("show.error.messages"=TRUE))
    where<-c("... labels","... labels as factor","... numeric data","Delete label or variable","Append label or variable")
    selected<-select.list(where,title="Edit...",multiple=FALSE)
    if(selected==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
    ee<-switch( which(where==selected),
        "editData(labels)",
        "editLabFactor()",
        "editData(WR)",
        "deleteSingle()",
        "appendSingle()"
    )
    cat("GCDkit->",ee,"\n")
    .save2hist(ee)
    eval(parse(text=ee))
}

.switch.subset<-function(){
    on.exit(options("show.error.messages"=TRUE))
    where<-c("... sample name or label","... range","... Boolean condition","... diagram ","Select whole data set")
    selected<-select.list(where,title="Select subset by...",multiple=FALSE)
    if(selected==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
    ee<-switch(which(where==selected),
        "selectByLabel()",
        "selectSubset(text=\"Enter sample range, e.g. 1:5,15,19\",range=TRUE)",
        "selectSubset()",
        "selectByDiagram()",
        "selectAll(GUI=T)"
    )
    cat("GCDkit->",ee,"\n")
    .save2hist(ee)
    eval(parse(text=ee))
}

.switch.norms<-function(){
    on.exit(options("show.error.messages"=TRUE))
    #norm.path<-paste(gcdx.dir,"/Norms",sep="")
    #where<-dir(norm.path,pattern = "[.][Rr]$",recursive=FALSE)
    
    where<-c("Niggli","CIPW","CIPW with hb","Granite Mesonorm","Catanorm","Mode","Debon and Le Fort","De la Roche","Miscellany")
    selected<-select.list(where,title="Norms",multiple=FALSE)
    if(selected==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
    ee<-switch(which(where==selected),
        "results<-Niggli(WR)",
        "results<-CIPW(WR)",
        "results<-CIPWhb(WR)",
        "results<-Mesonorm(WR,GUI=TRUE)",
        "results<-Catanorm(WR)",
        "results<-ModeMain(WR)",
        "results<-DebonCalc();print(results)",
        "results<-LaRocheCalc();print(results)",
        "results<-Misc(WR)"
    )
    cat("GCDkit->",ee,"\n")
    .save2hist(ee)
    ee<-try(eval(parse(text=ee)),silent=TRUE)
    if(class(ee)=="try-error"){
        winDialog(type="ok","The result is empty! Check the input data...")
        invisible()
    }else{
        #if(mode(results)!="list"){
        #    if(nrow(results)>200) winDialog(type="ok","Done!")
        #}
        results<<-results
    }
    whichnorm<-selected
    return(whichnorm)
}

.switch.multiple<-function(){    
    on.exit(options("show.error.messages"=TRUE))
    where<-c("1 vs. majors","1 vs. traces","Coplot","Coplot by groups")
    selected<-select.list(where,title="Multiple plots",multiple=FALSE)
    if(selected==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
    ee<-switch(which(where==selected),
        "multipleMjr()",
        "multipleTrc()",
        "coplotTri();figaroOff()",
        "coplotByGroup();figaroOff()"
    )
    cat("GCDkit->",ee,"\n")
    .save2hist(ee)
    eval(parse(text=ee))
}

.switch.spider<-function(){        
    on.exit(options("show.error.messages"=TRUE))
    where<-c("... for selected samples", "... for selected samples - colour coded","... for selected samples - double normalized", "... summary boxplot","... summary boxplot double normalized", "... by group - fields","... by group - patterns")
    selected<-select.list(where,title="Spider plots...",multiple=FALSE)
    if(selected==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
    ee<-switch(which(where==selected),
        "results<-spider.individual()",
        "results<-spider.contour()",
        "results<-spider2norm()",
        "results<-spiderBoxplot()",
        "results<-spiderBoxplot(doublenorm=TRUE)",
        "spiderByGroupFields();figaroOff()",
        "spiderByGroupPatterns();figaroOff()"
    )
    cat("GCDkit->",ee,"\n")
    .save2hist(ee)
    eval(parse(text=ee))
}

.switch.groups<-function(){
    on.exit(options("show.error.messages"=TRUE))
    where <- c("... label", "... numeric variable", "... diagram", 
        "... outline", "... cluster analysis", "Merge groups")
    selected <- select.list(where, title = "Set groups by...", 
        multiple = FALSE)
    if (selected == "") {
        cat("Cancelled.\n")
        options(show.error.messages = FALSE)
        stop()
    }
    ee <- switch(which(where == selected), "groupsByLabel()", 
        "cutMy()", "ee<-groupsByDiagram()", "figGbo()", 
        "groupsByCluster()", "joinGroups()")
    cat("GCDkit->", ee, "\n")
    .save2hist(ee)
    eval(parse(text = ee))
}


.switch.geotectonic<-function(){
    shut.bak<-getOption("gcd.shut.up") # Make less verbose temporarilly
    options("gcd.shut.up"=TRUE)
    
    clasl<-.tectlist()
    on.exit(options("show.error.messages"=TRUE))
    where <- clasl[,1]
    selected<-" "
    while (any(selected == c(" ","===GRANITOIDS===","===BASALTOIDS===","===UNIVERSAL==="))) {
        selected <- select.list(where, title = "Select diagram", multiple = FALSE)
    }
    
    if (selected == "") {
        cat("Cancelled.\n")
        options(show.error.messages = FALSE)
        stop()
    }
    
    options("show.error.messages"=TRUE)
    
    # Plate
    fun<-clasl[clasl[,1]==selected, 2]
    if(clasl[clasl[,1]==selected, 3]=="TRUE"){
        
        cmd<-paste("plotPlate(\"",fun,sep="")
        if(any(names(formals(fun))=="GUI")) cmd<-paste(cmd,"\",GUI=TRUE)",sep="") else cmd<-paste(cmd,"\")",sep="")
    }else{
    # Normal plot
        cmd<-paste("plotDiagram(\"",fun,"\")",sep="")
    }  
  
  cat("GCDkit-> ",cmd,"\n",sep="")
  .save2hist(cmd)
  try(x.data.bak<-x.data,silent=TRUE)
  try(y.data.bak<-y.data,silent=TRUE)
  try(sheet.bak<-sheet,silent=TRUE)
  ee<<-try(eval(parse(text=cmd)),silent=TRUE)
  
  if(class(ee)=="try-error"){
            winDialog(type="ok",paste("Error in",clasl[clasl[,1]==selected, 2],
            "\nPlease check whether are all necessary data present!"))
            try(x.data<<-x.data.bak,silent=TRUE)
            try(y.data<<-y.data.bak,silent=TRUE)
            try(sheet<<-sheet.bak,silent=TRUE)
            dev.off(dev.cur())
            invisible()
    }
    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){winMenuAddItem("Plot editing","Classify","disable")}
    options("gcd.shut.up"=shut.bak)
    invisible()
 }

.switch.user<-function(){
    on.exit(options("show.error.messages"=TRUE))
    clasl<-.userlist()
    
    if(nrow(clasl)==0){
        winDialog(type="ok",paste("No user-defined diagram templates found!"))
        options(show.error.messages=FALSE)
        stop(call. = FALSE)
    }
    
    where <- clasl[,1]
    selected <- select.list(where, title = "Select diagram", multiple = FALSE)
    
    if (selected == "") {
        cat("Cancelled.\n")
        options(show.error.messages = FALSE)
        stop()
    }
    
    options("show.error.messages"=TRUE)
    
    # Plate
    fun<-clasl[clasl[,1]==selected, 2]
    if(clasl[clasl[,1]==selected, 3]=="TRUE"){
        
        cmd<-paste("plotPlate(\"",fun,sep="")
        if(any(names(formals(fun))=="GUI")) cmd<-paste(cmd,"\",GUI=TRUE)",sep="") else cmd<-paste(cmd,"\")",sep="")
    }else{
    # Normal plot
        cmd<-paste("plotDiagram(\"",fun,"\")",sep="")
    }  
  
  cat("GCDkit-> ",cmd,"\n",sep="")
  .save2hist(cmd)
  try(x.data.bak<-x.data,silent=TRUE)
  try(y.data.bak<-y.data,silent=TRUE)
  try(sheet.bak<-sheet,silent=TRUE)
  ee<<-try(eval(parse(text=cmd)),silent=TRUE)
  
  if(class(ee)=="try-error"){
            winDialog(type="ok",paste("Error in",clasl[clasl[,1]==selected, 2],
            "\nPlease check whether are all necessary data present!"))
            try(x.data<<-x.data.bak,silent=TRUE)
            try(y.data<<-y.data.bak,silent=TRUE)
            try(sheet<<-sheet.bak,silent=TRUE)
            dev.off(dev.cur())
            invisible()
    }
    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){winMenuAddItem("Plot editing","Classify","disable")}
 }
 
.switch.classification<-function(action="plot"){
    #action="plot" plots diagram
    #action="plate" plots diagram to a plate
    #action="classify" classifies data
    on.exit(options("show.error.messages"=TRUE))
    if(action=="plot") single<-TRUE
    if(action=="plate") single<-FALSE
    where <- claslist[, 1]
    selected <- " "
    while (any(selected == c(" ", "===VOLCANITES===", "===PLUTONITES==="))) {
        selected <- select.list(where, title = "Select diagram", 
            multiple = FALSE)
    }
    if (selected == "") {
        cat("Cancelled.\n")
        options(show.error.messages = FALSE)
        stop()
    }
    if (action=="plot"){
        cat("GCDkit-> plotDiagram(\"", claslist[claslist[, 1] == 
        selected, 2], "\",TRUE,TRUE)\n", sep = "")
        .save2hist(paste("plotDiagram(\"", claslist[claslist[, 1] == selected, 2], "\",TRUE,TRUE)\n", sep = ""))
        plotDiagram(claslist[claslist[, 1] == selected, 2],single,single)
    }

    if (action=="plate"){    
        cat("GCDkit-> plotDiagram(\"", claslist[claslist[, 1] == 
        selected, 2], "\",TRUE,FALSE)\n", sep = "")
        .save2hist(paste("plotDiagram(\"", claslist[claslist[, 1] == selected, 2], "\",TRUE,FALSE)\n", sep = ""))
        plotDiagram(claslist[claslist[, 1] == selected, 2],single,single)
    }
    if (action=="classify") {
        cat("GCDkit-> classify(\"", claslist[claslist[, 1] == selected, 2],",labs=TRUE\")\n", sep = "")
        .save2hist(paste(paste("classify(\"", claslist[claslist[, 1] == selected, 2],"\", labs = TRUE)\n", sep = "")))
        classify(claslist[claslist[, 1] == selected, 2],grp = FALSE,labs=TRUE) 
    }    
}

.switch.modelling<-function (){    
    on.exit(options("show.error.messages"=TRUE))
    where<-c("Fractional crystallization - direct majors","Fractional crystallization - direct traces", "Fractional crystallization - reverse majors","Fractional crystallization - reverse traces")
    selected<-select.list(where,title="Petrogentic modelling",multiple=FALSE)
    if(selected==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
    ee<-switch(which(where==selected),
        ".fcMajorsDirectTcl()",
        ".fcTraceDirectTcl()",
        ".fcMajorsInverseTcl()",
        ".fcTraceInverseTcl()"
    )
    cat("GCDkit->",ee,"\n")
    .save2hist(ee)
    eval(parse(text=ee))
}

.switch.stats<-function (){
    on.exit(options("show.error.messages"=TRUE))
    where<-c("Single variable all/selection",   "                by groups",   "Ranges by groups",   "Majors summaryAll/selection",   "            by groups",   "Traces summaryAll/selection",   "            by groups ",   "Stripplot by groups",   "Stripplot by groups - with boxplots",   "------------------------------------",   "Plot summary by element and group",   "------------------------------------ ",   "Correlation: majors",   "            traces",   "Correlation coef patterns",   "------------------------------------  ",   "Principal components",   "Hierarchical clustering")
    selected<-select.list(where,title="Statistics",multiple=FALSE)
    if(selected==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
    ee<-switch(which(where==selected),
        "summarySingle()",
        "summarySingleByGroup()",
        "summaryRangesByGroup(silent=FALSE)",
        "summaryMajor()",
        "summaryByGroupMjr()",
        "summaryTrace()",
        "summaryByGroupTrc()",
        "strip();figaroOff()",
        "stripBoxplot(silent=FALSE);figaroOff()",
        "",
        "results<<-statsByGroupPlot();figaroOff()",
        "",
        "pairsMjr();figaroOff()",
        "pairsTrc();figaroOff()",
        "correlationCoefPlot();figaroOff()",
        "",
        "prComp(GUI=TRUE);figaroOff()",
        "cluster();figaroOff()"
    )
    cat("GCDkit->",ee,"\n")
    .save2hist(ee)
    eval(parse(text=ee))

}  
