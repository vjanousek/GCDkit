.HTMLInitFileMy<-function (file = .HTML.file){
    on.exit(options("show.error.messages"=TRUE))
    options(scipen=6)
    options(warn=-1)
        #ee<-require(R2HTML)
    options(warn=0)

    #if(!ee){
    #    cat("WARNING: library R2HTML is not installed! Quitting...\n")
    #}
    
    options(show.error.messages = FALSE)
    outdir<-dirname(file)
    dir.create(outdir,showWarnings=FALSE)
    options(show.error.messages = TRUE)
    ee<-file.copy(paste(gcdx.dir,"/R2HTML.css",sep=""),paste(outdir,"/R2HTML.css",sep=""),overwrite=TRUE)
    ee<-file.copy(paste(gcdx.dir,"/R2HTMLlogo.gif",sep=""),paste(outdir,"/R2HTMLlogo.gif",sep=""),overwrite=TRUE)
}

.HTMLEndFileMy<-function (file = .HTML.file,browse=TRUE) {
    #cat("<hr size=1>\n<font size=-1>\t 
    #Generated on: <I>", date(), "</i> - <b>",package.name," via R2HTML</b> \n<hr size=1>\n\t</body>\n</html>", 
    #sep = "", append = TRUE, file = file)
    if(browse)browseURL(file)
}

HTMLTableMain<-function(what,digits=2,desc=NULL,title=" ",sum.up=FALSE,open=TRUE,close=TRUE,browse=TRUE,filename=paste(data.dir,"R2HTML/htmltable",sep="/"),rotate=FALSE){          
#HTMLTableMain<-function(what,digits=2,desc=NULL,title=" ",sum.up=FALSE,open=TRUE,close=TRUE,filename=paste(gcdx.dir,"R2HTML/htmltable",sep="/"),rotate=FALSE){        
    .HTMLInitFileMy(filename)
    isotopes<-c("Rb.y","Sr.y","87Rb/86Sr","87Sr/86Sr","2s.Sr","87Sr/86Sri","Sm.y","Nd.y","147Sm/144Nd","143Nd/144Nd","2s.Nd","143Nd/144Ndi","EpsNdi","TDM.2stg")
    ee<-match(isotopes,colnames(WR))
    isotopes<-colnames(WR)[ee[!is.na(ee)]]
   
    options("warn"=-1)
    if(sum.up){
        sum<-apply(what,1,sum,na.rm=TRUE)
        sum[sum==0]<-NA
        what<-cbind(what,sum)
        colnames(what)[ncol(what)]<-"Sum"
    }
    
    if(open){
        outdir<-dirname(filename)
        #outdir<-paste(gcdx.dir,"R2HTML",sep="/")
        .HTML.file <<- HTMLInitFile(outdir=outdir, filename=basename(filename), BackGroundColor="#BBBBEE")
    }
    if(title!=" "){
        HTML(paste("<br><center><b>",title,"<br></b></center>",sep=""))
    }
   
    ee<-rownames(what)
    what<-filterOut(what,colnames(what),length(colnames(what)))
    if(is.vector(what))what<-as.matrix(what)
    #rownames(what)<-ee
    
    colnames(what) <- .HTML.reformat(colnames(what)) #NEEEEEWWW
    #print(colnames(what))
 
    if(is.numeric(as.matrix(what))){
        if(!is.null(digits)) what<-round(what,digits)
    }
   
    if(!is.null(desc)){
        if(package.name=="GCDkit.Mineral"){
            which<-colnames(what)
        }else{
            which<-rownames(what)
        }
        header<-subset(labels,rownames(labels)%in%which,desc,drop=FALSE)
        header<-header[which,]
    
        if(is.factor(header)) header<-as.character(header)
        header<-as.matrix(header)
        header[header==-1]<-NA
        colnames(header)<-desc
        rownames(header)<-which
        if(package.name=="GCDkit.Mineral"){
            what<-rbind(t(header),what)
        }else{
            what<-cbind(header,format(what,digits=NULL,trim=TRUE))
            what<-as.matrix(what)
            #what<-gsub("^NA$","\u2013",what) # en dash
            what<-gsub("^NA$","-",what) # dash
        }
        
    }else{
        header<-""
    }
    if(package.name=="GCDkit.Mineral"&is.data.frame(what) | rotate==TRUE)  what<-t(what)
    what<-as.data.frame(what)
    HTML(what,digits=15)
    if(close).HTMLEndFileMy(browse=browse)
    invisible(what)
    #if(close)browseURL(paste("file:/",data.dir,"/R2HTML/htmltable.html",sep=""))
}

.HTML.reformat<-function(text){
   on.exit(options("show.error.messages"=TRUE))
   options("warn"=-1)
   for(i in (1:length(text))){ 
        what<-as.character(annotate(text[i]))
        what<-gsub(" saturation T ","<BR>saturation T<BR>",what)
        what<-gsub("[\"][ ][\"]","",what)
        what<-gsub("2 \\* s\\.","2&sigma; ",what)       
        what<-gsub("\\[","<SUB>",what)
        what<-gsub("\\]","</SUB>",what)
        
        what<-gsub("(^Sigma)( )(\\*)( )","&Sigma;",what)
        what<-gsub("( )(\\*)( )([0-9A-Z])","</SUP>\\4",what)
        what<-gsub("\\^","<SUP>",what)
        what<-gsub("epsilon","&epsilon;",what)
        what<-gsub("delta","&delta;",what)
        what<-gsub("Tmz.sat.C","Mnz sat. T &deg;C",what)
        what<-gsub("TZr.sat.C","Zrc sat. T &deg;C",what)
        what<-gsub("Tap.sat.C.","Ap sat. T &deg;C ",what)
        what<-gsub("^Sum$","&Sigma;",what)
        what<-gsub("[.]PLUS$","<SUP>+</SUP>",what)
        what<-gsub("[.]MINUS$","<SUP>\u2013</SUP>",what) # en dash
        #what<-gsub("^H2O[.]PLUS$","H<SUB>2</SUB>O<SUP>+</SUP>",what)
        what<-gsub("^H2O-$","H<SUB>2</SUB>O<SUP>-</SUP>",what)
        what<-gsub("^FeII_","Fe<SUP>II</SUP>_",what)
        what<-gsub("^FeIII_","Fe<SUP>III</SUP>_",what)
        what<-gsub("^FeII$","Fe<SUP>II</SUP> ",what)
        what<-gsub("^FeIII$","Fe<SUP>III</SUP> ",what)
        what<-gsub(" FeII/FeIII "," Fe<SUP>II</SUP>/Fe<SUP>III</SUP> ",what)
        what<-gsub("^FeIII/Fetot$","Fe<SUP>III</SUP>/Fe<SUB>tot</SUB>",what)
        what<-gsub("^Mg/(Mg+FeII)","Mg/(Mg+Fe<SUP>II</SUP>)",what) #nefunguje
        what<-gsub("~"," ",what)

        what<-gsub("^XMg","X<SUB>Mg</SUB>",what)
        what<-gsub("AlIV/AlVI","Al<SUP>IV</SUP>/Al<SUP>VI</SUP>",what)
        what<-gsub("AlIV","Al<SUP>IV</SUP>",what)
        what<-gsub("AlVI","Al<SUP>VI</SUP>",what)
        what<-gsub("^LaN/YbN$","La<SUB>N</SUB>/Yb<SUB>N</SUP>",what)
        what<-gsub("^LaN/SmN$","La<SUB>N</SUB>/Sm<SUB>N</SUP>",what)
        what<-gsub("^EuN/YbN$","Eu<SUB>N</SUB>/Yb<SUB>N</SUP>",what)
        what<-gsub("^Eu/Eu*$","Eu/Eu<SUP>*</SUB>",what)
        text[i]<-gsub("</SUB></SUP>","</SUB>",what)
    }
   return(text)
}

.HTML.table.core<-function(what,silent=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    desc<-select.list(colnames(labels),title="Select labels to go with the data",multiple=TRUE)
    if(length(desc)==0)desc<-NULL
    
    ee<-winDialog("yesno","Sum the variables?")
    if(ee=="YES"){
        sum.up<-TRUE
    }else{
        sum.up<-FALSE
    }
    
    ee<-winDialogString("Precision of the output","2")
    if(is.null(ee)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}    
    digits<-as.numeric(ee)
    #digits<-2
    colnames(what)<-.HTML.reformat(colnames(what))
    HTMLTableMain(what,desc=desc,digits=digits,sum.up=sum.up)
    cat("Done.\n")
}

HTMLTableWR<-function(filename="htmltable"){    
    options("warn"=-1)
    i<-selectSamples()  
    xx<-selectColumnsLabels(where=colnames(WR),print=TRUE)
    if(is.null(xx)){
        winDialog(type = "ok", "No results. Selecting all variables")
        xx<-colnames(WR)
    }
    
    what<-data.frame(WR[i,xx])
    rownames(what)<-i
    colnames(what)<-xx
    .HTML.table.core(what)
}    

HTMLTableResults<-function(filename="htmltable"){    
    on.exit(options("show.error.messages"=TRUE))
    options("warn"=-1)
    if(is.null(results)){
        winDialog(type = "ok", "No data available")
        options("show.error.messages"=FALSE)
        stop()
    }
    
    if(is.data.frame(results)|is.matrix(results)){
        x<-as.matrix(results)
        x<-addResults(save=FALSE)
        x<-cbind(x,labels[rownames(x),])
        xx<-selectColumnsLabels(where=colnames(results))
        if(is.null(xx)){
            winDialog(type = "ok", "No results. Selecting all variables")
            xx<-1:ncol(results)
        }    
        what<-as.data.frame(results[,xx])
        if(ncol(what)==1){
            if(length(xx)==1){
                colnames(what)<-xx
            }else{
               colnames(what)<-rownames(results)
            }
        }

        .HTML.table.core(what)
    }
    if(is.list(results)){
        desc<-select.list(colnames(labels),title="Select labels to go with the data",multiple=TRUE)
        if(length(desc)==0)desc<-NULL
        digits<-winDialogString("Precision of the output","3")
        if(is.null(digits)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}  
        digits<-as.numeric(digits)
        #outdir<-paste(gcdx.dir,"R2HTML",sep="/")
        outdir<-paste(data.dir,"R2HTML",sep="/")
        #outdir<-"R2HTML"
        .HTMLInitFileMy(paste(outdir,filename,sep="\t"))
        
        dir.create(outdir,showWarnings=FALSE)
        .HTML.file <<- HTMLInitFile(outdir=outdir, filename=basename(filename), BackGroundColor="#BBBBEE")
        #.HTML.file <- HTMLInitFile(outdir=paste(data.dir,"/R2HTML/",sep=""),filename="htmltable", BackGroundColor="#BBBBEE")
        z<-sapply(names(results),function(i){
            if(length(results[[i]])!=0){
                colnames(results[[i]])<-.HTML.reformat(colnames(results[[i]]))
                title<-" "
                if(package.name=="GCDkit.Mineral"){
                    title<-.HTML.reformat(.header(min[[i]],html=TRUE))
                }
                names(i)<-NULL
                what<-t(as.data.frame(results[[i]]))
                colnames(what)<-.HTML.reformat(colnames(what))
                HTML(paste("<center><h3>",i,"</h3></center>",sep=""))
                ee<-HTMLTableMain(what,title=title,desc=desc,sum.up=FALSE,open=FALSE,close=FALSE,digits=digits)
            }
        })
        .HTMLEndFileMy()
    }    
}

# Makes a HTML table ordered on two criteria,
# key 1 is a parameter in WR, such as "SiO2"
# key 2 is a grouping information (name of a column in labels)

HTMLtableOrdered<-function(what,which=rownames(what),labs=labels,digits=2,desc=NULL,title=" ",sum.up=FALSE,key1=NULL,key2=NULL,filename=paste(data.dir,"R2HTML/htmltable",sep="/"),split.by=25,rotate=TRUE){ 
#HTMLtableOrdered<-function(what,which=rownames(what),labs=labels,digits=2,desc=NULL,title=" ",sum.up=FALSE,key1=NULL,key2=NULL,filename=paste(gcdx.dir,"R2HTML/htmltable",sep="/"),split.by=25,rotate=TRUE){ 
    if(is.numeric(which)) which<-rownames(WR)[which]
    if(is.vector(what)){
        what<-gsub("[ ]","",what) #remove trailing spaces
        what<-unlist(strsplit(what,","))

        out<-matrix(nrow=length(which),ncol=length(what))
        rownames(out)<-which
        colnames(out)<-what
        for(j in 1:length(what)){
            if(any(colnames(WR)==what[j])){
                out[,j]<-WR[which,what[j]]
            }else{
                ee<-(calcCore(what[j]))
                out[,j]<-ee$results[which]
            }
        }
    }else{
        out<-what[which,]
    }
   
    if(!is.null(key1)){
        if(is.null(key2)){
            which<-sort(out[,key1])
        }else{
            ee<-lapply(levels(factor(labs[which,key2])), function(i){
                ee<-rownames(labs)[labs[,key2]==i]
                ee2<-ee[ee%in%which]
                if(length(ee2)>1){
                    zz<-names(sort(WR[ee2,key1]))
                }else{
                    zz<-ee2
                }
                return(zz)
            }
            )
            which<-unlist(ee)
        }
    
    } 
    if(!is.null(digits)) out<-round(out[which,],digits) 
    lab2<-as.character(c(key2,desc))
    
    
     if(!rotate){
         if(sum.up){
            suma<-apply(out,1,sum,na.rm=TRUE)
            out<-cbind(out,suma)
            colnames(out)[ncol(out)]<-"Sum"
        }
        i<-seq(from=0,to=ncol(out)+length(lab2),by=split.by)
        if(i[length(i)]!=ncol(out)+length(lab2)) i<-c(i,ncol(out)+length(lab2))
        
        ee1<-subset(out,desc=lab2,rep(TRUE,split.by),colnames(out)%in%colnames(out)[1:(split.by-length(lab2))],drop=FALSE)
        HTMLTableMain(ee1,desc=lab2,digits = digits, title = title, sum.up = FALSE, filename = filename, rotate = rotate,open=TRUE,close=FALSE)
        
        if(ncol(out)+length(lab2)>split.by){ #Something still left
            ee<-lapply(2:(length(i)-1),function(j){
                ee1<-subset(out,TRUE,colnames(out)%in%colnames(out)[(i[j]+1-length(lab2)):(i[j+1]-length(lab2))],drop=FALSE)
                HTMLTableMain(ee1,digits = digits, title = " ", sum.up = FALSE, filename = filename, rotate = rotate,open=FALSE,close=FALSE)
            }
            )
        }    
    }else{
        i<-seq(from=0,to=nrow(out),by=split.by)
        if(i[length(i)]!=nrow(out)) i<-c(i,nrow(out))
        
        ee1<-subset(out,rownames(out)%in%rownames(out)[1:split.by],colnames(out),drop=FALSE)
        HTMLTableMain(ee1,desc=lab2,digits = digits, title = title, sum.up = sum.up, filename = filename, rotate = rotate,open=TRUE,close=FALSE)
        if(nrow(out)>split.by){ #Something still left
            ee<-lapply(2:(length(i)-1),function(j){
                ee1<-subset(out,rownames(out)%in%rownames(out)[(i[j]+1):(i[j+1])],colnames(out),drop=FALSE)
                HTMLTableMain(ee1,desc=lab2,digits = digits, title = " ", sum.up = sum.up, filename = filename, rotate = rotate,open=FALSE,close=FALSE)
            }
            )
        }
    }
    .HTMLEndFileMy()
    invisible()
}
  
    
