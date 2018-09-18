
#############################################################################
#                                                                           #
#                          Read info from schema                            #
#                                                                           #
#############################################################################
.getFromXML<-function(file,pathToNodes){
   on.exit(options("show.error.messages"=TRUE))
    
   options("show.error.messages"=FALSE)
   parsed<-try(tryCatch(xmlParse(file, asText = FALSE, xinclude = TRUE,error=NULL),XMLError = function(e){
        cat("... cannot connect to the web service, check your Internet connection! ***\n")
   }))
        
   if(is.null(parsed))return(-1) # No Internet
        
   fce<-function(path){
        nodes<-getNodeSet(parsed,path)
        result<-(sapply(nodes,xmlAttrs))
        names(result)<-NULL
        return(result)
   }
   if(is.list(pathToNodes)) results<-lapply(pathToNodes,fce) else results<-fce(pathToNodes)
   return(results)
}

uf<-require(XML)
if(!uf){cat("**** Package XML not found, no EarthChem import will be possible! ****\n")}
    
EChAttrPaths<-list()
EChAttrPaths[["itemlist"]]<-"//xs:element[@name='Item']//xs:enumeration"
EChAttrPaths[["agelist"]]<-"//xs:element[@name='Age']//xs:attribute[@name='geologicalage']//xs:enumeration"
EChAttrPaths[["SampleType-level1"]]<-"//xs:element[@name='SampleType']//xs:attribute[@name='level1']//xs:enumeration"
EChAttrPaths[["SampleType-level2"]]<-"//xs:element[@name='SampleType']//xs:attribute[@name='level2']//xs:enumeration"
EChAttrPaths[["SampleType-level3"]]<-"//xs:element[@name='SampleType']//xs:attribute[@name='level3']//xs:enumeration"
EChAttrPaths[["SampleType-level4"]]<-"//xs:element[@name='SampleType']//xs:attribute[@name='level4']//xs:enumeration"
EChAttrPaths[["Material"]]<-"//xs:element[@name='Material']//xs:enumeration"
    
schema.db<-.getFromXML("http://ecp.iedadata.org/soap_search_schema.xsd",EChAttrPaths)
detach("package:XML")
  
if(is.list(schema.db)){
    # items should be without the standard ones
    std.items<-"sample_id,source,title,journal,author,method,material,type,composition,rock_name,SiO2,TiO2,Al2O3,Cr2O3,Fe2O3,Fe2O3Total,FeO,FeOTotal,NiO,MnO,MgO,CaO,Na2O,K2O,P2O5,LOI,H2O,H2O-,H2O+,S,Co2,F,Cl,Ag,As,Au,B,Ba,Be,Bi,Cd,Co,Cr,Cs,Cu,Ga,Hf,Hg,I,Ir,K,Li,Mn,Mo,Nb,Ni,Os,Pb,Pd,Pt,Rb,Re,S,Sb,Sc,Se,Sn,Sr,Ta,Te,Th,Ti,Tl,U,V,W,Y,Zn,Zr,La,Ce,Pr,Nd,Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,87Sr/86Sr,143Nd/144Nd,206Pb/204Pb,207Pb/204Pb,208Pb/204Pb,176Hf/177Hf,176Lu/177Hf,187Os/188Os,10Be/Be,3He/4He,87Rb/86Sr,147Sm/144Nd,238U/206Pb,235U/207Pb,232Th/208Pb"
    std.items<-tolower(unlist(strsplit(std.items,",")))
    itemlist<-schema.db$itemlist
    eee<-itemlist%in%std.items
    schema.db$itemlist<-itemlist[!eee]
}

##############################################################
.xmlRead<-function(filename){
    db<-xmlTreeParse(filename)
    out<-xmlApply(xmlRoot(db),function(i){ 
        ee<-as.list(xmlSApply(i, xmlValue))
        lab<-ee[1]
        ee<-list(ee[-1])
        names(ee)<-lab
        if(length(ee[[1]])>0){
            labs<-names(ee[[1]])
            ee[[1]]<-strsplit(as.character(ee[[1]]),",")
            names(ee[[1]])<-labs
            
        }
        ee
    })
    out<-out[3:length(out)]
    names(out)<-NULL
    out<-unlist(out,recursive=FALSE)
    return(out)
}  

#############################################################################
#                                                                           #
#                             Main function                                 #
#                                                                           #
#############################################################################

# Return the query or -1 if no hits found
EarthChem<-function(x){
    on.exit(options("show.error.messages"=TRUE))
    options(show.error.messages=FALSE)
    ds<-try(dataset.name.bak<<-get("dataset.name",.GlobalEnv))
    if(class(ds)=="try-error"){
        dataset.name.bak<<-""
    }
    # Default values
    def<-list(author=NULL,title=NULL,journal=NULL,doi=NULL,minpubyear=NULL,maxpubyear=NULL,exactpubyear=NULL,keyword=NULL,sampleid=NULL,
    polygon=NULL,north=NULL,east=NULL,south=NULL,west=NULL,level1=NULL,level2=NULL,level3=NULL,level4=NULL,minage=NULL,maxage=NULL,exactage=NULL,
    geologicalage=NULL,material=NULL,searchtype="rowdata",outputtype="html",jsonfunction=NULL,outputlevel=NULL,#"sample"
    startrow=NULL,endrow=NULL,standarditems="yes",outputitems=NULL,showcolumnnames="yes")
        
    # Arguments from the call shoud overwrite the defaults
    #argus<<-sys.call() 
    #argus[[2]][[1]]<-NULL
    lst<<-x
    def[names(lst)]<-lst
    x<-def
    
    #  Auxiliary function that forms the (url) query(xx is a list with options)
    .formQuery<-function(xx){
        url<-"http://ecp.iedadata.org/restsearchservice?"
        ee<-sapply(names(xx),function(i){
            if(as.character(xx[i])!="NULL"){
                z<-paste(i,"=",xx[[i]],sep="")
                return(z)
            }else{
                invisible()
            }},simplify=TRUE)  
        ee<-paste(ee,collapse="&")
        ee<-gsub("&NULL","",ee)
        url<-paste(url,ee,sep="")
        return(url)
    }
    
    # Start XML library
    options(warn=-1)
    #prompt.bak<-options("prompt")
    #options(prompt="[Earthchem]   ")
    
    uf<-require(XML) 
    if(!uf){winDialog(type = "ok","XML support is absent");stop()}
    if(is.null(x$startrow)&!is.null(x$endrow)) x$startrow<-0

    cat("Calling EarthChem...Please be patient!\n")
    flush.console()
    
    # First, count the hits 
        ask<-x
        ask$searchtype<-"count"
        ask$outputtype<-"html"
        query<-.formQuery(ask)

        nn<-htmlParse(query)
        if(class(nn)=="try-error"){
            cat("...have to set a proxy for you...")
            flush.console()
            Sys.setenv(http_proxy=http_proxy)
        }
        nn<-coerce(nn,"character")
        nn<-sub("(.*<p>)([0-9]+)(.*)","\\2",as.character(nn))    
        nn<-as.numeric(nn)
    # range specified/not specified
    if(!is.null(x$startrow)){
        delta1<-nn
        delta2<-x$endrow-x$startrow
        if(delta1>delta2)nn<-delta2  
    }else{
        delta1<-nn
        delta2<-nn
    }
    query<-.formQuery(x)

    #options(prompt=as.character(prompt.bak))
    
    # No hits
    if(is.na(nn)){
            winDialog(type="ok","Your query produced no matches!")
            options(show.error.messages=FALSE)
            return(-1) 
    }
    
    # Ask whether to retrieve if more than 50 matches found
    if(nn>50){
        ok<-winDialog(paste(delta1," items found. Retrieve ",delta2," of them?",sep=""),type="yesno")
        if(ok=="NO"){
            detach("package:XML")
            cat("Cancelled.\n");options(show.error.messages=FALSE);stop(call. = FALSE)
        }
    }
    
    # Go on and retrieve the stuff
    if(nn<=50){
        which<-c(-1,nn-1)
    }else{
        which<-seq(-1,nn-1,by=49)
        if(which[length(which)]!=(nn-1)) which<-c(which,nn-1)
    }
    
    
    if(.Platform$GUI=="Rgui"){
        pb<-winProgressBar(title = "Importing...", label = "Sample", min = 1, max = delta1, initial = 1, width = 300)
    }else{
        if(!getOption("gcd.shut.up"))pb<-txtProgressBar(title = "Importing...", label = "Sample",min = 1, max = delta1, initial = 1,char = "=",width = NA, style = 3)
    }
    
    
    # Reading HTML
    # 'count' (number of matches) or  'distinctitems' (table of distinct items found) do not work 
    .readEarthChemHTML<-function(query){
        on.exit(options("show.error.messages"=TRUE))
        options("show.error.messages"=FALSE)
        prd<-try(htmlParse(query))
        if(class(prd)=="try-error"){
            close(pb)
            detach("package:XML")
            options("warn"=-1)
            winDialog(type="ok","Unspecified error in search pattern!")
            print(prd)
            print(query)
            options(show.error.messages=FALSE) 
            stop(call. = FALSE)
        }
        prd1<-readHTMLTable(prd)
        if(length(prd1)==0){
            close(pb)
            detach("package:XML")
            winDialog(type="ok","No results found!")
            print(prd)
            print(query)
            options(show.error.messages=FALSE)  
            stop(call. = FALSE)
        }
        zzz<-data.frame(prd1)
        colnames(zzz)<-colnames(prd1[[1]])
        if(.Platform$OS.type=="windows") zzz[zzz=="Â"]<-NA  # Does not work on Mac
        return(zzz)
    }
   
    # Reading XML
    # 'count' (number of matches) or 'distinctitems' (table of distinct items found) do not work 
    .readEarthChemXML<-function(query){    
        db<-.xmlRead(query)
            zzz<-sapply(names(db),function(i){
                z<-as.character(db[[i]])
                return(z)
            })
        zzz<-data.frame(t(zzz))
        colnames(zzz)<-names(db[[1]])
        zzz[zzz=="character(0)"]<-NA
        return(zzz)
    }
   
    # TODO types other  than html and xml not supported yet (including csv)
    incoming<-data.frame(NULL)
    ee<-sapply(1:(length(which)-1),function(i){
         if(.Platform$GUI=="Rgui"){
            setWinProgressBar(pb, i*50, title = NULL, label = paste("Sample",(i-1)*50,"of",delta1))
        }else{
            if(!getOption("gcd.shut.up"))setTxtProgressBar(pb,(i-1)*50, title = NULL, label = paste("Sample",(i-1)*50,"of",delta1))
        }
        #setWinProgressBar(pb,i,title = NULL)
        q<-x
        q$startrow<-which[i]+1
        q$endrow<-which[i+1]
        query<-.formQuery(q)
        where<-c("html","xml")
        options(show.error.messages=FALSE)
        how<-switch(which(where==x$outputtype),
            ".readEarthChemHTML(query)",
            ".readEarthChemXML(query)"
        )
        z<-eval(parse(text=how))
        out<-rbind(incoming,data.frame(z))
        assign("incoming",out,parent.env(environment()))
        return(query)
    })
    close(pb)
    
    detach("package:XML")
    flush.console()
    filename<<-"EarthChem"
    
    if(any(colnames(incoming)=="sample_id")){
        if(all(!duplicated(incoming[,"sample_id"]))){
            rownames(incoming)<-incoming[,"sample_id"]
            incoming<-incoming[,-1]
        }else{
            message<-"WARNING! Duplicated rownames found! Replaced by sequence numbers...."
            winDialog(type="ok",message)
            rownames(incoming)<-1:nrow(incoming)
        }
    }
    cat("\nProcessing..")
    flush.console()
    
    shut.bak<-getOption("gcd.shut.up")
    options("gcd.shut.up"=TRUE)
    
    .loadData.process(incoming,merging=FALSE,GUI=FALSE,source.first=TRUE)
    options("gcd.shut.up"=shut.bak)
    
    cat("..Done!\n")
    flush.console()
    
    dataset.name<-get("dataset.name",.GlobalEnv)
    WRCube[[dataset.name]]$source<-paste("Earthchem,",as.character(Sys.time())) # Indicate that this came from EartChem + time stamp
    WRCube[[dataset.name]]$EarthChem.query.url<-query # Store the query
    WRCube[[dataset.name]]$EarthChem.query.var<-x # Store the query var
    assign("WRCube",WRCube,.GlobalEnv)
    
    #if(dataset.name.bak!="") peekDataset(dataset.name.bak)
    rm("dataset.name.bak",envir=.GlobalEnv)
    ee<-menuOn()
    invisible(query)
}
