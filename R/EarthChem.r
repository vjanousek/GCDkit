.EarthChemStart<-function(){   
    on.exit(options("show.error.messages"=TRUE))
    cat("\nInitializing the EarthChem interface....\n")
    
    if(.Platform$OS.type!="windows"){
        schema.db<<-NULL
        cat("Testing - on Non-Windows system - skipping Eartchem interface\n")
        return()
    }
    
    #############################################################################
    #                                                                           #
    #                          Autoset proxy if necessary                       #
    #                                                                           #
    #############################################################################
    uf<-requireNamespace("curl",quietly=TRUE)
    #uf<-require(curl)
    http_proxy<-""
    
    if(!uf){
        cat("curl support is absent...you may like to set up proxy manually (if present)\n")
        #http_proxy<-"http://cache.cgu.cz:8080"  #### Default CGU proxy
    }else{
        prox<-curl::ie_proxy_info()$Proxy
        if(is.null(prox)) prox<-""
        if(prox!="") http_proxy<-paste("http://",prox,"/",sep="")
        unloadNamespace("curl")
        #detach("package:curl")
    }

    rproxy<<-Sys.getenv("http_proxy")
    cat("R proxy:",rproxy,"\n")         
    cat("System proxy:",http_proxy,"\n")
    
    if(rproxy=="" & http_proxy!=""){
        Sys.setenv(http_proxy=http_proxy)    #### IMPORTANT
        cat("System proxy detected! R proxy set to",http_proxy,"\n")   
    }
    
    # Source the functions to access EarthChem 
    earthchem.dir<<-paste(gcdx.dir,"EarthChem",sep="/")
    source(paste(earthchem.dir,"EarthChemFUN.r",sep="/"))
    
    if(!is.list(schema.db)){
        if(schema.db==-1)return() # No Internet connection
    }
    
    itemlist<<-schema.db[["itemlist"]] #source("outputitems.r")
    source(paste(earthchem.dir,"rockclass.r",sep="/"))
    agelist<<-schema.db[["agelist"]] #source("geolage.r")
    
    #############################################################################
    #                                                                           #
    #      Load  the GUI, selection from World map only on Win RGUI             #
    #                                                                           #
    #############################################################################
    
    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
        cat("\n")
        flush.console()
        #uf<-requireNamespace("rgdal",quietly=TRUE)
        uf<-require(rgdal)
        #if(!uf){winDialog(type = "ok","Package rgdal not found, no map support available!")}
        if(!uf){cat("**** Package rgdal not found, no map support available! ****\n")}
        cat("\n")
        flush.console()
        options("show.error.messages"=FALSE)
        #uf<-requireNamespace("tkrplot",quietly=TRUE)
        uf<-require(tkrplot)
        #if(!uf){winDialog(type = "ok","Package tkrplot not found, no map support available!")}
        if(!uf){cat("**** Package tkrplot not found, no map support available! ****\n")}
         
        source(paste(earthchem.dir,"SelectPgonMap.r",sep="/"))
        source(paste(earthchem.dir,"common_mytkrplot.r",sep="/"))
        source(paste(earthchem.dir,"common_graphcoord.r",sep="/"))
        
        # Compile some of the functions to make them less slow
        selectPgonMapC<<-cmpfun(selectPgonMap)
        tkrreplot.pxC<<-cmpfun(tkrreplot.px)
        
    }
    source(paste(earthchem.dir,"EarthChemGUI.r",sep="/"))
    EarthChemC<<-cmpfun(EarthChem)
}
