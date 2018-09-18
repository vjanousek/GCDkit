###########################################################################################
#                                                                                         #
# Pawlowsky-Glahn V, Egozcue JJ (2006) Compositional data and their analysis:             #
# an introduction. In: Buccianti A, Mateu-Figueras G, Pawlowsky-Glahn V (eds)             #
# Compositional Data Analysis in the Geosciences. Geological Society Special Publications #
# 264, London, pp 1–10                                                                    #
#                                                                                         #
###########################################################################################

# Centered log-ratio transformation - clr
clr.trans<-function(comp.data=NULL,GUI=FALSE){
    elems<-"SiO2,TiO2,Al2O3,FeOt,MnO,MgO,CaO,Na2O,K2O"
    
    if(is.null(comp.data)) comp.data<-unlist(strsplit(elems,","))
    if(GUI){
        which.samples<-selectSamples(print=TRUE)
        comp.data<-selectColumnsLabels(default=elems,print=FALSE)
    }else{
        which.samples<-rownames(comp.data)
    }
    
    if(is.vector(comp.data)){
        comp.data<-comp.data[comp.data%in%colnames(WR)]
        comp.data <- filterOut(WR, comp.data)
        ee<-apply(comp.data,2,function(i)all(is.na(i)))
        comp.data <- comp.data[,!ee]
    }    
    
    # Select samples
    if(GUI){
        ee<-rownames(comp.data)%in%which.samples
        comp.data<-comp.data[ee,] 
    }
    
    out<-sapply(1:nrow(comp.data),function(i){
        x<-comp.data[i,]
        # Geometric mean - fancy
        #if(i==74) browser()
        gm<-exp(mean(log(x[x>0])))
        z<-log(comp.data[i,]/gm)
        z[which(z==Inf|z==-Inf)]<-NA
        return(z)
    })
  
    out<-t(out)
    rownames(out)<-rownames(comp.data)
    ox.tr<-paste(colnames(comp.data),"clr",sep="_")
    colnames(out)<-ox.tr
    assign("results",out,.GlobalEnv)
    return(out)
}

# Principal components from scaled matrix - clr
pr.comp.clr<-function(comp.data="SiO2,TiO2,Al2O3,FeOt,MnO,MgO,CaO,Na2O,K2O",cor=TRUE,GUI=FALSE){
    shutup.bak<-getOption("gcd.shut.up")
    options("gcd.shut.up"=TRUE)
    trandak<-clr.trans(comp.data,GUI=FALSE)
    colnames(trandak)<-gsub("_clr","",colnames(trandak))
    pc.cr <-prComp(trandak,use.cov=!cor,GUI=FALSE)
    options("gcd.shut.up"=shutup.bak)
    invisible(pc.cr)
}

.closureMenu<-function(){
    on.exit(options("show.error.messages"=TRUE))
    
    where<-c("Centred-log-ratio (clr) transformation","Biplot of pricipal components (clr data)")
    selected<-select.list(where, multiple = FALSE, title = "Select the accessory")
    if(selected==""){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    ee<-switch(which(where==selected),
        "clr.trans(GUI=TRUE)",
        "pr.comp.clr(GUI=TRUE)",
    )
    cat("GCDkit->",ee,"\n")
    .save2hist(ee)
    eval(parse(text=ee))
}    
    
if(getOption("gcd.menus")!=""){winMenuAddItem("Plugins","Closure effect",".closureMenu()")}
