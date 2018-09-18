options(warn=-1)

if(!getOption("gcd.shut.up")){
    cat("Processing...Please wait...\n")
    flush.console()
    cat("Identifying the data types present...\n")
}

if(any(colnames(WR)=="SiO2")){
    if(!getOption("gcd.shut.up")) cat("Major elements\n")
    
    # Add missing columns for majors
    ################################

    what<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","MnO","MgO","CaO","Na2O","K2O","H2O.PLUS","CO2","P2O5","F","S")

    #for(f in 1:length(what)){
    #    if(!any(colnames(WR)==what[f])){
    #        WR<-cbind(WR,rep(NA,nrow(WR)))
    #        colnames(WR)[length(colnames(WR))]<-what[f]
    #    }
    #}
    
    prd<-sapply(what,function(f){
        if(all(colnames(WR)!=f,na.rm=TRUE)){
            #cat("missing:",f)
            WR<-cbind(WR,rep(NA,nrow(WR)))
            colnames(WR)[length(colnames(WR))]<-f
            assign("WR",WR,pos=parent.env(environment()))   
        }
    },simplify=TRUE)
    
    # Molecular weights
    #########################

    oxides<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","FeOt","MnO","MgO","CaO","Na2O","K2O","H2O.PLUS","CO2","P2O5","F","S")
    oxides.simple<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","FeO","MnO","MgO","CaO","Na2O","K2O","H2O","CO2","P2O5","F","S")
    #fact<-c(1,1,2,2,1,1,1,1,1,2,2,2,1,2,1,1)
    #names(fact)<-oxides

    if(length(ls(pat="^MW$"))==0){
        # Molecular weights -> MW, Numbers of atoms->x.atoms, Oxygen factors -> x.oxygen 
        mol.wt<-sapply(oxides.simple,molecularWeight)
        colnames(mol.wt)<-oxides

        # Initalize the data variables 
        attach(data.frame(t(mol.wt)))
        names(MW)<-colnames(mol.wt)
        names(x.atoms)<-colnames(mol.wt)
    }

    if(!getOption("gcd.shut.up")){
        cat("...")
        flush.console()
    }
    
    #Various useful parameters, append a column if missing completely
    what<-c("P","K","Ti","Ba","Sr","Cr2O3","FeOt","mg#","Mg#","A/NK","A/CNK","K2O/Na2O")
    for(f in 1:length(what)){
        if(!any(colnames(WR)==what[f])){
            WR<-cbind(WR,rep(NA,nrow(WR)))
            colnames(WR)[length(colnames(WR))]<-what[f]
        }
    }
    
    WR<-addOn("P",WR[,"P2O5"]*0.43646*1e4)
    WR<-addOn("K",WR[,"K2O"]*0.83013*1e4)
    WR<-addOn("Ti",WR[,"TiO2"]*0.5995*1e4)
    WR<-addOn("Ba",WR[,"BaO"]*0.89567*1e4)
    WR<-addOn("Sr",WR[,"SrO"]*0.84560*1e4)
    WR<-addOn("Cr2O3",WR[,"Cr"]*1.4615*1e-4)

    # Iron recalculation, FeOt if both FeO and Fe2O3 are available
    x<-WR[,"FeO"]
    y<-WR[,"Fe2O3"]
    x[is.na(x)]<-0
    y[is.na(y)]<-0
    #WR<-addOn("FeOt",x+y*0.89981)
    WR<-addOn("FeOt",x+y*MW["FeO"]/MW["Fe2O3"]*2)

    # Fe2O3t to be converted to FeOt
    if(any(colnames(WR)=="Fe2O3t")){
        WR[WR[,"FeOt"]==0,"FeOt"]<-WR[WR[,"FeOt"]==0,"Fe2O3t"]*MW["FeO"]/MW["Fe2O3"]*2
        #WR[WR[,"FeOt"]==0,"FeOt"]<-WR[WR[,"FeOt"]==0,"Fe2O3t"]*0.89981
    }
    
    # Zeros are the same as missing
    WR[WR[,"FeOt"]==0,"FeOt"]<-NA

    # Potassium vs. sodium balance by weight
    WR<-addOn("K2O/Na2O",WR[,"K2O"]/WR[,"Na2O"])

    # Recalc to anhydrous basis
    ###########################
    anhydrous.short<-c("SiO2","TiO2","Al2O3","FeOt","MnO","MgO","CaO","Na2O","K2O","P2O5")
    anhydrous.long<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","MnO","MgO","CaO","Na2O","K2O","P2O5")
    
    if(all(is.na(WR[,"Fe2O3"]))){
        anhydrous<-anhydrous.short
    }else{
        anhydrous<-c(anhydrous.long,"FeOt")
    }
    
    WRanh<-matrix(data = NA, ncol = length(anhydrous), nrow = nrow(WR), byrow = TRUE, dimnames = NULL)
    rownames(WRanh)<-rownames(WR)
    colnames(WRanh)<-anhydrous
    
    for (f in seq(1,nrow(WR))){
        if(is.na(WR[f,"Fe2O3"])){
            WRanh[f,anhydrous.short]<-WR[f,anhydrous.short]/sum(WR[f,anhydrous.short],na.rm=TRUE)*100
        }else{
            WRanh[f,anhydrous.long]<-WR[f,anhydrous.long]/sum(WR[f,anhydrous.long],na.rm=TRUE)*100
        }
    }
    
    # Calculate millications
    millications(WR,save=TRUE)

    if(!getOption("gcd.shut.up")){
        cat("...")
        flush.console()
    }
    
    # mg# from total Fe, always there 
    WR<-addOn("mg#",100 * milli[,"MgO"]/(WR[,"FeOt"]/MW["FeO"]*1000 + milli[,"MgO"]))
    
    # Mg# just for samples where both FeO and Fe2O3 are available
    if(any(colnames(milli)=="FeO")&any(colnames(milli)=="Fe2O3")){
        ee<-milli
        ee[ee==0]<-NA
        WR<-addOn("Mg#",100 * ee[,"MgO"]/(ee[,"FeO"] + ee[,"MgO"]))
    }
    
    # Peraluminosity, Peralkalinity
    #WR<-addOn("A/NK",(WR[,"Al2O3"]/MW["Al2O3"])/(WR[,"Na2O"]/MW["Na2O"] + WR[,"K2O"]/MW["K2O"]))
    #WR<-addOn("A/CNK",(WR[,"Al2O3"]/MW["Al2O3"])/(WR[,"Na2O"]/MW["Na2O"] + WR[,"K2O"]/MW["K2O"]+WR[,"CaO"]/MW["CaO"]))

    WR<-addOn("A/NK",milli[,"Al2O3"]/(milli[,"Na2O"] + milli[,"K2O"]))
    WR<-addOn("A/CNK",milli[,"Al2O3"]/(milli[,"Na2O"] + milli[,"K2O"]+2*milli[,"CaO"]))
    
    assign("WR",WR,.GlobalEnv)
    if(!getOption("gcd.shut.up")){
        cat("... done!\n")}
        flush.console()
}
# End of majors block

#############################################################################
#               Some useful symbolic names, default grouping                #
#                                                                           #
#############################################################################
where<-colnames(WR)

LILE<-c("Rb","Sr","Ba","K","Cs","Li")
HFSE<-c("Zr","Hf","Nb","Ta","Ti","La","Ce","Y","Ga","Sc","Th","U") #http://www.springerreference.com/docs/html/chapterdbid/30013.html
# REE and major have been defined already in onLoad

REE<-as.vector(charmatch(REE,where))
REE<-REE[!is.na(REE)]
REE<-where[REE]

LILE<-as.vector(charmatch(LILE,where))
LILE<-LILE[!is.na(LILE)]
LILE<-where[LILE]

HFSE<-as.vector(charmatch(HFSE,where))
HFSE<-HFSE[!is.na(HFSE)]
HFSE<-where[HFSE]
if(length(LILE)>0|length(HFSE)>0|length(REE)>0){
    if(!getOption("gcd.shut.up")){
        cat("Trace elements \n")
        cat("......... done!\n")
    }
}

# Isotopes Sr-Nd?
if(any(colnames(WR)=="87Sr/86Sr")|(any(colnames(WR)=="143Nd/144Nd"))){
     if(!getOption("gcd.shut.up")){
        cat("Sr and/or Nd isotopes\n")
        cat("......... done!\n")
    }
}

# Isotopes Hf?
if(length(grep("^176Hf/177Hf",colnames(WR)))>0){
     if(!getOption("gcd.shut.up")){
        cat("Hf isotopes\n")
        cat("......... done!\n")
    }
}

# Setup up the defaults groups
labels$Symbol<-as.vector(labels$Symbol)
groups<-labels[,"Symbol"]
grouping<-which(colnames(labels)=="Symbol")

# PDF options
pdf.options(useDingbats=FALSE) # IMPORTANT
pdf.options(pointsize=14)
    
# Make a classification list
claslist<-.claslist()
options(prompt="GCDkit-> ")

if(getOption("gcd.menus")!=""){
#if(.Platform$GUI=="Rgui"){
    menuOn()
    figaroOff()
}
