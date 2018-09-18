# 1. zavolas .buildPatch(c("Jensen","loadData")) ap, jako druhy parametr muzes mit cislo patche, jinak je podle aktualniho data. about() se aktualizuje automaticky
# 2. v adresari Patch se vytvori soubor installPatchXXXXXX.r
# 3. ten muzes sourcovat a v adresari Patch ti to udela vlastni zaplatu
# 4. .loadPatch by se mel volat na konci startu systemu, nacte posledni patch z adresare (podle cisla) a vsechny ne-patch soubory

.loadPatch<-function(){
    x<-list.files(paste(gcdx.dir,"/Patch",sep=""),pattern="[a-zA-Z0-9]+.[rR]$") #all from Patch dir
    
    otherToLoad<-x[grep("GCDpatch[0-9]{6}.[rR]$",x,invert=TRUE)]                #select other files
    for (i in otherToLoad){                                                     #load them
        cat("Loading file: ",i,"...\n")
        source(paste(gcdx.dir,"\\Patch\\",i,sep=""))
        cat("...Ok\n")
    }
   
    patchToLoad<-x[grep("GCDpatch[0-9]{6}.[rR]$",x)]                            #select patchfiles
    if(length(patchToLoad)>0){
        patchToLoad<-sort(patchToLoad,TRUE)[1]                                      #select newest patch
        cat("Loading patch: ",patchToLoad,"...\n",sep="")
        source(paste(gcdx.dir,"\\Patch\\",patchToLoad,sep=""))                      #source newest patch
        cat("...Ok\n")
    }   
}

.buildPatch<-function(updatedFn,patchID=format(Sys.Date(),"%y%m%d")){
    patchInstallName<-paste(gcdx.dir,"/Patch/installPatch",patchID,".r",sep="")
    patchName<-paste(gcdx.dir,"/Patch/GCDpatch",patchID,".r",sep="")
    cat("\nBuilding patch",patchID,".r install file\n",sep="")
    cat("#Install file for GCDkit 3.00 patch no",patchID,"\n",file=patchInstallName)
    cat("#The patch may be installed by:\n",file=patchInstallName,append=TRUE)
    cat("#   * copying the install file into directory GCDkit/Patch \n",file=patchInstallName,append=TRUE)
    cat("#   * drag & drop of this install file into the R Console \n",file=patchInstallName,append=TRUE)
    cat("#   * manual pasting of the install file content into the R Console (ctrl-C, ctrl-V)\n",file=patchInstallName,append=TRUE)
    cat("\n",file=patchInstallName,append=TRUE)
    cat("patchID<-\"",patchID,"\"\n",sep="",file=patchInstallName,append=TRUE)
    cat("if (file.access(paste(gcdx.dir,\"/Patch/patch\",patchID,\".r\",sep=\"\"), mode = 4)==-1){\n",sep="",file=patchInstallName,append=TRUE)
    cat("cat (\"Installing the file patch\",patchID,\".r\\n\",sep=\"\")\n",file=patchInstallName,append=TRUE)
    cat("patch<-c(","\n",file=patchInstallName,append=TRUE)
    cat("\"#GCDkit 3.00 patch no",patchID,"\",","\n",file=patchInstallName,append=TRUE)
    for (fnName in updatedFn){
        deparsed<-deparse(eval(parse(text=fnName)))
        cat("\"\",\n",sep="",file=patchInstallName,append=TRUE)
        cat("\"#updated function ",fnName,"()\",\n",sep="",file=patchInstallName,append=TRUE)
        cat("\"",fnName,"<-\",",sep="",file=patchInstallName,append=TRUE)
        for (fnLine in deparsed) {
            cat(deparse(fnLine),",\n",file=patchInstallName,append=TRUE)
        }
    }
    #update the about() file 
    fnName<-deparse(about)
    myMonths<-c('January','February','March','April','May','June','July','August','September','October','November','December')
    patchDate<-paste(as.numeric(substr(patchID,5,6))," ", myMonths[as.numeric(substr(patchID,3,4))]," 20", as.numeric(substr(patchID,1,2)),sep="")
    aboutReplacement<-paste("message <- \"GCDkit Win 3.00 patch ",patchID," [",patchDate,"]\\\\n (c) 1999-20",substr(patchID,1,2),sep="")
    deparsed<-sub("message <-[^(]+[0-9c() -]+",aboutReplacement,fnName)
    cat("\"\",\n",sep="",file=patchInstallName,append=TRUE)
    cat("\"#updated function about()\",\n",sep="",file=patchInstallName,append=TRUE)
    cat("\"about<-\",",sep="",file=patchInstallName,append=TRUE)
    for (fnLine in deparsed) {
        cat(deparse(fnLine),",\n",file=patchInstallName,append=TRUE)
    }


    cat("\"#end\")\n",file=patchInstallName,append=TRUE)

    cat("cat(patch,file=\"",patchName,"\",sep=\"\\n\")\n",sep="",file=patchInstallName,append=TRUE)
    cat("source(\"",patchName,"\")\n",sep="",file=patchInstallName,append=TRUE)
    cat("}",file=patchInstallName,append=TRUE)

}
