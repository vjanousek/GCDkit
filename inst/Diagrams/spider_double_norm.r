#############################################################################
#        Double normalized spider diagram after Thompson or Pearce          #
#             normalizing values are in spider.data                         #
#############################################################################


spider2norm<-function(rock=WR,norm=NULL,norm2=NULL,ymin=0,ymax=0,which=rep(TRUE,nrow(rock)),legend=FALSE,pch=labels$Symbol,col=labels$Colour,cex=labels$Size,plot=TRUE,join=TRUE,shaded.col="gray",density=-1,angle=0,xaxs="r",fill.col=FALSE,field=FALSE,add=FALSE,...){
    if(length(pch)==1 & !is.vector(rock)) pch<-rep(pch,nrow(rock)) 
    if(length(col)==1 & !is.vector(rock)) col<-rep(col,nrow(rock))
    if(length(cex)==1 & !is.vector(rock)) cex<-rep(cex,nrow(rock)) 

    options(warn=-1) 
    # Select the normalization scheme
    chondrit<-selectNorm(norm)
    model<-rownames(chondrit)
    temp<-filterOut(WR,colnames(chondrit))
    
    if(is.null(norm)){
        # Select the samples to be plotted
        i<-rownames(WR)[which]%in%rownames(temp)
        y1<-selectSubset(where=cbind(labels[i,],WR[i,]),save=FALSE)
        if(is.null(y1)){cat("Cancelled.\n");ident<-FALSE;return()}
    }else{
        i<-rownames(temp)%in%rownames(WR)[which]
        y1<-rownames(temp)[i]
    }
    
    if(is.null(norm)){
        # Select plot properties
            x<-winDialog(type="yesno","Use the assigned symbols/colours?") 
            if(x=="YES"){
            col<-col[rownames(WR)%in%y1]
            pch<-pch[rownames(WR)%in%y1]
            cex<-labels[rownames(WR)%in%y1,"Size"]
        }else{
            ee<-.autoassign.pch(what=y1,edit=TRUE,legend=FALSE)
            col<-ee$col
            pch<-ee$pch
            names(pch)<-y1
            legend<-TRUE
            cex<-as.numeric(winDialogString("Scaling factor","1"))
            if(length(cex)==0){cat("Cancelled.\n");return()} 
        }
        x<-winDialog(type="yesno","Unbroken patterns?")
        join<-(x=="YES")
    }else{
            i<-rownames(labels)%in%y1
            col<-col[i]
            pch<-pch[i]
            cex<-cex[i]
     }
    # Call the main routine
    A<-temp[y1,]
    A[A==0]<-NA
    
    ee<-.normalization(A,chondrit)
    if(all(colnames(temp)!=norm2)){
        norm2<-selectColumnLabel(colnames(ee), message = "Select a variable for double normalization",default = colnames(temp)[ncol(temp)], sample.names = FALSE, silent = TRUE, empty.ok = FALSE)
        norm2<-colnames(ee)[norm2]
    }
    
    if(!getOption("gcd.shut.up")){
        cat("\n")
        cat("Single normalized to ",model,"\n",sep="")
        print(round(ee,3))
    }
    
    ee<-ee[!is.na(ee[,norm2]),]
    ee<-ee/rep(ee[,norm2],ncol(ee))
    
    chondrit<-rep(1,length(chondrit))
    chondrit<-t(as.matrix(chondrit))
    colnames(chondrit)<-colnames(ee)    
    rownames(chondrit)<-paste(model, " and ", norm2, "[N]=1",sep="")
    
    #which<-y1%in%rownames(ee)
    bak<-getOption("gcd.shut.up")
    options("gcd.shut.up"=TRUE)

    normalized<-spider(ee,chondrit,col=col,pch=pch,cex=cex,join=join,main=rownames(chondrit),sub=annotate(selected),legend=legend,ymin=ymin,ymax=ymax,plot=plot,field=field,angle=angle,density=density,shaded.col=shaded.col,fill.col=fill.col,add=add,...)
    colnames(normalized)<-paste(colnames(normalized),"N",sep="")
    options("gcd.shut.up"=bak)
    if(!getOption("gcd.shut.up")){
        cat("\n")
        cat("Double normalized to ",model,"and ",norm2,"N = 1:","\n",sep="")
        print(round(normalized,3))
    }
    
    # Legend
    # if(legend){
    #    x<-par("usr")[2]+0.5
    #    y<-10^(diff(par("usr")[3:4])/2)
    #    legend(x=x,y=y,y1,bty=1,col=col,lty="solid",bg="white",pch=pch,cex=0.8,xpd=TRUE,yjust=0.5)
    #    sheet$demo$legend<<-list(x=x,y=y,legend=y1,col=col,pch=pch,cex=0.8,xpd=TRUE,yjust=0.5)
    #}
    results<<-normalized
    return(results)
}
