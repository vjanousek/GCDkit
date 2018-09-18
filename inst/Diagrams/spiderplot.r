#############################################################################
#                    General spider diagram                                 #
#  normalizing values are in spider.data, for REE calculates extra params   #
#############################################################################
spider.individual<-function(new=TRUE){
    options(warn=-1)

    # Select the normalization scheme
    chondrit<-selectNorm()
    model<-rownames(chondrit)
    temp<-filterOut(WR,colnames(chondrit))

    # Select the samples to be plotted
    y1<-selectSubset(where=cbind(labels[rownames(temp),],WR[rownames(temp),]),save=FALSE)
    cex<-labels[y1,"Size"]
    # Select plot properties
    x<-winDialog(type="yesno","Use the assigned symbols/colours?") 
    if(x=="YES"){
        col<-labels[y1,"Colour"]
        pch<-labels[y1,"Symbol"]
        leg<-FALSE
    }else{
        ee<-.autoassign.pch(what=y1,edit=TRUE,legend=FALSE)
        col<-ee$col
        pch<-ee$pch
        leg<-TRUE
        cex<-as.numeric(winDialogString("Scaling factor","1"))
        if(length(cex)==0){cat("Cancelled.\n");return()} 
    }

    x<-winDialog(type="yesno","Unbroken patterns?")
    join<-(x=="YES")

    # Call the main routine
    A<-temp[y1,]
    A[A==0]<-NA
    if(is.vector(A)){
        A<-t(as.matrix(A))
        rownames(A)<-rownames(temp)[y1]
    }
    
    normalized<-spider(A,chondrit,col=col,pch=pch,cex=cex,join=join,sub=selected,legend=leg,new=new,offset=TRUE, xrotate=FALSE, centered=FALSE)
    
    ymin<-sheet$demo$call$ylim[1]
    ymax<-sheet$demo$call$ylim[2]

    ii<-round(seq(log10(ymin),log10(ymax)),0)
    if(log10(ymin)--ii[1]<1e-5)ii<-ii[-1]
    if(log10(ymax)-ii[length(ii)]<1e-5)ii<-ii[-length(ii)]

    # Legend
    #if(leg){
    #   x<-par("usr")[2]+0.5
    #   y<-10^(diff(par("usr")[3:4])/2)
    #   sheet$demo$legend<<-list(x=x,y=y,legend=y1,col=col,pch=pch,cex=0.7,pt.cex=cex,lty="solid",bg="transparent",ncol=2,bty="o",xpd=TRUE,yjust=0.5)
    #   figRedraw()
    #}
    
    if(!getOption("gcd.shut.up")){
        cat("\n")
        cat("Normalized by ",model,":","\n",sep="")
        print(round(results,2))
    }
    figRedraw()
    invisible(results)
}
