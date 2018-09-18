#############################################################################
#                    Plot Gch spiderdiagrams with colours                   #
#           corresponding to an independent variable (such as SiO2)         #
#############################################################################
spider.contour<-function(chondrit=selectNorm(),what=NULL,colour.palette="heat.colors",ymin=0,ymax=0,cex=1,join=TRUE,pch=15,main="",sub="",offset=TRUE,centered=FALSE,xrotate=FALSE,xaxs="r",new=TRUE,legend=TRUE){
    options(warn=-1)
    
    if(is.null(what))GUI<-TRUE else GUI<-FALSE
     
    # Select the normalization scheme
    if(is.character(chondrit)) chondrit<-selectNorm(chondrit)
    model<-rownames(chondrit)
    temp<-filterOut(WR,colnames(chondrit))

    # Select the samples to be plotted
    if(GUI){
        y1<-selectSubset(where=cbind(labels[rownames(temp),],WR[rownames(temp),]),save=FALSE)
    }else{
        y1<-rownames(temp)
    }
    #print(y1)
            
    # Colours
    ee<-assignColVar(what=what,pal=colour.palette,save=FALSE)
    col<-ee$col
    names(col)<-rownames(WR)
    leg<-colnames(ee$leg)
    pal<-ee$leg
    zaxis<-rownames(ee$leg)
    
    # Unbroken patterns?
    if(GUI){
        x<-winDialog(type="yesno","Unbroken patterns?")
        join<-(x=="YES")
    }
    
    # Call the main routine
    A<-temp[y1,]
    A[A==0]<-NA
    if(is.vector(A)){
        A<-t(as.matrix(A))
        rownames(A)<-rownames(temp)[y1]
    }
    
    col<-col[rownames(A)]
    normalized<-spider(A,chondrit,col=col,pch=pch,cex=cex,join=join,legend=FALSE,plot=TRUE,new=new,main=main,sub=sub,offset=offset,centered=centered,xrotate=xrotate,xaxs=xaxs,ymin=ymin,ymax=ymax)
    #par(omd=c(0,1,0,1))
    #par(mar=c(6,4,6,11))
    ymin<-sheet$demo$call$ylim[1]
    ymax<-sheet$demo$call$ylim[2]

    ii<-round(seq(log10(ymin),log10(ymax)),0)
    if(log10(ymin)--ii[1]<1e-5)ii<-ii[-1]
    if(log10(ymax)-ii[length(ii)]<1e-5)ii<-ii[-length(ii)]
    
    # Legend
    if(legend){
        pch<-15
        #x<-par("usr")[2]+0.5
        #y<-10^(diff(par("usr")[3:4])/2+par("usr")[3])
        leg<-list(x="right",inset=c(-0.3,0),legend=leg,col=pal,pch=rep(pch,length(leg)),cex=0.9,pt.cex=1.5,lty="solid",bg="transparent",ncol=1,bty="o",xpd=TRUE,yjust=0.5,title=annotate(zaxis))
        if(screen()){
            screen(scr.old)
            plate[[scr.old]]$demo$legend<<-leg
        }
        sheet$demo$legend<<-leg
        #figRedraw()
   }
   
   if(!getOption("gcd.shut.up")){
        cat("\n")
        cat("Normalized by ",model,":","\n",sep="")
        print(round(results,2))
    }
    figRedraw()
    invisible(results)
}
