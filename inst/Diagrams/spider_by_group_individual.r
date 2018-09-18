#############################################################################
#                    General spider diagram                                 #
#  normalizing values are in spider.data, for REE calculates extra params   #
#############################################################################

spiderByGroupPatterns<-function(rock=WR,norm=NULL,bw=FALSE,ymin=0,ymax=0,xrotate=FALSE,offset=TRUE,centered=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    options(warn=-1)
    if(is.null(norm)) chondrit<-selectNorm() else chondrit<-selectNorm(norm)
    model<-rownames(chondrit)
    temp<-filterOut(WR,colnames(chondrit))

    fact.groups<-factor(groups)
    newplot<-TRUE
    ee<-.normalization(temp,chondrit)
    if(ymin==0|ymax==0){
        ymin<-min(ee[ee>0],na.rm=TRUE) #NEW
        ymin<-10^floor(log(ymin,10))
        ymax<-max(ee[ee>0],na.rm=TRUE) #NEW
        ymax<-10^ceiling(log(ymax,10))
    
        yy<-winDialogString("Enter min and max for y-axis, separated by commas",paste(ymin,ymax,sep=","))
        if(is.null(yy)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
        ee<-unlist(strsplit(yy,","))
        if(length(ee)<=1) {winDialog("ok","Invalid limits!");options("show.error.messages"=FALSE);stop()}
    ymin<-as.numeric(ee[[1]])
    ymax<-as.numeric(ee[[2]])
    }
    
    if(is.na(ymin)|is.na(ymax)){winDialog("ok","Invalid limits!");options("show.error.messages"=FALSE);stop()}
    if(ymin<=0)ymin<-0.01

    results<-list()
    
    for(i in 1:length(levels(fact.groups))){   
       #cat("\n",colnames(labels)[grouping]," = ",levels(fact.groups)[i],":\n",sep="")
       if (grouping>0){
            cat("\n",colnames(labels)[grouping]," = ",levels(fact.groups)[i],":\n",sep="")
       }else{
            cat("\nGroups = ",levels(fact.groups)[i],":\n",sep="")
       }
       #cat("\n",groups," = ",levels(fact.groups)[i],":\n",sep="")
       ee<-rownames(labels)[groups==levels(fact.groups)[i]]
       #ee<-rownames(labels)[labels[,grouping]==levels(fact.groups)[i]]
       y1<-match(ee,rownames(temp))    
       y1<-y1[!is.na(y1)]

    if (length(y1)==0){
        txt<-"No samples found with sufficient data required by the diagram"
        cat(txt,"\n\n")
        results[i]<-txt
    }else{
        if(bw) col<-rep("black",length.out=length(y1)) else col<-rep(barvy,length.out=length(y1))
        pch<-rep(1:18,length.out=length(y1))
        cex<-1.5
        #cat(rownames(temp)[y1],"\n\n")
        A<-temp[y1,]
        A[A==0]<-NA
        if(is.vector(A)){
            A<-t(as.matrix(A))
            rownames(A)<-rownames(temp)[y1]
        }
        normalized<-spider(A,chondrit,ymin=ymin,ymax=ymax,plot=TRUE,join=TRUE,field=FALSE,legend=TRUE,add=FALSE,cex=cex,pch=pch,col=col,shaded.col=col[i],sub=levels(fact.groups)[i],,xrotate=xrotate,offset=offset,centered=centered)
        ii<-round(seq(log10(ymin),log10(ymax)),0)
        #if(log10(ymin)--ii[1]<1e-5)ii<-ii[-1]
        #if(log10(ymax)-ii[length(ii)]<1e-5)ii<-ii[-length(ii)]
        #abline(h=10^ii,col="gray",lty="dotted")
        newplot<-FALSE

        rownames(normalized)<-rownames(A)
        #colnames(normalized)<-paste(colnames(normalized),"N",sep="")
        out<-normalized
    
        #Specific for REE
        if(substr(model,1,3)=="REE"){
            Eu<-normalized[,"EuN"]/sqrt(normalized[,"SmN"]*normalized[,"GdN"])
            out<-cbind(normalized,Eu,
                normalized[,"LaN"]/normalized[,"YbN"],
                normalized[,"LaN"]/normalized[,"SmN"],
                normalized[,"EuN"]/normalized[,"YbN"],
                normalized[,"CeN"]/normalized[,"YbN"],
                normalized[,"CeN"]/normalized[,"SmN"]
            )    
            if(is.matrix(A)) sum.ree<-apply(A,1,sum,na.rm=TRUE) else sum.ree<-sum(A,na.rm=TRUE)
            out<-cbind(out,sum.ree)
            colnames(out)[(ncol(chondrit)+1):(ncol(out))]<-c("Eu/Eu*","LaN/YbN","LaN/SmN","EuN/YbN","CeN/YbN","CeN/SmN","Sum_REE")
        }
        cat("\n")
        cat("Normalized by ",model,":","\n",sep="")
        print(round(out,2))
        results[[i]]<-out
        #results[[levels(fact.groups)[i]]]<-out
    
       # Legend
        #x<-par("usr")[2]+0.5
        #y<-10^(diff(par("usr")[3:4])/2)
        #legend(x=x,y=y,rownames(temp)[y1],bty=1,col=col,lty="solid",bg="white",pch=pch,cex=0.8,xpd=TRUE,yjust=0.5)
        #sheet$demo$legend<<-list(x=x,y=y,legend=y1,col=col,pch=pch,cex=0.8,xpd=TRUE,yjust=0.5)
     }
    }
    names(results)<-levels(fact.groups)
    results<<-results
}
