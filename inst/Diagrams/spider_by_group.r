#############################################################################
#                    General spider diagram                                 #
#  normalizing values are in spider.data, for REE calculates extra params   #
#############################################################################
spiderByGroupFields<-function(rock=WR,norm=NULL,bw=FALSE,fill=FALSE,ymin=0,ymax=0,xrotate=FALSE,offset=TRUE,centered=FALSE){
    on.exit(options("show.error.messages"=TRUE))
    shown<-NULL
    options(warn=-1)
    if(is.null(norm)) chondrit<-selectNorm() else chondrit<-selectNorm(norm)
    model<-rownames(chondrit)
    temp<-filterOut(rock,colnames(chondrit))
    fact.groups<-factor(groups)
    newplot<-TRUE
            
    times<-length(levels(fact.groups))
    angle<-rep(c(0,45,-45,60,-60),times=times)
    density<-c(rep(0.02,times=3),rep(0.01,times=times))

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
        if(is.na(ymin)|is.na(ymax)){winDialog("ok","Invalid limits!");options("show.error.messages"=FALSE);stop()}
        if(ymin<=0)ymin<-0.01  
    }
    
    if(is.null(norm)){
        plot.col<-winDialog(type="yesno","Plot in colour?") 
        bw<-plot.col=="NO"
    }
    if(bw){
             col<-rep("black",times=times)
    }else{
            col<-rep(barvy,times=times)
    }    
     
     if(is.null(norm)){      
            fill.col<-winDialog(type="yesno","Fill the fields?") 
            fill<-fill.col=="YES"
     }       
     
     if(fill){    
        shaded.col<-barvy[1:times]
        std2<-as.character(as.hexmode(col2rgb(shaded.col)))
        shaded.col<-paste("#",toupper(apply(std2,2,paste,collapse="")),"aa",sep="") # the last two digits are alpha channel
        fill.col<-TRUE
     }else{
        shaded.col<-rep(NA,times=times)
     }

    for(i in 1:times){
        if (grouping>0){
            cat("\n",colnames(labels)[grouping]," = ",levels(fact.groups)[i]," (",rep(barvy,times=500)[i],"):\n",sep="")
        }else{
            cat("\nGroups = ",levels(fact.groups)[i],":\n"," (",rep(barvy,times=500)[i],"):\n",sep="")
        }
        #cat("\n",colnames(labels)[grouping]," = ",levels(fact.groups)[i]," (",rep(barvy,times=500)[i],"):\n",sep="")
        ee<-rownames(labels)[groups==levels(fact.groups)[i]]
        y1<-match(ee,rownames(temp))    
        y1<-y1[!is.na(y1)]

    if (length(y1)==0){
        cat("No samples found with sufficient data required by the diagram","\n\n")
    }else{
        #cat(rownames(temp)[y1],"\n\n")
        A<-temp[y1,]
        A[A==0]<-NA
        if(!fill){
            normalized<-spider(A,chondrit,plot=TRUE,join=TRUE,field=TRUE,legend=TRUE,add=!newplot,col=col[i],shaded.col=shaded.col[i],fill.col=FALSE,density=density[i],angle=angle[i],ymin=ymin,ymax=ymax,new=TRUE,xrotate=xrotate,offset=offset,centered=centered)
        }else{
            normalized<-spider(A,chondrit,plot=TRUE,join=TRUE,field=TRUE,legend=TRUE,add=!newplot,shaded.col=shaded.col[i],fill.col=TRUE,ymin=ymin,ymax=ymax,xrotate=xrotate,offset=offset,centered=centered)
        }
        newplot<-FALSE
        shown<-c(shown,i)
    }
    }

    # Legend
    old.par <- par("mai")
    par(mai=c(0.1,0.1,0.1,0.1))
    #x<-par("usr")[2]+0.5
    #x<-"right"
    #y <- 10^(diff(par("usr")[3:4])/2+par("usr")[3]) 
    #y<-10^(diff(par("usr")[3:4])/2) # AULD

    if(fill){
        #legend(x,y,levels(fact.groups)[shown],inset=-0.5,bty="n",col=shaded.col[shown],pt.cex=2,pch=15,cex=0.75,xpd=TRUE,yjust=0.5,x.intersp=2, y.intersp=2)
        # Legend
        x<-par("usr")[2]+0.5
        y <- 10^(diff(par("usr")[3:4])/2+par("usr")[3]) # Fixed by Jeff
        pch<-15
        leg<-levels(fact.groups)[shown]
        legend(x,y,leg,col=shaded.col[shown],pch=rep(pch,length(leg)),cex=0.9,pt.cex=1.5,bg="transparent",ncol=1,bty="o",xpd=TRUE,yjust=0.5,y.intersp=2)
    }else{
        par(new=TRUE)
        plot(1,1,axes=FALSE,type="n",xlim=c(0,1),ylim=c(0,1))
        par(new=FALSE)
        x<-length(shown)
        sy<-0.02
        y<-0.07
        txt<-levels(fact.groups)[shown]
        rect(rep(0.8,x),0.8-(1:x)*y+sy,rep(0.85,x),0.8-(0:(x-1))*y,col=col[shown],density=density[shown]*1200,angle=angle[shown])
        text(rep(0.855,x),0.8-(seq(1,2*x,2))*y/2+sy/2,txt[1:x],pos=4,adj=c(0,1),cex=0.7)
     }
    
    par(mai=old.par)
}
