#############################################################################
#                 General function for plotting profiles                    #
#                                                                           #
#############################################################################

profiler<-function(x=NULL,y=NULL,method="Variable",legend=FALSE,pch=1,col="black",cex=1,xaxs="r",yaxs="i",main="",xmin=NULL,xmax=NULL){   
    on.exit(options("show.error.messages"=TRUE))
    # This is not a call from a command line, as no x axes are specified
    GUI<-is.null(y)
    
    # Select the samples to be plotted
    if(GUI){
        y1<-selectSubset(where=cbind(labels,WR),save=FALSE,GUI=TRUE)
    }
    
    if(is.null(y)){
        # Select the variables for y axis
        y<-selectColumnsLabels(message="Select variable(s) to be ploted as individual profiles",exact.only=FALSE)
    } 
    
    if(GUI){
        # Select plotting symbols and colours
        ee<-.autoassign.pch(what=t(y),edit=TRUE,legend=FALSE)
        col<-ee$col
        pch<-ee$pch  
        
        # Scaling
        cex<-as.numeric(winDialogString("Scaling factor","1"))
        if(length(cex)==0){cat("Cancelled.\n");return()}
    }else{
        y1<-rownames(WR)
        pch<-rep(pch,length(y))
        col<-rep(col,length(y))
    }
  
    #Select method if not specified in the call 
    profile.list<-c("Variable","Equidistant","From-To")
    if(!is.null(x))method<-"Variable"
    if(GUI|is.null(method)){
        which.x<-select.list(profile.list,title="What should be plotted as x axis?",preselect="Selected variable")
        if(nchar(which.x)==0){cat("Cancelled.\n");return()} 
    }else{
        which.x<-method
    }
    pick<-which(profile.list==which.x)
    if(length(pick)==0) {winDialog("ok",paste("Invalid method",which.x,"!"));options("show.error.messages"=FALSE);stop()}
    
    # Get the x data ready, if not specified in the call
    switch(pick,
        # Selected method - Variable
        {
        if(is.null(x)){
            xlab<-selectColumnLabel(where=colnames(WR),empty.ok=FALSE,silent=TRUE)
            if(is.null(xlab)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
            if(!is.na(as.numeric(xlab)))xlab<-colnames(WR)[as.numeric(xlab)]
        }else{
            xlab<-x
        }
        
        if(any(colnames(WR)==xlab)){
            B<-WR[,xlab]
        }else{
            ee<-calcCore(xlab)
            B<-ee$results
            xlab<-ee$equation
        }
        B<-B[y1]
        B<-sort(B)
        
        # Set the range to the x axis
        if(is.null(xmin)) xmin<-min(B,na.rm=TRUE)
        if(is.null(xmax)) xmax<-max(B,na.rm=TRUE)
       
        if(GUI){
                xx<-winDialogString("Enter min and max for x-axis, separated by commas",paste(xmin,xmax,sep=","))
                    if(is.null(xx)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
                ee1<-unlist(strsplit(xx,","))
                    if(length(ee1)<=1) {winDialog("ok","Invalid limits!");options("show.error.messages"=FALSE);stop()}
                xmin<-as.numeric(ee1[[1]])
                xmax<-as.numeric(ee1[[2]])
                if(xmin<0 | xmax<0 | xmin>=xmax) {winDialog("ok","Invalid limits!");options("show.error.messages"=FALSE);stop()}
        }
        x<-xlab
        ee1<- pretty(c(xmin,xmax))
        lbs<-ee1
        xlab<-annotate(xlab)
        },
        # Equidistant/unlabelled
        {
            B<-1:length(y1)
            names(B)<-y1
            ee1<-B            
            lbs<-rep("",length(B))
            xlab<-paste("n = ", max(B),sep="")
            x<-"Equidistant"
        },
        {
        # Equidistant/From-To
            # Set the range to the x axis
            if(is.null(xmin)) xmin<-0
            if(is.null(xmax)) xmax<-1
            ee1<- pretty(c(xmin,xmax))
            if(is.null(x)&GUI){
                xx<-winDialogString("Enter min and max for x-axis, separated by commas",paste(0,1,sep=","))
                    if(is.null(xx)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
                ee1<-unlist(strsplit(xx,","))
                    if(length(ee1)<=1) {winDialog("ok","Invalid limits!");options("show.error.messages"=FALSE);stop()}
                xmin<-as.numeric(ee1[[1]])
                xmax<-as.numeric(ee1[[2]])
                ee1<- pretty(c(xmin,xmax))
            }
            B<-seq(xmin,xmax,length.out=length(y1))
            names(B)<-y1
            
            lbs<-ee1
            xlab<-"distance"
            x<-"From-To"
        }
    )
    
    # Get the y data ready
    temp<-sapply(y[y!=x], function(f) {
        if(any(colnames(WR)==f)){
            i<-WR[,f]
        }else{
            ee<-calcCore(f)
            i<-ee$results
        }
    })
    A<-temp[names(B),]
    
    A[A==0]<-NA
    if(is.vector(A)){
        A<-t(as.matrix(A))
        rownames(A)<-colnames(temp)
        A<-t(A)
    }
    A<-t(A)
    
    # As well as the range to the y axis
    ymin<-min(A,na.rm=TRUE)
    ymax<-max(A,na.rm=TRUE)
    ee2<- pretty(c(ymin,ymax))
    if(is.null(y)){    
        yy<-winDialogString("Enter min and max for y-axis, separated by commas",paste(min(ee2),max(ee2),sep=","))
            if(is.null(yy)){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
        ee2<-unlist(strsplit(yy,","))
            if(length(ee2)<=1) {winDialog("ok","Invalid limits!");options("show.error.messages"=FALSE);stop()}
        ymin<-as.numeric(ee2[[1]])
        ymax<-as.numeric(ee2[[2]])
        if(ymin<0 | ymax<0 | ymin>=ymax) {winDialog("ok","Invalid limits!");options("show.error.messages"=FALSE);stop()}
        ee2<- pretty(c(ymin,ymax))
    }

    #windows(width = 8, height = 6.5, pointsize = 12,title="Profile")        
     xcex<-0.8
     ycex<-0.8
     temp<-list(
        axis1=list("axis",side=1,at=ee1,labels=lbs,cex.lab=0.8,cex.axis=xcex,las=0,hadj=NA,padj=NA,lty="solid"),
        axis2=list("axis",side=2,at=ee2,labels=ee2,cex.lab=0.8,cex.axis=xcex,las=0,hadj=NA,padj=NA,lty="solid")
     )
           
    sheet<-list(demo=list(fun="plot",call=list(xlim=range(ee1),ylim=range(ee2),xlab=xlab,ylab="",type="n",axes=FALSE,log="",xaxs=xaxs,yaxs=xaxs,bg="white",main=main),template=temp))
    ee<-lapply(1:nrow(A),function(i){ 
        eval(parse(text=(paste("sheet$demo$template$points",i,"<<-list(\"points\",x=B,y=A[",i,",],col=col[",i,"],pch=pch[",i,"],cex=cex)",sep=""))))
        tempy<-A[i,][!is.na(A[i,])]
        tempx<-B[!is.na(A[i,])]
        eval(parse(text=(paste("sheet$demo$template$lines",i,"<<-list(\"lines\",x=tempx,y=tempy,col=col[",i,"])",sep=""))))
    })
    
    ee<-lapply(1:nrow(A),function(i){   
        uf<-A[i,which(!is.na(A[i,]))]
        at<-uf[length(uf)]
        eval(parse(text=(paste("sheet$demo$template$mtext",i,"<<-list(\"mtext\",text=annotate(rownames(A)[i]),side=4,line=0,at=at,adj=0.5,padj=NA,las=0,col=col[i],cex=1)",sep=""))))
    })
   
    sheet$demo$template$box<-list("box",which="plot",col="black",lwd=1)
    sheet$demo$template$grid<-list("abline",h=ee2,col="gray",lty="dotted")  
    sheet$demo$template$GCDkit$plot.type<-"profile"

    tit<-paste("Profile (",method,") of ",paste(rownames(A),collapse=", "),sep="")
    sheet$demo$template$GCDkit$plot.name<-tit
    
    #sheet$demo$call$new<-TRUE
    
    assign("sheet",sheet,.GlobalEnv)
    pp<-figaro(demo,prefix="sheet")
    
    #x.data<<-B
    #y.data<<-A
    #results<<-t(y.data)
    assign("x.data",B,.GlobalEnv)
    assign("y.data",A,.GlobalEnv)
    assign("results",t(A),.GlobalEnv)
    
    #figRedraw()
    pp$draw(1,1,xlab=xlab,ylab="",cex=cex,main=main)
    
    # Legend
    if(legend){
        windows(width = 3.5, height = 8.5,points=10)
        plot(1,1,pch=0,axes=FALSE,col="white",xlab="",ylab="",xlim=c(-1,1),ylim=c(-1,1))
        legend(-1,1,sapply(rownames(A),annotate),bty=1,col=col,lty="solid",bg="white",pch=pch,cex=0.8)
        ee<-dev.set(dev.prev())
    }
    figaroOn()
}
