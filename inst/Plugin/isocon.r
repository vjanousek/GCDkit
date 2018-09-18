# For selecting end members
.select.rocks<-function(xaxis="SiO2",yaxis="MgO"){
    on.exit(options("show.error.messages"=TRUE))
    windows(width = 7.5, height = 7.5, pointsize = 10)
    plot(WR[,xaxis],WR[,yaxis],xlab=annotate(xaxis),ylab=yaxis,bg=1,pch=labels$Symbol,col=labels$Colour)

    x1<-identify(WR[,xaxis],WR[,yaxis],labels=rownames(WR),n=1,pos=3,offset=0.5,col="red")
    points(WR[x1$ind,xaxis],WR[x1$ind,yaxis],pch=1,col="red",cex=2.5)
    x1<-rownames(WR)[x1$ind]
    
    x2<-identify(WR[,xaxis],WR[,yaxis],labels=rownames(WR),n=1,pos=3,offset=0.5,col="red")
    points(WR[x2$ind,xaxis],WR[x2$ind,yaxis],pch=1,col="red",cex=2.5)
    x2<-rownames(WR)[x2$ind]
    
    x<-c(x1,x2)
    if(x1==x2){winDialog(type="ok","Both end compositions have to be different!");.select.rocks()}
    y<-winDialogString("Original, Altered",paste(x,collapse=","))
    if(length(y)==0){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
    x<-unlist(strsplit(y,","))

    dev.off(dev.cur())
    return(WR[x,])
}

# Get ready names of atoms from the oxide names
.atoms.from.formula<-function(oxides,valency=FALSE){
    z<-gsub("[0-9]","",oxides)                        # Remove numbers
    z<-sapply((strsplit(z,"O")),paste,collapse="")    # Remove oxygen's "O" and anything beyond
    if(valency){
        v<-.valency(oxides)
        v[is.na(v)]<-""
        z<-paste(z,v,sep="")
    }
    z[z=="Fet"]<-"Fe"
    return(z)
}

# Recast oxide wt % to atomic %
.oxide2atom<-function(where=WR,formula){
    ee<-strsplit(paste(formula," ",sep=""),"O")
    n<-as.numeric(gsub("[a-zA-Z]","",ee[[1]]))
    n[is.na(n)]<-1
    if(length(n)==1)n[2]<-0
    atom<-gsub("[0-9 ]","",ee[[1]][1])
    f<-n[1]*mw[atom]/(n[1]*mw[atom]+n[2]*mw["O"])
    z<-matrix(where[,formula]*f,ncol=1)
    rownames(z)<-rownames(where)
    colnames(z)<-atom
    return(z)
}

isocon<-function(x=NULL,whichelems=NULL, immobile=NULL, atomic=FALSE, plot=TRUE){ 
  on.exit(options("show.error.messages"=TRUE))
  if(is.null(x)){  
    # debugging data by Grant
    #x<-c(46.45,1.29,14.30,11.05,0.17,5.28,12.14,2.93,0.49,3.00,3.29,42,327,313,67,77,100,170,29,80,
    # 45.62,1.30,14.74,8.20,0.15,3.89,8.29,2.09,3.12,2.18,10.96,39,305,282,42,75,72,214,17,140)
    #if(is.vector(x)) {x<-matrix(x,ncol=2); x<-t(x); WR<-x}
    
    x<-.select.rocks()
    
  }
    samples<-rownames(x)
    rownames(x)<-c("Original","Altered")
    # debugging data by Grant
    #whichelems<-"SiO2,TiO2,Al2O3,Fe2O3,MnO,MgO,CaO,Na2O,K2O,H2O,CO2,Sc,V,Cr,Ni,Cu,Zn,Sr,Y,Ba"
    #colnames(x)<-unlist(strsplit(whichelems,","))
    if(is.null(whichelems)){
        if(plot){
            whichelems<-selectColumnsLabels(where=colnames(x),message="Select elements/oxides for plotting",default=paste(colnames(WR),collapse=","))
            x<-x[,whichelems]
        }     
    }else{
            whichelems<-unlist(strsplit(whichelems,","))
            x<-x[,whichelems]
    }
 
    
    # Recast to atoms if desired
    if(atomic==TRUE){
        rockdata<-t(sapply(colnames(x),function(i){z<-.oxide2atom(x,i)}))
        colnames(rockdata)<-c("Original","Altered")
        rownames(rockdata)<-.atoms.from.formula(whichelems)
        x<-rockdata
    # or leave as oxides
    }else{  
        x<-t(x)
        rockdata<-x
    }
    
    # Print slopes to individual data points
    cat("Slopes to individual data points:\n")
    slopes<-rockdata[,2]/rockdata[,1]
    print(round(slopes,3))
    cat("\n")
    
    cat("Sorted slopes to data points:\n")
    y<-rev(sort(slopes))
    print(round(y,3))
    cat("\n")
    
    # Plot slopes
    if(plot){
        windows(width = 10, height = 5, pointsize = 10)
        par(mfrow=c(1,2))
        par(pty="s")
        par(oma=c(1,1,1,1))
        par(mar=c(5,4,1,1))
                        
        plot(y,axes=FALSE,log="y",xlab="",ylab="Slopes to data points",type="h",lwd=7,col=c("darkblue","darkred"),ylim=c(min(y,na.rm=TRUE),1.2*max(y,na.rm=TRUE)))
        abline(h=1)
        at<-c(seq(0.1,0.9,0.1),1:10)
        axis(2,at=at)
        abline(h=at,lty="dashed")
    
        labs<-sapply(names(y),annotate)
        where1<-seq(1,length(y),2)
        where2<-seq(2,length(y),2)
    
        axis(1,labels=FALSE,at=1:length(y),ticks=TRUE)
    
        mtext(labs[where1],1,at=where1,line=0,cex=0.7,col="darkblue",padj=1)
        text(where2,y[where2]*1.15,labs[where2],cex=0.9,col="white",adj=0.5,font=2)
        text(where2,y[where2]*1.15,labs[where2],cex=0.7,col="darkred",adj=0.5)
        box()
    }
    
        # Scaling
        ii<-apply(rockdata,1,function(i)!any(is.na(i)))
        sc<-rev(seq(2,2*length(ii[ii]),2))
        i<-rownames(rockdata)%in%c(major,"CO2","H2O","H2O.PLUS","FeOt","Fe2O3t","Li2O","F","S")
        maxima<-rev(sort(c(rockdata[i,1]*1e4,rockdata[!i,1])))
        ee<-(names(maxima)%in%names(slopes[!is.na(slopes)]))
        
        names(sc)<-names(maxima)[ee]
  
        if(plot){
            sc<-edit(as.data.frame(sc))
            sc<-unlist(sc)
            sc<-as.numeric(sc)
        }
        names(sc)<-names(maxima)[ee]
        scaling<-sc/rockdata[names(sc),1]
        
        rockdata[names(sc),1]<-rockdata[names(sc),1]*scaling
        rockdata[names(sc),2]<-rockdata[names(sc),2]*scaling
    
    # Isocon plot
    if(plot){
        max<-max(rockdata,na.rm=TRUE)
        plot(rockdata[,1],rockdata[,2],xlim=c(0,max+max/15),ylim=c(0,max+max/15),pch=15,xlab=paste(samples[1],": [scaled] original rock (wt. % or ppm)"),ylab=paste(samples[2],": [scaled] altered rock (wt. % or ppm)"))
        #if(log) plot(rockdata[,1],rockdata[,2],xlim=c(0.01,max+max/15),ylim=c(0.01,max+max/15),log="xy",pch=15,xlab=paste(samples[1],": [unscaled] original rock (wt. % or ppm)"),ylab=paste(samples[2],": [unscaled] altered rock (wt. % or ppm)"))
        labs2<-sapply(rownames(rockdata),annotate)
        text(rockdata[,1],rockdata[,2]+max/25,labs2,cex=0.8)
        #if(log)text(rockdata[,1],rockdata[,2]*1.5,labs2,cex=0.8)
        abline(0,1,col="darkblue")
        text(max,max+5,"1:1",srt=45,col="darkblue")
    }
    
    # Select immobile elements
    if(is.null(immobile)){
        if(atomic){
            #immobile<-selectColumnsLabels(rownames(rockdata),message="Which elements are considered immobile",default="Al,Si",exact.only=TRUE)
            immobile<-winDialogString("Which elements are considered immobile","Al,Si")
        }else{
            #immobile<-selectColumnsLabels(rownames(rockdata),message="Which elements are considered immobile",default="Al2O3,SiO2",exact.only=TRUE)
            immobile<-winDialogString("Which elements are considered immobile","Al2O3,SiO2")
        }
        if(length(immobile)==0){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
        if(!is.null(immobile)&immobile!=""){
            immobile<-unlist(strsplit(immobile,","))
        }else{
            which<-NA
            immobile<-NULL
            while(length(which)>0){
                which<-identify(rockdata,labels=rep("",nrow(rockdata)),n=1)
                which<-rownames(rockdata)[which]
                if(all(immobile!=which)){
                    immobile<-c(immobile, which)
                    points(rockdata[which,1],rockdata[which,2],pch=1,col="red",cex=2)
                }else{
                    points(rockdata[which,1],rockdata[which,2],pch=1,col="white",cex=2)
                    immobile<-immobile[immobile!=which]
                }
            }
        }
        cat("\n")
        if(length(immobile)==0){winDialog(type="ok","No immobile elements were selected!");return()}
        cat("Selected immobile elements:",paste(immobile,collapse=","),"\n")
    }else{
        cat("Selected immobile elements:",immobile,"\n")
        immobile<-unlist(strsplit(immobile,","))    
    }
        
    
    b2<-mean(rockdata[immobile,2]/rockdata[immobile,1],na.rm=TRUE)    
    if(plot){
        lq<-lsfit(rockdata[immobile,1],rockdata[immobile,2],intercept=FALSE)
        b1<-lq$coeff
        cat("Slope obtained by linear regression [darkgreen]:     ",b1,"\n")
        abline(0,b1,lty="dotted",col="darkgreen")
        abline(0,b2,lty="dashed",col="darkred")
    }
    
    
    cat("and as an average from individual slopes [darkred]:  ",b2,"\n\n")
    gains1<-x[,2]/b1-x[,1]
    gains2<-x[,2]/b2-x[,1]
    
    out<-list()
    out$slope.avg<-b1
    names(out$slope.avg)<-NULL
    out$slope.regression<-b2
    
    out$balance<-cbind(round(x,2),round(slopes,3),round(gains1/x[,1],2),round(gains2/x[,1],2),round(gains1,2),round(gains2,2))
    colnames(out$balance)[1]<-paste(samples[1],"=orig.",sep="")
    colnames(out$balance)[2]<-paste(samples[2],"=alt.",sep="")
    colnames(out$balance)[3]<-"Slope data point"
    colnames(out$balance)[4]<-"G/L rel.(LQ)" 
    colnames(out$balance)[5]<-"G/L rel.(avg)"
    colnames(out$balance)[6]<-"G/L wt%/ppm(LQ)"
    colnames(out$balance)[7]<-"G/L wt%/ppm(avg)"
    return(out)
}

isoconAtoms<-function(){
    results<-isocon(atomic=TRUE, plot=TRUE)
    results<<-results
    print(results$balance)
}

isoconOxides<-function(){
    results<-isocon(atomic=FALSE, plot=TRUE)
    results<<-results
    print(results$balance)
}

# Concentration ratio plot(Ague 1994]
Ague<-function(x=NULL,whichelems="SiO2,TiO2,Al2O3,FeOt,MnO,MgO,CaO,Na2O,K2O,P2O5",immobile=NULL,bars=NULL,plot=TRUE){ 
    on.exit(options("show.error.messages"=TRUE))
    
    # Immobile elements have to be among those plotted
    if(!is.null(immobile)) whichelems<-paste(whichelems,immobile,sep=",")
    
    # Get ready the analyses of the parental and altered granites
    if(is.null(x)){   
        x<-.select.rocks()  
        samples<-rownames(x)
        GUI<-TRUE
    }else{
        samples<-x
        whichelems<-unique(unlist(strsplit(whichelems,",")))
        x<-WR[samples,whichelems]
        GUI<-FALSE
    }
    rownames(x)<-c("Original","Altered")
  
    # Prepare the names of individual geochemical species (oxides or atoms)
    if(GUI){
            whichelems<-selectColumnsLabels(where=colnames(x),message="Select elements/oxides for plotting",default=whichelems)
            x<-x[,whichelems]
    }     

    #Bars
    if(!is.null(bars)){
        if(length(bars)==1){
           stdev<-WR[bars,whichelems]/x[1,] # only errors for the altered rock are given
        }else{          
           stdev<-sqrt(WR[bars[1],whichelems]^2+WR[bars[2],whichelems]^2)/x[1,] # also errors for the protolith are available
        }
    }

    rockdata<-t(x)
    #rownames(rockdata)<-.atoms.from.formula(whichelems)
        
    # Print slopes to individual data points
    cat("Slopes to individual data points:\n")
    slopes<-rockdata[,2]/rockdata[,1]
    print(round(slopes,3))
    cat("\n")
    
    cat("Sorted slopes to data points:\n")
    y<-rev(sort(slopes))
    print(round(y,3))
    cat("\n")

    # Plot slopes
    if(plot){
        windows(title="Enrichment factors")                       
        plot(y,axes=FALSE,log="y",xlab="",ylab="Slopes to data points",type="h",lwd=7,col=c("darkblue","darkred"),ylim=c(min(y,na.rm=TRUE),1.2*max(y,na.rm=TRUE)))
        abline(h=1)
        at<-c(seq(0.1,0.9,0.1),1:10)
        axis(2,at=at)
        abline(h=at,lty="dashed")
    
        labs<-sapply(names(y),annotate)
        where1<-seq(1,length(y),2)
        where2<-seq(2,length(y),2)
    
        axis(1,labels=FALSE,at=1:length(y),ticks=TRUE)
    
        mtext(labs[where1],1,at=where1,line=0,cex=0.7,col="darkblue",padj=1)
        text(where2,y[where2]*1.15,labs[where2],cex=0.9,col="white",adj=0.5,font=2)
        text(where2,y[where2]*1.15,labs[where2],cex=0.7,col="darkred",adj=0.5)
        box()
    }
   
    # Select immobile elements
    if(is.null(immobile)){
        immobile<-winDialogString("Which elements are considered immobile","Al2O3,SiO2")
        if(length(immobile)==0){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
         
        if(!is.null(immobile)&immobile!=""){
            immobile<-unlist(strsplit(immobile,","))
        }else{
            which<-NA
            immobile<-NULL
            while(length(which)>0){
                which<-identify(rockdata,labels=rep("",nrow(rockdata)),n=1)
                which<-rownames(rockdata)[which]
                if(all(immobile!=which)){
                    immobile<-c(immobile, which)
                    points(rockdata[which,1],rockdata[which,2],pch=1,col="red",cex=2)
                }else{
                    points(rockdata[which,1],rockdata[which,2],pch=1,col="white",cex=2)
                    immobile<-immobile[immobile!=which]
                }
            }
        }
        cat("\n")
        
        if(length(immobile)==0){winDialog(type="ok","No immobile elements were selected!");return()}
        cat("Selected immobile elements:",paste(immobile,collapse=","),"\n")
    }else{
        cat("Selected immobile elements:",immobile,"\n")
        immobile<-unlist(strsplit(immobile,","))    
    }
    
    # Concentration ratio plot
    if(plot){
        rockdata<-t(rockdata)
        precursor<-subset(rockdata,rownames(rockdata)=="Original",drop=FALSE)
        rownames(precursor)<-samples[1]
        
        rough<-subset(rockdata,rownames(rockdata)=="Altered",drop=FALSE)
        rownames(rough)<-samples[2]
        
        rockdata<-t(rockdata)

        Y<-.normalization(rough,precursor) #calculate for correct scaling

        if(is.null(bars)){
            ymax<-ceiling(max(Y,na.rm=TRUE))
            #ymax<-ceiling(10*max(Y,na.rm=TRUE))/10
        }else{
            ymax<-ceiling(max(Y+stdev,na.rm=TRUE))
            #ymax<-ceiling(10*max(Y+stdev,na.rm=TRUE))/10
        }
        rownames(precursor)<-paste("normalized by sample",rownames(precursor))
        spider(rough,ymin=0,ymax=ymax,chondrit=precursor,pch=19,cex=1.5,type="h",log="",,xrotate=TRUE,offset=FALSE,new=TRUE,yaxs="i")
        sheet$demo$template$grid0<<-list("points",x.data,y.data,type="h",lty="dashed",col="gray")
        sheet$demo$call$ylab<<-paste(samples[2],"/",samples[1])
        
        # Get the mean slope of immobile components
        b2<-mean(rockdata[immobile,2]/rockdata[immobile,1],na.rm=TRUE) 
        
        # Gridlines for concentration ratios
        i<-1:ymax
        i<-i[i!=1]
        sheet$demo$template$grid<-list("abline",h=i,col="black",lty="dotted")
        sheet$demo$template$grid1<-list("abline",h=1,col="black")                  
        
        # Overplot grid
        rinv<-mean(rockdata[immobile,2]/rockdata[immobile,1],na.rm=TRUE)
        cat("rinv:",rinv,"\n")
        cat("Overall change in the rock mass (wt. %):",(1/rinv-1)*100,"\n")
        
        # Gridlines for concentration ratios
        ee<-c(seq((par("usr")[3]/rinv-1),0,by=0.5),seq(0,(par("usr")[4]/rinv-1),by=0.5))
        i<- rinv*(1+ee)
        sheet$demo$template$grid2<-list("abline",h=i,col="darkred",lty="longdash",lwd=1.5)
        sheet$demo$template$grid3<-list("abline",h=rinv*(1+c(0,ee)+0.25),col="darkred",lty="dotted",lwd=1)
        sheet$demo$template$grid4<-list("abline",h=rinv,col="darkred",lty="solid",lwd=1.5)
        ee<-c(-0.5,0,0.5,1:(par("usr")[4]/rinv-1))
        i<- rinv*(1+ee)
        sheet$demo$template$mtext99<-list("mtext",side=4,at=i,text=paste(ee*100,"%",sep=""),cex=0.8)
       
        # Adjust scaling of the axis labels
        sheet$demo$template$mtext1$cex<-0.75
        sheet$demo$template $axis2$cex.axis<-1
        assign("sheet",sheet,.GlobalEnv)
        figRedraw()

        if(!is.null(bars)){           
            ee<-sapply(1:length(whichelems),function(i){
               lines(c(i-0.5,i+0.5),c(Y[,i]+stdev[i],Y[,i]+stdev[i]),lwd=2,col="red")
               lines(c(i-0.5,i+0.5),c(Y[,i]-stdev[i],Y[,i]-stdev[i]),lwd=2,col="red")
               lines(c(i,i),c(Y[,i]+stdev[i],Y[,i]-stdev[i]),lwd=2,col="red")
            })
        }
           
    }
    # Gain/losses in %
    out<-round(cbind(slopes,(1/rinv*(rockdata[,2]/rockdata[,1])-1)*100),2)
    colnames(out)<-c("Altered/Protolith","Gain/loss in %")
    rownames(out)<-whichelems
    results<<-out
    return(out)
}

# Wedge diagrams (Ague 1994, Bucholz and Ague 2010)
Wedge<-function(x="Ti",y=NULL,protolith=NULL,outline="chull",precision=10,plotAltered=TRUE,xmin=0,ymin=0,xmax=NULL,ymax=NULL,fun=NULL){
    on.exit(options("show.error.messages"=TRUE))
    GUI<-FALSE
    if(is.null(y)){
        GUI<-TRUE
        y<-"SiO2,Al2O3"
    }
    if(GUI){
        xlab<-selectColumnLabel(colnames(WR),message="x-axis",default=x)
        xlab<-colnames(WR)[xlab]
    }else{
        xlab<-x
    }
    
    if(GUI){
        ylab<-selectColumnsLabels(colnames(WR),message="y-axes",default=y,exact.only=TRUE)
    }else{
        ylab<-unlist(strsplit(y,","))
    }
    
    ylab<-ylab[ylab!=xlab]
    ii<-ylab%in%colnames(WR)
    if(length(ii)>sum(ii)){
        winDialog(type="ok",paste("No data for",paste(ylab[!ii],collapse=", "),"available!"))
    }
    
    ylab<-ylab[ii]
    if(!is.null(ymax)){
        if(!is.numeric(ymax)|length(ymax)!=length(ylab)){
            winDialog(type="ok","Invalid y axes limits! None are set...")
            ylim<-NULL
        }
    }
   
    # Define the protolith samples
    protolith<-selectSubset(protolith,save=FALSE,text="PROTOLITH samples - specify search pattern \n by sample name, range or Boolean condition",GUI=GUI,all.nomatch=FALSE)
    
    if(protolith==""){winDialog(type="ok","No protolith samples selected!");return()}
    if(length(protolith)==nrow(WR)){winDialog(type="ok","No altered samples remain!");return()}
    
    clusters<-rep("Altered",times=nrow(WR))
    names(clusters)<-rownames(WR)
    clusters[protolith]<-"Protolith"
    
    # Set up the plate
    delka<-length(ylab)
    #if(!GUI){
        ncol<-n2mfrow(delka)[1]
        nrow<-n2mfrow(delka)[2]
    #}else{
    #    nrow=NULL
    #    ncol=NULL
    #}
    
    title<-paste("Wedge plots of ",xlab," vs. ","list(",paste(ylab,collapse=","),")",sep="")
    multiplePerPage(delka,nrow=nrow,ncol=ncol,title=title,dummy=FALSE)
    #Go!
    out<-sapply(1:delka,function(j){  
        screen(j)
        if(is.null(xmax)){
            digits.x<--as.integer(log10(max(WR[,xlab],na.rm=TRUE))-1)
            xmax=.round.max.up(WR[,xlab],dec.places=digits.x,expand=TRUE)
        }
        if(is.null(ymax)){
            digits.y<--as.integer(log10(max(WR[,ylab[j]],na.rm=TRUE))-1)
            ymax1<-.round.max.up(WR[,ylab[j]],dec.places=digits.y,expand=TRUE)
        }else{
            ymax1<-ymax[j]
        }
    # Plotting window title
        par(mar=c(4.5,4.5,1,1))
        par(pty="s")
        if(plotAltered){
            plotWithLimits(WR[,xlab],WR[,ylab[j]],xlab=annotate(xlab),ylab=annotate(ylab[j]),xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax1,new=FALSE) #Plot all data
        }else{
            plotWithLimits(WR[protolith,xlab],WR[protolith,ylab[j]],xlab=annotate(xlab),ylab=annotate(ylab[j]),xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax1,new=FALSE) #Plot all data
        }
        
        slopes<-y.data[protolith]/x.data[protolith] # Get the protolith slopes to draw the wedge
        bmin<-min(slopes,na.rm=TRUE)
        bmax<-max(slopes,na.rm=TRUE)
        sheet$demo$template$polygon<<-list("polygon",x=c(xmin,xmax,xmax,xmin,xmin),y=c(bmin*xmin,bmin*xmax,bmax*xmax,bmax*xmin,bmin*xmin),col="gray70",border=FALSE)
        sheet$demo$template$abline1<<-list("abline",a=0,b=bmin,h=NULL,v=NULL,col="black",lty="dashed",lwd=1.5)
        sheet$demo$template$abline2<<-list("abline",a=0,b=bmax,h=NULL,v=NULL,col="black",lty="dashed",lwd=1.5)
        pp<<-figaro(demo,prefix="sheet")      
        plate[[j]]<<-sheet
        figRedraw()
        
        figCex(1.8)
        figCexLab(1.5)

         if(!plotAltered){
            ee<-!rownames(WR)%in%protolith
            x<-WR[ee,xlab]
            y<-WR[ee,ylab[j]]
            i<-!is.na(x)&!is.na(y)
            contour(kde2d(x[i],y[i],n = 100,lims=c(sheet$d$c$xlim,sheet$d$c$ylim)),col="darkred",add=TRUE)
        }
        
        if(outline=="chull") chullGroups(clusters,border=c("khaki","white"),lwd=1.2)
        if(outline=="contour") contourGroups(clusters,precision=precision)
        
        # Extra function     
        if(!is.null(fun)) do.call(fun,list(xlab=xlab,ylab=ylab[j]))
        z<-slopes
        z[is.na(z)]<-0
        #z<-rev(sort(round(z,5)))      
        return(z)  
    },simplify=TRUE)

    if(is.vector(out)){
        out<-matrix(out,nrow=1,dimnames=list(protolith,ylab)) 
    }else{
        colnames(out)<-ylab 
    }
    out[out==0]<-NA
    results<<-out
    return(out)
}

.isoconMenu<-function(){
    on.exit(options("show.error.messages"=TRUE))
    where<-c("Isocon - atomic","Isocon - oxides","Concentration ratio diagram (Ague 1994)","Wedge plot (Ague 1994)")
    selected<-select.list(where, preselect = NULL, multiple = FALSE, title = "Open-system mass balance calculations")
    if(selected==""){cat("Cancelled.\n");options("show.error.messages"=FALSE);stop()}
    ee<-switch(which(where==selected),
        "isoconAtoms()",
        "isoconOxides()",
        "Ague()",
        "Wedge()"
    )
    cat("GCDkit->",ee,"\n")
    .save2hist(ee)
    eval(parse(text=ee))    
}    

if(getOption("gcd.menus")!=""){winMenuAddItem("Plugins","Isocon",".isoconMenu()")}
