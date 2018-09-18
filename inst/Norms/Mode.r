#####################################################################
#        Calculates mode from WR analysis and composition           #
#                   of minerals by least-squares                    #
#####################################################################

# Mode: calculates modal proportions from WRComposition (rock) and mineral compositions (mins)
#=============================================================================================
Mode<-function(rock,mins,sample.id=""){
    on.exit(options("show.error.messages"=TRUE))
    ee<-lsfit(t(mins),rock,intercept=FALSE)
    X<-ee$coefficients
    cat("\n###################################\n")
    cat("#         Unconstrained LQ        #")
    cat("\n###################################\n")
    cat("Estimated mineral proportions (%):\n")
    y<-c(100*X,sum(100*X))
    names(y)[length(y)]<-"Sum"
    print(round(y,3))
        
    estimated<-WRComp(mins,X)
    cat("\nRsquared: ",round(sum(ee$residuals^2),3),"\n")

    result<-cbind(rock,t(mins),estimated,ee$residuals)
    colnames(result)[1]<-sample.id
    colnames(result)[ncol(result)-1]<-"estimated"
    colnames(result)[ncol(result)]<-"difference"
    result<-round(result,3)
    print(result)
    
  # Ditto recast to 100%  
    cat("\nMineral proportions recast to 100%:\n")
    X1<-X/sum(X)
    y<-c(100*X1,sum(100*X1))
    names(y)[length(y)]<-"Sum"
    print(round(y,3))
    estimated1<-WRComp(mins,X1)
    r<-rock-estimated1
    cat("\nRsquared: ",round(sum(r^2),3),"\n")
    result1<-cbind(rock,t(mins),estimated1,r)
    colnames(result1)[1]<-sample.id
    colnames(result1)[ncol(result1)-1]<-"estimated"
    colnames(result1)[ncol(result1)]<-"difference"
    result1<-round(result1,2)
    print(result1)
   # but return the original mineral proportions    
    X<-c(X,sum(ee$residuals^2))
    names(X)[length(X)]<-"R^2"   
    return(list(table1=result,unconstrained=round(X,3)))
}


#####################################################################
#                   Constrained least-squares                       #
#                      from Albarede 1995)                          #
#####################################################################

ModeC<-function(rock,mins,sample.id=""){
    on.exit(options("show.error.messages"=TRUE))
    A<-t(mins/100)
    y<-rock/100

    AT<-aperm(A,c(2,1))
    B<-solve(AT%*%A); #(AT*A)-1
    xa<-B%*%AT%*%y
    J<-rep(1,ncol(A))

    C<-B%*%J
    D<-J%*%B%*%J
    lambda<-(1-sum(xa))/D
    lambda<-as.vector(lambda)
    X<-as.vector(xa+lambda*C)
    names(X)<-colnames(A)

    cat("\n###################################\n")
    cat("# Constrained LQ  (Albarede 1995) #")
    cat("\n###################################\n")
    cat("\nEstimated mineral proportions (%):\n")
    y<-c(100*X,sum(100*X))
    names(y)[length(y)]<-"Sum"
    print(round(y,3))
    cat("\n")

    estimated<-WRComp(mins,as.numeric(X))
    residue<-rock-estimated
    cat("Rsquared: ",round(sum(residue^2),3),"\n")
    result<-cbind(rock,t(mins),estimated,residue)
    colnames(result)[1]<-sample.id
    colnames(result)[ncol(result)-1]<-"estimated"
    colnames(result)[ncol(result)]<-"difference"
    result<-round(result,3)
    print(result)
    X<-c(X,sum(residue^2))
    names(X)[length(X)]<-"R^2"
    return(list(table2=result,constrained=round(X,3)))
}

.select.min.file<-function(){
    on.exit(options("show.error.messages"=TRUE))
    options(show.error.messages=FALSE)
        ee<-try(ls("^mins$"))
    options(show.error.messages=TRUE)    
        if(class(ee)!="try-error"){
            ee<-winDialog("yesno", "Select mineral data file?")
        }else{
            ee<-"YES"
        }
        if(ee=="YES"){
            options(show.error.messages=FALSE)
            file<-try(choose.files(default = "", caption = "Select mineral data file",multi = FALSE,filters=matrix(ncol=2,byrow=TRUE,c("GCDkit text files (*.data)","*.data"))))

            if(class(file)=="try-error"){cat("Cancelled.\n");stop()}
            options(show.error.messages=TRUE)
            
            cat(file,"\n\n")
            mins<-read.table(file,fill=TRUE,sep="\t")
            mins<-data.matrix(mins)
            mins[is.na(mins)]<-0
            #mins<<-mins
            mins.bak<<-mins
            mins.bak<-mins
        }
            whichmins<-rownames(mins.bak)
            return(whichmins)
}
  

ModeMain<-function(WR,sample.id="",select.oxides=TRUE,select.minerals=TRUE){
# Read composition of the minerals from a datafile
    on.exit(options("show.error.messages"=TRUE))
    if(select.minerals){ 
            which.mins<-.select.min.file()
            y1<-winDialogString("Selected minerals",paste(which.mins,collapse=","))
            if(is.null(y1)){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
            if(is.na(all(match(which.mins,rownames(mins.bak))))){
                   winDialog(type="ok","Your query produced no matches")
                   options(show.error.messages=FALSE)
                   stop("",call. = FALSE)
            }
            whichmins<-unlist(strsplit(y1,","))
    }else{
            whichmins<-rownames(mins.bak)
    }

    whichelems<-colnames(mins.bak)
    if(select.oxides){
        y1<-winDialogString("Selected oxides",paste(whichelems,collapse=","))
        if(is.null(y1)){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
        whichelems<-unlist(strsplit(y1,","))
    }
    mins<-mins.bak[whichmins,whichelems]

    # Select a sample
    if(sample.id==""){
        sample.id<-selectColumnLabel(rownames(WR),message="Select sample",default="",sample.names=FALSE,silent=TRUE,empty.ok=TRUE)
        if(length(sample.id)==0){
            sample.id<-select.list(rownames(WR),multiple=FALSE)
            if(sample.id==""){cat("Cancelled.\n");options(show.error.messages=FALSE);stop()}
        }
    }
    if(is.numeric(sample.id))sample.id<-rownames(WR)[sample.id]
    cat("\n")
    
    if(!is.na(as.numeric(sample.id))){
        sample.id<-as.numeric(sample.id)
            if(sample.id<1|sample.id>nrow(WR)){
                winDialog(type="ok","Sample not found")
                options(show.error.messages=FALSE)
            stop("",call. = FALSE)
            }
        sample.id<-rownames(WR)[sample.id]
    }else{
        if(all(rownames(WR)!=sample.id)){
            winDialog(type="ok","Sample not found")
            options(show.error.messages=FALSE)
            stop("",call. = FALSE)
        }
    }

    rock<-WR[sample.id,whichelems]
    rock[is.na(rock)]<-0
    results<-c(Mode(rock,mins,sample.id=sample.id),ModeC(rock,mins,sample.id=sample.id))
    return(results)
}

# Mode for all samples
ModeAll<-function(WR){
    mins<-.select.min.file()
    which<-selectSamples()
    what<-WR[which,]
    ee<-matrix(nrow=nrow(what),ncol=nrow(mins.bak)+1)
    rownames(ee)<-rownames(what)
    colnames(ee)<-c(rownames(mins.bak),"R^2")
    for(i in rownames(what)){
        ee[i,]<-ModeMain(WR,i,select.oxides=FALSE,select.minerals=FALSE)[[4]]
     }
    results<-(cbind(100*ee[,1:(ncol(ee)-1)],ee[,ncol(ee)]))
    colnames(results)[ncol(results)]<-"R^2"
    return(results)
}
