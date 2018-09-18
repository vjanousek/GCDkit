# figaro 103 May 11
# figures as R objects
# - figure creation and editing
# - style sheets
# - graphical objects

# part 1 - figure creation and editing
figaro <- function(name, prefix="fss") {
  on.exit(options("show.error.messages"=TRUE))
  my <- list(fun=NULL,call=NULL,template=NULL,cntpoints=0,cntlines=0)
  if(!is.character(name))
    name<-deparse(substitute(name))
  my$fun <- name
  template <- get(prefix)[my$fun]
  if (!is.null(unlist(template))){
    my<-template[[1]]
   }
  if(is.null(unlist(my$call$bg))) {
      my$call$bg<-par('bg')
    }
  if(is.null(unlist(my$call$fg))) {
      my$call$fg<-par('fg')
    }
   
  if(is.null(unlist(my$call$new))) {
      my$call$new<-TRUE
  }
  my.draw <- function() { 
  if(is.null(sheet$demo$call$new)) sheet$demo$call$new<<-TRUE
  if(sheet$demo$call$new){   
    #Vojta
         i<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
         if(i!="spider "&i!="GIS "&i!="PTmap "){
            windows(width = 6.5, height = 6.5,title=sheet$demo$template$GCDkit$plot.name)
            if(i=="binary "){ # Allow more space for epsilon symbol
                if(substr(sheet$demo$call$ylab,1,7)=="epsilon") par("mar"=c(5.1,5,4.1,2.1))
            }
         }
         
         #/Vojta
    }
    
    par(bg=my$call$bg)
    par(fg=my$call$fg)
    
    if (is.null(my$template)){
      do.call(my$fun, my$call)
    } else {
      #browser() # original problem with panel.first is back
      #do.call(my$fun, c( my$call, list(panel.first=quote(my.panel()))))
      do.call(my$fun, c(my$call, list(panel.first=my.panel)))
    }
    if(!is.null(unlist(my$addpoints))) {
      for ( i in 1:my$cntpoints) {
        do.call('points',my$addpoints[[i]])
      }
    }
    if(!is.null(unlist(my$addlines))) {
      for ( i in 1:my$cntlines) {
        do.call('lines',my$addlines[[i]])
      }
    }

    if (!is.null(unlist(my$legend))) {
      xy <-legend.go(my$call[[1]],my$legend)
    }
    sheet$demo$call$new<<-FALSE
  }
  
  my.template <- function(cmdlist) {
    for (i in 1:length(cmdlist)) {
      args <- cmdlist[[i]]
      
      switch(args[[1]],
             lines=lines(x=args$x,y=args$y,col=args$col, lty=args$lty, # modified by VJ
               lwd=args$lwd,pch=args$pch),
             polygon=polygon(args$x, args$y, col=args$col, border=FALSE),
             arrows=arrows(x0=args$x0,y0=args$y0,x1=args$x1,y1=args$y1,code=args$code,col=args$col,length=args$length,lty=args$lty,lwd=args$lwd),# added by VJ
             text=text(args$x, args$y, args$text, col=args$col,
               cex=args$cex, font=args$font,adj=args$adj,srt=args$srt),# modified by VJ
             etext=text(args$x, args$y, parse(text=args$text),
               col=args$col, cex=args$size, font=args$font),
             abline=abline(a=args$a,b=args$b,h=args$h,v=args$v,col=args$col,lty=args$lty,lwd=args$lwd), # modified by VJ
             points=points(x=args$x,y=args$y,col=args$col,pch=args$pch,cex=args$cex), # added by VJ
             box=box(which=args$which,col=args$col,lwd=args$lwd), # added by VJ
             mtext=mtext(text=args$text,side=args$side,line = args$line, at=args$at,adj=args$adj,padj=args$padj,las = args$las,col=args$col,cex=args$cex), # added by VJ
             axis=axis(side=args$side,at=args$at,labels=args$labels,cex.axis=args$cex.axis,cex.lab=args$cex.lab,las=args$las,hadj=args$hadj,padj=args$padj,lty=args$lty), # added by VJ
             rug=rug(args$x,ticksize=args$ticksize,side=args$side,lwd=args$lwd,col=args$col), # added by VJ
             reservoirs=figAddReservoirs(autoscale=FALSE,var.name=args$var.name,sample.names=args$sample.names,reserv.condition=args$reserv.condition,labs = args$labs,col=args$col,pch=args$pch,cex=args$cex,type=args$type,just.draw=TRUE),# added by VJ
             ArcGIS=.figArcMapDraw(object=args$object,map.col=args$map.col,map.palette=args$map.palette,labels.txt=args$labels.txt,col.txt=args$col.txt,cex.txt=args$cex.txt,xlim=args$xlim,ylim=args$ylim),# added by VJ
             PTmap=.perplexFilledContourRedraw(main=args$main,xlim=args$xlim,ylim=args$ylim,xlab=args$xlab,ylab=args$ylab,add=args$add,leg.title=args$leg.title,legend=args$legend),# added by VJ
             legend=legend(x=args$x,y=args$y,legend=args$legend,col=args$col,pch=args$pch,cex=args$cex,pt.cex=args$pt.cex,lty=args$lty,bg=args$bg,ncol=args$ncol,bty=args$bty,xpd=args$xpd,yjust=args$yjust,title=args$title)# added by VJ
      )
    }   
  }
  my.panel <- function() {
    my.template(my$template)
  }
  my.showlist <- function(lst,pfx=0) {
    end <- length(lst)
    if (end < 1) return(invisible(NULL))
    for (i in 1:end){
      if (is.list(lst[[i]])) {
        cat(rep(" ",pfx),names(lst)[i],"\n")
        pfx<- pfx+1
        my.showlist(lst[[i]],pfx)
        pfx <- pfx-1
      } else {
        cat(rep(" ",pfx),names(lst)[i],"\n")
        print(lst[[i]])
      }
    }
  }
  
  my.return.list <- function(lst,pfx=0) {
    end <- length(lst)
    if (end < 1) return(invisible(NULL))
    return(lst)
  }
  
  my.change <- function (pfx="my$", ...) {
    what <- list(...)
    if(names(what)[1] != ""){
      ctyp <- "fun" # set(name=value), apply change to plot function
      otyp <- ""
      start <- 1
    } else {
      ctyp <- "obj" # set("obj", name=value), apply change to object
      start <- 2
      if (!is.null(class(my$call$x)) && class(my$call$x)=="go"){
        otyp <- "go"
        if (what[[1]] == "points" || what[1]=="P"){
          pfx <- "my$call$x$pset"
        }
        if (what[[1]] == "lines" || what[1]=="L"){
          pfx <- "my$call$x$lset"
        }
      } else {
        otyp <- "ss"
        pfx<-paste("my$template$", what[[1]],"$",sep="")
      }
    }
    for ( i in start:length(what)) {
      if(is.character(what[[i]])) {
        val <- paste("\"",what[[i]],"\"", sep="")
      } else {
        val <- what[[i]]
      }
      if(ctyp == "fun" || otyp == "ss") {
        cmd <- paste(pfx,names(what)[i],"<<-c(",val,")",sep="")
      }
      if (otyp == "go"){
        val <- paste(val, collapse=",")
        cmd <- paste(pfx,"(",names(what)[i],"=c(",val,"))",sep="")
      }
    }
    eval(parse(text=cmd))
  }
  list(draw=function(...){
        dotdot<-list(...) #prints extra empty row, I cannot get rid of it
         if (length(dotdot) > 0){
           dotnames<- names(dotdot)
           if (is.null(dotnames))
             ncall<- length(dotdot)
           else
             ncall <- length(seq(dotdot)[names(dotdot) ==""])
           # default title and labels
           my$call$main<<-paste(my$fun)
           my$call$xlab<<-''
           my$call$ylab<<-''
           # ncall <- .ncall[my$fun]
           if (!is.null(unlist(ncall))){             
             if(ncall==1) {
               my$call$ylab<<-paste(sys.call()[2])
             }
             if (ncall==2) {
               my$call$xlab<<-paste(sys.call()[2])
               my$call$ylab<<-paste(sys.call()[3])
             }
             if(is.list(dotdot[[1]])) {
               navn <- names(dotdot[[1]])
               my$call$xlab<<-paste(sys.call()[2],"$",navn[1],sep="")
               my$call$ylab<<-paste(sys.call()[2],"$",navn[2], sep="")
             }
           }
           # labels from graphic object
           g <- dotdot[[1]]
           if (is.list(g) && !is.null(class(g)) && class(g) == "go") {
             if (!is.null(unlist(g$xlab())))
               my$call$xlab <<- g$xlab()
             if (!is.null(unlist(g$ylab())))
               my$call$ylab <<- g$ylab()
             # set name for g obj for use in my.change()
             
           }
           # save call settings           
           my$call<<-.mergelist(dotdot,my$call)
           if (!is.null(class(g)) && class(g) == "go")
               names(my$call)[1] <<- "x"         
         }
         
         #figRedraw()
         my.draw()
         invisible()
       },
       set=function(...){
         my.change("my$call$",...)
         my.draw()
       },
       show=function(x){
         if(missing(x)){
           what <- 'my'
         } else {
           what <- paste('my',x,sep='$')
         }
         what <- paste('my.showlist(',what,')',sep='')
         eval(parse(text=what))
       },
       zoom=function(){
         z <- locator(2,type="n")
         if (is.null(unlist(my$save$xlim))) {
            my$save$xlim<<-my$call$xlim
            my$save$ylim<<-my$call$ylim
        }else{
            my$prev$xlim<<-my$call$xlim
            my$prev$ylim<<-my$call$ylim
       }
         #Vojta
            ee<-z
            z$x<-sort(ee$x)
            z$y<-sort(ee$y)
            my$call$xlim<<-z$x
            my$call$ylim<<-z$y
            new<-par("usr")
            rect(z$x[1],z$y[1],z$x[2],z$y[2],density=5,angle=45,col="darkkhaki")
         
         #Vojta
         i<-paste(sheet$demo$template$GCDkit$plot.type," ",sep="")
         if(i=="spider "){
            windows(width = 8, height = 6.5, pointsize = 12,title=sheet$demo$template$GCDkit$plot.name)
            if(!is.null(sheet$demo$legend)){                       
                par(omd=c(0,1,0,1))
                par(mar=c(6,3,6,10))
            }
        }else{
            if(i=="GIS "){
                figRedraw(zoom=z)
                return(z)
            }else{
                windows(width = 6.5, height = 6.5,title=sheet$demo$template$GCDkit$plot.name)
                if(i=="binary "){ # Allow more space for epsilon symbol
                    if(substr(sheet$demo$call$ylab,1,7)=="epsilon") par("mar"=c(5.1,5,4.1,2.1))
                }
            }
        }

         #figRedraw(zoom=z)
         pp$draw()
         #/Vojta
         return(z)
       },
       unzoom=function() {
         my$call$xlim<<-my$save$xlim
         my$call$ylim<<-my$save$ylim
         figRedraw()
         #my.draw()
       },
       legend=function() {
         my$legend <<-legend.go(my$call[[1]])
         my.draw()
       },
       return=function(x){ # Vojta, returns the list
         if(missing(x)){
           what <- 'my'
         } else {
           what <- paste('my',x,sep='$')
         }
         what <- paste('my.return.list(',what,')',sep='')
         eval(parse(text=what)) 
       })
}


.mergelist <- function(lista, listb) {
  # return lista plus named components of listb not present in lista
  if(is.null(names(lista))) names(lista) <-rep("", length(lista))
  k <- seq(length(listb))[is.na(match(names(listb), names(lista)))]
  for(i in 1:length(k)) {
    x <- list(listb[[k[i]]])
    names(x) <- names(listb)[k[i]]
    lista <-c(lista, x)
  }
  lista
}

# part 3 - graphical objects

# create go from vectors
# x,y vectors, pch, col,cex constants,
# group vector same length as x giving group of each x,y pair,
#   numeric or character - unordered grouping
# group size of contiguous groups in x,y - ordered grouping
# sets grpnam only if group is character vector and grpsiz to NULL
# label character vector of labels for each group ( for legends)
as.pgo <- function(x,y=NULL,pch=1,col=1,cex=1,group=NULL,
                   label=NULL) {
  grpid <- NULL
  grpsiz <- NULL
  done<- FALSE
  while(!done) {
    # list
    if (is.list(x)) {
      xx <- x
      colnames <- names(xx)
      if (!is.na(match('x',colnames)))
        x <- xx$x
      if (!is.na(match('y',colnames)))
        y <- xx$y
      if (!is.na(match('col',colnames)) && missing(col))
        col <- xx$col
      if (!is.na(match('cex',colnames)) && missing(cex))
        cex <- xx$cex
      if (!is.na(match('pch',colnames)) && missing(pch))
        pch <- xx$pch
      if (!is.na(match('label', colnames)) && missing(label))
        label <- xx$label
      if (!is.na(match('group',colnames)) && missing(group)){
        grpsiz <- xx$group
        ng <- length(grpsiz)
      }else {
        if (!missing(group)){
          ng <- length(group)
          grpsiz <- group
        } else
          ng <- 1
      }
      if(length(x) != length(y))
        stop("lengths of x and y do not match")
      done <- TRUE
      break
    }
    # matrix
    if (is.matrix(x)){
      done <- TRUE
      m <- x
      ng <- nrow(m)
      if(ncol(m)< 2)
        stop("matrix with at least two columns required")
      colnames <- dimnames(m)[[2]]
      x <- m[,1]
      y <- m[,2]
      if (!is.na(match('col', colnames)))
        col <- m[,'col']
      if (!is.na(match('cex', colnames)))
        cex <- m[,'cex']
      if (!is.na(match('pch', colnames)))
        pch <- m[,'pch']
      if (!is.na(match('group', colnames)))
        grpid <- m[,'group']
      break
  }
    if(length(x) != length(y))
    stop("x and y lengths differ")
  # single group
  if (is.null(group)|| length(group) ==1){
    ng <- 1
    grpsiz <-length(x)
    if(length(pch) >1 || length(col) >1||length(cex)> 1|| length(label) > 1)
      warning("excess data in pch, col or cex")
    done <- TRUE
    break
  }
  # vectored groups
  if(length(group) == length(x)) {
    ng <- length(x)
    grpid <- group
    grpsiz <- NULL   
    done <- TRUE
    break
  } else {
    # group sizes
    if (sum(group) != length(x))
      stop("group size does not match x,y data")
    ng <- length(group)
    grpsiz <- group
    done <- TRUE
    break
 }
  }
  if(missing(col)) col <- rep(col,ng)
  if(missing(pch)) pch <- rep(pch,ng)
  if(missing(cex)) cex <- rep(cex,ng)
  if ( length(col) <ng ) {
    col <- .recycle(col,ng)
    warning("short of data in col - recycling" )
  }
  if ( length(cex) <ng ) {
    cex <- .recycle(cex,ng)
    warning("short of data in cex - recycling" )
  }
  if ( length(pch) <ng ) {
    pch <- .recycle(pch,ng)
    warning("short of data in pch - recycling" )
  }
    data <- list(x=x, y=y,grpsiz=grpsiz,grpid=grpid, pch=pch,
                 col=col, cex=cex, label=label)
    # class(data) <- "pgo"
    data
}            

is.pgo <- function(go) {
  !any(is.na( match(names(as.pgo(1,1)),names(go))))
}
is.lgo <- function(go) {
  !any(is.na( match(names(as.lgo(1,1)),names(go))))
}


# make line graphic object
# input x, y, lty, col, lwd as list, matrix or vectors
# accept one line only
as.lgo <- function(x, y=NULL, col=1, lty=1, lwd=1, label=NULL) {
  # extract elements of data
  done <- FALSE
  while (!done) {
  # list
  if (is.list(x)) {
    xx <- x
    colnames <- names(xx)
    if (!is.na(match('x',colnames)))
        x <- xx$x
    if (!is.na(match('y',colnames)))
        y <- xx$y
    # allow args to override list settings 
    if (!is.na(match('col',colnames)) && missing(col))
        col<- xx$col
    if (!is.na(match('lty',colnames)) && missing(lty))
        lty<- xx$lty
    if (!is.na(match('lwd',colnames)) && missing(lwd))
        lwd<- xx$lwd
    if (!is.na(match('label',colnames)) && missing(label))
        label <- xx$label
    done <- TRUE
    break
  }
  # matrix
  if (is.matrix(x)){
    m <- x
    if (ncol(m)<2) stop ("insufficient data in x matrix")
    x <- m[,1]
    y <- m[,2]
    done <- TRUE
    break
  }
  # vectors
  if (!missing(y)) {
    done <- TRUE
    break
  }
  stop ("usage: as.lgo(x,y,col,lty,lwd)")
}
  if (length(x) != length(y))
    stop("x and y lengths do not match")
  
#  # check number of line segments
#  ns <- length(which(is.na(x)))+1
#  # check parameters, .recycle if needed
#  if(length(col) < ns) {
#    col <- recycle(col, ns)
#    warning("short of data in col - recycling")
#  }
#  if(length(lty) < ns) {
#    lty <- .recycle(lty, ns)
#    warning("short of data in lty - recycling")
#  }
#  if(length(lwd) < ns) {
#    lwd <- .recycle(lwd, ns)
#    warning("short of data in lwd - recycling")
#  }
  
  # return
  list(x=list(x), y=list(y), col=col, lty=lty, lwd=lwd, label=label)
}

# combine pgo
# objects must be of same type, either points list or points matrix
# if mixed turn into points matrix, but may need to map lty and col
#   from char to int
cpgo<- function(...) {
a <- list(...)
d <- e <- NULL
# determine what we have got
  for( i in 1:length(a)) {
    b<- a[[i]]
    if(!is.null(b)) {
      if(!is.pgo(b)) stop(paste("item",i,"not a point graph object"))
      d <- c(d, b$grpid)
      e <- c(e, b$grpsiz)
    }
  }
if (is.null(d)){
  # cat ("list of grouped data\n")
  type <- "group"
}
if (is.null(e)) {
  # cat ("list of point data\n")
  type <- "point"
}
if (!is.null(d) && !is.null(e)) {
  # cat("mixed point and group types\n")
  type <- "mixte"
}
x <- y<-col <- cex <- pch <- grpsiz <- grpid <- label <- NULL
 for( i in 1:length(a)) {
   b<- a[[i]]
   if(!is.null(b)) {
   x <- c(x,b$x)
   y <- c(y,b$y)
   if (type=="mixte" && is.null(b$grpid)) {
       col <- c(col,rep(b$col, b$grpsiz))
       cex <- c(cex,rep(b$cex, b$grpsiz))
       pch <- c(pch,rep(b$pch, b$grpsiz))
       grpsiz <-NULL
       grpid <- c(grpid, rep(1:length(b$grpsiz),b$grpsiz))
       # labels for mixed modes unsupported
   }else{
     col <- c(col,b$col)
     cex <- c(cex,b$cex)
     pch<- c(pch,b$pch)
     grpsiz <- c(grpsiz, b$grpsiz)
     grpid <- c(grpid, b$grpid)
     label <- c(label, b$label)
   }   
 }
 }
if(type=="mixte" || type=="point")
  as.pgo(x,y,col=col,cex=cex,pch=pch, group=grpid, label=NULL)
else 
  as.pgo(x,y,col=col,cex=cex,pch=pch,group=grpsiz, label=label)
}

# combine lgo
clgo <- function(...) {
  a <- list(...)
  x <- y <- list()
  col<- lty <-lwd <- label <- NULL
  k <- 0
  for( i in 1:length(a)) {
     b <- a[[i]]
     if (!is.null(b)) {
       if(!is.lgo(b)) stop(paste("item",i,"not a line graph object"))
       for (j in 1:length(b$x)){
         k <- k+1
         x[[k]] <- b$x[[j]]
         y[[k]] <- b$y[[j]]
       }
       col <-  c(col,b$col)
       lty <-  c(lty,b$lty)
       lwd <-  c(lwd,b$lwd)
       label <- c(label, b$label)
     }
   }
    list(x=x,y=y, col=col, lty=lty, lwd=lwd, label=label)        
}

.if.else <- function(cond, x1, x2) {
    if(cond) return(x1)
    else return(x2)
  }

# make go
# as.go(points=list(), lines=list())
as.go.0 <- function(points=NULL, lines=NULL, xlab="", ylab="") {
  if(!is.null(points) && !is.pgo(points))
    stop("points arg is not a pgo in as.go")
  if(!is.null(lines) && !is.lgo(lines))
    stop("lines arg is not a lgo in as.go")
  g<-list(points=points, lines=lines,xlab=xlab, ylab=ylab)
  class(g) <- "go"
  g
}
as.go <- function(points=NULL,lines=NULL, xlab="", ylab="") {
  if(!is.null(points) && !is.pgo(points))
    stop("points arg is not a pgo in as.go")
  if(!is.null(lines) && !is.lgo(lines))
    stop("lines arg is not a lgo in as.go")
  g<-list(points=points, lines=lines, xlab=xlab, ylab=ylab)
  #class(g) <- "go"
this <- list(
             points=function(){
               g$points
             },
             lines=function(){
               g$lines
             },
             xlab=function(){
               g$xlab
             },
             ylab=function(){
               g$ylab
             },
             pset=function(...){
               args <- list(...)
               vargs<- c("col","cex","pch","label")
               argnames<- match(names(args),vargs)
               for(i in 1:length(args)){
                 if ( !is.na(argnames[i])) {
                   if (vargs[argnames[i]] == "col")
                     g$points$col<<-args[[i]]
                   if (vargs[argnames[i]] == "cex")
                     g$points$cex<<-args[[i]]
                   if (vargs[argnames[i]] == "pch")
                     g$points$pch<<-args[[i]]
                   if (vargs[argnames[i]] == "label")
                     g$points$label<<-args[[i]]
                 } else {
                   cat("ignored invalid argument", names(args)[i],"\n")
                 }
               }
             },
             lset=function(...){
               args <- list(...)
               vargs<- c("col","lty","lwd","label")
               argnames<- match(names(args),vargs)
               for(i in 1:length(args)){
                 if ( !is.na(argnames[i])) {
                   if (vargs[argnames[i]] == "col")
                     g$lines$col<<-args[[i]]
                   if (vargs[argnames[i]] == "lty")
                     g$lines$lty<<-args[[i]]
                   if (vargs[argnames[i]] == "lwd")
                     g$lines$lwd<<-args[[i]]
                   if (vargs[argnames[i]] == "label")
                     g$lines$label<<-args[[i]]
                 } else {
                   cat("ignored invalid argument", names(args)[i],"\n")
                 }
               }
             }    
           )
  class(this) <- "go"
  this
}

ngo.0 <- function(g) {
 
  this <-list(
       x=function(){
         g$points$x
       },
       y=function() {
         g$points$y
       },
       col=function() {
         g$points$col
       },
       grpsiz=function() {
         g$points$grpsiz
       },
       show=function(){
            print(g$points$col)
            },
       set=function(col=c("yellow", "green")) {
            g$points$col<<-col
          }
       )
  class (this) <- "ngo"
  this
}

# combine go
cgo <- function(...) {
  a <- list(...)
  l <- p <- NULL
  main <- xlab<-ylab<-NULL
  for (i in 1:length(a)){
    b <- a[[i]]
    if (class(b) !="go") stop(paste("data of wrong type - arg",i))
    if (i==1){
      if (!is.null(b$points())) p <-cpgo(NULL, b$points())
      if (!is.null(b$lines())) l <-clgo(NULL, b$lines())
    } else {
      p <- cpgo(p,b$points())
      l <- clgo(l, b$lines())
    }
  }
  as.go(points=p,lines=l)
}

.recycle <- function(short, size) {
  n <- length(short)
  k <- ceiling(size/length(short))
  rep(short,k)[1:size]
}
 # plot.ngo class function
plotGo <- function(x, main=NULL, sub=NULL, xlab=NULL, ylab=NULL,...) {
  g <- x
  if(class(g) != "go") stop("data not a graphic object")
  if(missing(xlab)) xlab<-g$xlab()
  if(missing(ylab)) ylab<-g$ylab()
  # only points
  if (is.null(g$lines())) {
  p <- g$points()
  plot(p$x, p$y, col=rep(p$col,p$grpsiz), pch=rep(p$pch, p$grpsiz),
       cex=rep(p$cex,p$grpsiz),main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
    return(invisible())
}
  # only lines
  if (is.null(g$points())) {
    l <- g$lines()
    plot(unlist(l$x),unlist(l$y), type="n", main=main, sub=sub, xlab=xlab, ylab=ylab )
    for( i in 1:length(l$col)) {
      lines(l$x[[i]], l$y[[i]], col=l$col[[i]], lty=l$lty[[i]],
          lwd=l$lwd[[i]], ...)
    }
    return(invisible())
  }
  # points and lines
  p <- g$points()
  l <- g$lines()
  xr <- range(p$x, unlist(l$x))
  yr <- range(p$y, unlist(l$y))
  plot(p$x, p$y, pch=rep(p$pch, p$grpsiz), col=rep(p$col,p$grpsiz),
         cex=rep(p$cex,p$grpsiz), xlim=xr, ylim=yr, main=main, sub=sub,
       xlab=xlab, ylab=ylab, ...)
  for( i in 1:length(l$col)) {
      lines(l$x[[i]], l$y[[i]], col=l$col[[i]], lty=l$lty[[i]],
          lwd=l$lwd[[i]])
    }
    return(invisible())   
}

# plotGo.0 class function
plotGo.0 <- function(g, main=NULL, sub=NULL, xlab=NULL, ylab=NULL,...) {
  if(class(g) != "go") stop("data not a graphic object")
  if(missing(xlab)) xlab<-g$xlab
  if( missing(ylab)) ylab<-g$ylab
  # only points
  if (is.null(g$lines)) {
    p <- g$points
    plot(p$x, p$y, pch=rep(p$pch, p$grpsiz), col=rep(p$col,p$grpsiz),
         cex=rep(p$cex,p$grpsiz),main=main, sub=sub,xlab=xlab, ylab=ylab, ...)
    return(invisible())
  }
  # only lines
  if (is.null(g$points)) {
    l <- g$lines
    plot(unlist(l$x),unlist(l$y), type="n", main=main, sub=sub, xlab=xlab, ylab=ylab )
    for( i in 1:length(l$col)) {
      lines(l$x[[i]], l$y[[i]], col=l$col[[i]], lty=l$lty[[i]],
          lwd=l$lwd[[i]], ...)
    }
    return(invisible())
  }
  # points and lines
  p <- g$points
  l <- g$lines
  xr <- range(p$x, unlist(l$x))
  yr <- range(p$y, unlist(l$y))
  plot(p$x, p$y, pch=rep(p$pch, p$grpsiz), col=rep(p$col,p$grpsiz),
         cex=rep(p$cex,p$grpsiz), xlim=xr, ylim=yr, main=main, sub=sub,
       xlab=xlab, ylab=ylab, ...)
  for( i in 1:length(l$col)) {
      lines(l$x[[i]], l$y[[i]], col=l$col[[i]], lty=l$lty[[i]],
          lwd=l$lwd[[i]])
    }
    return(invisible())
}
legend.go <- function(go,xy=locator(1)) {
  pt <- ln <- NULL
  if(!is.null(go$points()) && is.pgo(go$points()))
    pt <-go$points()   
  if(!is.null(go$lines()) && is.lgo(go$lines()))
    ln <- go$lines()
  if (is.null(pt) && is.null(ln)){
    cat(deparse(substitute(go)),"is not a graphic object\n")
    return(invisible())
  }
  if (!is.null(pt$label) && is.null(ln$label)) {
    legend(xy, legend=pt$label, pch=pt$pch, col=pt$col,cex=1)
    return(xy)
  }
  if (!is.null(ln$label) && is.null(pt$label)) {
    legend(xy , legend=ln$label, lty=ln$lty, lwd=ln$lwd, col=ln$col)
    return(xy)
  }
  if (!is.null(pt$label) && !is.null(ln$label)){
    k <- length(ln$label)
    m <- length(pt$label)
    legend(xy,legend=c(pt$label, ln$label),
           col=c(pt$col, ln$col),
           pch=c(pt$pch,rep(-1,k)),
           lty=c(rep(-1,m),ln$lty),
           lwd=c(rep(-1,m),ln$lwd),
           cex=pt$cex, #By VJ
           bg=pt$bg, #By VJ
           ncol=pt$ncol, #By VJ
           bty=pt$bty, #By VJ
           xpd=pt$xpd, #By VJ
           merge=TRUE)
 sheet$demo$template<-list(leg=list("legend",x="center",y=NULL,legend=args$legend,col=args$col,pch=args$pch,
            cex=args$cex,lty="solid",bg="transparent",ncol=2,bty="o",xpd=FALSE))
            sheet$demo$call<-list(xlab="",ylab="",main=NULL,type="n",xlim=c(-1,1),ylim=c(-1,1),axes=FALSE)
  }       
 return(xy)
  
}
# need a modified version of plot.default to handle panel.first as a
# function call rather than as an expression
plot.default <- function(x, y=NULL, type="p", xlim=NULL, ylim=NULL,
             log="", main=NULL, sub=NULL, xlab=NULL, ylab=NULL,
             ann=par("ann"), axes=TRUE, frame.plot=axes,
             panel.first=NULL, panel.last=NULL,
             col=par("col"), bg=NA, pch=par("pch"),
             cex=1, lty=par("lty"), lab=par("lab"),
                         lwd=par("lwd"), asp=NA, ...)
{
    xlabel <- if (!missing(x)) deparse(substitute(x))
    ylabel <- if (!missing(y)) deparse(substitute(y))
    xy <- xy.coords(x, y, log=log)
    #xy <- xy.coords(x, y, xlabel, ylabel, log)
    xlab <- if (is.null(xlab)) xy$xlab else xlab
    ylab <- if (is.null(ylab)) xy$ylab else ylab
    xlim <- if (is.null(xlim)) range(xy$x[is.finite(xy$x)]) else xlim
    ylim <- if (is.null(ylim)) range(xy$y[is.finite(xy$y)]) else ylim
    old <-par(no.readonly = TRUE) 
    if(screen()){
        par(bg=sheet$demo$call$bg)
        #erase.screen(screen())
        screen(screen(),new=TRUE)
    }
    plot.new()
    plot.window(xlim, ylim, log, asp, ...)
    if(is.function(panel.first)) {
    panel.first()
      }else {
    panel.first
      }
    plot.xy(xy, type, col=col, pch=pch, cex=cex, bg=bg, lty=lty, lwd=lwd, ...)
    panel.last
    if (axes) {
        axis(1, ...)
        axis(2, ...)
    }
    if (frame.plot)
        box(...)
    if (ann)
        title(main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
    invisible()
  }
