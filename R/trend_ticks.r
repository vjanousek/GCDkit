trendTicks<-function(equation,x,xmin=par("usr")[1],xmax=par("usr")[2],tick=abs(par("tcl")),text=FALSE,col="blue",lty="solid",lwd=1,arrow=FALSE,autoscale=TRUE){
     # TODO implementovat xminor
     on.exit(options("show.error.messages"=TRUE))
     #Debugging
     options(show.error.messages = TRUE)
     par.old<-par()
     
     # Is equation a character string?
     if(!is.call(equation)&!is.expression(equation)){
            # Wrong formula!
            selected <- try(parse(text = equation))
            if (class(selected) == "try-error") {
                winDialog(type = "ok", "Syntax error in formula!")
                stop("", call. = FALSE)
            }
            expr<-parse(text=equation)
     }else{
        expr<-as.expression(equation)    
     }
     
     # Calculate values y coordinate for given x
     y<-eval(expr)
    
     # Draw the curve
     curve.ee<-parse(text = paste("out<-curve(", expr,",",xmin,",",xmax,",col=\"", col, "\",lwd=", lwd,",lty=\"", lty, "\",n=300,add=TRUE)", sep = ""))
     eval(curve.ee)
     
     # Autoscale to accommodate both data and trend? Works only for Figaro compatible plots!!!
     if(autoscale){
        if(par("xlog")==""){
            figXlim(extendrange(c(out$x,x.data),f=0.05))
        }else{
            figXlim(exp(extendrange(log(c(out$x,x.data)),f=0.05)))
        }
        
        if(par("ylog")==""){
            figYlim(extendrange(c(out$y,y.data),f=0.05))
        }else{
            figYlim(exp(extendrange(log(c(out$y,y.data)),f=0.05)))
        }
        eval(curve.ee)  
     }
    
     # Aspect ratio in user coordinates
     delta_y<-par("usr")[4]-par("usr")[3]
     delta_x<-par("usr")[2]-par("usr")[1]
     distort<-delta_y/delta_x
     distort.scr<-par()$din[2]/par()$din[1]  # Aspect ratio of the current plotting window, TO DO STILL TO BE TAKEN INTO ACCOUNT
     
     # Transform current plotting window to normalized coordinates 
     #par.old<-par()
     xlog<-par("xlog")
     ylog<-par("ylog")
  
     if(xlog){
        usr.x<-grconvertX(10^(par("usr")[1:2]), from = "user", to = "npc")
     }else{
        usr.x<-grconvertX(par("usr")[1:2], from = "user", to = "npc")
     }
    
     if(ylog){
        usr.y<-grconvertY(10^(par("usr")[3:4]), from = "user", to = "npc")
     }else{
        usr.y<-grconvertY(par("usr")[3:4], from = "user", to = "npc")
     }
     
     # Coordinates of the tick intersections in normalized picture coordinates
     x.norm<-grconvertX(x, from = "user", to = "npc")
     y.norm<-grconvertY(y, from = "user", to = "npc")
     
     if(xlog){
        xx<-grconvertX(out$x, from = "user", to = "npc")
        yy<-grconvertY(out$y, from = "user", to = "npc")
     }
     
     # Get rid of logarithmic axes
     par("xlog"=FALSE)
     par("ylog"=FALSE) 
     
     # Set up the linear normalized coordinate system, test it
     par("usr"=c(usr.x,usr.y))     
     
     # For debugging show the normalized coordinate system
     #axis(3,col.axis="red")
     #axis(4,col.axis="red")
     
     #Plot the points for which derivations are to be determined
     #points(x.norm,y.norm,col=1:length(x),pch=19,cex=1.3)
      
     # Tangent in normalized coordinates; derivation of 'equation' gives the slope
     # in usr coordinates, but needs to be transformed to normalized ccords using "distort"
     
     # Debugging
     #rug(seq(0,1,0.1),side=1,col="violet")
     #rug(seq(0,1,0.1),side=2,col="violet")
     #rug(seq(0,1,0.1),side=3,col="violet")
     #rug(seq(0,1,0.1),side=4,col="violet")
     x.bak<-x
     if(xlog){
        # Fit a polynomial
        fit2<-lm(yy~poly(xx, 4, raw=TRUE))
        #points(xx, predict(fit2), type="l", col="red")
        
        # make an equation out of it
        
        eq2<-parse(text=paste(fit2[[1]][1],"+",fit2[[1]][2],"*x+",fit2[[1]][3],"*x^2+",fit2[[1]][4],"*x^3+",fit2[[1]][5],"*x^4",sep=""))
        
        #debugging, plot the curve fitted
        #curve.ee2<-parse(text = paste("out<-curve(", as.expression(eq2),",col=6,lwd=2,lty=3,n=300,add=TRUE)", sep = ""))    
        #eval(curve.ee2)
        x<-x.norm
        
        # Symbolic derivation of the polynomial just fitted
        expr2<-parse(text=as.expression(eq2))
        ee<-D(expr2,"x")
     
     }else{
        ee<-D(expr,"x") # xlog = FALSE, ylog = FALSE or TRUE
     }
     
     if(ylog&!xlog){
        uf<-as.character(as.expression(ee))
        uf<-paste("1/(",as.character(expr),"*log(10))","*",uf,sep="")
        ee<-parse(text=uf)
     } 
     
     # Numeric derivation
     derivace<-eval(ee)
     if(length(derivace)==1) derivace<-rep(eval(ee),length(x)) # If is derivation a constant
     if(!xlog){
        k1<-derivace/distort#*distort.scr
     }else{   
        k1<-derivace
     }
     
     k1[k1==Inf]<-1e10 # abline cannot take Inf
     k1[k1==-Inf]<--1e10
     
     q1<-y.norm-k1*(x.norm)
     
     # Debugging, plot the tangent line
     #eee<-lapply(1:length(q1),function(i){
        #abline(q1[i],k1[i],col=i,lty="dashed")
     #})
           
     # Perpendicular line in normalized coordinates
     k2<--1/k1
     k2[k2==Inf]<-1e10
     k2[k2==-Inf]<--1e10
     q2<-y.norm-k2*x.norm
     
     # Debugging, plot the line
     eee<-lapply(1:length(q2),function(i){
        #abline(q2[i],k2[i],col=i,lty="dashed")
     })
     
     # Get the ticks of the equal length, regardless of the orientation
     #tick<-0.01
     tick=0.02*tick
     offset<--sin(atan(k1))*tick #Delta x
     x0<-x.norm-offset
     x2<-x.norm+offset
     
     # Do not draw the last tick if there is to be an arrowhead
     if(arrow){
        x0<-x0[-length(x0)]
        x2<-x2[-length(x2)]
        k2<-k2[-length(k2)]
        q2<-q2[-length(q2)]
     }
     
     # Draw all ticks in a single go
     #eq<-paste(k2,"*x+",q2,sep="")
     #expres<-parse(text = paste("out<-curve(", as.expression(eq),",",x0,",",x2,",col=\"", col, "\",lwd=\"", lwd,"\",lty=\"", lty, "\",n=2,add=TRUE)", sep = ""))
     #print(parse(text=expres))
     #eval(expres)
     segments(x0,q2+k2*x0,x2,q2+k2*x2,col=col,lwd=lwd,lty=lty)
    
    # ARROW HEADS
    if(arrow){
        #The tip
        i<-length(x)
        xx1<-x.norm[i]
        yy1<-y.norm[i]
       
        # Fix the orientation
        if((delta_x>0 & x.norm[i]<x.norm[1])|(delta_x<0 & x.norm[i]<x.norm[1])){
            sign<-1
        }else{
            sign<--1
        }
        
        # First segment
        offset1<-(cos(atan(k1[i])+pi/6)*tick*2)
        k3<-tan(atan(k1[i])+pi/6)
        q3<-y.norm[i]-k3*x.norm[i]
        
        xx0<-xx1+sign*offset1
        yy0<-k3*(xx0)+q3
        #lines(c(x1,x0),c(y1,y0),col="red",lwd=lwd)
        
        # Second segment
        offset2<-(cos(atan(k1[i])-pi/6)*tick*2)
        k4<-tan(atan(k1[i])-pi/6)
        q4<-y.norm[i]-k4*x.norm[i]
        
        xx2<-xx1+sign*offset2
        yy2<-k4*(xx2)+q4
        #lines(c(xx2,xx1),c(yy2,yy1),col="green",lwd=lwd)
        
        # Draw it
        lines(c(xx0,xx1,xx2),c(yy0,yy1,yy2),lwd=lwd,col=col)
    }
    
    # Annotate text if reqrd
    x<-x.bak
    if(arrow){
        x<-x[-length(x)]
    }
     
    if(text){
        ee<-lapply(1:length(x),function(i){
            text(x2[i],k2[i]*x2[i]+q2[i],x[i],cex=0.75,col=col,font=3,srt=k2[i]/pi*180,offset=0.7,adj=0)
        })
    }
    
    # Restore the original user coordinates

    par("usr"=par.old$usr)
    par("xlog"=par.old$xlog)
    par("ylog"=par.old$ylog)

    out$y<-y
    out$x<-x.bak
    return(out)
}

.R.ex<-function(){
    c0<-100
    assign("c0",c0,.GlobalEnv)
    FF<-seq(0.1,0.5,0.05)
    #FF<-seq(1,0.5,-0.1)
    plot(1,1,type="n",xlim=c(0.7,0.1),ylim=c(1,1000),xlab="F",ylab="cL",log="x")
    abline(h=1,lty="dashed")

    equation<-"c0*x^(D-1)"
    
    DDD<-c(5,2,0.5,0.2,0.1,0)
    #DDD<-0
    ee<-lapply(1:length(DDD),function(i){
        assign("D",DDD[i],.GlobalEnv)
        trendTicks(equation,FF,1,min(FF),max(FF),col=i,lty="solid",lwd=2,arrow=TRUE,text=FALSE)
    })
}

.LogExample<-function(){
    windows(8,4)
    plot(1,1,type="n",xlim=c(3000,30),ylim=c(5,500),xlab="Rb",ylab="Sr",log="")
    equation<-"1/x"
    x<-seq(500,2000,by=100)
    trendTicks(equation,x,1,min(x),max(x),col="blue",lty="solid",lwd=2,arrow=TRUE,text=FALSE)
}

.LogExample2<-function(){
    plot(1,1,type="n",xlim=c(0.01,1),ylim=c(0,1),xlab="Rb",ylab="Sr",log="x")
    equation<-"6*x/8"
    x<-seq(0.01,1,by=0.1)
    eval(parse(text=paste("curve(",as.expression(equation),",col=\"red\",add=TRUE)",sep="")))
    trendTicks(equation,x,1,min(x),max(x),col="blue",lty="solid",lwd=2,arrow=TRUE,text=FALSE)
}
