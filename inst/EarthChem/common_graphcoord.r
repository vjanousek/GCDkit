#########################################################
#         Convert widget coordinates to graph coordinates
#########################################################

.PxToGraphCoords<-function(xpx,ypx,theplot,logX=F,logY=F){
    # Graph coordinates
    xMin<-theplot$mapx[2]
    xMax<-theplot$hpix-theplot$mapx[4]
    yMin<-theplot$vpix-theplot$mapx[1]
    yMax<-theplot$mapx[3]

    xRange<-sheet$demo$call$xlim  
    yRange<-sheet$demo$call$ylim

    if(logX){
        lxgraph<-log10(xRange[1])+(log10(xRange[2])-log10(xRange[1]))*(xpx-xMin)/(xMax-xMin)
        xgraph<-10^lxgraph
    }else{
        xgraph<-xRange[1]+(xRange[2]-xRange[1])*(xpx-xMin)/(xMax-xMin)
    }

    if(logY){
        lygraph<-log10(yRange[1])+(log10(yRange[2])-log10(yRange[1]))*(ypx-yMin)/(yMax-yMin)
        ygraph<-10^lygraph
    }else{
        ygraph<-yRange[1]+(yRange[2]-yRange[1])*(ypx-yMin)/(yMax-yMin)
    }

    #cat("Click at",xpx,ypx,"\n")
    res<-c(xgraph,ygraph)
    names(res)<-NULL
    #cat("Coordinates",res,"\n")
    return(res)
}
