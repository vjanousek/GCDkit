# object is the whole map

############################################################################
#                          Set up an ArcView map                           #
############################################################################

ArcMapSetup<-function(object,layers=NULL,map.col=NULL,map.palette="heat.colours",labels.txt=FALSE,col.txt="black",cex.txt=0.5,axes=TRUE,longlat=TRUE,xlab="Longitude",ylab="Latitude"){
    # Start the rgdal library
    options(warn=-1)
    uf<-requireNamespace("rgdal",quietly=TRUE)
    if(!uf){
        #uf<-require(rgdal) 
        #if(!uf){winDialog(type = "ok","rgdal library is absent");stop()}
        cat("WARNING: Library rgdal is missing! Quitting...\n")
    }
    
    # Setup the template
    temp<-list(GCDkit=list(plot.type="GIS"))
    if(is.null(layers)){
        layers<-1:length(object)
    }
    for(j in layers){
            eval(parse(text=paste("temp$map",j,"<-list(\"ArcGIS\",object=object[[j]],map.col=map.col,map.palette=map.palette,labels.txt=labels.txt,col.txt=col.txt,cex.txt=cex.txt)",sep="")))
    }
    sheet$d$t$map1$longlat<-TRUE   
    sheet<<-list(demo=list(fun="plot",call=list(xlim=object[[1]]@bbox[1,],ylim=object[[1]]@bbox[2,],axes=axes,xlab=xlab,ylab=ylab,longlat=longlat),template=temp))
    win.metafile()
    pp<<-figaro(demo,prefix="sheet")
    dev.off(dev.cur())
    invisible()
}
.figArcMapDraw<-function(object,map.col=NULL,map.palette="topo.colors",labels.txt=TRUE,col.txt="black",cex.txt=0.5,xlim=NULL,ylim=NULL){
    if(is.null(map.col)) map.col<-selectPalette(length(object),map.palette[1])
    if(is.null(xlim)) xlim<-sheet$demo$call$xlim
    if(is.null(ylim)) ylim<-sheet$demo$call$ylim
    if(!is.list(object)) object<-list(prd=object)
    
    # Plot it
    if(any(slotNames(GCDmap[[1]])=="polygons")){
        plot(object[[1]],bg="transparent",col=map.col,axes=FALSE,xlim=xlim,ylim=ylim,add=TRUE,new=FALSE) 
    }else{
        plot(object[[1]],bg="transparent",col=map.col,axes=FALSE,xlim=xlim,ylim=ylim,pch=19,add=TRUE,new=FALSE)
    }
        # Plot/label centres of polygons
        if(labels.txt){
            txt.coord<-getSpatialPolygonsLabelPoints(object[[1]])
            #points(txt.coord,col="red",cex=2)
            text(data.frame(txt.coord),col=col.txt,labels=as.vector(object[[1]]@data[,2]),cex=cex.txt)
        }
    sheet$demo$call$xlim<<-round(par("usr")[1:2],2)
    sheet$demo$call$ylim<<-round(par("usr")[3:4],2)
    # Plot meridians if reqrd
    if(sheet$demo$call$longlat)llgridlines(GCDmap[[1]])
}
