# Make sure to put the file in the right place
# Also remember common_mytkrplot.r is used elsewhere (tk binary GUI)
# It may not be wise to have different versions
# Although it should be fairly stable

##############################################
#         selectPgonMap() function
#       returns a list with the x and y of the selection pgon
#      shp.file: full path to a shape file to be used as basemap
##############################################
selectPgonMap<-function(shp.file){
    GCDmap<<-NULL
    #setwd(earthchem.dir)
    loadData(shp.file)
    palette<-"terrain.colors"
    GCDmap<-get("GCDmap",.GlobalEnv)
    flush.console()
    map.col<-selectPalette(length(GCDmap[[1]]),palette)
    
    graphics.off()
    win.metafile()
    
    xMinExtd<-sheet$demo$call$xlim["min"]
    xMaxExtd<-sheet$demo$call$xlim["max"]
    yMinExtd<-sheet$demo$call$ylim["min"]
    yMaxExtd<-sheet$demo$call$ylim["max"]

    pgon.x<-NULL
    pgon.y<-NULL

    ##############################################
    #         Plotting function                   
    ##############################################
    make.map<-function(){
        old.bg<-par()$bg
        par(bg="white")
   
        xMin<-sheet$demo$call$xlim["min"]
        xMax<-sheet$demo$call$xlim["max"]
        yMin<-sheet$demo$call$ylim["min"]
        yMax<-sheet$demo$call$ylim["max"]
        plot(GCDmap[[1]],col=map.col,axes=TRUE,xaxs="i",yaxs="i",add=FALSE,longlat=FALSE,xlim=c(xMin,xMax),ylim=c(yMin,yMax),type="n")  
   
        # Add a polygon  
        if(length(pgon.x)==1){
            points(pgon.x,pgon.y,col="red",pch=19)
        }
        
        if(length(pgon.x)==2){
            lines(pgon.x,pgon.y,col="red",lwd=2)
        }
        
        if(length(pgon.x)>2){
            polygon(pgon.x,pgon.y,border="red",lty="solid",lwd=2,density=5,angle=45,add=T)
        }           
        par(bg=old.bg)
    }
 
    ##############################################
    #         React to a click in the map         
    ##############################################

    ### Convert widget coordinates to graph coordinates
    #.PxToGraphCoords<-function(xpx,ypx){
    #
    ## Graph coordinates
    #
    #xMinPx<-map.img$mapx[2]
    #xMaxPx<-map.img$hpix-map.img$mapx[4]
    #yMinPx<-map.img$vpix-map.img$mapx[1]
    #yMaxPx<-map.img$mapx[3]
    #
    #xMin<-sheet$demo$call$xlim["min"]
    #xMax<-sheet$demo$call$xlim["max"]
    #yMin<-sheet$demo$call$ylim["min"]  
    #yMax<-sheet$demo$call$ylim["max"]
    #
    #xgraph<-xMin+(xMax-xMin)*(xpx-xMinPx)/(xMaxPx-xMinPx)
    #ygraph<-yMin+(yMax-yMin)*(ypx-yMinPx)/(yMaxPx-yMinPx)
    #
    #res<-c(xgraph,ygraph)
    #names(res)<-NULL
    #return(res)
    #}

    ### Not used -- for testing purposes
    .OnLeftClick.test <- function(x,y){
        # Pixel coordinates      
        xClick <- as.numeric(x)
        yClick <- as.numeric(y)
        cat("Click at X=",xClick," ; Y=",yClick,"\n") 

        ee<-.PxToGraphCoords(xClick,yClick,map.img)
        xPlotCoord<-ee[1] 
        yPlotCoord<-ee[2]
        
        cat("Graph coordinates X=",xPlotCoord," ; Y=",yPlotCoord,"\n") 
    }

    OnLeftClick<-function(x,y){
        switch(tclvalue(t.sel),
            "in"=Zoom(x,y,0.5),
            "out"=Zoom(x,y,2),
            "cent"=Zoom(x,y,1), 
            "sel"=SelectPgon(x,y))     
    }

    ##############################################
    #                  Actions                    
    ##############################################

    Zoom<-function(x,y,zf){
        ee<-.PxToGraphCoords(as.numeric(x),as.numeric(y),map.img)
        #cat("zoom, zf=",zf,"x=",ee[1],"y=",ee[2],"\n")
        map.w<-sheet$demo$call$xlim["max"]-sheet$demo$call$xlim["min"]
        map.h<-sheet$demo$call$ylim["max"]-sheet$demo$call$ylim["min"]
  
        xmin.prov<-ee[1]-map.w*zf/2
        xmax.prov<-ee[1]+map.w*zf/2
        ymin.prov<-ee[2]-map.h*zf/2
        ymax.prov<-ee[2]+map.h*zf/2
 
        xmin<-max(xMinExtd,xmin.prov)
        xmax<-min(xMaxExtd,xmax.prov)
        ymin<-max(yMinExtd,ymin.prov)
        ymax<-min(xMaxExtd,ymax.prov)
  
        sheet$demo$call$xlim["min"]<<-xmin
        sheet$demo$call$xlim["max"]<<-xmax
        sheet$demo$call$ylim["min"]<<-ymin
        sheet$demo$call$ylim["max"]<<-ymax
  
        tkrreplot.pxC(map.img)
    }

    SelectPgon<-function(x,y){
        ee<-.PxToGraphCoords(as.numeric(x),as.numeric(y),map.img)
        pgon.x<<-c(pgon.x,ee[1])
        pgon.y<<-c(pgon.y,ee[2])
        tkrreplot.pxC(map.img)
    }

    ##############################################
    #            Actions on buttons               
    ##############################################
    clear.pgon<-function(){
        pgon.x<<-NULL
        pgon.y<<-NULL
        tkrreplot.pxC(map.img)
    }
 
    onQuit<-function(){
        tkdestroy(tmap)
        tkraise(tt)
    }

    ##############################################
    #         Build interface                     
    ##############################################

    ### Top-level
    tmap<-tktoplevel()
    tkwm.title(tmap,"Map of the World")

    ### Map 
    # Not used, but could be used to fit the window to the screen if required...
    #GUI.AvailableScreenWidth <- round(as.numeric(tkwinfo("screenwidth", 
    #        tmap)))
    #GUI.AvailableScreenHeight <- round(as.numeric(tkwinfo("screenheight", 
    #        tmap)))

    hv<-1.585   ## Empirical, looks good like that!
    vert<-500
    hrz<-vert*hv    
    map.frame<- ttklabelframe(tmap, text="")
    map.img <- tkrplot.px(map.frame,fun=make.map,hpix=hrz,vpix=vert)
    tkbind(map.img,"<Button-1>",OnLeftClick)

    ### Alternative bindings
    # Left here, but beware - both single and double click are binded so both actions
    # will occur on double click...

    OnRightClick<-function(x,y){
        Zoom(x,y,2)
    }
    tkbind(map.img,"<Button-3>",OnRightClick)

    OnDoubleClick<-function(x,y){
        Zoom(x,y,0.5)
    }
    tkbind(map.img,"<Double-Button-1>",OnDoubleClick)

    ### Radio buttons
    tool.frame <- ttklabelframe(tmap, text="Select tool")

    tb1 <- ttkradiobutton(tool.frame)
    tb2 <- ttkradiobutton(tool.frame)
    tb3 <- ttkradiobutton(tool.frame)
    tb4 <- ttkradiobutton(tool.frame)

    t.sel <- tclVar("cent")

    foo<-function(){}
    tkconfigure(tb1,variable=t.sel,value="in",command=function() foo())
    tkconfigure(tb2,variable=t.sel,value="out",command=function() foo())
    tkconfigure(tb3,variable=t.sel,value="cent",command=function() foo())
    tkconfigure(tb4,variable=t.sel,value="sel",command=function() foo())

    tb1.lab<-tklabel(tool.frame,text="Zoom in")
    tb2.lab<-tklabel(tool.frame,text="Zoom out")
    tb3.lab<-tklabel(tool.frame,text="Center")
    tb4.lab<-tklabel(tool.frame,text="Select")

    ### Big buttons
    clear.but<-tkbutton(tool.frame,text="Clear",command=function() clear.pgon())
    ok.but<-tkbutton(tool.frame,text="Ok",command=function() onQuit())

    ### Grid
    tkgrid(map.frame,tool.frame)
    tkgrid(map.img)
    tkgrid(tb1,tb1.lab)
    tkgrid(tb2,tb2.lab)
    tkgrid(tb3,tb3.lab)
    tkgrid(tb4,tb4.lab)

    tkgrid(clear.but,ok.but)

    tkwait.window(tmap)
    tkfocus(tt)
    return(list(x=pgon.x,y=pgon.y))
}     # end of selectPgonMap()

##### Usage
#qq<-selectPgonMap("world_country_admin_boundary_shapefile_with_fips_codes.shp")
