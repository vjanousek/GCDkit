##############################################################
#
#     Modif of tkrplot
#
##############################################################

## Modified after tkrplot so that it does not depend on the resolution
# of the local system...
# We generate functions using the size of figure in px, not in inches...

#Windows only !

.my.tkdev.px <- function(hpix=400, vpix=400){
        h.in<-grconvertX(hpix,from="device",to="inches")
        v.in<-grconvertX(vpix,from="device",to="inches")
        # does not feel right to use grconvertX in both directions, 
        # but using grconvertY gives weird results !
        win.metafile(width=h.in,height=v.in,restoreConsole=FALSE)
}

.make.tkindex <-
    local({
        .My.Tk.index <- 0
        function() {
            .My.Tk.index <<- .My.Tk.index + 1
            .My.Tk.index
        }
    })

tkrplot.px <- function(parent, fun, hpix=400, vpix=400) {
    image <- paste("Rplot", .make.tkindex(), sep="") 
    .my.tkdev.px(hpix, vpix)
    #margins  -- not in the original code
    # bottom
    m.bot<-grconvertX(par()$mai[1],from="inches",to="device")
    # left
    m.left<-grconvertX(par()$mai[2],from="inches",to="device")
    # top
    m.top<-grconvertX(par()$mai[3],from="inches",to="device")
    # right
    m.right<-grconvertX(par()$mai[4],from="inches",to="device")
    try(fun())
    .Tcl(paste("image create Rplot", image))
    lab<-tklabel(parent,image=image) #**** use try, delete image on failure
    tkbind(lab,"<Destroy>", function() .Tcl(paste("image delete", image)))
    lab$image <- image
    lab$fun <- fun
    lab$hpix <- hpix     # original code was using scale
    lab$vpix <- vpix
    # combine and add margins to lab
    lab$mapx<-c(m.bot,m.left,m.top,m.right)
    lab
}

tkrreplot.px <- function(lab, fun = lab$fun,hpix=lab$hpix, vpix=lab$vpix) {
    .my.tkdev.px(hpix, vpix)
    try(fun())
    #try(figRedraw())
    .Tcl(paste("image create Rplot", lab$image))
}

###### end of tkrplot modified code ##################
