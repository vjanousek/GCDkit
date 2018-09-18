.copydevSWord<-function(filename,which="PDF",newdir=FALSE){
   on.exit(options(show.error.messages=TRUE))
   which<-tolower(which)
   # Dimensions
   width<-6
   height<-6
   pointsize<-12
   
   .copyCore<-function(format,newdir=FALSE){
        if(format=="postscript")format<-"ps"
        if(newdir){
            dir.create(paste(getwd(),toupper(format),sep="/"))
            file.out<-paste(getwd(),"/",format,"/",filename,".",format,sep="")
        }else{
            if(dirname(filename)=="."){
                file.out<-paste(getwd(),"/",filename,".",format,sep="")
            }else{
                file.out<-filename
            }
        }
        ee<-switch(format,
                ps=postscript(file.out, horizontal=FALSE, onefile=FALSE, paper="special", width=width, height=height),
                eps=postscript(file.out, horizontal=FALSE, onefile=FALSE, paper = "special", width=width, height=height),
                png=png(file.out,width=width,height=height,units="in",pointsize = pointsize, bg = "white",res=75),
                pdf=pdf(file.out, paper="a4"),
                tif=tiff(file.out,width=width,height=height,units="in",pointsize=pointsize,compression="lzw", bg = "white",res=75),
                wmf=win.metafile(file.out,pointsize = pointsize),
                paste("Format",format,"is not supported!")
        #bmp, jpeg...??? or rather not.
        )
        if(!is.null(ee)){
            cat(ee,"\n")
            options(show.error.messages=FALSE)
            stop(call. = TRUE)    
        }
        mydev<-dev.cur()
        ee<-dev.set(dev.prev())
        ee<-dev.copy(which=mydev)
        ee<-dev.off(which = mydev)
        invisible(format)
   }
   z<-sapply(which,.copyCore,simplify=TRUE,newdir=newdir)
   z<-paste(filename,"--->",paste(names(z),collapse=", "))
   if(getOption("gcd.shut.up"))invisible() else return(z)
}



  

   
