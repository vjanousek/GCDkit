.GetValue <- function(ee) {
    variable <- as.character(tclvalue(ee))
    if (variable==""){
        return (NULL)
    }else{
        return(variable)
    }
}


.ECInputList <- function() {
    .SillyDates <- function() {
        tkmessageBox(title="Warning",message="Warning! You introduced very suspicious publication dates!",icon="warning",type="ok")
        stop()
    }
  
    .InvalidDates <- function() {
        tkmessageBox(title="Error",message="Warning! You introduced invalid publication dates!",icon="error",type="ok")
        stop()
    }
  
    .InvalidAges <- function() {
        tkmessageBox(title="Error",message="Warning! You introduced invalid age(s)!",icon="error",type="ok")
        stop()
    }
    
    .InvalidLoc <- function() {
        tkmessageBox(title="Error",message="Warning! You introduced invalid coordinates for the bounding box!",icon="error",type="ok")
        stop()
    }
  
    next.year<-as.numeric(format(Sys.time(), "%Y"))+1
  
    if (as.character(tclvalue(rmValue))=="not_defined") rmValue <<- tclVar()
    
    if (as.character(tclvalue(ryValue))=="Nodate") {
        minpubyear.t <<- tclVar()
        maxpubyear.t <<- tclVar()
        exactpubyear.t <<- tclVar()
    }
    
    if (as.character(tclvalue(ryValue))=="ExactPubYear") {
        minpubyear.t <<- tclVar()
        maxpubyear.t <<- tclVar()
        
        if (is.na(as.numeric(tclvalue(exactpubyear.t)))) {
            .InvalidDates()
        }else{
            if (as.numeric(tclvalue(exactpubyear.t))<1800|as.numeric(tclvalue(exactpubyear.t))>next.year) {.SillyDates()}
        }                    
    }
    
    if (as.character(tclvalue(ryValue))=="MinMaxPubYear") {
        exactpubyear.t <<- tclVar()
        if (is.na(as.numeric(tclvalue(minpubyear.t)))|is.na(as.numeric(tclvalue(maxpubyear.t)))) {
            .InvalidDates()
        }else{
            if(as.numeric(tclvalue(minpubyear.t))>as.numeric(tclvalue(maxpubyear.t))) .InvalidDates()
            if (as.numeric(tclvalue(minpubyear.t))<1800|as.numeric(tclvalue(minpubyear.t))>next.year|as.numeric(tclvalue(maxpubyear.t))<1800|as.numeric(tclvalue(maxpubyear.t))>next.year) {.SillyDates()}
        }
    }                                                
    if (as.character(tclvalue(rlValue))=="Noloc") {
        polygon.t <- NULL
        north.t <<- tclVar()
        south.t <<- tclVar()
        east.t <<- tclVar()
        west.t <<- tclVar()}
    
    if (as.character(tclvalue(rlValue))=="Locpolygon") {
        north.t <<- tclVar()
        south.t <<- tclVar()
        east.t <<- tclVar()
        west.t <<- tclVar()
    }
    
    if (as.character(tclvalue(rlValue))=="LocNSEW") {
        polygon.t <- NULL
    
        if(is.na(as.numeric(tclvalue(north.t)))|is.na(as.numeric(tclvalue(south.t)))|is.na(as.numeric(tclvalue(east.t)))|is.na(as.numeric(tclvalue(west.t)))) {
            .InvalidLoc()
        }else{
            if(as.numeric(tclvalue(north.t))>90|as.numeric(tclvalue(south.t))<(-90)|as.numeric(tclvalue(east.t))>180|as.numeric(tclvalue(west.t))<(-180)) .InvalidLoc
            if(as.numeric(tclvalue(north.t))<as.numeric(tclvalue(south.t))|as.numeric(tclvalue(east.t))<as.numeric(tclvalue(west.t))) .InvalidLoc 
        }
  }
  
  if (as.character(tclvalue(raValue))=="Noage") {
    minage.t <<- tclVar()
    maxage.t <<- tclVar()
    exage.t <<- tclVar()
    geolage.t <<- tclVar()
  }
  
  if (as.character(tclvalue(raValue))=="MinMaxAge") {
    exage.t <<- tclVar()
    geolage.t <<- tclVar()
    
    if(as.numeric(tclvalue(minage.t))>as.numeric(tclvalue(maxage.t))) .InvalidAges()
  }
  
  if (as.character(tclvalue(raValue))=="ExactAge") {
    minage.t <<- tclVar()
    maxage.t <<- tclVar()
    geolage.t <<- tclVar()
    
    if(is.na(as.numeric(tclvalue(exage.t)))) {
        .InvalidAges()
    }else{
      if(as.numeric(tclvalue(exage.t))<0|as.numeric(tclvalue(exage.t))>4560) .InvalidAges()
    }
  }
  
  if (as.character(tclvalue(raValue))=="GeolAge") {
    minage.t <<- tclVar()
    maxage.t <<- tclVar()
    exage.t <<- tclVar()
  }                                                  
  
  if (!is.null(oitems)) {
  for (i in 1:length(oitems)) {
    if (i == 1) {
        oitems.t <- oitems[i]
    }else{
        oitems.t <- paste(oitems.t,oitems[i],sep=",")}
    }
  }else{
    oitems.t <- NULL
  }
  
  rosValue <<- tclVar("yes") # Modified by VJ, standard items to be always present                                                  
  allquery <- list(author=.GetValue(author.t),title=.GetValue(title.t),journal=.GetValue(journal.t),doi=.GetValue(doi.t),minpubyear=.GetValue(minpubyear.t),maxpubyear=.GetValue(maxpubyear.t),exactpubyear=.GetValue(exactpubyear.t),keyword=.GetValue(keyword.t),sampleid=.GetValue(sampleid.t),polygon=polygon.t,north=.GetValue(north.t),east=.GetValue(east.t),south=.GetValue(south.t),west=.GetValue(west.t),level1=lev1,level2=lev2,level3=lev3,level4=lev4,minage=.GetValue(minage.t),maxage=.GetValue(maxage.t),exactage=.GetValue(exage.t),geologicalage=.GetValue(geolage.t),material=.GetValue(rmValue),searchtype="rowdata",outputtype=.GetValue(rotValue),jsonfunction=NULL,outputlevel=.GetValue(rolValue),startrow=NULL,endrow=NULL,standarditems=.GetValue(rosValue),outputitems=oitems.t,showcolumnnames=.GetValue(rovValue))
  
  AppType <- .GetValue(roaValue)
  print(AppType)
  tkdestroy(tt)
  ee<-EarthChemC(allquery)
  invisible(ee)
}


#############################################################################
#                                                                           #
#                             Main function                                 #
#                                                                           #
#############################################################################
.EarthChemGUI<-function(){
    graphicsOff()
    # Initialize the system if necessary
    if(length(ls(pattern="^EarthChem$"))>0) .EarthChemStart()
    
    tt <<- tktoplevel()
    #tkconfigure(tt,padx=20,pady=20,width=200)
    
    tkwm.title(tt,"Import data from EarthChem Portal")
    left.f <<- ttkframe(tt)
    right.f <<- ttkframe(tt)
    button.f <<- ttkframe(tt)
    dfFont <<- tkfont.create(family=system,size=10)
    #italicFont <- tkfont.create(family=system,slant="italic")

    ################################################################################
    ###                 Frame for data source specification                      ###
    ################################################################################

    f.datasource <- ttklabelframe(left.f,text="Data source information")

    author.t <<- tclVar()
    author.l <- ttklabel(f.datasource,text="Author name  ",font=dfFont)
    author.e <- ttkentry(f.datasource,text=author.t,width=75)
    title.t <<- tclVar()
    title.l <- ttklabel(f.datasource,text="Publication title  ",font=dfFont)
    title.e <- ttkentry(f.datasource,text=title.t,width=75)
    journal.t <<- tclVar()
    journal.l <- ttklabel(f.datasource,text="Journal name  ",font=dfFont)
    journal.e <- ttkentry(f.datasource,text=journal.t,width=75)
    doi.t <<- tclVar()
    doi.l <- ttklabel(f.datasource,text="Publication DOI  ",font=dfFont)
    doi.e <- ttkentry(f.datasource,text=doi.t,width=75)
    keyword.t <<- tclVar()
    keyword.l <- ttklabel(f.datasource,text="Keyword  ",font=dfFont)
    keyword.e <- ttkentry(f.datasource,text=keyword.t,width=75)
    sampleid.t <<- tclVar()
    sampleid.l <- ttklabel(f.datasource,text="Sample ID ",font=dfFont)
    sampleid.e <- ttkentry(f.datasource,text=sampleid.t,width=75)

    pyr <- ttkframe(f.datasource)

    .selDate <- function(qq) {
        if(qq==1) {tkconfigure(exactpubyear.e,state="disabled")
             tkconfigure(minpubyear.e,state="disabled")
             tkconfigure(maxpubyear.e,state="disabled")}
        if(qq==2) {tkconfigure(exactpubyear.e,state="normal")
             tkconfigure(minpubyear.e,state="disabled")
             tkconfigure(maxpubyear.e,state="disabled")}
        if(qq==3) {tkconfigure(exactpubyear.e,state="disabled")
             tkconfigure(minpubyear.e,state="normal")
             tkconfigure(maxpubyear.e,state="normal")}
    }

    ry1 <- ttkradiobutton(pyr)
    ry2 <- ttkradiobutton(pyr)
    ry3 <- ttkradiobutton(pyr)
    ryValue <<- tclVar("Nodate")
    tkconfigure(ry1,variable=ryValue,value="Nodate",command=function() .selDate(1))
    tkconfigure(ry2,variable=ryValue,value="ExactPubYear",command=function() .selDate(2))
    tkconfigure(ry3,variable=ryValue,value="MinMaxPubYear",command=function() .selDate(3))  

    pyr.l <- ttklabel(pyr,text="Year of publication",font=dfFont)
    nopubyear.l <- ttklabel(pyr,text="No specification               ",font=dfFont)
    exactpubyear.t <<- tclVar()
    exactpubyear.l <- ttklabel(pyr,text="Exactly = ",font=dfFont)
    exactpubyear.e <- ttkentry(pyr,text=exactpubyear.t,width=5,state="disabled")
    minpubyear.t <<- tclVar()
    minpubyear.l <- ttklabel(pyr,text="Between ",font=dfFont)
    minpubyear.e <- ttkentry(pyr,text=minpubyear.t,width=5,state="disabled")
    maxpubyear.t <<- tclVar()
    maxpubyear.l <- ttklabel(pyr,text=" and ",font=dfFont)
    maxpubyear.e <- ttkentry(pyr,text=maxpubyear.t,width=5,state="disabled")

    ################################################################################
    ###                    Frame for geographic location                         ###
    ################################################################################

    f.loc <- ttklabelframe(left.f,text="Geographic location")

    otherloc <- ttkframe (f.loc)
    nsew <- ttkframe(f.loc)

    .selLoc <- function(zz) {
        if(zz==1) {tkconfigure(map.but,state="disabled")
           tkconfigure(north.e,state="disabled")
           tkconfigure(south.e,state="disabled")
           tkconfigure(east.e,state="disabled")
           tkconfigure(west.e,state="disabled")
        }
        if(zz==2) {tkconfigure(map.but,state="normal")
           tkconfigure(north.e,state="disabled")
           tkconfigure(south.e,state="disabled")
           tkconfigure(east.e,state="disabled")
           tkconfigure(west.e,state="disabled")
        }
        if(zz==3) {tkconfigure(map.but,state="disabled")
           tkconfigure(north.e,state="normal")
           tkconfigure(south.e,state="normal")
           tkconfigure(east.e,state="normal")
           tkconfigure(west.e,state="normal")
        }
    }

    .OpenMap <- function() {
        poly.coords <<- selectPgonMapC(paste(earthchem.dir,"world_country_admin_boundary_shapefile_with_fips_codes.shp",sep="/"))
        for (i in (1:length(poly.coords$x))) {
            if (i==1) {
                polygon.t <<- paste(poly.coords$x[i],poly.coords$y[i],sep=" ")
            }else{
                cc <- paste(poly.coords$x[i],poly.coords$y[i],sep=" ")
                polygon.t <<- paste(polygon.t,cc,sep=",") 
            }
        }
        initial.coords <- paste(poly.coords$x[1],poly.coords$y[1],sep=" ")
        polygon.t <<- paste(polygon.t,initial.coords,sep=",")
    }

    rl1 <- ttkradiobutton(otherloc) 
    rl2 <- ttkradiobutton(otherloc)
    rl3 <- ttkradiobutton(otherloc)
    rlValue <<- tclVar("Noloc")
    tkconfigure(rl1,variable=rlValue,value="Noloc",command=function() .selLoc(1))
    tkconfigure(rl2,variable=rlValue,value="Locpolygon",command=function() .selLoc(2))
    tkconfigure(rl3,variable=rlValue,value="LocNSEW",command=function() .selLoc(3))

    north.t <<- tclVar()
    south.t <<- tclVar()
    east.t <<- tclVar()
    west.t <<- tclVar()

    noloc.l <- ttklabel(otherloc,text="No geographic specifications",font=dfFont)
    nsewloc.l <- ttklabel(otherloc,text="Define a box from N-S-E-W bounds using coordinates (in digital degrees) :",font=dfFont)
    north.l <- ttklabel(nsew,text="            North ",font=dfFont)
    south.l <- ttklabel(nsew,text="      South ",font=dfFont)
    east.l <- ttklabel(nsew,text="      East ",font=dfFont)
    west.l <- ttklabel(nsew,text="      West ",font=dfFont)
    north.e <- ttkentry(nsew,text=north.t,width=10,state="disabled")
    south.e <- ttkentry(nsew,text=south.t,width=10,state="disabled")
    east.e <- ttkentry(nsew,text=east.t,width=10,state="disabled")
    west.e <- ttkentry(nsew,text=west.t,width=10,state="disabled")

    polyloc.f <- ttkframe(otherloc)
    polyloc.l <- ttklabel(polyloc.f,text="Define a polygon from an interactive map :    ",font=dfFont)
    map.but <- ttkbutton(polyloc.f,text="Open map",command=function() .OpenMap(),width=15,state="disabled")

    ################################################################################
    ###                       Frame for material type                            ###
    ################################################################################

    f.material <- ttklabelframe(right.f,text="Material type")

    rmValue <<- tclVar("not_defined")
    rm1 <- ttkradiobutton(f.material,variable=rmValue,value="not_defined")
    rm2 <- ttkradiobutton(f.material,variable=rmValue,value="bulk")
    rm3 <- ttkradiobutton(f.material,variable=rmValue,value="whole rock")
    rm4 <- ttkradiobutton(f.material,variable=rmValue,value="glass")
    rm5 <- ttkradiobutton(f.material,variable=rmValue,value="inclusion")
    rm6 <- ttkradiobutton(f.material,variable=rmValue,value="mineral")

    ################################################################################
    ###                   Frame for EarthChem's rock class                       ###
    ################################################################################

    f.rock <- ttklabelframe(right.f,text="Rock class (according to EarthChem classification scheme)")

    .SelLevel1 <- function () {
        lev1 <<- names(rock.class)[as.numeric(tkcurselection(listbox1))+1]
        lev2 <<- NULL
        lev3 <<- NULL
        lev4 <<- NULL
        tkconfigure(dslist2,listvariable=tclVar())
        tkconfigure(dslist3,listvariable=tclVar())
        tkconfigure(dslist4,listvariable=tclVar())
  
        if (lev1=="not_defined"){tkconfigure(listbox2,listvariable=tclVar(""))
            tkconfigure(listbox3,listvariable=tclVar(""))
            tkconfigure(listbox4,listvariable=tclVar(""))
            lev1 <<- NULL
        }else{
            if (lev1=="igneous"){
                tkconfigure(listbox2,listvariable=tclVar(names(rock.class$ign)))
            }else{
                tkconfigure(listbox2,selectmode="multiple")
                tkconfigure(listbox3,listvariable=tclVar(""))
                tkconfigure(listbox4,listvariable=tclVar(""))
            }
            if (lev1=="metamorphic"){tkconfigure(listbox2,listvariable=tclVar(rock.class$met))}
            if (lev1=="alteration"){tkconfigure(listbox2,listvariable=tclVar(rock.class$alt))}
            if (lev1=="vein"){tkconfigure(listbox2,listvariable=tclVar(rock.class$vein))}
            if (lev1=="ore"){tkconfigure(listbox2,listvariable=tclVar(rock.class$ore))}
            if (lev1=="sedimentary"){tkconfigure(listbox2,listvariable=tclVar(rock.class$sed))}
            if (lev1=="xenolith"){tkconfigure(listbox2,listvariable=tclVar(rock.class$xen))}
            tkconfigure(dslist1,listvariable=tclVar(lev1))
        }
    }

    .SelLevel2 <- function() {
        lev3 <<- NULL
        lev4 <<- NULL
        tkconfigure(dslist3,listvariable=tclVar())
        tkconfigure(dslist4,listvariable=tclVar())
  
        if (lev1=="igneous"){tkconfigure(listbox2,selectmode="single")
            lev2 <<- names(rock.class$igneous)[as.numeric(tkcurselection(listbox2))+1]
            tkconfigure(listbox3,listvariable=tclVar(names(rock.class$igneous$plutonic)))
            tkconfigure(listbox4,listvariable=tclVar(""))
            tkconfigure(dslist2,listvariable=tclVar(lev2))
        }else{
            if (lev1=="metamorphic"){lev4 <<- rock.class$met[as.numeric(tkcurselection(listbox2))+1]}
            if (lev1=="alteration"){lev4 <<- rock.class$alt[as.numeric(tkcurselection(listbox2))+1]}        
            if (lev1=="vein"){lev4 <<- rock.class$vein[as.numeric(tkcurselection(listbox2))+1]}
            if (lev1=="ore"){lev4 <<- rock.class$ore[as.numeric(tkcurselection(listbox2))+1]}
            if (lev1=="sedimentary"){lev4 <<- rock.class$sed[as.numeric(tkcurselection(listbox2))+1]}
            if (lev1=="xenolith"){lev4 <<- rock.class$xen[as.numeric(tkcurselection(listbox2))+1]}
            tkconfigure(dslist2,listvariable=tclVar(lev4))           
        }
    }

    .SelLevel3 <- function() {
        lev3 <<- names(rock.class$ign$plut)[as.numeric(tkcurselection(listbox3))+1]
        lev4 <<- NULL
        tkconfigure(dslist3,listvariable=tclVar(lev3))
        tkconfigure(dslist4,listvariable=tclVar())
  
        if (lev2=="plutonic"){
            if (lev3=="ultramafic") {tkconfigure(listbox4,listvariable=tclVar(rock.class$igneous$plutonic$ultramaf))}
            if (lev3=="mafic") {tkconfigure(listbox4,listvariable=tclVar(rock.class$igneous$plutonic$maf))}
            if (lev3=="intermediate") {tkconfigure(listbox4,listvariable=tclVar(rock.class$igneous$plutonic$inter))}
            if (lev3=="felsic") {tkconfigure(listbox4,listvariable=tclVar(rock.class$igneous$plutonic$felsic))}
            if (lev3=="exotic") {tkconfigure(listbox4,listvariable=tclVar(rock.class$igneous$plutonic$exo))}
        }
        if (lev2=="volcanic"){
            if (lev3=="ultramafic") {tkconfigure(listbox4,listvariable=tclVar(rock.class$igneous$volca$ultramaf))}
            if (lev3=="mafic") {tkconfigure(listbox4,listvariable=tclVar(rock.class$igneous$volca$maf))}
            if (lev3=="intermediate") {tkconfigure(listbox4,listvariable=tclVar(rock.class$igneous$volca$inter))}
            if (lev3=="felsic") {tkconfigure(listbox4,listvariable=tclVar(rock.class$igneous$volca$felsic))}
            if (lev3=="exotic") {tkconfigure(listbox4,listvariable=tclVar(rock.class$igneous$volca$exo))}
        }
    }

    .SelLevel4 <- function() {
        if (lev2=="plutonic"){
            if (lev3=="ultramafic") {lev4 <<- rock.class$igneous$plut$ultramaf[as.numeric(tkcurselection(listbox4))+1]}
            if (lev3=="mafic") {lev4 <<- rock.class$igneous$plut$maf[as.numeric(tkcurselection(listbox4))+1]}
            if (lev3=="intermediate") {lev4 <<- rock.class$igneous$plut$intermed[as.numeric(tkcurselection(listbox4))+1]}
            if (lev3=="felsic") {lev4 <<- rock.class$igneous$plut$fels[as.numeric(tkcurselection(listbox4))+1]}
            if (lev3=="exotic") {lev4 <<- rock.class$igneous$plut$exo[as.numeric(tkcurselection(listbox4))+1]}
        }
        if (lev2=="volcanic"){
            if (lev3=="ultramafic") {lev4 <<- rock.class$igneous$volc$ultramaf[as.numeric(tkcurselection(listbox4))+1]}
            if (lev3=="mafic") {lev4 <<- rock.class$igneous$volc$maf[as.numeric(tkcurselection(listbox4))+1]}
            if (lev3=="intermediate") {lev4 <<- rock.class$igneous$volc$intermed[as.numeric(tkcurselection(listbox4))+1]}
            if (lev3=="felsic") {lev4 <<- rock.class$igneous$volc$fels[as.numeric(tkcurselection(listbox4))+1]}
            if (lev3=="exotic") {lev4 <<- rock.class$igneous$volc$exo[as.numeric(tkcurselection(listbox4))+1]}
        }
        tkconfigure(dslist4,listvariable=tclVar(lev4))
    }

    lev1 <<- NULL
    lev2 <<- NULL
    lev3 <<- NULL
    lev4 <<- NULL

    scroll.listbox1 <- tkscrollbar(f.rock, repeatinterval=5,command=function(...)tkyview(listbox1,...))
    listbox1 <- tklistbox(f.rock,height=5,listvariable=tclVar(names(rock.class)), selectmode="single",yscrollcommand=function(...)tkset(scroll.listbox1,...),background="white")
    scroll.listbox2 <- tkscrollbar(f.rock, repeatinterval=5,command=function(...)tkyview(listbox2,...))
    listbox2 <- tklistbox(f.rock,height=5,selectmode="multiple",yscrollcommand=function(...)tkset(scroll.listbox2,...),background="white")
    scroll.listbox3 <- tkscrollbar(f.rock, repeatinterval=5,command=function(...)tkyview(listbox3,...))
    listbox3 <- tklistbox(f.rock,height=5,selectmode="single",yscrollcommand=function(...)tkset(scroll.listbox3,...),background="white")
    scroll.listbox4 <- tkscrollbar(f.rock, repeatinterval=5,command=function(...)tkyview(listbox4,...))
    listbox4 <- tklistbox(f.rock,height=5,selectmode="multiple",yscrollcommand=function(...)tkset(scroll.listbox4,...),background="white")

    tkselection.set(listbox1,0)

    dispsel.l <- ttklabel(f.rock,text="Selected rock types :",font=tkfont.create(family=system,slant="italic",size=9))
    dslist1 <- tklistbox(f.rock,height=3,listvariable=tclVar(),state="disabled",background="light grey")
    scroll.dslist2 <- tkscrollbar(f.rock, repeatinterval=3,command=function(...)tkyview(dslist2,...))
    dslist2 <- tklistbox(f.rock,height=3,listvariable=tclVar(),state="disabled",background="light grey")
    dslist3 <- tklistbox(f.rock,height=3,listvariable=tclVar(),state="disabled",background="light grey")
    scroll.dslist4 <- tkscrollbar(f.rock, repeatinterval=3,command=function(...)tkyview(dslist4,...))
    dslist4 <- tklistbox(f.rock,height=3,listvariable=tclVar(),state="disabled",background="light grey")

    tkbind(listbox1,"<<ListboxSelect>>",function() .SelLevel1())
    tkbind(listbox2,"<<ListboxSelect>>",function() .SelLevel2())
    tkbind(listbox3,"<<ListboxSelect>>",function() .SelLevel3())
    tkbind(listbox4,"<<ListboxSelect>>",function() .SelLevel4())

    ################################################################################
    ###                          Frame for sample age                            ###
    ################################################################################

    f.age <- ttklabelframe(left.f,text="Geological age")

    otherage <- ttkframe(f.age)
    slids <- ttkframe(f.age)

    .selAge <- function(yy) {
        if(yy==1) {tkconfigure(minS,state="disabled")
           tkconfigure(MaxS,state="disabled")
           tkconfigure(geolage.cbbx,state="disabled")
           tkconfigure(exage.e,state="disabled")}
        if(yy==2) {tkconfigure(minS,state="normal")
           tkconfigure(MaxS,state="normal")
           tkconfigure(geolage.cbbx,state="disabled")
           tkconfigure(exage.e,state="disabled")}
        if(yy==3) {tkconfigure(minS,state="disabled")
           tkconfigure(MaxS,state="disabled")
           tkconfigure(geolage.cbbx,state="disabled")
           tkconfigure(exage.e,state="normal")}
        if(yy==4) {tkconfigure(minS,state="disabled")
           tkconfigure(MaxS,state="disabled")
           tkconfigure(geolage.cbbx,state="readonly")
           tkconfigure(exage.e,state="disabled")}           
    }

    ra1 <- ttkradiobutton(otherage)
    ra2 <- ttkradiobutton(otherage)
    ra3 <- ttkradiobutton(otherage)
    ra4 <- ttkradiobutton(otherage)
    raValue <<- tclVar("Noage")
    tkconfigure(ra1,variable=raValue,value="Noage",command=function() .selAge(1))
    tkconfigure(ra2,variable=raValue,value="MinMaxAge",command=function() .selAge(2))
    tkconfigure(ra3,variable=raValue,value="ExactAge",command=function() .selAge(3))
    tkconfigure(ra4,variable=raValue,value="GeolAge",command=function() .selAge(4))
    ra1.l <- ttklabel(otherage,text="No age specification",font=dfFont)
    ra2.l <- ttklabel(otherage,text="Define a range of ages :",font=dfFont)

    changelab <- function(z){
        if (z==1) {
            tkconfigure(minS,label=paste("Minimum age = ",as.numeric(tclvalue(minage.t))," Ma",sep=""))
        }else{
            tkconfigure(MaxS,label=paste("Maximum age = ",as.numeric(tclvalue(maxage.t))," Ma",sep=""))
        }
    }

    minage.t <<- tclVar(0)
    maxage.t <<- tclVar(4560)
    minS <- tkscale(slids,from=0,to=4560,orient="horizontal",resolution=10,length=370,variable=minage.t,label=paste("Minimum age = ",as.numeric(tclvalue(minage.t))," Ma",sep=""),showvalue=FALSE,state="disabled")
    MaxS <- tkscale(slids,from=0,to=4560,orient="horizontal",resolution=10,length=370,variable=maxage.t,label=paste("Maximum age = ",as.numeric(tclvalue(maxage.t))," Ma",sep=""),showvalue=FALSE,state="disabled")
    tkbind(minS,"<Motion>",function() changelab(1))
    tkbind(MaxS,"<Motion>",function() changelab(2))

    exage.f <- ttkframe(otherage)
    exage.t <<- tclVar()
    exage.l <- ttklabel(exage.f,text="Define an exact age (in Ma) : ",font=dfFont)
    exage.e <- ttkentry(exage.f,text=exage.t,width=5,state="disabled")

    geolage.f <- ttkframe(otherage)
    geolage.t <<- tclVar()
    geolage.l <- ttklabel(geolage.f,text="Select a geological age : ",font=dfFont)
    geolage.cbbx <- ttkcombobox(f.age,values=agelist,textvariable=geolage.t,width=max(nchar(agelist)),state="disabled")

    ################################################################################
    ###                         Frame for data output                            ###
    ################################################################################

    .SelItems <- function(uu) {
        if (uu==1) tkconfigure(listbox5,listvariable=tclVar())
        if (uu==2) tkconfigure(listbox5,listvariable=tclVar(itemlist))
        if (uu==3) oitems <<- itemlist[as.numeric(tkcurselection(listbox5))+1]
    }

    f.output <- ttklabelframe(right.f,text="Data output setup")
    #outputappend.l1 <- ttklabel(f.output,text="How to deal with the imported data ?     ",font=dfFont)
    #outputappend.l2 <- ttklabel(f.output,text="Append to current file     ",font=dfFont)
    #outputappend.l3 <- ttklabel(f.output,text="Create a new file",font=dfFont)
    #outputtype.l1 <- ttklabel(f.output,text="Format of data output ?",font=dfFont)
    #outputtype.l2 <- ttklabel(f.output,text="HTML",font=dfFont)
    #outputtype.l3 <- ttklabel(f.output,text="XML",font=dfFont)                     
    #outputlevel.l1 <- ttklabel(f.output,text="Data output sorting level ?",font=dfFont)
    #outputlevel.l2 <- ttklabel(f.output,text="Sample",font=dfFont)
    #outputlevel.l3 <- ttklabel(f.output,text="Method",font=dfFont)
    #outputcolnames.l1 <- ttklabel(f.output,text="Import the names of the variables ?",font=dfFont)
    #outputcolnames.l2 <- ttklabel(f.output,text="Yes",font=dfFont)
    #outputcolnames.l3 <- ttklabel(f.output,text="No",font=dfFont)
    outputstd.l1 <- ttklabel(f.output,text="Output standard items only?",font=dfFont)
    outputstd.l2 <- ttklabel(f.output,text="Yes",font=dfFont)
    outputstd.l3 <- ttklabel(f.output,text="No",font=dfFont)

    f.items <- ttkframe(f.output) 
    oitems <<- NULL
    outputitems.l <- ttklabel(f.items,text="Select additional items to output :",font=dfFont)
    scroll.listbox5 <- tkscrollbar(f.items, repeatinterval=5,command=function(...)tkyview(listbox5,...))
    listbox5 <- tklistbox(f.items,height=5,selectmode="multiple",yscrollcommand=function(...)tkset(scroll.listbox5,...),background="white")
    tkbind(listbox5,"<<ListboxSelect>>",function() .SelItems(3)) 

    roaValue <<- tclVar("WRappend")
    #roa1 <- ttkradiobutton(f.output,variable=roaValue,value="WRappend")
    #roa2 <- ttkradiobutton(f.output,variable=roaValue,value="new_file")
    rotValue <<- tclVar("html")
    #rot1 <- ttkradiobutton(f.output,variable=rotValue,value="html")
    #rot2 <- ttkradiobutton(f.output,variable=rotValue,value="xml")
    rolValue <<- tclVar("sample")
    #rol1 <- ttkradiobutton(f.output,variable=rolValue,value="sample")
    #rol2 <- ttkradiobutton(f.output,variable=rolValue,value="method")
    rovValue <<- tclVar("yes")
    #rov1 <- ttkradiobutton(f.output,variable=rovValue,value="yes")
    #rov2 <- ttkradiobutton(f.output,variable=rovValue,value="no")
    rosValue <<- tclVar("yes")
    ros1 <- ttkradiobutton(f.output,variable=rosValue,value="yes")
    ros2 <- ttkradiobutton(f.output,variable=rosValue,value="no") 
    tkconfigure(ros1,command=function () .SelItems(1))
    tkconfigure(ros2,command=function () .SelItems(2))

    ################################################################################
    ###                          Submit query button                             ###
    ################################################################################

    OK.but <- tkbutton(button.f,text="Submit query!",command=.ECInputList,width=30,fg="darkred")

    ################################################################################
    ###                          Geometry management                             ###
    ################################################################################

    tkgrid(left.f,ttklabel(tt,text="   "),right.f)
    tkgrid.configure(left.f,right.f,sticky="n")
    tkgrid(ttklabel(tt,text=""))
    tkgrid(button.f,columnspan=3)
    tkgrid(ttklabel(tt,text=""))

    tkpack(f.datasource,fill="x",anchor="n")
    tkpack(ttklabel(left.f,text=""),anchor="n")
    tkpack(f.loc,fill="x",anchor="n")
    tkpack(ttklabel(left.f,text=""),anchor="n")
    tkpack(f.age,fill="x",anchor="n")
    tkpack(f.material,fill="x",anchor="n")
    tkpack(ttklabel(right.f,text=""),anchor="n")
    tkpack(f.rock,fill="x",anchor="n")
    tkpack(ttklabel(right.f,text=""),anchor="n")
    tkpack(f.output,fill="x",anchor="n",expand=TRUE)

    tkgrid(author.l,author.e)
    tkgrid(title.l,title.e)
    tkgrid(journal.l,journal.e)
    tkgrid(doi.l,doi.e)
    tkgrid(keyword.l,keyword.e)
    tkgrid(sampleid.l,sampleid.e)
    tkgrid.configure(author.e,title.e,journal.e,doi.e,keyword.e,sampleid.e,sticky="w")
    tkgrid.configure(author.l,title.l,journal.l,doi.l,keyword.l,sampleid.l,sticky="e")

    tkgrid(pyr,columnspan=2,sticky="ew")
    tkgrid(pyr.l,columnspan=11)
    tkgrid.configure(pyr.l,sticky="w")
    tkgrid(ry1,nopubyear.l,ry2,exactpubyear.l,exactpubyear.e,ttklabel(pyr,text="               "),ry3,minpubyear.l,minpubyear.e,maxpubyear.l,maxpubyear.e)
    tkgrid.configure(nopubyear.l,exactpubyear.l,minpubyear.l,maxpubyear.l,sticky="w")

    tkgrid(otherloc)
    tkgrid.configure(otherloc,sticky="w")
    tkgrid(rl1,noloc.l)
    tkgrid.configure(noloc.l,sticky="w")
    tkgrid(rl2,polyloc.f)
    tkgrid.configure(polyloc.f,sticky="w")
    if(.Platform$OS.type=="windows"&.Platform$GUI=="Rgui"){
        tkgrid(polyloc.l,map.but)
        tkgrid.configure(polyloc.l,sticky="w")
    }
    tkgrid(rl3,nsewloc.l)
    tkgrid.configure(nsewloc.l,sticky="w")

    tkgrid(nsew,columnspan=2)
    tkpack(north.l,north.e,south.l,south.e,east.l,east.e,west.l,west.e,side="left")

    tkgrid(otherage,sticky="w")
    tkgrid(ra1,ra1.l)
    tkgrid.configure(ra1.l,sticky="w")
    tkgrid(ra4,geolage.f)
    tkgrid.configure(geolage.f,sticky="w")
    tkgrid(geolage.l,geolage.cbbx)
    tkgrid(ra3,exage.f)
    tkgrid.configure(exage.f,sticky="w")
    tkgrid(exage.l,exage.e)
    tkgrid.configure(exage.e,sticky="w")
    tkgrid(ra2,ra2.l)
    tkgrid.configure(ra2.l,sticky="w")
    tkgrid(slids,columnspan=2)
    tkgrid(minS,MaxS)

    tkpack(rm1,tklabel(f.material,text="Not specified       ",font=dfFont),rm2,tklabel(f.material,text="Bulk       ",font=dfFont),rm3,tklabel(f.material,text="Whole rock       ",font=dfFont),rm4,tklabel(f.material,text="Glass       ",font=dfFont),rm5,tklabel(f.material,text="Inclusion       ",font=dfFont),rm6,tklabel(f.material,text="Mineral",font=dfFont),side="left",anchor="w")

    tkgrid(listbox1,scroll.listbox1,listbox2,scroll.listbox2,listbox3,scroll.listbox3,listbox4,scroll.listbox4)
    tkgrid.configure(scroll.listbox1,rowspan=5,sticky="nsw")
    tkgrid.configure(scroll.listbox2,rowspan=5,sticky="nsw")
    tkgrid.configure(scroll.listbox3,rowspan=5,sticky="nsw")
    tkgrid.configure(scroll.listbox4,rowspan=5,sticky="nsw")
    tkgrid(dispsel.l,columnspan=8)
    tkgrid.configure(dispsel.l,sticky="w")
    tkgrid(dslist1,ttklabel(f.rock,text=""),dslist2,scroll.dslist2,dslist3,ttklabel(f.rock,text=""),dslist4,scroll.dslist4)
    tkgrid.configure(scroll.dslist2,scroll.dslist4,rowspan=3,sticky="nsw")

    #tkgrid(outputappend.l1,roa1,outputappend.l2,roa2,outputappend.l3)
    #tkgrid(outputtype.l1,rot1,outputtype.l2,rot2,outputtype.l3)
    #tkgrid(outputlevel.l1,rol1,outputlevel.l2,rol2,outputlevel.l3)
    #tkgrid(outputcolnames.l1,rov1,outputcolnames.l2,rov2,outputcolnames.l3)
    
    tkgrid(outputstd.l1,ros1,outputstd.l2,ros2,outputstd.l3)
    tkgrid(f.items,columnspan=5,sticky="ew")
    
    tkgrid.configure(outputstd.l1,outputstd.l2,outputstd.l3,sticky="w")
    #tkgrid.configure(outputappend.l1,outputappend.l2,outputappend.l3,outputtype.l1,outputtype.l2,outputtype.l3,outputlevel.l1,outputlevel.l2,outputlevel.l3,outputcolnames.l1,outputcolnames.l2,outputcolnames.l3,outputstd.l1,outputstd.l2,outputstd.l3,sticky="w")
    tkgrid(outputitems.l,listbox5,scroll.listbox5)
    tkgrid.configure(outputitems.l,sticky="w")
    tkgrid.configure(listbox5,sticky="nw")
    tkgrid.configure(scroll.listbox5,rowspan=5,sticky="nsw")

    tkgrid(OK.but)
    #tkfocus(tt)
    tkwait.window(tt)
    invisible()
}
