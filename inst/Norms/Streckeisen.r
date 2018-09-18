#####################################################################
#             Streckeisen and Le Maitre (1979) Q'-ANOR              #
#                                                                   #
#####################################################################

Streckeisen<-function(x,new=TRUE){
    on.exit(options(show.error.messages = TRUE))
    options(show.error.messages = FALSE)

    result<-matrix(nrow=nrow(x),ncol=2)
    milli[is.na(milli)]<-0
    for (f in seq(1,nrow(x))){
        Q<-100*x[f,"Quartz"]/(x[f,"Quartz"]+x[f,"Orthoclase"]+x[f,"Albite"]+x[f,"Anorthite"])
        ANOR<-100*x[f,"Anorthite"]/(x[f,"Orthoclase"]+x[f,"Anorthite"])
        result[f, 1]<-ANOR
        result[f, 2]<-Q
    }

    rownames(result)<-rownames(x)
    colnames(result)<-c("ANOR","Q'")

#x.data<<-result[,1]
#y.data<<-result[,2]

    assign("x.data", result[,1], .GlobalEnv)
    assign("y.data", result[,2], .GlobalEnv)
    
    temp1<-list(
        lines1=list("abline",h=5,col=plt.col[2]),
        lines2=list("abline",h=20,col=plt.col[2]),
        lines3=list("lines",x=c(70,50),y=c(0,50),col=plt.col[2]),
        lines4=list("lines",x=c(50,40,25),y=c(0,20,50),col=plt.col[2]),
        lines5=list("lines",x=c(50,40,25),y=c(0,20,50),col=plt.col[2]),
        lines6=list("lines",x=c(17.5,10,5),y=c(0,30,50),col=plt.col[2]),
        lines7=list("lines",x=c(32,30,8),y=c(0,5,50),col=plt.col[2]),
        lines8=list("lines",x=c(88,72.5),y=c(0,50),col=plt.col[2],lty="dashed"),
        GCDkit=list("NULL",plot.type="binary",plot.name="Q'-ANOR plot (Streckeisen and Le Maitre 1979)")
        )
    temp2<-list(
        text1=list("text",x=5,y=25,text="(2)",col=plt.col[3]),
        text2=list("text",x=15,y=25,text="(3a)",col=plt.col[3]),
        text3=list("text",x=28,y=25,text="(3b)",col=plt.col[3]),
        text4=list("text",x=50,y=25,text="(4)",col=plt.col[3]),
        text5=list("text",x=70,y=25,text="(5a)",col=plt.col[3]),
        text6=list("text",x=90,y=25,text="(5b)",col=plt.col[3]),

        text7=list("text",x=7,y=13,text="(6*)",col=plt.col[3]),
        text8=list("text",x=20,y=13,text="(7*)",col=plt.col[3]),
        text9=list("text",x=35,y=13,text="(8*)",col=plt.col[3]),
        text10=list("text",x=55,y=13,text="(9*)",col=plt.col[3]),
        text11=list("text",x=75,y=13,text="(10a*)",col=plt.col[3]),
        text12=list("text",x=92,y=13,text="(10b*)",col=plt.col[3]),

        text13=list("text",x=9,y=2.5,text="(6)",col=plt.col[3]),
        text14=list("text",x=25,y=2.5,text="(7)",col=plt.col[3]),
        text15=list("text",x=40,y=2.5,text="(8)",col=plt.col[3]),
        text16=list("text",x=58,y=2.5,text="(9)",col=plt.col[3]),
        text17=list("text",x=78,y=2.5,text="(10a)",col=plt.col[3]),
        text18=list("text",x=95,y=2.5,text="(10b)",col=plt.col[3])
        )

    if(getOption("gcd.plot.text")){
        temp<-c(temp1,temp2)
    }else{
        temp<-temp1
    }

    sheet<<-list(demo=list(fun="plot", call=list(xlim=c(0,100),ylim=c(0,50),col="green",bg="white",fg="black",xlab="ANOR",ylab="Q'",xaxs="r",yaxs="r"),template=temp))
    pp<-figaro(demo,prefix="sheet")
    sheet$demo$call$new <<- new
    pp$draw(x.data,y.data,col=labels$Colour,pch=labels$Symbol,xlab="ANOR",ylab="Q'",main="Q'-ANOR plot (Streckeisen and Le Maitre 1979)",cex=labels$Size,new=new)
    assign("pp", pp, .GlobalEnv)
    if(getOption("gcd.ident"))ee<-ID(x.data,y.data)
    if (!new){
        options(show.error.messages = FALSE)
        ee <- screen()
        .saveCurPlotDef(ee)
        scr.old <<- ee
        if (ee < length(plate.data)) 
            screen(ee + 1, new = FALSE)
    }else{
        figaroOn()
    }
    invisible()
}
