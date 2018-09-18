LaYb<-function(ybrep=FALSE){
    if (ybrep) {    # This block of code proposes to use Y/2.4 instead of Yb.
                # It's unlikely to have any practical use, since data missing Yb values will probably also miss La
                # So the diagram cannot be plotted anyway
                # This is therefore hidden behing ybrep=FALSE, and the replacement code will never be executed
                # unless LaYb() is called with ybrep=TRUE
                # Which the GUI will not do, by default.
                # The code is left here is somebody wants to try it
        ybreplace<-winDialog("yesno","Replace missing Yb values by Y/2.4 ?")
        if (ybreplace=="YES"){
            isyb<-!is.na(WR[,"Yb"])
            isy<-!is.na(WR[,"Y"])
            ee<-WR[,c("Y","Yb")]
            ee[is.na(ee)]<-0
            Yb<-isyb*ee[,"Yb"]+ (isy&!isyb)*ee[,"Y"]/2.4
            Yb[Yb==0]<-NA
            xtitle<-annotate("Yb[N] or (Y/2.4)[N]")
            ytitle<- annotate("(La/Yb or Y/2.4)[N]")
        }else{
            Yb<-WR[,"Yb"]
            xtitle<-annotate("Yb[N]")
            ytitle<-annotate(("(La/Yb)[N]"))
        }
    }else{   # Do not check Yb replacement
        Yb<-WR[,"Yb"]
        xtitle<-annotate("Yb[N]")
        ytitle<-annotate(("(La/Yb)[N]"))
    }

    # Normalize by chondrite (Nakamura 1974)
    x.data<<-Yb/0.22
    y.data<<-(WR[,"La"]/0.33)/(Yb/0.22)

    temp1<-list(
        clssf=list("NULL",use=2:3,rcname=c("TTG/adakite","ordinary")),
        lines1=list("lines",x=c(0.28,1.55,2.55,5.3,8.6,8.6,3,1,0.28,0.28),y=c(150,150,70,40,25,5,5,15,35,150),col=plt.col[2]),
        lines2=list("lines",x=c(4.5,4.5,20,20,4.5),y=c(2,30,10,2,2),col=plt.col[2]),
        GCDkit=list("NULL",plot.type="binary",plot.position=109,plot.name="YbN vs LaN/YbN (Martin 1986) TTG/adakite")
        )
    temp2<-list(
        text1=list("text",x=2.5,y=100,text="TTG/adakite",col=plt.col[2],adj=0),
        text2=list("text",x=14,y=22,text="ordinary",col=plt.col[2],adj=0)
    )
    if(getOption("gcd.plot.text")){
        temp<-c(temp1,temp2)
    }else{
        temp<-temp1
    }

    sheet<<-list(
           demo=list(
                  fun="plot", 
                  call=list(
                          xlim=c(0,24),
                          ylim=c(0,175),
                          main=annotate("Y[N] vs. La[N]/Yb[N] (Martin 1986)"),
                          col="green",
                          bg="transparent",
                          fg="black",
                          xlab=xtitle,
                          ylab=ytitle,
                          main=""
                          ),
                  template=temp
                  )
           )
}
