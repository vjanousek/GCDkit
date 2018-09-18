#####################################################################
#                                                                   #
#                                                                   #
#####################################################################

# ab, an, or can be text strigs = names of existing columns in WR 
OConnorPlut<-function(ab=NULL,an=NULL,or=NULL){
    on.exit(options("show.error.messages"=TRUE))
    options("show.error.messages"=FALSE)
    
    template<-temp<-list()
    if (!just.sheets){
        if(is.null(ab)|is.null(an)|is.null(or)){ #added
            z<-winDialog(type = "yesno", "Calculate CIPW norm now?")
            if (z=="YES"){
                x<-CIPW(WR)
                #aaa<-"Ab"
                #bbb<-"An"
                #ccc<-"Or"
                aa<-x[,"Ab"]
                bb<-x[,"An"]
                cc<-x[,"Or"]
                x<-rownames(x)
            }else{
                
                ab <- selectColumnLabel(colnames(WR), message = "Select Albite source column\nor press ENTER to pick from a list", default = "", silent = TRUE, empty.ok = FALSE)
                aa<-WR[,ab]
                an <- selectColumnLabel(colnames(WR), message = "Select Anorthite source column\nor press ENTER to pick from a list", default = "", silent = TRUE, empty.ok = FALSE)
                bb<-WR[,bbb]
                or <- selectColumnLabel(colnames(WR), message = "Select K-feldspar source column\nor press ENTER to pick from a list", default = "", silent = TRUE, empty.ok = FALSE)
                cc<-WR[,ccc]
                x<-rownames(WR)
            }
        }else{
            aa<-WR[,ab]
            bb<-WR[,an]
            cc<-WR[,or]
            x<-rownames(WR)
            z<-"FROM COL"
        }

        suma<-(aa+bb+cc)/100
        aa<-aa/suma
        bb<-bb/suma
        cc<-cc/suma

        x.data<<-cc+bb/2
        y.data<<-sqrt(3)*bb/2
       
        results<<-cbind(aa,bb,cc)
        colnames(results)<<-c("Ab","An","Or")
        rownames(results)<<-x

        if (z=="YES"){
        template<-list(
            clssf=list("NULL",use=2:6,rcname=c("trondhjemite","granite","quartz monzonite","granodiorite","tonalite")),
            polygon1=list("NULL",x=c(0/2+0,0/2+30,17.5/2+30,25/2+0),y=sqrt(3)*c(0/2,0/2,17.5/2,25/2),lwd=3,col=plt.col[2]),
            polygon2=list("NULL",x=c(0/2+30,0/2+100,17.5/2+30),y=sqrt(3)/2*c(0,0,17.5),lwd=3,col=plt.col[2]),
            polygon3=list("NULL",x=c(16.25/2+35,12.5/2+50,27.5/2+50,35.75/2+35),y=sqrt(3)/2*c(16.25,12.5,27.5,35.75),lwd=3,col=plt.col[2]),
            polygon4=list("NULL",x=c(20/2+20,16.25/2+35,35.75/2+35,44/2+20),y=sqrt(3)/2*c(20,16.25,35.75,44),lwd=3,col=plt.col[2]),
            polygon5=list("NULL",x=c(25/2+0,20/2+20,44/2+20,55/2+0),y=sqrt(3)/2*c(25,20,44,55),lwd=3,col=plt.col[2]),
        
            lines1=list("lines",x=c(0/2+30,17.5/2+30),y=sqrt(3)/2*c(0,17.5),col=plt.col[2]),
            lines2=list("lines",x=c(20/2+20,44/2+20),y=sqrt(3)/2*c(20,44),col=plt.col[2]),
            lines3=list("lines",x=c(16.25/2+35,35.75/2+35),y=sqrt(3)/2*c(16.25,35.75),col=plt.col[2],lty="dashed"),
            lines4=list("lines",x=c(12.5/2+50,27.5/2+50),y=sqrt(3)/2*c(12.5,27.5),col=plt.col[2],lty="dashed"),
            lines5=list("lines",x=c(25/2+0,12.5/2+50),y=sqrt(3)/2*c(25,12.5),col=plt.col[2]),
            lines6=list("lines",x=c(12.5/2+50,2.5/2+90),y=sqrt(3)/2*c(12.5,2.5),col=plt.col[2],lty="dashed"),
            lines7=list("lines",x=c(2.5/2+90,5.5/2+90),y=sqrt(3)/2*c(2.5,5.5),col=plt.col[2],lty="dashed"),                                       
        
            lines8=list("lines",x=c(0,100,50,0),y=c(0,0,sqrt(3)*100/2,0),col="black")
        )
        }else{
        template<-list(
            clssf=list("NULL",use=2:6,rcname=c("Trondhjemite","Granite","Quartz monzonite","Granodiorite","Tonalite")),
            polygon1=list("NULL",x=c(0/2+0,0/2+20,20/2+20,25/2+0),y=sqrt(3)*c(0/2,0/2,20/2,25/2),lwd=3,col=plt.col[2]),
            polygon2=list("NULL",x=c(0/2+20,0/2+100,20/2+20),y=sqrt(3)*c(0/2,0/2,20/2),lwd=3,col=plt.col[2]),
            polygon3=list("NULL",x=c(16.25/2+35,10/2+60,20/2+60,32.5/2+35),y=sqrt(3)*c(16.25/2,10/2,20/2,32.5/2),lwd=3,col=plt.col[2]),
            polygon4=list("NULL",x=c(22.5/2+10,16.25/2+35,32.5/2+35,45/2+10),y=sqrt(3)*c(22.5/2,16.25/2,32.5/2,45/2),lwd=3,col=plt.col[2]),
            polygon5=list("NULL",x=c(22.5/2+10,45/2+10,50/2+0,25/2+0),y=sqrt(3)*c(22.5/2,45/2,50/2,25/2),lwd=3,col=plt.col[2]),
            
            lines1=list("lines",x=c(0/2+20,20/2+20),y=c(sqrt(3)*0/2,sqrt(3)*20/2),col=plt.col[2]),
            lines2=list("lines",x=c(25/2+0,10/2+60),y=c(sqrt(3)*25/2,sqrt(3)*10/2),col=plt.col[2]),
            lines3=list("lines",x=c(10/2+60,0/2+100),y=c(sqrt(3)*10/2,sqrt(3)*0/2),col=plt.col[2],lty="dashed"),
            lines4=list("lines",x=c(10/2+60,20/2+60),y=c(sqrt(3)*10/2,sqrt(3)*20/2),col=plt.col[2]),
            lines5=list("lines",x=c(16.25/2+35,32.5/2+35),y=c(sqrt(3)*16.25/2,sqrt(3)*32.5/2),col=plt.col[2]),
            lines6=list("lines",x=c(22.5/2+10,45/2+10),y=c(sqrt(3)*22.5/2,sqrt(3)*45/2),col=plt.col[2]),

            lines7=list("lines",x=c(0,100,50,0),y=c(0,0,sqrt(3)*100/2,0),col="black")        
        )
        }
    }
    temp<-list(        
        text1=list("text",x=15,y=sqrt(3)*10/2,text="Trondhjemite",col=plt.col[2],srt=30),
        text2=list("text",x=40+7/2,y=sqrt(3)*7/2,text="Granite",col=plt.col[2]),
        text3=list("text",x=42+25/2,y=sqrt(3)*25/2,text="Quartz\nmonzonite",col=plt.col[2],srt=60),
        text4=list("text",x=27+30/2,y=sqrt(3)*30/2,text="Grano-\ndiorite",col=plt.col[2]),
        text5=list("text",x=5+40/2,y=sqrt(3)*40/2,text="Tonalite",col=plt.col[2],srt=60)
    )
        
    common<-list(
        A=list("text",x=0,y=-5,text="Ab",adj=0.5),
        C=list("text",x=100,y=-5,text="Or",adj=0.5),
        B=list("text",x=50,y=sqrt(3)*100/2+3,text="An",adj=0.5),
        GCDkit=list("NULL",plot.type="ternary",plot.position=39,plot.name="Feldspar triangle (O'Connor 1965) ")
    )
    template<-c(template,common)        
    if(getOption("gcd.plot.text"))template<-c(template,temp)

    sheet<<-list(demo=list(fun="plot",call=list(xlim=c(-3,103),main=annotate(template$GCDkit$plot.name),ylim=c(-10,103),xlab="",ylab="",bg="transparent",fg="black",asp=1,axes=FALSE),template=template))
}
