#####################################################################
#       Adjust Fe2O3/FeO ratio according to TAS classification      #
#                based originally on Middlemost (1989)              #
#         modified by Verma et al. (2002), SINCLAS - Fig. 1b        #
#####################################################################

FeMiddlemost<-function(anhydrous=TRUE){
    on.exit(options("show.error.messages"=TRUE))
    # Classify by TAS
    groups.bak<-groups
    TAS.bak<-TAS
    diagram<-paste(gcdx.dir,"Diagrams/Classification/English/TAS.r",sep="/")
    source(diagram)
    xx<-eval(call("TAS"))
        if (xx == "error") {
            options(show.error.messages = FALSE)
            stop("", call. = FALSE)
    }
    tas<-classify("TAS",source.sheet = TRUE,silent=TRUE)
    xx<-eval(do.call("TAS.bak",list()))
    TAS<<-TAS.bak
    groups<<-groups.bak
    
    # Define the Fe2O3/FeO ratios
    tas.names<- c("foidite","picrobasalt","basalt","basaltic andesite","andesite","dacite",
                "rhyolite","trachybasalt","basaltic trachyandesite","trachyandesite","trachyte/trachydacite",
                "tephrite/basanite","phonotephrite","tephriphonolite","phonolite")
    f<-c(NA,0.15,0.20,0.30,0.35,0.40,
        0.50,0.30,0.35,0.40,0.50,
        NA,0.35,0.40,0.50)
    names(f)<-tas.names
    i<-match(results,tas.names)
    alk<-WRanh[,"Na2O"]+WRanh[,"K2O"]
    
    iron<-data.frame(results)
    iron[,2]<-alk
    iron[,3]<-f[i]
    colnames(iron)<-c("TAS name","Total alkalis","Fe2O3/FeO")
    GCDkit=list("NULL",plot.type="binary",plot.position=31,plot.name="Adjust Fe2O3/FeO ratio according to TAS classification based originally on Middlemost (1989)")
    
    # Foidite subdivison according to SINCLAS, Verma et al. 2002, Fig. 1b
    iron[iron[,1]=="foidite" & alk <= 3, 3]<-0.15
    iron[iron[,1]=="foidite" & alk > 3 & alk <= 7, 3]<-0.2
    iron[iron[,1]=="foidite" & alk > 7 & alk <= 10, 3]<-0.3
    iron[iron[,1]=="foidite" & alk > 10, 3]<-0.4
    
    # Tephrite/basanite subdivison according to SINCLAS, Verma et al. 2002, Fig. 1b
    iron[iron[,1]=="tephrite/basanite" & alk <=6, 3]<-0.2
    iron[iron[,1]=="tephrite/basanite" & alk > 6, 3]<-0.3

    # Asssign adjusted Fe2O3 and FeO
    
    Fe2O3adj<-iron[,3]*WR[,"FeOt"]/(1+0.89981*iron[,3])
    FeOadj<-Fe2O3adj/iron[,3]
    #FeOadj2<-WR[,"FeOt"]-0.89981*Fe2O3adj
    #cbind(Fe2O3adj,FeOadj,FeOadj2,Fe2O3adj/FeOadj,iron[,3],WR[,"FeOt"],FeOadj+0.89981*Fe2O3adj)

    WR[,"Fe2O3"]<-Fe2O3adj
    WR[,"FeO"]<-FeOadj
    if(anhydrous){
        # New recalculation to anhydrous basis
        anh<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","MnO","MgO","CaO","Na2O","K2O","P2O5")

        WRanh<-matrix(data = NA, ncol = length(anh), nrow = nrow(WR), byrow = TRUE, dimnames = NULL)
        rownames(WRanh)<-rownames(WR)
        colnames(WRanh)<-anh
        for (f in seq(1,nrow(WR))){
            WRanh[f,anh]<-WR[f,anh]/sum(WR[f,anh],na.rm=TRUE)*100
        }
        return(WRanh)
    }else{
        return(WR)
    }
}


# Micdlemost (1989) abstract:
# The following standard ratios are recommended: foidite 0.15–0.40, picrobasalt 0.15, 
# basanite/tephrite 0.20–0.30, basalt 0.20, trachybasalt 0.30, basaltic andesite 0.30, 
# phonotephrite 0.35, basaltic trachyandesite 0.35, andesite 0.35, tephriphonolite 0.40, 
# trachyandesite 0.40, dacite 0.40, phonolite 0.50, trachyte/trachydacite 0.50 and rhyolite 0.50.
