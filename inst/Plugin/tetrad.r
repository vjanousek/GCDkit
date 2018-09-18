# Calculates magnitude of the lathanide tetrad effect following Irber (1999) 
tetrad<-function(method=NULL){
   if(is.null(method)) method<-"Nakamura"
   if(method!="Nakamura" & method!="Boynton") return("Wrong normalizing method")
   chondrit<-selectNorm(method,REE.only=TRUE)
   model<-rownames(chondrit)
   A<-filterOut(WR,colnames(chondrit),2)
   A[A==0]<-NA
   x<-.normalization(A,chondrit)
   cet<-x[,"Ce"]/(x[,"La"]^(2/3)*x[,"Nd"]^(1/3))
   prt<-x[,"Pr"]/(x[,"La"]^(1/3)*x[,"Nd"]^(2/3))
   te1<-sqrt(prt*cet)

   tbt<-x[,"Tb"]/(x[,"Gd"]^(2/3)*x[,"Ho"]^(1/3))
   dyt<-x[,"Dy"]/(x[,"Gd"]^(1/3)*x[,"Ho"]^(2/3))
   te3<-sqrt(tbt*dyt)

   te.mean<-sqrt(te1*te3)
   z<-cbind(cet,prt,te1,tbt,dyt,te3,te.mean)
   colnames(z)<-c("Ce/Cet","Pr/Prt","t1","Tb/Tbt","Dy/Dyt","t3","TE 1-3")
   results<<-z
   return(z)
}

if(getOption("gcd.menus")!=""){winMenuAddItem("Plugins","REE tetrad effect","tetrad()")}
