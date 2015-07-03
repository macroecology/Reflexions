
reflexions<- function (datos){
#Species richness
ks0<-rowSums(datos)
#Species range size
kp0<-colSums(datos)
#Create a datos of L(No. sites) x 19(No. moments), 
kp<- matrix (data=0,ncol=length(datos),nrow=19)
#Create a datos of T(No. speccies, columns) x 19(No. moments)
ks<- matrix(data=0,nrow=19,ncol=nrow(datos))
#Assign to the first row the first moment
kp[1,]<-kp0
ks[1,]<-ks0

for (j in 1:18)
{
  for (i in 1:nrow(datos))
  {
    ks[j+1,i]<- 1/ks[1,i]*sum(datos[i,]*kp[j,])
    
    for (z in 1:ncol(datos))
    {  
      kp[j+1,z]<- 1/kp[1,z]*sum(datos[,z]*ks[j,])
    }
  }
}

write.table(ks,"ks.txt",sep="\t",
            row.names=c(0:18))
write.table(kp,"kp.txt",sep="\t",
            row.names=c(0:18))
return (list (ks=ks, kp=kp))
}


