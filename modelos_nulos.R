goods<- c(1:7)

res<- list ()
for (i in 1:7){
  case<- combn (goods, i)
  res [[i]]<- case
}


market <- matrix (0, 7, 127)
a<- 0
for (i in 1:7){
  for (j in 1:ncol (res [[i]])){
    a<- a+1
    market [res [[i]][,j] , a]<- 1
  }  
}

buyer_type<- t(market)
store<- t(market)


store_richness<- function (buyer_type, store){
  richness<- matrix (0, nrow(buyer_type), nrow(store))
  for (i in 1:nrow (buyer_type)){
    resources<- sum (buyer_type [i,])
    cases<- which (colSums (buyer_type [i,] * t(store))== resources)
    richness [cases,i]<- 1
  } 
  richness
}

rich<- store_richness (buyer_type, store)

remove<- sample (which (rich==1), length (which (rich==1))*0.25)
vector_matriz<- as.vector(rich) 
vector_matriz [remove]<- 0
matrix_25<- matrix (vector_matriz, 127, 127)
rowSums (matrix_25)

res<- reflexions (matrix_25)
head (res$ks)
tail (res$ks)

rich2<- rich

rich2<- rbind (rich, rich)

calcula_richness<- function (matrix, quita){
remove<- sample (which (rich2==1), length (which (rich2==1))*quita)
vector_matriz<- as.vector(rich2) 
vector_matriz [remove]<- 0
matrix_25<- matrix (vector_matriz, nrow (rich2), 127)
suma<- rowSums (matrix_25)
matrix_sin0<- matrix_25 [rowSums (matrix_25)!= 0, ]
pot<- rowSums (rich2)[rowSums (matrix_25)!= 0]
res2<- reflexions (matrix_sin0)
res2
}


res_5<- calcula_richness (rbind (rich, rich), 0.05)
res_10<- calcula_richness (rbind (rich, rich), 0.10)
res_25<- calcula_richness (rbind (rich, rich), 0.25)
res_30<- calcula_richness (rbind (rich, rich), 0.25)


X11()
plot (pot, res2$ks[19,])
points (pot3,res3$ks[19,], pch=16, col="red" )




rich3<- rbind (rich, rich, rich)
res3<- reflexions (rich2)
pot3 <- rowSums (rich3)


length (pot)
length (res2$ks[19,])
X11()
plot (pot, res2$ks[19,])
points (pot3,res3$ks[19,], pch=16, col="red" )





rowSums (rich)

bt_rangesize<- function (buyer_type, store){
  bt<- NULL
  for (i in 1:nrow (buyer_type)){
    resources<- sum (buyer_type [i,])
    cases<- which (colSums (buyer_type [i,] * t(store))== resources)
    bt<- c(bt, length (cases))
  } 
  bt
}

bt_rangesize(buyer_type, store)




rowSums (matrix_25)
