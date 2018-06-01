set.seed(140)

arr <- c(3,6,9)
p <- c(0.15,0.5,0.35)
n <- c(4)
k <- c(20)
b1 <- c(100)
b2 <- c(50000)
a<-sqrt(n/(n-1))
mediamuestral <- array(1:2000)


limiteInf <- array(1:b1)
limiteSup <- array(1:b1)



for (l in 1:b1){
  
  samples <- replicate(k, sample(arr,size=n,prob=p,replace=TRUE)) 
    
  mediaTotal <- mean(samples)
    
  mediaCol <- array (1:k)
    
  mediaCol <- colMeans(samples, dims = 1)
    
  for (i in 1:n){
    for (j in 1:k){
      samples[i,j] <- samples[i,j] - mediaCol[j]
    }
  }

  boots <- replicate(100,samples)

  perm <- sample(boots,size=8000)
  
  
  for (i in 0:1999){
    mediamuestral[i+1] = ((perm[(4*i)+1]+perm[(4*i)+2]+perm[(4*i)+3]+perm[(4*i)+4])/4)
    mediamuestral[i+1] = mediamuestral[i+1]*a + mediaTotal 
  }
  
  mediamuestral <- sort(mediamuestral)
  
  limiteInf[l] <- mediamuestral[50]
  limiteSup[l] <- mediamuestral[1950]

}

print(limiteInf)
print(limiteSup)

#hist(samples)
#hist(perm)

#for (i in 1:n){
#  boots[i] <- mediaTotal + boots[i]*a
#}
