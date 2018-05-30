set.seed(140)

arr <- c(3,6,9)
p <- c(0.15,0.5,0.35)
n <- c(4)
k <- c(20)
b1 <- c(1000)
b2 <- c(50000)
a<-sqrt(n/(n-1))
mediamuestral <- array(1:b2)





for (l in 1:b2){
  samples <- replicate(k, sample(arr,size=n,prob=p,replace=TRUE)) 
  
  mediaTotal <- mean(samples)
  
  mediaCol <- array (1:k)
  
  mediaCol <- colMeans(samples, dims = 1)
  
  for (i in 1:n){
    for (j in 1:k){
      samples[i,j] <- samples[i,j] - mediaCol[j]
    }
  }
  
  boots <- sample(samples,size=n,replace=TRUE)
  
  for (i in 1:n){
    boots[i] <- mediaTotal + boots[i]*a
  }
  mediamuestral[l] <- mean(boots)
}

mediamuestral <- sort(mediamuestral)
print("Termine")

print(mediamuestral)