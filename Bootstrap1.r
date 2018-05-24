set.seed(140)

arr <- c(3,6,9)
p <- c(0.15,0.5,0.35)
n <- c(4)
k <- c(20)
b1 <- c(1000)
b2 <- c(50000)


samples <- replicate(k, sample(arr,size=n,prob=p,replace=TRUE)) 
samples

media <- array (1:k)



for (i in 0:k){

  media[i] <- rowMeans(samples, na.rm = FALSE, dims = 1)
  media
  for (j in 0:n){
    samples[i][j] <- samples[i][j] - media
  }
}

samples