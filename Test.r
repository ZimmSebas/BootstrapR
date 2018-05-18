set.seed(156)

n <- c(6)
arr <- c(10,20,2)
prob <- c(0.3,0.1,0.6)
k <- c(10)
med <- array(1:k)

for (i in 1:k){
  med[i] <- mean(sample(arr,size=n,prob=prob,replace=TRUE))
}

plot(med)
