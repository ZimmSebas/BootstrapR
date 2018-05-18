set.seed(140)

arr <- c(3,6,9)
p <- c(0.2,0.5,0.3)
n <- c(4)
k <- c(20)
b1 <- c(1000)
b2 <- c(50000)

test <- c()
test

for (i in 0:k){
   test[length(test)+1] = sample(arr,size=n,prob=p,replace=TRUE)
   test
  media <- mean(test[i])
  media
  for (j in 0:n){
    test[i][j] <- test[i][j] - media
  }
}

test