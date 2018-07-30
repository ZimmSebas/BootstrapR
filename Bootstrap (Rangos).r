set.seed(140)

#Datos para el algoritmo
arr <- c(3,6,9)
p <- c(0.15,0.5,0.35)
n <- c(4)
k <- c(20)
b1 <- c(100)
b2 <- c(50000)
b <- b1
a<-sqrt(n/(n-1))
d2 <- c(1.128,1.693,2.059,2.326,2.534,2.704,2.847,2.970,3.078,3.173,3.258,3.336,3.407,3.472,3.532,3.588,3.640,3.689,3.735,3.778,3.819)
d3 <- c(0.853,0.888,0.880,0.864,0.848,0.833,0.820,0.808,0.797,0.787,0.778,0.770,0.763,0.756,0.750,0.744,0.739,0.733,0.729,0.724,0.720)

#Medias y límites
mediamuestral <- array(1:2000)
rangoboots <- array(1:2000)
BootsMedia <- matrix(nrow=b, ncol=2)
BootsRango <- matrix(nrow=b, ncol=2)
ShewartMedia <- matrix(nrow=b, ncol=2)
ShewartRango <- matrix(nrow=b, ncol=2)

n*k*b


#Itero de 1 a b
for (l in 1:b){
  samples <- replicate(k, sample(arr,size=n,prob=p,replace=TRUE)) 
  mediaTotal <- mean(samples)
  
  r <- (max(samples)-min(samples)/k) 
  alpha <- r / d2[n]
  
  ShewartMedia[l,1] <- mediaTotal+(3*alpha)/sqrt(n)
  ShewartMedia[l,2] <- mediaTotal-(3*alpha)/sqrt(n)
  
  ShewartRango[l,1] <- r+3*d3[n]*alpha
  ShewartRango[l,2] <- r-3*d3[n]*alpha
  
  mediaCol <- array (1:k)
  mediaCol <- colMeans(samples, dims = 1)
  
  for (i in 1:n){
    for (j in 1:k){
      samples[i,j] <- samples[i,j] - mediaCol[j]
    }
  }
  
  boots <- replicate(b,samples)
  
  perm <- sample(boots,size=(n*k*b))
  
  
  for (i in 0:((k*b)-1)){ #Permutaciones
    rangoboots[i+1] = (max(perm[(4*i)+1],perm[(4*i)+2],perm[(4*i)+3],perm[(4*i)+4])-min(perm[(4*i)+1],perm[(4*i)+2],perm[(4*i)+3],perm[(4*i)+4]))/k
    mediamuestral[i+1] = ((perm[(4*i)+1]+perm[(4*i)+2]+perm[(4*i)+3]+perm[(4*i)+4])/4)
    mediamuestral[i+1] = mediamuestral[i+1]*a + mediaTotal
    
  }
  
  mediamuestral <- sort(mediamuestral)
  rangoboots <- sort(rangoboots)
  
  BootsRango[l,1] <- rangoboots[as.integer((k*b)*0.025)]
  BootsRango[l,2] <- rangoboots[as.integer((k*b)*0.975)]
  BootsMedia[l,1] <- mediamuestral[as.integer((k*b)*0.025)]
  BootsMedia[l,2] <- mediamuestral[as.integer((k*b)*0.975)]
}


print(BootsMedia)
print(BootsRango)
print(ShewartMedia)
print(ShewartRango)

#hist(samples)
#hist(perm)

#for (i in 1:n){
#  boots[i] <- mediaTotal + boots[i]*a
#}