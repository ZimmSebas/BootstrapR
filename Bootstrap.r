if (!require('ggplot2')) install.packages('ggplot2'); 
library(ggplot2) 
#Verifica de instalar la librería para gráficos

set.seed(140)

#Datos para el algoritmo
arr <- c(3,6,9)
p <- c(0.15,0.5,0.35)
n <- c(4)
k <- c(20)
b1 <- c(100)
b2 <- c(10000)
a<-sqrt(n/(n-1))
d2 <- c(1.128,1.693,2.059,2.326,2.534,2.704,2.847,2.970,3.078,3.173,3.258,3.336,3.407,3.472,3.532,3.588,3.640,3.689,3.735,3.778,3.819)
d3 <- c(0.853,0.888,0.880,0.864,0.848,0.833,0.820,0.808,0.797,0.787,0.778,0.770,0.763,0.756,0.750,0.744,0.739,0.733,0.729,0.724,0.720)
versum <- c(0)

#Aclaro nro de bootstrap
b <- b1

#Medias y rangos
mediamuestral <- array(1:2000)
rangoboots <- array(1:2000)
rangoshewart <- array(1:k)

#Limites
BootsMedia <- matrix(nrow=b, ncol=2)
BootsRango <- matrix(nrow=b, ncol=2)
ShewartMedia <- matrix(nrow=b, ncol=2)
ShewartRango <- matrix(nrow=b, ncol=2)

#Probabilidades para graficar
probgraph <- matrix(nrow=n+1,ncol=(max(arr)*(n+2)))
probacum <- array(1:(max(arr)*(n+2)))


# --------- Algoritmo para graficar probabilidad ---------

for (l in 1:length(probgraph)){ #inicializo el arreglo
  probgraph[l] <- 0
}

for (l in 1:length(arr)){ #Inicializo el caso base
  probgraph[1,arr[l]] <- p[l]
}

for (i in 1:n){ #Trabajo para paso i+1
  for(j in 1:((max(arr)*n-1)+1)){
    for(l in 1:length(arr)){
      aux <- probgraph[i+1,j+arr[k]]
      probgraph[i+1,j+arr[k]] <- aux + (probgraph[i,j]*p[k])
    } 
  }
}

#Verifico si la suma da 1.
versum <- probgraph[n,1]

#Preparo la probabilidad acumulada
probacum <- probgraph[n,1]

for (i in 2:(max(arr)*(n+2))){
  probacum[i] <- probacum[i-1] + probgraph[n,i]
  versum <- versum + probgraph[n,i]
}
qplot(1:(max(arr)*(n+2)),probgraph[n,],color=13,main="Magia",xlab="Cosa A",ylab="Cosa B")
barplot(probgraph[n,])




# --------- Algoritmo para calcular limites ---------



for (l in 1:b){ 
  samples <- replicate(k, sample(arr,size=n,prob=p,replace=TRUE)) 
  
  for(m in 1:k){
    rangoshewart[m] <- max(samples[,m])-min(samples[,m])
  }

  mediaTotal <- mean(samples)
  rmedia <- mean(rangoshewart)
  
  desvio <- rmedia / d2[n]
  
  ShewartMedia[l,1] <- mediaTotal+(3*desvio)/sqrt(n)
  ShewartMedia[l,2] <- mediaTotal-(3*desvio)/sqrt(n)
  
  ShewartRango[l,1] <- rmedia+3*d3[n]*desvio
  ShewartRango[l,2] <- rmedia-3*d3[n]*desvio
  
  mediaCol <- array (1:k)
  mediaCol <- colMeans(samples, dims = 1)
  
  bootsrango <- replicate(b,samples)
  permrango <- sample(bootsrango,size=(n*k*b))
  permrango <- array(permrango,c(n,k*b))
  
  
  for (i in 1:n){
    for (j in 1:k){
      samples[i,j] <- samples[i,j] - mediaCol[j] #Le resto la media
    }
  }
  
  boots <- replicate(b,samples)
  perm <- sample(boots,size=(n*k*b))
  perm <- array(perm,c(n,k*b))
  
  
  for (i in 0:((k*b)-1)){ 
    rangoboots[i+1] = (max(permrango[,i+1])-min(permrango[,i+1]))
    mediamuestral[i+1] = mean(perm[,i+1])
    mediamuestral[i+1] = mediamuestral[i+1]*a + mediaTotal
  }
  
  mediamuestral <- sort(mediamuestral)
  rangoboots <- sort(rangoboots)
  
  print(mediamuestral)
  print(rangoboots)
  
  BootsRango[l,1] <- rangoboots[as.integer((k*b)*0.025)]
  BootsRango[l,2] <- rangoboots[as.integer((k*b)*0.975)]
  BootsMedia[l,1] <- mediamuestral[as.integer((k*b)*0.025)]
  BootsMedia[l,2] <- mediamuestral[as.integer((k*b)*0.975)]
}

print(BootsMedia)
print(BootsRango)
print(ShewartMedia)
print(ShewartRango)