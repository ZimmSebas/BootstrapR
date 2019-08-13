if (!require('ggplot2')) install.packages('ggplot2'); 
library(ggplot2)
#Verifica de instalar la librer???a para gr???ficos
set.seed(140)

#Datos para el algoritmo
#distribuci?n exponencial d=7
arr <- c(1,2,3,4,5,6,7)
p <- c(0.3510258583,0.2352996695,0.1577260853,0.1057269567,0.0708708985,0.047506184,0.0318443474)
n <- c(10)
k <- c(10)
z <- c(2.5758293035)
alphainit <- c(0.01)
b <- c(250)
a<-sqrt(n/(n-1))
d2 <- c(1.128,1.693,2.059,2.326,2.534,2.704,2.847,2.970,3.078,3.173,3.258,3.336,3.407,3.472,3.532,3.588,3.640,3.689,3.735,3.778,3.819)
d3 <- c(0.853,0.888,0.880,0.864,0.848,0.833,0.820,0.808,0.797,0.787,0.778,0.770,0.763,0.756,0.750,0.744,0.739,0.733,0.729,0.724,0.720)
len <- length(arr)

#Probabilidades para graficar por promedio de suma
probgraph <- matrix(nrow=n+1,ncol=(max(arr)*(n+2)))
probacum <- array(1:(max(arr)*(n+2)))
versum <- c(0)

#Probabilidades para graficar por rango
rangomaxi <- max(arr)-min(arr)
rangraph <- array(dim = c(length(arr),length(arr),n))
resrango <- array(dim = c(rangomaxi+1))
rangacum <- array(dim = c(rangomaxi+1))

#Probabilidades para graficar por desvio
combinaciones <- array(dim = c(8008,length(arr)))
itcomb <- c(1)
previo <- array(dim = c(7))
total = len ^ k

#Limites
ShewartDesvio <- matrix(nrow=b, ncol=2)
ShewartMedia <- matrix(nrow=b, ncol=2)

#Factorial, necesario para calcular límites de combinatoria
fact <- array(dim = c(16))

#La función no era necesaria, pero por las dudas.
factorial <- function(m){
  if (m == 0) 1
  else if(m == 1) 1
  else
    m*factorial(m-1)
}


for (i in 1:16){
  fact[i] = factorial(i-1)
}

print(fact)

 
# ------------------------------------------------------------------
# ------- Algoritmo para graficar por distribucion promedio --------
# ------------------------------------------------------------------


for (l in 1:length(probgraph)){ #inicializo el arreglo
  probgraph[l] <- 0
}

for (l in 1:length(arr)){ #Inicializo el caso base
  probgraph[1,arr[l]] <- p[l]
}

for (i in 1:n){ #Trabajo para paso i+1
  for(j in 1:((max(arr)*n-1)+1)){
    for(l in 1:length(arr)){
      aux <- probgraph[i+1,j+arr[l]]
      probgraph[i+1,j+arr[l]] <- aux + (probgraph[i,j]*p[l])
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
#qplot(1:(max(arr)*(n+2)),probgraph[n,],color=13,main="Magia",xlab="Cosa A",ylab="Cosa B")
#barplot(probgraph[n,])
#print(probgraph[n,])
#print(probacum)


# ------------------------------------------------------------------
# ------- Algoritmo para graficar por distribucion desvio est ------
# ------------------------------------------------------------------

generador <- function(it,sum,prev){
  if(it == 0){
    if (sum == 0){
      for (i in 1:len){
        combinaciones[itcomb,i] <<- c(prev[i])
      }
      itcomb <<- (itcomb + 1)
    }
  } else {
    for (i in 1:(sum+1)){
      prev[it] = (i-1)
      generador(it-1,sum-(i-1),prev)
      prev[it] = 0
    }
  }
}


for (i in 1:len){
  previo[i] = 0;
}

generador(len,k,previo);

rangraph <- array(dim = c(length(arr),length(arr),n))


print(itcomb)
print(combinaciones)

suma = 1
probdesvio = array(dim = c(itcomb))


for (i in 1:(itcomb-1)){
  for(j in 1:len){
    suma = suma * fact[(combinaciones[i,j]+1) ]
  }
  probdesvio[i] = ((fact[k+1] / suma)/total)
  suma = 1
}

print(probdesvio)

plot(probdesvio)

versuma = 0

for (i in 1:(itcomb-1)){
  versuma = versuma+probdesvio[i]
}
print(versuma)

calcdesvio = array(dim = c(itcomb))
mediaaux = 0
desvioaux = 0

for (i in 1:(itcomb-1)){
  for(j in 1:len){
    mediaaux = mediaaux + combinaciones[i,j]*j
  }
  mediaaux = (mediaaux / len)
  
  for(j in 1:len){
    if(combinaciones[i,j] != 0){
      desvioaux = ((j-mediaaux)^2)*combinaciones[i,j]
    }
  }
  
  calcdesvio[i] = sqrt(desvioaux / k)
  
}

plot(calcdesvio)

# ------------------------------------------------------------------
# --------------- Algoritmo para calcular limites ------------------
# ------------------------------------------------------------------


for (l in 1:b){ 
  samples <- replicate(k, sample(arr,size=n,prob=p,replace=TRUE)) 
  
  mediaTotal <- mean(samples)
  desviomedia <- mean(calcdesvio)
  
  ShewartMedia[l,1] <- mediaTotal-(z*desvio)/sqrt(n)
  ShewartMedia[l,2] <- mediaTotal+(z*desvio)/sqrt(n)
  
  
}