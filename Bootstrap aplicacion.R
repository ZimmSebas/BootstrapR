if (!require('ggplot2')) install.packages('ggplot2'); 
library(ggplot2)

if (!require('readxl')) install.packages("readxl"); 
library(readxl)


set.seed(140)

#Datos para el algoritmo
#distribuci?n exponencial d=7
samples = c(9,10,11,11,12,10,9,11,12,10,11,12,12,12,11)
n <- c(3)
k <- c(5)
alphainit <- c(0.0026997961)
bboots <- c(300)
a<-sqrt(n/(n-1))

#Medias y rangos
mediamuestral <- array(1:(k*bboots)+1)
rangoboots <- array(1:(k*bboots)+1)

#Limites
BootsMedia <- array(1:2)
BootsRango <- array(1:2)

#Media y rango
Rango = array(1:k)
Media = array(1:k)

# ------------------------------------------------------------------
# --------------- Algoritmo para calcular limites ------------------
# ------------------------------------------------------------------

for(i in 0:(k-1)){
  maximo = min(samples)
  minimo = max(samples)
  Media[i+1] = 0
  for(j in 1:n){ 
    if(maximo < samples[i*n+j]){
      maximo = samples[i*n+j]
    }
    if(minimo > samples[i*n+j]){
      minimo = samples[i*n+j]
    }
    Media[i+1] = Media[i+1] + samples[i*n+j]
  }
  Rango[i+1] = maximo - minimo
  Media[i+1] = Media[i+1] / n
}


bootsrango <- replicate(bboots,samples)
permrango <- sample(bootsrango,size=(n*k*bboots))
permrango <- array(permrango,c(n,k*bboots))

boots <- replicate(bboots,samples)
perm <- sample(boots,size=(n*k*bboots))
perm <- array(perm,c(n,k*bboots))


for (i in 0:((k*bboots)-1)){ 
  rangoboots[i+1] = (max(permrango[,i+1])-min(permrango[,i+1]))
  mediamuestral[i+1] = mean(perm[,i+1])
}
mediamuestral <- sort(mediamuestral)
rangoboots <- sort(rangoboots)

BootsRango[1] <- rangoboots[as.integer((k*bboots)*alphainit/2)]
BootsRango[2] <- rangoboots[as.integer((k*bboots)*(1-alphainit/2))+1]
BootsMedia[1] <- mediamuestral[as.integer((k*bboots)*alphainit/2)]
BootsMedia[2] <- mediamuestral[as.integer((k*bboots)*(1-alphainit/2))+1]

print(mediamuestral)
print(rangoboots)

print(BootsMedia)
print(BootsRango)

# ------------------------------------------------------------------
# -------------------- Impresiones y Gr�ficos ----------------------
# ------------------------------------------------------------------


#Graficos con limites Shewart y Bootstrap

datmedia <- data.frame(Media)
datrango <- data.frame(Rango)
x_i <- 1:k

#Grafico Bootstrap Media
medgraph <- ggplot(datmedia, aes(x_i,Media), colour = "#610a15")
(medgraph + geom_point(colour = "#610a15")) + geom_hline(yintercept = BootsMedia[1], colour="steelblue") + geom_hline(yintercept = BootsMedia[2], colour = "#1e6b70" )

#Grafico Bootstrap Rango
rangraph <- ggplot(datrango, aes(x_i,Rango), colour = "#610a15")
(rangraph + geom_point(colour = "#610a15")) + geom_hline(yintercept = BootsRango[1], colour="steelblue") + geom_hline(yintercept = BootsRango[2], colour = "#1e6b70" )

#Creacion de archivos txt
#write.table(BootsMedia,"Limite Bootstrap Media 0,0027.txt",sep = "\t",row.names = FALSE, col.names = c("Limite inferior", "Limite superior"))
#write.table(BootsRango,"Limite Bootstrap Rango 0,0027.txt",sep = "\t",row.names = FALSE, col.names = c("Limite inferior", "Limite superior"))

