if (!require('ggplot2')) install.packages('ggplot2'); 
library(ggplot2)
#Verifica de instalar la librer???a para gr???ficos

set.seed(140)

#Datos para el algoritmo
#distribuci?n exponencial d=7
arr <- c(1,2,3,4,5,6,7)
p <- c(0.3510258583,0.2352996695,0.1577260853,0.1057269567,0.0708708985,0.047506184,0.0318443474)
n <- c(3)
k <- c(15)
z <- c(2.5758293035)
alphainit <- c(0.01)
b <- c(25)
bboots <- c(300)
a<-sqrt(n/(n-1))
d2 <- c(1.128,1.693,2.059,2.326,2.534,2.704,2.847,2.970,3.078,3.173,3.258,3.336,3.407,3.472,3.532,3.588,3.640,3.689,3.735,3.778,3.819)
d3 <- c(0.853,0.888,0.880,0.864,0.848,0.833,0.820,0.808,0.797,0.787,0.778,0.770,0.763,0.756,0.750,0.744,0.739,0.733,0.729,0.724,0.720)

#print(d2[n])
#Medias y rangos
mediamuestral <- array(1:(k*bboots)+1)
rangoboots <- array(1:(k*bboots)+1)
rangoshewart <- array(1:k)

#Limites
BootsMedia <- matrix(nrow=b, ncol=2)
BootsRango <- matrix(nrow=b, ncol=2)
ShewartMedia <- matrix(nrow=b, ncol=2)
ShewartRango <- matrix(nrow=b, ncol=2)

#Alphas
AlphaBootsMedia <- matrix(nrow=b, ncol=2)
AlphaBootsRango <- matrix(nrow=b, ncol=2)
AlphaShewartMedia <- matrix(nrow=b, ncol=2)
AlphaShewartRango <- matrix(nrow=b, ncol=2)
AlphaBootsMediaProm <- array(1:2)
AlphaBootsRangoProm <- array(1:2)
AlphaShewartMediaProm <- array(1:2)
AlphaShewartRangoProm <- array(1:2)


#Probabilidades para graficar por promedio de suma
probgraph <- matrix(nrow=n+1,ncol=(max(arr)*(n+2)))
probacum <- array(1:(max(arr)*(n+2)))
versum <- c(0)

#Probabilidades para graficar por rango
rangomaxi <- max(arr)-min(arr)
rangraph <- array(dim = c(length(arr),length(arr),n))
resrango <- array(dim = c(rangomaxi+1))
rangacum <- array(dim = c(rangomaxi+1))


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
qplot(1:(max(arr)*(n+2)),probgraph[n,],color=13,main="Magia",xlab="Cosa A",ylab="Cosa B")
barplot(probgraph[n,])
print(probgraph[n,])

# ------------------------------------------------------------------
# --------- Algoritmo para graficar por distribucion rango ---------
# ------------------------------------------------------------------

for (l in 1:length(rangraph)){ #inicializo el arreglo
  rangraph[l] <- 0
}

for (i in 1:length(arr)){ #Preparo caso base
  rangraph[i,i,1] <- p[i]
}

for (it in 2:n){
  for (mini in 1:length(arr)){
    for(maxi in 1:length(arr)){
      for(t in 1:length(arr)){
        if(arr[t]>arr[maxi]){
          rangraph[mini,t,it] <- rangraph[mini,t,it] + rangraph[mini,maxi,it-1]*p[t]
        }
        else if(arr[t]<arr[mini]){
          rangraph[t,maxi,it] <- rangraph[t,maxi,it] + rangraph[mini,maxi,it-1]*p[t]
        }
        else{
          rangraph[mini,maxi,it] <- rangraph[mini,maxi,it] + rangraph[mini,maxi,it-1]*p[t]
        }
      }
    } 
  }
}

for(i in 1:length(resrango)){
  resrango[i] <- 0
}

for(i in 1:length(arr)){
  for(j in i:length(arr)){
    resrango[(arr[j]-arr[i])+1] <- resrango[(arr[j]-arr[i])+1] + rangraph[i,j,n] 
  }
}

rangacum[1] = resrango[1]

for(i in 2:length(rangacum))
  rangacum[i] <- rangacum[i-1] + resrango[i]

plot(0:rangomaxi,resrango)
print(resrango)

# ------------------------------------------------------------------
# --------------- Algoritmo para calcular limites ------------------
# ------------------------------------------------------------------


for (l in 1:b){ 
  samples <- replicate(k, sample(arr,size=n,prob=p,replace=TRUE)) 
  
  for(m in 1:k){
    rangoshewart[m] <- max(samples[,m])-min(samples[,m])
  }
  
  mediaTotal <- mean(samples)
  rmedia <- mean(rangoshewart)
  desvio <- rmedia / d2[n+1]
  
  
  ShewartMedia[l,1] <- mediaTotal-(z*desvio)/sqrt(n)
  ShewartMedia[l,2] <- mediaTotal+(z*desvio)/sqrt(n)
  
  ShewartRango[l,1] <- max(0,rmedia-z*d3[n+1]*desvio)
  ShewartRango[l,2] <- rmedia+z*d3[n+1]*desvio
  
  mediaCol <- array (1:k)
  mediaCol <- colMeans(samples, dims = 1)
  
  bootsrango <- replicate(bboots,samples)
  permrango <- sample(bootsrango,size=(n*k*bboots))
  permrango <- array(permrango,c(n,k*bboots))
  
#ACA LE SAQU? LA PARTE DE RESTAR LAS MEDIAS PORQUE NOTE QUE ERA MEJOR EL RESULTADO SIN ESO  
 # for (i in 1:n){
#    for (j in 1:k){
#      samples[i,j] <- samples[i,j] - mediaCol[j] #Le resto la media
#    }
#  }
  
  boots <- replicate(bboots,samples)
  perm <- sample(boots,size=(n*k*bboots))
  perm <- array(perm,c(n,k*bboots))


  for (i in 0:((k*bboots)-1)){ 
    rangoboots[i+1] = (max(permrango[,i+1])-min(permrango[,i+1]))
    mediamuestral[i+1] = mean(perm[,i+1])
#   mediamuestral[i+1] = mediamuestral[i+1]*a + mediaTotal
  }
  
  mediamuestral <- sort(mediamuestral)
  rangoboots <- sort(rangoboots)
  

  
  BootsRango[l,1] <- rangoboots[as.integer((k*bboots)*alphainit/2)]
  BootsRango[l,2] <- rangoboots[as.integer((k*bboots)*(1-alphainit/2))+1]
  BootsMedia[l,1] <- mediamuestral[as.integer((k*bboots)*alphainit/2)]
  BootsMedia[l,2] <- mediamuestral[as.integer((k*bboots)*(1-alphainit/2))+1]
}
#print(rangoshewart)
#print(samples)
#print(mediaTotal)
#print(mediamuestral)
#print(perm[1,])

# ------------------------------------------------------------------
# ----------------------- Calculo de Alpha -------------------------
# ------------------------------------------------------------------
#ACA NO HACIA FALTA RESTARLE 1 EN EL CASO QUE COINCID?A, ASI QUE QUEDO CON EL if SIN HACER NADA
#LE CORREG? PAR?NTESIS MAL UBICADOS Y UNA REFERENCIA QUE HACIA A UN VECTOR EQUIVOCADO
for (i in 1:b){
  
  #Calculo alpha de Bootstrap Media CORRECCION YA HECHA Y VERIFICADA
  AlphaBootsMedia[i,1] = probacum[as.integer(BootsMedia[i,1]*n-0.0000001)+1]
  AlphaBootsMedia[i,2] = 1 - probacum[as.integer(BootsMedia[i,2]*n)+1]
  

  
  #Calculo alpha de Bootstrap Rango CORRECCION YA HECHA Y VERIFICADA
  if( (as.integer(BootsRango[i,1]+0.9999999)) < 1)
    AlphaBootsRango[i,1] = 0
  else
    AlphaBootsRango[i,1] = rangacum[(as.integer(BootsRango[i,1]-0.0000001))+1]
  AlphaBootsRango[i,2] = 1 - rangacum[min(length(rangacum),(as.integer(BootsRango[i,2])+1))]
  
   
  #Calculo alpha de Shewart Media CORRECCION YA HECHA Y VERIFICADA
  AlphaShewartMedia[i,1] = probacum[as.integer(ShewartMedia[i,1]*n-0.0000001)+1]
  AlphaShewartMedia[i,2] = 1 - probacum[as.integer(ShewartMedia[i,2]*n)+1]

  
  #Calculo alpha de Shewart Rango CORRECCION YA HACHA Y VERIFICADA  
  if ((as.integer(ShewartRango[i,1]+0.9999999)) < 1 )
    AlphaShewartRango[i,1] = 0
  else
    AlphaShewartRango[i,1] = rangacum[(as.integer(ShewartRango[i,1]-0.0000001))+1]
  
  AlphaShewartRango[i,2] = 1 - rangacum[min(length(rangacum),(as.integer(ShewartRango[i,2]))+1)]

}

AlphaBootsMediaProm[1] = mean(AlphaBootsMedia[,1])
AlphaBootsMediaProm[2] = mean(AlphaBootsMedia[,2])

AlphaBootsRangoProm[1] = mean(AlphaBootsRango[,1])
AlphaBootsRangoProm[2] = mean(AlphaBootsRango[,2])

AlphaShewartMediaProm[1] = mean(AlphaShewartMedia[,1])
AlphaShewartMediaProm[2] = mean(AlphaShewartMedia[,2])

AlphaShewartRangoProm[1] = mean(AlphaShewartRango[,1])
AlphaShewartRangoProm[2] = mean(AlphaShewartRango[,2])
# ------------------------------------------------------------------
# -------------------- Impresiones y Gr�ficos ----------------------
# ------------------------------------------------------------------

#print(samples)

#Estos son todos los limites:
#print(BootsMedia)
#print(BootsRango)
#print(ShewartMedia)
#print(ShewartRango)

#Estos son los alphas de los l�mites:
#print(AlphaBootsMedia)
#print(AlphaBootsRango)
#print(AlphaShewartMedia)
#print(AlphaShewartRango)

#print(AlphaBootsMediaProm)
#print(AlphaBootsRangoProm)
#print(AlphaShewartMediaProm)
#print(AlphaShewartRangoProm)

#Esta es la distribucion de probabilidad
#print(probgraph[n,])
#print(rangacum)
#print(probacum)


#Graficos con limites Shewart y Bootstrap
vargrafico <- ( 1:(max(arr)*(n+2)))/n

#Grafico Shewart Media
(qplot(vargrafico,probgraph[n,],color=13,main="Distribuci�n de sumas",xlab="Suma",ylab="Probabilidad", ylim = c(0.0000000000001,max(probgraph[n,]))) + geom_vline(xintercept = mean(ShewartMedia[,1]), colour="green") + geom_vline(xintercept = mean(ShewartMedia[,2]), colour = "red") )

#Grafico Shewart Rango
#(qplot(0:rangomaxi,resrango,color=13,main="Distribuci???n de rango",xlab="Rango",ylab="Probabilidad",ylim = c(0.0000000000001,max(resrango))) + geom_vline(xintercept = mean(ShewartRango[,1]), colour="green") + geom_vline(xintercept = mean(ShewartRango[,2]), colour = "red" ) )

#Grafico Bootstrap Media
#(qplot(vargrafico,probgraph[n,],color=13,main="Distribuci???n de sumas",xlab="Suma",ylab="Probabilidad",ylim = c(0.0000000000000000001,max(probgraph[n,]))) + geom_vline(xintercept = mean(BootsMedia[,1]), colour="green") + geom_vline(xintercept = mean(BootsMedia[,2]), colour = "red" ) )

#Grafico Bootstrap Rango
#(qplot(0:rangomaxi,resrango,color=13,main="Distribuci???n de rango",xlab="Suma",ylab="Probabilidad", ylim = c(0.0000000000001,max(resrango))) + geom_vline(xintercept = mean(BootsRango[,1]), colour="green") + geom_vline(xintercept = mean(BootsRango[,2]), colour = "red" ) )


#Distribucion probabilidad0...
#qplot(vargrafico,probgraph[n,],color=13,main="Distribuci???n de sumas",xlab="Suma",ylab="Probabilidad",ylim = c(0.0000000000000000001,max(probgraph[n,])))



#Creacion de archivos txt
write.table(AlphaBootsMedia,"Alpha Bootstrap Media 0,01.txt",sep = "\t",row.names = FALSE, col.names = c("Alfa inferior", "Alfa superior"))
write.table(AlphaBootsRango,"Alpha Bootstrap Rango 0,01.txt",sep = "\t",row.names = FALSE, col.names = c("Alfa inferior", "Alfa superior"))
write.table(AlphaShewartMedia,"Alpha Shewart Media 0,01.txt",sep = "\t",row.names = FALSE, col.names = c("Alfa inferior", "Alfa superior"))
write.table(AlphaShewartRango,"Alpha Shewart Rango 0,01.txt",sep = "\t",row.names = FALSE, col.names = c("Alfa inferior", "Alfa superior"))

write.table(BootsMedia,"Limite Bootstrap Media 0,01.txt",sep = "\t",row.names = FALSE, col.names = c("Limite inferior", "Limite superior"))
write.table(BootsRango,"Limite Bootstrap Rango 0 01.txt",sep = "\t",row.names = FALSE, col.names = c("Limite inferior", "Limite superior"))
write.table(ShewartMedia,"Limite Shewart Media 0 01.txt",sep = "\t",row.names = FALSE, col.names = c("Limite inferior", "Limite superior"))
write.table(ShewartRango,"Limite Shewart Rango 0 01.txt",sep = "\t",row.names = FALSE, col.names = c("Limite inferior", "Limite superior"))

# Z sale del 1-alpha/2 en la inversa de la acumulada de la normal.

