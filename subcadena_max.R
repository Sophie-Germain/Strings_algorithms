#######################################################################
#Este codigo genera el numero maximo de subcadenas de simbolos iguales
#de una cadena dada
#######################################################################
library(stringi)
library(digest)

#Tama??o cadena
n<-1000
#Generamos cadena
cadena <- toString(stri_rand_strings(1, n, pattern = "[1234]"))
s<-4
simbol <- matrix(0, s, n)
simbol_h <- matrix(0, s, n)

#Matriz hash cadenas buscadas
for(i in 1:n)
{
simbol[1,i] <- toString(stri_rand_strings(1, i+1, pattern = "[1]"))
simbol[2,i] <- toString(stri_rand_strings(1, i+1, pattern = "[2]"))
simbol[3,i] <- toString(stri_rand_strings(1, i+1, pattern = "[3]"))
simbol[4,i] <- toString(stri_rand_strings(1, i+1, pattern = "[4]"))
simbol_h[1,i] <- digest(simbol[1,i])
simbol_h[2,i] <- digest(simbol[2,i])
simbol_h[3,i] <- digest(simbol[3,i])
simbol_h[4,i] <- digest(simbol[4,i])
}

#Contador aparicion de subcadenas
cuenta <- matrix(0, s, n)

    for (k in 1:n){
    nsubcad <- n-k+1
      for (i in 1:nsubcad)
      {
        cad <- substr(cadena, start=i, stop=k+i) 
        if(digest(cad)==simbol_h[1,k]){
          cuenta[1,k] = cuenta[1,k] +1
        } else if(digest(cad)==simbol_h[2,k]){
          cuenta[2,k] = cuenta[2,k]+1
        } else if(digest(cad)==simbol_h[3,k]){
          cuenta[3,k] = cuenta[3,k]+1
        } else if(digest(cad)==simbol_h[4,k]){
          cuenta[4,k] = cuenta[4,k]+1
        }
      }
    } 

#Imprimimos resultados
cuenta
max<- max(cuenta)
max_cad <- which(cuenta==max(cuenta), arr.ind=TRUE)
sprintf("El numero maximo de subcadenas es de %i. Corresponde al simbolo %o y son de tama??o %x ", max, max_cad[1, 1], max_cad[1, 2]+1)



