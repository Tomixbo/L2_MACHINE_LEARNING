rm(list=ls())

#chargement de la librairie
library(rmr2)

#fonctionnement local
rmr.options(backend="local")

#data est un vecteur de valeurs ici
the.map <- function(.,data){
  #nombre d'obsv
  n <- length(data)
  #génération de la clé via un modulo 4
  cle <- 1+(1:n)%%4
  #output
  output <- keyval(cle,data)
  print(output)
  #print(output)
  #
  return(output)
}

#reduce ==> somme
the.reduce <- function(k,v){
  print("on est dans reduce")
  #
  print(k)
  print(v)
  #minimum
  mini <- min(v)
  #resultat
  resultat <- keyval(k,mini)
  #
  return(resultat)
}


#vérification
set.seed(1)
x <- rnorm(20)

#résultat direct
print(min(x))

#copie des données sur HDFS
dfs.x <- to.dfs(x)

#appel de la fonction MapReduce
res.dfs <- mapreduce(input=dfs.x,map=the.map,reduce=the.reduce)

#récupération en mémoire des résultats sur HDFS
res <- from.dfs(res.dfs)
print(res)

#calcul du résultat final
print(min(res$val))
