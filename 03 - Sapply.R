# TD 2
# Guillem Rigaill - Plan d'experience
# 21/01/16

# Exercice 1
# 1)
# 10 individus par groupe

# 2)
# Var moy 1/sqrt(10)


n1 <- c(1:10)
n2 <- c(1:10)

test <- function(n1, n2){
population1 <- rnorm(n1, 175, 1) # On génère notre loi normale pour nos 2 populations
population2 <- rnorm(n2, 176, 1)

t.test(population1, population2)$p.value

}
simul <- sapply(rep(1, 1000), FUN = function(x){test(n1,n2)})

sum(simul <= 0.05) # Nombre des fois où la pvalue est inférieure au seuil alpha : rejet H0, diff entre les pops
mean(simul<0.05) # Moyenne des fois où la pvalue est inférieure au seuil alpha

# 3)

# Fonction qui reprend test, qui change le n1 et le n2, et qui fait le plot des moyenne des sapply de nos tests
evolution <- function() {
  i = 2
  retour <- numeric(20)
  while (i<= 18){
    sappli <-sapply(rep(1, 1000), FUN = function(x){test(i, 20-i)})
    retour[i] <- mean(sappli <= 0.05)
    i = i + 1
  }
  return(retour)

}
plot(evolution(), type = "l", ylab="Moy Pvalues", xlab="Taille de n1")

# 4)
test <- function(n1, n2){
  population1 <- rnorm(n1, 175, 1) # On génère notre loi normale pour nos 2 populations avec 2 de différence
  population2 <- rnorm(n2, 177, 1)
  
  t.test(population1, population2)$p.value
  
}
evolution2 <- function() {
  i = 2
  retour <- numeric(20)
  while (i<= 18){
    sappli <-sapply(rep(1, 1000), FUN = function(x){test(i, 20-i)})
    retour[i] <- mean(sappli <= 0.05)
    i = i + 1
  }
  return(retour)
  
}
plot(evolution2(), type = "l", ylab="Moy Pvalues", xlab="Taille de n1")

# 5)
test3 <- function(n1, n2){
  population1 <- 175 + rt(n1, df=8)/sqrt(8/6) # On génère un bruit de Student sur nos moyennes
  population2 <- 176 + rt(n2, df=8)/sqrt(8/6)
  
  t.test(population1, population2)$p.value
  
}
test3(10,10)

evolution3 <- function() {
  i = 2
  retour <- numeric(20)
  while (i<= 18){
    sappli <-sapply(rep(1, 1000), FUN = function(x){test3(i, 20-i)})
    retour[i] <- mean(sappli <= 0.05)
    i = i + 1
  }
  return(retour)
  
}
lines(evolution3(), col = "blue")
"""
Peu importe la distribution de la population (Student ou normale), il faut prendre 10 et 10 comme populations,
pour avoir la plus forte moyenne des pvalues inférieures à 0.05.
"""
# 6)
test6 <- function(n1, n2){
  population1 <- rnorm(n1, 175, 1) # On génère notre loi normale pour nos 2 populations avec 2 de différence
  population2 <- rnorm(n2, 176, 1)
  
  wilcox.test(population1, population2)$p.value
  
}
evolution6 <- function() {
  i = 2
  retour <- numeric(20)
  while (i<= 18){
    sappli <-sapply(rep(1, 1000), FUN = function(x){test(i, 20-i)})
    retour[i] <- mean(sappli <= 0.05)
    i = i + 1
  }
  return(retour)
  
}
plot(evolution6(), type = "l", ylab="Moy Pvalues", xlab="Taille de n1")

# 7)

test7 <- function(n1, n2){
  population1 <- rnorm(n1, 175, 1) # On génère notre loi normale pour nos 2 populations avec 2 de différence
  population2 <- rnorm(n2, 176, 1)
  
  t.test(population1, population2)$p.value
  
}
evolution7 <- function() {
  i = 2
  retour <- numeric(12)
  while (i<= 10){
    sappli <-sapply(rep(1, 1000), FUN = function(x){test(i, 12-i)})
    retour[i] <- mean(sappli <= 0.05)
    i = i + 1
  }
  return(retour)
  
}
plot(evolution7(), type = "l", ylab="Moy Pvalues", xlab="Taille de n1")

# Exercice 2
# 1)

testlezard <- function(n1, n2){
  popl1 <- rnorm(n1, 30, sd = 1.2) # On génère notre loi normale pour nos 2 populations
  popl2 <- rnorm(n2, 30.05, sd = 3)
  
  t.test(popl1, popl2)$p.value
}
resP <- list()
i=1
for(n1 in 3*2:100){
  for(n2 in 3*2:100){
    resP[[i]] <- c(n1, n2, n1+n2, mean( sapply(rep(n1, 10), FUN=testlezard, n2=n2) < 0.05))
    i = i+1
  }
}
mat <- do.call("rbind", resP)

plot(mat[, 3], mat[, 4])


# Exercice 3
# 1)

testgene <- function(n1, n2){
  patient1 <- rnorm(n1, 10 , sd = 1)
  patient2 <- rnorm(n2, 11 , sd = 1)
  
  t.test(patient1, patient2)$p.value
}

resul <- list()
i=1
for(n1 in 2:24){
  n2 = (10000 - 400*n1)/100
    resul[[i]] <- c(n1, n2, n1+n2, mean( sapply(rep(n1, 10000), FUN=testgene, n2=n2) < 0.05))
    i = i+1
  }
mat <- do.call("rbind", resul)
plot(mat[, 1], mat[, 4], xlab = "Nombre de patients sains", ylab = "Pourcentage de réussite au T-test (avec 1000 simulations)")
lines(mat[, 1], mat[, 4], xlab = "Nombre de patients sains", ylab = "Pourcentage de réussite au T-test (avec 1000 simulations)")

mat

# 2)
testgene <- function(n1, n2){
  patient1 <- rnorm(n1, 10 , sd = 1)
  patient2 <- rnorm(n2, 10.5 , sd = 1)
  
  t.test(patient1, patient2)$p.value
}

resul <- list()
i=1
for(n1 in 2:24){
  n2 = (10000 - 400*n1)/100
  resul[[i]] <- c(n1, n2, n1+n2, mean( sapply(rep(n1, 1000), FUN=testgene, n2=n2) < 0.05))
  i = i+1
}
mat <- do.call("rbind", resul)
plot(mat[, 1], mat[, 4], xlab = "Nombre de patients sains", ylab = "Pourcentage de réussite au T-test (avec 1000 simulations)")
mat

# Exercice 4
# 1)
salle <- matrix(1,4,8)
salle
