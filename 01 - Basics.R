# TD 1
# Guillem RIgaill - Plan d'experience
# 18/01/16


# Exercice 1

# 1-2)
setwd("/home/etudiant/Bureau")

?rbinom
data<-rbinom(100, 1, 0.5)
plot(data)
hist(data)


simus <- sapply(rep(1,10^4), FUN=function(x) {rbinom(1, size=10000, prob=1/2)/10000 })
hist(simus, main="", xlab="Moyenne", ylab="Nombre de lancers")
# Il faudrait faire la variance selon le nombre d'expériences pour voir la convergence vers la loi centrale

#3)
lance<-sample.int(6, size = 1000, replace = TRUE)
hist(lance)
mean(lance)
simus2 <- sapply(rep(1,10^4), FUN=function(x) {mean(sample.int(6, size = 1000, replace = TRUE))})
hist(simus2)

#4)
simus3 <- sapply(rep(1,10^4), FUN=function(x) {mean(runif(10000, min = 2, max = 4))}) #Avec la moyenne
hist(simus3)
plot(density(simus3))
simus4 <- sapply(rep(1,10^4), FUN=function(x) {sd(runif(10000, min = 2, max = 4))}) #Avec la variance
hist(simus4)
plot(density(simus4))

# Exercice 2
# 1)
sum(sample(rep(1:13,4),5)==1)==4 # Test d'un jeu de carte
#On a un vecteur dans lequel on repete les chiffres de 1 à 13 et on prend que les as dans une main de 5 cartes
sim5 <- sapply(rep(1,10^5), FUN=function(x) {sum(sample(rep(1:13,4),5)==1)==4}) # 
mean(sim5) # On a un vecteur de 0 et de 1. On fait la moyenne pour avoir la proba

#2)
sim6 <- sapply(rep(1,10^5), FUN=function(x) {sum(sample(rep(1:13,4),5)==13)==2}) # 
mean(sim6)

#3)
sim7 <- sapply(rep(1,10^5), FUN=function(x) {sum(sample(rep(1:13,4),5)==13)==2 & sum(sample(rep(1:13,4),5)==12)==2}) # 
mean(sim7)

#4)
paquet <- rep(1:13,4)
tirage = sample(paquet,5)

sim8 <- sapply(rep(1,10^5), FUN=function(x) {tirage <- sample(paquet,5); sum(table(tirage)==4)==1})
# La fonction table crée un tableau avec autant de colonnes dans le tirage et compter combien de fois la valeur apparait
mean(sim8)

#5)
sim9 <- sapply(rep(1,10^5), FUN=function(x) {tirage <- sample(paquet,5); sum(table(tirage)==3)==1 & sum(table(tirage)==2)==1})
# La fonction table crée un tableau avec autant de colonnes dans le tirage et compter combien de fois la valeur apparait
mean(sim9)

# Exercice 3
#1)
anniv <- sample.int(365, 30, replace = TRUE)

simu.anniv.2 <- function(x){
  anniv <- sample.int(365, 30, replace = TRUE)
  sum(table(anniv)>=2)>=1
}
sim10 <- sapply(rep(1,10^4), FUN=simu.anniv.2)
mean(sim10)

