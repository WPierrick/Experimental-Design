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

#3)
anniv <- sample.int(365, 30, replace = TRUE)

simu.anniv.3 <- function(x){
  anniv <- sample.int(365, 30, replace = TRUE)
  sum(table(anniv)>=3)>=1
}
sim11 <- sapply(rep(1,10^4), FUN=simu.anniv.3)
mean(sim11)

#4)
simu.anniv.4 <- function(x){
  anniv <- sample.int(365, 50, replace = TRUE)
  sum(table(anniv)>=3)>=1
}
sim12 <- sapply(rep(1,10^4), FUN=simu.anniv.4)
mean(sim12)

# Exercice 5)

#1)
derive <- function(nA=15, n=30, T){
  evolution <- numeric(T)
  evolution[1] = nA
  i =2
  while (i <= T) {
  evolution[i] <- rhyper(2*evolution[i-1], 2*(n-evolution[i-1]), k=n, nn=1)
  i=i +1
    }
  return (evolution)
}

plot(derive(T=100), type="l", xlab = "Generations", ylab="Fixation" )
# Fonction permettant de simuler t générations successives (pour une réalisation)

#2) 
repr <- sapply(rep(1,10), FUN = function(x) {derive(T=300)})
matplot(repr, type="l", xlab = "Générations", ylab = "Nombre d'individus")
# Evolution du nombre d'individus porteurs de l'allèle A en fonction du temps de génération pour plusieurs réalisations

#3) 
# On peut voir une fixation ou disparition des allèles après environ plusieurs générations(40 à 150-200, variable selons les individus)
# Après 100 générations, une estimation visuelle nous montre que la majorité des allèles se évoluent vers 100% ou 0% (2 groupes)

#4)
derive2 <- function(nA=30, n=60, T){
  evolution <- numeric(T)
  evolution[1] = nA
  i =2
  while (i <= T) {
    evolution[i] <- rhyper(2*evolution[i-1], 2*(n-evolution[i-1]), k=n, nn=1)
    i=i +1
  }
  return (evolution)
}

repr2 <- sapply(rep(1,1000), FUN = function(x) {evolution = derive2(T=50)
  evolution[50]==0
  })
mean(repr2) # On a environ 5% de chances que l'allèle disparaisse après 50 générations


#5)
derive3 <- function(nA, n=60, T){
  evolution <- numeric(T)
  evolution[1] = nA
  i =2
  while (i <= T) {
    evolution[i] <- rhyper(2*evolution[i-1], 2*(n-evolution[i-1]), k=n, nn=1)
    i=i +1
  }
  return (evolution)
}

proba.geneT <- function(n=60, T){
  res = numeric(61)
  i = 0
  while (i <= n) {
    repr3 <- sapply(rep(1,1000), FUN = function(x) { derive3(nA=i, n, T=T)[T]==0 })
    mean(repr3)
    res[i+1]<-mean(repr3)
    i = i+1
  }
  return (res)
  }
x <- proba.geneT(T=50)
plot(x,type="l", xlab = "Population nA", ylab = "Probabilité de disparition")


# La probabilité que l'allèle A disparaisse après 50 générations diminue à mesure que le nA initial augmente

#6)
derive4 <- function(nA=30, n=60, T){
  evolution <- numeric(T)
  evolution[1] = nA
  i =2
  while (i <= T) {
    evolution[i] <- rhyper(2*evolution[i-1], 2*(n-evolution[i-1]), k=n, nn=1)
    i=i +1
  }
  return (evolution)
}

repr4 <- sapply(rep(1,5000), FUN = function(x) {evolution = derive4(T=300)
evolution[300]==0
})
mean(repr4) # On a environ 45% de chances que l'allèle disparaisse après 300 générations
# Plus le nombre de générations augmente, plus on a de chances que l'allèle se fixe ou disparaisse 

#7)
#Nous pouvons utiliser le t-test pour que le théorème de limite central soit valide, en augmentant le nombre de réalisation 
#Car on sait que plus le nombre de réalisation est grand plus on tend vers une loi Normale centrée réduite. 
t.test(repr4)
# 95 percent confidence interval:
# 0.5402476 0.5597524

# On a donc un intervalle de confiance qui borne la moyenne empirique de la probabilité que l'allèle disparaisse
# Entre 0.54 et 0.55, les données de notre intervalle de confiance sont cohérents la moyenne trouvée

#8)

derive5 <- function(nA, n=60, T){
  evolution <- numeric(T)
  evolution[1] = nA
  i =2
  while (i <= T) {
    evolution[i] <- rhyper(2*evolution[i-1], 2*(n-evolution[i-1]), k=n, nn=1)
    i=i +1
  }
  return (evolution)
}

proba.geneT <- function(n=60, T){
  res = numeric(61)
  i = 0
  while (i <= n) {
    repr300 <- sapply(rep(1,500), FUN = function(x) { derive5(nA=i, n, T=T)[T]==0 })
    mean(repr300)
    res[i+1]<-mean(repr300)
    i = i+1
  }
  return (res)
}
x <- proba.geneT(T=300)
plot(x,type="l", xlab = "Population nA", ylab = "Probabilité de disparition")

# Là encore, la probabilité que l'allèle A disparaisse diminue à mesure que le nA initial augmente.
# Cependant, le fait d'augmenter le temps de génération linéarise la baisse de la probabilité de disparitionavec l'augmentation de la population nA.
# Cela s'explique par un plus grand temps de génération.


#9)
derive <- function(nA=15, n=30, T){
  evolution <- numeric(T)
  evolution[1] = nA
  i =2
  while (i <= T) {
    evolution[i] <- rhyper(3*evolution[i-1], 2*(n-evolution[i-1]), k=n, nn=1)
    i=i +1
  }
  return (evolution)
}

repr <- sapply(rep(1,10), FUN = function(x) {derive(T=300)})
matplot(repr, type="l", xlab = "Générations", ylab = "Nombre d'individus")

#9.3) 
# Nous pouvons constater que l'allèle A se fixe très rapidement dans la population après seulement quelques générations (<40)

#9.4)
derive2 <- function(nA=30, n=60, T){
  evolution <- numeric(T)
  evolution[1] = nA
  i =2
  while (i <= T) {
    evolution[i] <- rhyper(3*evolution[i-1], 2*(n-evolution[i-1]), k=n, nn=1)
    i=i +1
  }
  return (evolution)
}

repr2 <- sapply(rep(1,1000), FUN = function(x) {evolution = derive2(T=50)
evolution[50]==0
})
mean(repr2) # On a 0% de chances que l'allèle disparaisse après 50 générations : l'allèle se fixe très rapidement


#9.5)
derive3 <- function(nA, n=60, T){
  evolution <- numeric(T)
  evolution[1] = nA
  i =2
  while (i <= T) {
    evolution[i] <- rhyper(3*evolution[i-1], 2*(n-evolution[i-1]), k=n, nn=1)
    i=i +1
  }
  return (evolution)
}

proba.geneT <- function(n=60, T){
  res = numeric(61)
  i = 0
  while (i <= n) {
    repr3 <- sapply(rep(1,1000), FUN = function(x) { derive3(nA=i, n, T=T)[T]==0 })
    mean(repr3)
    res[i+1]<-mean(repr3)
    i = i+1
  }
  return (res)
}
x <- proba.geneT(T=50)
plot(x,type="l", xlab = "Population nA", ylab = "Probabilité de disparition")

# Après 50 générations, sauf dans les cas où nA est très petit, on a une probabilité de disparition proche de 0%.

#9.6)
derive4 <- function(nA=30, n=60, T){
  evolution <- numeric(T)
  evolution[1] = nA
  i =2
  while (i <= T) {
    evolution[i] <- rhyper(3*evolution[i-1], 2*(n-evolution[i-1]), k=n, nn=1)
    i=i +1
  }
  return (evolution)
}

repr4 <- sapply(rep(1,5000), FUN = function(x) {evolution = derive4(T=300)
evolution[300]==0
})
mean(repr4) # On a environ 0% de chances que l'allèle disparaisse après 300 générations, c'est cohérent avec les résultats précédents (fixation rapide de l'allèle).

#9.7)
t.test(repr4)
# 95 percent confidence interval:
#  NaN NaN

# Ici, la probabilité étant de 0%, l'intervalle de confiance n'est pas applicable. 

#8)

derive5 <- function(nA, n=60, T){
  evolution <- numeric(T)
  evolution[1] = nA
  i =2
  while (i <= T) {
    evolution[i] <- rhyper(3*evolution[i-1], 2*(n-evolution[i-1]), k=n, nn=1)
    i=i +1
  }
  return (evolution)
}

proba.geneT <- function(n=60, T){
  res = numeric(61)
  i = 0
  while (i <= n) {
    repr300 <- sapply(rep(1,500), FUN = function(x) { derive5(nA=i, n, T=T)[T]==0 })
    mean(repr300)
    res[i+1]<-mean(repr300)
    i = i+1
  }
  return (res)
}
x <- proba.geneT(T=300)
plot(x,type="l", xlab = "Population nA", ylab = "Probabilité de disparition")

# Là encore, on a une probabilité de disparition proche de 0%.


