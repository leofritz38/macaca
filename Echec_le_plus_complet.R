# Directionnaries 

#setwd("C:/Users/yangc/Documents/Cours/ASA/projet")
setwd("C:/Users/nitro/Documents/Cours/macaca")

# Packages 

install.packages("readxl")
library(readxl)

library(ade4)
library(vegan)
library("ggplot2")
library("factoextra")
library(corrplot)
library(RVAideMemoire)
library("PerformanceAnalytics")
library(GGally)
library(MASS)

library(dplyr)

# Data 

macaca_activity=read_xlsx("data_Macaca.xlsx",sheet=2)
macaca_food=read_xlsx("data_Macaca.xlsx",sheet=3)

# Leo's exploration ------------------------------------------------------------

summary(macaca_food)
#Mise en forme du tableau pour que les pourcentages de diets ne soient pas considérés comme
#des chaines de caractère (oui j'aurais pu faire plus simple mais je suis un golem)
macaca_food$Tree_bush_natural=as.numeric(macaca_food$Tree_bush_natural)
macaca_food$Cereal=as.numeric(macaca_food$Cereal)
macaca_food$Herba_incrop=as.numeric(macaca_food$Herba_incrop)
macaca_food$Herba_notincrop=as.numeric(macaca_food$Herba_notincrop)
macaca_food$Mushroom=as.numeric(macaca_food$Mushroom)
macaca_food$Animal=as.numeric(macaca_food$Animal)
macaca_food$Nut_intree=as.numeric(macaca_food$Nut_intree)
macaca_food$Cherry_onground=as.numeric(macaca_food$Cherry_onground)
macaca_food$Other_fruit=as.numeric(macaca_food$Other_fruit)
macaca_food$Cherry_intree=as.numeric(macaca_food$Cherry_intree)
macaca_food$Type_site=as.factor(macaca_food$Type_site)
macaca_food$Group=as.factor(macaca_food$Group)
summary(macaca_food)

#Création d'un vecteur time_serie avec en temps 0 le premier jour de collect de donnée?
#formule t=d+sum(day_per_months[m0:m]
m0=min(macaca_food$Month)
day_per_months=c(31,28,31,30,31,30,31,31,30,31,30,31)
t1=c()
for (i in 1:length(macaca_food$Day)){
  t1=c(t1,macaca_food$Day[i]+sum(day_per_months[m0:macaca_food$Month[i]])-day_per_months[m0])
}
t1
macaca_food$relative_time=t1
#On fait tout pareil pour l'activité
summary(macaca_activity)
macaca_activity$Feeding=as.numeric(macaca_activity$Feeding)
macaca_activity$Moving=as.numeric(macaca_activity$Moving)
macaca_activity$Foraging=as.numeric(macaca_activity$Foraging)
macaca_activity$Resting=as.numeric(macaca_activity$Resting)
macaca_activity$Social=as.numeric(macaca_activity$Social)
macaca_activity$Type_site=as.factor(macaca_activity$Type_site)
macaca_activity$Group=as.factor(macaca_activity$Group)
summary(macaca_activity)
m0=min(macaca_activity$Month)
day_per_months=c(31,28,31,30,31,30,31,31,30,31,30,31)
t1=c()
for (i in 1:length(macaca_activity$Day)){
  t1=c(t1,macaca_activity$Day[i]+sum(day_per_months[m0:macaca_activity$Month[i]])-day_per_months[m0])
}
macaca_activity$relative_time=t1
t1

###### GLM binomial
# Here the goal is to see if there is some association between activity/diet and the type of site 
# (agri vs non agri)

# We try to fit a model on a binary variable and go toward a glm binomial with logit activation function
# Since the size of group / group name is directly dependent on the type of site and it makes no sense to see if the month is a group predictor
# for anthropisation we focus directly on the activity / diet variable

#### Starting with activity

### Data exploration

## Data distribution

hist(macaca_activity$Feeding)
hist(macaca_activity$Social)
hist(macaca_activity$Moving)
hist(macaca_activity$Foraging)
hist(macaca_activity$Resting)

# Most of the variable distribution don't seem to be inequal so we will keep all variable as it
# Only foraging distribution might have some outlier but we will rediscuss that later

## Correlation between explenatory variable

library(corrplot)
cormatrix=cor(macaca_activity[,6:10])
corrplot(cormatrix,type="upper",addCoef.col = TRUE)

# Here we can see that only resting and feeding appears to be negativly correlated (-0.63)
# Since there is no direct biological link for those two variable and that -0.63 is not a too high corelation we decide to keep both variable

## Checking if explanatory variable vary a lot between the two Type_site

boxplot(macaca_activity$Feeding~macaca_activity$Type_site)
boxplot(macaca_activity$Social~macaca_activity$Type_site)
boxplot(macaca_activity$Moving~macaca_activity$Type_site)
boxplot(macaca_activity$Foraging~macaca_activity$Type_site)
boxplot(macaca_activity$Resting~macaca_activity$Type_site)

# The distribution don't differe too much depending on the category, nevertheless few outliers appears for Non agri 
# in Social and foraging. We keep that in mind


plot(macaca_activity$Foraging~macaca_activity$Resting,ylab = "Ants Species Richness",xlab="Latitude")

### GLM 0 clock
tab_glm=macaca_activity[,c(1,6:10)]
summary(tab_glm)
## Création du modèle avec méthode de selection backward, on prend l'ensemble des interaction d'ordre 2
modmax=glm(Type_site~.^2, family=binomial("logit"),data=tab_glm)
summary(modmax)

# Ici on observe que les erreurs standards et les coefficient ne paraissent pas comme étant entrain de converger 
# (Valeur absurde). Plusieurs explications possibles à cela, la premère serait une colinéarité parfaite
# Entre deux de nos variables, ce qui n'apparait pas comme étant le cas au vu du correlogramme réalisé
# Plus tôt. La seconde serait la présence de prédicteur dit "presque-parfait", mais au vu de nos boxplots précédent
# il n'apparait pas que cela soit le cas non plus. On observe donc le vif de notre modèle complet
library(car)
vif(modmax)
# On observe ici qu'il existe une multi-colinéarité forte entre nos variables explicatives
# C'est en fait assez logique puisque nous sommes sur des données de composition, et que la somme
# de nos variables explicatives mises ensemble fait toujours 1. Ainsi nous ne pouvons conclure 
# sur ce modèle

## Quelles solutions? 
# Plusieurs transformation de données sont possible, nous allons évoquer la transformation asin(sqrt()) 
# La transformation CLR et la transformation ILR. 
## Transformation asin(sqrt())

asin.sqrt=function(x){
  return(asin(sqrt(x)))
}
tab_glm.asinsqrt=cbind(tab_glm[,1],sapply(tab_glm[,2:6],asin.sqrt))
# On revérifie la distribution des variables
hist(tab_glm.asinsqrt$Feeding)
hist(tab_glm.asinsqrt$Social)
hist(tab_glm.asinsqrt$Moving)
hist(tab_glm.asinsqrt$Foraging)
hist(tab_glm.asinsqrt$Resting)
# On revérifie les corrélation
cormatrix=cor(tab_glm.asinsqrt[,2:6])
corrplot(cormatrix,type="upper",addCoef.col = TRUE)
# On observe que la transformation n'a pas impacté les corrélations entre les variables, une fois encore on conserve l'ensemble des variables
boxplot(tab_glm.asinsqrt$Feeding~macaca_activity$Type_site)
boxplot(tab_glm.asinsqrt$Social~macaca_activity$Type_site)
boxplot(tab_glm.asinsqrt$Moving~macaca_activity$Type_site)
boxplot(tab_glm.asinsqrt$Foraging~macaca_activity$Type_site)
boxplot(tab_glm.asinsqrt$Resting~macaca_activity$Type_site)
# On observe que la transformation a causé l'apparition de nouvelles valeurs abhérantes
modmax=glm(Type_site~., family=binomial("logit"),data=tab_glm.asinsqrt)
summary(modmax)
modmin=glm(Type_site~1,family=binomial("logit"),data=tab_glm.asinsqrt)
vif(modmax)
# On observe ici que les variables Foraging et Social sont significatives.
# Ainsi si la proportion de temps alloué au social augmente relativement par rapport aux autres activité
# la probabilité d'être dans un site anthropisé diminue, de la même façon si la proportion relative 
# de temps passé à chercher de la nouriture augmente on a moins de chance de se trouver dans un site antrhopisé
# Cela ne signifie pas que les singes des sites anthropisé ont plus de temps alloué au social
# mais plutôt que la proportion de temps alloué au social est plus importante relativement aux 
# autres variables explicatives. La conclusion assez général est la suivante, l'allocation 
# du budget de temps varie selon le type de site. Ce modèle ne permet cependant pas d'affirmer 
# comment cette allocation varie, les modèles suivant auront pour objectif de répondre à cette question

# On étudie maintenant le pouvoir explicatif du modèle
nagelkerke(modmax)
(modmax$null.deviance-modmax$deviance)/modmax$null.deviance
# pseudoR2 de 0.09, faible pouvoir explicatif donc prendre les résultats avec des pincettes
resid=residuals(modmax,type="pearson")
# Histogram
hist(resid,col='dodgerblue3',xlab="residuals",main="")
# Quantile-Quantile plot
qqnorm(resid,pch=16,col='dodgerblue3',xlab='')
qqline(resid,col='red',lwd=2)

# residuals vs fitted
plot(resid~fitted(mod1)
     , col='dodgerblue3'
     , pch=16)
abline(h = 0)


plot(resid~tab_glm.asinsqrt$Feeding)
abline(h=0)
plot(resid~tab_glm.asinsqrt$Foraging)
abline(h=0)
plot(resid~tab_glm.asinsqrt$Moving)
abline(h=0)
plot(resid~tab_glm.asinsqrt$Social)
abline(h=0)
plot(resid~tab_glm.asinsqrt$Foraging)
abline(h=0)
# Analyse des observations influentes
par(mfrow = c(1, 1))
plot(cooks.distance(modmax), type = "h", ylim = c(0, 1))
abline(h = 1, col = 2,lwd = 3)
# Beaucoup d'observations donc pas d'individus influents

boxplot(macaca_activity$Foraging~macaca_activity$Month)




#### 
library(compositions)

# Transformer vos données compositionnelles
activites_comp = acomp(tab_glm[,2:6])

# Transformation ILR
activites_ilr = ilr(activites_comp)
activites_ilr= cbind(tab_glm[,1],activites_ilr)
# Modèle sur données transformées
modmax <- glm(Type_site ~ ., family = binomial("logit"),data=activites_ilr)
summary(modmax)
ilrBase(activites_ilr)
# Conclure sur les résultats
#


### Diet
summary(mamacaca_food)













