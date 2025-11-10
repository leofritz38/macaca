# Co inertie
# J'ai décidé de faire une co-inertie entre les données d'activité et le régime
# alimentaire. Ainsi, la question que l'on pourrait
# se poser serait : Y-a-til une structure commune entre le time buget et la diète ? 
# Je vais donc faire deux co-interties ACP / ACP.

# Packages installation --------------------------------------------------------

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

# Data importation -------------------------------------------------------------

macaca_activity = read_xlsx("data_Macaca.xlsx", sheet = 2)
macaca_food = read_xlsx("data_Macaca.xlsx", sheet = 3)

# On transforme les characters en données numériques ---------------------------


summary(macaca_activity)
names(macaca_activity)
colnames = c("Feeding", "Moving", "Foraging", "Resting", "Social")

for (col in colnames) {
  macaca_activity[[col]] = as.numeric(macaca_activity[[col]])
}
summary(macaca_activity)

summary(macaca_food)
names(macaca_food)
colnames = c(
  "Tree_bush_natural",
  "Other_fruit",
  "Cereal",
  "Cherry_intree" ,
  "Cherry_onground",
  "Mushroom" ,
  "Herba_incrop",
  "Herba_notincrop" ,
  "Animal" ,
  "Nut_intree"
)
for (col in colnames) {
  macaca_food[[col]] = as.numeric(macaca_food[[col]])
}
summary(macaca_activity)

# Lavage des données -----------------------------------------------------------
# Il y a des dates d'échantillonnage en trop dans les time_budget, 
# Je fais une clé unique qui permet d'associer un label commun entre les deux tableaux 

# Création d'une clé unique combinant Type_site, Group, Day, Month et Group_size
macaca_activity$id_unique <- paste(
  macaca_activity$Type_site,
  macaca_activity$Group,
  macaca_activity$Day,
  macaca_activity$Month,
  macaca_activity$Group_size,
  sep = "_"
)

macaca_food$id_unique <- paste(
  macaca_food$Type_site,
  macaca_food$Group,
  macaca_food$Day,
  macaca_food$Month,
  macaca_food$Group_size,
  sep = "_"
)

common_ids <- intersect(macaca_activity$id_unique, macaca_food$id_unique)

# Ici je conserve uniquementles données des clés communes 

macaca_activity_sync <- macaca_activity[macaca_activity$id_unique %in% common_ids, ]
macaca_food_sync     <- macaca_food[macaca_food$id_unique %in% common_ids, ]

# On transfrome les données pour les distances euclidiennes --------------------
# Parce que les ACP se font sur la distance euclidienne et nous on est en % 
# donc il faut transformer les données. 
# D'après l'article , pour les % il faut faire une transformation arcsin(racine carré)

vars1 = c("Feeding", "Moving", "Foraging", "Resting", "Social")
vars2 = c(
  "Tree_bush_natural",
  "Other_fruit",
  "Cereal",
  "Cherry_intree" ,
  "Cherry_onground",
  "Mushroom" ,
  "Herba_incrop",
  "Herba_notincrop" ,
  "Animal" ,
  "Nut_intree"
)

# Stockage de  Month séparément (cette étape c'est pour garder le label des mois
# parce qu'après j'aimerai moyenner les données sur les mois)

months_act <- macaca_activity_sync$Month
months_food <- macaca_food_sync$Month

# transforme les colonnes numériques 
macaca_activity_nums <- as.data.frame(apply(macaca_activity_sync[, vars1], 2, function(x)
  asin(sqrt(x))))
macaca_food_nums     <- as.data.frame(apply(macaca_food_sync[, vars2], 2, function(x)
  asin(sqrt(x))))

# recolle Month
macaca_activity_arcsin <- cbind(Month = months_act, macaca_activity_nums)
macaca_food_arcsin     <- cbind(Month = months_food, macaca_food_nums)

# moyenne par mois
# Pareil j'ai fait ça parce que dans son article ils faisaient aussi une 
# moyenne par mois. 

macaca_activity_month <- macaca_activity_arcsin %>%
  group_by(Month) %>%
  summarise(across(all_of(vars1), ~ mean(.x))) %>%
  arrange(Month)

macaca_food_month <- macaca_food_arcsin %>%
  group_by(Month) %>%
  summarise(across(all_of(vars2), ~ mean(.x))) %>%
  arrange(Month)

## Cleaning des variables ------------------------------------------------------
# On retire la valeur du mois parce que ça sert à rien pour la co-inertie

head(macaca_activity_month)
macaca_activity_month <- macaca_activity_month[, -1]
head(macaca_activity_month)

names(macaca_food_month)
macaca_food_month = macaca_food_month[, -1]
head(macaca_food_month)

# Vérifie le résultat
nrow(macaca_activity_month)
nrow(macaca_food_month)

# Corrélation des varaiables ---------------------------------------------------

cor_activity = cor(macaca_activity_month)
cor_diet = cor(macaca_food_month)

library(corrplot)
corrplot(cor(macaca_activity_month), method = "color", tl.cex = 0.8)
corrplot(cor(macaca_food_month), method = "color", tl.cex = 0.8)

# Retire tout ce qui est trop corrélé ------------------------------------------

macaca_food_month = macaca_food_month[, -c(2,5,9)]
macaca_food_month = macaca_food_month[, -c(2)] # J'avais oublié de retirer les céréales

# Co-inertie entre deux ACP ----------------------------------------------------

pca1 <- dudi.pca(macaca_activity_month, scannf = FALSE)   # ACP sur time budget
pca2 <- dudi.pca(macaca_food_month, scannf = FALSE)   # ACP sur food
coi2 <- coinertia(pca2, pca1, scannf = FALSE, nf = 2)  # co-inertie
print(coi2)
# Interprétation :
# RV = 0.76 : mesure la corrélation globale entre les deux jeux de données
# Les deux tableaux ont une forte co-structure

# tests et plots
randtest
par(mfrow = c(1, 1))
rt = randtest(coi2, nrepet = 9999)
rt
plot(rt, main = "Monte-Carlo test (coinertia PCA/PCA)")


# visualisation
scatter(coi2)
plot(coi2)
MVA.synt(coi2)
MVA.plot(coi2, "corr", space = 1)  # cercle de corrélations table 1
MVA.plot(coi2, "corr", space = 2)  # cercle de corrélations table 2
MVA.plot(coi2, space = 1)
MVA.plot(coi2, space = 2)
s.match(coi2$mX, coi2$mY)
