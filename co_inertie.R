# Co inertie 
# J'ai décidé de faire une co-inertie entre les données d'activité et le régime
# alimentair en fonction du groupe agricole. Ainsi, la question que l'on pourrait 
# se poser serait : Y-a-til un lien entre le time buget et le type de régime 
# alimentaire ? et faire égalementune étude comparative entre les deux types de 
# site : agricole et non agricole. 
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

macaca_activity=read_xlsx("data_Macaca.xlsx",sheet=2)
macaca_food=read_xlsx("data_Macaca.xlsx",sheet=3)


summary(macaca_activity)
names(macaca_activity)
colnames = c("Feeding", "Moving", "Foraging", "Resting", "Social")

for (col in colnames){
  macaca_activity[[col]] = as.numeric(macaca_activity[[col]])
}
summary(macaca_activity)

summary(macaca_food)
names(macaca_food)
colnames = c("Tree_bush_natural", "Other_fruit", "Cereal", "Cherry_intree" , "Cherry_onground",
             "Mushroom" , "Herba_incrop", "Herba_notincrop" , "Animal" , "Nut_intree")
for (col in colnames){
  macaca_food[[col]] = as.numeric(macaca_food[[col]])
}
summary(macaca_activity)

# Lavage des données -----------------------------------------------------------

# Création d'une clé unique combinant Type_site, Group, Day, Month et Group_size
macaca_activity$id_unique <- paste(macaca_activity$Type_site, macaca_activity$Group, macaca_activity$Day,
                                   macaca_activity$Month,macaca_activity$Group_size,sep = "_")

macaca_food$id_unique <- paste(macaca_food$Type_site, macaca_food$Group, macaca_food$Day,
                               macaca_food$Month, macaca_food$Group_size, sep = "_")

common_ids <- intersect(macaca_activity$id_unique, macaca_food$id_unique)

macaca_activity_sync <- macaca_activity[macaca_activity$id_unique %in% common_ids, ]
macaca_food_sync     <- macaca_food[macaca_food$id_unique %in% common_ids, ]

# On transfrome les données pour les distances euclidiennes --------------------
vars1 = c("Feeding", "Moving", "Foraging", "Resting", "Social")
vars2 = c("Tree_bush_natural", "Other_fruit", "Cereal", "Cherry_intree" , "Cherry_onground",
                     "Mushroom" , "Herba_incrop", "Herba_notincrop" , "Animal" , "Nut_intree")

# Extrait Month séparément
months_act <- macaca_activity_sync$Month
months_food <- macaca_food_sync$Month

# transforme uniquement les colonnes numériques (sans enlever Month)
macaca_activity_nums <- as.data.frame(apply(macaca_activity_sync[, vars1], 2, function(x) asin(sqrt(x))))
macaca_food_nums     <- as.data.frame(apply(macaca_food_sync[, vars2], 2, function(x) asin(sqrt(x))))

# recolle Month
macaca_activity_arcsin <- cbind(Month = months_act, macaca_activity_nums)
macaca_food_arcsin     <- cbind(Month = months_food, macaca_food_nums)

# moyenne par mois
macaca_activity_month <- macaca_activity_arcsin %>%
  group_by(Month) %>%
  summarise(across(all_of(vars1), ~ mean(.x, na.rm = TRUE))) %>%
  arrange(Month)

macaca_food_month <- macaca_food_arcsin %>%
  group_by(Month) %>%
  summarise(across(all_of(vars2), ~ mean(.x, na.rm = TRUE))) %>%
  arrange(Month)

## Cleaning des variables ------------------------------------------------------

head(macaca_activity_month)
macaca_activity_month <- macaca_activity_month[, -1]
head(macaca_activity_month)

names(macaca_food_month)
macaca_food_month = macaca_food_month[, -1]
head(macaca_food_month)

# Vérifie le résultat
nrow(macaca_activity_month)
nrow(macaca_food_month)

# Co-inertie entre deux ACP ----------------------------------------------------

pca1 <- dudi.pca(macaca_activity_month, scannf = FALSE)   # ACP sur time budget
pca2 <- dudi.pca(macaca_food_month,  scannf = FALSE)   # ACP sur food
coi2 <- coinertia(pca2, pca1, scannf = FALSE, nf = 2)  # co-inertie 
print(coi2)
# Interprétation : 
# RV = 0.76 : mesure la corrélation globale entre les deux jeux de données
# Les deux tableaux ont une forte co-structure

# tests et plots
randtest
par(mfrow = c(1, 1))
plot(randtest(coi2), main = "Monte-Carlo test (coinertia PCA/PCA)")
randtest(coi2)

# visualisation
scatter(coi2)
plot(coi2)
MVA.synt(coi2)
MVA.plot(coi2, "corr", space = 1)  # cercle de corrélations table 1
MVA.plot(coi2, "corr", space = 2)  # cercle de corrélations table 2
MVA.plot(coi2, space = 1)
MVA.plot(coi2, space = 2)
s.match(coi2$mX, coi2$mY)
