# Etude de l'autocorrélation temporelle 

# Working directory ------------------------------------------------------------

setwd("C:/Users/lylym/git/macaca")

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

# Formatage des données --------------------------------------------------------
# Transformation des characters en valeurs numériques 
# Etape a skip si on importe les données avec un read.table, il suffit de mettre dec = ",". 

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

# Creation des series temporelles ----------------------------------------------

# Création d'un vecteur time_serie avec en temps 0 le premier jour de collecte

# Pour la food
#formule t=d+sum(day_per_months[m0:m]
m0=min(macaca_food$Month)
day_per_months=c(31,28,31,30,31,30,31,31,30,31,30,31)
t1=c()
for (i in 1:length(macaca_food$Day)){
  t1=c(t1,macaca_food$Day[i]+sum(day_per_months[m0:macaca_food$Month[i]])-day_per_months[m0])
}
macaca_food$relative_time=t1

# Pareil pour l'activité 

m0=min(macaca_activity$Month)
day_per_months=c(31,28,31,30,31,30,31,31,30,31,30,31)
t1=c()
for (i in 1:length(macaca_activity$Day)){
  t1=c(t1,macaca_activity$Day[i]+sum(day_per_months[m0:macaca_activity$Month[i]])-day_per_months[m0])
}
macaca_activity$relative_time=t1

# On met les données dans l'ordre croissant 
macaca_food = macaca_food %>% arrange(macaca_food$relative_time)
macaca_activity = macaca_activity %>% arrange(macaca_activity$relative_time)

# On regroupe en semaines ? 
macaca_food$week <- floor(macaca_food$relative_time / 7)
# Créer une colonne week qui dit a quelle semaine appartient chaque observation 
# Ensuite on créer un tableau avec les groupes demaine et la moyenne de chaque groupe

macaca_food_week <- macaca_food %>% group_by(week) %>%
  summarise( Three_bush_natural = mean(Tree_bush_natural), 
             Other_fruit = mean(Other_fruit),
             Cereal = mean(Cereal), 
             Cherry_intree = mean(Cherry_intree), 
             Cherry_onground = mean(Cherry_onground), 
             Mushroom = mean(Mushroom), 
             Herba_incrop = mean(Herba_incrop), 
             Herba_notincrop = mean(Herba_notincrop), 
             Animal = mean(Animal), 
             Nut_intree= mean(Nut_intree))

# Tentative de plot les series temporelles : 
# On prépare la fenêtre de graphiques : 2 lignes, 1 colonne
# 5 lignes × 2 colonnes = 10 graphiques
par(mfrow=c(3,2), mar=c(4,4,2,1), oma=c(2,2,2,2))
plot(macaca_food_week$week, macaca_food_week$Three_bush_natural, type="b",
     xlab="Week", ylab="Three_bush_natural", col="forestgreen", pch=16)
plot(macaca_food_week$week, macaca_food_week$Other_fruit, type="b",
     xlab="Week", ylab="Other_fruit", col="orange", pch=16)
plot(macaca_food_week$week, macaca_food_week$Cereal, type="b",
     xlab="Week", ylab="Cereal", col="pink", pch=16)
plot(macaca_food_week$week, macaca_food_week$Cherry_intree, type="b",
     xlab="Week", ylab="Other_fruit", col="red", pch=16)
plot(macaca_food_week$week, macaca_food_week$Cherry_onground, type="b",
     xlab="Week", ylab="Other_fruit", col="lightblue", pch=16)
plot(macaca_food_week$week, macaca_food_week$Mushroom, type="b",
     xlab="Week", ylab="Mushroom", col="brown", pch=16)

par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(2,2,2,2))
plot(macaca_food_week$week, macaca_food_week$Herba_incrop, type="b",
     xlab="Week", ylab="Other_fruit", col="violet", pch=16)
plot(macaca_food_week$week, macaca_food_week$Herba_notincrop, type="b",
     xlab="Week", ylab="Other_fruit", col="green", pch=16)
plot(macaca_food_week$week, macaca_food_week$Animal, type="b",
     xlab="Week", ylab="Other_fruit", col="gold", pch=16)
plot(macaca_food_week$week, macaca_food_week$Nut_intree, type="b",
     xlab="Week", ylab="Other_fruit", col="cyan", pch=16)

# Faire pareil pour activity 
macaca_activity$week <- floor(macaca_activity$relative_time / 7)
macaca_activity_week <- macaca_activity %>% group_by(week) %>% 
  summarise( Feeding = mean(Feeding), 
             Moving = mean(Moving), 
             Foraging = mean(Foraging), 
             Resting = mean(Resting), 
             Social = mean(Social))
par(mfrow=c(3,2), mar=c(4,4,2,1), oma=c(2,2,2,2))
plot(macaca_activity_week$week, macaca_activity_week$Feeding, type="b",
     xlab="Week", ylab="Feeding", col="forestgreen", pch=16)
plot(macaca_activity_week$week, macaca_activity_week$Moving, type="b",
     xlab="Week", ylab="Moving", col="orange", pch=16)
plot(macaca_activity_week$week, macaca_activity_week$Foraging, type="b",
     xlab="Week", ylab="Foraging", col="pink", pch=16)
plot(macaca_activity_week$week, macaca_activity_week$Resting, type="b",
     xlab="Week", ylab="Resting", col="red", pch=16)
plot(macaca_activity_week$week, macaca_activity_week$Social, type="b",
     xlab="Week", ylab="Social", col="lightblue", pch=16)

# ACF 

# Pour l'activité 
par(mfrow=c(1,1),  mar=c(4,4,4,1), oma=c(2,2,2,2))
acf(macaca_activity_week$Feeding, main="Feeding")    # rien 
acf(macaca_activity_week$Moving, main="Moving")      # rien
acf(macaca_activity_week$Foraging, main="Foraging")  # lag 1 
acf(macaca_activity_week$Resting, main="Resting")    # rien 
acf(macaca_activity_week$Social, main="Social")      # lag 2 

# Pour la food 
acf(macaca_food_week$Three_bush_natural, main="Three_bush_natural")  # lag 2
acf(macaca_food_week$Other_fruit, main="Other_fruit")                # rien
acf(macaca_food_week$Cereal, main = "Cereal")                        # on tient un truc
acf(macaca_food_week$Cherry_intree, main="Cherry intree")            # log 3
acf(macaca_food_week$Cherry_onground, main="Cherry Onground")        # positif et négatif
acf(macaca_food_week$Mushroom, main="Mushroom")                      # rien
acf(macaca_food_week$Herba_incrop, main="Herba incrop")              # laf 1 
acf(macaca_food_week$Herba_notincrop, main="Herba notincrop")        # pos and neg
acf(macaca_food_week$Animal, main="Animal")                          # rien 
acf(macaca_food_week$Nut_intree, main="Nut intree")                  # rien

# Ensuite construire des modèles linéaires ou Arima sur les séries temporelles 
# qui sont indépendantes ou présentent une périodicité. 

# Refaire pareil mais pas pas semaines, par mois. 
# Il faut tout reprendre avant le traitement des données. 

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

# Regroupement par mois

macaca_food_month <- macaca_food %>% 
  group_by(Month) %>%     
  summarise(
    Tree_bush_natural = mean(Tree_bush_natural),
    Other_fruit       = mean(Other_fruit),
    Cereal            = mean(Cereal),
    Cherry_intree     = mean(Cherry_intree),
    Cherry_onground   = mean(Cherry_onground),
    Mushroom          = mean(Mushroom),
    Herba_incrop      = mean(Herba_incrop),
    Herba_notincrop   = mean(Herba_notincrop),
    Animal            = mean(Animal),
    Nut_intree        = mean(Nut_intree)
  ) %>%
  arrange(Month)
  
par(mfrow=c(3,2), mar=c(4,4,2,1), oma=c(2,2,2,2))
plot(macaca_food_month$Month, macaca_food_month$Tree_bush_natural, type="b",
       xlab="Week", ylab="Tree_bush_natural", col="forestgreen", pch=16)
plot(macaca_food_month$Month, macaca_food_month$Other_fruit, type="b",
       xlab="Week", ylab="Other_fruit", col="orange", pch=16)
plot(macaca_food_month$Month, macaca_food_month$Cereal, type="b",
       xlab="Week", ylab="Cereal", col="pink", pch=16)
plot(macaca_food_month$Month, macaca_food_month$Cherry_intree, type="b",
       xlab="Week", ylab="Cherry intree", col="red", pch=16)
plot(macaca_food_month$Month, macaca_food_month$Cherry_onground, type="b",
       xlab="Week", ylab="Cherry onground", col="lightblue", pch=16)
plot(macaca_food_month$Month, macaca_food_month$Mushroom, type="b",
       xlab="Week", ylab="Mushroom", col="brown", pch=16)
  
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(2,2,2,2))
plot(macaca_food_month$Month, macaca_food_month$Herba_incrop, type="b",
       xlab="Week", ylab="Herba incrop", col="violet", pch=16)
plot(macaca_food_month$Month, macaca_food_month$Herba_notincrop, type="b",
       xlab="Week", ylab="Herba not incrop", col="green", pch=16)
plot(macaca_food_month$Month, macaca_food_month$Animal, type="b",
       xlab="Week", ylab="Animal", col="gold", pch=16)
plot(macaca_food_month$Month, macaca_food_month$Nut_intree, type="b",
       xlab="Week", ylab="Nut in tree", col="cyan", pch=16)

# Pour la food 
par(mfrow=c(1,1),  mar=c(4,4,4,1), oma=c(2,2,2,2))
acf(macaca_food_month$Tree_bush_natural, main="Three_bush_natural")  
acf(macaca_food_month$Other_fruit, main="Other_fruit")                
acf(macaca_food_month$Cereal, main = "Cereal")                        
acf(macaca_food_month$Cherry_intree, main="Cherry intree")            
acf(macaca_food_month$Cherry_onground, main="Cherry Onground")        
acf(macaca_food_month$Mushroom, main="Mushroom")                      
acf(macaca_food_month$Herba_incrop, main="Herba incrop")              
acf(macaca_food_month$Herba_notincrop, main="Herba notincrop")        
acf(macaca_food_month$Animal, main="Animal")                         
acf(macaca_food_month$Nut_intree, main="Nut intree")                 
# Absolument rien 
  
macaca_activity_month <- macaca_activity %>% group_by(Month) %>% 
    summarise( Feeding = mean(Feeding), 
               Moving = mean(Moving), 
               Foraging = mean(Foraging), 
               Resting = mean(Resting), 
               Social = mean(Social)) %>%
  arrange(Month)

par(mfrow=c(3,2), mar=c(4,4,2,1), oma=c(2,2,2,2))
plot(macaca_activity_month$Month, macaca_activity_month$Feeding, type="b",
     xlab="Week", ylab="Feeding", col="forestgreen", pch=16)
plot(macaca_activity_month$Month, macaca_activity_month$Moving, type="b",
     xlab="Week", ylab="Moving", col="orange", pch=16)
plot(macaca_activity_month$Month, macaca_activity_month$Foraging, type="b",
     xlab="Week", ylab="Foraging", col="pink", pch=16)
plot(macaca_activity_month$Month, macaca_activity_month$Resting, type="b",
     xlab="Week", ylab="Resting", col="red", pch=16)
plot(macaca_activity_month$Month, macaca_activity_month$Social, type="b",
     xlab="Week", ylab="Social", col="lightblue", pch=16)

# ACF 

# Pour l'activité 
par(mfrow=c(1,1),  mar=c(4,4,4,1), oma=c(2,2,2,2))
acf(macaca_activity_month$Feeding, main="Feeding")     
acf(macaca_activity_month$Moving, main="Moving")      
acf(macaca_activity_month$Foraging, main="Foraging")  
acf(macaca_activity_month$Resting, main="Resting")    
acf(macaca_activity_month$Social, main="Social")       
