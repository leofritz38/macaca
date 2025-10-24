#setwd("C:/Users/yangc/Documents/Cours/ASA/projet")
setwd("C:/Users/lylym/git/macaca")
install.packages("readxl")
library(readxl)
macaca_activity=read_xlsx("data_Macaca.xlsx",sheet=2)
macaca_food=read_xlsx("data_Macaca.xlsx",sheet=3)
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
#Quels questionnements? 
# Si glm - variables réponses éventuelles, hypothèses biologique derrière



# Si multivarié - hypothèse biologique éventuelles?
# On pourrait potentiellement partir sur : Is there a link between the time budget 
# and the food composition ? 

##### Exploration des données ~ activité :
#Distribution
par(mfrow=c(3,2))
hist(macaca_activity$Feeding)
hist(macaca_activity$Moving)
hist(macaca_activity$Foraging)
hist(macaca_activity$Resting)
hist(macaca_activity$Social)
#Représentation des classes 
par(mfrow=c(1,1))
plot(macaca_activity$Group)
plot(macaca_activity$Type_site)
#Variabilité des réponses celon les groupes

par(mfrow=c(3,2))
boxplot(macaca_activity$Feeding~macaca_activity$Group,label="Feeding",ylab="proportion feeding time",xlab="Group")
boxplot(macaca_activity$Moving~macaca_activity$Group,label="Moving",ylab="proportion moving time",xlab="Group")
boxplot(macaca_activity$Foraging~macaca_activity$Group,label="Foraging",ylab="proportion foraging time",xlab="Group")
boxplot(macaca_activity$Resting~macaca_activity$Group,label="Resting",ylab="proportion resting time",xlab="Group")
boxplot(macaca_activity$Social~macaca_activity$Group,label="Social",ylab="proportion social time",xlab="Group")


#Variabilité des réponses celon les milieux

par(mfrow=c(3,2))
boxplot(macaca_activity$Feeding~macaca_activity$Type_site,label="Feeding",ylab="proportion feeding time",xlab="type")
boxplot(macaca_activity$Moving~macaca_activity$Type_site,label="Moving",ylab="proportion moving time",xlab="type")
boxplot(macaca_activity$Foraging~macaca_activity$Type_site,label="Foraging",ylab="proportion foraging time",xlab="type")
boxplot(macaca_activity$Resting~macaca_activity$Type_site,label="Resting",ylab="proportion resting time",xlab="type")
boxplot(macaca_activity$Social~macaca_activity$Type_site,label="Social",ylab="proportion social time",xlab="type")

################################# CO INTERTIE ##################################
## Cointertia test 
# Création d'une clé unique combinant Type_site, Group, Day, Month et Group_size
macaca_activity$id_unique <- paste(macaca_activity$Type_site, macaca_activity$Group, macaca_activity$Day,
                                   macaca_activity$Month,macaca_activity$Group_size,sep = "_")

macaca_food$id_unique <- paste(macaca_food$Type_site, macaca_food$Group, macaca_food$Day,
                               macaca_food$Month, macaca_food$Group_size, sep = "_")

common_ids <- intersect(macaca_activity$id_unique, macaca_food$id_unique)

macaca_activity_sync <- macaca_activity[macaca_activity$id_unique %in% common_ids, ]
macaca_food_sync     <- macaca_food[macaca_food$id_unique %in% common_ids, ]

## Cleaning des variables (oui j'aurai pu le faire d'un coup mis j'ai eu la flemme quand j'ai vu que j'avais oublié des colonnes)
head(macaca_activity_sync)
macaca_activity_sync <- macaca_activity_sync[, -c(1:5)]
head(macaca_activity_sync)
macaca_activity_sync <- macaca_activity_sync[-6]
head(macaca_activity_sync)

names(macaca_food_sync)
macaca_food_sync = macaca_food_sync[, -c(1:4, 16)]
names(macaca_food_sync)
macaca_food_sync = macaca_food_sync[-11]
names(macaca_food_sync)

# Vérifie le résultat
nrow(macaca_activity_sync)
nrow(macaca_food_sync)

# packages nécessaires (tirés du code de Pannard)
library(ade4)
library(vegan)
library("ggplot2")
library("factoextra")
library(corrplot)
library(RVAideMemoire)
library("PerformanceAnalytics")
library(GGally)
library(MASS)

# Co-inertie entre deux ACP 
pca1 <- dudi.pca(macaca_activity_sync, scannf = FALSE)   # ACP sur time budget
pca2 <- dudi.pca(macaca_food_sync,  scannf = FALSE)   # ACP sur food
coi2 <- coinertia(pca2, pca1, scannf = FALSE, nf = 2)  # co-inertie 
print(coi2)

# tests et plots
randtest
par(mfrow = c(1, 1))
plot(randtest(coi2), main = "Monte-Carlo test (coinertia PCA/PCA)")

# visualisation
scatter(coi2)
plot(coi2)
MVA.synt(coi2)
MVA.plot(coi2, "corr", space = 1)  # cercle de corrélations table 1
MVA.plot(coi2, "corr", space = 2)  # cercle de corrélations table 2
MVA.plot(coi2, space = 1)
MVA.plot(coi2, space = 2)
s.match(coi2$mX, coi2$mY)








#################################
# Visualisation de la diet par type de site
#################################

library(ggplot2)
library(reshape2)
library(dplyr)

# On garde uniquement les colonnes utiles
food_data <- macaca_food %>%
  select(Type_site, Month,
         Tree_bush_natural, Cereal, Herba_incrop, Herba_notincrop, 
         Mushroom, Animal, Nut_intree, Cherry_onground, Other_fruit, Cherry_intree)

# Passage en format long (pour ggplot)
food_long <- melt(food_data, 
                  id.vars = c("Type_site", "Month"), 
                  variable.name = "Food_item", 
                  value.name = "Percentage")

# Moyenne par mois et type de site
food_summary <- food_long %>%
  group_by(Type_site, Month, Food_item) %>%
  summarise(Mean_perc = mean(Percentage, na.rm = TRUE)) %>%
  ungroup()

# changement des noms pour le graphique
food_summary$Food_item <- factor(food_summary$Food_item,
                                 levels = c("Cherry_intree", "Cherry_onground", "Other_fruit", "Cereal", "Nut_intree",
                                            "Herba_incrop", "Herba_notincrop", "Tree_bush_natural", "Mushroom", "Animal"),
                                 labels = c("Cherry in tree", "Cherry on ground", "Other fruit", "Wheat", "Walnut",
                                            "Herbaceous in crops", "Herbaceous in forest", 
                                            "Forest shrub & tree", "Lichen & mushroom", "Insect"))


# On trace le graphique
ggplot(food_summary, aes(x = factor(Month), y = Mean_perc, fill = Food_item)) +
  geom_bar(stat = "identity", position = "fill", colour = "black") +
  facet_wrap(~Type_site, ncol = 2, labeller = labeller(Type_site = c(
    "Agri" = "Agricultural sites",
    "Non-agri" = "Natural sites"
  ))) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Month", y = "Percentage of diet", fill = "Food category") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "Cherry in tree" = "#b2182b",
    "Cherry on ground" = "#ef8a62",
    "Other fruit" = "#fddbc7",
    "Wheat" = "#f7f7f7",
    "Walnut" = "#d1e5f0",
    "Herbaceous in crops" = "#67a9cf",
    "Herbaceous in forest" = "#1a9641",
    "Forest shrub & tree" = "#a6d96a",
    "Lichen & mushroom" = "grey70",
    "Insect" = "black"
  )) +
  labs(x = "Month", y = "Percentage of diet", fill = "Food category") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.position = "right",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10))
