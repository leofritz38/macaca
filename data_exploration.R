setwd("C:/Users/yangc/Documents/Cours/ASA/projet")
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
