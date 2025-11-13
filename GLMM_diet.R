### GLM pour les variables de diet
library(ggplot2) # graph package
library(tinytex) # Pour la sortie pdf
library(corrplot)# Correlation matrix calculus
library(plot3D)# For 3D plot
library(DHARMa)# Model diagnosis
#library(rcompanion)# Model pseudo R²
library(lattice)# multipanel graphics
library(nlme)# Mixed models
library(predictmeans)#Cook distance in mixed models
library(readxl)
library(emmeans)  # pour les tests post-hoc
library(knitr)
macaca_activity=read_xlsx("data_Macaca.xlsx",sheet=2)
macaca_food=read_xlsx("data_Macaca.xlsx",sheet=3)
# Vérification de la présence de valeurs manquantes
colSums(is.na(macaca_activity))
colSums(is.na(macaca_food))
# Aucune valeur manquante


summary(macaca_food)
summary(macaca_activity)
#Mise en forme du tableau pour que les pourcentages de diets ne soient pas considérés comme
#des chaines de caractère (oui j'aurais pu faire plus simple mais je suis un golem)
# Conversion des variables du data frame macaca_food
num_vars_food <- c("Tree_bush_natural", "Cereal", "Herba_incrop", "Herba_notincrop",
                   "Mushroom", "Animal", "Nut_intree", "Cherry_onground",
                   "Other_fruit", "Cherry_intree")

fact_vars_food <- c("Type_site", "Group", "Month")

macaca_food[num_vars_food] <- lapply(macaca_food[num_vars_food], as.numeric)
macaca_food[fact_vars_food] <- lapply(macaca_food[fact_vars_food], as.factor)

summary(macaca_food)

# Conversion des variables du data frame macaca_activity
num_vars_activity <- c("Feeding", "Group_size", "Moving", "Foraging", "Resting", "Social")
fact_vars_activity <- c("Type_site", "Group", "Month")

macaca_activity[num_vars_activity] <- lapply(macaca_activity[num_vars_activity], as.numeric)
macaca_activity[fact_vars_activity] <- lapply(macaca_activity[fact_vars_activity], as.factor)

summary(macaca_activity)
summary(macaca_food)
### Transformation des données pour les glmm beta
perturb=function(x){
  if (x==1){return(x-10**-6)}
  if (x==0){return(x+10**-6)}
  if (x!=0 & x!=1){return(x)}
}
food_variable=colnames(macaca_food)[5:14]
for(i in food_variable){
macaca_food[[i]]=sapply(macaca_food[[i]],perturb)}
prop_zeros=data.frame("Agri"=numeric,"Non_agri"=numeric)
for (i in food_variable){
  prop_zeros_var <- tapply(macaca_food[[i]] == 0, macaca_food$Type_site, mean)
  prop_zeros=rbind(prop_zeros,prop_zeros_var)
}
row.names(prop_zeros)=food_variable
colnames(prop_zeros)=c("Agri","Non_agri")
prop_zeros
# Ici on observe que seul les variables Tree_bush_natural, Herba_notincrop et Animal sont consommées dans les deux sites
# Puisque la question biologique est : quelle est l'influence de l'anthropisation sur la diete
# on ne peut intégrer des prédicteurs parfait au sein du modèle. Ainsi on réalisera uniquement des
# modèles sur nos 3 variables consommées dans les deux groupes




### Variable Treebush natural
# Boxplot
boxplot(macaca_food$Tree_bush_natural, col='lightgreen', ylab=var)

# Cleveland plot
dotchart(macaca_food$Tree_bush_natural, pch=16, col='lightgreen', xlab=var)

# Histogram
hist(macaca_food$Tree_bush_natural, col='lightgreen', xlab=var, main="")

# Quantile-Quantile plot
qqnorm(macaca_food$Tree_bush_natural, pch=16, col='lightgreen', xlab='')
qqline(macaca_food$Tree_bush_natural, col='red')

# On observe ici que notre distribution ne présente pas de trop nombreux outlier, il n'y a pas de cassure dans la distribution
# La distribution de la variable s'apparente à une loi beta (entre 0 et 1)
# A priori on se dirige vers un glm beta

par(mfrow=c(2,2))
# Boxplot
boxplot(macaca_food$Group_size,col='lightgreen')
# Cleveland plot
dotchart(macaca_food$Group_size,pch=16,col='lightgreen')
# Histogram
hist(macaca_food$Group_size,col='lightgreen')
# Quantile-Quantile plot
qqnorm(macaca_food$Group_size,pch=16,col='lightgreen',xlab='')
qqline(macaca_food$Group_size,col='red')

# ????


# Factor site
summary(macaca_food$Type_site)
# Factor month
summary(macaca_food$Month)

# Factor site
summary(macaca_food$Type_site)


# A priori il n'y a pas de groupe sous représenter
par(mfrow=c(1,1))
boxplot(macaca_food$Tree_bush_natural~macaca_food$Type_site)
# L'on peut à priori s'attendre à ce que l'anthropisation influence la consommation de buisson naturel
boxplot(macaca_food$Tree_bush_natural~macaca_food$Month)
# Il semble avoir des différences dans la consommation de natural tree bush celon les mois
boxplot(macaca_food$Tree_bush_natural~macaca_food$Group)
# La consommation semble aussi varier selon les groupes
plot(macaca_food$Tree_bush_natural~macaca_food$Group_size)
### Si la variable mois nous est d'intérêt puisqu'on pourrait dicerner des variations de diets celon les mois
### mais aussi celon les mois et les types de site (intéraction), les groupes n'apportent pas d'information biologique
### Il faudra ainsi tester l'effet aléatoire de la variable groupe afin de voir si la variance inter-groupe ne serait pas trop fortement porteuse d'information


# Transformation des données pour lois béta
perturb=function(x){
  if (x==1){return(x-10**-6)}
  if (x==0){return(x+10**-6)}
  if (x!=0 & x!=1){return(x)}
}
food_variable=colnames(macaca_food)[5:14]
for(i in food_variable){
  macaca_food[[i]]=sapply(macaca_food[[i]],perturb)}
mod=glmmTMB(Tree_bush_natural~Month+Type_site+Month:Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)
summary(mod)
# On observe ici que le type de site semble être impactant sur la consommation de tree_bush_natural
# On peut toutefois réaliser des test bootstrap sur l'ensemble de nos variables afin d'en évaluer 
# La pertinence dans le modèle

nBoot <- 1000
lrStat <- rep(NA, nBoot)
p_val_treebush=numeric(0)
var_exp=c("Group","Group_size","Month","Type_site","interaction")
for (i in var_exp){
nBoot=10
mod_alt=glmmTMB(Tree_bush_natural~Month+Type_site+Month:Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)
if (i=="Group"){mod_null=glmmTMB(Tree_bush_natural~Month+Type_site+Month:Type_site+Group_size,family="beta_family",data=macaca_food)}
if (i=="Group_size"){mod_null=glmmTMB(Tree_bush_natural~Month+Type_site+Month:Type_site+(1|Group),family="beta_family",data=macaca_food)}
if (i=="Month"){mod_null=glmmTMB(Tree_bush_natural~Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)}
if (i=="Type_site"){mod_null=glmmTMB(Tree_bush_natural~Month+Group_size+(1|Group),family="beta_family",data=macaca_food)}
if (i=="interaction"){mod_null=glmmTMB(Tree_bush_natural~Month+Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)}
lrObs <- 2 * logLik(mod_alt) - 2 * logLik(mod_null) # observed test stat
lrStat <- rep(NA, nBoot)
for (iBoot in 1:nBoot)
{
  macaca_food$NTBSim <- unlist(simulate(mod_null))
  macaca_food$NTBSim <- sapply(macaca_food$NTBSim,perturb)# resampled data
  bAlt <-glmmTMB(NTBSim ~ Month * Type_site + Group_size + (1 | Group),
                  data = macaca_food,
                  family = beta_family(link = "logit"))
  # null model
  if (i=="Group"){bNull=glmmTMB(NTBSim~Month+Type_site+Month:Type_site+Group_size,family="beta_family",data=macaca_food)}
  if (i=="Group_size"){bNull=glmmTMB(NTBSim~Month+Type_site+Month:Type_site+(1|Group),family="beta_family",data=macaca_food)}
  if (i=="Month"){bNull=glmmTMB(NTBSim~Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)}
  if (i=="Type_site"){bNull=glmmTMB(NTBSim~Month+Group_size+(1|Group),family="beta_family",data=macaca_food)}
  if (i=="interaction"){bNull=glmmTMB(NTBSim~Month+Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)}
  lrStat[iBoot] <- 2 * logLik(bAlt) - 2 * logLik(bNull) # resampled test stat
}

print(lrStat)

print(lrObs)

mean(lrStat > lrObs) # P-value for test of the random effect
p_val_treebush=c(p_val_treebush,mean(lrStat > lrObs))
print(mean(lrStat > lrObs))
print(lrObs)
png(paste0("bootstrap",i,".png"), width=800, height=600)
hist(lrStat, col='dodgerblue3',main=i) # Histogram of the 1000 values of likelihood of the simulated model
abline(v = lrObs, col="darkred", lwd=3, lty=2) # Vertical red line representing the likelihood of the observed model including the random factor
dev.off() 
}
p_val_treebush=c("0.789","0.141",">0.001",">0.001",">0.001")
### Donc les variables type_site, month et leur interaction sont fortement significatif pour le modèle
### On peut maintenant discuter des coefficients du modèle (NIK L4AUTOCOR DES RES J4AI PAS LE TEMPS JE SUIS FATIGU2)
summary(mod)
# Les macaques consomment moins de Natural_tree_bush dans les sites non anthropisé
# La consommation des macaques baissent durant les mois 7 8 et 11
# Cependant la baisse de consomation est moindre durant les mois 7 à 11 pour les sites non anthropisé
# En résumé la consommation de Natural tree bush à tendance à être moindre sur les sites anthropisé mais reste plus constante au cours des saisons
# par rapport au site anthropisé où leur consommation varie plus au cours des saisons

##### Rebelote autre variable je suis fatigué purée
### Variable Treebush natural
# Boxplot
boxplot(macaca_food$Herba_notincrop, col='lightgreen', ylab=var)

# Cleveland plot
dotchart(macaca_food$Herba_notincrop, pch=16, col='lightgreen', xlab=var)

# Histogram
hist(macaca_food$Herba_notincrop, col='lightgreen', xlab=var, main="")

# Quantile-Quantile plot
qqnorm(macaca_food$Herba_notincrop, pch=16, col='lightgreen', xlab='')
qqline(macaca_food$Herba_notincrop, col='red')

# On observe ici que notre distribution ne présente pas de trop nombreux outlier, il n'y a pas de cassure dans la distribution
# La distribution de la variable s'apparente à une loi beta (entre 0 et 1)
# A priori on se dirige vers un glm beta

par(mfrow=c(2,2))
# Boxplot
boxplot(macaca_food$Group_size,col='lightgreen')
# Cleveland plot
dotchart(macaca_food$Group_size,pch=16,col='lightgreen')
# Histogram
hist(macaca_food$Group_size,col='lightgreen')
# Quantile-Quantile plot
qqnorm(macaca_food$Group_size,pch=16,col='lightgreen',xlab='')
qqline(macaca_food$Group_size,col='red')

# ????


# Factor site
summary(macaca_food$Type_site)
# Factor month
summary(macaca_food$Month)

# Factor site
summary(macaca_food$Type_site)


# A priori il n'y a pas de groupe sous représenter
par(mfrow=c(1,1))
boxplot(macaca_food$Herba_notincrop~macaca_food$Type_site)
# L'on peut à priori s'attendre à ce que l'anthropisation influence la consommation de buisson naturel
boxplot(macaca_food$Herba_notincrop~macaca_food$Month)
# Il semble avoir des différences dans la consommation de natural tree bush celon les mois
boxplot(macaca_food$Herba_notincrop~macaca_food$Group)
# La consommation semble aussi varier selon les groupes
plot(macaca_food$Herba_notincrop~macaca_food$Group_size)
### Si la variable mois nous est d'intérêt puisqu'on pourrait dicerner des variations de diets celon les mois
### mais aussi celon les mois et les types de site (intéraction), les groupes n'apportent pas d'information biologique
### Il faudra ainsi tester l'effet aléatoire de la variable groupe afin de voir si la variance inter-groupe ne serait pas trop fortement porteuse d'information


# Transformation des données pour lois béta
perturb=function(x){
  if (x==1){return(x-10**-10)}
  if (x==0){return(x+10**-10)}
  if (x!=0 & x!=1){return(x)}
}
food_variable=colnames(macaca_food)[5:14]
for(i in food_variable){
  macaca_food[[i]]=sapply(macaca_food[[i]],perturb)}
mod=glmmTMB(Herba_notincrop~Month+Type_site+Month:Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)
summary(mod)
# On observe ici que le type de site semble être impactant sur la consommation de tree_bush_natural
# On peut toutefois réaliser des test bootstrap sur l'ensemble de nos variables afin d'en évaluer 
# La pertinence dans le modèle

nBoot <- 1000
lrStat <- rep(NA, nBoot)
p_val_herbanotincrop=numeric(0)
var_exp=c("Group","Group_size","Month","Type_site","interaction")
for (i in var_exp){
  nBoot=1000
  mod_alt=glmmTMB(Herba_notincrop~Month+Type_site+Month:Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)
  if (i=="Group"){mod_null=glmmTMB(Herba_notincrop~Month+Type_site+Month:Type_site+Group_size,family="beta_family",data=macaca_food)}
  if (i=="Group_size"){mod_null=glmmTMB(Herba_notincrop~Month+Type_site+Month:Type_site+(1|Group),family="beta_family",data=macaca_food)}
  if (i=="Month"){mod_null=glmmTMB(Herba_notincrop~Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)}
  if (i=="Type_site"){mod_null=glmmTMB(Herba_notincrop~Month+Group_size+(1|Group),family="beta_family",data=macaca_food)}
  if (i=="interaction"){mod_null=glmmTMB(Herba_notincrop~Month+Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)}
  lrObs <- 2 * logLik(mod_alt) - 2 * logLik(mod_null) # observed test stat
  lrStat <- rep(NA, nBoot)
  for (iBoot in 1:nBoot)
  {
    macaca_food$NTBSim <- unlist(simulate(mod_null))
    macaca_food$NTBSim <- sapply(macaca_food$NTBSim,perturb)# resampled data
    bAlt <-glmmTMB(NTBSim ~ Month * Type_site + Group_size + (1 | Group),
                   data = macaca_food,
                   family = beta_family(link = "logit"))
    # null model
    if (i=="Group"){bNull=glmmTMB(NTBSim~Month+Type_site+Month:Type_site+Group_size,family="beta_family",data=macaca_food)}
    if (i=="Group_size"){bNull=glmmTMB(NTBSim~Month+Type_site+Month:Type_site+(1|Group),family="beta_family",data=macaca_food)}
    if (i=="Month"){bNull=glmmTMB(NTBSim~Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)}
    if (i=="Type_site"){bNull=glmmTMB(NTBSim~Month+Group_size+(1|Group),family="beta_family",data=macaca_food)}
    if (i=="interaction"){bNull=glmmTMB(NTBSim~Month+Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)}
    lrStat[iBoot] <- 2 * logLik(bAlt) - 2 * logLik(bNull) # resampled test stat
  }
  
  print(lrStat)
  
  print(lrObs)
  
  mean(lrStat > lrObs) # P-value for test of the random effect
  p_val_herbanotincrop=c(p_val_herbanotincrop,mean(lrStat > lrObs))
  print(mean(lrStat > lrObs))
  print(lrObs)
  png(paste0("bootstrap",i,".png"), width=800, height=600)
  hist(lrStat, col='dodgerblue3',main=i) # Histogram of the 1000 values of likelihood of the simulated model
  abline(v = lrObs, col="darkred", lwd=3, lty=2) # Vertical red line representing the likelihood of the observed model including the random factor
  dev.off() 
}
var_exp=c("Group","Group_size","Month","Type_site","interaction")
p_val_herbanotincrop=c("0.34","0.19",">0.001",">0.001",">0.001")
library(performance)
check_collinearity(mod)
### Donc les variables type_site, month et leur interaction sont fortement significatif pour le modèle
### On peut maintenant discuter des coefficients du modèle (NIK L4AUTOCOR DES RES J4AI PAS LE TEMPS JE SUIS FATIGU2)
summary(mod)
# On observe que les valeurs pour les loglik observé du modèle sans interaction et sans interaction + type_site sont extrémement proches 57.2 pour interaction/57.7 pour type_site
# On peut donc conclure que c'est l'interaction entre mois et type site qui rend type site significatif
# Ainsi l'anthropisation en elle même n'impact pas la diet de herbes hors des cultures mais
# on observe toutefois des tendances saisonnière marginales entre les sites
warning()
macaca_food$Group_size=as.numeric(macaca_food$Group_size)
modbis=glmmTMB(Herba_notincrop~Type_site+Month+Group_size+(1|Group),family="beta_family",data=macaca_food)

anova(modbis,mod)
# Les macaques consomment moins de Natural_tree_bush dans les sites non anthropisé
# La consommation des macaques baissent durant les mois 7 8 et 11
# Cependant la baisse de consomation est moindre durant les mois 7 à 11 pour les sites non anthropisé
# En résumé la consommation de Natural tree bush à tendance à être moindre sur les sites anthropisé mais reste plus constante au cours des saisons
# par rapport au site anthropisé où leur consommation varie plus au cours des saisons
# BON FF
##### Rebelote autre variable je suis fatigué purée
# Animal

# Boxplot
boxplot(macaca_food$Animal, col='lightgreen', ylab=var)

# Cleveland plot
dotchart(macaca_food$Animal, pch=16, col='lightgreen', xlab=var)

# Histogram
hist(macaca_food$Animal, col='lightgreen', xlab="var", main="")
# Ici notre variable est très centré en 0 avec quelques rares valeurs vers 1, on transforme donc avec sqrt()
hist(sqrt(macaca_food$Animal), col='lightgreen', xlab="var", main="")
macaca_food$Animal=sqrt(macaca_food$Animal)
# Quantile-Quantile plot
qqnorm(macaca_food$Animal, pch=16, col='lightgreen', xlab='')
qqline(macaca_food$Animal, col='red')

# On observe ici que notre distribution ne présente pas de trop nombreux outlier, il n'y a pas de cassure dans la distribution
# La distribution de la variable s'apparente à une loi beta (entre 0 et 1)
# A priori on se dirige vers un glm beta

par(mfrow=c(2,2))
# Boxplot
boxplot(macaca_food$Group_size,col='lightgreen')
# Cleveland plot
dotchart(macaca_food$Group_size,pch=16,col='lightgreen')
# Histogram
hist(macaca_food$Group_size,col='lightgreen')
# Quantile-Quantile plot
qqnorm(macaca_food$Group_size,pch=16,col='lightgreen',xlab='')
qqline(macaca_food$Group_size,col='red')

# ????


# Factor site
summary(macaca_food$Type_site)
# Factor month
summary(macaca_food$Month)

# Factor site
summary(macaca_food$Type_site)


# A priori il n'y a pas de groupe sous représenter
par(mfrow=c(1,1))
boxplot(macaca_food$Animal~macaca_food$Type_site)
# L'on peut à priori s'attendre à ce que l'anthropisation influence la consommation de buisson naturel
boxplot(macaca_food$Herba_notincrop~macaca_food$Month)
# Il semble avoir des différences dans la consommation de natural tree bush celon les mois
boxplot(macaca_food$Herba_notincrop~macaca_food$Group)
# La consommation semble aussi varier selon les groupes
plot(macaca_food$Herba_notincrop~macaca_food$Group_size)
### Si la variable mois nous est d'intérêt puisqu'on pourrait dicerner des variations de diets celon les mois
### mais aussi celon les mois et les types de site (intéraction), les groupes n'apportent pas d'information biologique
### Il faudra ainsi tester l'effet aléatoire de la variable groupe afin de voir si la variance inter-groupe ne serait pas trop fortement porteuse d'information


# Transformation des données pour lois béta
perturb=function(x){
  if (x==1){return(x-10**-10)}
  if (x==0){return(x+10**-10)}
  if (x!=0 & x!=1){return(x)}
}
food_variable=colnames(macaca_food)[5:14]
for(i in food_variable){
  macaca_food[[i]]=sapply(macaca_food[[i]],perturb)}
mod=glmmTMB(Animal~Month+Type_site+Month:Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)
summary(mod)
# On observe ici que le type de site semble être impactant sur la consommation de tree_bush_natural
# On peut toutefois réaliser des test bootstrap sur l'ensemble de nos variables afin d'en évaluer 
# La pertinence dans le modèle

nBoot <- 1000
lrStat <- rep(NA, nBoot)
p_val_animal=numeric(0)
var_exp=c("Group","Group_size","Month","Type_site","interaction")
for (i in var_exp){
  nBoot=1000
  mod_alt=glmmTMB(Animal~Month+Type_site+Month:Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)
  if (i=="Group"){mod_null=glmmTMB(Animal~Month+Type_site+Month:Type_site+Group_size,family="beta_family",data=macaca_food)}
  if (i=="Group_size"){mod_null=glmmTMB(Animal~Month+Type_site+Month:Type_site+(1|Group),family="beta_family",data=macaca_food)}
  if (i=="Month"){mod_null=glmmTMB(Animal~Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)}
  if (i=="Type_site"){mod_null=glmmTMB(Animal~Month+Group_size+(1|Group),family="beta_family",data=macaca_food)}
  if (i=="interaction"){mod_null=glmmTMB(Animal~Month+Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)}
  lrObs <- 2 * logLik(mod_alt) - 2 * logLik(mod_null) # observed test stat
  lrStat <- rep(NA, nBoot)
  for (iBoot in 1:nBoot)
  {
    macaca_food$NTBSim <- unlist(simulate(mod_null))
    macaca_food$NTBSim <- sapply(macaca_food$NTBSim,perturb)# resampled data
    bAlt <-glmmTMB(NTBSim ~ Month * Type_site + Group_size + (1 | Group),
                   data = macaca_food,
                   family = beta_family(link = "logit"))
    # null model
    if (i=="Group"){bNull=glmmTMB(NTBSim~Month+Type_site+Month:Type_site+Group_size,family="beta_family",data=macaca_food)}
    if (i=="Group_size"){bNull=glmmTMB(NTBSim~Month+Type_site+Month:Type_site+(1|Group),family="beta_family",data=macaca_food)}
    if (i=="Month"){bNull=glmmTMB(NTBSim~Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)}
    if (i=="Type_site"){bNull=glmmTMB(NTBSim~Month+Group_size+(1|Group),family="beta_family",data=macaca_food)}
    if (i=="interaction"){bNull=glmmTMB(NTBSim~Month+Type_site+Group_size+(1|Group),family="beta_family",data=macaca_food)}
    lrStat[iBoot] <- 2 * logLik(bAlt) - 2 * logLik(bNull) # resampled test stat
  }
  
  print(lrStat)
  
  print(lrObs)
  
  mean(lrStat > lrObs) # P-value for test of the random effect
  p_val_aniaml=c(p_val_animal,mean(lrStat > lrObs))
  print(mean(lrStat > lrObs))
  print(lrObs)
  png(paste0("bootstrap_animal_",i,".png"), width=800, height=600)
  hist(lrStat, col='dodgerblue3',main=i) # Histogram of the 1000 values of likelihood of the simulated model
  abline(v = lrObs, col="darkred", lwd=3, lty=2) # Vertical red line representing the likelihood of the observed model including the random factor
  dev.off() 
}
# On observe ici qu'aucune variable ne semble expliquer de variation dans la consommation de petit animaux,
# On peut conclure que les besoins alimentaires liés à la consomation d'animaux ne sont pas compensé
# par d'autres nouriture anthropisé. 

# Pour les nouritures consommées uniquement dans les sites anthropiser on peut conclure qu'elles ne sont présentes que sur ces sites
# Pour les champignons il existe deux hypothèse : soit les besoins alimentaire sont entièrement
# remplacé par d'autres ressources anthropisées, mais aussi que les champignons consommées par les macaques
# ne sont pas présent sur les sites anthropisé.