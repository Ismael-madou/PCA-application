library(readxl)
library (lmtest)
library(broom)
donnee <- read_excel("Data/Concrete_Data.xls")

#reecriture des variables pour faciliter l'etude
names(donnee) <- c('cement',
               'blast_furnace',
               'fly_ash',
               'water',
               'super_plast',
               'coarse',
               'fine_aggr',
               'age',
               'y_concrete_compresive')

str(donnee)

#contruction du modèle
regression.lineaire <- lm(formula = y_concrete_compresive ~ ., data=donnee)
summary(regression.lineaire)

# pour intercept, la p-value est de 0.38 qui est superieure à 0.05, donc on decide l'hypothèse H0
# selon laquelle cette valeur n'est pas significative. Ensuite, pour les variables comme "Cement",
# "blast_furnace", "fly_ash", "water", "super_plast" et "age", leurs p-values sont inferieures à 0.05 alors 
# on decide l'hypothése H1 selon laquelle elles sont significatives. Néanmoins, pour les variables "coarse" et "fine_aggr",
# on a des p-values superieures au seuil de 0.05 donc elles ne sont pas significatives à ce seuil par contre,
# elles le sont pour un seuil de 0.1.


#--------------------autocorrelation-----------------------------------------#

#test d'autocorrelation 
dwtest(regression.lineaire, alternative = c("two.sided"))

#Durbin-Watson test
#data:  regression.lineaire
#DW = 1.2815, p-value < 2.2e-16
#alternative hypothesis: true autocorrelation is not 0
# d'après ce test, la p-value est inferieure à 0.05 on decide l'hypothése H1 selon laquelle les erreurs sont 
#fortement corrélées.

acf(regression.lineaire$residuals, main = "Autocorrélation des erreurs")
# on remarque que plusieurs barres sortent de l'intervaller de confiance, ce qui vient soutenir la conclusion 
# du test de Durbin watson selon laquelle les erreurs sont autocorrélées.

#-----------------------------------homoscédasticité--------------------------------------#

bptest(regression.lineaire, studentize = FALSE)
#Breusch-Pagan test
#data:  regression.lineaire
#BP = 140.25, df = 8, p-value < 2.2e-16
# d'après les resulats du test de breush pagan, on choisit l'hypothèse H1 selon laquelle 
# l"erreur est hétéroscedastique c'est-à-dire qu'il y'a une dependance entre l'erreur et les 
#variables explicatives.

# ou 

bptest(regression.lineaire)

#----------------------------------test de normalité-------------------------------------#

shapiro.test(regression.lineaire$residuals)

#Shapiro-Wilk normality test
#data:  regression.lineaire$residuals
#W = 0.99532, p-value = 0.002986
# selon les résulats de ce test, la p-value est inferieure au seuil qui est de 5%, alors nous 
# rejetons l'hypothése nulle de normalité des erreurs et par consequent les erreurs ne sont pas
#normales.

# ou 

shapiro.test(rstudent(regression.lineaire))

#------------------------------graphique validation du modèle-----------------------------#

regression.lineaire.metriques <- augment(regression.lineaire)
head(regression.lineaire.metriques[,10:15])

plot(regression.lineaire,1)
# d'après ce plot, il n'existe aucune linéarité entre la variable réponse et les variables explicatives.
# car la courbe rouge n'est pas horizontale par consequent cette relation n'est pas linéaire. les lignes (betons dans notre cas)
# 382, 384 et 115 sont les betons ayant les grandes valeurs en terme des residus.

plot(regression.lineaire,3)
abline(h=sqrt(3), col = "green", lty = 2, lwd = 3)
# Dans notre cas la courbe rouge n'est horizontale et ne separe pas les points de maniere homogéne de part et d'autre 
# alors cela indique que les erreurs ne sont pas homogénes ce qui rejoint la conclusion du test de 
# Breush Pagan.


plot(regression.lineaire, 2)
# ce plot de l'hypothése de normalité nous montre que plusieurs points ne suivent pas la ligne malgrés qu'ils sont proches,
# ceci nous conduit à confirmer les resultats du test de normalité qui a été effectué avec la methode de shapiro-wilk.

plot(regression.lineaire, 5)
abline(h = c(-3, 3), v = 2 * (8 + 1) / 1030, col = c("yellow", "yellow", "blue"), lty = c(2, 2, 2), lwd = c(3, 3, 3))
#tous les points qui se trouvent à droite de la droite verticale blue sont des points leviers extremes c'est-à-dire
# des valeurs extremes des valeurs explicatives et tous les points qiu se trouver en dehors de l'intervalle fromé par les 
#points tillés en jaune sont des outliers c'est-à-dire des valeurs extremes de la varaibles cible. Dans notre cas, nous avons
# que deux outliers et plusieurs points leviers extremes.

plot(regression.lineaire,4)
abline(h= 4/( 1030-8-1 ), col = c( "red" ), lty = 2, lwd = 3)
# suite à l'observation des resultats données par le graphique, on remarque que plusieurs points depassent la ligne construite 
# par les points tillées rouges qui represente le seuil à partir duquel un point est considéré comme influent. les betons 57, 225 et 611
# sont les plus influents.

#----------------------------------test du modèle global------------------------------------#

