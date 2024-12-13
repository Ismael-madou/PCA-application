airports <- read.csv('/Users/nickpomozov/Документы/MA/World_Airports_Edited.csv', sep = ";")
summary(airports)

install.packages("Factoshiny")
install.packages("FactoInvestigate")
install.packages("DataExplorer") #pour des analyses uni et bi-variees rapides
install.packages("corrplot")
install.packages("FactoMineR")

library("DataExplorer")
library("FactoMineR")
library("Factoshiny")

#Effacer les lignes avec NAs
sum(is.na(airports)) 
airports_clean <- na.omit(airports)
head(airports_clean)

library(dplyr)

#Arrangement des données. 1. Filtrer les types d'aeroports
# 2. Changer les types restants aux numeros 1-4
airports_clean2 <- airports_clean %>%
  filter(!type %in% c("heliport", "balloonport", "seaplane_base")) %>%
  mutate(type = recode(type,
                       "small_airport" = 1,
                       "medium_airport" = 2,
                       "large_airport" = 3,
                       "closed" = 4))
  
# Choisir les variables numeriques pour l'ACP
numeric_airports <- airports_clean2 %>% select_if(is.numeric)
V <- cov(numeric_airports) #matrice des covariances
R <- cor(numeric_airports) #matrice des correlation

corrplot::corrplot(R, type = "upper", method = "square")
res_pca <- PCA(X = numeric_airports,
               scale.unit = TRUE, # On précise qu'on souhaite réaliser une ACP sur données réduites
               graph = FALSE)     # On désactive les graphiques automatiques

# On peut représenter le résultat de l'acp par la suite
plot.PCA(res_pca, axes = c(1,2), choix = "ind")
plot.PCA(res_pca, axes = c(1,2), choix = "var")

res_pca$eig
barplot(res_pca$eig[,1])

  PCAshiny(X = res_pca)


