#Analyse bivariée ----


# Installer et charger le package corrplot ----
install.packages("readxl")
install.packages("corrplot")
library(readxl)
library(corrplot)




# Charger les données ( Verifier si on se trouve bien dans le repertoire juste avant "Data") ----
bdd <- read_excel("Data/Concrete_Data.xls")

# Vérifier les premières lignes des données
head(bdd)

# Vérifier si toutes les colonnes sont numériques
str(bdd)

bdd_numeric <- bdd[, sapply(bdd, is.numeric)]

# Calcul de la matrice de corrélation 
# cor_matrix <- cor(bdd_numeric, use = "complete.obs")

#matrice de corrélation
print(cor_matrix)


# Charger les bibliothèques nécessaires
install.packages("ggplot2")
library(ggplot2)

# Charger les données
bdd <- Concrete_Data

# Variables d'intérêt
variables <- c("Cement (component 1)(kg in a m^3 mixture)", 
               "Blast Furnace Slag (component 2)(kg in a m^3 mixture)",
               "Fly Ash (component 3)(kg in a m^3 mixture)", 
               "Water  (component 4)(kg in a m^3 mixture)", 
               "Superplasticizer (component 5)(kg in a m^3 mixture)",
               "Coarse Aggregate  (component 6)(kg in a m^3 mixture)", 
               "Fine Aggregate (component 7)(kg in a m^3 mixture)", 
               "Age (day)", 
               "Concrete compressive strength(MPa, megapascals)")

# Boucle pour générer les graphiques
for (var in variables) {
  # Histogramme
  ggplot(bdd, aes_string(x = var)) + 
    geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) + 
    labs(title = paste("Histogramme de", var), x = var, y = "Fréquence") + 
    theme_minimal()
  
  # Boxplot
  ggplot(bdd, aes_string(x = var)) + 
    geom_boxplot(fill = "lightgreen", color = "darkgreen", alpha = 0.7) + 
    labs(title = paste("Boxplot de", var), x = var, y = "") + 
    theme_minimal()
  
  # Densité (courbe de densité)
  ggplot(bdd, aes_string(x = var)) + 
    geom_density(fill = "lightcoral", color = "red", alpha = 0.6) + 
    labs(title = paste("Densité de", var), x = var, y = "Densité") + 
    theme_minimal()
}


