
library(readxl)
df <- read_excel("Data/Concrete_Data.xls")

#reecriture des variables pour faciliter l'etude
names(df) <- c('cement',
                'blast_furnace',
                'fly_ash',
                'water',
                'super_plast',
                'coarse',
                'fine_aggr',
                'age',
                'y_concrete_compresive')

# Chargement de la librairie FactoMineR 
library(FactoMineR)

# Chargement de la librairie FactoMineR 
library(FactoMineR)

# Réalisation de l'ACP :
# On considère les 8 premières colonnes comme variables actives (expliquant la résistance)
# et la 9ème colonne ("y_concrete_compresive") comme variable quantitative supplémentaire.
res_pca <- PCA(bdd,
               scale.unit = TRUE,    # Standardisation des données
               graph = FALSE)        # Pas de graphique initial

# Visualisation des valeurs propres (histogramme)
barplot(
  res_pca$eig[, 1],                 # Extraction des valeurs propres
  xlab = "Composantes principales", # Étiquette axe X
  ylab = "Valeur propre",           # Étiquette axe Y
  col = "#DDB688",                  # Couleur des barres
  border = "#4B0000",
  cex.names = 0.65,
  cex.axis = 0.65,
  las = 2                           # Orientation des étiquettes
)


# Analyse sur les 2 premiers axes

# Représentation des variables sur le plan factoriel (axes 1 et 2)
plot.PCA(res_pca,
  axes = c(1, 2),             # On se concentre sur les 2 premiers axes
  choix = "var",              # Afficher les variables dans le plan factoriel
  col.var = "#4B0000",        # Couleur des variables alignée au style
  label = "all",
  title = "",
  addgrid.col = "#DDB688" ,
  cex = 0.65) # Taille des labels


# Tableau des contributions des variables
contrib_var <- as.data.frame(res_pca$var$contrib)
print(contrib_var)


# Tableau des cos² des variables
cos2_var <- as.data.frame(res_pca$var$cos2)
print(cos2_var)


# Représentation des variables sur le plan factoriel (axes 1 et 3)
plot.PCA(
  res_pca,
  axes = c(1, 3),             # On se concentre sur les 2 premiers axes
  choix = "var",              # Afficher les variables dans le plan factoriel
  col.var = "#4B0000",        # Couleur des variables alignée au style
  label = "all",
  title = "",
  addgrid.col = "#DDB688" ,
  cex = 0.65) # Taille des labels

##----------------------------------------------interpretation------------------------------------------------------

# les deux axes representent environ 41% de la variance totale (dim 1 constitue 25.4% et dim 2 constitue 21.5%). 










  # Représentation des variables sur le plan factoriel (axes 2 et 3)
  plot.PCA(
    res_pca,
    axes = c(2, 3),             # On se concentre sur les 2 premiers axes
    choix = "var",              # Afficher les variables dans le plan factoriel
    col.var = "#4B0000",        # Couleur des variables alignée au style
    label = "all",
    title = "",
    addgrid.col = "#DDB688" ,
    cex = 0.65) # Taille des labels
    






