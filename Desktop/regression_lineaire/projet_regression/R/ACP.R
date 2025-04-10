
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

# Réalisation de l'ACP :
# On considère les 8 premières colonnes comme variables actives (expliquant la résistance)
# et la 9ème colonne ("y_concrete_compresive") comme variable quantitative supplémentaire.
res_pca <- PCA(df,
               quanti.sup = 9,       # 9ème colonne : résistance à la compression (variable cible)
               scale.unit = TRUE,    # Standardisation des données
               graph = FALSE)        # Pas de graphique initial

# Visualisation des valeurs propres (histogramme)
barplot(
  res_pca$eig[, 1],                 # Extraction des valeurs propres
  xlab = "Composantes principales", # Étiquette axe X
  ylab = "Valeur propre",           # Étiquette axe Y
  col = "#DDB688",                  # Couleur des barres
  border = "#4B0000"                # Couleur de la bordure des barres
)

# Commentaire : 

# Le graphique illustre l’histogramme des valeurs propres issu de l’ACP,
# permettant d’identifier combien d’axes principaux il est pertinent de retenir.
# Le premier axe affiche une valeur propre autour de 2.0, indiquant qu’il concentre
# une part importante de la variance totale.
# Le deuxième axe, avec une valeur propre avoisinant 1.5, conserve également une
# proportion significative d’information.
# Les troisième et quatrième axes présentent des valeurs légèrement inférieures
# , ajoutant encore un complément notable de variance.
# À partir du cinquième axe, les valeurs propres décroissent nettement, indiquant
# une contribution plus marginale à la variabilité des données.
# En appliquant la règle du coude, qui consiste à repérer un point où la
# décroissance des valeurs propres ralentit, il est judicieux de retenir les deux
# ou trois premiers axes pour une analyse plus approfondie.



# Analyse sur les 2 premiers axes



# Représentation des variables sur le plan factoriel (axes 1 et 2)
plot.PCA(
  res_pca,
  axes = c(1, 2),             # On se concentre sur les 2 premiers axes
  choix = "var",              # Afficher les variables dans le plan factoriel
  col.var = "#4B0000",        # Couleur des variables alignée au style
  col.quanti.sup = "#0000FF", # Couleur pour la variable quantitative supplémentaire
  label = "all",
  title = "",
  addgrid.col = "#DDB688"     # Couleur de la grille
)



# Tableau des contributions des variables
contrib_var <- as.data.frame(res_pca$var$contrib)
print(contrib_var)

# Tableau des cos² des variables
cos2_var <- as.data.frame(res_pca$var$cos2)
print(cos2_var)




# Le cercle de corrélation met en lumière les relations entre les variables étudiées, projetées
# sur les deux premiers axes principaux. de l’ACP. Ces axes
# capturent ensemble 46,2 % de la variance totale des données (28,50 % pour Dim 1
# et 17,7 % pour Dim 2).

# A. Interprétation du premier axe (Dim 1 : 28,50 %)
# -------------------------------------------------
# Le premier axe oppose la teneur en eau ("water") et l’âge du béton ("age") 
# aux formulations utilisant des superplastifiants ("super_plast"), des cendres volantes ("fly_ash") 
# et une plus grande proportion de granulats fins ("fine_aggr").
# - "water" (29,93 % de contribution, cos² = 0.68) est la variable la plus représentée sur Dim 1. 
# - "super_plast" (25,59 % de contribution, cos² = 0.58) est projeté dans la direction opposée à "water". 
#   Cela traduit son rôle dans l’amélioration de la fluidité du béton et la réduction du besoin en eau, 
#   permettant une meilleure résistance.
# - "fly_ash" (15,57 % de contribution, cos² = 0.35) est bien projeté sur cet axe, montrant que 
#   l’incorporation de cendres volantes influence la résistance du béton.
# - "fine_aggr" (16,15 % de contribution, cos² = 0.37) est bien représenté sur Dim 1, indiquant que 
#   la proportion de granulats fins influence la maniabilité et la compacité du béton, 
#   ce qui impacte sa résistance.
# - "age" (8,50 % de contribution, cos² = 0.19) est aussi bien représenté sur Dim 1.
# - "y_concrete_compresive" est projetée dans la même direction que "super_plast"  
#   suggérant que ce composant est associé à une meilleure résistance finale, 
#   tandis qu’une forte teneur en eau ("water") est corrélée négativement à la résistance.

# B. Interprétation du deuxième axe (Dim 2 : 17,7 %)
# ---------------------------------------------------
# Le deuxième axe met en opposition la quantité de laitier de haut fourneau ("blast_furnace") 
# et la quantité de granulats grossiers ("coarse").
# - "blast_furnace" (47,01 % de contribution, cos² = 0.66) est la variable la plus représentée 
#   sur Dim 2. Son influence importante sur cet axe montre que l’ajout de laitier de haut fourneau 
#   différencie certaines formulations de béton, influençant leur durabilité.
# - "coarse" (39,73 % de contribution, cos² = 0.56) est aussi bien projeté sur Dim 2.
# C. Implications pour la modélisation de la résistance du béton
# --------------------------------------------------------------
# - L’axe 1 montre une forte opposition entre "water" et "super_plast", indiquant 
#   qu’un terme d’interaction entre ces deux variables pourrait être testé dans 
#   notre modèle linéaire.
# - "fine_aggr" étant bien projeté sur Dim 1, son interaction avec "super_plast" pourrait 
#   être pertinente pour examiner leur effet combiné sur la compacité et la résistance.
# - "age" étant bien projeté sur Dim 1, son interaction avec "water" pourrait 
#   également être pertinente, car l’effet de l’eau sur la résistance varie avec le temps.
# - L’axe 2 suggère que l’interaction entre "blast_furnace" et "coarse" pourrait 
#   être testée pour examiner comment leur combinaison influence la résistance mécanique.



# Représentation des variables sur le plan factoriel (axes 1 et 3)
plot.PCA(
  res_pca,
  axes = c(1, 3),             # On se concentre sur les 2 premiers axes
  choix = "var",              # Afficher les variables dans le plan factoriel
  col.var = "#4B0000",        # Couleur des variables alignée au style
  col.quanti.sup = "#0000FF", # Couleur pour la variable quantitative supplémentaire
  label = "all",
  title = "",
  addgrid.col = "#DDB688"     # Couleur de la grille
)

# D. interpretation du troisème axe avec dim(1-3) 
------------------------------------------------------------------------------------------------------------------
# ces deux axes 1 et 3 forment 45% de la variance totale des données.
# cette dimension oppose water,cement contre super_plast,fly_ash. Dans le soucis de connaitre les variables 
# réellement opposés, nous nous interessons à la representativité mais aussi la contribution.
# - la variable cement est très bien representée (cos² = 0.88) et contribue en grande partie à la construction 
# de cet axe (66% terme de contributions).
#- la variable fly_ash contribue à la hauteur de 17% à la construction de cet axe et est bien representée avec cos² = 0.22.
# contrairement aux deux variables qui ne contribuent pas enormement et qui ne sont pas aussi bien representées.
# la relation entre cement et fly_ash sera bien anlysée par le modèle qui sera construit à la suite de ce travail.
# De plus, la variable cible suit la meme direction que la variable cement donc cette varaible va certainement plus
#impactée positivement la resistance à la compression du beton comparé aux autres.
  
  # Représentation des variables sur le plan factoriel (axes 2 et 3)
  plot.PCA(
    res_pca,
    axes = c(2, 3),             # On se concentre sur les axes 2 et 3
    choix = "var",              # Afficher les variables dans le plan factoriel
    col.var = "#4B0000",        # Couleur des variables alignée au style
    col.quanti.sup = "#0000FF", # Couleur pour la variable quantitative supplémentaire
    label = "all",
    title = "",
    addgrid.col = "#DDB688"     # Couleur de la grille
  )

# D. interpretation du troisème axe avec dim(2-3) 
------------------------------------------------------------------------------------------------------------------
# les deux axes 2 et 3 forment à leur tour 35% de la vraiance totale. 
# A ce  niveau, contrairement au précedent, on on remarque les deux variables retenues à savoir cement et fly_ash. 
# ce qui veut dire qu'en réalité, en combinant les deux, la resistance du beton augmente. ce petit contraste comparé 
# à la representation précedente va etre bien plus explicite après la construction du modèle.



# En conclusion, cette ACP met en évidence deux dimensions
# de la composition du béton :
# 1) Un premier axe (Dim 1) montrant les formulations riches en eau 
#    avec celles utilisant des superplastifiants, des cendres volantes et une proportion plus élevée 
#    de granulats fins.
# 2) Un deuxième axe (Dim 2) mettant en opposition le laitier de haut fourneau 
#    et les granulats grossiers, soulignant leur effet sur la structure du béton 
#    et sa prise à long terme.
# 3) un troisième axe montrant l'influence du lien cement-fly_ash sur la resistance à la compression du beton.











