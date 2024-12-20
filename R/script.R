#chargement des données 
data <- read.csv("C:/Users/MGI/Desktop/projet-analysis-homework/World_Airports.csv", sep = ";")

#statistiques descriptives 
summary(data)
colSums(is.na(data))

# Fréquences pour une variable catégorielle
frequence_type <- table(data$type)
frequence_iso_country <- table(data$iso_country)


# Diagramme en barres pour 'le type d'aéroport'
barplot(frequence_type, 
        main = "Le type d'aéroport", 
        col = "skyblue", 
        xlab = "Type", 
        ylab = "Fréquence du type")

# Diagramme en barres pour 'iso_country' (pays)
barplot(frequence_iso_country, 
        main = "Répartition par pays", 
        col = "lightgreen", 
        xlab = "Pays", 
        ylab = "Fréquence des pays")

# Tableau de contingence (tableau croisé)
tableau_croise <- table(data$type, data$iso_country)

# Test du chi-carré
chi2_test <- chisq.test(tableau_croise)

# Test exact de Fisher avec approximation via Monte Carlo
fisher_test_approx <- fisher.test(tableau_croise, simulate.p.value = TRUE, B = 10000)
print(fisher_test_approx)


# Test de McNemar
tableau_mcnemar <- table(data$type, data$iso_country)
mcnemar_result <- mcnemar.test(tableau_mcnemar)

# Installer le package 'vcd' pour calculer Cramér's V
install.packages("vcd")
library(vcd)

# Calcul de Cramér's V
assoc_result <- assocstats(table(data$type, data$iso_country))


#chargement des libraries
library(tidyverse)
library(dplyr)

#suprresion de certaines 
donnee_temporaire <- data %>% select(type, iso_country)

#statitiques sur les colonnes retenues
summary(donnee_afc)
is.na(donnee_afc)

# Extraire et afficher les pays uniques

pays_uniques <- donnee_temporaire %>% 
  select(iso_country) %>%  # Sélectionner la colonne iso_country
  distinct()               # Récupérer les valeurs uniques

# Calculer le nombre d'aéroports par pays
nb_aeroports_par_pays <- donnee_temporaire %>%
  group_by(iso_country) %>%  # Grouper par pays
  summarise(nb_aeroports = n()) %>%  # Compter le nombre de lignes par groupe
  arrange(desc(nb_aeroports))  # Trier par ordre décroissant du nombre d'aéroports


# Calculer le nombre d'aéroports par type et par pays
nb_aeroports_par_type_pays <- donnee_temporaire %>%
  group_by(iso_country, type) %>%  # Grouper par pays et par type
  summarise(nb_aeroports = n(), .groups = "drop") %>%  # Compter le nombre de lignes par groupe
  arrange(iso_country, desc(nb_aeroports))  # Trier par pays et par nombre d’aéroports décroissant

# application de l'AFC sur tableau_contingence_is_no
library(FactoMineR)
library(Factoshiny)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)

# Création du tableau de contingence (avec 'type' comme colonnes et 'iso_country' comme lignes)
tableau_contingence <- donnee_temporaire %>%
  count(type, iso_country) %>%
  spread(key = type, value = n, fill = 0)

# Calculer le nombre d'aéroports par pays
aeroports_par_pays <- donnee_temporaire %>%
  count(iso_country) %>%
  rename(nombre_aeroports = n)

# Décider d'un seuil basé sur la distribution (par exemple, utiliser le quantile)
seuil <- 200 # Utiliser le 10ème percentile

# Créer une nouvelle colonne qui regroupe les pays avec moins que le seuil en "Autres"
aeroports_par_pays_regroupe <- aeroports_par_pays %>%
  mutate(iso_country_regroupe = ifelse(nombre_aeroports < seuil, "Autres", iso_country))

# Compter le nombre de pays avec moins de 200 aéroports
nombre_pays_faible_aeroports <- aeroports_par_pays %>%
  filter(nombre_aeroports < seuil) %>%
  count()

# Afficher la distribution après regroupement
table(aeroports_par_pays_regroupe$iso_country_regroupe)

# Recréer la table de contingence avec les pays regroupés
tableau_contingence_regroupe <- donnee_temporaire %>%
  mutate(iso_country_regroupe = ifelse(iso_country %in% aeroports_par_pays_regroupe$iso_country[aeroports_par_pays_regroupe$nombre_aeroports < seuil], 
                                       "Autres", iso_country)) %>%
  count(iso_country_regroupe, type) %>%
  spread(key = type, value = n, fill = 0)

# Calcul de la matrice de corrélation entre les colonnes (types d'aéroports)
# Nous excluons la colonne iso_country
matrice_corr <- cor(tableau_contingence_regroupe[, -1], method = "pearson")

# Affichage de la matrice de corrélation avec corrplot
corrplot(matrice_corr, method = "color", type = "full", 
         col = colorRampPalette(c("white", "blue"))(200),
         title = "Matrice de Corrélation entre les Types d'Aéroports",
         addCoef.col = "black")  # Ajouter les coefficients de corrélation dans le graphique
 
# Vérifier s'il y a des valeurs manquantes dans la colonne iso_country
if(any(is.na(tableau_contingence_regroupe$iso_country_regroupe))) {
  # Si oui, nous les supprimons (ou vous pouvez les gérer différemment)
  tableau_contingence_regroupe <- tableau_contingence_regroupe %>% filter(!is.na(iso_country_regroupe))
}

# Supprimer la colonne iso_country et mettre iso_country comme rownames
tableau_contingence_no_iso <- tableau_contingence_regroupe%>%
  select(-iso_country_regroupe)

# Ajouter les iso_country comme rownames
rownames(tableau_contingence_no_iso) <- tableau_contingence_regroupe$iso_country_regroupe

#calcul des profils lignes 
PL <- prop.table(as.matrix(tableau_contingence_no_iso, margin=1))

#verification
apply(PL,1,sum)
barplot(height = t(PL), beside = FALSE,legend.text=colnames(PL))

#representation du profil ligne
barplot(height = t(PL), beside = FALSE,legend.text=colnames(PL))

#calcul des profils colonnes
PC <- prop.table(as.matrix(tableau_contingence_no_iso), margin = 2)

#representation des profils colonnes 
barplot(height = PC, beside = FALSE, legend.text = rownames(PC))

#test d'independence du Khi deux
chisq.test(tableau_contingence_no_iso)-> res_chisq
res_chisq$statistic
#le calcul du test d'independance est impossible parce que certaines cellules 
# contiennent la valeur zero ou proche de zero ce qui rend ce calcul impossible(incorrect)

#Inertie du nuage des profils 
res_chisq$statistic/sum(children) + 1
#l'impossibilité du calcul du test d'independance rend aussi impossible le calcul
# de l'inertie puisqu'elle est dependante du test d'independance.

#recherche des valeurs propres
install.packages("FactoMineR")
library(FactoMineR)
res_afc <- CA(X = tableau_contingence_no_iso, ncp = 7,
   graph = FALSE)
res_afc
res_afc$eig

# tracer l'histogramme des valeurs propres
barplot(res_afc$eig[,1])
barplot

# regle du coude : on retient  3 valeurs propres qui representent 88,41% de l'inertie

# projection des nuages des profils lignes sur le plan principal
plot.CA(x = res_afc, choix = "CA", axes = c(1,2), cex = 0.5)

CAshiny(res_afc)

