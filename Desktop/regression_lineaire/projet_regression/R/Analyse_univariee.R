#Analyse univariée ----

# Installer et charger les package
install.packages("readxl")
install.packages("corrplot")
library(readxl)
library(corrplot)
library(ggplot2)

# Charger les données ( Verifier si on se trouve bien dans le repertoire juste avant "Data")

bdd <- read_excel("Data/Concrete_Data.xls")

#Afficher les 5 premieres lignes
head(bdd,5)
#Afficher le nom des variables
colnames(bdd)

summary(bdd$`Concrete compressive strength(MPa, megapascals)`)

# fichier changé avec le chemin de base : "Data/Concrete_Data.xls"
bdd <- read_excel("regression_grande_dimension/Data/Concrete_Data.xls")


#reecriture des variables pour faciliter l'etude
names(bdd) <- c('cement',
                'blast_furnace',
                'fly_ash',
                'water',
                'super_plast',
                'coarse',
                'fine_aggr',
                'age',
                'y_concrete_compresive')

#il n'y a que des variables numérique
str(bdd)

#il n'y a pas de Na
sum(is.na(bdd$y_concrete_compresive))


#analyse variable à expliquer ( y_concrete_compressive) :
#distribution

boite_moust <- ggplot(bdd, aes_string(y= bdd$y_concrete_compresive)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen", alpha = 0.7) + 
  geom_hline(yintercept = mean(bdd$y_concrete_compresive), color = "red", linetype = "dashed") +
  labs(title = paste("Boxplot de", bdd$y_concrete_compresive), x = bdd$y_concrete_compresive, y = "") 

#*Distribution de la variable à expliqué correcte, il n'y a que 3
#* Valeurs qui dépassent les lignes de la boites
#* Il n'y a pas un tres gros écart entre la médiane et la moyenne
#* ==> ccl :Tres bonne distribution pour faire une étude





# Boucle pour générer un histogramme des frequences pour chaque variable au lieu de le faire 1 par 1. 
for (var in colnames(bdd)) {
    p <- ggplot(bdd, aes_string(x = var)) +
      geom_histogram(bins = 30, fill = "blue", color = "darkgreen") +
      labs(
        title = paste("Distribution de la variable", var),
        x = var,
        y = "Fréquence"
      ) +
      theme_minimal() 
    
    print(p)  # Affiche chaque graphique 
  }


#Analyse 


#Variable resistance du beton("y_concrete_compresive ") :
#Variable ("fine_aggr ") :
#Variable eau ("Water"):

#les variable sont principalements concentrés autour de la médiane  
#ce qui pourrait indiquer par exemple pour la variable de resitance du beton 
#que la plupart  des échantillons de béton ont une résistance similaire


#Variable cendre volante("fly_ash "):
#Variable Quantité de laitier("blast_furnace "):
#Variable cendre volante("fly_ash "):
#Variable superplastique("super_plast "):

#Ces variables sont fortements concentrés surdes données faibles
#et minimales


