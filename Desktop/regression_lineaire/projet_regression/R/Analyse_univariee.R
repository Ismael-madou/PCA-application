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





# 1️⃣ Histogramme pour Cement
p1 <- ggplot(bdd, aes(x = cement)) +
  geom_histogram(bins = 30, fill = "blue", color = "darkgreen") +
  labs(title = "Distribution de la variable Cement", x = "Cement", y = "Fréquence") +
  theme_minimal()
print(p1)

# 2️⃣ Histogramme pour Blast Furnace Slag
p2 <- ggplot(bdd, aes(x = blast_furnace)) +
  geom_histogram(bins = 30, fill = "red", color = "darkred") +
  labs(title = "Distribution de la variable Blast Furnace Slag", x = "Blast Furnace Slag", y = "Fréquence") +
  theme_minimal()
print(p2)

# 3️⃣ Histogramme pour Fly Ash
p3 <- ggplot(bdd, aes(x = fly_ash)) +
  geom_histogram(bins = 30, fill = "purple", color = "darkblue") +
  labs(title = "Distribution de la variable Fly Ash", x = "Fly Ash", y = "Fréquence") +
  theme_minimal()
print(p3)

# 4️⃣ Histogramme pour Water
p4 <- ggplot(bdd, aes(x = water)) +
  geom_histogram(bins = 30, fill = "cyan", color = "darkblue") +
  labs(title = "Distribution de la variable Water", x = "Water", y = "Fréquence") +
  theme_minimal()
print(p4)

# 5️⃣ Histogramme pour Superplasticizer
p5 <- ggplot(bdd, aes(x = super_plast)) +
  geom_histogram(bins = 30, fill = "orange", color = "darkorange") +
  labs(title = "Distribution de la variable Superplasticizer", x = "Superplasticizer", y = "Fréquence") +
  theme_minimal()
print(p5)

# 6️⃣ Histogramme pour Coarse Aggregate
p6 <- ggplot(bdd, aes(x = coarse)) +
  geom_histogram(bins = 30, fill = "yellow", color = "gold") +
  labs(title = "Distribution de la variable Coarse Aggregate", x = "Coarse Aggregate", y = "Fréquence") +
  theme_minimal()
print(p6)

# 7️⃣ Histogramme pour Fine Aggregate
p7 <- ggplot(bdd, aes(x = fine_aggr)) +
  geom_histogram(bins = 30, fill = "pink", color = "black") +
  labs(title = "Distribution de la variable Fine Aggregate", x = "Fine Aggregate", y = "Fréquence") +
  theme_minimal()
print(p7)

# 8️⃣ Histogramme pour Age
p8 <- ggplot(bdd, aes(x = age)) +
  geom_histogram(bins = 30, fill = "brown", color = "black") +
  labs(title = "Distribution de la variable Age", x = "Age (jours)", y = "Fréquence") +
  theme_minimal()
print(p8)

# 9️⃣ Histogramme pour Concrete Compressive Strength
p9 <- ggplot(bdd, aes(x = y_concrete_compresive)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "black") +
  labs(title = "Distribution de la variable Concrete Compressive Strength", x = "Concrete Compressive Strength (MPa)", y = "Fréquence") +
  theme_minimal()
print(p9)



