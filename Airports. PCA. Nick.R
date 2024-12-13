
# j'ai besoin de ISO codes pour joindre le tableau d'aeroports avec le income
library(dplyr)    

income_table <- read.csv("C:/Users/MGI/Desktop/projet-analysis-homework/National income.csv", sep = ";") 
iso_table <- read.csv("C:/Users/MGI/Desktop/projet-analysis-homework/Countries ISO.csv", sep = ",")        

iso_table <- iso_table %>%
  rename(Country = Name)

income_table$Country <- trimws(income_table$Country)
combined_table <- income_table %>%
  left_join(iso_table, by = "Country")  

missing_iso <- combined_table %>% filter(is.na(Code))
print(missing_iso)

combined_table$Code[combined_table$Country == "USA"] <- "US"
combined_table$Code[combined_table$Country == "Bolivia"] <- "BO"
combined_table$Code[combined_table$Country == "Cabo Verde"] <- "CV"
combined_table$Code[combined_table$Country == "Cote d'Ivoire"] <- "CI"
combined_table$Code[combined_table$Country == "Curacao"] <- "CW"
combined_table$Code[combined_table$Country == "DR Congo"] <- "CD"
combined_table$Code[combined_table$Country == "Iran"] <- "IR"
combined_table$Code[combined_table$Country == "Lao PDR"] <- "LA"
combined_table$Code[combined_table$Country == "Moldova"] <- "MD"
combined_table$Code[combined_table$Country == "North Macedonia"] <- "MK"
combined_table$Code[combined_table$Country == "Taiwan"] <- "TW"
combined_table$Code[combined_table$Country == "Tanzania"] <- "TZ"
combined_table$Code[combined_table$Country == "Venezuela"] <- "VE"
combined_table$Code[combined_table$Country == "British Virgin Islands"] <- "VG"
combined_table$Code[combined_table$Country == "Cote d’Ivoire"] <- "CI"
combined_table$Code[combined_table$Country == "Namibia"] <- "NA"

combined_table <- combined_table %>%
  rename(iso_country = Code)

airports <- read.csv("C:/Users/MGI/Desktop/projet-analysis-homework/World_Airports_Edited.csv", sep = ";")

combined_table_final <- airports %>%
  left_join(combined_table, by = "iso_country")  


combined_table_final$Country[combined_table_final$iso_country == "VI"] <- "Virgin Islands (US)"

# changer les types d'aeroports par numeros 
combined_table_final <- combined_table_final %>%
  filter(!type %in% c("heliport", "balloonport", "seaplane_base")) %>%
  mutate(type = recode(type,
                       "small_airport" = 1,
                       "medium_airport" = 2,
                       "large_airport" = 3,
                       "closed" = 4))

# adding passengers info for top airports 

passengers <- read.csv("C:/Users/MGI/Desktop/projet-analysis-homework/Top airports filtered - Top airports filtered-2.csv", sep = ",") 
passengers <- passengers %>%
  rename(name = Airport) %>% 
  mutate(name = recode(name, "Heathrow Airport"="London Heathrow Airport"))


final_data <- combined_table_final %>%
  left_join(passengers, by = "name")  

colnames(final_data)

data_with_passengers <- final_data %>% filter(!is.na(passengers))

data_for_pca <- data_with_passengers %>% 
  select("name", "iso_country", "National.income", "runway_length_ft", "runway_width_ft", "passengers")

data_for_pca[data_for_pca$name == "Toronto Aerodrome", "runway_length_ft"] <- 12000
data_for_pca[data_for_pca$name == "Toronto Aerodrome", "runway_width_ft"] <- 200
data_for_pca[data_for_pca$name == "Incheon International Airport", "National.income"] <- 33299


sapply(data_for_pca[, c("National.income", "runway_length_ft", "runway_width_ft", "passengers")], class)
data_for_pca$passengers <- as.numeric(gsub(",", "", data_for_pca$passengers))
pca <- data_for_pca[, c("National.income", "runway_length_ft", "runway_width_ft", "passengers")]
pca_data_scaled <- scale(pca)

install.packages("Factoshiny")
install.packages("FactoInvestigate")
install.packages("DataExplorer") #pour des analyses uni et bi-variees rapides
install.packages("corrplot")
install.packages("FactoMineR")
library("DataExplorer")
library("FactoMineR")
library("Factoshiny")

V <- cov(pca_data_scaled) #matrice des covariances
R <- cor(pca_data_scaled) #matrice des correlation

corrplot::corrplot(R, type = "upper", method = "square")
res_pca <- PCA(X = pca_data_scaled,
               scale.unit = TRUE, # On précise qu'on souhaite réaliser une ACP sur données réduites
               graph = FALSE)     # On désactive les graphiques automatiques

# On peut représenter le résultat de l'acp par la suite
plot.PCA(res_pca, axes = c(1,2), choix = "ind")
plot.PCA(res_pca, axes = c(1,2), choix = "var")

res_pca$eig
barplot(res_pca$eig[,1])

PCAshiny(X = res_pca)
