# Analyse des Infrastructures Aéroportuaires

## 📝 Description
Ce projet analyse l'impact des revenus nationaux sur le développement des infrastructures aéroportuaires à travers le monde. Il combine des méthodes statistiques avancées (AFC et ACP) pour étudier les relations entre les types d'aéroports, la richesse des nations et le trafic passager.

## 📊 Méthodologie
- **Analyse Factorielle des Correspondances (AFC)** : Étude des liens entre pays et types d'aéroports
- **Analyse en Composantes Principales (ACP)** : Exploration des relations revenus/trafic/dimensions des pistes

## 🔍 Principaux Résultats
- Les pays riches investissent davantage dans les grands aéroports
- Les États-Unis et la Chine dominent en nombre d'aéroports moyens
- Le Canada et le Brésil privilégient les hydravions pour les zones isolées
- Le Japon et la Corée du Sud ont développé de nombreux héliports

## 🛠️ Structure du Projet# Projet Analysis Homework
  📦 Projet
📦 ICA_application
├── 📂 data/ # Jeux de données bruts
│ ├── airports.csv
│ └── national_income.csv
├── 📂 renv/ # Configuration environnement R
├── 📂 src/ # Scripts d'analyse
│ ├── AFC_analysis.R
│ └── PCA_analysis.R
├── 📂 docs/ # Documentation technique
├── .gitignore
└── .DS_Store


## ⚙️ Installation
1. Cloner le dépôt :
```bash
git clone https://github.com/ravel/main/ICA_application.git
install.packages(c("FactoMineR", "ggplot2", "dplyr"))
```

## 📈 Visualisations
Graphiques AFC des types d'aéroports par pays

Matrices de corrélation ACP

Cartes de distribution géographique

## 📚 Sources de Données
World Airports Data

Liste des aéroports les plus fréquentés

## 👥 Auteurs
Ismaël Madou Gagi Grema

Nikita Pomozov

## 📅 Dernière Mise à Jour
18 décembre 2024