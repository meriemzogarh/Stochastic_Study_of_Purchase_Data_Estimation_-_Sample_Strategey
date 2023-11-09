library(readxl)
library(tidyverse)


chemin_fichier <- "sales_data_sample.xlsx"


data <- read_excel(chemin_fichier)


head(data)

# Afficher des informations sur les variables
str(data)

# Résumé statistique des variables numériques
summary(data)

valeurs_uniques <- unique(data$PRODUCTLINE)

# Afficher les valeurs uniques
print(valeurs_uniques)

# Nombre d'échantillons à créer
num_samples <- 5

# Diviser les données en sous-groupes basés sur la ligne de produit (PRODUCTLINE)
data_by_product <- split(data, data$PRODUCTLINE)

# Initialiser une liste pour stocker les échantillons
sample_list <- list()

# Boucle pour créer les échantillons
for (i in 1:num_samples) {
  # Échantillonner chaque sous-groupe
  sampled_data <- lapply(data_by_product, function(df) {
    dplyr::sample_n(df, size = nrow(df), replace = TRUE)
  })
  
  # Ajouter les échantillons à la liste
  sample_list[[i]] <- sampled_data
}

# Vérifier les échantillons de chaque groupe (par exemple, pour le premier échantillon)
print(sample_list[[1]]$Motorcycles)
print(sample_list[[1]]$'Classic Cars')
print(sample_list[[1]]$'Trucks and Buses')
print(sample_list[[1]]$'Vintage Cars')
print(sample_list[[1]]$Planes)
print(sample_list[[1]]$Ships)
print(sample_list[[1]]$Trains)

# Initialiser une liste pour stocker les statistiques descriptives
descriptive_stats_list <- list()

# Boucle pour calculer les statistiques descriptives pour chaque échantillon
for (i in 1:num_samples) {
  descriptive_stats <- lapply(sample_list[[i]], function(df) {
    dplyr::summarise(df,
                     mean_quantity = mean(QUANTITYORDERED),
                     median_quantity = median(QUANTITYORDERED),
                     sd_quantity = sd(QUANTITYORDERED),
                     min_quantity = min(QUANTITYORDERED),
                     max_quantity = max(QUANTITYORDERED))
  })
  
  # Ajouter les statistiques descriptives à la liste
  descriptive_stats_list[[i]] <- descriptive_stats
}

# Vérifier les statistiques descriptives de chaque groupe (par exemple, pour le premier échantillon)
print("Échantillon 1: Motorcycles")
print(descriptive_stats_list[[1]]$Motorcycles)

print("Échantillon 1: Classic Cars")
print(descriptive_stats_list[[1]]$'Classic Cars')

print("Échantillon 1: Trucks and Buses")
print(descriptive_stats_list[[1]]$'Trucks and Buses')

print("Échantillon 1: Vintage Cars")
print(descriptive_stats_list[[1]]$'Vintage Cars')

print("Échantillon 1: Planes")
print(descriptive_stats_list[[1]]$Planes)

print("Échantillon 1: Ships")
print(descriptive_stats_list[[1]]$Ships)

print("Échantillon 1: Trains")
print(descriptive_stats_list[[1]]$Trains)


# Initialiser une liste pour stocker les estimations ponctuelles
point_estimates_list <- list()

# Boucle pour calculer l'estimation ponctuelle pour chaque échantillon
for (i in 1:num_samples) {
  point_estimates <- lapply(sample_list[[i]], function(df) {
    dplyr::summarise(df,
              mean_quantity_estimation = mean(QUANTITYORDERED))
  })
  
  # Ajouter l'estimation ponctuelle à la liste
  point_estimates_list[[i]] <- point_estimates
}

# Vérifier l'estimation ponctuelle de chaque groupe
print("Échantillon 1: Motorcycles")
print(point_estimates_list[[1]]$Motorcycles)

print("Échantillon 1: Classic Cars")
print(point_estimates_list[[1]]$`Classic Cars`)

print("Échantillon 1: Trucks and Buses")
print(point_estimates_list[[1]]$`Trucks and Buses`)

print("Échantillon 1: Vintage Cars")
print(point_estimates_list[[1]]$`Vintage Cars`)

print("Échantillon 1: Planes")
print(point_estimates_list[[1]]$Planes)

print("Échantillon 1: Ships")
print(point_estimates_list[[1]]$Ships)

print("Échantillon 1: Trains")
print(point_estimates_list[[1]]$Trains)

