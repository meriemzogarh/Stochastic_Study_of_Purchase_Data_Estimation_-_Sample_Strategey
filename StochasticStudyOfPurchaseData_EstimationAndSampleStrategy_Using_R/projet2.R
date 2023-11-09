# Charger les bibliothèques
library(readxl)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(plotly)


#importation du dataset
chemin_fichier <- "sales_data_sample.xlsx"
data <- read_excel(chemin_fichier)
print(data)

# Isoler la colonne "QUANTITYORDERED"
population <- data$QUANTITYORDERED

# Afficher le résultat
print(population)

# Données pour la population
pop <- c(Moyenne = 35.092809, Variance = 94.895707, `Écart-type` = 9.741443)

# Créer un data.frame pour les données de la pop
donnees_pop <- data.frame(Mesure = names(pop),
                                 Valeur = pop)

# Créer un graphique à barres pour la pop
barplot_pop <- ggplot(data = donnees_pop, aes(x = Mesure, y = Valeur, fill = Mesure)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Valeur, 2)), vjust = -0.5, color = "black") +  # Ajouter les étiquettes des valeurs
  labs(title = "Valeurs de la Moyenne, Variance et Écart-type de la pop",
       x = "Mesure",
       y = "Valeur") +
  theme_minimal() +
  theme(legend.position = "none")

# Afficher le graphique à barres pour la pop
print(barplot_pop)



# Calculer la moyenne
mean_value <- mean(population)
# Calculer la médiane
median_value <- median(population)
# Calculer la variance
variance_value <- var(population)
# Calculer le minimum
min_value <- min(population) 
# Calculer le maximum
max_value <- max(population)
# Calculer l'espérance (qui est équivalente à la moyenne)
expectation_value <- mean_value
# Calculer l'écart-type
sd_value <- sd(population)

print(min_value)

set.seed(123)  # Pour la reproductibilité des résultats

sample_size1 <- 15
sample_size2 <- 25
sample_size3 <- 35
sample_size4 <- 45

echantillon1 <- sample(population, size = sample_size1, replace = length(population) < sample_size1)


mean_echantillon1 <- mean(echantillon1)
median_echantillon1 <- median(echantillon1)
variance_echantillon1 <- var(echantillon1)
min_echantillon1 <- min(echantillon1)
max_echantillon1 <- max(echantillon1)
expectation_echantillon1 <- mean_echantillon1
sd_echantillon1 <- sd(echantillon1)


echantillon2 <- sample(population, size = sample_size2, replace = length(population) < sample_size2)

mean_echantillon2 <- mean(echantillon2)
median_echantillon2 <- median(echantillon2)
variance_echantillon2 <- var(echantillon2)
min_echantillon2 <- min(echantillon2)
max_echantillon2 <- max(echantillon2)
expectation_echantillon2 <- mean_echantillon2
sd_echantillon2 <- sd(echantillon2)

echantillon3 <- sample(population, size = sample_size3, replace = length(population) < sample_size3)

mean_echantillon3 <- mean(echantillon3)
median_echantillon3 <- median(echantillon3)
variance_echantillon3 <- var(echantillon3)
min_echantillon3 <- min(echantillon3)
max_echantillon3 <- max(echantillon3)
expectation_echantillon3 <- mean_echantillon3
sd_echantillon3 <- sd(echantillon3)

echantillon4 <- sample(population, size = sample_size4, replace = length(population) < sample_size4)

mean_echantillon4 <- mean(echantillon4)
median_echantillon4 <- median(echantillon4)
variance_echantillon4 <- var(echantillon4)
min_echantillon4 <- min(echantillon4)
max_echantillon4 <- max(echantillon4)
expectation_echantillon4 <- mean_echantillon4
sd_echantillon4 <- sd(echantillon4)



#rassembler les paramètres de la population et des échantillons dans un seul dataframe
statistiques <- data.frame(
  Paramètre = c("Moyenne", "Médiane", "Variance", "Minimum", "Maximum", "Espérance", "Écart-type"),
  Colonne_d_origine = c(mean_value, median_value, variance_value, min_value, max_value, expectation_value, sd_value),
  Echantillon_1 = c(mean_echantillon1, median_echantillon1, variance_echantillon1, min_echantillon1, max_echantillon1, expectation_echantillon1, sd_echantillon1),
  Echantillon_2 = c(mean_echantillon2, median_echantillon2, variance_echantillon2, min_echantillon2, max_echantillon2, expectation_echantillon2, sd_echantillon2),
  Echantillon_3 = c(mean_echantillon3, median_echantillon3, variance_echantillon3, min_echantillon3, max_echantillon3, expectation_echantillon3, sd_echantillon3),
  Echantillon_4 = c(mean_echantillon4, median_echantillon4, variance_echantillon4, min_echantillon4, max_echantillon4, expectation_echantillon4, sd_echantillon4)
  )

# Afficher le tableau formaté
kable(statistiques, format = "html", caption = "Statistiques pour la colonne et les échantillons") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)


# Convertir le dataframe en format long pour faciliter la création du graphique
statistiques_long <- tidyr::pivot_longer(statistiques, -Paramètre, names_to = "Groupe", values_to = "Valeur")

# Créer un graphique en barres pour comparer les moyennes des quatres groupes
ggplot(statistiques_long, aes(x = Paramètre, y = Valeur, fill = Groupe)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparaison des statistiques", x = "Paramètre", y = "Valeur") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Créer un vecteur pour stocker les tailles des échantillons
tailles_echantillons <- c(sample_size1, sample_size2, sample_size3, sample_size4)

# Créer un vecteur pour stocker les moyennes des estimateurs pour chaque échantillon
moyennes <- c(mean_echantillon1, mean_echantillon2, mean_echantillon3, mean_echantillon4)

# Créer un vecteur pour stocker les variances des estimateurs pour chaque échantillon
variances <- c(variance_echantillon1, variance_echantillon2, variance_echantillon3, variance_echantillon4)

# Créer un vecteur pour stocker les écarts-types des estimateurs pour chaque échantillon
ecart_type <- c(sd_echantillon1, sd_echantillon2, sd_echantillon3, sd_echantillon4)

# Créer un data.frame pour les données
donnees <- data.frame(Taille_echantillon = tailles_echantillons,
                      Moyenne = moyennes,
                      Variance = variances,
                      EcartType = ecart_type,
                      Type = c("Echantillon 1", "Echantillon 2", "Echantillon 3", "Echantillon 4"))


graphique <- ggplot(data = donnees, aes(x = Type, group = 1)) +
  geom_line(aes(y = Moyenne), color = "blue", linetype = "solid") +
  geom_point(aes(y = Moyenne), color = "blue") +
  geom_line(aes(y = Variance), color = "red", linetype = "dotted") +
  geom_point(aes(y = Variance), color = "red") +
  geom_line(aes(y = EcartType), color = "green", linetype = "dashed") +
  geom_point(aes(y = EcartType), color = "green") +
  geom_text(aes(label = round(Moyenne, 2), y = Moyenne + 0.5), vjust = -0.5, color = "blue", size = 3) +
  geom_text(aes(label = round(Variance, 2), y = Variance - 1), vjust = 0.5, color = "red", size = 3) +
  geom_text(aes(label = round(EcartType, 2), y = EcartType - 1), vjust = 0.5, color = "green", size = 3) +
  labs(title = "Estimateur de la moyenne / Variance / Écart-type",
       x = "Echantillon",
       y = "moyenne / Variance / Écart-type") +
  scale_x_discrete(labels = c("Echantillon 1 (n=15)", "Echantillon 2 (n=25)", "Echantillon 3 (n=35)", "Echantillon 4 (n=45)")) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Variance / Écart-type")) +
  theme_minimal()

# Afficher le graphique
print(graphique)


# Données des échantillons
echantillon_1 <- c(34.800000, echantillon1) 
echantillon_2 <- c(32.46667, echantillon2) 
echantillon_3 <- c(36.800000, echantillon3) 
echantillon_4 <- c(35.26667, echantillon4) 

# Calcul de l'intervalle de confiance pour chaque échantillon
conf_int_1 <- t.test(echantillon_1, conf.level = 0.95)$conf.int
conf_int_2 <- t.test(echantillon_2, conf.level = 0.95)$conf.int
conf_int_3 <- t.test(echantillon_3, conf.level = 0.95)$conf.int
conf_int_4 <- t.test(echantillon_4, conf.level = 0.95)$conf.int

# Affichage des intervalles de confiance
print(conf_int_1)
print(conf_int_2)
print(conf_int_3)
print(conf_int_4)



# Rassembler les paramètres de la population et des échantillons dans un seul dataframe
statistiques <- data.frame(
  Paramètre = c("Variance"),
  Echantillon_1 = c(variance_echantillon1),
  Echantillon_2 = c(variance_echantillon2),
  Echantillon_3 = c(variance_echantillon3),
  Echantillon_4 = c(variance_echantillon4)
)

# Convertir le dataframe en format long pour faciliter la création du graphique
statistiques_long <- tidyr::pivot_longer(statistiques, -Paramètre, names_to = "Groupe", values_to = "Valeur")

# Créer un graphique en barres pour comparer les moyennes des quatre echantillons
ggplot(statistiques_long, aes(x = Paramètre, y = Valeur, fill = Groupe)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2f", Valeur)), position = position_dodge(width = 0.9), vjust = -0.5) +  # Ajouter les étiquettes avec deux chiffres après la virgule
  labs(title = "Comparaison des des variances des échantillons", x = "Paramètre", y = "Valeur") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#afficher le contenu de chaque echantillon

print(echantillon1)
print(echantillon2)
print(echantillon3)
print(echantillon4)

print(population)

# Calcul de l'étendue pour la population
etendue_population <- range(population)

# Calcul de l'étendue pour chaque échantillon
etendue_echantillon1 <- range(echantillon1)
etendue_echantillon2 <- range(echantillon2)
etendue_echantillon3 <- range(echantillon3)
etendue_echantillon4 <- range(echantillon4)

# Affichage des résultats
cat("Étendue de la population : ", etendue_population[1], " à ", etendue_population[2], "\n")
cat("Étendue de l'échantillon 1 : ", etendue_echantillon1[1], " à ", etendue_echantillon1[2], "\n")
cat("Étendue de l'échantillon 2 : ", etendue_echantillon2[1], " à ", etendue_echantillon2[2], "\n")
cat("Étendue de l'échantillon 3 : ", etendue_echantillon3[1], " à ", etendue_echantillon3[2], "\n")
cat("Étendue de l'échantillon 4 : ", etendue_echantillon4[1], " à ", etendue_echantillon4[2], "\n")

# Créer un vecteur pour stocker les valeurs min et max de la population et des échantillons
min_values <- c(min_value, min_echantillon1, min_echantillon2, min_echantillon3, min_echantillon4)
max_values <- c(max_value, max_echantillon1, max_echantillon2, max_echantillon3, max_echantillon4)

# Créer un vecteur pour stocker les noms des échantillons
echantillon_noms <- c("Population", "echantillon1", "echantillon2", "echantillon3", "echantillon4")

# Créer un data.frame pour les données
donnees_min_max <- data.frame(Echantillon = echantillon_noms,
                              Min = min_values,
                              Max = max_values)

# Créer un graphique à barres avec les intervalles min et max
barplot_min_max <- ggplot(data = donnees_min_max, aes(x = Echantillon, y = Max - Min, fill = Echantillon)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_label(aes(label = Max), vjust = -0.5, position = position_dodge(width = 0.9)) +
  geom_label(aes(label = Min), vjust = 1.5, position = position_dodge(width = 0.9)) +
  labs(title = "L'étendue de la Population et des Échantillons",
       x = "Échantillon",
       y = "Étendue") +
  theme_minimal()

print(barplot_min_max)

diff_moyennes <- numeric(4)
diff_moyennes[1] <- abs(mean(echantillon1)-mean(population))
diff_moyennes[2] <- abs(mean(echantillon2)-mean(population))
diff_moyennes[3] <- abs(mean(echantillon3)-mean(population))
diff_moyennes[4] <- abs(mean(echantillon4)-mean(population))

ggplot(data = data.frame(x = seq_along(diff_moyennes), y = diff_moyennes), aes(x = x, y = y)) +
  geom_line(color = "red") +
  theme_get() +
  labs(x = "Echantillons", y = "Difference avec la population", title = "Graphe de Convergence")


print(diff_moyennes)