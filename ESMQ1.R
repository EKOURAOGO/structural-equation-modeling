# =========================================================
# QUESTION 1 - EXPLORATION DES DONNÉES
# =========================================================

# =========================================================
# 0. PACKAGES
# =========================================================
library(dplyr)
library(ggplot2)
library(corrplot)

# =========================================================
# 1. IMPORTATION DES DONNÉES
# =========================================================
data <- read.table(
  "C:/Users/Pc/OneDrive/Bureau/Dossier/IMSD/ESM/mobile 1-10.txt",
  header = TRUE,
  sep = "\t"
)

cat("=========================================================\n")
cat("1. IMPORTATION DES DONNÉES\n")
cat("=========================================================\n")
cat("Dimensions de la base brute :", nrow(data), "lignes et", ncol(data), "colonnes\n\n")

cat("Noms des variables :\n")
print(names(data))

cat("\nStructure des données :\n")
str(data)

# =========================================================
# 2. EXCLUSION DE LA VARIABLE CUSCO
# =========================================================
# Le TP indique que le modèle étudié est une version simplifiée
# du modèle ECSI dans laquelle la dimension "plaintes" n'est pas retenue.
# La variable CUSCO, présente dans le fichier brut mais absente
# du modèle simplifié utilisé ensuite, est donc retirée de l'analyse.
data_sem <- data %>% select(-CUSCO)

cat("\n=========================================================\n")
cat("2. EXCLUSION DE LA VARIABLE CUSCO\n")
cat("=========================================================\n")
cat("Dimensions après exclusion :", nrow(data_sem), "lignes et", ncol(data_sem), "colonnes\n\n")

cat("Variables conservées :\n")
print(names(data_sem))

# =========================================================
# 3. CONTRÔLES DE QUALITÉ DE BASE
# =========================================================
cat("\n=========================================================\n")
cat("3. CONTRÔLES DE QUALITÉ DE BASE\n")
cat("=========================================================\n")

# 3.1 Valeurs manquantes
na_par_variable <- colSums(is.na(data_sem))
cat("\n--- Valeurs manquantes par variable ---\n")
print(na_par_variable)
cat("\nNombre total de valeurs manquantes :", sum(is.na(data_sem)), "\n")

# 3.2 Vérification des bornes
mins <- sapply(data_sem, min, na.rm = TRUE)
maxs <- sapply(data_sem, max, na.rm = TRUE)

cat("\n--- Minimums observés ---\n")
print(mins)

cat("\n--- Maximums observés ---\n")
print(maxs)

# 3.3 Nombre de valeurs distinctes
nb_modalites <- sapply(data_sem, function(x) length(unique(x)))
cat("\n--- Nombre de valeurs distinctes par variable ---\n")
print(nb_modalites)

# =========================================================
# 4. STATISTIQUES DESCRIPTIVES
# =========================================================
cat("\n=========================================================\n")
cat("4. STATISTIQUES DESCRIPTIVES\n")
cat("=========================================================\n")

desc_stats <- data.frame(
  Variable   = names(data_sem),
  Moyenne    = sapply(data_sem, mean, na.rm = TRUE),
  Ecart_Type = sapply(data_sem, sd, na.rm = TRUE),
  Minimum    = sapply(data_sem, min, na.rm = TRUE),
  Q1         = sapply(data_sem, quantile, probs = 0.25, na.rm = TRUE),
  Mediane    = sapply(data_sem, median, na.rm = TRUE),
  Q3         = sapply(data_sem, quantile, probs = 0.75, na.rm = TRUE),
  Maximum    = sapply(data_sem, max, na.rm = TRUE),
  row.names  = NULL
)

desc_stats[, -1] <- round(desc_stats[, -1], 2)
print(desc_stats)

# =========================================================
# 5. VISUALISATIONS UNIVARIÉES
# =========================================================
cat("\n=========================================================\n")
cat("5. VISUALISATIONS UNIVARIÉES\n")
cat("=========================================================\n")

data_long <- stack(as.data.frame(data_sem))
colnames(data_long) <- c("Valeur", "Variable")

# 5.1 Histogrammes
ggplot(data_long, aes(x = Valeur)) +
  geom_histogram(binwidth = 1, boundary = 0.5, closed = "right") +
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Distribution des variables manifestes",
    x = "Score",
    y = "Fréquence"
  )

# 5.2 Boxplots
ggplot(data_long, aes(x = Variable, y = Valeur)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Boxplots des variables manifestes",
    x = "",
    y = "Score"
  )

# =========================================================
# 6. MATRICE DE CORRÉLATION
# =========================================================
cat("\n=========================================================\n")
cat("6. MATRICE DE CORRÉLATION\n")
cat("=========================================================\n")

cor_mat <- cor(data_sem, use = "complete.obs")
cat("\n--- Matrice de corrélation arrondie ---\n")
print(round(cor_mat, 2))

corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.cex = 0.7,
  addCoef.col = "black",
  number.cex = 0.5,
  number.digits = 2,
  diag = FALSE
)

# =========================================================
# 7. DÉFINITION DES BLOCS LATENTS
# =========================================================
blocs <- list(
  Image = c("IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5"),
  Attentes = c("CUEX1", "CUEX2", "CUEX3"),
  Qualite_percue = c("PERQ1", "PERQ2", "PERQ3", "PERQ4", "PERQ5", "PERQ6", "PERQ7"),
  Valeur_percue = c("PERV1", "PERV2"),
  Satisfaction = c("CUSA1", "CUSA2", "CUSA3"),
  Fidelite = c("CUSL1", "CUSL2", "CUSL3")
)

# =========================================================
# 8. RÉSUMÉ DESCRIPTIF PAR BLOC
# =========================================================
resume_blocs <- lapply(names(blocs), function(b) {
  vars <- blocs[[b]]
  x <- unlist(data_sem[, vars], use.names = FALSE)
  
  data.frame(
    Bloc = b,
    Nb_variables = length(vars),
    Moyenne_globale = mean(x, na.rm = TRUE),
    Ecart_type_global = sd(x, na.rm = TRUE),
    Minimum = min(x, na.rm = TRUE),
    Maximum = max(x, na.rm = TRUE)
  )
}) %>% bind_rows()

resume_blocs[, -1] <- round(resume_blocs[, -1], 2)

cat("\n=========================================================\n")
cat("8. RÉSUMÉ DESCRIPTIF PAR BLOC\n")
cat("=========================================================\n")
print(resume_blocs)