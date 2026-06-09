# =========================================================
# QUESTION 4 - MÉTHODE LISREL AVEC LAVAAN
# =========================================================
#install.packages("semTools")
library(lavaan)
library(semTools)
library(dplyr)

cat("=========================================================\n")
cat("QUESTION 4 - MÉTHODE LISREL AVEC LAVAAN\n")
cat("=========================================================\n\n")

# =========================================================
# 1. SPÉCIFICATION DU MODÈLE ECSI SIMPLIFIÉ
# =========================================================
modele_lisrel <- '
# ---------------------------------------------------------
# Modèle de mesure
# ---------------------------------------------------------
IMAG =~ IMAG1 + IMAG2 + IMAG3 + IMAG4 + IMAG5
CUEX =~ CUEX1 + CUEX2 + CUEX3
PERQ =~ PERQ1 + PERQ2 + PERQ3 + PERQ4 + PERQ5 + PERQ6 + PERQ7
PERV =~ PERV1 + PERV2
CUSA =~ CUSA1 + CUSA2 + CUSA3
CUSL =~ CUSL1 + CUSL2 + CUSL3

# ---------------------------------------------------------
# Modèle structurel
# ---------------------------------------------------------
CUEX ~ IMAG
PERQ ~ IMAG + CUEX
PERV ~ CUEX + PERQ
CUSA ~ IMAG + PERQ + PERV
CUSL ~ IMAG + CUSA
'

cat("Modèle spécifié sous lavaan :\n")
cat(modele_lisrel, "\n")

# =========================================================
# 2. ESTIMATION DU MODÈLE
# =========================================================
fit_lisrel <- sem(
  model = modele_lisrel,
  data = data_sem,
  std.lv = TRUE,
  estimator = "MLR",      # estimateur robuste
  missing = "fiml"
)

# =========================================================
# 3. RÉSUMÉ GÉNÉRAL DU MODÈLE
# =========================================================
cat("\n=========================================================\n")
cat("3. RÉSUMÉ GÉNÉRAL DU MODÈLE\n")
cat("=========================================================\n")

summary(
  fit_lisrel,
  standardized = TRUE,
  fit.measures = TRUE,
  rsquare = TRUE
)

# =========================================================
# 4. INDICES D’AJUSTEMENT GLOBAL
# =========================================================
cat("\n=========================================================\n")
cat("4. INDICES D’AJUSTEMENT GLOBAL\n")
cat("=========================================================\n")

fit_indices <- fitMeasures(
  fit_lisrel,
  c(
    "chisq", "df", "pvalue",
    "cfi", "tli",
    "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
    "srmr",
    "aic", "bic"
  )
)

fit_indices <- round(fit_indices, 4)
print(fit_indices)

# =========================================================
# 5. PARAMÈTRES STANDARDISÉS
# =========================================================
cat("\n=========================================================\n")
cat("5. PARAMÈTRES STANDARDISÉS\n")
cat("=========================================================\n")

params_std <- parameterEstimates(
  fit_lisrel,
  standardized = TRUE
)

# Charges factorielles
loadings_std <- params_std %>%
  filter(op == "=~") %>%
  select(lhs, rhs, est, se, z, pvalue, std.all)

cat("\n--- Charges factorielles standardisées ---\n")
print(loadings_std)

# Coefficients structurels
paths_std <- params_std %>%
  filter(op == "~", lhs %in% c("CUEX", "PERQ", "PERV", "CUSA", "CUSL")) %>%
  select(lhs, rhs, est, se, z, pvalue, std.all)

cat("\n--- Coefficients structurels standardisés ---\n")
print(paths_std)

# =========================================================
# 6. R² DES VARIABLES LATENTES ENDOGÈNES
# =========================================================
cat("\n=========================================================\n")
cat("6. R² DES VARIABLES LATENTES ENDOGÈNES\n")
cat("=========================================================\n")

r2_vals <- inspect(fit_lisrel, "r2")
print(round(r2_vals, 4))

# =========================================================
# 7. FIABILITÉ ET VALIDITÉ CONVERGENTE
# =========================================================
cat("\n=========================================================\n")
cat("7. FIABILITÉ ET VALIDITÉ CONVERGENTE\n")
cat("=========================================================\n")

# Fiabilité composite et AVE
reliab <- semTools::reliability(fit_lisrel)

cat("\n--- Fiabilité composite / AVE ---\n")
print(round(reliab, 4))

# =========================================================
# 8. VALIDITÉ DISCRIMINANTE
# =========================================================
cat("\n=========================================================\n")
cat("8. VALIDITÉ DISCRIMINANTE\n")
cat("=========================================================\n")

# Corrélations entre variables latentes
lv_cor <- inspect(fit_lisrel, "cor.lv")
cat("\n--- Corrélations entre variables latentes ---\n")
print(round(lv_cor, 4))

# Racine carrée de l'AVE
ave_vals <- reliab["avevar", ]
sqrt_ave <- sqrt(ave_vals)

cat("\n--- Racine carrée de l'AVE ---\n")
print(round(sqrt_ave, 4))

# Matrice de Fornell-Larcker
fornell <- lv_cor
diag(fornell) <- sqrt_ave[colnames(fornell)]

cat("\n--- Matrice de Fornell-Larcker ---\n")
print(round(fornell, 4))

# =========================================================
# 9. RÉSIDUS ET INDICES DE MODIFICATION
# =========================================================
cat("\n=========================================================\n")
cat("9. RÉSIDUS ET INDICES DE MODIFICATION\n")
cat("=========================================================\n")

# Résidus standardisés
residus_std <- residuals(fit_lisrel, type = "standardized")
cat("\n--- Résidus standardisés (matrice de covariance) ---\n")
print(round(residus_std$cov, 3))

# Indices de modification les plus élevés
mi <- modificationIndices(fit_lisrel, sort. = TRUE)

cat("\n--- Principaux indices de modification ---\n")
print(head(mi, 15))

# =========================================================
# 10. TABLEAUX SYNTHÉTIQUES
# =========================================================
cat("\n=========================================================\n")
cat("10. TABLEAUX SYNTHÉTIQUES\n")
cat("=========================================================\n")

# Tableau synthèse des charges
table_loadings <- loadings_std %>%
  mutate(
    Interpretation = case_when(
      std.all >= 0.70 ~ "Très bon",
      std.all >= 0.50 ~ "Acceptable",
      TRUE ~ "Faible"
    )
  )

cat("\n--- Synthèse des charges factorielles ---\n")
print(table_loadings)

# Tableau synthèse des chemins
table_paths <- paths_std %>%
  mutate(
    Significatif = ifelse(pvalue < 0.05, "Oui", "Non")
  )

cat("\n--- Synthèse des chemins structurels ---\n")
print(table_paths)