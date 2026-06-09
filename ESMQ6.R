# =========================================================
# QUESTION 6 - ESTIMATION DU MODÈLE PAR PLS-PM
# =========================================================

library(plspm)
library(dplyr)

cat("=========================================================\n")
cat("QUESTION 6 - ESTIMATION DU MODÈLE PAR PLS-PM\n")
cat("=========================================================\n\n")

# =========================================================
# 1. DÉFINITION DES BLOCS DE MESURE
# =========================================================
blocs_pls <- list(
  IMAG = c("IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5"),
  CUEX = c("CUEX1", "CUEX2", "CUEX3"),
  PERQ = c("PERQ1", "PERQ2", "PERQ3", "PERQ4", "PERQ5", "PERQ6", "PERQ7"),
  PERV = c("PERV1", "PERV2"),
  CUSA = c("CUSA1", "CUSA2", "CUSA3"),
  CUSL = c("CUSL1", "CUSL2", "CUSL3")
)

cat("Blocs de mesure :\n")
print(blocs_pls)

# =========================================================
# 2. MATRICE DU MODÈLE INTERNE
# =========================================================
# plspm exige une matrice triangulaire inférieure
# Ordre des construits : IMAG, CUEX, PERQ, PERV, CUSA, CUSL

inner <- rbind(
  IMAG = c(0, 0, 0, 0, 0, 0),
  CUEX = c(1, 0, 0, 0, 0, 0),
  PERQ = c(1, 1, 0, 0, 0, 0),
  PERV = c(0, 1, 1, 0, 0, 0),
  CUSA = c(1, 0, 1, 1, 0, 0),
  CUSL = c(1, 0, 0, 0, 1, 0)
)

colnames(inner) <- rownames(inner) <- c("IMAG", "CUEX", "PERQ", "PERV", "CUSA", "CUSL")

cat("\n=========================================================\n")
cat("MATRICE DU MODÈLE INTERNE\n")
cat("=========================================================\n")
print(inner)

# =========================================================
# 3. MODES DE MESURE
# =========================================================
# Tous les blocs sont réflexifs
modes <- rep("A", 6)

cat("\nModes de mesure :\n")
print(modes)

# =========================================================
# 4. ESTIMATION DU MODÈLE PLS-PM
# =========================================================
pls_ecsi <- plspm(
  Data = data_sem,
  path_matrix = inner,
  blocks = blocs_pls,
  modes = modes,
  scaled = TRUE
)

cat("\n=========================================================\n")
cat("RÉSUMÉ GÉNÉRAL DU MODÈLE PLS-PM\n")
cat("=========================================================\n")
print(summary(pls_ecsi))

# =========================================================
# 5. COEFFICIENTS DE CHEMIN
# =========================================================
cat("\n=========================================================\n")
cat("5. COEFFICIENTS DE CHEMIN\n")
cat("=========================================================\n")

cat("\n--- Matrice des coefficients de chemin ---\n")
print(round(pls_ecsi$path_coefs, 4))

# =========================================================
# 6. R² DES VARIABLES LATENTES ENDOGÈNES
# =========================================================
cat("\n=========================================================\n")
cat("6. R² DES VARIABLES LATENTES ENDOGÈNES\n")
cat("=========================================================\n")

print(pls_ecsi$inner_summary)

r2_pls <- data.frame(
  Bloc = rownames(pls_ecsi$inner_summary),
  R2 = pls_ecsi$inner_summary[, "R2"]
)

cat("\n--- Tableau synthétique des R² ---\n")
print(r2_pls)

# =========================================================
# 7. QUALITÉ DU MODÈLE DE MESURE
# =========================================================
cat("\n=========================================================\n")
cat("7. QUALITÉ DU MODÈLE DE MESURE\n")
cat("=========================================================\n")

cat("\n--- Outer model : loadings, weights, communalities ---\n")
print(pls_ecsi$outer_model)

cat("\n--- Indices d'unidimensionnalité ---\n")
print(pls_ecsi$unidim)

cat("\n--- Goodness of Fit global ---\n")
print(pls_ecsi$gof)

# =========================================================
# 8. TABLEAUX SYNTHÉTIQUES
# =========================================================
cat("\n=========================================================\n")
cat("8. TABLEAUX SYNTHÉTIQUES\n")
cat("=========================================================\n")

# Synthèse des loadings
table_loadings_pls <- pls_ecsi$outer_model %>%
  select(name, block, weight, loading, communality) %>%
  mutate(
    Interpretation_loading = case_when(
      abs(loading) >= 0.70 ~ "Très bon",
      abs(loading) >= 0.50 ~ "Acceptable",
      TRUE ~ "Faible"
    )
  )

cat("\n--- Synthèse des loadings ---\n")
print(table_loadings_pls)

# Synthèse de l'unidimensionnalité
table_unidim_pls <- pls_ecsi$unidim %>%
  mutate(
    Interpretation_alpha = case_when(
      `C.alpha` >= 0.70 ~ "Bon",
      `C.alpha` >= 0.60 ~ "Acceptable",
      TRUE ~ "Faible"
    ),
    Interpretation_dg = case_when(
      DG.rho >= 0.70 ~ "Bon",
      DG.rho >= 0.60 ~ "Acceptable",
      TRUE ~ "Faible"
    )
  )

cat("\n--- Synthèse de l'unidimensionnalité ---\n")
print(table_unidim_pls)

# =========================================================
# 9. EFFETS DIRECTS, INDIRECTS ET TOTAUX
# =========================================================
cat("\n=========================================================\n")
cat("9. EFFETS DIRECTS, INDIRECTS ET TOTAUX\n")
cat("=========================================================\n")

print(pls_ecsi$effects)

# =========================================================
# 10. VISUALISATIONS
# =========================================================
cat("\n=========================================================\n")
cat("10. VISUALISATIONS\n")
cat("=========================================================\n")

plot(pls_ecsi, what = "inner")
plot(pls_ecsi, what = "outer")

# =========================================================
# 11. BOOTSTRAP
# =========================================================
cat("\n=========================================================\n")
cat("11. BOOTSTRAP\n")
cat("=========================================================\n")

set.seed(123)

pls_boot <- plspm(
  Data = data_sem,
  path_matrix = inner,
  blocks = blocs_pls,
  modes = modes,
  scaled = TRUE,
  boot.val = TRUE,
  br = 500
)

cat("\n--- Résultats bootstrap des chemins ---\n")
print(pls_boot$boot$paths)

cat("\n--- Résultats bootstrap des poids externes ---\n")
print(pls_boot$boot$weights)

# =========================================================
# 12. EXTRACTIONS UTILES POUR LE COMMENTAIRE
# =========================================================
cat("\n=========================================================\n")
cat("12. EXTRACTIONS UTILES POUR L'INTERPRÉTATION\n")
cat("=========================================================\n")

cat("\n--- Path coefficients ---\n")
print(round(pls_ecsi$path_coefs, 4))

cat("\n--- Inner summary ---\n")
print(pls_ecsi$inner_summary)

cat("\n--- Outer model ---\n")
print(pls_ecsi$outer_model)

cat("\n--- Unidim ---\n")
print(pls_ecsi$unidim)

cat("\n--- GOF ---\n")
print(pls_ecsi$gof)

cat("\n--- Bootstrap paths ---\n")
print(pls_boot$boot$paths)