# =========================================================
# QUESTION 3 - VÉRIFICATION DE L’UNIDIMENSIONNALITÉ DES BLOCS
# =========================================================

library(dplyr)
library(psych)
library(FactoMineR)

cat("=========================================================\n")
cat("QUESTION 3 - VÉRIFICATION DE L’UNIDIMENSIONNALITÉ DES BLOCS\n")
cat("=========================================================\n\n")

# =========================================================
# 1. DÉFINITION DES BLOCS
# =========================================================
blocs <- list(
  IMAG = c("IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5"),
  CUEX = c("CUEX1", "CUEX2", "CUEX3"),
  PERQ = c("PERQ1", "PERQ2", "PERQ3", "PERQ4", "PERQ5", "PERQ6", "PERQ7"),
  PERV = c("PERV1", "PERV2"),
  CUSA = c("CUSA1", "CUSA2", "CUSA3"),
  CUSL = c("CUSL1", "CUSL2", "CUSL3")
)

# =========================================================
# 2. FONCTION D’ANALYSE D’UN BLOC
# =========================================================
analyse_bloc <- function(data_bloc, nom_bloc) {
  
  # Corrélation intra-bloc
  cor_bloc <- cor(data_bloc, use = "complete.obs")
  cor_moy <- if(ncol(data_bloc) > 1) {
    mean(cor_bloc[lower.tri(cor_bloc)])
  } else {
    NA
  }
  
  # ACP sur matrice centrée-réduite
  pca_bloc <- PCA(data_bloc, scale.unit = TRUE, graph = FALSE)
  eig <- pca_bloc$eig
  
  val_propre_1 <- eig[1, 1]
  val_propre_2 <- if(nrow(eig) >= 2) eig[2, 1] else NA
  var_expl_1 <- eig[1, 2]
  
  # Alpha de Cronbach
  alpha_val <- psych::alpha(data_bloc)$total$raw_alpha
  
  # Corrélation item-total corrigée (moyenne)
  alpha_obj <- psych::alpha(data_bloc)
  rit_moy <- mean(alpha_obj$item.stats$r.drop, na.rm = TRUE)
  
  # Conclusion automatique simple
  conclusion <- if (!is.na(val_propre_2) &&
                    val_propre_1 > 1 &&
                    val_propre_2 < 1 &&
                    alpha_val >= 0.70) {
    "Oui"
  } else if (!is.na(alpha_val) && alpha_val >= 0.60) {
    "A nuancer"
  } else {
    "Non"
  }
  
  list(
    nom_bloc = nom_bloc,
    cor_mat = cor_bloc,
    eig = eig,
    loadings = pca_bloc$var$coord[, 1, drop = FALSE],
    resume = data.frame(
      Bloc = nom_bloc,
      Nb_items = ncol(data_bloc),
      Correlation_moyenne_intra = round(cor_moy, 3),
      Valeur_propre_1 = round(val_propre_1, 3),
      Valeur_propre_2 = round(val_propre_2, 3),
      Variance_expliquee_axe1 = round(var_expl_1, 2),
      Alpha_Cronbach = round(alpha_val, 3),
      RIT_moyen = round(rit_moy, 3),
      Unidimensionnel = conclusion
    )
  )
}

# =========================================================
# 3. ANALYSE DE TOUS LES BLOCS
# =========================================================
resultats_blocs <- lapply(names(blocs), function(b) {
  analyse_bloc(data_sem[, blocs[[b]]], b)
})

# Tableau de synthèse
resume_unidim <- bind_rows(lapply(resultats_blocs, function(x) x$resume))

cat("=========================================================\n")
cat("TABLEAU DE SYNTHÈSE DE L’UNIDIMENSIONNALITÉ\n")
cat("=========================================================\n")
print(resume_unidim)

# =========================================================
# 4. DÉTAILS PAR BLOC
# =========================================================
for(res in resultats_blocs) {
  
  cat("\n=========================================================\n")
  cat("BLOC :", res$nom_bloc, "\n")
  cat("=========================================================\n")
  
  cat("\n--- Matrice de corrélations ---\n")
  print(round(res$cor_mat, 3))
  
  cat("\n--- Valeurs propres ---\n")
  print(round(res$eig, 3))
  
  cat("\n--- Loadings sur la première composante ---\n")
  print(round(res$loadings, 3))
  
  cat("\n--- Résumé du bloc ---\n")
  print(res$resume)
}

# =========================================================
# 5. VISUALISATION : VALEURS PROPRES PAR BLOC
# =========================================================
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

for(b in names(blocs)) {
  data_bloc <- data_sem[, blocs[[b]]]
  pca_bloc <- PCA(data_bloc, scale.unit = TRUE, graph = FALSE)
  eig <- pca_bloc$eig[, 1]
  
  barplot(
    eig,
    main = paste("Bloc", b),
    xlab = "Composantes",
    ylab = "Valeurs propres"
  )
  abline(h = 1, lty = 2)
}

par(mfrow = c(1, 1))