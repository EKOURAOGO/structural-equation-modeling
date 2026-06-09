# =========================================================
# QUESTION 7 - APPLICATION DE L’APPROCHE RFPC
# =========================================================

library(dplyr)

cat("=========================================================\n")
cat("QUESTION 7 - APPLICATION DE L’APPROCHE RFPC\n")
cat("=========================================================\n\n")

# =========================================================
# 0. CHARGEMENT DE LA FONCTION RFPC
# =========================================================
source("C:/Users/Pc/OneDrive/Bureau/Dossier/IMSD/ESM/RFPC_model (function).txt")

if(!exists("RFPC_pm")) {
  stop("La fonction RFPC_pm n'a pas pu être chargée.")
}

cat("Arguments de RFPC_pm :\n")
print(args(RFPC_pm))

# =========================================================
# 1. PRÉPARATION DES DONNÉES
# =========================================================
# Données centrées-réduites sous forme de data.frame numérique
data_rfpc <- as.data.frame(scale(data_sem))

cat("\nStructure des données RFPC :\n")
print(str(data_rfpc))

cat("\nTypes des colonnes :\n")
print(sapply(data_rfpc, class))

# =========================================================
# 2. DÉFINITION DES BLOCS
# =========================================================
# RFPC_pm semble attendre des indices de colonnes plutôt que des noms
blocs_rfpc <- list(
  IMAG = match(c("IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5"), colnames(data_rfpc)),
  CUEX = match(c("CUEX1", "CUEX2", "CUEX3"), colnames(data_rfpc)),
  PERQ = match(c("PERQ1", "PERQ2", "PERQ3", "PERQ4", "PERQ5", "PERQ6", "PERQ7"), colnames(data_rfpc)),
  PERV = match(c("PERV1", "PERV2"), colnames(data_rfpc)),
  CUSA = match(c("CUSA1", "CUSA2", "CUSA3"), colnames(data_rfpc)),
  CUSL = match(c("CUSL1", "CUSL2", "CUSL3"), colnames(data_rfpc))
)

cat("\nBlocs RFPC (indices de colonnes) :\n")
print(blocs_rfpc)

# =========================================================
# 3. MATRICE DU MODÈLE INTERNE
# =========================================================
inner_rfpc <- rbind(
  IMAG = c(0, 0, 0, 0, 0, 0),
  CUEX = c(1, 0, 0, 0, 0, 0),
  PERQ = c(1, 1, 0, 0, 0, 0),
  PERV = c(0, 1, 1, 0, 0, 0),
  CUSA = c(1, 0, 1, 1, 0, 0),
  CUSL = c(1, 0, 0, 0, 1, 0)
)

colnames(inner_rfpc) <- rownames(inner_rfpc) <- c("IMAG", "CUEX", "PERQ", "PERV", "CUSA", "CUSL")

cat("\n=========================================================\n")
cat("MATRICE DU MODÈLE INTERNE RFPC\n")
cat("=========================================================\n")
print(inner_rfpc)

# =========================================================
# 4. ESTIMATION DU MODÈLE RFPC
# =========================================================
rfpc_ecsi <- RFPC_pm(
  data = data_rfpc,
  path = inner_rfpc,
  blocks = blocs_rfpc
)

cat("\n=========================================================\n")
cat("RÉSULTATS BRUTS RFPC\n")
cat("=========================================================\n")
print(rfpc_ecsi)

# =========================================================
# 5. STRUCTURE DE L’OBJET RFPC
# =========================================================
cat("\n=========================================================\n")
cat("STRUCTURE DE L'OBJET RFPC\n")
cat("=========================================================\n")

print(class(rfpc_ecsi))

if(is.list(rfpc_ecsi)) {
  print(names(rfpc_ecsi))
} else {
  cat("L'objet RFPC n'est pas une liste.\n")
}

# =========================================================
# 6. EXTRACTIONS UTILES SI DISPONIBLES
# =========================================================
if(is.list(rfpc_ecsi)) {
  
  if("path_coefs" %in% names(rfpc_ecsi)) {
    cat("\n--- Coefficients de chemin RFPC ---\n")
    print(round(rfpc_ecsi$path_coefs, 4))
  }
  
  if("inner_summary" %in% names(rfpc_ecsi)) {
    cat("\n--- Résumé du modèle interne RFPC ---\n")
    print(rfpc_ecsi$inner_summary)
  }
  
  if("outer_model" %in% names(rfpc_ecsi)) {
    cat("\n--- Modèle externe RFPC ---\n")
    print(rfpc_ecsi$outer_model)
  }
  
  if("unidim" %in% names(rfpc_ecsi)) {
    cat("\n--- Unidimensionnalité RFPC ---\n")
    print(rfpc_ecsi$unidim)
  }
  
  if("scores" %in% names(rfpc_ecsi)) {
    cat("\n--- Scores latents RFPC ---\n")
    print(head(rfpc_ecsi$scores))
  }
  
  if("gof" %in% names(rfpc_ecsi)) {
    cat("\n--- GOF RFPC ---\n")
    print(rfpc_ecsi$gof)
  }
}

# =========================================================
# 7. EXTRACTION ROBUSTE DES R² SI DISPONIBLES
# =========================================================
if(is.list(rfpc_ecsi) && "inner_summary" %in% names(rfpc_ecsi)) {
  if("R2" %in% colnames(rfpc_ecsi$inner_summary)) {
    r2_rfpc <- data.frame(
      Bloc = rownames(rfpc_ecsi$inner_summary),
      R2 = rfpc_ecsi$inner_summary[, "R2"]
    )
    
    cat("\n--- Tableau synthétique des R² RFPC ---\n")
    print(r2_rfpc)
  }
}

# =========================================================
# 8. EXTRACTION ROBUSTE DES LOADINGS SI DISPONIBLES
# =========================================================
if(is.list(rfpc_ecsi) && "outer_model" %in% names(rfpc_ecsi)) {
  if(all(c("name", "block", "loading") %in% colnames(rfpc_ecsi$outer_model))) {
    
    table_loadings_rfpc <- rfpc_ecsi$outer_model %>%
      mutate(
        Interpretation_loading = case_when(
          abs(loading) >= 0.70 ~ "Très bon",
          abs(loading) >= 0.50 ~ "Acceptable",
          TRUE ~ "Faible"
        )
      )
    
    cat("\n--- Synthèse des loadings RFPC ---\n")
    print(table_loadings_rfpc)
  }
}

# =========================================================
# 9. EXTRACTION ROBUSTE DE L’UNIDIMENSIONNALITÉ SI DISPONIBLE
# =========================================================
if(is.list(rfpc_ecsi) && "unidim" %in% names(rfpc_ecsi)) {
  cat("\n--- Synthèse de l'unidimensionnalité RFPC ---\n")
  print(rfpc_ecsi$unidim)
}