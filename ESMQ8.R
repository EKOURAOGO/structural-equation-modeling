# =========================================================
# QUESTION 8 - COMPARAISON DES POUVOIRS EXPLICATIFS
# LISREL / PLS / RFPC
# =========================================================

library(dplyr)

cat("=========================================================\n")
cat("QUESTION 8 - COMPARAISON DES POUVOIRS EXPLICATIFS\n")
cat("=========================================================\n\n")

# Vérification des objets nécessaires issus des questions précédentes
objets_requis <- c("fit_lisrel", "pls_ecsi", "rfpc_ecsi", "pls_boot")
objets_manquants <- objets_requis[!sapply(objets_requis, exists)]

if(length(objets_manquants) > 0){
  stop(paste("Objets manquants :", paste(objets_manquants, collapse = ", "),
             "\nExécute d'abord les questions 4, 6 et 7."))
}

# =========================================================
# 1. MODÈLE DE MESURE : LISREL
# =========================================================
cat("=========================================================\n")
cat("1. MODÈLE DE MESURE - LISREL\n")
cat("=========================================================\n")

mesure_lisrel <- parameterEstimates(fit_lisrel, standardized = TRUE) %>%
  filter(op == "=~") %>%
  transmute(
    Methode = "LISREL",
    Bloc = lhs,
    Item = rhs,
    Loading = round(std.all, 3),
    Qualite = case_when(
      abs(std.all) >= 0.70 ~ "Très bon",
      abs(std.all) >= 0.50 ~ "Acceptable",
      TRUE ~ "Faible"
    )
  )

cat("\n--- Charges standardisées LISREL ---\n")
print(mesure_lisrel)

reliab_lisrel <- semTools::reliability(fit_lisrel)

resume_mesure_lisrel <- data.frame(
  Bloc = colnames(reliab_lisrel),
  Alpha = round(reliab_lisrel["alpha", ], 3),
  Omega = round(reliab_lisrel["omega", ], 3),
  AVE = round(reliab_lisrel["avevar", ], 3),
  row.names = NULL
)

cat("\n--- Fiabilité et AVE LISREL ---\n")
print(resume_mesure_lisrel)

# =========================================================
# 2. MODÈLE DE MESURE : PLS
# =========================================================
cat("\n=========================================================\n")
cat("2. MODÈLE DE MESURE - PLS\n")
cat("=========================================================\n")

mesure_pls <- pls_ecsi$outer_model %>%
  transmute(
    Methode = "PLS",
    Bloc = block,
    Item = name,
    Loading = round(loading, 3),
    Communalite = round(communality, 3),
    Qualite = case_when(
      abs(loading) >= 0.70 ~ "Très bon",
      abs(loading) >= 0.50 ~ "Acceptable",
      TRUE ~ "Faible"
    )
  )

cat("\n--- Loadings PLS ---\n")
print(mesure_pls)

resume_mesure_pls <- data.frame(
  Bloc = rownames(pls_ecsi$unidim),
  Alpha = round(pls_ecsi$unidim[, "C.alpha"], 3),
  DG_rho = round(pls_ecsi$unidim[, "DG.rho"], 3),
  eig1 = round(pls_ecsi$unidim[, "eig.1st"], 3),
  eig2 = round(pls_ecsi$unidim[, "eig.2nd"], 3),
  row.names = NULL
)

cat("\n--- Unidimensionnalité PLS ---\n")
print(resume_mesure_pls)

# =========================================================
# 3. MODÈLE DE MESURE : RFPC
# =========================================================
cat("\n=========================================================\n")
cat("3. MODÈLE DE MESURE - RFPC\n")
cat("=========================================================\n")

mesure_rfpc <- rfpc_ecsi$outer_model %>%
  transmute(
    Methode = "RFPC",
    Bloc = block,
    Item = name,
    Loading = round(loading, 3),
    Communalite = round(communality, 3),
    Qualite = case_when(
      abs(loading) >= 0.70 ~ "Très bon",
      abs(loading) >= 0.50 ~ "Acceptable",
      TRUE ~ "Faible"
    )
  )

cat("\n--- Loadings RFPC ---\n")
print(mesure_rfpc)

resume_mesure_rfpc <- data.frame(
  Bloc = rfpc_ecsi$unidim$Block,
  Alpha = round(rfpc_ecsi$unidim$C.alpha, 3),
  DG_rho = round(rfpc_ecsi$unidim$DG.rho, 3),
  eig1 = round(rfpc_ecsi$unidim$eig.1st, 3),
  eig2 = round(rfpc_ecsi$unidim$eig.2nd, 3),
  row.names = NULL
)

cat("\n--- Unidimensionnalité RFPC ---\n")
print(resume_mesure_rfpc)

# =========================================================
# 4. TABLEAU COMPARATIF GLOBAL DU MODÈLE DE MESURE
# =========================================================
cat("\n=========================================================\n")
cat("4. COMPARAISON GLOBALE DU MODÈLE DE MESURE\n")
cat("=========================================================\n")

comparaison_mesure <- bind_rows(
  resume_mesure_lisrel %>% mutate(Methode = "LISREL"),
  resume_mesure_pls %>% mutate(Methode = "PLS"),
  resume_mesure_rfpc %>% mutate(Methode = "RFPC")
) %>%
  select(Methode, Bloc, everything())

print(comparaison_mesure)

# =========================================================
# 5. MODÈLE DE STRUCTURE : LISREL
# =========================================================
cat("\n=========================================================\n")
cat("5. MODÈLE DE STRUCTURE - LISREL\n")
cat("=========================================================\n")

structure_lisrel <- parameterEstimates(fit_lisrel, standardized = TRUE) %>%
  filter(op == "~", lhs %in% c("CUEX", "PERQ", "PERV", "CUSA", "CUSL")) %>%
  transmute(
    Methode = "LISREL",
    Relation = paste(rhs, "->", lhs),
    Coef = round(std.all, 3),
    p_value = round(pvalue, 4),
    Significatif = ifelse(pvalue < 0.05, "Oui", "Non")
  )

cat("\n--- Chemins structurels LISREL ---\n")
print(structure_lisrel)

r2_lisrel_raw <- inspect(fit_lisrel, "r2")
r2_lisrel <- data.frame(
  Methode = "LISREL",
  Bloc = names(r2_lisrel_raw),
  R2 = as.numeric(r2_lisrel_raw),
  row.names = NULL
) %>%
  filter(Bloc %in% c("CUEX", "PERQ", "PERV", "CUSA", "CUSL"))

r2_lisrel$R2 <- round(r2_lisrel$R2, 3)

cat("\n--- R² LISREL ---\n")
print(r2_lisrel)

# =========================================================
# 6. MODÈLE DE STRUCTURE : PLS
# =========================================================
cat("\n=========================================================\n")
cat("6. MODÈLE DE STRUCTURE - PLS\n")
cat("=========================================================\n")

structure_pls <- pls_boot$boot$paths %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Relation") %>%
  transmute(
    Methode = "PLS",
    Relation = Relation,
    Coef = round(Original, 3),
    IC_inf = round(perc.025, 3),
    IC_sup = round(perc.975, 3),
    Significatif = ifelse(perc.025 * perc.975 > 0, "Oui", "Non")
  )

cat("\n--- Chemins structurels PLS ---\n")
print(structure_pls)

r2_pls_comp <- data.frame(
  Methode = "PLS",
  Bloc = rownames(pls_ecsi$inner_summary),
  R2 = pls_ecsi$inner_summary[, "R2"],
  row.names = NULL
)

r2_pls_comp$R2 <- round(r2_pls_comp$R2, 3)

cat("\n--- R² PLS ---\n")
print(r2_pls_comp)

# =========================================================
# 7. MODÈLE DE STRUCTURE : RFPC
# =========================================================
cat("\n=========================================================\n")
cat("7. MODÈLE DE STRUCTURE - RFPC\n")
cat("=========================================================\n")

inner_rfpc_mat <- rfpc_ecsi$inner_model
rownames(inner_rfpc_mat) <- c(
  "Intercept_CUEX", "IMAG -> CUEX",
  "Intercept_PERQ", "IMAG -> PERQ", "CUEX -> PERQ",
  "Intercept_PERV", "CUEX -> PERV", "PERQ -> PERV",
  "Intercept_CUSA", "IMAG -> CUSA", "PERQ -> CUSA", "PERV -> CUSA",
  "Intercept_CUSL", "IMAG -> CUSL", "CUSA -> CUSL"
)

structure_rfpc <- data.frame(
  Methode = "RFPC",
  Relation = rownames(inner_rfpc_mat),
  Coef = inner_rfpc_mat[, "Estimate"],
  p_value = inner_rfpc_mat[, "Pr(>|t|)"],
  row.names = NULL
) %>%
  filter(!grepl("^Intercept", Relation)) %>%
  mutate(
    Coef = round(Coef, 3),
    p_value = round(p_value, 4),
    Significatif = ifelse(p_value < 0.05, "Oui", "Non")
  )

cat("\n--- Chemins structurels RFPC ---\n")
print(structure_rfpc)

r2_rfpc_comp <- data.frame(
  Methode = "RFPC",
  Bloc = rownames(rfpc_ecsi$inner_summary),
  R2 = rfpc_ecsi$inner_summary[, "R2"],
  row.names = NULL
)

r2_rfpc_comp$R2 <- round(r2_rfpc_comp$R2, 3)

cat("\n--- R² RFPC ---\n")
print(r2_rfpc_comp)

# =========================================================
# 8. TABLEAU COMPARATIF GLOBAL DU MODÈLE DE STRUCTURE
# =========================================================
cat("\n=========================================================\n")
cat("8. COMPARAISON GLOBALE DU MODÈLE DE STRUCTURE\n")
cat("=========================================================\n")

comparaison_r2 <- bind_rows(
  r2_lisrel,
  r2_pls_comp,
  r2_rfpc_comp
) %>%
  arrange(Bloc, Methode)

cat("\n--- Comparaison des R² ---\n")
print(comparaison_r2)

# =========================================================
# 9. TABLEAU SYNTHÉTIQUE FINAL
# =========================================================
cat("\n=========================================================\n")
cat("9. SYNTHÈSE FINALE\n")
cat("=========================================================\n")

cat("\n--- Comparaison du modèle de mesure ---\n")
print(comparaison_mesure)

cat("\n--- Comparaison des R² structurels ---\n")
print(comparaison_r2)

cat("\n--- Chemins LISREL ---\n")
print(structure_lisrel)

cat("\n--- Chemins PLS ---\n")
print(structure_pls)

cat("\n--- Chemins RFPC ---\n")
print(structure_rfpc)

# =========================================================
# 10. EXTRACTION COURTE POUR COMMENTAIRE
# =========================================================
cat("\n=========================================================\n")
cat("10. EXTRACTION COURTE POUR LE COMMENTAIRE\n")
cat("=========================================================\n")

cat("\nMéthodes les plus proches sur le modèle de mesure : PLS et RFPC\n")
cat("Bloc le plus problématique dans les trois approches : CUSL (surtout CUSL2)\n")
cat("Bloc également fragile : CUEX\n")
cat("Blocs les plus solides : PERQ, PERV, CUSA\n")
cat("Sur le modèle de structure, PLS et RFPC ont des R² proches.\n")
cat("LISREL est plus instable et moins interprétable sur ces données.\n")