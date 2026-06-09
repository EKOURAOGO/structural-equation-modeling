# =========================================================
# QUESTION 9 - VARIABLES LATENTES ET POIDS EXTERNES NORMALISÉS
# + SYNTHÈSE FINALE ET CHOIX MÉTHODOLOGIQUE
# =========================================================

library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(purrr)
library(semTools)

cat("=========================================================\n")
cat("QUESTION 9 - VARIABLES LATENTES ET POIDS EXTERNES NORMALISÉS\n")
cat("=========================================================\n\n")

# =========================================================
# 0. VÉRIFICATION DES OBJETS NÉCESSAIRES
# =========================================================
objets_requis <- c("fit_lisrel", "pls_ecsi", "rfpc_ecsi", "pls_boot", "data_sem")
objets_manquants <- objets_requis[!sapply(objets_requis, exists)]

if(length(objets_manquants) > 0){
  stop(paste(
    "Objets manquants :", paste(objets_manquants, collapse = ", "),
    "\nExécute d'abord les questions 4, 6, 7 et 8."
  ))
}

# =========================================================
# 1. DÉFINITION DES BLOCS
# =========================================================
blocs_list <- list(
  IMAG = c("IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5"),
  CUEX = c("CUEX1", "CUEX2", "CUEX3"),
  PERQ = c("PERQ1", "PERQ2", "PERQ3", "PERQ4", "PERQ5", "PERQ6", "PERQ7"),
  PERV = c("PERV1", "PERV2"),
  CUSA = c("CUSA1", "CUSA2", "CUSA3"),
  CUSL = c("CUSL1", "CUSL2", "CUSL3")
)

blocs <- names(blocs_list)
blocs_endogenes <- c("CUEX", "PERQ", "PERV", "CUSA", "CUSL")

cat("Blocs étudiés :\n")
print(blocs)

# =========================================================
# 2. CONSTRUCTION EXPLORATOIRE LISREL : ACP PAR BLOC
# =========================================================
cat("\n=========================================================\n")
cat("1. CONSTRUCTION EXPLORATOIRE LISREL (ACP PAR BLOC)\n")
cat("=========================================================\n")

# Dans l'esprit de la consigne, l'approche exploratoire LISREL
# consiste ici à construire chaque variable latente par ACP bloc par bloc.
# La première composante principale fournit :
# - les poids externes normalisés exploratoires
# - les scores latents exploratoires

harmoniser_signe <- function(weights, scores = NULL) {
  idx_ref <- which.max(abs(weights))
  signe <- ifelse(weights[idx_ref] >= 0, 1, -1)
  weights_out <- weights * signe
  if (is.null(scores)) {
    return(list(weights = weights_out))
  } else {
    scores_out <- scores * signe
    return(list(weights = weights_out, scores = scores_out))
  }
}

poids_lisrel_exp_list <- list()
scores_lisrel_exp_list <- list()

for (bloc in blocs) {
  vars_bloc <- blocs_list[[bloc]]
  X_bloc <- scale(data_sem[, vars_bloc, drop = FALSE])
  
  acp_bloc <- prcomp(X_bloc, center = FALSE, scale. = FALSE)
  
  poids_bruts <- acp_bloc$rotation[, 1]
  scores_bruts <- acp_bloc$x[, 1]
  
  res_sign <- harmoniser_signe(poids_bruts, scores_bruts)
  
  poids_norm <- as.numeric(res_sign$weights)
  scores_norm <- as.numeric(scale(res_sign$scores)[, 1])
  
  poids_lisrel_exp_list[[bloc]] <- data.frame(
    Bloc = bloc,
    Item = vars_bloc,
    Poids_LISREL_exp = round(poids_norm, 3),
    row.names = NULL
  )
  
  scores_lisrel_exp_list[[bloc]] <- scores_norm
}

poids_lisrel_exp <- bind_rows(poids_lisrel_exp_list)

scores_lisrel_exp <- as.data.frame(scores_lisrel_exp_list)
scores_lisrel_exp <- scores_lisrel_exp[, blocs, drop = FALSE]
colnames(scores_lisrel_exp) <- blocs

cat("\n--- Poids externes exploratoires LISREL (ACP par bloc) ---\n")
print(poids_lisrel_exp)

# =========================================================
# 3. POIDS EXTERNES PLS
# =========================================================
cat("\n=========================================================\n")
cat("2. POIDS EXTERNES PLS\n")
cat("=========================================================\n")

# En PLS, les poids externes sont directement fournis par outer_model$weight
poids_pls <- pls_ecsi$outer_model %>%
  transmute(
    Bloc = block,
    Item = name,
    Poids_PLS = round(weight, 3)
  )

cat("\n--- Poids externes PLS ---\n")
print(poids_pls)

# =========================================================
# 4. POIDS EXTERNES RFPC
# =========================================================
cat("\n=========================================================\n")
cat("3. POIDS EXTERNES RFPC\n")
cat("=========================================================\n")

# Pour RFPC, les loadings de la première composante sont utilisés
# comme approximation opérationnelle des poids externes normalisés.
# Le signe est harmonisé bloc par bloc pour permettre la comparaison.
poids_rfpc_raw <- rfpc_ecsi$outer_model %>%
  transmute(
    Bloc = block,
    Item = name,
    Poids_RFPC_brut = loading
  )

poids_rfpc <- poids_rfpc_raw %>%
  group_by(Bloc) %>%
  group_modify(~{
    w <- .x$Poids_RFPC_brut
    idx_ref <- which.max(abs(w))
    signe <- ifelse(w[idx_ref] >= 0, 1, -1)
    .x$Poids_RFPC <- round(w * signe, 3)
    .x
  }) %>%
  ungroup() %>%
  select(Bloc, Item, Poids_RFPC)

cat("\n--- Poids externes RFPC ---\n")
print(poids_rfpc)

# =========================================================
# 5. TABLEAU COMPARATIF DES POIDS EXTERNES NORMALISÉS
# =========================================================
cat("\n=========================================================\n")
cat("4. TABLEAU COMPARATIF DES POIDS EXTERNES NORMALISÉS\n")
cat("=========================================================\n")

comparaison_poids <- poids_lisrel_exp %>%
  left_join(poids_pls, by = c("Bloc", "Item")) %>%
  left_join(poids_rfpc, by = c("Bloc", "Item")) %>%
  arrange(match(Bloc, blocs), Item)

cat("\n--- Comparaison des poids externes normalisés ---\n")
print(comparaison_poids)

# Corrélations globales entre poids externes
cor_poids_pls_rfpc <- cor(comparaison_poids$Poids_PLS, comparaison_poids$Poids_RFPC)
cor_poids_pls_lisrel <- cor(comparaison_poids$Poids_PLS, comparaison_poids$Poids_LISREL_exp)
cor_poids_rfpc_lisrel <- cor(comparaison_poids$Poids_RFPC, comparaison_poids$Poids_LISREL_exp)

cat("\n--- Corrélations globales entre poids externes ---\n")
cat("Corrélation PLS - RFPC   :", round(cor_poids_pls_rfpc, 3), "\n")
cat("Corrélation PLS - LISREL :", round(cor_poids_pls_lisrel, 3), "\n")
cat("Corrélation RFPC - LISREL:", round(cor_poids_rfpc_lisrel, 3), "\n")

# =========================================================
# 6. COMPARAISON DES SCORES LATENTS ENTRE MÉTHODES
# =========================================================
cat("\n=========================================================\n")
cat("5. COMPARAISON DES VARIABLES LATENTES (SCORES)\n")
cat("=========================================================\n")

# Scores PLS
scores_pls <- as.data.frame(pls_ecsi$scores)
scores_pls <- scores_pls[, blocs, drop = FALSE]

# Scores RFPC
scores_rfpc <- as.data.frame(rfpc_ecsi$scores)
scores_rfpc <- scores_rfpc[, blocs, drop = FALSE]

# Harmonisation du signe des scores RFPC par rapport à PLS
for (bloc in blocs) {
  cor_tmp <- cor(scores_pls[[bloc]], scores_rfpc[[bloc]])
  if (!is.na(cor_tmp) && cor_tmp < 0) {
    scores_rfpc[[bloc]] <- -scores_rfpc[[bloc]]
  }
}

# Harmonisation du signe des scores LISREL exploratoires par rapport à PLS
for (bloc in blocs) {
  cor_tmp <- cor(scores_pls[[bloc]], scores_lisrel_exp[[bloc]])
  if (!is.na(cor_tmp) && cor_tmp < 0) {
    scores_lisrel_exp[[bloc]] <- -scores_lisrel_exp[[bloc]]
  }
}

comparaison_scores <- data.frame(
  Bloc = blocs,
  Corr_LISRELexp_PLS = round(sapply(blocs, function(b) cor(scores_lisrel_exp[[b]], scores_pls[[b]])), 3),
  Corr_LISRELexp_RFPC = round(sapply(blocs, function(b) cor(scores_lisrel_exp[[b]], scores_rfpc[[b]])), 3),
  Corr_PLS_RFPC = round(sapply(blocs, function(b) cor(scores_pls[[b]], scores_rfpc[[b]])), 3),
  row.names = NULL
)

cat("\n--- Corrélations entre scores latents ---\n")
print(comparaison_scores)

# =========================================================
# 7. EXTRACTION DES INDICATEURS CLÉS - LISREL
# =========================================================
cat("\n=========================================================\n")
cat("6. EXTRACTION DES INDICATEURS - LISREL\n")
cat("=========================================================\n")

fit_lisrel_indices <- fitMeasures(
  fit_lisrel,
  c("cfi", "tli", "rmsea", "srmr")
)

resume_fit_lisrel <- data.frame(
  Methode = "LISREL",
  CFI = round(fit_lisrel_indices["cfi"], 3),
  TLI = round(fit_lisrel_indices["tli"], 3),
  RMSEA = round(fit_lisrel_indices["rmsea"], 3),
  SRMR = round(fit_lisrel_indices["srmr"], 3),
  row.names = NULL
)

cat("\n--- Ajustement global LISREL ---\n")
print(resume_fit_lisrel)

r2_lisrel_raw <- inspect(fit_lisrel, "r2")
r2_lisrel <- data.frame(
  Bloc = names(r2_lisrel_raw),
  R2 = as.numeric(r2_lisrel_raw),
  row.names = NULL
) %>%
  filter(Bloc %in% blocs_endogenes)

r2_lisrel$R2 <- round(r2_lisrel$R2, 3)

cat("\n--- R² LISREL ---\n")
print(r2_lisrel)

loadings_lisrel <- parameterEstimates(fit_lisrel, standardized = TRUE) %>%
  filter(op == "=~") %>%
  transmute(
    Bloc = lhs,
    Item = rhs,
    Loading = round(std.all, 3)
  )

cat("\n--- Charges standardisées LISREL ---\n")
print(loadings_lisrel)

paths_lisrel <- parameterEstimates(fit_lisrel, standardized = TRUE) %>%
  filter(op == "~", lhs %in% blocs_endogenes) %>%
  transmute(
    Relation = paste(rhs, "->", lhs),
    Coef = round(std.all, 3),
    p_value = round(pvalue, 4),
    Significatif = ifelse(pvalue < 0.05, "Oui", "Non")
  )

cat("\n--- Chemins LISREL ---\n")
print(paths_lisrel)

# Remplacement de reliability() par compRelSEM() + AVE()
omega_lisrel <- semTools::compRelSEM(fit_lisrel, tau.eq = FALSE)
ave_lisrel <- semTools::AVE(fit_lisrel)

resume_mesure_lisrel <- data.frame(
  Bloc = names(omega_lisrel),
  Omega = round(as.numeric(omega_lisrel), 3),
  AVE = round(as.numeric(ave_lisrel), 3),
  row.names = NULL
)

cat("\n--- Fiabilité LISREL (Omega + AVE) ---\n")
print(resume_mesure_lisrel)

# =========================================================
# 8. EXTRACTION DES INDICATEURS CLÉS - PLS
# =========================================================
cat("\n=========================================================\n")
cat("7. EXTRACTION DES INDICATEURS - PLS\n")
cat("=========================================================\n")

resume_fit_pls <- data.frame(
  Methode = "PLS",
  GOF = round(pls_ecsi$gof, 3),
  row.names = NULL
)

cat("\n--- GOF PLS ---\n")
print(resume_fit_pls)

r2_pls <- data.frame(
  Bloc = rownames(pls_ecsi$inner_summary),
  R2 = pls_ecsi$inner_summary[, "R2"],
  row.names = NULL
) %>%
  filter(Bloc %in% blocs)

r2_pls$R2 <- round(r2_pls$R2, 3)

cat("\n--- R² PLS ---\n")
print(r2_pls)

loadings_pls <- pls_ecsi$outer_model %>%
  transmute(
    Bloc = block,
    Item = name,
    Loading = round(loading, 3),
    Communalite = round(communality, 3)
  )

cat("\n--- Loadings PLS ---\n")
print(loadings_pls)

structure_pls <- pls_boot$boot$paths %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Relation") %>%
  transmute(
    Relation = Relation,
    Coef = round(Original, 3),
    IC_inf = round(perc.025, 3),
    IC_sup = round(perc.975, 3),
    Significatif = ifelse(perc.025 * perc.975 > 0, "Oui", "Non")
  )

cat("\n--- Chemins PLS (bootstrap) ---\n")
print(structure_pls)

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
# 9. EXTRACTION DES INDICATEURS CLÉS - RFPC
# =========================================================
cat("\n=========================================================\n")
cat("8. EXTRACTION DES INDICATEURS - RFPC\n")
cat("=========================================================\n")

resume_fit_rfpc <- data.frame(
  Methode = "RFPC",
  GOF = round(rfpc_ecsi$gof, 3),
  row.names = NULL
)

cat("\n--- GOF RFPC ---\n")
print(resume_fit_rfpc)

r2_rfpc <- data.frame(
  Bloc = rownames(rfpc_ecsi$inner_summary),
  R2 = rfpc_ecsi$inner_summary[, "R2"],
  row.names = NULL
) %>%
  filter(Bloc %in% blocs)

r2_rfpc$R2 <- round(r2_rfpc$R2, 3)

cat("\n--- R² RFPC ---\n")
print(r2_rfpc)

loadings_rfpc <- rfpc_ecsi$outer_model %>%
  transmute(
    Bloc = block,
    Item = name,
    Loading = round(loading, 3),
    Communalite = round(communality, 3)
  )

cat("\n--- Loadings RFPC ---\n")
print(loadings_rfpc)

inner_rfpc_mat <- rfpc_ecsi$inner_model
rownames(inner_rfpc_mat) <- c(
  "Intercept_CUEX", "IMAG -> CUEX",
  "Intercept_PERQ", "IMAG -> PERQ", "CUEX -> PERQ",
  "Intercept_PERV", "CUEX -> PERV", "PERQ -> PERV",
  "Intercept_CUSA", "IMAG -> CUSA", "PERQ -> CUSA", "PERV -> CUSA",
  "Intercept_CUSL", "IMAG -> CUSL", "CUSA -> CUSL"
)

structure_rfpc <- data.frame(
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

cat("\n--- Chemins RFPC ---\n")
print(structure_rfpc)

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
# 10. TABLEAUX COMPARATIFS
# =========================================================
cat("\n=========================================================\n")
cat("9. TABLEAUX COMPARATIFS\n")
cat("=========================================================\n")

comparaison_mesure <- bind_rows(
  resume_mesure_lisrel %>% mutate(Methode = "LISREL"),
  resume_mesure_pls %>% mutate(Methode = "PLS"),
  resume_mesure_rfpc %>% mutate(Methode = "RFPC")
) %>%
  select(Methode, Bloc, everything())

cat("\n--- Comparaison du modèle de mesure ---\n")
print(comparaison_mesure)

comparaison_r2 <- bind_rows(
  r2_lisrel %>% mutate(Methode = "LISREL"),
  r2_pls %>% mutate(Methode = "PLS"),
  r2_rfpc %>% mutate(Methode = "RFPC")
) %>%
  arrange(Bloc, Methode)

cat("\n--- Comparaison des R² ---\n")
print(comparaison_r2)

cat("\n--- Tableau comparatif des poids externes normalisés ---\n")
print(comparaison_poids)

cat("\n--- Corrélations entre variables latentes estimées ---\n")
print(comparaison_scores)

# =========================================================
# 11. SYNTHÈSE ANALYTIQUE DES TROIS MÉTHODES
# =========================================================
cat("\n=========================================================\n")
cat("10. SYNTHÈSE ANALYTIQUE DES TROIS MÉTHODES\n")
cat("=========================================================\n")

synthese_methodes <- data.frame(
  Methode = c("LISREL", "PLS", "RFPC"),
  Points_forts = c(
    "Dispose d'indices globaux d'ajustement (CFI, TLI, RMSEA, SRMR) ; cadre confirmatoire",
    "Estimation stable ; bons loadings ; bootstrap simple ; interprétation robuste",
    "Très proche de PLS sur le modèle de mesure ; structure stable ; résultats cohérents"
  ),
  Points_faibles = c(
    "Coefficients structurels instables ; plusieurs chemins non significatifs ; R² anormalement élevés",
    "Pas d'indices globaux d'ajustement de type SEM covariance ; bloc CUEX et item CUSL2 fragiles",
    "Pas d'indices globaux type LISREL ; signes parfois inversés sur les composantes ; item CUSL2 fragile"
  ),
  Conclusion = c(
    "Peu convaincant sur ces données malgré un ajustement global moyen",
    "Méthode la plus opérationnelle et la plus robuste ici",
    "Bonne alternative à PLS, résultats très proches"
  ),
  row.names = NULL
)

print(synthese_methodes)

# =========================================================
# 12. CLASSEMENT FINAL
# =========================================================
cat("\n=========================================================\n")
cat("11. CLASSEMENT FINAL DES MÉTHODES\n")
cat("=========================================================\n")

classement_final <- data.frame(
  Rang = c(1, 2, 3),
  Methode = c("PLS", "RFPC", "LISREL"),
  Justification = c(
    "Meilleur compromis entre qualité du modèle de mesure, stabilité structurelle et interprétabilité",
    "Résultats très proches de PLS ; méthode pertinente mais un peu moins standard dans le rendu",
    "Résultats structurels instables malgré des indices d'ajustement globaux acceptables"
  ),
  row.names = NULL
)

print(classement_final)

# =========================================================
# 13. CHOIX MÉTHODOLOGIQUE FINAL
# =========================================================
cat("\n=========================================================\n")
cat("12. CHOIX MÉTHODOLOGIQUE FINAL\n")
cat("=========================================================\n")

choix_final <- data.frame(
  Methode_retenue = "PLS-PM",
  Motif_1 = "Le modèle de mesure est globalement satisfaisant pour IMAG, PERQ, PERV et CUSA.",
  Motif_2 = "Les R² sont cohérents et proches de ceux obtenus par RFPC.",
  Motif_3 = "Les coefficients structurels bootstrapés sont majoritairement significatifs.",
  Motif_4 = "La méthode est plus robuste que LISREL sur cet échantillon et plus simple à interpréter que RFPC.",
  Limite_principale = "Les blocs CUEX et surtout CUSL restent fragiles, en particulier l'item CUSL2.",
  row.names = NULL
)

print(choix_final)

# =========================================================
# 14. EXTRACTION COURTE POUR LA RÉDACTION
# =========================================================
cat("\n=========================================================\n")
cat("13. EXTRACTION COURTE POUR LA RÉDACTION\n")
cat("=========================================================\n")

cat("\n1) Les méthodes PLS et RFPC donnent des résultats très proches, tant sur le modèle de mesure que sur le modèle de structure.\n")
cat("2) L'approche exploratoire LISREL par ACP fournit des poids externes et des scores latents globalement cohérents avec ceux de PLS et RFPC sur les blocs les plus stables.\n")
cat("3) LISREL présente un ajustement global moyen (CFI = 0.879 ; TLI = 0.861 ; RMSEA = 0.075 ; SRMR = 0.054), mais des coefficients structurels instables et peu interprétables.\n")
cat("4) Les blocs les plus solides dans l'ensemble des approches sont PERQ, PERV et CUSA.\n")
cat("5) Les blocs les plus fragiles sont CUEX et surtout CUSL, principalement à cause de l'item CUSL2.\n")
cat("6) La comparaison des poids externes normalisés confirme une forte proximité entre PLS et RFPC, tandis que l'approche exploratoire LISREL restitue la même structure générale sur les blocs les plus cohérents.\n")
cat("7) Le modèle PLS-PM est retenu comme approche finale car il fournit les résultats les plus robustes, cohérents et interprétables sur ces données.\n")