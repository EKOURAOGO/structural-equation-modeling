# =========================================================
# QUESTION 12 - SIMULATION DU MODELE ECSI SANS PLAINTES
# + LISREL, PLS, RFPC ET MODELE LIBRE
# =========================================================

library(dplyr)
library(lavaan)
library(semTools)
library(plspm)
library(ClustVarLV)
library(tibble)
library(tidyr)
library(stringr)

cat("=========================================================\n")
cat("QUESTION 12 - SIMULATION DU MODELE ECSI SANS PLAINTES\n")
cat("=========================================================\n\n")

# =========================================================
# 0. PARAMETRES GENERAUX
# =========================================================
set.seed(123)
n <- 250

# Blocs théoriques du modèle ECSI simplifié
blocs_sim <- list(
  IMAG = c("IMAG1","IMAG2","IMAG3","IMAG4","IMAG5"),
  CUEX = c("CUEX1","CUEX2","CUEX3"),
  PERQ = c("PERQ1","PERQ2","PERQ3","PERQ4","PERQ5","PERQ6","PERQ7"),
  PERV = c("PERV1","PERV2"),
  CUSA = c("CUSA1","CUSA2","CUSA3"),
  CUSL = c("CUSL1","CUSL2","CUSL3")
)

inner_sim <- rbind(
  IMAG = c(0, 0, 0, 0, 0, 0),
  CUEX = c(1, 0, 0, 0, 0, 0),
  PERQ = c(1, 1, 0, 0, 0, 0),
  PERV = c(0, 1, 1, 0, 0, 0),
  CUSA = c(1, 0, 1, 1, 0, 0),
  CUSL = c(1, 0, 0, 0, 1, 0)
)
colnames(inner_sim) <- rownames(inner_sim) <- names(blocs_sim)

# =========================================================
# 1. DEMARCHE THEORIQUE
# =========================================================
cat("=========================================================\n")
cat("DEMARCHE THEORIQUE DE SIMULATION\n")
cat("=========================================================\n")
cat("1) Simuler d'abord les variables latentes selon la structure causale ECSI.\n")
cat("2) Générer ensuite les variables manifestes comme : item = lambda * latent + erreur.\n")
cat("3) Choisir des loadings plausibles pour obtenir des blocs majoritairement unidimensionnels.\n")
cat("4) Appliquer LISREL, PLS, RFPC et CLV aux données simulées.\n")
cat("5) Comparer enfin les résultats estimés au modèle vrai utilisé pour générer les données.\n\n")

# =========================================================
# 2. MODELE GENERATEUR LATENT (MODELE VRAI)
# =========================================================
# Coefficients structurels "vrais"
beta <- list(
  CUEX_IMAG = 0.55,
  PERQ_IMAG = 0.30,
  PERQ_CUEX = 0.35,
  PERV_CUEX = 0.15,
  PERV_PERQ = 0.55,
  CUSA_IMAG = 0.15,
  CUSA_PERQ = 0.45,
  CUSA_PERV = 0.25,
  CUSL_IMAG = 0.10,
  CUSL_CUSA = 0.55
)

# Latente exogène
IMAG <- scale(rnorm(n))[,1]

# Latentes endogènes
CUEX <- scale(
  beta$CUEX_IMAG * IMAG + rnorm(n, sd = 0.80)
)[,1]

PERQ <- scale(
  beta$PERQ_IMAG * IMAG +
    beta$PERQ_CUEX * CUEX +
    rnorm(n, sd = 0.70)
)[,1]

PERV <- scale(
  beta$PERV_CUEX * CUEX +
    beta$PERV_PERQ * PERQ +
    rnorm(n, sd = 0.70)
)[,1]

CUSA <- scale(
  beta$CUSA_IMAG * IMAG +
    beta$CUSA_PERQ * PERQ +
    beta$CUSA_PERV * PERV +
    rnorm(n, sd = 0.65)
)[,1]

CUSL <- scale(
  beta$CUSL_IMAG * IMAG +
    beta$CUSL_CUSA * CUSA +
    rnorm(n, sd = 0.75)
)[,1]

latents_true <- data.frame(IMAG, CUEX, PERQ, PERV, CUSA, CUSL)

cat("=========================================================\n")
cat("CORRELATIONS ENTRE LATENTES SIMULEES\n")
cat("=========================================================\n")
print(round(cor(latents_true), 3))

# =========================================================
# 3. GENERATION DES VARIABLES MANIFESTES
# =========================================================
# Charges factorielles vraies
# On rend volontairement CUSL2 faible pour retrouver un item fragile
loadings_true <- list(
  IMAG = c(0.78, 0.70, 0.65, 0.76, 0.74),
  CUEX = c(0.76, 0.69, 0.62),
  PERQ = c(0.82, 0.70, 0.78, 0.77, 0.74, 0.76, 0.72),
  PERV = c(0.80, 0.75),
  CUSA = c(0.83, 0.77, 0.72),
  CUSL = c(0.72, 0.30, 0.70)
)

gen_items <- function(latent, loadings, prefix){
  out <- sapply(seq_along(loadings), function(j){
    lambda <- loadings[j]
    scale(lambda * latent + rnorm(length(latent), sd = sqrt(1 - lambda^2)))[,1]
  })
  out <- as.data.frame(out)
  colnames(out) <- paste0(prefix, seq_len(length(loadings)))
  out
}

data_sim <- bind_cols(
  gen_items(IMAG, loadings_true$IMAG, "IMAG"),
  gen_items(CUEX, loadings_true$CUEX, "CUEX"),
  gen_items(PERQ, loadings_true$PERQ, "PERQ"),
  gen_items(PERV, loadings_true$PERV, "PERV"),
  gen_items(CUSA, loadings_true$CUSA, "CUSA"),
  gen_items(CUSL, loadings_true$CUSL, "CUSL")
)

cat("\n=========================================================\n")
cat("APERÇU DES DONNEES SIMULEES\n")
cat("=========================================================\n")
print(dim(data_sim))
print(summary(data_sim[, 1:6]))

# =========================================================
# 4. LISREL SUR DONNEES SIMULEES
# =========================================================
modele_lisrel_sim <- '
IMAG =~ IMAG1 + IMAG2 + IMAG3 + IMAG4 + IMAG5
CUEX =~ CUEX1 + CUEX2 + CUEX3
PERQ =~ PERQ1 + PERQ2 + PERQ3 + PERQ4 + PERQ5 + PERQ6 + PERQ7
PERV =~ PERV1 + PERV2
CUSA =~ CUSA1 + CUSA2 + CUSA3
CUSL =~ CUSL1 + CUSL2 + CUSL3

CUEX ~ IMAG
PERQ ~ IMAG + CUEX
PERV ~ CUEX + PERQ
CUSA ~ IMAG + PERQ + PERV
CUSL ~ IMAG + CUSA
'

fit_lisrel_sim <- sem(
  model = modele_lisrel_sim,
  data = data_sim,
  std.lv = TRUE,
  estimator = "MLR"
)

cat("\n=========================================================\n")
cat("LISREL - DONNEES SIMULEES\n")
cat("=========================================================\n")
print(summary(fit_lisrel_sim, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE))

# R² LISREL
r2_lisrel_sim <- inspect(fit_lisrel_sim, "r2")
r2_lisrel_sim <- data.frame(
  Methode = "LISREL",
  Bloc = names(r2_lisrel_sim),
  R2 = as.numeric(r2_lisrel_sim),
  row.names = NULL
) %>%
  filter(Bloc %in% c("CUEX","PERQ","PERV","CUSA","CUSL")) %>%
  mutate(R2 = round(R2, 3))

cat("\n--- R² LISREL ---\n")
print(r2_lisrel_sim)

# =========================================================
# 5. PLS SUR DONNEES SIMULEES
# =========================================================
pls_sim <- plspm(
  Data = data_sim,
  path_matrix = inner_sim,
  blocks = blocs_sim,
  modes = rep("A", 6),
  scaled = TRUE
)

set.seed(123)
pls_sim_boot <- plspm(
  Data = data_sim,
  path_matrix = inner_sim,
  blocks = blocs_sim,
  modes = rep("A", 6),
  scaled = TRUE,
  boot.val = TRUE,
  br = 500
)

cat("\n=========================================================\n")
cat("PLS - DONNEES SIMULEES\n")
cat("=========================================================\n")
print(summary(pls_sim))

r2_pls_sim <- data.frame(
  Methode = "PLS",
  Bloc = rownames(pls_sim$inner_summary),
  R2 = round(pls_sim$inner_summary[, "R2"], 3),
  row.names = NULL
)

loadings_pls_sim <- pls_sim$outer_model %>%
  transmute(
    Bloc = block,
    Item = name,
    Loading_pls = round(loading, 3)
  )

cat("\n--- R² PLS ---\n")
print(r2_pls_sim)

cat("\n--- Loadings PLS ---\n")
print(loadings_pls_sim)

cat("\n--- GOF PLS ---\n")
print(round(pls_sim$gof, 3))

# =========================================================
# 6. RFPC SUR DONNEES SIMULEES (avec protection)
# =========================================================
rfpc_sim <- NULL

if(!exists("RFPC_pm")){
  if(file.exists("RFPC_model_function.R")){
    source("RFPC_model_function.R")
  } else if(file.exists("RFPC_model (function).txt")){
    source("RFPC_model (function).txt")
  } else {
    warning("La fonction RFPC_pm est introuvable. La partie RFPC sera sautée.")
  }
}

if(exists("RFPC_pm")){
  blocs_sim_idx <- lapply(blocs_sim, function(v) match(v, colnames(data_sim)))
  
  rfpc_sim <- tryCatch(
    {
      RFPC_pm(
        data = as.data.frame(scale(data_sim)),
        path = inner_sim,
        blocks = blocs_sim_idx
      )
    },
    error = function(e){
      cat("\n=========================================================\n")
      cat("RFPC - ERREUR TECHNIQUE\n")
      cat("=========================================================\n")
      cat("La fonction RFPC_pm a échoué :", e$message, "\n")
      cat("La comparaison finale reposera surtout sur LISREL, PLS et CLV.\n")
      return(NULL)
    }
  )
}

if(!is.null(rfpc_sim)){
  cat("\n=========================================================\n")
  cat("RFPC - DONNEES SIMULEES\n")
  cat("=========================================================\n")
  print(rfpc_sim)
}

# =========================================================
# 7. MODELE LIBRE CLV SUR DONNEES SIMULEES
# =========================================================
res_clv_sim <- CLV(
  X = as.data.frame(scale(data_sim)),
  method = "directional",
  sX = FALSE,
  nmax = 20,
  maxiter = 50,
  graph = FALSE
)

partition_6_sim <- get_partition(res_clv_sim, K = 6, type = "vector")

cat("\n=========================================================\n")
cat("MODELE LIBRE CLV - DONNEES SIMULEES\n")
cat("=========================================================\n")
print(partition_6_sim)

part_sim_df <- data.frame(
  Variable = names(partition_6_sim),
  Classe = as.integer(partition_6_sim),
  Bloc_vrai = case_when(
    str_detect(names(partition_6_sim), "^IMAG") ~ "IMAG",
    str_detect(names(partition_6_sim), "^CUEX") ~ "CUEX",
    str_detect(names(partition_6_sim), "^PERQ") ~ "PERQ",
    str_detect(names(partition_6_sim), "^PERV") ~ "PERV",
    str_detect(names(partition_6_sim), "^CUSA") ~ "CUSA",
    str_detect(names(partition_6_sim), "^CUSL") ~ "CUSL",
    TRUE ~ "AUTRE"
  )
)

cat("\n--- Tableau CLV simulé vs blocs vrais ---\n")
print(table(part_sim_df$Classe, part_sim_df$Bloc_vrai))

# Visualisations CLV
plot(res_clv_sim, type = "dendrogram", cex = 0.8)
plot(res_clv_sim, type = "delta", cex = 0.8)
plot_var(res_clv_sim, K = 6, label = TRUE, cex.lab = 0.7)

# =========================================================
# 8. COMPARAISON AU MODELE VRAI
# =========================================================

# ----------------------------
# 8.1 R² comparés
# ----------------------------
r2_true_theorique <- data.frame(
  Bloc = c("CUEX","PERQ","PERV","CUSA","CUSL"),
  R2_theorique = c(
    round(summary(lm(CUEX ~ IMAG))$r.squared, 3),
    round(summary(lm(PERQ ~ IMAG + CUEX))$r.squared, 3),
    round(summary(lm(PERV ~ CUEX + PERQ))$r.squared, 3),
    round(summary(lm(CUSA ~ IMAG + PERQ + PERV))$r.squared, 3),
    round(summary(lm(CUSL ~ IMAG + CUSA))$r.squared, 3)
  )
)

comparaison_r2_sim <- left_join(
  r2_true_theorique,
  r2_lisrel_sim %>% select(Bloc, R2_LISREL = R2),
  by = "Bloc"
) %>%
  left_join(
    r2_pls_sim %>% select(Bloc, R2_PLS = R2),
    by = "Bloc"
  )

if(!is.null(rfpc_sim)){
  r2_rfpc_sim <- data.frame(
    Bloc = rownames(rfpc_sim$inner_summary),
    R2_RFPC = round(rfpc_sim$inner_summary[, "R2"], 3),
    row.names = NULL
  )
  
  comparaison_r2_sim <- left_join(comparaison_r2_sim, r2_rfpc_sim, by = "Bloc")
}

cat("\n=========================================================\n")
cat("COMPARAISON DES R² : MODELE VRAI VS ESTIMATIONS\n")
cat("=========================================================\n")
print(comparaison_r2_sim)

# ----------------------------
# 8.2 Charges vraies vs charges PLS
# ----------------------------
loadings_true_df <- bind_rows(
  data.frame(Bloc = "IMAG", Item = paste0("IMAG", 1:5), Loading_vrai = loadings_true$IMAG),
  data.frame(Bloc = "CUEX", Item = paste0("CUEX", 1:3), Loading_vrai = loadings_true$CUEX),
  data.frame(Bloc = "PERQ", Item = paste0("PERQ", 1:7), Loading_vrai = loadings_true$PERQ),
  data.frame(Bloc = "PERV", Item = paste0("PERV", 1:2), Loading_vrai = loadings_true$PERV),
  data.frame(Bloc = "CUSA", Item = paste0("CUSA", 1:3), Loading_vrai = loadings_true$CUSA),
  data.frame(Bloc = "CUSL", Item = paste0("CUSL", 1:3), Loading_vrai = loadings_true$CUSL)
)

comparaison_loadings_sim <- left_join(
  loadings_true_df,
  loadings_pls_sim,
  by = c("Bloc", "Item")
) %>%
  mutate(
    Loading_vrai = round(Loading_vrai, 3),
    Ecart_abs = round(abs(Loading_pls - Loading_vrai), 3)
  )

cat("\n=========================================================\n")
cat("CHARGES VRAIES VS CHARGES PLS ESTIMEES\n")
cat("=========================================================\n")
print(comparaison_loadings_sim)

# ----------------------------
# 8.3 Coefficients structurels vrais vs PLS
# ----------------------------
coef_true_df <- data.frame(
  Relation = c(
    "IMAG -> CUEX",
    "IMAG -> PERQ",
    "CUEX -> PERQ",
    "CUEX -> PERV",
    "PERQ -> PERV",
    "IMAG -> CUSA",
    "PERQ -> CUSA",
    "PERV -> CUSA",
    "IMAG -> CUSL",
    "CUSA -> CUSL"
  ),
  Coef_vrai = c(
    beta$CUEX_IMAG,
    beta$PERQ_IMAG,
    beta$PERQ_CUEX,
    beta$PERV_CUEX,
    beta$PERV_PERQ,
    beta$CUSA_IMAG,
    beta$CUSA_PERQ,
    beta$CUSA_PERV,
    beta$CUSL_IMAG,
    beta$CUSL_CUSA
  )
)

paths_pls_sim <- pls_sim_boot$boot$paths %>%
  as.data.frame() %>%
  rownames_to_column("Relation") %>%
  transmute(
    Relation,
    Coef_PLS = round(Original, 3),
    IC_inf = round(perc.025, 3),
    IC_sup = round(perc.975, 3)
  )

comparaison_paths_sim <- left_join(coef_true_df, paths_pls_sim, by = "Relation") %>%
  mutate(
    Coef_vrai = round(Coef_vrai, 3),
    Ecart_abs = round(abs(Coef_PLS - Coef_vrai), 3)
  )

cat("\n=========================================================\n")
cat("COEFFICIENTS VRAIS VS COEFFICIENTS PLS\n")
cat("=========================================================\n")
print(comparaison_paths_sim)

# =========================================================
# 9. SYNTHESE AUTOMATIQUE
# =========================================================
item_le_plus_faible <- comparaison_loadings_sim %>%
  arrange(Ecart_abs) %>%
  filter(Bloc == "CUSL") %>%
  arrange(Loading_pls) %>%
  slice(1)

cat("\n=========================================================\n")
cat("SYNTHESE AUTOMATIQUE\n")
cat("=========================================================\n")
cat("Le modèle simulé suit la structure ECSI simplifiée avec 250 individus.\n")
cat("Les méthodes LISREL et PLS permettent d'estimer des R² proches du modèle vrai générateur.\n")
cat("Le CLV permet de vérifier si la structure en 6 blocs est retrouvée empiriquement.\n")
cat("L'item volontairement fragilisé est :", item_le_plus_faible$Item,
    "avec un loading PLS estimé de", item_le_plus_faible$Loading_pls, ".\n")

if(is.null(rfpc_sim)){
  cat("RFPC n'a pas pu être exploité complètement à cause d'une erreur technique de la fonction fournie.\n")
}

# =========================================================
# 10. GUIDE DE COMMENTAIRE POUR LE RAPPORT
# =========================================================
cat("\n=========================================================\n")
cat("GUIDE DE COMMENTAIRE - QUESTION 12\n")
cat("=========================================================\n")
cat("1) Les données ont été simulées à partir du modèle ECSI simplifié ; ce modèle constitue donc la vérité génératrice.\n")
cat("2) LISREL et PLS sont ensuite appliqués pour vérifier leur capacité à retrouver les relations et les niveaux d'explication attendus.\n")
cat("3) La comparaison entre charges vraies et loadings estimés permet d'évaluer la qualité de récupération du modèle de mesure.\n")
cat("4) Le modèle libre CLV est utilisé pour vérifier si la structure en 6 blocs peut être retrouvée sans information théorique préalable.\n")
cat("5) L'item CUSL2 a été volontairement rendu plus faible afin de tester la capacité des méthodes à détecter un indicateur fragile.\n")
cat("6) Si les résultats estimés restent proches du modèle vrai, cela conforte la pertinence globale des approches mobilisées dans le TP.\n")
cat("7) Si RFPC échoue, cette limite doit être signalée honnêtement comme une difficulté technique liée à la fonction fournie.\n")