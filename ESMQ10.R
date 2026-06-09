# =========================================================
# QUESTION 10 - MODELE LIBRE AVEC CLV
# + CONSTRUCTION D'UN MODELE EXPERT
# =========================================================

library(dplyr)
library(ClustVarLV)
library(tidyr)
library(stringr)

cat("=========================================================\n")
cat("QUESTION 10 - MODELE LIBRE AVEC CLV\n")
cat("=========================================================\n\n")

# =========================================================
# 0. PREPARATION
# =========================================================
# Hypothèse naturelle dans ton TP :
# data_sem existe déjà et contient les 23 variables manifestes
# du modèle ECSI simplifié (sans plaintes).

if(!exists("data_sem")){
  stop("L'objet 'data_sem' est introuvable. Exécute d'abord la préparation des données (Q1).")
}

vars_manifestes <- c(
  "IMAG1","IMAG2","IMAG3","IMAG4","IMAG5",
  "CUEX1","CUEX2","CUEX3",
  "PERQ1","PERQ2","PERQ3","PERQ4","PERQ5","PERQ6","PERQ7",
  "PERV1","PERV2",
  "CUSA1","CUSA2","CUSA3",
  "CUSL1","CUSL2","CUSL3"
)

blocs_theo <- list(
  IMAG = c("IMAG1","IMAG2","IMAG3","IMAG4","IMAG5"),
  CUEX = c("CUEX1","CUEX2","CUEX3"),
  PERQ = c("PERQ1","PERQ2","PERQ3","PERQ4","PERQ5","PERQ6","PERQ7"),
  PERV = c("PERV1","PERV2"),
  CUSA = c("CUSA1","CUSA2","CUSA3"),
  CUSL = c("CUSL1","CUSL2","CUSL3")
)

data_clv <- data_sem[, vars_manifestes] %>%
  as.data.frame() %>%
  scale() %>%
  as.data.frame()

cat("Dimensions des données centrées-réduites :", dim(data_clv)[1], "x", dim(data_clv)[2], "\n")

# =========================================================
# 1. MODELE LIBRE : CLV
# =========================================================
# Selon la version du package, free_model peut être disponible.
# On prévoit un fallback sur CLV si besoin.

if(exists("free_model")){
  res_clv <- free_model(
    X = data_clv,
    K = 6,
    method = "directional"
  )
  
  # Si la sortie contient directement la partition
  if("partition" %in% names(res_clv)){
    partition_6 <- res_clv$partition
  } else {
    # fallback si la structure diffère
    if(exists("get_partition")){
      partition_6 <- get_partition(res_clv, K = 6, type = "vector")
    } else {
      stop("Impossible d'extraire la partition depuis free_model.")
    }
  }
  
} else {
  res_clv <- CLV(
    X = data_clv,
    method = "directional",
    sX = FALSE,     # données déjà standardisées
    nmax = 20,
    maxiter = 50,
    graph = FALSE
  )
  
  partition_6 <- get_partition(res_clv, K = 6, type = "vector")
}

cat("\n=========================================================\n")
cat("PARTITION EN 6 CLASSES\n")
cat("=========================================================\n")
print(partition_6)

table_partition <- data.frame(
  Variable = names(partition_6),
  Classe = as.integer(partition_6),
  row.names = NULL
) %>%
  arrange(Classe, Variable)

cat("\n--- Variables par classe ---\n")
print(table_partition)

classes_liste <- split(table_partition$Variable, table_partition$Classe)

cat("\n--- Liste des 6 classes ---\n")
print(classes_liste)

# =========================================================
# 2. VISUALISATIONS ET COMPOSANTES DU MODELE LIBRE
# =========================================================
cat("\n=========================================================\n")
cat("VISUALISATIONS ET COMPOSANTES CLV\n")
cat("=========================================================\n")

# Dendrogramme
plot(res_clv, type = "dendrogram", cex = 0.8)

# Evolution du critère
plot(res_clv, type = "delta", cex = 0.8)

# Représentation des variables
plot_var(res_clv, K = 6, label = TRUE, cex.lab = 0.8)

# Composantes associées aux 6 groupes
comp_6 <- get_comp(res_clv, K = 6)

cat("\n--- Corrélations entre composantes du modèle libre ---\n")
print(round(cor(comp_6), 3))

# =========================================================
# 3. COMPARAISON CLV VS BLOCS THEORIQUES ECSI
# =========================================================
get_theoretical_block <- function(varname){
  case_when(
    str_detect(varname, "^IMAG") ~ "IMAG",
    str_detect(varname, "^CUEX") ~ "CUEX",
    str_detect(varname, "^PERQ") ~ "PERQ",
    str_detect(varname, "^PERV") ~ "PERV",
    str_detect(varname, "^CUSA") ~ "CUSA",
    str_detect(varname, "^CUSL") ~ "CUSL",
    TRUE ~ "AUTRE"
  )
}

table_partition <- table_partition %>%
  mutate(Bloc_theorique = sapply(Variable, get_theoretical_block))

cat("\n=========================================================\n")
cat("TABLEAU DE CORRESPONDANCE : CLV VS ECSI THEORIQUE\n")
cat("=========================================================\n")

table_theo_vs_clv <- table(table_partition$Classe, table_partition$Bloc_theorique)
print(table_theo_vs_clv)

resume_classes <- table_partition %>%
  group_by(Classe) %>%
  summarise(
    Nb_variables = n(),
    Variables = paste(Variable, collapse = ", "),
    Composition_theorique = paste(sort(unique(Bloc_theorique)), collapse = ", "),
    .groups = "drop"
  )

cat("\n--- Résumé des classes ---\n")
print(resume_classes)

# =========================================================
# 4. CONSTRUCTION D'UN MODELE EXPERT
# =========================================================
# Principe retenu :
# - chaque classe CLV est affectée au bloc théorique majoritaire
# - si plusieurs classes renvoient au même bloc, on garde celle ayant
#   le plus grand recouvrement avec le bloc théorique concerné
# - les variables non reprises automatiquement peuvent être réaffectées
#   au bloc théorique le plus proche pour garder 6 blocs interprétables

# Recouvrement classe x bloc théorique
overlap_df <- expand.grid(
  Classe = sort(unique(table_partition$Classe)),
  Bloc = names(blocs_theo),
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    Recouvrement = sum(classes_liste[[as.character(Classe)]] %in% blocs_theo[[Bloc]])
  ) %>%
  ungroup()

cat("\n=========================================================\n")
cat("RECOUVREMENT ENTRE CLASSES CLV ET BLOCS ECSI\n")
cat("=========================================================\n")
print(overlap_df %>% arrange(Bloc, desc(Recouvrement), Classe))

# Classe dominante par bloc théorique
mapping_bloc_classe <- overlap_df %>%
  group_by(Bloc) %>%
  arrange(desc(Recouvrement), Classe, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

cat("\n--- Classe retenue pour chaque bloc théorique ---\n")
print(mapping_bloc_classe)

# Construction initiale des blocs experts
blocs_expert <- lapply(names(blocs_theo), function(b){
  cl <- mapping_bloc_classe$Classe[mapping_bloc_classe$Bloc == b][1]
  unique(classes_liste[[as.character(cl)]])
})
names(blocs_expert) <- names(blocs_theo)

# Nettoyage :
# on retire les doublons éventuels entre blocs
vars_deja_prises <- character(0)
for(b in names(blocs_expert)){
  blocs_expert[[b]] <- setdiff(blocs_expert[[b]], vars_deja_prises)
  vars_deja_prises <- c(vars_deja_prises, blocs_expert[[b]])
}

# Réintégration des variables encore non affectées
vars_non_affectees <- setdiff(vars_manifestes, unlist(blocs_expert))

if(length(vars_non_affectees) > 0){
  for(v in vars_non_affectees){
    bloc_v <- get_theoretical_block(v)
    blocs_expert[[bloc_v]] <- c(blocs_expert[[bloc_v]], v)
  }
}

# Tri dans chaque bloc
blocs_expert <- lapply(blocs_expert, sort)

cat("\n=========================================================\n")
cat("MODELE EXPERT PROPOSE A PARTIR DU CLV\n")
cat("=========================================================\n")
print(blocs_expert)

# Vérification finale
cat("\n--- Vérification : 23 variables affectées une seule fois ? ---\n")
print(length(unlist(blocs_expert)))
print(anyDuplicated(unlist(blocs_expert)))
print(setequal(sort(unlist(blocs_expert)), sort(vars_manifestes)))

# =========================================================
# 5. STRUCTURE INTERNE DU MODELE EXPERT
# =========================================================
# Alignement au TP :
# on conserve la structure causale ECSI simplifiée,
# mais avec les blocs experts issus du CLV.
# Cela permet une vraie comparaison "même structure / autre mesure".

inner_expert <- rbind(
  IMAG = c(0, 0, 0, 0, 0, 0),
  CUEX = c(1, 0, 0, 0, 0, 0),
  PERQ = c(1, 1, 0, 0, 0, 0),
  PERV = c(0, 1, 1, 0, 0, 0),
  CUSA = c(1, 0, 1, 1, 0, 0),
  CUSL = c(1, 0, 0, 0, 1, 0)
)
colnames(inner_expert) <- rownames(inner_expert) <- names(blocs_expert)

cat("\n=========================================================\n")
cat("MATRICE STRUCTURELLE DU MODELE EXPERT\n")
cat("=========================================================\n")
print(inner_expert)

# =========================================================
# 6. COMMENTAIRE GUIDÉ POUR LE RAPPORT
# =========================================================
cat("\n=========================================================\n")
cat("GUIDE DE COMMENTAIRE - QUESTION 10\n")
cat("=========================================================\n")
cat("1) Le modèle libre regroupe les 23 variables en 6 classes sur données centrées-réduites.\n")
cat("2) Le tableau de correspondance CLV / ECSI permet de voir si les blocs théoriques sont confirmés empiriquement.\n")
cat("3) Le modèle expert proposé conserve 6 blocs interprétables, construits à partir des classes observées.\n")
cat("4) Les éventuels déplacements de variables doivent être commentés, surtout s'ils concernent des items fragiles.\n")
cat("5) On garde ensuite la structure causale ECSI simplifiée pour comparer proprement ECSI vs modèle expert.\n")