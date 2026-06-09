# =========================================================
# QUESTION 2 - SPÉCIFICATION DU MODÈLE ECSI SIMPLIFIÉ
# =========================================================

library(igraph)

# =========================================================
# 1. DÉFINITION DES VARIABLES LATENTES ET DES BLOCS
# =========================================================
latents <- c("IMAG", "CUEX", "PERQ", "PERV", "CUSA", "CUSL")

blocs <- list(
  IMAG = c("IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5"),
  CUEX = c("CUEX1", "CUEX2", "CUEX3"),
  PERQ = c("PERQ1", "PERQ2", "PERQ3", "PERQ4", "PERQ5", "PERQ6", "PERQ7"),
  PERV = c("PERV1", "PERV2"),
  CUSA = c("CUSA1", "CUSA2", "CUSA3"),
  CUSL = c("CUSL1", "CUSL2", "CUSL3")
)

cat("=========================================================\n")
cat("QUESTION 2 - SPÉCIFICATION DU MODÈLE ECSI SIMPLIFIÉ\n")
cat("=========================================================\n\n")

cat("Variables latentes retenues :\n")
print(latents)

cat("\nBlocs de mesure :\n")
print(blocs)

# =========================================================
# 2. SPÉCIFICATION TEXTUELLE DU MODÈLE
# =========================================================
modele_ecsi <- '
# ---------------------------------------------------------
# MODELE DE MESURE
# ---------------------------------------------------------
IMAG =~ IMAG1 + IMAG2 + IMAG3 + IMAG4 + IMAG5
CUEX =~ CUEX1 + CUEX2 + CUEX3
PERQ =~ PERQ1 + PERQ2 + PERQ3 + PERQ4 + PERQ5 + PERQ6 + PERQ7
PERV =~ PERV1 + PERV2
CUSA =~ CUSA1 + CUSA2 + CUSA3
CUSL =~ CUSL1 + CUSL2 + CUSL3

# ---------------------------------------------------------
# MODELE STRUCTUREL
# ---------------------------------------------------------
CUEX ~ IMAG
PERQ ~ IMAG + CUEX
PERV ~ CUEX + PERQ
CUSA ~ IMAG + PERQ + PERV
CUSL ~ IMAG + CUSA
'

cat("\n=========================================================\n")
cat("SPÉCIFICATION TEXTUELLE DU MODÈLE\n")
cat("=========================================================\n")
cat(modele_ecsi, "\n")

# =========================================================
# 3. ÉQUATIONS STRUCTURELLES SOUS FORME LISIBLE
# =========================================================
cat("\n=========================================================\n")
cat("ÉQUATIONS STRUCTURELLES\n")
cat("=========================================================\n")
cat("CUEX = beta1 * IMAG + zeta1\n")
cat("PERQ = beta2 * IMAG + beta3 * CUEX + zeta2\n")
cat("PERV = beta4 * CUEX + beta5 * PERQ + zeta3\n")
cat("CUSA = beta6 * IMAG + beta7 * PERQ + beta8 * PERV + zeta4\n")
cat("CUSL = beta9 * IMAG + beta10 * CUSA + zeta5\n")

# =========================================================
# 4. SCHÉMA STRUCTUREL UNIQUEMENT
# =========================================================
edges_struct <- matrix(c(
  "IMAG", "CUEX",
  "IMAG", "PERQ",
  "IMAG", "CUSA",
  "IMAG", "CUSL",
  "CUEX", "PERQ",
  "CUEX", "PERV",
  "PERQ", "PERV",
  "PERQ", "CUSA",
  "PERV", "CUSA",
  "CUSA", "CUSL"
), byrow = TRUE, ncol = 2)

g_struct <- graph_from_edgelist(edges_struct, directed = TRUE)

coords_struct <- rbind(
  IMAG = c(0, 0),
  CUEX = c(2, 1.2),
  PERQ = c(2, -1.2),
  PERV = c(4, 1.2),
  CUSA = c(4, 0),
  CUSL = c(6, 0)
)

coords_struct <- coords_struct[V(g_struct)$name, ]

par(mar = c(1, 1, 3, 1))
plot(
  g_struct,
  layout = coords_struct,
  vertex.size = 42,
  vertex.color = "#BFD7EA",
  vertex.frame.color = "gray30",
  vertex.label.color = "navy",
  vertex.label.cex = 1.1,
  edge.color = "gray35",
  edge.width = 1.8,
  edge.arrow.size = 0.5,
  edge.curved = 0,
  main = "Modèle structurel ECSI simplifié"
)

# =========================================================
# 5. SCHÉMA COMPLET : STRUCTURE + MESURE
# =========================================================
edges_full <- matrix(c(
  # -------------------------------------------------------
  # Modèle structurel
  # -------------------------------------------------------
  "IMAG", "CUEX",
  "IMAG", "PERQ",
  "IMAG", "CUSA",
  "IMAG", "CUSL",
  "CUEX", "PERQ",
  "CUEX", "PERV",
  "PERQ", "PERV",
  "PERQ", "CUSA",
  "PERV", "CUSA",
  "CUSA", "CUSL",
  
  # -------------------------------------------------------
  # Modèle de mesure
  # -------------------------------------------------------
  "IMAG", "IMAG1",
  "IMAG", "IMAG2",
  "IMAG", "IMAG3",
  "IMAG", "IMAG4",
  "IMAG", "IMAG5",
  
  "CUEX", "CUEX1",
  "CUEX", "CUEX2",
  "CUEX", "CUEX3",
  
  "PERQ", "PERQ1",
  "PERQ", "PERQ2",
  "PERQ", "PERQ3",
  "PERQ", "PERQ4",
  "PERQ", "PERQ5",
  "PERQ", "PERQ6",
  "PERQ", "PERQ7",
  
  "PERV", "PERV1",
  "PERV", "PERV2",
  
  "CUSA", "CUSA1",
  "CUSA", "CUSA2",
  "CUSA", "CUSA3",
  
  "CUSL", "CUSL1",
  "CUSL", "CUSL2",
  "CUSL", "CUSL3"
), byrow = TRUE, ncol = 2)

g_full <- graph_from_edgelist(edges_full, directed = TRUE)

coords_full <- rbind(
  # Latentes
  IMAG  = c(0.0,  0.0),
  CUEX  = c(2.0,  1.2),
  PERQ  = c(2.0, -1.2),
  PERV  = c(4.2,  1.8),
  CUSA  = c(4.2,  0.0),
  CUSL  = c(6.4,  0.0),
  
  # Items de IMAG
  IMAG1 = c(-0.8,  0.8),
  IMAG2 = c(-1.0,  0.3),
  IMAG3 = c(-1.0, -0.3),
  IMAG4 = c(-0.8, -0.8),
  IMAG5 = c( 0.0, -1.1),
  
  # Items de CUEX
  CUEX1 = c(1.0,  2.0),
  CUEX2 = c(2.0,  2.4),
  CUEX3 = c(3.0,  2.0),
  
  # Items de PERQ
  PERQ1 = c(0.8, -2.0),
  PERQ2 = c(1.5, -2.4),
  PERQ3 = c(2.5, -2.4),
  PERQ4 = c(3.2, -2.0),
  PERQ5 = c(3.4, -1.3),
  PERQ6 = c(0.6, -1.3),
  PERQ7 = c(2.0, -2.8),
  
  # Items de PERV
  PERV1 = c(3.7,  2.9),
  PERV2 = c(4.7,  2.9),
  
  # Items de CUSA
  CUSA1 = c(4.7,  0.9),
  CUSA2 = c(5.1,  0.0),
  CUSA3 = c(4.7, -0.9),
  
  # Items de CUSL
  CUSL1 = c(7.3,  0.9),
  CUSL2 = c(7.6,  0.0),
  CUSL3 = c(7.3, -0.9)
)

# Réordonner selon l'ordre des sommets du graphe
coords_full <- coords_full[V(g_full)$name, ]

# Styles
vertex_colors <- ifelse(V(g_full)$name %in% latents, "#BFD7EA", "#B7E4A5")
vertex_sizes  <- ifelse(V(g_full)$name %in% latents, 38, 28)
label_cex     <- ifelse(V(g_full)$name %in% latents, 1.0, 0.85)

is_structural <- ends(g_full, E(g_full))[, 1] %in% latents &
  ends(g_full, E(g_full))[, 2] %in% latents

edge_widths <- ifelse(is_structural, 1.8, 1.1)
edge_colors <- ifelse(is_structural, "gray35", "gray65")
arrow_sizes <- ifelse(is_structural, 0.45, 0.30)

par(mar = c(1, 1, 3, 1))
plot(
  g_full,
  layout = coords_full,
  vertex.size = vertex_sizes,
  vertex.color = vertex_colors,
  vertex.frame.color = "gray30",
  vertex.label.color = "navy",
  vertex.label.cex = label_cex,
  vertex.label.family = "sans",
  edge.color = edge_colors,
  edge.width = edge_widths,
  edge.arrow.size = arrow_sizes,
  edge.curved = 0,
  main = "Modèle ECSI simplifié : structure et mesure"
)

legend(
  "topleft",
  legend = c("Variables latentes", "Variables manifestes"),
  pt.bg = c("#BFD7EA", "#B7E4A5"),
  pch = 21,
  pt.cex = 2,
  bty = "n"
)
