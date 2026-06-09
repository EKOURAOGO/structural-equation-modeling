# Structural Equation Modeling (SEM) - ECSI & Russet

> Modèles à équations structurelles en R · PLS-PM · LISREL · lavaan · RFPC · CLV  
> Données de téléphonie mobile (modèle ECSI simplifié) 

---

## Contexte

Ce projet applique plusieurs approches de modélisation par équations structurelles (SEM) à un jeu de données issu du secteur de la **téléphonie mobile** (250 individus, 23 variables manifestes).

L'objectif est triple :
- Estimer et comparer **LISREL**, **PLS-PM** et **RFPC** sur la même structure théorique
- Explorer la structure empirique via une **classification de variables (CLV)**
- Valider les méthodes par une **étude de simulation** avec modèle générateur connu

> Encadrant : Christian DERQUENNE

---

## Modèle ECSI simplifié

| Variable latente | Abrév. | Nb d'items |
|---|---|---|
| Image | IMAG | 5 |
| Attentes | CUEX | 3 |
| Qualité perçue | PERQ | 7 |
| Valeur perçue | PERV | 2 |
| Satisfaction | CUSA | 3 |
| Fidélité | CUSL | 3 |

---

## Structure du projet

```
structural-equation-modeling/
├── ESMQ1.R     # Exploration descriptive
├── ESMQ2.R     # Spécification du modèle ECSI
├── ESMQ3.R     # Unidimensionnalité (ACP, alpha, RIT)
├── ESMQ4.R     # LISREL via lavaan (MLR)
├── ESMQ6.R     # PLS-PM + bootstrap
├── ESMQ7.R     # RFPC
├── ESMQ8.R     # Synthèse comparative LISREL / PLS-PM / RFPC
├── ESMQ9.R     # Poids externes & scores latents
├── ESMQ10.R    # Modèle libre CLV
├── ESMQ11.R    # Modèle expert PLS + RFPC
├── ESMQ12.R    # Étude de simulation
└── RAPPORT.pdf # Rapport complet
```

---

## Méthodes implémentées

- **LISREL** (lavaan, MLR) - CFI, TLI, RMSEA, SRMR, AVE, Fornell-Larcker, HTMT
- **PLS-PM** (plspm) - Mode A réflexif, bootstrap, R², GOF, outer weights
- **RFPC** - ACP bloc + régressions structurelles sur scores factoriels
- **CLV** - Classification hiérarchique sur 23 variables manifestes
- **Modèle expert** - Réaffectation de CUSA3 → bloc PERV
- **Simulation** - Validation par récupération des paramètres vrais

---

## Principaux résultats

### Qualité du modèle de mesure

| Bloc | Alpha | DG.rho | AVE | Diagnostic |
|---|---|---|---|---|
| IMAG | 0.714 | 0.819 | 0.311 | Acceptable |
| CUEX | 0.433 | 0.732 | 0.221 | Fragile |
| PERQ | 0.872 | 0.905 | 0.496 | Très bon |
| PERV | 0.817 | 0.919 | 0.692 | Très bon |
| CUSA | 0.770 | 0.872 | 0.555 | Bon |
| CUSL | 0.442 | 0.729 | 0.326 | Fragile |

### Conclusion comparative

| Critère | LISREL | PLS-PM | RFPC |
|---|---|---|---|
| Stabilité structurelle | Faible | Bonne | Bonne |
| Robustesse non-normalité | Moyenne | Elevée | Elevée |
| Recommandé | Non | Oui | Oui |

---

## Installation

```r
install.packages(c("lavaan","plspm","psych","corrplot","ggplot2",
                   "factoextra","cluster","semPlot","semTools","dplyr","tidyr"))
```

---

## Utilisation

```r
source("ESMQ1.R")   # Exploration
source("ESMQ2.R")   # Spécification
source("ESMQ3.R")   # Unidimensionnalité
source("ESMQ4.R")   # LISREL
source("ESMQ6.R")   # PLS-PM
source("ESMQ7.R")   # RFPC
source("ESMQ8.R")   # Synthèse
source("ESMQ9.R")   # Poids & scores
source("ESMQ10.R")  # CLV
source("ESMQ11.R")  # Modèle expert
source("ESMQ12.R")  # Simulation
```

---


## Stack technique

![R](https://img.shields.io/badge/R-276DC3?style=flat-square&logo=r&logoColor=white)
![lavaan](https://img.shields.io/badge/lavaan-CFA%20·%20SEM-blue?style=flat-square)
![plspm](https://img.shields.io/badge/plspm-PLS--PM-green?style=flat-square)
![psych](https://img.shields.io/badge/psych-Alpha%20·%20ACP-orange?style=flat-square)
![ggplot2](https://img.shields.io/badge/ggplot2-visualization-red?style=flat-square)
![factoextra](https://img.shields.io/badge/factoextra-CLV%20·%20dendrogrammes-purple?style=flat-square)
![semPlot](https://img.shields.io/badge/semPlot-graphes%20SEM-darkblue?style=flat-square)

---

## Auteur

**KOURAOGO Emmanuel**   
Data Scientist & Data Analyst 

[![GitHub](https://img.shields.io/badge/GitHub-EKOURAOGO-181717?style=flat-square&logo=github)](https://github.com/EKOURAOGO)

