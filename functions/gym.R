# Fonction pour calculer le 1RM
calculer_1RM_brycki <- function(poids, reps) {
  # Formule d'estimation du 1RM basée sur la formule d'Epley
  return(round(poids / (1.0278 - (0.0278*reps)),1))
}

# Fonction pour calculer le 1RM
calculer_1RM_epley <- function(poids, reps) {
  # Formule d'estimation du 1RM basée sur la formule d'Epley
  return(round(poids*(1+ (0.0333*reps)),1))
}

# Fonction pour calculer le 1RM
calculer_1RM_lander <- function(poids, reps) {
  # Formule d'estimation du 1RM basée sur la formule d'Epley
  return(round((100*poids)/(101.3-(2.67123*reps)),1))
}

# Fonction pour calculer le 1RM
calculer_1RM_lombardi <- function(poids, reps) {
  # Formule d'estimation du 1RM basée sur la formule d'Epley
  return(round((poids*reps^0.1),1))
}

# Fonction pour calculer le 1RM
calculer_1RM_mayhew <- function(poids, reps) {
  # Formule d'estimation du 1RM basée sur la formule d'Epley
  return(round((100*poids)/(52.2 + (41.9*exp(-0.055 * reps))),1))
}

# Fonction pour calculer le 1RM
calculer_1RM_oconner <- function(poids, reps) {
  # Formule d'estimation du 1RM basée sur la formule d'Epley
  return(round(poids * (1 + 0.025*reps),1))
}

# Fonction pour calculer le 1RM
calculer_1RM_wathan <- function(poids, reps) {
  # Formule d'estimation du 1RM basée sur la formule d'Epley
  return(round((100*poids)/(48.8 + (53.8 * exp(-0.075*reps))),1))
}

# Fonction pour obtenir le maximum parmi les résultats de toutes les fonctions de calcul de 1RM
obtenir_max_1RM <- function(poids, reps) {
  # Appliquer chaque fonction de calcul de 1RM à la paire de poids et de répétitions
  resultats <- c(
    calculer_1RM_brycki(poids, reps),
    calculer_1RM_epley(poids, reps),
    calculer_1RM_lander(poids, reps),
    calculer_1RM_lombardi(poids, reps),
    calculer_1RM_mayhew(poids, reps),
    calculer_1RM_oconner(poids, reps),
    calculer_1RM_wathan(poids, reps)
  )
  
  # Retourner le maximum parmi tous les résultats
  max_resultat <- unique(max(resultats))
  return(max_resultat)
}

# Fonction pour obtenir le maximum parmi les résultats de toutes les fonctions de calcul de 1RM
obtenir_min_1RM <- function(poids, reps) {
  # Appliquer chaque fonction de calcul de 1RM à la paire de poids et de répétitions
  resultats <- c(
    calculer_1RM_brycki(poids, reps),
    calculer_1RM_epley(poids, reps),
    calculer_1RM_lander(poids, reps),
    calculer_1RM_lombardi(poids, reps),
    calculer_1RM_mayhew(poids, reps),
    calculer_1RM_oconner(poids, reps),
    calculer_1RM_wathan(poids, reps)
  )
  
  # Retourner le maximum parmi tous les résultats
  min_resultat <- unique(min(resultats))
  return(min_resultat)
}



