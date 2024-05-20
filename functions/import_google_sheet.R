library(googlesheets4)
library(dplyr)
library(tidyr)

# Fonction pour importer les données depuis Google Sheets
importer_google_sheet <- function(url) {
  data <- read_sheet(url)
  data = data %>%
    mutate(Exercice = tolower(Exercice))
  
  return(data)
}

# Fonction pour séparer les valeurs séparées par des virgules en colonnes distinctes
separer_valeurs <- function(data, colonne) {
  valeurs <- strsplit(data[[colonne]], ",")
  max_valeurs <- max(sapply(valeurs, length))
  noms_colonnes <- paste0(colonne, "_", 1:max_valeurs)
  
  valeurs <- lapply(valeurs, function(x) {
    if (length(x) < max_valeurs) {
      x <- c(x, rep(NA, max_valeurs - length(x)))
    }
    as.numeric(x)
  })
  
  data[noms_colonnes] <- as.data.frame(do.call(rbind, valeurs))
  data
}

# Fonction principale pour traiter les données
traiter_google_sheet <- function(url) {
  data <- importer_google_sheet(url)
  data <- separer_valeurs(data, "Répétitions")
  data <- separer_valeurs(data, "Poids")
  
  # Convertir les données en lignes
  data <- data %>%
    pivot_longer(cols = starts_with(c("Répétitions", "Poids")), 
                 names_to = c(".value", "série"),
                 names_pattern = "([^_]+)_(\\d+)") %>% 
    na.omit() %>% 
    mutate(Date = as.Date(Date),
           week = week(Date),
           semaine = weekdays(Date),
           mois = months(Date),
           annee = year(Date))
  
  return(data)
}

