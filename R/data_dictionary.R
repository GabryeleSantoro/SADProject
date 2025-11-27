# SADProject - Data Dictionary
# Centralized definition of column types, valid values, and labels
# Used by anomalies.R, distributions.R, and other analysis functions

#' Get the complete data dictionary
#'
#' @return List containing valid_values, labels, and column_types
get_data_dictionary <- function() {
  list(
    # Valid values for categorical columns (for domain validation)
    valid_values = list(
      Gender = c(0, 1),
      family_history_with_overweight = c(0, 1),
      FAVC = c(0, 1),
      SMOKE = c(0, 1),
      SCC = c(0, 1),
      FCVC = c(1, 2, 3),
      NCP = c(1, 2, 3, 4),
      CH2O = c(1, 2, 3),
      FAF = c(0, 1, 2, 3),
      TUE = c(0, 1, 2),
      CALC = c(0, 1, 2, 3),
      MTRANS = c(0, 1, 2, 3),
      class = c(1)
    ),
    
    # Labels for categorical values (for display in plots and reports)
    labels = list(
      Gender = c("0" = "Femmina", "1" = "Maschio"),
      family_history_with_overweight = c("0" = "No", "1" = "Sì"),
      FAVC = c("0" = "No", "1" = "Sì"),
      FCVC = c("1" = "Mai", "2" = "A volte", "3" = "Sempre"),
      NCP = c("1" = "1 pasto", "2" = "2 pasti", "3" = "3 pasti", "4" = "4+ pasti"),
      CAEC = c("0" = "No", "1" = "A volte", "2" = "Frequentemente", "3" = "Sempre"),
      SMOKE = c("0" = "No", "1" = "Sì"),
      CH2O = c("1" = "<1L", "2" = "1-2L", "3" = ">2L"),
      SCC = c("0" = "No", "1" = "Sì"),
      FAF = c("0" = "Nessuna", "1" = "1-2 giorni", "2" = "2-4 giorni", "3" = "4-5 giorni"),
      TUE = c("0" = "0-2 ore", "1" = "3-5 ore", "2" = ">5 ore"),
      CALC = c("0" = "Mai", "1" = "A volte", "2" = "Frequentemente", "3" = "Sempre"),
      MTRANS = c("0" = "Automobile", "1" = "Moto", "2" = "Bicicletta", "3" = "Trasporto pubblico/Piedi"),
      class = c("1" = "Obeso")
    ),
    
    # Column types: "numeric", "categorical"
    column_types = list(
      Gender = "categorical",
      AGE = "numeric",
      Height = "numeric",
      Weight = "numeric",
      family_history_with_overweight = "categorical",
      FAVC = "categorical",
      FCVC = "categorical",
      NCP = "categorical",
      CAEC = "categorical",
      SMOKE = "categorical",
      CH2O = "categorical",
      SCC = "categorical",
      FAF = "categorical",
      TUE = "categorical",
      CALC = "categorical",
      MTRANS = "categorical",
      class = "categorical"
    ),
    
    # Column descriptions (what the variable represents)
    descriptions = list(
      Gender = "Genere del soggetto",
      AGE = "Età del soggetto in anni",
      Height = "Altezza del soggetto in metri",
      Weight = "Peso del soggetto in chilogrammi",
      family_history_with_overweight = "Presenza di casi di sovrappeso in famiglia",
      FAVC = "Consumo frequente di cibi ad alto contenuto calorico",
      FCVC = "Frequenza di consumo di verdure",
      NCP = "Numero di pasti principali al giorno",
      CAEC = "Consumo di cibo tra i pasti",
      SMOKE = "Abitudine al fumo",
      CH2O = "Quantità di acqua bevuta al giorno (litri)",
      SCC = "Monitoraggio del consumo calorico",
      FAF = "Frequenza di attività fisica settimanale",
      TUE = "Tempo giornaliero trascorso con dispositivi tecnologici",
      CALC = "Consumo di alcol",
      MTRANS = "Mezzo di trasporto principale",
      class = "Classe target (livello di obesità)",
      bmi = "Indice di Massa Corporea (BMI = Weight / Height²)"
    )
  )
}

#' Get valid values for categorical columns
#'
#' @return List mapping column names to valid value vectors
get_valid_values <- function() {
  dict <- get_data_dictionary()
  return(dict$valid_values)
}

#' Get labels for categorical values
#'
#' @return List mapping column names to value label vectors
get_categorical_labels <- function() {
  dict <- get_data_dictionary()
  return(dict$labels)
}

#' Get column type (numeric or categorical)
#'
#' @param col_name Character string. Column name
#' @return Character string. Column type or NULL if not found
get_column_type <- function(col_name) {
  dict <- get_data_dictionary()
  return(dict$column_types[[col_name]])
}

#' Get all categorical column names
#'
#' @return Character vector. Names of categorical columns
get_categorical_columns <- function() {
  dict <- get_data_dictionary()
  return(names(dict$valid_values))
}

#' Get all numeric column names
#'
#' @return Character vector. Names of numeric columns
get_numeric_columns <- function() {
  dict <- get_data_dictionary()
  types <- dict$column_types
  return(names(types)[types == "numeric"])
}

#' Get column description
#'
#' @param col_name Character string. Column name
#' @return Character string. Column description or NULL if not found
get_column_description <- function(col_name) {
  dict <- get_data_dictionary()
  return(dict$descriptions[[col_name]])
}

