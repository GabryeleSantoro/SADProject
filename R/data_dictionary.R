# SADProject - Data Dictionary
# Definizione centralizzata di tipi di colonne, valori validi ed etichette
# Utilizzato da anomalies.R, distributions.R e altre funzioni di analisi

#' Restituisce il dizionario completo dei dati del progetto SADProject.
#' Il dizionario contiene informazioni centralizzate su tipi di colonne,
#' valori validi per variabili categoriche, etichette per visualizzazione,
#' e descrizioni delle variabili.
#'
#' @return Lista con i seguenti elementi:
#'   \item{valid_values}{Lista che mappa nomi di colonne categoriche ai loro valori validi (vettori numerici)}
#'   \item{labels}{Lista che mappa nomi di colonne categoriche alle loro etichette di visualizzazione (vettori carattere)}
#'   \item{column_types}{Lista che mappa nomi di colonne ai loro tipi ("numeric" o "categorical")}
#'   \item{descriptions}{Lista che mappa nomi di colonne alle loro descrizioni testuali}
get_data_dictionary <- function() {
  list(
    # Valori validi per colonne categoriche (per validazione del dominio)
    valid_values = list(
      Gender = c(0, 1),
      fhwo = c(0, 1),
      FAVC = c(0, 1),
      SMOKE = c(0, 1),
      SCC = c(0, 1),
      FCVC = c(1, 2, 3),
      NCP = c(1, 2, 3, 4),
      CH2O = c(1, 2, 3),
      FAF = c(0, 1, 2, 3),
      TUE = c(0, 1, 2),
      CALC = c(0, 1, 2, 3),
      CAEC = c(0, 1, 2, 3),
      MTRANS = c(0, 1, 2, 3),
      class = c(1)
    ),
    
    # Etichette per valori categorici (per visualizzazione in grafici e report)
    labels = list(
      Gender = c("0" = "Femmina", "1" = "Maschio"),
      fhwo = c("0" = "No", "1" = "Sì"),
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
    
    # Tipi di colonne: "numeric", "categorical"
    column_types = list(
      Gender = "categorical",
      AGE = "numeric",
      Height = "numeric",
      Weight = "numeric",
      fhwo = "categorical",
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
    
    # Descrizioni delle colonne (cosa rappresenta la variabile)
    descriptions = list(
      Gender = "Genere del soggetto",
      AGE = "Età del soggetto in anni",
      Height = "Altezza del soggetto in metri",
      Weight = "Peso del soggetto in chilogrammi",
      fhwo = "Presenza di casi di sovrappeso in famiglia",
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
      BMI = "Indice di Massa Corporea (Weight / Height²)",
      bmi = "Indice di Massa Corporea (BMI = Weight / Height²)"
    )
  )
}

#' Restituisce i valori validi per tutte le colonne categoriche definite nel dizionario.
#' Questi valori sono utilizzati per la validazione del dominio durante l'analisi delle anomalie.
#'
#' @return Lista che mappa nomi di colonne categoriche ai loro valori validi.
#'   Ogni elemento della lista è un vettore numerico contenente i valori accettabili per quella colonna.
#'   Esempio: \code{list(Gender = c(0, 1), FCVC = c(1, 2, 3), ...)}
get_valid_values <- function() {
  dict <- get_data_dictionary()
  return(dict$valid_values)
}

#' Restituisce le etichette di visualizzazione per tutte le colonne categoriche.
#' Queste etichette sono utilizzate nei grafici e nei report per mostrare valori leggibili
#' invece di codici numerici.
#'
#' @return Lista che mappa nomi di colonne categoriche ai loro vettori di etichette.
#'   Ogni elemento è un vettore carattere con nomi corrispondenti ai valori numerici.
#'   Esempio: \code{list(Gender = c("0" = "Femmina", "1" = "Maschio"), ...)}
get_categorical_labels <- function() {
  dict <- get_data_dictionary()
  return(dict$labels)
}

#' Restituisce il tipo di una colonna specifica dal dizionario dati.
#' Il tipo può essere "numeric" per variabili continue o "categorical" per variabili discrete.
#'
#' @param col_name Stringa carattere. Nome della colonna di cui si vuole conoscere il tipo.
#'   Deve corrispondere esattamente al nome presente nel dizionario.
#'
#' @return Stringa carattere. Tipo della colonna ("numeric" o "categorical").
#'   Restituisce \code{NULL} se la colonna non è presente nel dizionario.
get_column_type <- function(col_name) {
  dict <- get_data_dictionary()
  return(dict$column_types[[col_name]])
}

#' Restituisce i nomi di tutte le colonne categoriche definite nel dizionario.
#' La colonna 'class' (variabile target) viene esclusa automaticamente.
#'
#' @return Vettore carattere. Vettore di nomi delle colonne categoriche,
#'   esclusa la colonna 'class'. Include variabili come Gender, FAVC, FCVC, NCP, ecc.
get_categorical_columns <- function() {
  dict <- get_data_dictionary()
  cols <- names(dict$valid_values)
  # Escludi la colonna 'class'
  return(cols[cols != "class"])
}

#' Restituisce i nomi di tutte le colonne numeriche definite nel dizionario.
#' Include solo le variabili numeriche originali (AGE, Height, Weight),
#' non le feature derivate come BMI.
#'
#' @return Vettore carattere. Vettore di nomi delle colonne numeriche originali.
#'   Tipicamente include: c("AGE", "Height", "Weight")
get_numeric_columns <- function() {
  dict <- get_data_dictionary()
  types <- dict$column_types
  return(names(types)[types == "numeric"])
}

#' Restituisce la descrizione testuale di una colonna specifica dal dizionario.
#' La descrizione spiega cosa rappresenta la variabile nel contesto del dataset.
#'
#' @param col_name Stringa carattere. Nome della colonna di cui si vuole la descrizione.
#'   Deve corrispondere esattamente al nome presente nel dizionario.
#'
#' @return Stringa carattere. Descrizione testuale della colonna.
#'   Restituisce \code{NULL} se la colonna non è presente nel dizionario.
get_column_description <- function(col_name) {
  dict <- get_data_dictionary()
  return(dict$descriptions[[col_name]])
}

#' Restituisce i nomi di tutte le colonne definite nel dizionario, sia numeriche che categoriche.
#' La colonna 'class' (variabile target) viene esclusa automaticamente.
#' Utile per ottenere una lista completa di tutte le variabili disponibili per l'analisi.
#'
#' @return Vettore carattere. Vettore di nomi di tutte le colonne (numeriche e categoriche),
#'   esclusa la colonna 'class'. Include variabili originali ma non le feature derivate.
get_all_columns <- function() {
  dict <- get_data_dictionary()
  all_cols <- unique(c(
    names(dict$column_types),
    names(dict$valid_values),
    names(dict$descriptions)
  ))
  return(all_cols[all_cols != "class"])
}

#' Restituisce i nomi delle colonne numeriche originali del dataset,
#' escludendo le feature derivate come BMI, Diet_Quality_Index, Activity_Index.
#' Queste sono le variabili numeriche presenti nel dataset originale prima di qualsiasi
#' trasformazione o creazione di feature.
#'
#' @return Vettore carattere. Vettore con i nomi delle colonne numeriche originali:
#'   c("AGE", "Height", "Weight")
get_original_numeric_columns <- function() {
  # Colonne numeriche originali (prima della creazione di feature derivate)
  return(c("AGE", "Height", "Weight"))
}

#' Restituisce i nomi delle feature derivate create durante l'analisi del dataset.
#' Queste feature sono calcolate a partire dalle variabili originali e includono:
#' BMI (calcolato da Weight e Height), Diet_Quality_Index (da variabili dietetiche),
#' e Activity_Index (da variabili di attività fisica).
#'
#' @return Vettore carattere. Vettore con i nomi delle feature derivate:
#'   c("BMI", "Diet_Quality_Index", "Activity_Index")
get_derived_features <- function() {
  return(c("BMI", "Diet_Quality_Index", "Activity_Index"))
}

#' Restituisce i nomi di tutte le colonne numeriche, sia originali che derivate.
#' Include le variabili numeriche originali (AGE, Height, Weight) e le feature derivate
#' (BMI, Diet_Quality_Index, Activity_Index).
#'
#' @return Vettore carattere. Vettore con i nomi di tutte le colonne numeriche:
#'   c("AGE", "Height", "Weight", "BMI", "Diet_Quality_Index", "Activity_Index")
get_all_numeric_columns <- function() {
  original <- get_original_numeric_columns()
  derived <- get_derived_features()
  return(c(original, derived))
}

#' Restituisce i nomi delle colonne numeriche tipicamente utilizzate nell'analisi bivariata.
#' Include le variabili numeriche originali più BMI, che è la feature derivata più comunemente
#' utilizzata per analisi di correlazione e scatter plots.
#'
#' @return Vettore carattere. Vettore con i nomi delle colonne numeriche per analisi bivariata:
#'   c("Weight", "Height", "AGE", "BMI")
get_bivariate_numeric_columns <- function() {
  # Include original numeric columns + BMI (most commonly used)
  return(c("Weight", "Height", "AGE", "BMI"))
}

#' Restituisce i nomi delle colonne numeriche da controllare per la presenza di outlier.
#' Include le variabili numeriche originali più BMI, che viene controllato con un dominio
#' specifico (Obesity Lev 1: 30.0-34.9) invece del metodo IQR standard.
#'
#' @return Vettore carattere. Vettore con i nomi delle colonne numeriche per controllo outlier:
#'   c("Weight", "Height", "AGE", "BMI")
get_outlier_numeric_columns <- function() {
  # Include original numeric columns + BMI
  return(c("Weight", "Height", "AGE", "BMI"))
}

#' Restituisce i nomi di tutte le colonne numeriche (originali e derivate) da utilizzare
#' per l'analisi di adattamento a distribuzioni note. Include tutte le variabili numeriche
#' per verificare se seguono distribuzioni statistiche teoriche (es. normale, uniforme).
#'
#' @return Vettore carattere. Vettore con i nomi di tutte le colonne numeriche:
#'   c("AGE", "Height", "Weight", "BMI", "Diet_Quality_Index", "Activity_Index")
get_distribution_fit_numeric_columns <- function() {
  return(get_all_numeric_columns())
}

