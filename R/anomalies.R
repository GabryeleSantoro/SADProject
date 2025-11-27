library(isotree)
library(dplyr)

check_domain_anomalies <- function(data) {
  
  if (!is.data.frame(data)) {
    stop("L'input deve essere un data.frame.")
  }
  
  # Source data dictionary if not already available
  if (!exists("get_valid_values", envir = .GlobalEnv)) {
    source("R/data_dictionary.R")
  }
  
  # Get valid values from centralized data dictionary
  valid_values <- get_valid_values()
  
  rows_to_remove <- rep(FALSE, nrow(data))
  total_removed <- 0
  
  for (col_name in names(valid_values)) {
    if (!col_name %in% names(data)) {
      next
    }
    
    column_data <- data[[col_name]]
    allowed <- valid_values[[col_name]]
    
    if (is.numeric(column_data)) {
      column_checked <- round(column_data)
    } else {
      column_checked <- column_data
    }
    
    invalid_mask <- is.na(column_checked) | !(column_checked %in% allowed)
    
    if (any(invalid_mask)) {
      rows_to_remove <- rows_to_remove | invalid_mask
      n_invalid <- sum(invalid_mask)
      total_removed <- total_removed + n_invalid
      invalid_examples <- unique(column_checked[invalid_mask])
      cat(sprintf("Righe rimosse per colonna '%s': %d (valori invalidi: %s)\n",
                  col_name,
                  n_invalid,
                  paste(invalid_examples, collapse = ", ")))
    }
  }
  
  if (!any(rows_to_remove)) {
    cat("Nessuna violazione delle regole categoriche rilevata.\n")
    return(data)
  }
  
  cleaned_data <- data[!rows_to_remove, , drop = FALSE]
  cat(sprintf("\nTotale righe rimosse: %d\n", sum(rows_to_remove)))
  cat(sprintf("Righe rimanenti: %d\n", nrow(cleaned_data)))
  
  return(cleaned_data)
}

detect_statistical_outliers <- function(data, contamination = 0.05, remove_outliers = FALSE) {
  cat("\n--- RILEVAMENTO OUTLIER STATISTICI (Isolation Forest) ---\n")
  
  # 1. Selezioniamo solo le feature numeriche/rilevanti
  # Escludiamo la variabile target 'class' per non influenzare l'analisi
  numeric_cols <- sapply(data, is.numeric)
  numeric_cols[names(numeric_cols) == "class"] <- FALSE
  if (sum(numeric_cols) < 2) {
    stop("Servono almeno due colonne numeriche per l'Isolation Forest.")
  }
  features <- data[, numeric_cols, drop = FALSE]
  feature_matrix <- as.matrix(features)
  
  # ntrees: numero di alberi (default 100 va bene)
  iso_model <- isolation.forest(feature_matrix,
                                ntrees = 100,
                                ndim = 1,
                                seed = 42)
  
  # In isotree, valori vicini a 1 sono anomalie, vicini a 0 sono normali
  scores <- predict(iso_model, feature_matrix)
  
  # Usiamo il quantile basato sulla contaminazione attesa (es. top 5%)
  threshold <- quantile(scores, 1 - contamination)
  
  # Identifica gli outlier
  is_outlier <- scores >= threshold
  n_outliers <- sum(is_outlier)
  
  cat(sprintf("Individuati %d outlier statistici (%.1f%% del dataset).\n", 
              n_outliers, contamination * 100))
  
  if (remove_outliers) {
    # Rimuovi gli outlier e restituisci solo i dati puliti
    data_clean <- data[!is_outlier, , drop = FALSE]
    cat(sprintf("Rimossi %d outlier. Righe rimanenti: %d\n", 
                n_outliers, nrow(data_clean)))
    return(data_clean)
  } else {
    # Aggiungi colonne informative ma mantieni tutti i dati
    data_out <- data %>%
      mutate(anomaly_score = scores,
             is_outlier = is_outlier)
    return(data_out)
  }
}
