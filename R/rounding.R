# rounding.R
# Funzioni per arrotondare i valori numerici del dataset

#' Arrotonda i valori delle variabili numeriche specificate nel dataset al numero di cifre
#' decimali appropriato. La funzione supporta valori predefiniti ottimizzati per ogni variabile
#' (AGE=0, Weight=1, Height=2, BMI=2) oppure valori personalizzati.
#'
#' La funzione identifica automaticamente quali valori vengono modificati dall'arrotondamento
#' e stampa un report dettagliato per ogni variabile. Se un valore è già arrotondato correttamente,
#' non viene modificato.
#'
#' @param data Data frame. Dataset contenente le variabili numeriche da arrotondare.
#' @param variables Vettore carattere o NULL. Nomi delle variabili numeriche da arrotondare.
#'   Se NULL, usa le variabili predefinite: c("AGE", "Weight", "Height", "BMI").
#'   Predefinito: NULL.
#' @param digits Vettore numerico o NULL. Numero di cifre decimali per l'arrotondamento.
#'   Se NULL, usa valori predefiniti appropriati per ogni variabile:
#'   \itemize{
#'     \item AGE: 0 decimali (anni interi)
#'     \item Weight: 1 decimale (chilogrammi)
#'     \item Height: 2 decimali (metri)
#'     \item BMI: 2 decimali
#'   }
#'   Se un singolo valore numerico, viene applicato a tutte le variabili.
#'   Se un vettore, deve avere la stessa lunghezza di variables.
#'
#' @return Data frame. Il dataset originale con le variabili numeriche arrotondate.
#'   Il numero di valori modificati per ogni variabile viene stampato a console.
round_numeric_variables <- function(data,
                                    variables = NULL,
                                    digits = NULL) {
  
  # Se non specificate, usa le variabili numeriche comuni
  if (is.null(variables)) {
    variables <- c("AGE", "Weight", "Height", "BMI")
  }
  
  # Filtra le variabili presenti nel dataset
  available_vars <- variables[variables %in% colnames(data)]
  
  # Se non specificate le cifre decimali, usa valori di default appropriati
  if (is.null(digits)) {
    digits_list <- list(
      "AGE" = 0,        # Età: arrotonda agli interi (anni)
      "Weight" = 1,     # Peso: arrotonda a 1 decimale (kg)
      "Height" = 2,     # Altezza: arrotonda a 2 decimali (metri)
      "BMI" = 2         # BMI: arrotonda a 2 decimali
    )
  } else if (length(digits) == 1) {
    # Se viene fornito un singolo valore, usa lo stesso per tutte le variabili
    digits_list <- setNames(rep(digits, length(available_vars)), available_vars)
  } else {
    # Se viene fornito un vettore, deve avere la stessa lunghezza delle variabili
    if (length(digits) != length(available_vars)) {
      stop("Il numero di cifre decimali deve corrispondere al numero di variabili")
    }
    digits_list <- setNames(digits, available_vars)
  }
  
  data_rounded <- data
  
  for (var_name in available_vars) {
    if (!var_name %in% colnames(data_rounded)) {
      cat(sprintf("⚠️  Variabile '%s' non trovata nel dataset, saltata\n", var_name))
      next
    }
    
    var_data <- data_rounded[[var_name]]
    
    # Verifica che sia numerica
    if (!is.numeric(var_data)) {
      cat(sprintf("⚠️  '%s' non è una variabile numerica, saltata\n", var_name))
      next
    }
    
    # Ottiene il numero di cifre decimali per questa variabile
    if (is.null(digits)) {
      n_digits <- ifelse(var_name %in% names(digits_list), 
                        digits_list[[var_name]], 
                        2)  # Predefinito: 2 decimali
    } else if (length(digits) == 1) {
      n_digits <- digits
    } else {
      n_digits <- digits_list[[var_name]]
    }
    
    # Arrotonda i valori
    original_values <- var_data
    data_rounded[[var_name]] <- round(var_data, digits = n_digits)
    
    # Conta quanti valori sono stati modificati
    n_changed <- sum(original_values != data_rounded[[var_name]], na.rm = TRUE)
    
    if (n_changed > 0) {
      cat(sprintf("✓ '%s' arrotondato a %d decimali: %d valori modificati\n", 
                  var_name, n_digits, n_changed))
    } else {
      cat(sprintf("✓ '%s' già arrotondato a %d decimali (nessuna modifica)\n", 
                  var_name, n_digits))
    }
  }
  
  cat(sprintf("\n✓ Arrotondamento completato per %d variabili\n", length(available_vars)))
  
  return(data_rounded)
}
