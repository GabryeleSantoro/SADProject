# anomalies.R

# Limiti BMI per Obesity Lev 1 (Class I obesity, WHO):
# Valori validi: 30.0 <= BMI <= 34.9 kg/m²
# Tutti i valori fuori da questo intervallo sono anomalie
BMI_OBESITY_LEV1_MIN <- 30.0
BMI_OBESITY_LEV1_MAX <- 34.9

#' Identifica valori anomali (outlier) nelle variabili quantitative utilizzando due metodi:
#' \itemize{
#'   \item \strong{BMI}: Utilizza un dominio specifico basato sulla classificazione WHO Obesity Lev 1.
#'     Valori validi: 30.0 <= BMI <= 34.9 kg/m². Tutti i valori fuori da questo intervallo sono considerati anomalie.
#'   \item \strong{Altre variabili}: Utilizza il metodo IQR (Interquartile Range) standard dei boxplot.
#'     Valori anomali sono quelli al di fuori di [Q1 - 1.5*IQR, Q3 + 1.5*IQR].
#' }
#'
#' La funzione genera un boxplot per ogni variabile analizzata, con linee di riferimento per i limiti
#' di anomalia. I valori anomali vengono identificati e segnalati, ma \strong{NON vengono rimossi}
#' dal dataset (la funzione restituisce il dataset originale invariato).
#'
#' @param data Data frame. Dataset contenente le variabili numeriche da analizzare.
#' @param variables Vettore carattere. Nomi delle variabili numeriche da controllare per outlier.
#'   Predefinito: c("Weight", "Height", "AGE", "BMI").
#' @param output_dir Stringa carattere. Directory dove salvare i boxplot generati.
#'   Predefinito: "output/anomalie". La directory viene creata automaticamente se non esiste.
#'
#' @return Data frame. Il dataset originale senza modifiche. I valori anomali vengono solo
#'   identificati e segnalati, ma non rimossi. Il numero totale di outlier trovati viene stampato
#'   a console insieme ai dettagli per ogni variabile.
anomalies_quantitative <- function(data,
                                   variables = c("Weight", "Height", "AGE", "BMI"),
                                   output_dir = "output/anomalie") {
  # Crea la directory di output se non esiste
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Filtra le variabili presenti nel dataset
  available_vars <- variables[variables %in% colnames(data)]
  if (length(available_vars) == 0) {
    stop("Nessuna variabile quantitativa trovata nel dataset")
  }
  
  # Crea un vettore per tracciare le righe con outlier
  rows_with_outliers <- logical(nrow(data))
  
  total_anomalies <- 0
  
  for (var_name in available_vars) {
    # Estrae i dati della variabile
    var_data <- data[[var_name]]
    
    # Verifica che sia numerica
    if (!is.numeric(var_data)) {
      cat(sprintf("⚠️  '%s' non è una variabile numerica, saltata\n", var_name))
      next
    }
    
    # BMI: dominio Obesity Lev 1 (30.0 - 34.9). Fuori = anomalia
    if (var_name == "BMI") {
      lower_bound <- BMI_OBESITY_LEV1_MIN
      upper_bound <- BMI_OBESITY_LEV1_MAX
      outlier_mask <- var_data < lower_bound | var_data > upper_bound
      outliers <- var_data[outlier_mask]
      outliers <- outliers[!is.na(outliers)]
      n_outliers <- length(outliers)
      q1 <- NA
      q3 <- NA
      iqr <- NA
    } else {
      # Altre variabili: metodo IQR (boxplot)
      box_stats <- boxplot.stats(var_data)
      outliers <- box_stats$out
      n_outliers <- length(outliers)
      q1 <- quantile(var_data, 0.25, na.rm = TRUE)
      q3 <- quantile(var_data, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      outlier_mask <- var_data < lower_bound | var_data > upper_bound
    }
    
    outlier_indices <- which(outlier_mask)
    
    # Marca queste righe come contenenti outlier
    rows_with_outliers[outlier_indices] <- TRUE
    
    # Stampa informazioni sui valori anomali
    if (n_outliers > 0) {
      total_anomalies <- total_anomalies + n_outliers
      cat(sprintf("⚠️  VALORI ANOMALI trovati in '%s': %d outlier in %d righe\n", 
                  var_name, n_outliers, length(outlier_indices)))
      if (var_name == "BMI") {
        cat(sprintf("   Dominio valido (Obesity Lev 1): [%.1f, %.1f] kg/m²\n", lower_bound, upper_bound))
        cat(sprintf("   Valori fuori classe (anomalie): %s\n\n", 
                    paste(sort(unique(outliers)), collapse = ", ")))
      } else {
        cat(sprintf("   Q1: %.2f, Q3: %.2f, IQR: %.2f\n", q1, q3, iqr))
        cat(sprintf("   Limite inferiore: %.2f, Limite superiore: %.2f\n", lower_bound, upper_bound))
        cat(sprintf("   Valori anomali: %s\n\n", 
                    paste(sort(unique(outliers)), collapse = ", ")))
      }
    } else {
      cat(sprintf("✓ Nessun valore anomalo trovato in '%s'\n", var_name))
    }
    
    # Prepara il file di output
    png_file <- file.path(output_dir, paste0(var_name, "_boxplot.png"))
    png(png_file, width = 800, height = 600, res = 150)
    
    # Crea il boxplot
    par(mar = c(5, 5, 4, 2))
    if (var_name == "BMI") {
      boxplot(var_data,
              main = sprintf("Boxplot di %s (Obesity Lev 1)\n(Anomalie: BMI ∉ [30, 34.9] = %d)", var_name, n_outliers),
              ylab = var_name,
              col = "lightblue",
              border = "black",
              notch = FALSE,
              outline = TRUE)
      stats_text <- sprintf(
        "Obesity Lev 1: [%.1f, %.1f]\nMediana: %.2f\nAnomalie: %d",
        lower_bound, upper_bound, median(var_data, na.rm = TRUE), n_outliers
      )
    } else {
      boxplot(var_data,
              main = sprintf("Boxplot di %s\n(Valori anomali: %d)", var_name, n_outliers),
              ylab = var_name,
              col = "lightblue",
              border = "black",
              notch = FALSE,
              outline = TRUE)
      stats_text <- sprintf(
        "Q1: %.2f\nMediana: %.2f\nQ3: %.2f\nIQR: %.2f\nOutlier: %d",
        q1, median(var_data), q3, iqr, n_outliers
      )
    }
    
    # Aggiunge linee di riferimento per i limiti
    abline(h = lower_bound, col = "red", lty = 2, lwd = 1.5)
    abline(h = upper_bound, col = "red", lty = 2, lwd = 1.5)
    
    # Aggiunge annotazioni per i limiti
    text(x = 0.5, y = lower_bound, 
         labels = if (var_name == "BMI") sprintf("Obesity Lev 1 inf: %.1f", lower_bound) 
                  else sprintf("Limite inf: %.2f", lower_bound),
         pos = 3, cex = 0.7, col = "red")
    text(x = 0.5, y = upper_bound,
         labels = if (var_name == "BMI") sprintf("Obesity Lev 1 sup: %.1f", upper_bound) 
                  else sprintf("Limite sup: %.2f", upper_bound),
         pos = 1, cex = 0.7, col = "red")
    legend("topright",
           legend = stats_text,
           bty = "n",
           cex = 0.8,
           bg = "white")
    
    dev.off()
    cat(sprintf("✓ Boxplot salvato: %s\n", png_file))
  }
  
  # Conta le righe con outlier (solo per report, non vengono rimosse)
  n_rows_with_outliers <- sum(rows_with_outliers)
  
  cat(sprintf("\n✓ Analisi valori anomali completata.\n"))
  cat(sprintf("   Totale outlier trovati: %d\n", total_anomalies))
  cat(sprintf("   Righe con outlier: %d (su %d righe totali)\n", 
              n_rows_with_outliers, nrow(data)))
  cat(sprintf("   NOTA: Le righe con outlier non sono state rimosse dal dataset\n"))
  
  # Restituisce il dataset originale senza modifiche
  return(data)
}

#' Identifica valori anomali nelle variabili qualitative confrontando i valori presenti nel dataset
#' con i valori validi definiti nel data dictionary. Un valore è considerato anomalo se non è
#' presente nella lista dei valori validi per quella variabile.
#'
#' La funzione utilizza il data dictionary per ottenere i valori validi per ogni variabile categorica
#' e confronta i valori effettivi nel dataset. Valori mancanti (NA) non sono considerati anomalie.
#'
#' \strong{Nota importante}: I valori anomali vengono solo identificati e segnalati, ma \strong{NON vengono
#' rimossi} dal dataset (la funzione restituisce il dataset originale invariato).
#'
#' @param data Data frame. Dataset contenente le variabili categoriche da analizzare.
#'   Le variabili categoriche vengono ottenute automaticamente dal data dictionary.
#'
#' @return Data frame. Il dataset originale senza modifiche. I valori anomali vengono solo
#'   identificati e segnalati, ma non rimossi. Il numero totale di anomalie trovate viene stampato
#'   a console insieme ai dettagli per ogni variabile (valori anomali e valori validi attesi).
anomalies_qualitative <- function(data) {
  dict <- get_data_dictionary()
  categorical_cols <- get_categorical_columns()

  # Crea un vettore per tracciare le righe con valori anomali
  rows_with_anomalies <- logical(nrow(data))

  total_anomalies <- 0

  for (col_name in categorical_cols) {
    if (!col_name %in% colnames(data)) {
      cat(sprintf("⚠️  Colonna '%s' non trovata nel dataset\n", col_name))
      next
    }

    valid_values <- dict$valid_values[[col_name]]

    # Estrai i valori dalla colonna
    col_data <- data[[col_name]]

    # Identifica valori anomali (valori non presenti in valid_values)
    # Gestisci sia valori numerici, character che factors
    if (is.factor(col_data)) {
      # Per factors, confronta i livelli
      col_data_numeric <- as.numeric(as.character(col_data))
      anomalous_mask <- !col_data_numeric %in% valid_values & !is.na(col_data)
    } else if (is.numeric(col_data)) {
      anomalous_mask <- !col_data %in% valid_values & !is.na(col_data)
    } else {
      anomalous_mask <- !as.character(col_data) %in% 
        as.character(valid_values) & !is.na(col_data)
    }

    # Marca le righe con valori anomali
    rows_with_anomalies[anomalous_mask] <- TRUE

    anomalous_values <- unique(col_data[anomalous_mask])
    n_anomalies <- sum(anomalous_mask)

    if (n_anomalies > 0) {
      total_anomalies <- total_anomalies + n_anomalies
      cat(sprintf("⚠️  ANOMALIE trovate in '%s': %d valori anomali in %d righe\n", 
                  col_name, n_anomalies, sum(anomalous_mask)))
      cat(sprintf("   Valori anomali: %s\n", 
                  paste(anomalous_values, collapse = ", ")))
      cat(sprintf("   Valori validi attesi: %s\n\n", 
                  paste(valid_values, collapse = ", ")))
    } else {
      cat(sprintf("✓ Nessuna anomalia trovata in '%s'\n", col_name))
    }
  }
  
  # Conta le righe con anomalie (solo per report, non vengono rimosse)
  n_rows_with_anomalies <- sum(rows_with_anomalies)
  
  cat(sprintf("\n✓ Analisi anomalie qualitative completata.\n"))
  cat(sprintf("   Totale valori anomali trovati: %d\n", total_anomalies))
  if (n_rows_with_anomalies > 0) {
    cat(sprintf("   Righe con anomalie: %d (su %d righe totali)\n", 
                n_rows_with_anomalies, nrow(data)))
  } else {
    cat(sprintf("   Nessuna riga con anomalie trovata\n"))
  }
  cat(sprintf("   NOTA: Le righe con anomalie non sono state rimosse dal dataset\n"))
  
  # Restituisce il dataset originale senza modifiche
  return(data)
}

#' Identifica e rimuove righe duplicate dal dataset. Due righe sono considerate duplicate
#' se hanno gli stessi valori in tutte le colonne. La funzione mantiene solo la prima occorrenza
#' di ogni gruppo di righe duplicate.
#'
#' La funzione utilizza \code{\link{duplicated}} per identificare le righe duplicate e mostra
#' informazioni dettagliate sui duplicati trovati, inclusi gli indici delle righe duplicate
#' e delle righe originali corrispondenti. Se ci sono molti duplicati, mostra solo i primi esempi.
#'
#' @param data Data frame. Dataset da cui rimuovere le righe duplicate.
#'
#' @return Data frame. Dataset senza righe duplicate. Se non ci sono duplicati, restituisce
#'   il dataset originale invariato. Il numero di righe rimosse viene stampato a console insieme
#'   agli indici delle righe duplicate e degli esempi.
find_duplicates <- function(data) {
  # Conta il numero totale di righe
  n_total <- nrow(data)
  
  # Identifica le righe duplicate
  # duplicated() restituisce TRUE per le righe che sono duplicati di righe precedenti
  duplicate_mask <- duplicated(data)
  n_duplicates <- sum(duplicate_mask)
  
  if (n_duplicates > 0) {
    # Ottieni gli indici delle righe duplicate
    duplicate_indices <- which(duplicate_mask)
    
    # Identifica i gruppi di righe duplicate
    # Per ogni riga duplicata, trova la riga originale corrispondente
    # Usa interaction() per creare un identificatore univoco per ogni combinazione di valori
    data_keys <- do.call(paste, c(data, sep = "|"))
    duplicate_keys <- data_keys[duplicate_mask]
    
    # Trova le righe originali corrispondenti
    original_indices <- match(duplicate_keys, data_keys)
    
    cat(sprintf("⚠️  RIGHE DUPLICATE trovate: %d righe duplicate\n", n_duplicates))
    cat(sprintf("   Righe duplicate (indici): %s\n", 
                paste(duplicate_indices, collapse = ", ")))
    cat(sprintf("   Righe originali corrispondenti (indici): %s\n", 
                paste(original_indices, collapse = ", ")))
    
    # Mostra un esempio delle righe duplicate
    if (n_duplicates <= 5) {
      cat(sprintf("\n   Esempi di righe duplicate:\n"))
      for (i in seq_along(duplicate_indices)) {
        cat(sprintf("   Riga %d (duplicato di riga %d):\n", 
                    duplicate_indices[i], original_indices[i]))
        print(data[duplicate_indices[i], , drop = FALSE])
      }
    } else {
      cat(sprintf("\n   Prime 3 righe duplicate (su %d totali):\n", n_duplicates))
      for (i in 1:min(3, n_duplicates)) {
        cat(sprintf("   Riga %d (duplicato di riga %d):\n", 
                    duplicate_indices[i], original_indices[i]))
        print(data[duplicate_indices[i], , drop = FALSE])
      }
    }
    
    # Rimuove le righe duplicate, mantenendo solo la prima occorrenza
    data_clean <- data[!duplicate_mask, ]
    
    cat(sprintf("\n✓ Rimozione righe duplicate completata.\n"))
    cat(sprintf("   Righe rimosse: %d (da %d a %d righe)\n", 
                n_duplicates, n_total, nrow(data_clean)))
  } else {
    cat(sprintf("✓ Nessuna riga duplicata trovata.\n"))
    data_clean <- data
  }
  
  # Restituisce il dataset senza duplicati
  return(data_clean)
}
