# frequences.R
# Funzioni per visualizzare le distribuzioni dei dati

# Palette per presentazioni universitarie (blu/grigi sobri)
COL_FILL    <- "#5B8FA3"   # Blu muted per barre/istogrammi
COL_BORDER  <- "#2C3E50"   # Grigio ardesia per bordi
COL_DENSITY <- "#1A5276"   # Blu scuro per curva di densità

#' Restituisce un'etichetta formattata per una variabile combinando il nome della variabile
#' con la sua descrizione dal data dictionary. Se la descrizione è disponibile, restituisce
#' "NomeVariabile (Descrizione)", altrimenti restituisce solo il nome della variabile.
#'
#' Questa funzione è utilizzata per migliorare la leggibilità dei grafici aggiungendo descrizioni
#' informative alle etichette degli assi e dei titoli.
#'
#' @param var_name Stringa carattere. Nome della variabile di cui si vuole l'etichetta formattata.
#'
#' @return Stringa carattere. Etichetta formattata con formato "NomeVariabile (Descrizione)"
#'   se la descrizione è disponibile nel data dictionary, altrimenti solo "NomeVariabile".
label_with_description <- function(var_name) {
  desc <- get_column_description(var_name)
  if (!is.null(desc) && nchar(desc) > 0) {
    paste0(var_name, " (", desc, ")")
  } else {
    var_name
  }
}

#' Genera grafici delle distribuzioni per tutte le variabili specificate nel dataset.
#' La funzione rileva automaticamente se una variabile è quantitativa o qualitativa e genera
#' il tipo di grafico appropriato:
#' \itemize{
#'   \item \strong{Variabili quantitative}: Genera un grafico composito con 4 pannelli:
#'     \enumerate{
#'       \item Istogramma con frequenze assolute
#'       \item Istogramma con curva di densità sovrapposta
#'       \item Boxplot orizzontale
#'       \item Statistiche descrittive (media, mediana, SD, min, max, n)
#'     }
#'   \item \strong{Variabili qualitative}: Genera un barplot con frequenze per ogni categoria,
#'     utilizzando le etichette dal data dictionary quando disponibili. I valori vengono mostrati
#'     sopra ogni barra con sfondo semi-trasparente per migliorare la leggibilità.
#' }
#'
#' I grafici utilizzano una palette di colori coerente con il progetto (blu/grigi sobri) e
#' vengono salvati come file PNG nella directory di output specificata.
#'
#' @param data Data frame. Dataset contenente le variabili da visualizzare.
#' @param variables Vettore carattere o NULL. Nomi delle variabili da visualizzare.
#'   Se NULL, vengono visualizzate tutte le colonne del dataset.
#'   Predefinito: NULL.
#' @param output_dir Stringa carattere. Directory dove salvare i grafici generati.
#'   Predefinito: "output/distribuzioni". La directory viene creata automaticamente se non esiste.
#'
#' @return Invisibile. I grafici vengono salvati come file PNG nella directory di output.
#'   Il nome del file è "{nome_variabile}_distribution.png".
plot_distributions <- function(data,
                               variables = NULL,
                               output_dir = "output/distribuzioni") {
  
  # Crea la directory di output se non esiste
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Se non specificate, usa tutte le variabili del dataset
  if (is.null(variables)) {
    variables <- colnames(data)
  }
  
  # Filtra le variabili presenti nel dataset
  available_vars <- variables[variables %in% colnames(data)]
  
  if (length(available_vars) == 0) {
    stop("Nessuna variabile trovata nel dataset")
  }
  
  dict <- get_data_dictionary()
  categorical_cols <- get_categorical_columns()
  
  cat(sprintf("Generazione grafici di distribuzione per %d variabili...\n", length(available_vars)))
  
  for (var_name in available_vars) {
    var_data <- data[[var_name]]
    
    # Determina se la variabile è quantitativa o qualitativa
    is_categorical <- var_name %in% categorical_cols || is.factor(var_data) || 
                     (is.numeric(var_data) && length(unique(var_data)) <= 10)
    
    if (is_categorical) {
      # ========================================================================
      # DISTRIBUZIONE PER VARIABILI QUALITATIVE
      # ========================================================================
      
      # Calcola le frequenze
      freq_table <- table(var_data, useNA = "ifany")
      freq_names <- names(freq_table)
      
      # Prepara le etichette usando il data dictionary
      plot_labels <- character(length(freq_names))
      labels_dict <- dict$labels[[var_name]]
      
      for (i in seq_along(freq_names)) {
        if (is.na(freq_names[i])) {
          plot_labels[i] <- "NA"
        } else if (!is.null(labels_dict) && freq_names[i] %in% names(labels_dict)) {
          plot_labels[i] <- labels_dict[[freq_names[i]]]
        } else {
          plot_labels[i] <- as.character(freq_names[i])
        }
      }
      
      # Salva il barplot
      png_file <- file.path(output_dir, paste0(var_name, "_distribution.png"))
      
      # Calcola l'altezza necessaria in base alla lunghezza delle etichette
      max_label_length <- max(nchar(plot_labels))
      # Aumenta l'altezza se le etichette sono lunghe
      base_height <- 600
      extra_height <- ifelse(max_label_length > 15, 200, 0)
      png(png_file, width = 1200, height = base_height + extra_height, res = 150)
      
      # Aumenta i margini inferiori per le etichette lunghe
      bottom_margin <- ifelse(max_label_length > 15, 12, 8)
      # Aumenta il margine superiore per separare il titolo dal grafico
      par(mar = c(bottom_margin, 5, 6, 2))
      
      # Calcola lo spazio necessario sopra le barre per i numeri
      max_freq <- max(freq_table)
      # Aumenta il limite superiore dell'asse Y per dare spazio ai numeri
      y_upper_limit <- max_freq * 1.15
      
      # Calcola le posizioni delle barre per aggiungere i valori
      var_label <- label_with_description(var_name)
      bp <- barplot(freq_table,
                    names.arg = plot_labels,
                    col = COL_FILL,
                    border = COL_BORDER,
                    main = sprintf("Distribuzione di %s", var_label),
                    xlab = "",
                    ylab = "Frequenza",
                    las = 2,
                    cex.names = ifelse(max_label_length > 15, 0.7, 0.8),
                    cex.main = 1.2,
                    ylim = c(0, y_upper_limit))  # Aumenta lo spazio sopra le barre
      
      # Aggiunge i valori sopra le barre con sfondo per migliorare la leggibilità
      text_y_positions <- freq_table + max_freq * 0.08  # Posiziona il testo più in alto
      
      for (i in seq_along(bp)) {
        # Calcola le dimensioni del testo
        text_label <- as.character(freq_table[i])
        text_width <- strwidth(text_label, cex = 1.0)
        text_height <- strheight(text_label, cex = 1.0)
        
        # Disegna uno sfondo bianco semi-trasparente per il testo (senza bordo)
        rect(xleft = bp[i] - text_width/2 - 0.02,
             ybottom = text_y_positions[i] - text_height/2 - 0.01,
             xright = bp[i] + text_width/2 + 0.02,
             ytop = text_y_positions[i] + text_height/2 + 0.01,
             col = rgb(1, 1, 1, alpha = 0.8),
             border = NA)  # Nessun bordo
        
        # Aggiunge il testo in nero e più grande
        text(x = bp[i],
             y = text_y_positions[i],
             labels = text_label,
             cex = 1.0,
             col = "black",
             font = 2)  # font = 2 per grassetto
      }
      
      dev.off()
      cat(sprintf("✓ Grafico salvato: %s\n", png_file))
      
    } else if (is.numeric(var_data)) {
      # ========================================================================
      # DISTRIBUZIONE PER VARIABILI QUANTITATIVE
      # ========================================================================
      
      # Rimuove eventuali valori mancanti
      var_data_clean <- var_data[!is.na(var_data)]
      
      if (length(var_data_clean) == 0) {
        cat(sprintf("⚠️  '%s' contiene solo valori mancanti, saltata\n", var_name))
        next
      }
      
      # Salva un grafico combinato con istogramma, densità e boxplot
      png_file <- file.path(output_dir, paste0(var_name, "_distribution.png"))
      png(png_file, width = 1400, height = 1000, res = 150)
      
      # Layout: 2 righe, 2 colonne (istogramma, istogramma+densità, boxplot, statistiche)
      layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE))
      
      var_label <- label_with_description(var_name)
      # 1. Istogramma con frequenze
      # Aumenta il margine superiore per separare il titolo dal grafico
      par(mar = c(5, 5, 6, 2))
      hist(var_data_clean,
           breaks = "Sturges",
           col = COL_FILL,
           border = COL_BORDER,
           main = sprintf("Istogramma di %s", var_label),
           xlab = var_label,
           ylab = "Frequenza",
           probability = FALSE,
           cex.main = 1.1,
           cex.lab = 1.0)
      
      # 2. Istogramma con curva di densità
      par(mar = c(5, 5, 6, 2))
      hist(var_data_clean,
           breaks = "Sturges",
           col = COL_FILL,
           border = COL_BORDER,
           main = sprintf("Istogramma con Densità di %s", var_label),
           xlab = var_label,
           ylab = "Densità",
           probability = TRUE,
           cex.main = 1.1,
           cex.lab = 1.0)
      lines(density(var_data_clean), col = COL_DENSITY, lwd = 2)
      
      # 3. Boxplot
      par(mar = c(5, 5, 6, 2))
      boxplot(var_data_clean,
              col = COL_FILL,
              border = COL_BORDER,
              main = sprintf("Boxplot di %s", var_label),
              ylab = var_label,
              horizontal = TRUE,
              cex.main = 1.1,
              cex.lab = 1.0)
      
      # 4. Statistiche descrittive
      par(mar = c(4, 4, 4, 4))
      plot.new()
      stats_text <- sprintf(
        "Statistiche Descrittive\n\nMedia: %.2f\nMediana: %.2f\nSD: %.2f\nMin: %.2f\nMax: %.2f\nn: %d",
        mean(var_data_clean),
        median(var_data_clean),
        sd(var_data_clean),
        min(var_data_clean),
        max(var_data_clean),
        length(var_data_clean)
      )
      
      text(0.5, 0.5, stats_text,
           cex = 1.1,
           family = "mono",
           adj = c(0.5, 0.5))
      
      dev.off()
      cat(sprintf("✓ Grafico salvato: %s\n", png_file))
      
    } else {
      cat(sprintf("⚠️  Tipo di variabile non supportato per '%s', saltata\n", var_name))
    }
  }
  
  cat(sprintf("\n✓ Generazione distribuzioni completata per %d variabili\n", length(available_vars)))
}
