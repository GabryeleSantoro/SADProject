# analysis.R
# Funzioni per analisi statistiche bivariate e HCA

#' Esegue un'analisi bivariata completa per tutte le coppie di variabili quantitative specificate.
#' Per ogni coppia di variabili, genera:
#' \itemize{
#'   \item Scatter plot con linea di regressione lineare
#'   \item Statistiche descrittive per entrambe le variabili (media, mediana, SD)
#'   \item Covarianza campionaria
#'   \item Coefficiente di correlazione di Pearson (r)
#'   \item Heatmap delle correlazioni tra tutte le variabili (matrice di correlazione)
#' }
#'
#' I grafici vengono salvati come file PNG nella directory di output. Ogni coppia di variabili
#' genera un file separato, mentre la heatmap delle correlazioni viene salvata una volta per tutte
#' le variabili analizzate.
#'
#' @param data Data frame. Dataset contenente le variabili numeriche da analizzare.
#' @param variables Vettore carattere. Nomi delle variabili quantitative da analizzare.
#'   Deve contenere almeno 2 variabili. Predefinito: c("Weight", "Height", "AGE").
#' @param output_dir Stringa carattere. Directory dove salvare i grafici generati.
#'   Predefinito: "output/correlazioni". La directory viene creata automaticamente se non esiste.
#'
#' @return Invisibile. I grafici vengono salvati come file PNG nella directory di output.
#'   Ogni coppia genera un file "{var1}_vs_{var2}_scatter.png" e viene generata una heatmap
#'   "correlation_heatmap.png" con tutte le correlazioni.
bivariate_analysis <- function(data,
                                variables = c("Weight", "Height", "AGE"),
                                output_dir = "output/correlazioni") {
  
  # ============================================================================
  # FASE 1: PREPARAZIONE DELL'AMBIENTE
  # ============================================================================
  
  # Crea la directory di output se non esiste già
  # recursive = TRUE permette di creare anche le directory padre se mancanti
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # ============================================================================
  # FASE 2: VALIDAZIONE DELLE VARIABILI
  # ============================================================================
  
  # Filtra le variabili specificate mantenendo solo quelle presenti nel dataset
  # Questo evita errori se alcune variabili non esistono
  available_vars <- variables[variables %in% colnames(data)]
  
  # Verifica che ci siano almeno 2 variabili disponibili
  # Serve almeno una coppia per generare un'analisi bivariata
  if (length(available_vars) < 2) {
    stop("Almeno 2 variabili devono essere presenti nel dataset")
  }
  
  # ============================================================================
  # FASE 3: GENERAZIONE DELLE COPPIE DI VARIABILI
  # ============================================================================
  
  # Conta il numero di variabili disponibili
  n_vars <- length(available_vars)
  
  # Loop annidato per generare tutte le possibili coppie univoche di variabili
  # Il loop esterno va da 1 a (n_vars - 1) per evitare di ripetere coppie
  # Il loop interno parte da (i + 1) per evitare coppie duplicate (es. A-B e B-A)
  for (i in 1:(n_vars - 1)) {
    for (j in (i + 1):n_vars) {
      
      # Assegna le variabili della coppia corrente
      # var_x sarà la variabile sull'asse X (orizzontale)
      # var_y sarà la variabile sull'asse Y (verticale)
      var_x <- available_vars[i]
      var_y <- available_vars[j]
      
      # ========================================================================
      # FASE 4: ESTRAZIONE DEI DATI
      # ========================================================================
      
      # Estrae i vettori numerici per le due variabili
      # I dati sono già stati puliti precedentemente nel workflow
      x_data <- data[[var_x]]
      y_data <- data[[var_y]]
      
      # ========================================================================
      # FASE 5: CALCOLO DELLE STATISTICHE DESCRITTIVE
      # ========================================================================
      
      # Calcola le statistiche descrittive per la variabile X (asse orizzontale)
      # I dati sono già puliti, quindi non servono precauzioni per valori mancanti
      x_mean <- mean(x_data)      # Media campionaria
      x_median <- median(x_data)  # Mediana campionaria
      x_sd <- sd(x_data)          # Deviazione standard campionaria
      
      # Calcola le statistiche descrittive per la variabile Y (asse verticale)
      y_mean <- mean(y_data)      # Media campionaria
      y_median <- median(y_data)  # Mediana campionaria
      y_sd <- sd(y_data)          # Deviazione standard campionaria
      
      # ========================================================================
      # FASE 5.1: CALCOLO DELLE STATISTICHE BIVARIATE
      # ========================================================================
      
      # Calcola la covarianza campionaria tra le due variabili
      # cov() calcola la covarianza campionaria (divisore n-1)
      # La covarianza misura la relazione lineare tra due variabili
      covariance <- cov(x_data, y_data)
      
      # Calcola il coefficiente di correlazione campionario (r di Pearson)
      # cor() calcola il coefficiente di correlazione lineare
      # Alternativamente: r = cov(x, y) / (sd(x) * sd(y))
      # Il coefficiente di correlazione varia tra -1 e +1
      correlation <- cor(x_data, y_data)
      
      # ========================================================================
      # FASE 6: PREPARAZIONE DEL FILE DI OUTPUT
      # ========================================================================
      
      # Genera il nome del file PNG usando il formato "VariabileX_vs_VariabileY.png"
      # file.path() costruisce il percorso in modo cross-platform
      png_file <- file.path(output_dir, sprintf("%s_vs_%s.png", var_x, var_y))
      
      # Apre il dispositivo grafico PNG per salvare il grafico
      # width e height sono in pixel, res è la risoluzione in DPI
      png(png_file, width = 1000, height = 800, res = 150)
      
      # ========================================================================
      # FASE 7: CREAZIONE DELLO SCATTERPLOT
      # ========================================================================

      # Imposta i margini del grafico (bottom, left, top, right)
      # Margini più ampi per evitare che le etichette vengano tagliate
      par(mar = c(5, 5, 4, 2))

      # Crea lo scatterplot base con i punti
      # pch = 19: simbolo pieno circolare
      # cex = 0.6: dimensione dei punti ridotta per evitare sovrapposizione
      # col: colore blu semi-trasparente (alpha = 0.5) per vedere sovrapposizioni
      plot(x_data, y_data,
           xlab = var_x,  # Etichetta asse X
           ylab = var_y,  # Etichetta asse Y
           main = sprintf("Analisi Bivariata: %s vs %s", var_x, var_y),  # Titolo
           pch = 19,
           cex = 0.6,
           col = rgb(0, 0, 1, alpha = 0.5))

      # ========================================================================
      # FASE 8: AGGIUNTA DELLE LINEE STATISTICHE VERTICALI (variabile X)
      # ========================================================================

      # Linea verticale continua rossa per la media della variabile X
      # v = valore verticale, lwd = spessore linea, lty = tipo linea (1 = continua)
      abline(v = x_mean, col = "red", lwd = 2, lty = 1)

      # Linea verticale tratteggiata blu per la mediana della variabile X
      # lty = 2 = linea tratteggiata
      abline(v = x_median, col = "blue", lwd = 2, lty = 2)

      # ========================================================================
      # FASE 9: AGGIUNTA DELLE LINEE STATISTICHE ORIZZONTALI (variabile Y)
      # ========================================================================

      # Linea orizzontale continua rossa per la media della variabile Y
      # h = valore orizzontale
      abline(h = y_mean, col = "red", lwd = 2, lty = 1)
      
      # Linea orizzontale tratteggiata blu per la mediana della variabile Y
      abline(h = y_median, col = "blue", lwd = 2, lty = 2)
      
      # ========================================================================
      # FASE 10: AGGIUNTA DELLE BANDE DI DEVIAZIONE STANDARD (variabile X)
      # ========================================================================
      
      # Linee verticali tratteggiate arancioni per mostrare ±1 deviazione standard
      # dalla media della variabile X
      # lty = 3 = linea punteggiata
      abline(v = x_mean + x_sd, col = "orange", lwd = 1.5, lty = 3)  # Media + SD
      abline(v = x_mean - x_sd, col = "orange", lwd = 1.5, lty = 3)  # Media - SD
      
      # ========================================================================
      # FASE 11: AGGIUNTA DELLE BANDE DI DEVIAZIONE STANDARD (variabile Y)
      # ========================================================================
      
      # Linee orizzontali tratteggiate arancioni per mostrare ±1 deviazione standard
      # dalla media della variabile Y
      abline(h = y_mean + y_sd, col = "orange", lwd = 1.5, lty = 3)  # Media + SD
      abline(h = y_mean - y_sd, col = "orange", lwd = 1.5, lty = 3)  # Media - SD
      
      # ========================================================================
      # FASE 12: CREAZIONE DELLA LEGENDA
      # ========================================================================
      
      # Aggiunge una legenda in alto a destra del grafico
      # La legenda mostra tutte le statistiche calcolate con i relativi colori e stili
      legend("topright",
             # Testo della legenda: mostra media, mediana, SD per entrambe le variabili
             # e le statistiche bivariate (covarianza e correlazione)
             legend = c(
               sprintf("Media %s: %.2f", var_x, x_mean),
               sprintf("Mediana %s: %.2f", var_x, x_median),
               sprintf("SD %s: ±%.2f", var_x, x_sd),
               sprintf("Media %s: %.2f", var_y, y_mean),
               sprintf("Mediana %s: %.2f", var_y, y_median),
               sprintf("SD %s: ±%.2f", var_y, y_sd),
               sprintf("Covarianza: %.2f", covariance),
               sprintf("Correlazione: %.3f", correlation)
             ),
             # Colori corrispondenti: rosso per media, blu per mediana, arancione per SD
             # Verde per covarianza e correlazione (non hanno linee sul grafico)
             col = c("red", "blue", "orange", "red", "blue", "orange", "darkgreen", "darkgreen"),
             # Stili di linea: continua (1), tratteggiata (2), punteggiata (3), nessuna (0)
             lty = c(1, 2, 3, 1, 2, 3, 0, 0),
             # Spessori di linea: 2 per media/mediana, 1.5 per SD, 0 per statistiche bivariate
             lwd = c(2, 2, 1.5, 2, 2, 1.5, 0, 0),
             cex = 0.8,      # Dimensione del testo della legenda
             bg = "white")   # Sfondo bianco per la legenda

      # ========================================================================
      # FASE 13: CHIUSURA DEL DISPOSITIVO GRAFICO
      # ========================================================================

      # Chiude il dispositivo PNG e salva il file
      dev.off()

      # Messaggio di conferma per l'utente
      cat(sprintf("✓ Grafico bivariato salvato: %s\n", png_file))
    }
  }
  
  # ============================================================================
  # FASE 14: HEATMAP DELLA MATRICE DI CORRELAZIONE
  # ============================================================================
  
  cor_matrix <- NULL
  heatmap_file <- NULL
  
  # Estrae i dati numerici per le variabili analizzate
  data_numeric <- data[, available_vars, drop = FALSE]
  data_complete <- data_numeric[complete.cases(data_numeric), , drop = FALSE]
  
  if (nrow(data_complete) >= 2) {
    # Calcola la matrice di correlazione di Pearson
    cor_matrix <- cor(data_complete, use = "complete.obs", method = "pearson")
    
    # Salva la heatmap
    heatmap_file <- file.path(output_dir, "correlation_heatmap.png")
    png(heatmap_file, width = 1000, height = 900, res = 150)
    
    par(mar = c(8, 8, 4, 4))
    # Palette RdBu (blu-bianco-rosso): classica per correlazioni, leggibile e professionale
    pal <- colorRampPalette(c("#2166AC", "#67A9CF", "#F7F7F7", "#EF8A62", "#B2182B"))(100)
    image(seq_len(ncol(cor_matrix)), seq_len(nrow(cor_matrix)),
          t(cor_matrix[nrow(cor_matrix):1, , drop = FALSE]),
          col = pal,
          main = "Heatmap delle Correlazioni (Pearson)\nVariabili Quantitative Analizzate",
          xlab = "", ylab = "",
          axes = FALSE,
          zlim = c(-1, 1))
    
    axis(1, at = seq_len(ncol(cor_matrix)),
         labels = colnames(cor_matrix),
         las = 2, cex.axis = 0.9)
    axis(2, at = seq_len(nrow(cor_matrix)),
         labels = rev(rownames(cor_matrix)),
         las = 2, cex.axis = 0.9)
    
    # Aggiunge i valori di correlazione sulla heatmap
    for (i in seq_len(nrow(cor_matrix))) {
      for (j in seq_len(ncol(cor_matrix))) {
        row_plot <- nrow(cor_matrix) - i + 1
        val <- cor_matrix[i, j]
        text_col <- if (abs(val) > 0.6) "white" else "black"
        text(j, row_plot, sprintf("%.2f", val),
             cex = 0.8,
             col = text_col)
      }
    }
    
    dev.off()
    cat(sprintf("✓ Heatmap correlazioni salvata: %s\n", heatmap_file))
  } else {
    cat("⚠️  Heatmap non generata: dati insufficienti (richieste almeno 2 osservazioni complete)\n")
  }
  
  # ============================================================================
  # FASE 15: MESSAGGIO FINALE E RETURN
  # ============================================================================
  
  # Messaggio riepilogativo con il numero totale di coppie analizzate
  # choose(n, k) calcola il coefficiente binomiale: numero di combinazioni
  # choose(n_vars, 2) = n_vars! / (2! * (n_vars-2)!) = numero di coppie univoche
  cat(sprintf("\n✓ Analisi bivariata completata per %d coppie di variabili\n", 
              choose(n_vars, 2)))
  
  invisible(list(
    variables = available_vars,
    correlation_matrix = cor_matrix,
    heatmap_path = heatmap_file
  ))
}


# La funzione pareto_features crea un diagramma di Pareto per le feature del dataset.
# Per ogni feature calcola una metrica di "informatività" (varianza per numeriche,
# entropia di Shannon per categoriche), ordina in modo decrescente e mostra barre + curva cumulativa.
#
# Parametri:
#' Esegue un'analisi di Pareto delle feature qualitative utilizzando l'entropia di Shannon
#' come misura di informatività. Le variabili vengono ordinate per entropia decrescente
#' e visualizzate in un diagramma di Pareto che mostra sia le barre con l'entropia di ogni
#' feature sia la curva cumulativa percentuale.
#'
#' L'entropia di Shannon misura l'informatività di una variabile categorica: valori più alti
#' indicano maggiore variabilità e quindi maggiore informazione. Il diagramma di Pareto aiuta
#' a identificare le feature più informative che contribuiscono alla maggior parte dell'informazione.
#'
#' @param data Data frame. Dataset contenente le variabili categoriche da analizzare.
#' @param variables Vettore carattere o NULL. Nomi delle variabili categoriche da analizzare.
#'   Se NULL, usa tutte le variabili categoriche dal data dictionary (escluso 'class').
#'   Predefinito: NULL.
#' @param output_dir Stringa carattere. Directory dove salvare il grafico generato.
#'   Predefinito: "output". La directory viene creata automaticamente se non esiste.
#' @param output_file Stringa carattere. Nome del file PNG da generare.
#'   Predefinito: "pareto_features.png".
#'
#' @return Invisibile. Il grafico viene salvato come file PNG nella directory di output.
#'   Il grafico mostra barre con entropia di Shannon per ogni feature e una curva cumulativa
#'   che indica la percentuale cumulativa di informazione.
pareto_features <- function(data,
                             variables = NULL,
                             output_dir = "output",
                             output_file = "pareto_features.png") {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  if (is.null(variables)) {
    categorical_cols <- get_categorical_columns()
    variables <- categorical_cols[categorical_cols %in% colnames(data)]
  }
  available_vars <- variables[variables %in% colnames(data)]

  if (length(available_vars) == 0) {
    stop("Nessuna variabile qualitativa trovata nel dataset")
  }

  # Metrica di informatività: entropia di Shannon (solo variabili qualitative)
  scores <- numeric(length(available_vars))
  names(scores) <- available_vars

  for (i in seq_along(available_vars)) {
    x <- data[[available_vars[i]]]
    x_clean <- x[!is.na(x)]
    # Solo qualitative: entropia di Shannon
    p <- prop.table(table(x_clean))
    p <- p[p > 0]
    scores[i] <- -sum(p * log2(p))
  }

  # Ordina in modo decrescente
  ord <- order(scores, decreasing = TRUE)
  scores_ord <- scores[ord]
  cum_pct <- 100 * cumsum(scores_ord) / sum(scores_ord)

  # Salva il grafico
  out_path <- file.path(output_dir, output_file)
  n_vars <- length(available_vars)
  png(out_path, width = max(1200, n_vars * 60), height = 700, res = 150)

  par(mar = c(8, 5, 5, 5))
  bp <- barplot(scores_ord,
                col = "#5B8FA3",
                border = "#2C3E50",
                main = "Diagramma di Pareto delle Feature Qualitative\n(Entropia di Shannon - ordinamento decrescente)",
                xlab = "",
                ylab = "Entropia (bit)",
                las = 2,
                cex.names = 0.85,
                names.arg = names(scores_ord))

  par(new = TRUE)
  plot(bp[, 1], cum_pct,
       type = "o",
       pch = 19,
       col = "#1A5276",
       lwd = 2,
       axes = FALSE,
       xlab = "",
       ylab = "",
       xlim = range(bp),
       ylim = c(0, 105))

  axis(4, at = seq(0, 100, 20), las = 1, col = "#1A5276", col.axis = "#1A5276")
  mtext("Cumulativo %", side = 4, line = 3, col = "#1A5276", cex = 1)
  abline(h = 80, col = "gray60", lty = 2, lwd = 1)
  legend("right", legend = "Cumulativo %", col = "#1A5276", lty = 1, lwd = 2, pch = 19, bty = "n")

  dev.off()
  cat(sprintf("✓ Diagramma di Pareto salvato: %s\n", out_path))

  invisible(list(
    scores = scores_ord,
    cumulative_pct = cum_pct,
    path = out_path
  ))
}


# La funzione kmeans_feature_importance analizza l'importanza delle feature qualitative/ordinali
# usando k-means. Per ogni variabile categoriale si calcola quanto contribuisce a separare i cluster
# (rapporto varianza-between / varianza-within, stile F-statistic ANOVA).
#
# Parametri:
#' Analizza l'importanza delle feature qualitative utilizzando K-means clustering su variabili
#' codificate con one-hot encoding. L'importanza di ogni feature è misurata come il rapporto
#' tra varianza between-cluster e varianza within-cluster: feature con rapporto più alto
#' sono più importanti per separare i cluster.
#'
#' La funzione:
#' \enumerate{
#'   \item Converte le variabili categoriche in dummy variables (one-hot encoding)
#'   \item Se k non è specificato, determina il numero ottimale di cluster usando il metodo del gomito
#'   \item Esegue K-means clustering
#'   \item Calcola l'importanza di ogni feature come rapporto varianza between/within
#'   \item Genera grafici: elbow plot (se k=NULL), barplot delle importanze, e scatter plot dei cluster
#' }
#'
#' @param data Data frame. Dataset contenente le variabili categoriche da analizzare.
#' @param variables Vettore carattere o NULL. Nomi delle variabili categoriche da analizzare.
#'   Se NULL, usa tutte le variabili categoriche dal data dictionary (escluso 'class').
#'   Predefinito: NULL.
#' @param k Numerico o NULL. Numero di cluster da utilizzare per K-means.
#'   Se NULL, viene scelto automaticamente usando il metodo del gomito (elbow method)
#'   testando valori da 2 a min(10, n_variabili). Predefinito: NULL.
#' @param output_dir Stringa carattere. Directory dove salvare i grafici generati.
#'   Predefinito: "output/kmeans". La directory viene creata automaticamente se non esiste.
#'
#' @return Lista invisibile con elementi:
#'   \item{kmeans_result}{Oggetto risultato di kmeans()}
#'   \item{feature_importance}{Data frame con importanza di ogni feature}
#'   \item{k}{Numero di cluster utilizzato}
#'   \item{output_dir}{Percorso della directory di output}
kmeans_feature_importance <- function(data,
                                      variables = NULL,
                                      k = NULL,
                                      output_dir = "output/kmeans") {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  if (is.null(variables)) {
    categorical_cols <- get_categorical_columns()
    variables <- categorical_cols[categorical_cols %in% colnames(data)]
  }
  available_vars <- variables[variables %in% colnames(data)]

  if (length(available_vars) < 2) {
    stop("Almeno 2 variabili qualitative devono essere presenti nel dataset")
  }

  data_subset <- data[, available_vars, drop = FALSE]
  data_subset <- data_subset[complete.cases(data_subset), , drop = FALSE]

  if (nrow(data_subset) < 10) {
    stop("Dati insufficienti: servono almeno 10 osservazioni complete")
  }

  # Codifica: one-hot per tutte le variabili categoriche
  encoded_list <- list()
  var_to_cols <- list()

  for (var_name in available_vars) {
    x <- data_subset[[var_name]]
    x <- as.factor(x)
    dm <- model.matrix(~ x - 1)
    colnames(dm) <- paste0(var_name, "_", levels(x))
    encoded_list[[var_name]] <- dm
    var_to_cols[[var_name]] <- colnames(dm)
  }

  X_encoded <- do.call(cbind, encoded_list)
  X_scaled <- scale(X_encoded, center = TRUE, scale = TRUE)

  # Gestione colonne con varianza nulla dopo scaling
  col_sd <- apply(X_scaled, 2, sd, na.rm = TRUE)
  valid_cols <- !is.na(col_sd) & col_sd > 1e-10
  if (sum(!valid_cols) > 0) {
    X_scaled <- X_scaled[, valid_cols, drop = FALSE]
  }

  n <- nrow(X_scaled)
  p <- ncol(X_scaled)
  col_names <- colnames(X_scaled)

  # Scegli k con metodo del gomito se non specificato
  if (is.null(k)) {
    k_max <- min(10, n %/% 3, length(available_vars))
    if (k_max < 2) k_max <- 2
    wss <- numeric(k_max)
    for (i in 1:k_max) {
      km <- kmeans(X_scaled, centers = i, nstart = 25)
      wss[i] <- km$tot.withinss
    }
    # Curvatura: differenza delle differenze
    d <- diff(wss)
    d2 <- diff(d)
    k <- which.max(abs(d2)) + 1
    if (k < 2) k <- 2
    k <- min(k, k_max)
    cat(sprintf("Numero di cluster scelto (metodo gomito): k = %d\n", k))
  }

  # K-means
  set.seed(42)
  km <- kmeans(X_scaled, centers = k, nstart = 50)
  clusters <- km$cluster
  centers <- km$centers

  # Importanza per colonna: F = between_SS / within_SS (per dimensione)
  # Between_SS_j = sum_g n_g * (x̄_gj - x̄_j)^2
  # Within_SS_j = sum_g sum_i_in_g (x_ij - x̄_gj)^2
  importance_col <- numeric(p)
  names(importance_col) <- col_names

  grand_mean <- colMeans(X_scaled, na.rm = TRUE)
  n_g <- table(clusters)

  for (j in 1:p) {
    x_j <- X_scaled[, j]
    between_ss <- 0
    within_ss <- 0
    for (g in seq_len(k)) {
      idx <- clusters == g
      n_gg <- sum(idx)
      if (n_gg > 0) {
        mean_g <- mean(x_j[idx], na.rm = TRUE)
        between_ss <- between_ss + n_gg * (mean_g - grand_mean[j])^2
        within_ss <- within_ss + sum((x_j[idx] - mean_g)^2, na.rm = TRUE)
      }
    }
    importance_col[j] <- if (within_ss > 1e-10) between_ss / within_ss else 0
  }

  # Aggrega importanza per variabile originale (media delle colonne one-hot)
  importance_var <- numeric(length(available_vars))
  names(importance_var) <- available_vars

  for (var_name in available_vars) {
    cols <- var_to_cols[[var_name]]
    cols <- cols[cols %in% names(importance_col)]
    if (length(cols) > 0) {
      importance_var[var_name] <- mean(importance_col[cols], na.rm = TRUE)
    }
  }

  importance_ord <- sort(importance_var, decreasing = TRUE)

  # Salva grafico
  png_file <- file.path(output_dir, "kmeans_feature_importance.png")
  png(png_file, width = 1200, height = 700, res = 150)

  par(mar = c(8, 6, 5, 4))
  barplot(importance_ord,
          col = "#5B8FA3",
          border = "#2C3E50",
          main = "Importanza delle feature qualitative (K-means)\nRapporto varianza between/within per cluster",
          xlab = "",
          ylab = "Importanza (between/within SS)",
          las = 2,
          cex.names = 0.9)

  dev.off()
  cat(sprintf("✓ Grafico importanza feature salvato: %s\n", png_file))

  # Stampa classifica
  cat("\n--- Classifica feature per importanza (k-means) ---\n")
  for (i in seq_along(importance_ord)) {
    cat(sprintf("%2d. %s: %.3f\n", i, names(importance_ord)[i], importance_ord[i]))
  }

  invisible(list(
    importance = importance_var,
    importance_ranked = importance_ord,
    kmeans_result = km,
    k = k,
    path = png_file
  ))
}


# La funzione pca_qualitative effettua MCA (Multiple Correspondence Analysis),
# l'analogo della PCA per variabili qualitative. Analizza la struttura delle
# associazioni tra categorie e riduce la dimensionalità.
#
# Parametri:
#   - data: dataframe
#   - variables: variabili qualitative (default: tutte dal data dictionary, escluso class)
#' Esegue un'Analisi delle Corrispondenze Multiple (MCA) su variabili qualitative.
#' La MCA è l'equivalente dell'Analisi in Componenti Principali (PCA) per variabili categoriche
#' e permette di ridurre la dimensionalità e visualizzare le associazioni tra categorie.
#'
#' La funzione utilizza il pacchetto FactoMineR per eseguire la MCA e genera:
#' \itemize{
#'   \item Scree plot mostrando la varianza spiegata per ogni dimensione
#'   \item Plot delle variabili sui primi 2 assi principali (mostra associazioni tra categorie)
#'   \item Plot degli individui sui primi 2 assi principali (mostra similarità tra osservazioni)
#'   \item Barplot dei contributi delle variabili alla prima dimensione (identifica variabili più influenti)
#' }
#'
#' @param data Data frame. Dataset contenente le variabili categoriche da analizzare.
#' @param variables Vettore carattere o NULL. Nomi delle variabili categoriche da analizzare.
#'   Se NULL, usa tutte le variabili categoriche dal data dictionary (escluso 'class').
#'   Deve contenere almeno 2 variabili. Predefinito: NULL.
#' @param ncp Numerico. Numero di dimensioni/componenti principali da estrarre.
#'   Predefinito: 5. Un numero maggiore permette di catturare più varianza ma aumenta il tempo di calcolo.
#' @param output_dir Stringa carattere. Directory dove salvare i grafici generati.
#'   Predefinito: "output/pca_qualitative". La directory viene creata automaticamente se non esiste.
#'
#' @return Lista invisibile con elementi:
#'   \item{mca_result}{Oggetto risultato di FactoMineR::MCA()}
#'   \item{eigenvalues}{Data frame con autovalori e varianza spiegata per dimensione}
#'   \item{output_dir}{Percorso della directory di output}
pca_qualitative <- function(data,
                            variables = NULL,
                            ncp = 5,
                            output_dir = "output/pca_qualitative") {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  if (is.null(variables)) {
    categorical_cols <- get_categorical_columns()
    variables <- categorical_cols[categorical_cols %in% colnames(data)]
  }
  available_vars <- variables[variables %in% colnames(data)]

  if (length(available_vars) < 2) {
    stop("Almeno 2 variabili qualitative devono essere presenti nel dataset")
  }

  data_subset <- data[, available_vars, drop = FALSE]
  data_subset <- data_subset[complete.cases(data_subset), , drop = FALSE]

  if (nrow(data_subset) < 10) {
    stop("Dati insufficienti: servono almeno 10 osservazioni complete")
  }

  # Converte in factor per MCA
  for (v in available_vars) {
    data_subset[[v]] <- as.factor(data_subset[[v]])
  }

  # MCA
  mca_res <- FactoMineR::MCA(data_subset, ncp = ncp, graph = FALSE, method = "Indicator")

  # Scree plot: varianza spiegata
  eig <- mca_res$eig
  pct_var <- eig[, 2]
  cum_var <- eig[, 3]

  png_scree <- file.path(output_dir, "pca_qualitative_scree.png")
  png(png_scree, width = 1000, height = 600, res = 150)
  par(mar = c(5, 5, 5, 5))
  n_dim <- min(ncp, nrow(eig))
  barplot(pct_var[1:n_dim],
          col = "#5B8FA3",
          border = "#2C3E50",
          main = "MCA - Variabili Qualitative: Varianza spiegata per dimensione",
          xlab = "Dimensione",
          ylab = "% varianza spiegata",
          names.arg = paste0("Dim ", 1:n_dim))
  par(new = TRUE)
  plot(1:n_dim, cum_var[1:n_dim],
       type = "o", pch = 19, col = "#1A5276", lwd = 2,
       axes = FALSE, xlab = "", ylab = "", ylim = c(0, 100))
  axis(4, at = seq(0, 100, 20), las = 1, col = "#1A5276", col.axis = "#1A5276")
  mtext("Cumulativo %", side = 4, line = 3, col = "#1A5276")
  dev.off()
  cat(sprintf("✓ Scree plot salvato: %s\n", png_scree))

  # Aggrega coordinate e contributi a livello variabile (non per singola categoria)
  # rownames var_coord sono "var_level" (es. "Gender_0", "FAVC_1")
  var_coord_cat <- mca_res$var$coord
  var_contrib_cat <- mca_res$var$contrib
  cat_names <- rownames(var_coord_cat)
  var_names_from_cat <- sub("_[^_]*$", "", cat_names)

  # Coordinate variabile = baricentro delle sue categorie
  var_coord <- matrix(NA, nrow = length(available_vars), ncol = ncol(var_coord_cat))
  rownames(var_coord) <- available_vars
  colnames(var_coord) <- colnames(var_coord_cat)

  var_contrib <- matrix(NA, nrow = length(available_vars), ncol = ncol(var_contrib_cat))
  rownames(var_contrib) <- available_vars
  colnames(var_contrib) <- colnames(var_contrib_cat)

  for (v in available_vars) {
    idx <- var_names_from_cat == v
    if (sum(idx) > 0) {
      var_coord[v, ] <- colMeans(var_coord_cat[idx, , drop = FALSE])
      var_contrib[v, ] <- colSums(var_contrib_cat[idx, , drop = FALSE])
    }
  }

  # Plot variabili (una per variabile, non per categoria) sui primi 2 assi
  png_var <- png_ind <- NULL
  if (ncol(var_coord) >= 2) {
    png_var <- file.path(output_dir, "pca_qualitative_variables.png")
    png(png_var, width = 1000, height = 900, res = 150)
    par(mar = c(6, 6, 5, 2))
    plot(var_coord[, 1], var_coord[, 2],
         type = "n",
         main = sprintf("MCA - Variabili qualitative sui primi 2 assi\n(Dim1: %.1f%%, Dim2: %.1f%%)", pct_var[1], pct_var[2]),
         xlab = paste0("Dimensione 1 (", round(pct_var[1], 1), "%)"),
         ylab = paste0("Dimensione 2 (", round(pct_var[2], 1), "%)"))
    abline(h = 0, v = 0, col = "gray80", lty = 2)
    text(var_coord[, 1], var_coord[, 2],
         labels = rownames(var_coord),
         cex = 1,
         col = "#2C3E50")
    dev.off()
    cat(sprintf("✓ Plot variabili salvato: %s\n", png_var))
  }

  # Plot individui sui primi 2 assi
  ind_coord <- mca_res$ind$coord
  if (ncol(ind_coord) >= 2) {
    png_ind <- file.path(output_dir, "pca_qualitative_individui.png")
    png(png_ind, width = 900, height = 900, res = 150)
    par(mar = c(5, 5, 5, 2))
    plot(ind_coord[, 1], ind_coord[, 2],
         pch = 19,
         col = rgb(0.36, 0.56, 0.64, 0.5),
         main = sprintf("MCA - Individui sui primi 2 assi\n(n = %d)", nrow(ind_coord)),
         xlab = paste0("Dimensione 1 (", round(pct_var[1], 1), "%)"),
         ylab = paste0("Dimensione 2 (", round(pct_var[2], 1), "%)"))
    abline(h = 0, v = 0, col = "gray80", lty = 2)
    dev.off()
    cat(sprintf("✓ Plot individui salvato: %s\n", png_ind))
  }

  # Contributi delle variabili (non delle singole categorie) alla Dimensione 1
  contrib_dim1 <- var_contrib[, 1]
  contrib_ord <- sort(contrib_dim1, decreasing = TRUE)

  png_contrib <- file.path(output_dir, "pca_qualitative_contributi.png")
  png(png_contrib, width = 1000, height = 600, res = 150)
  par(mar = c(8, 5, 5, 2))
  barplot(contrib_ord,
          col = "#5B8FA3",
          border = "#2C3E50",
          main = "Contributo delle variabili qualitative alla Dimensione 1",
          xlab = "",
          ylab = "Contributo (%)",
          las = 2,
          cex.names = 0.9)
  dev.off()
  cat(sprintf("✓ Plot contributi salvato: %s\n", png_contrib))

  # Riepilogo
  cat(sprintf("\n--- MCA Variabili Qualitative ---\n"))
  cat(sprintf("Variabili analizzate: %d\n", length(available_vars)))
  cat(sprintf("Osservazioni: %d\n", nrow(data_subset)))
  cat(sprintf("Dimensioni estratte: %d\n", ncp))
  cat(sprintf("Varianza cumulativa (prime 2 dim): %.1f%%\n", cum_var[2]))

  invisible(list(
    mca_result = mca_res,
    eigenvalues = eig,
    variables = available_vars,
    path_scree = png_scree,
    path_variables = png_var,
    path_individui = png_ind,
    path_contributi = png_contrib
  ))
}


# La funzione contingency_bivariate_analysis analizza le coppie di variabili categoriche:
#' Esegue un'analisi bivariata completa per tutte le coppie di variabili qualitative specificate.
#' Per ogni coppia di variabili, genera:
#' \itemize{
#'   \item Tabella di contingenza con frequenze assolute
#'   \item Frequenze relative (percentuali) per riga e colonna
#'   \item Frequenze condizionate (distribuzione di una variabile dato l'altra)
#'   \item Barplot stacked mostrando la distribuzione condizionata
#'   \item Barplot affiancati per confronto diretto delle frequenze
#'   \item Calcolo di Cramér's V come misura di associazione tra variabili categoriche
#' }
#'
#' Cramér's V varia tra 0 (indipendenza) e 1 (associazione perfetta) e permette di quantificare
#' la forza dell'associazione tra variabili categoriche.
#'
#' @param data Data frame. Dataset contenente le variabili categoriche da analizzare.
#' @param variables Vettore carattere o NULL. Nomi delle variabili categoriche da analizzare.
#'   Se NULL, usa tutte le variabili categoriche dal data dictionary (escluso 'class').
#'   Deve contenere almeno 2 variabili. Predefinito: NULL.
#' @param output_dir Stringa carattere. Directory dove salvare i grafici generati.
#'   Predefinito: "output/contingency". La directory viene creata automaticamente se non esiste.
#'
#' @return Invisibile. I grafici vengono salvati come file PNG nella directory di output.
#'   Ogni coppia genera file "{var1}_vs_{var2}_contingency.png" con i barplot.
#'   Le statistiche (tabelle di contingenza, Cramér's V) vengono stampate a console.
contingency_bivariate_analysis <- function(data,
                                           variables = NULL,
                                           output_dir = "output/contingency") {

  # Helper function: applica etichette dal data dictionary a un vettore di livelli
  # Converte valori numerici/codici in etichette leggibili per visualizzazione nei grafici
  apply_labels <- function(levels_vec, var_name) {
    labels_dict <- get_categorical_labels()[[var_name]]
    if (is.null(labels_dict)) return(as.character(levels_vec))
    out <- character(length(levels_vec))
    for (k in seq_along(levels_vec)) {
      key <- as.character(levels_vec[k])
      out[k] <- if (!is.null(labels_dict[[key]])) labels_dict[[key]] else key
    }
    out
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  if (is.null(variables)) {
    categorical_cols <- get_categorical_columns()
    variables <- categorical_cols[categorical_cols %in% colnames(data)]
  }
  available_vars <- variables[variables %in% colnames(data)]

  if (length(available_vars) < 2) {
    stop("Almeno 2 variabili categoriche devono essere presenti nel dataset")
  }

  dict <- get_data_dictionary()
  results <- list()
  n_pairs <- 0

  cat(sprintf("Analisi bivariata categoriale su %d variabili\n", length(available_vars)))
  cat(sprintf("Coppie da analizzare: %d\n\n", choose(length(available_vars), 2)))

  for (i in 1:(length(available_vars) - 1)) {
    for (j in (i + 1):length(available_vars)) {
      var_x <- available_vars[i]
      var_y <- available_vars[j]

      x_data <- data[[var_x]]
      y_data <- data[[var_y]]
      valid <- complete.cases(x_data, y_data)
      if (sum(valid) < 2) next

      x_clean <- x_data[valid]
      y_clean <- y_data[valid]

      # 1) Tabella di contingenza e frequenze
      tbl <- table(x_clean, y_clean)
      n <- sum(tbl)

      # Frequenze relative congiunte: n_ij / n
      joint_rel <- tbl / n

      # Frequenze relative condizionate (per riga): P(Y|X)
      row_totals <- rowSums(tbl)
      cond_row <- sweep(tbl, 1, row_totals, "/")
      cond_row[is.nan(cond_row)] <- 0

      # Frequenze relative condizionate (per colonna): P(X|Y)
      col_totals <- colSums(tbl)
      cond_col <- sweep(tbl, 2, col_totals, "/")
      cond_col[is.nan(cond_col)] <- 0

      # Test chi-quadro e Cramér's V per il commento
      chi2_res <- suppressWarnings(chisq.test(tbl))
      chi2_stat <- chi2_res$statistic
      p_val <- chi2_res$p.value
      min_dim <- min(nrow(tbl), ncol(tbl))
      cramer_v <- if (min_dim > 1) sqrt(chi2_stat / (n * (min_dim - 1))) else 0

      # Etichette per i grafici
      row_labels <- apply_labels(rownames(tbl), var_x)
      col_labels <- apply_labels(colnames(tbl), var_y)
      rownames(joint_rel) <- row_labels
      colnames(joint_rel) <- col_labels
      rownames(cond_row) <- row_labels
      colnames(cond_row) <- col_labels

      # 2) Visualizzazioni
      # Palette professionale (blu/grigi come nel resto del progetto)
      pal <- colorRampPalette(c("#5B8FA3", "#2C3E50", "#7BA3C9", "#4A6FA5"))(ncol(tbl))

      # A) Stacked barplot delle frequenze relative congiunte
      png_stacked <- file.path(output_dir, sprintf("%s_vs_%s_stacked.png", var_x, var_y))
      png(png_stacked, width = 1000, height = 700, res = 150)
      par(mar = c(6, 5, 5, 8))
      bp <- barplot(t(joint_rel),
                    beside = FALSE,
                    col = pal,
                    border = "#2C3E50",
                    main = sprintf("Frequenze relative congiunte\n%s vs %s", var_x, var_y),
                    xlab = var_x,
                    ylab = "Frequenza relativa",
                    las = 2,
                    legend.text = col_labels,
                    args.legend = list(x = "topright", inset = c(-0.22, 0), bty = "n", cex = 0.85, title = var_y))
      abline(h = 0)
      dev.off()
      cat(sprintf("✓ Stacked barplot salvato: %s\n", png_stacked))

      # B) Barplot affiancati delle frequenze condizionate P(Y|X)
      png_cond <- file.path(output_dir, sprintf("%s_vs_%s_conditional.png", var_x, var_y))
      png(png_cond, width = 1000, height = 700, res = 150)
      par(mar = c(6, 5, 5, 8))
      bp <- barplot(t(cond_row),
                    beside = TRUE,
                    col = pal,
                    border = "#2C3E50",
                    main = sprintf("Frequenze relative condizionate P(%s | %s)\n%s vs %s", var_y, var_x, var_x, var_y),
                    xlab = var_x,
                    ylab = "Frequenza relativa condizionata",
                    ylim = c(0, 1.05),
                    las = 2,
                    legend.text = col_labels,
                    args.legend = list(x = "topright", inset = c(-0.22, 0), bty = "n", cex = 0.85, title = var_y))
      abline(h = 0)
      dev.off()
      cat(sprintf("✓ Barplot condizionate salvato: %s\n", png_cond))

      # 3) Commento su indipendenza vs associazione
      if (cramer_v < 0.1) {
        commento <- "Quasi indipendenza: le distribuzioni condizionate sono molto simili tra loro."
      } else if (cramer_v < 0.3) {
        commento <- "Associazione debole: le distribuzioni condizionate presentano differenze modeste."
      } else if (cramer_v < 0.5) {
        commento <- "Associazione moderata: le distribuzioni condizionate differiscono in modo evidente."
      } else {
        commento <- "Associazione forte: le distribuzioni condizionate sono molto diverse (forte dipendenza)."
      }

      cat(sprintf("\n--- %s vs %s ---\n", var_x, var_y))
      cat(sprintf("Chi-quadro: %.2f, p-value: %.4f, Cramér's V: %.3f\n", chi2_stat, p_val, cramer_v))
      cat(sprintf("Interpretazione: %s\n", commento))

      n_pairs <- n_pairs + 1
      results[[n_pairs]] <- list(
        var_x = var_x,
        var_y = var_y,
        contingency_table = tbl,
        joint_relative = joint_rel,
        conditional_row = cond_row,
        conditional_col = cond_col,
        chi2 = chi2_stat,
        pvalue = p_val,
        cramers_v = cramer_v,
        interpretation = commento
      )
    }
  }

  # Heatmap Cramér's V (coefficiente di associazione per variabili categoriche)
  n_vars <- length(available_vars)
  cramer_matrix <- matrix(1, nrow = n_vars, ncol = n_vars)
  rownames(cramer_matrix) <- available_vars
  colnames(cramer_matrix) <- available_vars

  data_cat <- data[, available_vars, drop = FALSE]
  data_complete <- data_cat[complete.cases(data_cat), , drop = FALSE]

  if (nrow(data_complete) >= 2) {
    for (i in seq_len(n_vars)) {
      for (j in seq_len(n_vars)) {
        if (i == j) {
          cramer_matrix[i, j] <- 1
        } else {
          tbl_ij <- table(data_complete[[available_vars[i]]], data_complete[[available_vars[j]]])
          chi2_res <- suppressWarnings(chisq.test(tbl_ij))
          n_ij <- sum(tbl_ij)
          min_dim <- min(nrow(tbl_ij), ncol(tbl_ij))
          cramer_matrix[i, j] <- if (min_dim > 1) {
            sqrt(chi2_res$statistic / (n_ij * (min_dim - 1)))
          } else 0
        }
      }
    }

    heatmap_file <- file.path(output_dir, "cramers_v_heatmap.png")
    png(heatmap_file, width = 1200, height = 1100, res = 150)

    par(mar = c(10, 10, 5, 4))
    pal <- colorRampPalette(c("#F7F7F7", "#DEEBF7", "#9ECAE1", "#4292C6", "#2166AC"))(100)
    image(seq_len(n_vars), seq_len(n_vars),
          t(cramer_matrix[n_vars:1, , drop = FALSE]),
          col = pal,
          main = "Heatmap Cramér's V\n(Associazione tra variabili categoriche)",
          xlab = "", ylab = "",
          axes = FALSE,
          zlim = c(0, 1))

    axis(1, at = seq_len(n_vars), labels = available_vars, las = 2, cex.axis = 0.9)
    axis(2, at = seq_len(n_vars), labels = rev(available_vars), las = 2, cex.axis = 0.9)

    for (i in seq_len(n_vars)) {
      for (j in seq_len(n_vars)) {
        row_plot <- n_vars - i + 1
        val <- cramer_matrix[i, j]
        text_col <- if (val > 0.5) "white" else "black"
        text(j, row_plot, sprintf("%.2f", val), cex = 0.75, col = text_col)
      }
    }

    dev.off()
    cat(sprintf("✓ Heatmap Cramér's V salvata: %s\n", heatmap_file))
  }

  cat(sprintf("\n✓ Analisi contingenza completata per %d coppie\n", n_pairs))
  invisible(results)
}

# =============================================================================
# Valutazione adattamento dati sintetici a distribuzioni note
# =============================================================================

#' Valuta se i dati sintetici seguono distribuzioni statistiche note utilizzando test statistici
#' e visualizzazioni. Per variabili continue, testa l'ipotesi di normalità. Per variabili binarie,
#' analizza se seguono una distribuzione di Bernoulli.
#'
#' \strong{Per variabili continue:}
#' \itemize{
#'   \item Test di Shapiro-Wilk per normalità (per campioni <= 5000)
#'   \item Test di Kolmogorov-Smirnov per normalità (per campioni > 5000)
#'   \item Q-Q plot confrontando quantili osservati vs teorici (normale)
#'   \item Istogramma con densità osservata e densità teorica normale sovrapposta
#'   \item ECDF (Empirical Cumulative Distribution Function) vs CDF teorica normale
#' }
#'
#' \strong{Per variabili binarie:}
#' \itemize{
#'   \item Stima del parametro p della distribuzione di Bernoulli
#'   \item Intervallo di confidenza per p (95\%)
#'   \item Test binomiale per verificare se p = 0.5 (distribuzione uniforme)
#'   \item Barplot delle frequenze osservate vs attese
#' }
#'
#' @param data Data frame. Dataset sintetico da analizzare.
#' @param output_dir Stringa carattere. Directory dove salvare i grafici generati.
#'   Predefinito: "output/synthetic_distribution_fit". La directory viene creata automaticamente se non esiste.
#' @param continuous_vars Vettore carattere o NULL. Nomi delle variabili numeriche continue da analizzare.
#'   Se NULL, usa tutte le variabili numeriche dal data dictionary (originali + derivate).
#'   Predefinito: NULL.
#' @param binary_vars Vettore carattere o NULL. Nomi delle variabili binarie da analizzare.
#'   Se NULL, rileva automaticamente colonne con esattamente 2 livelli unici.
#'   Predefinito: NULL.
#' @param alpha Numerico. Livello di significatività per i test statistici.
#'   Predefinito: 0.05. Un test con p-value >= alpha indica che i dati sono compatibili con la distribuzione teorica.
#'
#' @return Lista invisibile con elementi:
#'   \item{summary_table}{Data frame con risultati dei test per ogni variabile}
#'   \item{plot_paths}{Vettore carattere con percorsi dei grafici generati}
#'   \item{output_dir}{Percorso della directory di output}
evaluate_synthetic_distribution_fit <- function(data,
                                                output_dir = "output/synthetic_distribution_fit",
                                                continuous_vars = NULL,
                                                binary_vars = NULL,
                                                alpha = 0.05) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Helper function: converte un vettore a numerico gestendo factors
  # Utile per variabili categoriche codificate come numeri o factors
  to_num <- function(x) {
    if (is.factor(x)) return(as.numeric(as.character(x)))
    as.numeric(x)
  }

  # Variabili continue di default
  cand_cont <- c("Age", "Height", "Weight", "BMI", "Activity_Index", "Diet_Quality_Index", "Diet_Quality")
  if (is.null(continuous_vars)) {
    continuous_vars <- intersect(cand_cont, colnames(data))
  } else {
    continuous_vars <- continuous_vars[continuous_vars %in% colnames(data)]
  }

  results_list <- list()
  plot_paths <- list(continuous = character(0), binary = character(0), summary = NULL)

  # ---------- Variabili continue: Normalità ----------
  for (var in continuous_vars) {
    x <- to_num(data[[var]])
    x <- x[!is.na(x)]
    if (length(x) < 5) next

    mu <- mean(x)
    sigma <- sd(x)
    n <- length(x)

    # Test Shapiro-Wilk (solo se n <= 5000)
    sw_stat <- sw_pval <- NA
    if (n <= 5000) {
      sw <- shapiro.test(x)
      sw_stat <- sw$statistic
      sw_pval <- sw$p.value
    }

    # KS vs N(mu, sigma) - p-value approssimato (conservativo se parametri stimati; warning "ties" soppresso per dati arrotondati)
    ks <- suppressWarnings(ks.test(x, "pnorm", mean = mu, sd = sigma))
    ks_stat <- ks$statistic
    ks_pval <- ks$p.value

    results_list[[length(results_list) + 1]] <- data.frame(
      Variable = var,
      Type = "continuous",
      Distribution = "Normal",
      Test = c(if (n <= 5000) "Shapiro-Wilk", "Kolmogorov-Smirnov"),
      Statistic = c(if (n <= 5000) round(sw_stat, 4), round(ks_stat, 4)),
      P_value = c(if (n <= 5000) round(sw_pval, 4), round(ks_pval, 4)),
      Fit_acceptable = c(
        if (n <= 5000) sw_pval >= alpha else NA,
        ks_pval >= alpha
      ),
      N = n,
      Lambda = NA_real_,
      Estimate_p = NA_real_,
      CI_low = NA_real_,
      CI_high = NA_real_,
      stringsAsFactors = FALSE
    )

    # Grafici: Q-Q, istogramma + densità, ECDF vs CDF
    png_file <- file.path(output_dir, sprintf("fit_continuous_%s.png", var))
    png(png_file, width = 1400, height = 500, res = 150)
    par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
    # Q-Q plot
    qqnorm(x, main = sprintf("Q-Q plot vs Normale - %s", var), pch = 19, col = rgb(0.2, 0.4, 0.6, 0.6))
    qqline(x, distribution = qnorm, col = "red", lwd = 2)
    # Istogramma + densità normale
    hist(x, breaks = "FD", prob = TRUE, col = "lightsteelblue", border = "white",
         main = sprintf("Istogramma + Normale - %s", var), xlab = var)
    xgrid <- seq(min(x), max(x), length.out = 200)
    lines(xgrid, dnorm(xgrid, mu, sigma), col = "darkred", lwd = 2)
    legend("topright", legend = c("Dati", "N(media, sd)"), fill = c("lightsteelblue", NA), border = NA, lty = c(NA, 1), col = c(NA, "darkred"), lwd = c(NA, 2), bty = "n")
    # ECDF vs CDF
    plot(ecdf(x), main = sprintf("ECDF vs CDF Normale - %s", var), xlab = var, ylab = "F(x)")
    curve(pnorm(x, mu, sigma), add = TRUE, col = "darkred", lwd = 2, lty = 2)
    legend("bottomright", legend = c("ECDF", "CDF Normale"), col = c("black", "darkred"), lty = c(1, 2), lwd = c(1, 2), bty = "n")
    dev.off()
    plot_paths$continuous[var] <- png_file
    cat(sprintf("✓ Grafici continui salvati: %s\n", png_file))
  }

  # ---------- Variabili binarie: Bernoulli ----------
  if (is.null(binary_vars)) {
    # Auto-detect: colonne con esattamente 2 livelli (0/1 o due valori distinti)
    binary_vars <- character(0)
    for (col in colnames(data)) {
      if (col %in% continuous_vars) next
      x <- data[[col]]
      x <- x[!is.na(x)]
      if (length(x) < 5) next
      u <- unique(x)
      if (length(u) != 2L) next
      binary_vars <- c(binary_vars, col)
    }
  } else {
    binary_vars <- binary_vars[binary_vars %in% colnames(data)]
  }

  for (var in binary_vars) {
    raw <- data[[var]]
    ok <- !is.na(raw)
    raw <- raw[ok]
    u <- unique(raw)
    if (length(u) != 2L) next
    # Mappa a 0/1 (0 = primo livello, 1 = secondo)
    x <- as.integer(factor(raw, levels = u)) - 1L
    if (length(x) < 5) next
    n <- length(x)
    n_success <- sum(x == 1)
    p_hat <- n_success / n
    bt <- binom.test(n_success, n, p = 0.5, alternative = "two.sided")
    ci_low <- bt$conf.int[1]
    ci_high <- bt$conf.int[2]
    p_value <- bt$p.value

    results_list[[length(results_list) + 1]] <- data.frame(
      Variable = var,
      Type = "binary",
      Distribution = "Bernoulli",
      Test = "Binomial (H0: p=0.5)",
      Statistic = round(p_hat, 4),
      P_value = round(p_value, 4),
      Fit_acceptable = p_value >= alpha,
      N = n,
      Lambda = NA_real_,
      Estimate_p = round(p_hat, 4),
      CI_low = round(ci_low, 4),
      CI_high = round(ci_high, 4),
      stringsAsFactors = FALSE
    )

    png_file <- file.path(output_dir, sprintf("fit_binary_%s.png", var))
    png(png_file, width = 800, height = 600, res = 150)
    par(mar = c(5, 5, 8, 2))
    tab <- table(factor(x, levels = c(0, 1)))
    barplot(prop.table(tab) * 100, col = c("steelblue", "coral"),
            names.arg = c("0", "1"), main = NA,
            xlab = "Valore", ylab = "Percentuale (%)", ylim = c(0, 100))
    abline(h = p_hat * 100, lty = 2, lwd = 2, col = "darkred")
    title(main = sprintf("Bernoulli(p) - %s", var), line = 6.2)
    mtext(sprintf("p = %.3f  (IC95%%: %.3f-%.3f)", p_hat, ci_low, ci_high), side = 3, line = 4.8, cex = 0.9)
    mtext("H0: p = 0.5", side = 3, line = 3.6, cex = 0.85)
    dev.off()
    plot_paths$binary[var] <- png_file
    cat(sprintf("✓ Grafico Bernoulli salvato: %s\n", png_file))
  }

  # ---------- Tabella riepilogativa ----------
  summary_table <- do.call(rbind, results_list)
  if (nrow(summary_table) == 0) {
    cat("Nessuna variabile analizzata.\n")
    return(invisible(list(summary_table = NULL, plot_paths = plot_paths)))
  }

  # La tabella summary viene calcolata ma non salvata (rimossa su richiesta)
  # summary_table contiene i risultati dei test di adattamento

  # Stampa a video
  cat("\n--- Riepilogo test di adattamento ---\n")
  print(summary_table)
  cat("\n")

  return(invisible(list(
    summary_table = summary_table,
    plot_paths = plot_paths,
    alpha = alpha
  )))
}

