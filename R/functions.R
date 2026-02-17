# functions.R

#' Crea un indice composito di qualità della dieta (Diet_Quality_Index) combinando
#' quattro variabili dietetiche: consumo di acqua (CH2O), consumo di cibi ad alto contenuto
#' calorico (FAVC), frequenza di consumo di verdure (FCVC), e consumo di cibo tra i pasti (CAEC).
#'
#' La formula utilizzata è:
#' \deqn{DQI = 0.4 \times CH2O_n + 0.3 \times (1 - FAVC_n) + 0.2 \times FCVC_n + 0.1 \times (1 - CAEC_n)}
#'
#' Dove tutte le variabili sono normalizzate su [0,1] usando i loro range teorici:
#' \itemize{
#'   \item CH2O: [1, 3] → [0, 1] (più acqua = migliore qualità)
#'   \item FAVC: [0, 1] → [0, 1], poi invertito (meno cibi calorici = migliore qualità)
#'   \item FCVC: [1, 3] → [0, 1] (più verdure = migliore qualità)
#'   \item CAEC: [0, 3] → [0, 1], poi invertito (meno spuntini = migliore qualità)
#' }
#'
#' L'indice risultante varia tra 0 (dieta di bassa qualità) e 1 (dieta di alta qualità).
#'
#' @param data Data frame. Dataset contenente le variabili dietetiche necessarie.
#'   Deve contenere le colonne: CH2O, FAVC, FCVC, CAEC.
#' @param feature_name Stringa carattere. Nome della colonna da creare nel dataset.
#'   Predefinito: "Diet_Quality_Index".
#' @param verbose Logico. Se TRUE, stampa informazioni dettagliate sul processo di creazione.
#'   Predefinito: TRUE.
#'
#' @return Data frame. Il dataset originale con una nuova colonna aggiunta contenente
#'   i valori di Diet_Quality_Index calcolati. I valori sono compresi tra 0 e 1,
#'   con NA se almeno una delle variabili componenti è mancante.
create_dietary_summary_feature <- function(data,
                                            feature_name = "Diet_Quality_Index",
                                            verbose = TRUE) {
  
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  # Variabili nella formula DQI
  variables <- c("CH2O", "FAVC", "FCVC", "CAEC")
  
  # Range teorici per la normalizzazione (dal data dictionary)
  theoretical_ranges <- list(
    "CH2O" = c(1, 3),   # 1-3
    "FAVC" = c(0, 1),   # 0-1
    "FCVC" = c(1, 3),   # 1-3
    "CAEC" = c(0, 3)    # 0-3
  )
  
  # Verifica che tutte le variabili siano presenti
  missing_vars <- variables[!variables %in% colnames(data)]
  if (length(missing_vars) > 0) {
    stop(sprintf("Variabili mancanti nel dataset: %s", paste(missing_vars, collapse = ", ")))
  }
  
  if (verbose) {
    cat("\n", rep("=", 70), "\n", sep = "")
    cat("CREAZIONE DIET_QUALITY_INDEX (DQI)\n")
    cat(rep("=", 70), "\n\n", sep = "")
    cat("Formula: DQI = 0.4 * CH2O_n + 0.3 * (1 - FAVC_n) + 0.2 * FCVC_n + 0.1 * (1 - CAEC_n)\n\n")
    cat("Variabili e range di normalizzazione:\n")
    for (var in variables) {
      r <- theoretical_ranges[[var]]
      cat(sprintf("  %s: [%d, %d] → [0, 1]\n", var, r[1], r[2]))
    }
    cat("\n")
  }
  
  n_rows <- nrow(data)
  
  # Normalizza ogni variabile su [0, 1]
  var_n_list <- list()
  for (var_name in variables) {
    var_data <- data[[var_name]]
    if (is.factor(var_data)) var_data <- as.numeric(as.character(var_data))
    else if (!is.numeric(var_data)) var_data <- as.numeric(var_data)
    r <- theoretical_ranges[[var_name]]
    vn <- (var_data - r[1]) / (r[2] - r[1])
    vn <- pmax(0, pmin(1, vn))
    vn[is.na(var_data)] <- NA
    var_n_list[[var_name]] <- vn
    if (verbose) {
      cat(sprintf("✓ %s normalizzato su [%d, %d] → [0, 1]\n", var_name, r[1], r[2]))
    }
  }
  
  # DQI = 0.4 * CH2O_n + 0.3 * (1 - FAVC_n) + 0.2 * FCVC_n + 0.1 * (1 - CAEC_n)
  dqi <- 0.4 * var_n_list[["CH2O"]] +
         0.3 * (1 - var_n_list[["FAVC"]]) +
         0.2 * var_n_list[["FCVC"]] +
         0.1 * (1 - var_n_list[["CAEC"]])
  
  # NA se almeno una variabile è NA
  na_mask <- is.na(var_n_list[["CH2O"]]) | is.na(var_n_list[["FAVC"]]) |
             is.na(var_n_list[["FCVC"]]) | is.na(var_n_list[["CAEC"]])
  dqi[na_mask] <- NA
  
  data[[feature_name]] <- dqi
  
  n_missing <- sum(is.na(dqi))
  feature_min <- min(dqi, na.rm = TRUE)
  feature_max <- max(dqi, na.rm = TRUE)
  feature_mean <- mean(dqi, na.rm = TRUE)
  feature_sd <- sd(dqi, na.rm = TRUE)
  
  if (verbose) {
    cat(sprintf("\n✓ Feature '%s' (Diet_Quality_Index) creata con successo\n", feature_name))
    cat(sprintf("   Formula: DQI = 0.4*CH2O_n + 0.3*(1-FAVC_n) + 0.2*FCVC_n + 0.1*(1-CAEC_n)\n"))
    cat(sprintf("   Range: [%.4f, %.4f]\n", feature_min, feature_max))
    cat(sprintf("   Media: %.4f\n", feature_mean))
    cat(sprintf("   Deviazione Standard: %.4f\n", feature_sd))
    if (n_missing > 0) {
      cat(sprintf("   Valori mancanti: %d (%.2f%%)\n", n_missing, (n_missing / n_rows) * 100))
    } else {
      cat(sprintf("   Valori mancanti: 0\n"))
    }
    cat("\n")
  }
  
  return(data)
}

#' Genera una serie completa di grafici per analizzare la metrica Diet_Quality_Index (DQI).
#' Include distribuzioni, relazioni con le variabili componenti, confronti con BMI,
#' e statistiche descrittive.
#'
#' I grafici generati includono:
#' \itemize{
#'   \item Distribuzione DQI (istogramma con curva di densità)
#'   \item Boxplot DQI per livelli di ciascuna variabile componente (CH2O, FAVC, FCVC, CAEC)
#'   \item Scatter plot DQI vs variabili componenti normalizzate con correlazioni
#'   \item Confronto DQI vs indice teorico di comportamenti di qualità
#'   \item Scatter plot e boxplot DQI vs BMI (se disponibile)
#'   \item Riepilogo statistico con media, SD, min, max, correlazioni
#' }
#'
#' @param data Data frame. Dataset contenente la colonna Diet_Quality_Index.
#'   Se include_bmi=TRUE, deve contenere anche la colonna BMI.
#' @param output_dir Stringa carattere. Directory dove salvare i grafici generati.
#'   Predefinito: "output/dqi_analysis". La directory viene creata automaticamente se non esiste.
#' @param include_bmi Logico. Se TRUE, genera anche grafici che confrontano DQI con BMI.
#'   Richiede che il dataset contenga la colonna BMI. Predefinito: TRUE.
#'
#' @return Lista invisibile con elementi:
#'   \item{output_dir}{Stringa carattere. Percorso della directory di output}
#'   \item{n}{Numerico. Numero di osservazioni non mancanti per DQI}
analyze_diet_quality_index <- function(data,
                                        output_dir = "output/dqi_analysis",
                                        include_bmi = TRUE) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  if (!"Diet_Quality_Index" %in% colnames(data)) {
    stop("Il dataset deve contenere la colonna 'Diet_Quality_Index'. Eseguire prima create_dietary_summary_feature(data).")
  }
  if (include_bmi && !"BMI" %in% colnames(data)) {
    include_bmi <- FALSE
    cat("⚠️  Colonna BMI non presente: grafici vs BMI saltati\n")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  dqi <- data[["Diet_Quality_Index"]]
  n <- sum(!is.na(dqi))
  components <- c("CH2O", "FAVC", "FCVC", "CAEC")

  # Palette coerente con il progetto
  col_fill <- "#5B8FA3"
  col_border <- "#2C3E50"
  col_line <- "#1A5276"

  # -------------------------------------------------------------------------
  # 1. Distribuzione DQI (istogramma + densità)
  # -------------------------------------------------------------------------
  png_1 <- file.path(output_dir, "dqi_distribution.png")
  png(png_1, width = 1000, height = 700, res = 150)
  par(mar = c(5, 5, 5, 2))
  hist(dqi, breaks = 15, col = col_fill, border = col_border,
       main = "Distribuzione Diet_Quality_Index (DQI)",
       xlab = "DQI", ylab = "Frequenza", cex.main = 1.2)
  lines(density(dqi, na.rm = TRUE), col = col_line, lwd = 2)
  abline(v = mean(dqi, na.rm = TRUE), col = "red", lty = 2, lwd = 1.5)
  legend("topright", legend = c("Densità", "Media"), col = c(col_line, "red"), lty = c(1, 2), lwd = c(2, 1.5), bty = "n")
  dev.off()
  cat(sprintf("✓ Distribuzione DQI salvata: %s\n", png_1))

  # -------------------------------------------------------------------------
  # 2. DQI vs variabili componenti (boxplot per livello)
  # -------------------------------------------------------------------------
  png_2 <- file.path(output_dir, "dqi_by_components.png")
  png(png_2, width = 1400, height = 1000, res = 150)
  par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
  for (var in components) {
    if (!var %in% colnames(data)) next
    x <- data[[var]]
    if (is.factor(x)) x <- as.numeric(as.character(x))
    boxplot(dqi ~ x, col = col_fill, border = col_border,
            main = sprintf("DQI per %s", var), xlab = var, ylab = "Diet_Quality_Index")
  }
  dev.off()
  cat(sprintf("✓ DQI per componenti salvato: %s\n", png_2))

  # -------------------------------------------------------------------------
  # 3. Scatter DQI vs ciascuna componente (normalizzata)
  # -------------------------------------------------------------------------
  theoretical_ranges <- list(CH2O = c(1, 3), FAVC = c(0, 1), FCVC = c(1, 3), CAEC = c(0, 3))
  png_3 <- file.path(output_dir, "dqi_vs_components_scatter.png")
  png(png_3, width = 1200, height = 1200, res = 150)
  par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
  for (var in components) {
    if (!var %in% colnames(data)) next
    x <- data[[var]]
    if (is.factor(x)) x <- as.numeric(as.character(x))
    r <- theoretical_ranges[[var]]
    x_n <- (x - r[1]) / (r[2] - r[1])
    x_n <- pmax(0, pmin(1, x_n))
    plot(x_n, dqi, pch = 19, col = rgb(0.36, 0.56, 0.64, 0.5),
         main = sprintf("DQI vs %s (normalizzato)", var), xlab = paste0(var, "_n"), ylab = "DQI")
    if (sum(!is.na(x_n) & !is.na(dqi)) > 1) {
      abline(lm(dqi ~ x_n, na.action = na.exclude), col = "red", lwd = 2, lty = 2)
      cor_val <- cor(x_n, dqi, use = "complete.obs")
      text(0.05, max(dqi, na.rm = TRUE) * 0.95, sprintf("r = %.3f", cor_val), pos = 4, font = 2)
    }
  }
  dev.off()
  cat(sprintf("✓ Scatter DQI vs componenti salvato: %s\n", png_3))

  # -------------------------------------------------------------------------
  # 4. DQI vs indice "comportamenti di qualità" (atteso: correlazione positiva)
  # -------------------------------------------------------------------------
  # Comportamenti di qualità: CH2O alto, FAVC basso (1-FAVC alto), FCVC alto, CAEC basso (1-CAEC alto)
  # DQI già incorpora questi; creiamo un indice analogo per confronto
  if (all(components %in% colnames(data))) {
    r <- list(CH2O = c(1, 3), FAVC = c(0, 1), FCVC = c(1, 3), CAEC = c(0, 3))
    qual_ch2o <- (data[["CH2O"]] - 1) / 2
    qual_favc <- 1 - data[["FAVC"]]
    qual_fcvc <- (data[["FCVC"]] - 1) / 2
    qual_caec <- 1 - data[["CAEC"]] / 3
    quality_behaviors <- (0.4 * qual_ch2o + 0.3 * qual_favc + 0.2 * qual_fcvc + 0.1 * qual_caec)
    qual_cor <- cor(quality_behaviors, dqi, use = "complete.obs")

    png_4 <- file.path(output_dir, "dqi_vs_quality_behaviors.png")
    png(png_4, width = 900, height = 700, res = 150)
    par(mar = c(5, 5, 5, 2))
    plot(quality_behaviors, dqi, pch = 19, col = rgb(0.36, 0.56, 0.64, 0.6),
         main = "DQI vs Indice Comportamenti di Qualità\n(atteso: r ≈ 1)",
         xlab = "Indice comportamenti di qualità (stessa formula)", ylab = "DQI")
    abline(0, 1, col = "red", lty = 2, lwd = 2)
    text(0.05, max(dqi, na.rm = TRUE) * 0.95, sprintf("Correlazione: %.4f", qual_cor), pos = 4, font = 2)
    dev.off()
    cat(sprintf("✓ DQI vs comportamenti di qualità salvato: %s\n", png_4))
  }

  # -------------------------------------------------------------------------
  # 5. DQI vs BMI (se disponibile)
  # -------------------------------------------------------------------------
  if (include_bmi) {
    bmi <- data[["BMI"]]
    dqi_bmi_cor <- cor(dqi, bmi, use = "complete.obs")

    png_5 <- file.path(output_dir, "dqi_vs_bmi.png")
    png(png_5, width = 1200, height = 600, res = 150)
    par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
    plot(dqi, bmi, pch = 19, col = rgb(0.36, 0.56, 0.64, 0.6),
         main = sprintf("DQI vs BMI (r = %.3f)", dqi_bmi_cor),
         xlab = "Diet_Quality_Index", ylab = "BMI")
    if (sum(!is.na(dqi) & !is.na(bmi)) > 1) {
      abline(lm(bmi ~ dqi, na.action = na.exclude), col = "red", lwd = 2, lty = 2)
    }
    # Boxplot BMI per quartili DQI
    dqi_breaks <- quantile(dqi, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    dqi_breaks <- unique(dqi_breaks)
    if (length(dqi_breaks) >= 2) {
      dqi_q <- cut(dqi, breaks = dqi_breaks, include.lowest = TRUE,
                   labels = paste0("Q", seq_len(length(dqi_breaks) - 1)))
      boxplot(bmi ~ dqi_q, col = col_fill, border = col_border,
              main = "BMI per quartili DQI", xlab = "Quartile DQI", ylab = "BMI")
    }
    dev.off()
    cat(sprintf("✓ DQI vs BMI salvato: %s\n", png_5))
  }

  # -------------------------------------------------------------------------
  # 6. Riepilogo statistico DQI e correlazioni
  # -------------------------------------------------------------------------
  png_6 <- file.path(output_dir, "dqi_summary.png")
  png(png_6, width = 1000, height = 700, res = 150)
  par(mar = c(2, 2, 5, 2))
  plot.new()
  stats_text <- sprintf(
    "Diet_Quality_Index - Riepilogo\n\nn = %d\nMedia = %.4f\nSD = %.4f\nMin = %.4f\nMax = %.4f",
    n, mean(dqi, na.rm = TRUE), sd(dqi, na.rm = TRUE),
    min(dqi, na.rm = TRUE), max(dqi, na.rm = TRUE)
  )
  if (include_bmi && exists("dqi_bmi_cor")) {
    stats_text <- paste0(stats_text, sprintf("\n\nCorrelazione DQI-BMI: %.4f", dqi_bmi_cor))
  }
  text(0.5, 0.5, stats_text, cex = 1.2, font = 2)
  dev.off()
  cat(sprintf("✓ Riepilogo DQI salvato: %s\n", png_6))

  cat(sprintf("\n✓ Analisi DQI completata. Grafici in: %s\n", output_dir))
  invisible(list(output_dir = output_dir, n = n))
}

#' Crea un indice composito di attività fisica (Activity_Index) combinando tre variabili:
#' frequenza di attività fisica settimanale (FAF), tempo trascorso con dispositivi tecnologici (TUE),
#' e mezzo di trasporto utilizzato (MTRANS).
#'
#' La formula utilizzata è:
#' \deqn{AI = 0.5 \times FAF_n + 0.3 \times (1 - TUE_n) + 0.2 \times MTRANS\_score}
#'
#' Dove:
#' \itemize{
#'   \item FAF_n: FAF normalizzato su [0,1] da range [0, 3] (più attività fisica = migliore)
#'   \item TUE_n: TUE normalizzato su [0,1] da range [0, 2], poi invertito (meno tempo dispositivi = migliore)
#'   \item MTRANS_score: MTRANS/3, dove 0=Automobile (meno attivo) e 3=Trasporto pubblico/Piedi (più attivo)
#' }
#'
#' L'indice risultante varia tra 0 (bassa attività) e 1 (alta attività).
#'
#' @param data Data frame. Dataset contenente le variabili di attività fisica necessarie.
#'   Deve contenere le colonne: FAF, TUE, MTRANS.
#' @param feature_name Stringa carattere. Nome della colonna da creare nel dataset.
#'   Predefinito: "Activity_Index".
#' @param verbose Logico. Se TRUE, stampa informazioni dettagliate sul processo di creazione.
#'   Predefinito: TRUE.
#'
#' @return Data frame. Il dataset originale con una nuova colonna aggiunta contenente
#'   i valori di Activity_Index calcolati. I valori sono compresi tra 0 e 1,
#'   con NA se almeno una delle variabili componenti è mancante.
create_activity_index <- function(data,
                                  feature_name = "Activity_Index",
                                  verbose = TRUE) {
  
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  variables <- c("FAF", "TUE", "MTRANS")
  missing_vars <- variables[!variables %in% colnames(data)]
  if (length(missing_vars) > 0) {
    stop(sprintf("Variabili mancanti nel dataset: %s", paste(missing_vars, collapse = ", ")))
  }
  
  if (verbose) {
    cat("\n", rep("=", 70), "\n", sep = "")
    cat("CREAZIONE ACTIVITY_INDEX (AI)\n")
    cat(rep("=", 70), "\n\n", sep = "")
    cat("Formula: AI = 0.5 * FAF_n + 0.3 * (1 - TUE_n) + 0.2 * MTRANS_score\n\n")
    cat("Variabili e range:\n")
    cat("  FAF: [0, 3] → FAF_n in [0, 1]\n")
    cat("  TUE: [0, 2] → TUE_n in [0, 1], contributo (1 - TUE_n)\n")
    cat("  MTRANS: [0, 3] → MTRANS_score = MTRANS/3 (più alto = più attivo)\n\n")
  }
  
  n_rows <- nrow(data)
  
  # FAF_n: [0, 3] → [0, 1]
  faf_data <- data[["FAF"]]
  if (is.factor(faf_data)) faf_data <- as.numeric(as.character(faf_data))
  else if (!is.numeric(faf_data)) faf_data <- as.numeric(faf_data)
  FAF_n <- faf_data / 3
  FAF_n <- pmax(0, pmin(1, FAF_n))
  FAF_n[is.na(faf_data)] <- NA
  
  # TUE_n: [0, 2] → [0, 1]
  tue_data <- data[["TUE"]]
  if (is.factor(tue_data)) tue_data <- as.numeric(as.character(tue_data))
  else if (!is.numeric(tue_data)) tue_data <- as.numeric(tue_data)
  TUE_n <- tue_data / 2
  TUE_n <- pmax(0, pmin(1, TUE_n))
  TUE_n[is.na(tue_data)] <- NA
  
  # MTRANS_score: [0, 3] → [0, 1] (0=Auto, 3=PT/Piedi = più attivo)
  mtrans_data <- data[["MTRANS"]]
  if (is.factor(mtrans_data)) mtrans_data <- as.numeric(as.character(mtrans_data))
  else if (!is.numeric(mtrans_data)) mtrans_data <- as.numeric(mtrans_data)
  MTRANS_score <- mtrans_data / 3
  MTRANS_score <- pmax(0, pmin(1, MTRANS_score))
  MTRANS_score[is.na(mtrans_data)] <- NA
  
  # AI = 0.5 * FAF_n + 0.3 * (1 - TUE_n) + 0.2 * MTRANS_score
  activity_index <- 0.5 * FAF_n + 0.3 * (1 - TUE_n) + 0.2 * MTRANS_score
  na_mask <- is.na(FAF_n) | is.na(TUE_n) | is.na(MTRANS_score)
  activity_index[na_mask] <- NA
  
  data[[feature_name]] <- activity_index
  
  n_missing <- sum(is.na(activity_index))
  feature_min <- min(activity_index, na.rm = TRUE)
  feature_max <- max(activity_index, na.rm = TRUE)
  feature_mean <- mean(activity_index, na.rm = TRUE)
  feature_sd <- sd(activity_index, na.rm = TRUE)
  
  if (verbose) {
    cat(sprintf("✓ FAF normalizzato [0, 3] → [0, 1]\n"))
    cat(sprintf("✓ TUE normalizzato [0, 2] → [0, 1]\n"))
    cat(sprintf("✓ MTRANS_score = MTRANS/3\n"))
    cat(sprintf("\n✓ Feature '%s' creata con successo\n", feature_name))
    cat(sprintf("   Range: [%.4f, %.4f]\n", feature_min, feature_max))
    cat(sprintf("   Media: %.4f\n", feature_mean))
    cat(sprintf("   Deviazione Standard: %.4f\n", feature_sd))
    if (n_missing > 0) {
      cat(sprintf("   Valori mancanti: %d (%.2f%%)\n", n_missing, (n_missing / n_rows) * 100))
    } else {
      cat(sprintf("   Valori mancanti: 0\n"))
    }
    cat("\n")
  }
  
  return(data)
}

#' Genera tutti i grafici per analizzare la metrica Activity_Index (AI)
#' Richiede che il dataset contenga Activity_Index; opzionale BMI per grafici vs BMI.
analyze_activity_index <- function(data,
                                    output_dir = "output/activity_index_analysis",
                                    include_bmi = TRUE) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  if (!"Activity_Index" %in% colnames(data)) {
    stop("Il dataset deve contenere 'Activity_Index'. Eseguire prima create_activity_index(data).")
  }
  if (include_bmi && !"BMI" %in% colnames(data)) {
    include_bmi <- FALSE
    cat("⚠️  Colonna BMI non presente: grafici vs BMI saltati\n")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  ai <- data[["Activity_Index"]]
  n <- sum(!is.na(ai))
  components <- c("FAF", "TUE", "MTRANS")
  col_fill <- "#5B8FA3"
  col_border <- "#2C3E50"
  col_line <- "#1A5276"

  # -------------------------------------------------------------------------
  # 1. Distribuzione Activity_Index
  # -------------------------------------------------------------------------
  png_1 <- file.path(output_dir, "activity_index_distribution.png")
  png(png_1, width = 1000, height = 700, res = 150)
  par(mar = c(5, 5, 5, 2))
  hist(ai, breaks = 15, col = col_fill, border = col_border,
       main = "Distribuzione Activity_Index (AI)",
       xlab = "Activity_Index", ylab = "Frequenza", cex.main = 1.2)
  lines(density(ai, na.rm = TRUE), col = col_line, lwd = 2)
  abline(v = mean(ai, na.rm = TRUE), col = "red", lty = 2, lwd = 1.5)
  legend("topright", legend = c("Densità", "Media"), col = c(col_line, "red"), lty = c(1, 2), lwd = c(2, 1.5), bty = "n")
  dev.off()
  cat(sprintf("✓ Distribuzione AI salvata: %s\n", png_1))

  # -------------------------------------------------------------------------
  # 2. AI per livello delle componenti (boxplot)
  # -------------------------------------------------------------------------
  png_2 <- file.path(output_dir, "activity_index_by_components.png")
  png(png_2, width = 1400, height = 500, res = 150)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2))
  for (var in components) {
    if (!var %in% colnames(data)) next
    x <- data[[var]]
    if (is.factor(x)) x <- as.numeric(as.character(x))
    boxplot(ai ~ x, col = col_fill, border = col_border,
            main = sprintf("Activity_Index per %s", var), xlab = var, ylab = "Activity_Index")
  }
  dev.off()
  cat(sprintf("✓ AI per componenti salvato: %s\n", png_2))

  # -------------------------------------------------------------------------
  # 3. Scatter AI vs componenti normalizzate
  # -------------------------------------------------------------------------
  # FAF_n, TUE_n, MTRANS_score
  faf_n <- data[["FAF"]] / 3
  if (is.factor(data[["FAF"]])) faf_n <- as.numeric(as.character(data[["FAF"]])) / 3
  tue_n <- data[["TUE"]] / 2
  if (is.factor(data[["TUE"]])) tue_n <- as.numeric(as.character(data[["TUE"]])) / 2
  mtrans_s <- data[["MTRANS"]] / 3
  if (is.factor(data[["MTRANS"]])) mtrans_s <- as.numeric(as.character(data[["MTRANS"]])) / 3

  png_3 <- file.path(output_dir, "activity_index_vs_components_scatter.png")
  png(png_3, width = 1200, height = 450, res = 150)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2))
  plot(faf_n, ai, pch = 19, col = rgb(0.36, 0.56, 0.64, 0.5),
       main = "AI vs FAF (normalizzato)", xlab = "FAF_n", ylab = "Activity_Index")
  if (sum(!is.na(faf_n) & !is.na(ai)) > 1) {
    abline(lm(ai ~ faf_n, na.action = na.exclude), col = "red", lwd = 2, lty = 2)
    text(0.05, max(ai, na.rm = TRUE) * 0.95, sprintf("r = %.3f", cor(faf_n, ai, use = "complete.obs")), pos = 4, font = 2)
  }
  plot(tue_n, ai, pch = 19, col = rgb(0.36, 0.56, 0.64, 0.5),
       main = "AI vs TUE (normalizzato)\n(atteso: r negativo)", xlab = "TUE_n", ylab = "Activity_Index")
  if (sum(!is.na(tue_n) & !is.na(ai)) > 1) {
    abline(lm(ai ~ tue_n, na.action = na.exclude), col = "red", lwd = 2, lty = 2)
    text(0.05, max(ai, na.rm = TRUE) * 0.95, sprintf("r = %.3f", cor(tue_n, ai, use = "complete.obs")), pos = 4, font = 2)
  }
  plot(mtrans_s, ai, pch = 19, col = rgb(0.36, 0.56, 0.64, 0.5),
       main = "AI vs MTRANS_score", xlab = "MTRANS_score", ylab = "Activity_Index")
  if (sum(!is.na(mtrans_s) & !is.na(ai)) > 1) {
    abline(lm(ai ~ mtrans_s, na.action = na.exclude), col = "red", lwd = 2, lty = 2)
    text(0.05, max(ai, na.rm = TRUE) * 0.95, sprintf("r = %.3f", cor(mtrans_s, ai, use = "complete.obs")), pos = 4, font = 2)
  }
  dev.off()
  cat(sprintf("✓ Scatter AI vs componenti salvato: %s\n", png_3))

  # -------------------------------------------------------------------------
  # 4. Verifica formula: AI vs combinazione pesata (atteso r ≈ 1)
  # -------------------------------------------------------------------------
  ai_theoretical <- 0.5 * faf_n + 0.3 * (1 - tue_n) + 0.2 * mtrans_s
  ai_theoretical[is.na(faf_n) | is.na(tue_n) | is.na(mtrans_s)] <- NA
  cor_check <- cor(ai, ai_theoretical, use = "complete.obs")

  png_4 <- file.path(output_dir, "activity_index_formula_check.png")
  png(png_4, width = 800, height = 700, res = 150)
  par(mar = c(5, 5, 5, 2))
  plot(ai_theoretical, ai, pch = 19, col = rgb(0.36, 0.56, 0.64, 0.6),
       main = "Activity_Index vs formula teorica\n(atteso: r ≈ 1)",
       xlab = "0.5*FAF_n + 0.3*(1-TUE_n) + 0.2*MTRANS_score", ylab = "Activity_Index")
  abline(0, 1, col = "red", lty = 2, lwd = 2)
  text(0.05, max(ai, na.rm = TRUE) * 0.95, sprintf("Correlazione: %.4f", cor_check), pos = 4, font = 2)
  dev.off()
  cat(sprintf("✓ Verifica formula AI salvata: %s\n", png_4))

  # -------------------------------------------------------------------------
  # 5. AI vs BMI (se disponibile)
  # -------------------------------------------------------------------------
  if (include_bmi) {
    bmi <- data[["BMI"]]
    ai_bmi_cor <- cor(ai, bmi, use = "complete.obs")

    png_5 <- file.path(output_dir, "activity_index_vs_bmi.png")
    png(png_5, width = 1200, height = 600, res = 150)
    par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
    plot(ai, bmi, pch = 19, col = rgb(0.36, 0.56, 0.64, 0.6),
         main = sprintf("Activity_Index vs BMI (r = %.3f)", ai_bmi_cor),
         xlab = "Activity_Index", ylab = "BMI")
    if (sum(!is.na(ai) & !is.na(bmi)) > 1) {
      abline(lm(bmi ~ ai, na.action = na.exclude), col = "red", lwd = 2, lty = 2)
    }
    ai_breaks <- quantile(ai, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    ai_breaks <- unique(ai_breaks)
    if (length(ai_breaks) >= 2) {
      ai_q <- cut(ai, breaks = ai_breaks, include.lowest = TRUE,
                  labels = paste0("Q", seq_len(length(ai_breaks) - 1)))
      boxplot(bmi ~ ai_q, col = col_fill, border = col_border,
              main = "BMI per quartili Activity_Index", xlab = "Quartile AI", ylab = "BMI")
    }
    dev.off()
    cat(sprintf("✓ AI vs BMI salvato: %s\n", png_5))
  }

  # -------------------------------------------------------------------------
  # 6. Heatmap AI per FAF x TUE (media per cella)
  # -------------------------------------------------------------------------
  if ("FAF" %in% colnames(data) && "TUE" %in% colnames(data)) {
    ai_by_faf_tue <- tapply(ai, list(data[["FAF"]], data[["TUE"]]), mean, na.rm = TRUE)
    png_6 <- file.path(output_dir, "activity_index_heatmap_faf_tue.png")
    png(png_6, width = 800, height = 700, res = 150)
    par(mar = c(5, 5, 5, 4))
    image(0:3, 0:2, ai_by_faf_tue,
          main = "Media Activity_Index per FAF x TUE",
          xlab = "FAF (Frequenza attività fisica)", ylab = "TUE (Tempo dispositivi)",
          col = colorRampPalette(c("#F7F7F7", "#4292C6", "#2166AC"))(20))
    contour(0:3, 0:2, ai_by_faf_tue, add = TRUE, labcex = 0.8)
    dev.off()
    cat(sprintf("✓ Heatmap AI (FAF x TUE) salvata: %s\n", png_6))
  }

  # -------------------------------------------------------------------------
  # 7. Riepilogo statistico
  # -------------------------------------------------------------------------
  png_7 <- file.path(output_dir, "activity_index_summary.png")
  png(png_7, width = 1000, height = 600, res = 150)
  par(mar = c(2, 2, 5, 2))
  plot.new()
  stats_text <- sprintf(
    "Activity_Index - Riepilogo\n\nFormula: AI = 0.5*FAF_n + 0.3*(1-TUE_n) + 0.2*MTRANS_score\n\nn = %d\nMedia = %.4f\nSD = %.4f\nMin = %.4f\nMax = %.4f",
    n, mean(ai, na.rm = TRUE), sd(ai, na.rm = TRUE), min(ai, na.rm = TRUE), max(ai, na.rm = TRUE))
  if (include_bmi && exists("ai_bmi_cor")) {
    stats_text <- paste0(stats_text, sprintf("\n\nCorrelazione AI-BMI: %.4f", ai_bmi_cor))
  }
  text(0.5, 0.5, stats_text, cex = 1.1, font = 2)
  dev.off()
  cat(sprintf("✓ Riepilogo AI salvato: %s\n", png_7))

  cat(sprintf("\n✓ Analisi Activity_Index completata. Grafici in: %s\n", output_dir))
  invisible(list(output_dir = output_dir, n = n))
}

#' Legge un file CSV utilizzando impostazioni predefinite ottimizzate per il progetto SADProject.
#' Le impostazioni predefinite includono: header=TRUE, separatore virgola, stringsAsFactors=FALSE,
#' e check.names=FALSE per preservare i nomi delle colonne originali.
#'
#' @param file_path Stringa carattere. Percorso completo o relativo al file CSV da leggere.
#'   Il file deve esistere, altrimenti viene generato un errore.
#' @param ... Argomenti aggiuntivi passati a \code{\link{read.csv}}.
#'   Questi argomenti sovrascrivono le impostazioni predefinite se specificati.
#'
#' @return Data frame. Dataset letto dal file CSV con le impostazioni applicate.
read_csv_file <- function(file_path, ...) {
  if (!file.exists(file_path)) {
    stop(sprintf("File '%s' does not exist.", file_path))
  }
  
  # Argomenti predefiniti per read.csv
  default_args <- list(
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # Merge with user-provided arguments
  args <- modifyList(default_args, list(...))
  args$file <- file_path
  
  # Read the CSV file
  data <- do.call(read.csv, args)
  
  return(data)
}

#' Rimuove dal dataset tutte le righe che contengono almeno un valore mancante (NA o NULL).
#' Utilizza \code{\link{complete.cases}} per identificare le righe complete.
#' Questa funzione è utilizzata nella fase di controllo qualità dati del workflow principale.
#'
#' @param data Data frame. Dataset da pulire. Deve essere un data frame valido.
#'
#' @return Data frame. Dataset pulito contenente solo righe complete (senza valori mancanti).
#'   Il numero di righe rimosse viene stampato a console.
remove_missing_values <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  original_rows <- nrow(data)
  
  # Usa complete.cases() per identificare righe complete 
  complete_rows <- complete.cases(data)
  data_clean <- data[complete_rows, , drop = FALSE]
  
  removed_rows <- original_rows - nrow(data_clean)
  
  if (removed_rows > 0) {
    cat(sprintf("Rimosse %d righe con valori mancanti (da %d a %d righe)\n", 
                removed_rows, original_rows, nrow(data_clean)))
  } else {
    cat("Nessuna riga con valori mancanti trovata.\n")
  }
  
  return(data_clean)
}

#' Calcola l'Indice di Massa Corporea (BMI) a partire da peso e altezza.
#' La formula utilizzata è la standard: BMI = Weight (kg) / Height (m)².
#'
#' Il BMI viene aggiunto come nuova colonna al dataset. Se la colonna specificata
#' esiste già, viene sovrascritta con i nuovi valori calcolati.
#'
#' @param data Data frame. Dataset contenente le colonne Weight e Height.
#' @param weight_col Stringa carattere. Nome della colonna contenente il peso in chilogrammi.
#'   Predefinito: "Weight".
#' @param height_col Stringa carattere. Nome della colonna contenente l'altezza in metri.
#'   Predefinito: "Height".
#' @param bmi_col Stringa carattere. Nome della colonna da creare per il BMI calcolato.
#'   Predefinito: "BMI".
#'
#' @return Data frame. Il dataset originale con una nuova colonna aggiunta contenente
#'   i valori di BMI calcolati. Il range di BMI viene stampato a console.
calculate_bmi <- function(data,
                         weight_col = "Weight",
                         height_col = "Height",
                         bmi_col = "BMI") {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  if (!weight_col %in% colnames(data)) {
    stop(sprintf("Column '%s' not found in data.", weight_col))
  }
  
  if (!height_col %in% colnames(data)) {
    stop(sprintf("Column '%s' not found in data.", height_col))
  }
  
  # Calculate BMI: weight (kg) / height (m)^2
  data[[bmi_col]] <- data[[weight_col]] / (data[[height_col]]^2)
  
  cat(sprintf("Colonna '%s' creata: BMI = %s / %s^2\n", 
              bmi_col, weight_col, height_col))
  cat(sprintf("Range BMI: %.2f - %.2f\n", 
              min(data[[bmi_col]], na.rm = TRUE), 
              max(data[[bmi_col]], na.rm = TRUE)))
  
  return(data)
}

#' Analizza l'impatto dell'arrotondamento su variabili numeriche confrontando statistiche
#' descrittive e valori prima e dopo l'arrotondamento. Genera un report dettagliato che mostra
#' come le statistiche (media, mediana, SD, IQR, min, max) cambiano dopo l'arrotondamento.
#'
#' La funzione calcola le statistiche descrittive per ogni variabile prima e dopo l'arrotondamento,
#' mostra le differenze, e opzionalmente mostra esempi di valori modificati. Può anche generare
#' una tabella PNG con il confronto se richiesto.
#'
#' @param data Data frame. Dataset contenente le variabili numeriche da analizzare.
#' @param variables Vettore carattere. Nomi delle variabili numeriche da confrontare.
#'   Predefinito: c("AGE", "Weight", "Height").
#' @param digits Vettore numerico o NULL. Numero di cifre decimali per l'arrotondamento.
#'   Se NULL, usa valori predefiniti appropriati per ogni variabile (AGE=0, Weight=1, Height=2, BMI=2).
#'   Se un singolo valore, viene applicato a tutte le variabili.
#'   Se un vettore, deve avere la stessa lunghezza di variables.
#' @param show_examples Logico. Se TRUE, mostra esempi di valori modificati dall'arrotondamento.
#'   Predefinito: TRUE.
#' @param n_examples Numerico. Numero di esempi da mostrare per ogni variabile.
#'   Predefinito: 5.
#' @param output_dir Stringa carattere. Directory dove salvare le tabelle PNG di confronto.
#'   Se NULL, le tabelle PNG non vengono generate. Predefinito: NULL.
#' @param save_png Logico. Se TRUE, salva una tabella PNG con il confronto per ogni variabile.
#'   Richiede che gridExtra sia installato. Predefinito: FALSE.
#'
#' @return Lista invisibile con elementi:
#'   \item{comparisons}{Lista di data frame, uno per variabile, con statistiche prima/dopo/differenza}
#'   \item{output_dir}{Stringa carattere. Directory di output se save_png=TRUE}
compare_rounding <- function(data,
                             variables = c("AGE", "Weight", "Height"),
                             digits = NULL,
                             show_examples = TRUE,
                             n_examples = 5,
                             output_dir = "output/rounding_comparison",
                             save_png = TRUE) {
  
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  # Se non specificate, usa le variabili numeriche comuni
  if (is.null(variables)) {
    variables <- c("AGE", "Weight", "Height", "BMI")
  }
  
  # Filtra le variabili presenti nel dataset
  available_vars <- variables[variables %in% colnames(data)]
  
  # Filtra solo le variabili numeriche
  numeric_vars <- available_vars[sapply(data[, available_vars, drop = FALSE], is.numeric)]
  
  if (length(numeric_vars) == 0) {
    cat("Nessuna variabile numerica trovata per il confronto.\n")
    return(invisible(NULL))
  }
  
  # Crea la directory di output se necessario
  if (save_png) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
  }
  
  # Verifica se gridExtra è disponibile per le tabelle PNG
  if (save_png && !requireNamespace("gridExtra", quietly = TRUE)) {
    warning("Il pacchetto 'gridExtra' non è disponibile. Le tabelle PNG non verranno generate.")
    save_png <- FALSE
  }
  
  # Se non specificate le cifre decimali, usa valori di default appropriati
  if (is.null(digits)) {
    digits_list <- list(
      "AGE" = 0,
      "Weight" = 1,
      "Height" = 2,
      "BMI" = 2
    )
  } else if (length(digits) == 1) {
    digits_list <- setNames(rep(digits, length(numeric_vars)), numeric_vars)
  } else {
    if (length(digits) != length(numeric_vars)) {
      stop("Il numero di cifre decimali deve corrispondere al numero di variabili")
    }
    digits_list <- setNames(digits, numeric_vars)
  }
  
  cat("\n" , rep("=", 70), "\n", sep = "")
  cat("CONFRONTO VALORI PRIMA E DOPO ARROTONDAMENTO\n")
  cat(rep("=", 70), "\n\n", sep = "")
  
  # Crea un data frame per memorizzare le statistiche
  comparison_stats <- data.frame(
    Variable = character(),
    Digits = integer(),
    Values_Changed = integer(),
    Pct_Changed = numeric(),
    Mean_Before = numeric(),
    Mean_After = numeric(),
    SD_Before = numeric(),
    SD_After = numeric(),
    Min_Before = numeric(),
    Min_After = numeric(),
    Max_Before = numeric(),
    Max_After = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (var_name in numeric_vars) {
    var_data <- data[[var_name]]
    
    # Ottiene il numero di cifre decimali per questa variabile
    if (is.null(digits)) {
      n_digits <- ifelse(var_name %in% names(digits_list), 
                        digits_list[[var_name]], 
                        2)
    } else if (length(digits) == 1) {
      n_digits <- digits
    } else {
      n_digits <- digits_list[[var_name]]
    }
    
    # Calcola i valori arrotondati (senza modificare i dati originali)
    rounded_values <- round(var_data, digits = n_digits)
    
    # Conta i valori modificati
    n_changed <- sum(var_data != rounded_values, na.rm = TRUE)
    n_total <- sum(!is.na(var_data))
    pct_changed <- ifelse(n_total > 0, (n_changed / n_total) * 100, 0)
    
    # Calcola statistiche descrittive
    mean_before <- mean(var_data, na.rm = TRUE)
    mean_after <- mean(rounded_values, na.rm = TRUE)
    sd_before <- sd(var_data, na.rm = TRUE)
    sd_after <- sd(rounded_values, na.rm = TRUE)
    median_before <- median(var_data, na.rm = TRUE)
    median_after <- median(rounded_values, na.rm = TRUE)
    
    # Calcola quartili e IQR
    quantiles_before <- quantile(var_data, probs = c(0.25, 0.75), na.rm = TRUE)
    quantiles_after <- quantile(rounded_values, probs = c(0.25, 0.75), na.rm = TRUE)
    q1_before <- quantiles_before[1]
    q3_before <- quantiles_before[2]
    q1_after <- quantiles_after[1]
    q3_after <- quantiles_after[2]
    iqr_before <- q3_before - q1_before
    iqr_after <- q3_after - q1_after
    
    min_before <- min(var_data, na.rm = TRUE)
    min_after <- min(rounded_values, na.rm = TRUE)
    max_before <- max(var_data, na.rm = TRUE)
    max_after <- max(rounded_values, na.rm = TRUE)
    
    # Aggiunge le statistiche al data frame
    comparison_stats <- rbind(comparison_stats, data.frame(
      Variable = var_name,
      Digits = n_digits,
      Values_Changed = n_changed,
      Pct_Changed = round(pct_changed, 2),
      Mean_Before = round(mean_before, 6),
      Mean_After = round(mean_after, 6),
      SD_Before = round(sd_before, 6),
      SD_After = round(sd_after, 6),
      Median_Before = round(median_before, 6),
      Median_After = round(median_after, 6),
      IQR_Before = round(iqr_before, 6),
      IQR_After = round(iqr_after, 6),
      Min_Before = round(min_before, 6),
      Min_After = round(min_after, 6),
      Max_Before = round(max_before, 6),
      Max_After = round(max_after, 6),
      stringsAsFactors = FALSE
    ))
    
    # Genera la tabella di confronto per questa variabile
    cat(sprintf("\n%s\n", rep("=", 80)))
    cat(sprintf("Variabile: %s (arrotondamento a %d decimali)\n", var_name, n_digits))
    cat(sprintf("Valori modificati: %d su %d (%.2f%%)\n", n_changed, n_total, pct_changed))
    cat(sprintf("%s\n\n", rep("=", 80)))
    
    # Crea la tabella formattata
    stats_names <- c("Media", "Deviazione Standard", "Mediana", "IQR (Q3-Q1)", "Min", "Max")
    stats_before <- c(mean_before, sd_before, median_before, iqr_before, min_before, max_before)
    stats_after <- c(mean_after, sd_after, median_after, iqr_after, min_after, max_after)
    stats_diff <- stats_after - stats_before
    
    # Crea il data frame per la tabella
    comparison_table <- data.frame(
      Statistica = stats_names,
      Prima = sprintf("%.6f", stats_before),
      Dopo = sprintf("%.6f", stats_after),
      Differenza = sprintf("%.6f", stats_diff),
      stringsAsFactors = FALSE
    )
    
    # Stampa l'intestazione della tabella
    cat(sprintf("%-25s %20s %20s %20s\n", "Statistica", "Prima", "Dopo", "Differenza"))
    cat(sprintf("%s\n", rep("-", 85)))
    
    # Stampa ogni riga della tabella
    for (i in seq_along(stats_names)) {
      cat(sprintf("%-25s %20.6f %20.6f %20.6f\n", 
                  stats_names[i], 
                  stats_before[i], 
                  stats_after[i], 
                  stats_diff[i]))
    }
    
    cat(sprintf("%s\n", rep("-", 85)))
    
    # Genera la tabella PNG se richiesto
    if (save_png) {
      png_file <- file.path(output_dir, sprintf("%s_rounding_comparison.png", var_name))
      png(png_file, width = 1000, height = 700, res = 150)
      
      # Crea il titolo
      title_text <- sprintf("Confronto Arrotondamento: %s\n(Arrotondamento a %d decimali - %d valori modificati su %d, %.2f%%)",
                           var_name, n_digits, n_changed, n_total, pct_changed)
      
      # Crea la tabella usando gridExtra
      grid::grid.newpage()
      
      # Aggiunge il titolo in alto con margine
      grid::grid.text(title_text,
                     x = 0.5,
                     y = 0.92,
                     gp = grid::gpar(fontsize = 14, fontface = "bold"),
                     just = "top")
      
      # Crea un viewport per la tabella con margine superiore
      # La tabella viene posizionata più in basso per lasciare spazio sopra al titolo
      table_vp <- grid::viewport(x = 0.5, y = 0.40, width = 0.9, height = 0.65)
      grid::pushViewport(table_vp)
      
      gridExtra::grid.table(comparison_table,
                           theme = gridExtra::ttheme_default(
                             core = list(
                               fg_params = list(fontsize = 12),
                               bg_params = list(fill = c("white", "gray95"))
                             ),
                             colhead = list(
                               fg_params = list(fontsize = 13, fontface = "bold"),
                               bg_params = list(fill = "lightblue")
                             ),
                             rowhead = list(
                               fg_params = list(fontsize = 12, fontface = "bold")
                             )
                           ),
                           rows = NULL)
      
      grid::popViewport()
      
      dev.off()
      cat(sprintf("✓ Tabella PNG salvata: %s\n", png_file))
    }
    
    # Mostra esempi di valori modificati
    if (show_examples && n_changed > 0) {
      # Trova gli indici dei valori modificati
      changed_indices <- which(var_data != rounded_values & !is.na(var_data) & !is.na(rounded_values))
      
      if (length(changed_indices) > 0) {
        n_show <- min(n_examples, length(changed_indices))
        sample_indices <- sample(changed_indices, n_show)
        
        cat(sprintf("\n  Esempi di valori modificati (primi %d):\n", n_show))
        for (idx in sample_indices) {
          diff <- rounded_values[idx] - var_data[idx]
          cat(sprintf("    Riga %d: %.10f → %.10f (differenza: %+.10f)\n", 
                      idx, var_data[idx], rounded_values[idx], diff))
        }
      }
    }
    
    cat("\n")
  }
  
  # Riepilogo generale
  cat(rep("-", 70), "\n", sep = "")
  cat("RIEPILOGO GENERALE\n")
  cat(rep("-", 70), "\n", sep = "")
  cat(sprintf("Variabili analizzate: %d\n", length(numeric_vars)))
  cat(sprintf("Valori totali modificati: %d\n", sum(comparison_stats$Values_Changed)))
  cat(sprintf("Percentuale media di valori modificati: %.2f%%\n", 
              mean(comparison_stats$Pct_Changed)))
  cat("\n")
  
  # Restituisce il data frame con le statistiche
  return(invisible(comparison_stats))
}

#' Confronto tra dataset reale e sintetico: tabella, densita' e categoriche
#' Confronta un dataset reale con un dataset sintetico generato, analizzando sia variabili
#' numeriche che categoriche. Per le variabili numeriche, genera grafici di densità sovrapposti,
#' confronta statistiche descrittive (media, mediana, SD, quartili), e calcola distanze statistiche.
#' Per le variabili categoriche, confronta distribuzioni di frequenza con barplot sovrapposti.
#'
#' La funzione genera una serie completa di visualizzazioni per valutare la qualità del dataset
#' sintetico rispetto a quello reale. Ogni variabile viene analizzata separatamente con grafici
#' dedicati salvati nella directory di output.
#'
#' @param real_data Data frame. Dataset reale di riferimento per il confronto.
#' @param synthetic_data Data frame. Dataset sintetico da confrontare con quello reale.
#' @param output_dir Stringa carattere. Directory dove salvare i grafici generati.
#'   Predefinito: "output/real_vs_synthetic". La directory viene creata automaticamente se non esiste.
#' @param numeric_vars Vettore carattere. Nomi delle variabili numeriche da confrontare.
#'   Se character(0) o NULL, nessuna variabile numerica viene analizzata.
#'   Predefinito: c("Age", "Weight", "Height", "BMI", "Activity_Index", "Diet_Quality").
#' @param categorical_vars Vettore carattere o NULL. Nomi delle variabili categoriche da confrontare.
#'   Se NULL o character(0), nessuna variabile categorica viene analizzata.
#'   Predefinito: NULL.
#'
#' @return Lista invisibile con elementi:
#'   \item{summary_table}{Data frame con statistiche descrittive confrontate per variabili numeriche}
#'   \item{output_dir}{Stringa carattere. Percorso della directory di output}
#'   \item{numeric_vars_compared}{Vettore carattere. Variabili numeriche effettivamente confrontate}
#'   \item{categorical_vars_compared}{Vettore carattere. Variabili categoriche effettivamente confrontate}
compare_real_vs_synthetic <- function(real_data,
                                      synthetic_data,
                                      output_dir = "output/real_vs_synthetic",
                                      numeric_vars = c("Age", "Weight", "Height", "BMI",
                                                       "Activity_Index", "Diet_Quality"),
                                      categorical_vars = NULL) {
  if (!is.data.frame(real_data) || !is.data.frame(synthetic_data)) {
    stop("Input must be two data frames.")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Helper function: converte un vettore a numerico gestendo factors e character
  # Utile per normalizzare variabili categoriche codificate prima del calcolo
  to_numeric <- function(x) {
    if (is.factor(x)) return(as.numeric(as.character(x)))
    if (is.character(x)) return(as.numeric(x))
    as.numeric(x)
  }
  
  # Helper function: risolve il nome di colonna tra possibili varianti
  # Gestisce differenze di case e nomi alternativi (es. Age vs AGE)
  resolve_col <- function(df, desired) {
    if (desired %in% colnames(df)) return(desired)
    desired_lower <- tolower(desired)
    names_lower <- tolower(colnames(df))
    if (desired_lower %in% names_lower) {
      return(colnames(df)[match(desired_lower, names_lower)])
    }
    if (desired == "Diet_Quality" && "Diet_Quality_Index" %in% colnames(df)) {
      return("Diet_Quality_Index")
    }
    if (desired == "Diet_Quality_Index" && "Diet_Quality" %in% colnames(df)) {
      return("Diet_Quality")
    }
    NA_character_
  }

  # -------------------------------------------------------------------------
  # 1) Confronto descrittivo (tabella)
  # -------------------------------------------------------------------------
  available_num <- character(0)
  missing_num <- character(0)
  for (var in numeric_vars) {
    real_col <- resolve_col(real_data, var)
    syn_col <- resolve_col(synthetic_data, var)
    if (is.na(real_col) || is.na(syn_col)) {
      missing_num <- c(missing_num, var)
    } else {
      available_num <- c(available_num, var)
    }
  }
  if (length(missing_num) > 0) {
    cat(sprintf("⚠️  Variabili numeriche mancanti: %s\n", paste(missing_num, collapse = ", ")))
  }

  summary_table <- data.frame(
    Feature = character(0),
    Real_Mean = numeric(0),
    Syn_Mean = numeric(0),
    Real_SD = numeric(0),
    Syn_SD = numeric(0),
    Delta_Pct = numeric(0),
    stringsAsFactors = FALSE
  )

  for (var in available_num) {
    real_col <- resolve_col(real_data, var)
    syn_col <- resolve_col(synthetic_data, var)
    real_vals <- to_numeric(real_data[[real_col]])
    syn_vals <- to_numeric(synthetic_data[[syn_col]])

    real_mean <- mean(real_vals, na.rm = TRUE)
    syn_mean <- mean(syn_vals, na.rm = TRUE)
    real_sd <- sd(real_vals, na.rm = TRUE)
    syn_sd <- sd(syn_vals, na.rm = TRUE)
    delta_pct <- if (is.na(real_mean) || real_mean == 0) NA else ((syn_mean - real_mean) / real_mean) * 100

    summary_table <- rbind(summary_table, data.frame(
      Feature = var,
      Real_Mean = round(real_mean, 4),
      Syn_Mean = round(syn_mean, 4),
      Real_SD = round(real_sd, 4),
      Syn_SD = round(syn_sd, 4),
      Delta_Pct = round(delta_pct, 2),
      stringsAsFactors = FALSE
    ))
  }

  # La tabella summary viene calcolata ma non salvata (rimossa su richiesta)
  # summary_table contiene le statistiche descrittive per le variabili numeriche

  # -------------------------------------------------------------------------
  # 2) Confronto distribuzioni (KDE sovrapposte)
  # -------------------------------------------------------------------------
  col_real <- "#1F77B4"
  col_syn <- "#E67E22"
  density_paths <- list()

  for (var in available_num) {
    real_col <- resolve_col(real_data, var)
    syn_col <- resolve_col(synthetic_data, var)
    real_vals <- to_numeric(real_data[[real_col]])
    syn_vals <- to_numeric(synthetic_data[[syn_col]])
    real_vals <- real_vals[!is.na(real_vals)]
    syn_vals <- syn_vals[!is.na(syn_vals)]

    if (length(real_vals) < 2 || length(syn_vals) < 2) next

    dens_real <- density(real_vals)
    dens_syn <- density(syn_vals)

    png_file <- file.path(output_dir, sprintf("density_compare_%s.png", var))
    png(png_file, width = 1000, height = 650, res = 150)
    par(mar = c(5, 5, 4, 2))
    plot(dens_real, col = col_real, lwd = 2,
      main = sprintf("Confronto Densita' - %s", var),
      xlab = var, ylab = "Densita'")
    lines(dens_syn, col = col_syn, lwd = 2)
    legend("topright",
           legend = c("Reale", "Sintetico"),
           col = c(col_real, col_syn),
           lwd = 2, bty = "n")
    dev.off()
    density_paths[[var]] <- png_file
    cat(sprintf("✓ Densita' sovrapposta salvata: %s\n", png_file))
  }

  # -------------------------------------------------------------------------
  # 3) Confronto categoriche (barplot affiancati)
  # -------------------------------------------------------------------------
  # Analizza solo le variabili categoriche specificate in input
  # Se categorical_vars = NULL, non analizza nessuna variabile categorica
  available_cat <- character(0)
  missing_cat <- character(0)
  
  if (!is.null(categorical_vars) && length(categorical_vars) > 0) {
    for (var in categorical_vars) {
      real_col <- resolve_col(real_data, var)
      syn_col <- resolve_col(synthetic_data, var)
      if (is.na(real_col) || is.na(syn_col)) {
        missing_cat <- c(missing_cat, var)
      } else {
        available_cat <- c(available_cat, var)
      }
    }
    
    if (length(missing_cat) > 0) {
      cat(sprintf("⚠️  Variabili categoriche mancanti: %s\n", paste(missing_cat, collapse = ", ")))
    }
  }
  
  categorical_vars <- available_cat

  barplot_paths <- list()
  if (length(categorical_vars) == 0) {
    cat("ℹ️  Nessuna variabile categorica specificata per il confronto.\n")
  } else {
    for (var in categorical_vars) {
      real_col <- resolve_col(real_data, var)
      syn_col <- resolve_col(synthetic_data, var)
      real_vals <- real_data[[real_col]]
      syn_vals <- synthetic_data[[syn_col]]
      if (is.factor(real_vals)) real_vals <- as.character(real_vals)
      if (is.factor(syn_vals)) syn_vals <- as.character(syn_vals)

      levels_all <- sort(unique(c(real_vals, syn_vals)))
      real_tab <- table(factor(real_vals, levels = levels_all))
      syn_tab <- table(factor(syn_vals, levels = levels_all))

      real_pct <- (real_tab / sum(real_tab)) * 100
      syn_pct <- (syn_tab / sum(syn_tab)) * 100

      pct_matrix <- rbind(as.numeric(real_pct), as.numeric(syn_pct))
      rownames(pct_matrix) <- c("Reale", "Sintetico")
      colnames(pct_matrix) <- levels_all

      png_file <- file.path(output_dir, sprintf("categorical_compare_%s.png", var))
      png(png_file, width = 1100, height = 650, res = 150)
      par(mar = c(7, 5, 4, 2))
      barplot(pct_matrix, beside = TRUE,
              col = c(col_real, col_syn),
              ylim = c(0, max(pct_matrix, na.rm = TRUE) * 1.2),
              ylab = "Percentuale",
              main = sprintf("Confronto categorie - %s", var))
      legend("topright", legend = c("Reale", "Sintetico"),
             fill = c(col_real, col_syn), bty = "n")
      dev.off()
      barplot_paths[[var]] <- png_file
      cat(sprintf("✓ Barplot categorico salvato: %s\n", png_file))
    }
  }

  return(invisible(list(
    summary_table = summary_table,
    density_paths = density_paths,
    barplot_paths = barplot_paths
  )))
}