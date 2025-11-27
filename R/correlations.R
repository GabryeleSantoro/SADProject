check_correlations <- function(data,
                               threshold = 0.5,
                               method = "pearson",
                               use = "pairwise.complete.obs",
                               plot_pairs = FALSE,
                               plot_dir = "output/correlations",
                               ordinal_vars = NULL) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) < 2) {
    stop("At least two numeric columns are required to compute correlations.")
  }
  
  numeric_data <- data[, numeric_cols, drop = FALSE]
  
  # Se non specificato, identifica automaticamente variabili categoriche numeriche
  # (binarie e ordinali) dal data dictionary
  if (is.null(ordinal_vars)) {
    # Usa il data dictionary per identificare tutte le variabili categoriche
    # che sono numeriche (binarie 0/1 e ordinali)
    if (exists("get_categorical_columns", mode = "function")) {
      categorical_cols <- get_categorical_columns()
      # Filtra solo quelle che sono effettivamente numeriche nel dataset
      ordinal_vars <- categorical_cols[categorical_cols %in% colnames(numeric_data)]
    } else {
      # Fallback: lista completa di variabili categoriche numeriche
      ordinal_vars <- c("Gender", "family_history_with_overweight", "FAVC", "SMOKE", "SCC",
                        "FCVC", "NCP", "CAEC", "CH2O", "FAF", "TUE", "CALC", "MTRANS", "class")
      # Filtra solo quelle presenti nel dataset
      ordinal_vars <- ordinal_vars[ordinal_vars %in% colnames(numeric_data)]
    }
  }
  
  # Determina il metodo di correlazione appropriato per ogni coppia
  corr_matrix <- matrix(NA, 
                       nrow = ncol(numeric_data), 
                       ncol = ncol(numeric_data),
                       dimnames = list(colnames(numeric_data), colnames(numeric_data)))
  
  for (i in seq_len(ncol(numeric_data))) {
    for (j in seq_len(ncol(numeric_data))) {
      if (i == j) {
        corr_matrix[i, j] <- 1
        next
      }
      
      var1 <- colnames(numeric_data)[i]
      var2 <- colnames(numeric_data)[j]
      
      # Determina se una delle variabili è ordinale
      is_ordinal1 <- var1 %in% ordinal_vars
      is_ordinal2 <- var2 %in% ordinal_vars
      
      # Scegli il metodo appropriato
      if (is_ordinal1 || is_ordinal2) {
        # Usa Spearman per variabili ordinali
        corr_method <- "spearman"
      } else {
        # Usa il metodo specificato (default: pearson) per variabili continue
        corr_method <- method
      }
      
      # Calcola la correlazione
      corr_value <- cor(numeric_data[[var1]], 
                       numeric_data[[var2]], 
                       use = use, 
                       method = corr_method)
      corr_matrix[i, j] <- corr_value
    }
  }
  
  cat(sprintf("\nAnalisi correlazioni (metodo adattivo, threshold = %.2f)\n", threshold))
  cat(sprintf("Nota: Spearman usato per variabili categoriche/ordinali, %s per variabili continue\n", method))
  cat(sprintf("Variabili categoriche identificate: %s\n", paste(ordinal_vars, collapse = ", ")))
  
  cov_matrix <- cov(numeric_data, use = use, method = method)
  
  if (plot_pairs) {
    if (!dir.exists(plot_dir)) {
      dir.create(plot_dir, recursive = TRUE)
    }
  }
  plotted_pairs <- character()
  
  for (col_name in colnames(corr_matrix)) {
    other_cols <- setdiff(colnames(corr_matrix), col_name)
    correlations <- corr_matrix[col_name, other_cols]
    strong_corr <- correlations[abs(correlations) >= threshold]
    
    if (length(strong_corr) > 0) {
      cat(sprintf("\nColonna: %s\n", col_name))
      for (other in names(strong_corr)) {
        if (is.na(other) || other == "") {
          next
        }
        
        # Determina il metodo usato per questa coppia
        is_ordinal1 <- col_name %in% ordinal_vars
        is_ordinal2 <- other %in% ordinal_vars
        used_method <- if (is_ordinal1 || is_ordinal2) "spearman" else method
        
        if (!is.null(cov_matrix) &&
            col_name %in% rownames(cov_matrix) &&
            other %in% colnames(cov_matrix)) {
          cov_value <- cov_matrix[col_name, other]
        } else {
          cov_value <- NA
        }
        cat(sprintf("  - %s: corr = %.3f (%s), cov = %s\n",
                    other,
                    strong_corr[other],
                    used_method,
                    if (is.na(cov_value)) "NA" else sprintf("%.3f", cov_value)))
        
        if (plot_pairs) {
          pair_key <- paste(sort(c(col_name, other)), collapse = "__")
          if (!(pair_key %in% plotted_pairs)) {
            plotted_pairs <- c(plotted_pairs, pair_key)
            plot_path <- file.path(plot_dir,
                                   sprintf("%s_vs_%s.png", col_name, other))
            png(plot_path, width = 900, height = 700)
            
            # Per variabili ordinali, usa boxplot o violin plot
            if (is_ordinal1 || is_ordinal2) {
              ordinal_var <- if (is_ordinal1) col_name else other
              continuous_var <- if (is_ordinal1) other else col_name
              
              # Boxplot: variabile continua per livello ordinale
              boxplot(numeric_data[[continuous_var]] ~ numeric_data[[ordinal_var]],
                     main = sprintf("%s vs %s\ncorr (Spearman) = %.3f",
                                   continuous_var, ordinal_var,
                                   strong_corr[other]),
                     xlab = ordinal_var,
                     ylab = continuous_var,
                     col = rgb(0.2, 0.4, 0.8, 0.6))
            } else {
              # Scatter plot per variabili continue
              plot(numeric_data[[col_name]],
                   numeric_data[[other]],
                   main = sprintf("Scatter %s vs %s\ncorr (%s) = %.3f, cov = %s",
                                 col_name,
                                 other,
                                 method,
                                 strong_corr[other],
                                 if (is.na(cov_value)) "NA" else sprintf("%.3f", cov_value)),
                   xlab = col_name,
                   ylab = other,
                   pch = 19,
                   col = rgb(0.2, 0.4, 0.8, 0.6))
              if (!all(is.na(numeric_data[[col_name]])) &&
                  !all(is.na(numeric_data[[other]]))) {
                model <- tryCatch(lm(numeric_data[[other]] ~ numeric_data[[col_name]]),
                                error = function(e) NULL)
                if (!is.null(model)) {
                  abline(model, col = "red", lwd = 2)
                }
              }
            }
            dev.off()
            cat(sprintf("    -> Plot salvato: %s\n", plot_path))
          }
        }
      }
    }
  }
  
  invisible(corr_matrix)
}
