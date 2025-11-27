# SADProject - Distribution Analysis Functions
# Functions for visualizing distributions of all variables

#' Plot distribution for a numeric variable
#'
#' @param data Data frame
#' @param col_name Character string. Column name
#' @param output_dir Character string. Output directory for plots
#' @return Character string. Path to saved plot
plot_numeric_distribution <- function(data, col_name, output_dir = "output/distributions") {
  if (!col_name %in% colnames(data)) {
    stop(sprintf("Column '%s' not found in data.", col_name))
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  col_data <- data[[col_name]]
  
  # Remove NA values for plotting
  col_data_clean <- col_data[!is.na(col_data)]
  
  if (length(col_data_clean) == 0) {
    warning(sprintf("No valid data for column '%s'. Skipping plot.", col_name))
    return(NULL)
  }
  
  # Source data dictionary if not already available
  if (!exists("get_column_description", envir = .GlobalEnv)) {
    source("R/data_dictionary.R")
  }
  
  # Get column description
  col_description <- get_column_description(col_name)
  if (is.null(col_description)) {
    col_description <- col_name  # Fallback to column name if no description
  }
  
  # Create combined plot: histogram + density + boxplot
  plot_path <- file.path(output_dir, sprintf("%s_distribution.png", col_name))
  png(plot_path, width = 1200, height = 800)
  
  # Set up 2x2 layout
  par(mfrow = c(2, 2), mar = c(4, 4, 4, 2))
  
  # 1. Histogram
  hist(col_data_clean,
       main = sprintf("Istogramma: %s\n%s", col_name, col_description),
       xlab = col_name,
       ylab = "Frequenza",
       col = "steelblue",
       border = "white",
       breaks = "Sturges",
       cex.main = 0.9)
  
  # 2. Density plot
  dens <- density(col_data_clean)
  plot(dens,
       main = sprintf("Densità: %s\n%s", col_name, col_description),
       xlab = col_name,
       ylab = "Densità",
       col = "darkblue",
       lwd = 2,
       cex.main = 0.9)
  polygon(dens, col = rgb(0.2, 0.4, 0.8, 0.3), border = "darkblue")
  
  # 3. Boxplot
  boxplot(col_data_clean,
          main = sprintf("Boxplot: %s\n%s", col_name, col_description),
          ylab = col_name,
          col = "lightblue",
          border = "darkblue",
          horizontal = TRUE,
          cex.main = 0.9)
  
  # 4. Summary statistics text
  plot.new()
  par(mar = c(0, 0, 0, 0))
  stats_text <- c(
    sprintf("Statistiche descrittive: %s", col_name),
    sprintf("Descrizione: %s", col_description),
    "",
    sprintf("Media: %.3f", mean(col_data_clean)),
    sprintf("Mediana: %.3f", median(col_data_clean)),
    sprintf("Deviazione Standard: %.3f", sd(col_data_clean)),
    sprintf("Min: %.3f", min(col_data_clean)),
    sprintf("Max: %.3f", max(col_data_clean))
  )
  text(0.5, 0.5, paste(stats_text, collapse = "\n"),
       cex = 1.2, family = "mono", adj = c(0.5, 0.5))
  
  dev.off()
  
  return(plot_path)
}

#' Plot distribution for a categorical variable
#'
#' @param data Data frame
#' @param col_name Character string. Column name
#' @param output_dir Character string. Output directory for plots
#' @return Character string. Path to saved plot
plot_categorical_distribution <- function(data, col_name, output_dir = "output/distributions") {
  if (!col_name %in% colnames(data)) {
    stop(sprintf("Column '%s' not found in data.", col_name))
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  col_data <- data[[col_name]]
  
  # Remove NA values for counting
  col_data_clean <- col_data[!is.na(col_data)]
  
  if (length(col_data_clean) == 0) {
    warning(sprintf("No valid data for column '%s'. Skipping plot.", col_name))
    return(NULL)
  }
  
  # Count frequencies
  freq_table <- table(col_data_clean)
  freq_df <- data.frame(
    Value = names(freq_table),
    Count = as.numeric(freq_table),
    stringsAsFactors = FALSE
  )
  freq_df$Percentage <- (freq_df$Count / length(col_data_clean)) * 100
  
  # Source data dictionary if not already available
  if (!exists("get_categorical_labels", envir = .GlobalEnv)) {
    source("R/data_dictionary.R")
  }
  
  # Get column description
  col_description <- get_column_description(col_name)
  if (is.null(col_description)) {
    col_description <- col_name  # Fallback to column name if no description
  }
  
  # Get labels for this column if available
  labels <- get_categorical_labels()
  if (col_name %in% names(labels)) {
    # Map numeric values to their labels
    value_labels <- labels[[col_name]]
    freq_df$Label <- sapply(freq_df$Value, function(v) {
      if (as.character(v) %in% names(value_labels)) {
        return(value_labels[[as.character(v)]])
      }
      return(as.character(v))
    })
    # Create combined label: "Valore (Etichetta)" or just "Etichetta"
    freq_df$DisplayLabel <- paste0(freq_df$Value, " = ", freq_df$Label)
  } else {
    freq_df$Label <- freq_df$Value
    freq_df$DisplayLabel <- as.character(freq_df$Value)
  }
  
  freq_df <- freq_df[order(-freq_df$Count), ]  # Sort by frequency
  
  # Create combined plot: barplot + pie chart + frequency table
  plot_path <- file.path(output_dir, sprintf("%s_distribution.png", col_name))
  png(plot_path, width = 1200, height = 800)
  
  # Set up 2x2 layout
  par(mfrow = c(2, 2), mar = c(4, 4, 4, 2))
  
  # 1. Barplot (horizontal) with labels
  barplot(freq_df$Count,
          names.arg = freq_df$DisplayLabel,
          main = sprintf("Distribuzione: %s\n%s", col_name, col_description),
          xlab = "Frequenza",
          ylab = "Valore",
          col = "steelblue",
          border = "white",
          horiz = TRUE,
          las = 1,
          cex.names = 0.7,
          cex.main = 0.9)
  
  # 2. Barplot (vertical) with percentages and labels
  barplot(freq_df$Percentage,
          names.arg = freq_df$DisplayLabel,
          main = sprintf("Percentuali: %s\n%s", col_name, col_description),
          xlab = "Valore",
          ylab = "Percentuale (%)",
          col = "lightblue",
          border = "darkblue",
          las = 2,
          cex.names = 0.6,
          cex.main = 0.9)
  
  # 3. Pie chart with labels
  pie_labels <- paste0(freq_df$DisplayLabel, "\n(", sprintf("%.1f%%", freq_df$Percentage), ")")
  pie(freq_df$Count,
      labels = pie_labels,
      main = sprintf("Distribuzione percentuale: %s\n%s", col_name, col_description),
      col = rainbow(nrow(freq_df)),
      cex = 0.7,
      cex.main = 0.9)
  
  # 4. Summary statistics text
  plot.new()
  par(mar = c(0, 0, 0, 0))
  # Build statistics text with labels
  stats_lines <- c(
    sprintf("Statistiche: %s", col_name),
    sprintf("Descrizione: %s", col_description),
    "",
    sprintf("Numero di categorie: %d", nrow(freq_df)),
    sprintf("Valore più frequente: %s (%.1f%%)", 
            freq_df$DisplayLabel[1], freq_df$Percentage[1]),
    sprintf("Valore meno frequente: %s (%.1f%%)", 
            freq_df$DisplayLabel[nrow(freq_df)], freq_df$Percentage[nrow(freq_df)]),
    "",
    "Frequenze:"
  )
  
  # Add frequency lines with labels
  freq_lines <- sapply(seq_len(nrow(freq_df)), function(i) {
    sprintf("  %s: %d (%.1f%%)", 
            freq_df$DisplayLabel[i], freq_df$Count[i], freq_df$Percentage[i])
  })
  
  stats_text <- c(
    stats_lines,
    freq_lines,
    "",
    sprintf("Valori mancanti: %d (%.1f%%)", 
            sum(is.na(col_data)), 
            (sum(is.na(col_data)) / length(col_data)) * 100)
  )
  text(0.5, 0.5, paste(stats_text, collapse = "\n"),
       cex = 1.0, family = "mono", adj = c(0.5, 0.5))
  
  dev.off()
  
  return(plot_path)
}

#' Plot distributions for all variables in the dataset
#'
#' @param data Data frame
#' @param output_dir Character string. Output directory for plots
#' @param exclude_cols Character vector. Column names to exclude from analysis
#' @return List. Paths to all saved plots
plot_all_distributions <- function(data, 
                                    output_dir = "output/distributions",
                                    exclude_cols = NULL) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Get all column names
  all_cols <- colnames(data)
  
  # Automatically exclude technical/metadata columns and target variable
  technical_cols <- c("anomaly_score", "is_outlier", "class")
  all_cols <- setdiff(all_cols, technical_cols)
  
  # Exclude specified columns
  if (!is.null(exclude_cols)) {
    all_cols <- setdiff(all_cols, exclude_cols)
  }
  
  if (length(all_cols) == 0) {
    stop("No columns to plot.")
  }
  
  # Separate numeric and categorical columns
  # A column is considered categorical if:
  # 1. It's not numeric, OR
  # 2. It's numeric but has <= 10 unique values (likely encoded categorical)
  numeric_cols <- character()
  categorical_cols <- character()
  
  for (col in all_cols) {
    col_data <- data[[col]]
    unique_count <- length(unique(col_data[!is.na(col_data)]))
    
    if (is.numeric(col_data)) {
      # If numeric with few unique values, treat as categorical
      if (unique_count <= 10 && unique_count < nrow(data) * 0.1) {
        categorical_cols <- c(categorical_cols, col)
      } else {
        numeric_cols <- c(numeric_cols, col)
      }
    } else {
      categorical_cols <- c(categorical_cols, col)
    }
  }
  
  cat(sprintf("\n=== Generazione distribuzioni ===\n"))
  cat(sprintf("Colonne numeriche: %d\n", length(numeric_cols)))
  cat(sprintf("Colonne categoriche: %d\n", length(categorical_cols)))
  cat(sprintf("Directory output: %s\n\n", output_dir))
  
  plot_paths <- list()
  
  # Plot numeric distributions
  if (length(numeric_cols) > 0) {
    cat("Generazione plot per variabili numeriche...\n")
    for (col_name in numeric_cols) {
      tryCatch({
        path <- plot_numeric_distribution(data, col_name, output_dir)
        if (!is.null(path)) {
          plot_paths[[col_name]] <- path
          cat(sprintf("  ✓ %s\n", col_name))
        }
      }, error = function(e) {
        warning(sprintf("Errore nel plot di '%s': %s", col_name, e$message))
      })
    }
  }
  
  # Plot categorical distributions
  if (length(categorical_cols) > 0) {
    cat("\nGenerazione plot per variabili categoriche...\n")
    for (col_name in categorical_cols) {
      tryCatch({
        path <- plot_categorical_distribution(data, col_name, output_dir)
        if (!is.null(path)) {
          plot_paths[[col_name]] <- path
          cat(sprintf("  ✓ %s\n", col_name))
        }
      }, error = function(e) {
        warning(sprintf("Errore nel plot di '%s': %s", col_name, e$message))
      })
    }
  }
  
  cat(sprintf("\n=== Completato: %d plot generati ===\n", length(plot_paths)))
  
  return(invisible(plot_paths))
}

