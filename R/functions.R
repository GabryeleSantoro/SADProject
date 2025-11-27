# SADProject - Utility Functions
# Functions for reading and processing CSV files

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("Colonna", "Mancanti", "Percentuale"))
}

#' Read a single CSV file
#'
#' @param file_path Character string. Full path to the CSV file
#' @param ... Additional arguments passed to read.csv()
#' @return Data frame containing the CSV data
#' @examples
#' data <- read_csv_file("data/Obesity_onevsrest_4_min.csv")
read_csv_file <- function(file_path, ...) {
  if (!file.exists(file_path)) {
    stop(sprintf("File '%s' does not exist.", file_path))
  }
  
  # Default arguments for read.csv
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

# ============================================================================
# DATA CLEANING FUNCTIONS - Missing Values
# ============================================================================

#' Visualize missing values pattern
#'
#' @param data Data frame
#' @param plot_type Type of plot: "pattern", "matrix", or "bar"
#' @return ggplot object (if ggplot2 is available)
#' @examples
#' plot_missing_values(data, plot_type = "bar")
plot_missing_values <- function(data, plot_type = "bar") {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("ggplot2 non disponibile. Installare con: install.packages('ggplot2')")
    return(NULL)
  }
  
  missing_count <- colSums(is.na(data))
  missing_pct <- (missing_count / nrow(data)) * 100
  
  if (plot_type == "bar") {
    missing_df <- data.frame(
      Colonna = names(missing_count),
      Mancanti = missing_count,
      Percentuale = missing_pct,
      stringsAsFactors = FALSE
    )
    missing_df <- missing_df[missing_df$Mancanti > 0, ]
    
    if (nrow(missing_df) == 0) {
      cat("Nessun valore mancante da visualizzare.\n")
      return(NULL)
    }
    
    missing_df$Colonna <- factor(
      missing_df$Colonna,
      levels = missing_df$Colonna[order(-missing_df$Mancanti)]
    )
    
    p <- ggplot2::ggplot(missing_df, ggplot2::aes_string(x = "Colonna", y = "Mancanti")) +
      ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
      ggplot2::labs(title = "Valori Mancanti per Colonna",
                    x = "Colonna", y = "Numero di Valori Mancanti") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    
    return(p)
  } else if (plot_type == "pattern") {
    if (requireNamespace("VIM", quietly = TRUE)) {
      return(VIM::aggr(data, col = c('navyblue', 'red'), 
                       numbers = TRUE, sortVars = TRUE,
                       labels = names(data), cex.axis = .7,
                       gap = 3, ylab = c("Pattern di Valori Mancanti", "Frequenza")))
    } else {
      warning("VIM non disponibile per il pattern plot.")
      return(NULL)
    }
  } else {
    stop("plot_type deve essere 'bar', 'pattern', o 'matrix'")
  }
}

#' Remove rows with missing values (NULL/NA)
#'
#' @param data Data frame
#' @return Cleaned data frame with only complete rows
#' @examples
#' clean_data <- remove_missing_values(data)
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

# ============================================================================
# COMPREHENSIVE DATA CLEANING FUNCTION
# ============================================================================

#' Comprehensive data cleaning pipeline (missing values only)
#'
#' @param data Data frame
#' @param print_summary If TRUE, prints cleaning summary
#' @return Cleaned data frame (rows with missing values removed)
#' @examples
#' clean_data <- clean_dataset(data)
clean_dataset <- function(data,
                          print_summary = TRUE) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  original_rows <- nrow(data)
  original_cols <- ncol(data)
  
  if (print_summary) {
    cat("\n=== Pipeline di Pulizia Dati ===\n")
    cat(sprintf("Dati originali: %d righe, %d colonne\n", original_rows, original_cols))
  }
  
  data <- remove_missing_values(data)
  
  final_rows <- nrow(data)
  if (print_summary) {
    cat(sprintf("\nDati finali: %d righe, %d colonne\n", final_rows, ncol(data)))
    if (original_rows > 0) {
      cat(sprintf("Righe rimosse: %d (%.2f%%)\n", 
                  original_rows - final_rows,
                  ((original_rows - final_rows) / original_rows) * 100))
    }
    cat("=== Pulizia Completata ===\n\n")
  }
  
  return(data)
}

# ============================================================================
# FEATURE ENGINEERING FUNCTIONS
# ============================================================================

#' Calculate BMI (Body Mass Index) from Weight and Height
#'
#' @param data Data frame containing Weight and Height columns
#' @param weight_col Character string. Name of the weight column (default: "Weight")
#' @param height_col Character string. Name of the height column (default: "Height")
#' @param bmi_col Character string. Name of the BMI column to create (default: "bmi")
#' @return Data frame with added BMI column
#' @examples
#' data <- calculate_bmi(data)
calculate_bmi <- function(data,
                         weight_col = "Weight",
                         height_col = "Height",
                         bmi_col = "bmi") {
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

