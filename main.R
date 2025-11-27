# SADProject - Main Script
# Statistics and Data Analysis Project
# This is the main entry point for the project

# Source utility functions
source("R/functions.R")
source("R/packages.R")
source("R/data_dictionary.R")  # Load data dictionary first (used by other modules)
source("R/correlations.R")
source("R/anomalies.R")
source("R/distributions.R")

# Set project paths
data_dir <- "data"
output_dir <- "output"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

data <- read_csv_file(file.path(data_dir, "Obesity_onevsrest_4_min_labeled.csv"))

cat("\n=== SADProject - Inizializzazione completata ===\n")

cat("\n=== SADProject - Controllo anomalie ===\n")
data = check_domain_anomalies(data)
cat("\n=== SADProject - Controllo anomalie completato ===\n")

cat("\n=== SADProject - Rilevamento outlier statistici ===\n")
data = detect_statistical_outliers(data)
cat("\n=== SADProject - Rilevamento outlier statistici completato ===\n")

cat("\n=== SADProject - Pulizia dati ===\n")
data = clean_dataset(data)
cat("\n=== SADProject - Pulizia dati completata ===\n")

cat("\n=== SADProject - Calcolo BMI ===\n")
data = calculate_bmi(data)
cat("\n=== SADProject - Calcolo BMI completato ===\n")

cat("\n=== SADProject - Analisi distribuzioni ===\n")
plot_all_distributions(data)
cat("\n=== SADProject - Analisi distribuzioni completata ===\n")

cat("\n=== SADProject - Controllo correlazioni ===\n")
check_correlations(data, plot_pairs = TRUE)
cat("\n=== SADProject - Controllo correlazioni completato ===\n")

