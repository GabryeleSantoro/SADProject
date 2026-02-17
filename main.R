# SADProject - Main Script
# Statistics and Data Analysis Project
# This is the main entry point for the project

# =============================================================================
# SETUP INIZIALE
# =============================================================================

# Source utility functions
source("R/data_dictionary.R")  # Carica prima il data dictionary
source("R/functions.R")
source("R/packages.R")
source("R/anomalies.R")
source("R/analysis.R")
source("R/distributions.R")
source("R/rounding.R")

# Set project paths
data_dir <- "data"
output_dir <- "output"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Caricamento dataset
data <- read_csv_file(
  file.path(data_dir, "Obesity_onevsrest_4_min_labeled.csv")
)
syn_data <- read_csv_file(
  file.path(data_dir, "syntetic_obesity_data.csv")
)

cat("\n", rep("=", 80), "\n", sep = "")
cat("SADProject - Analisi Dataset Obesity\n")
cat(rep("=", 80), "\n\n", sep = "")
cat(sprintf("Dataset caricato: %d righe, %d colonne\n", nrow(data), ncol(data)))
cat(sprintf("Dataset sintetico caricato: %d righe, %d colonne\n\n", nrow(syn_data), ncol(syn_data)))

# =============================================================================
# FASE 1: CONTROLLO QUALITÀ DATI
# =============================================================================
# - Controllo valori mancanti
# - Analisi sull'arrotondamento
# - Controllo outlier (con dataset arrotondato)
# - Rimozione duplicati

cat("\n", rep("=", 80), "\n", sep = "")
cat("FASE 1: CONTROLLO QUALITÀ DATI\n")
cat(rep("=", 80), "\n\n", sep = "")

# Salva il numero iniziale di righe per il riepilogo finale
n_rows_initial <- nrow(data)

# -----------------------------------------------------------------------------
# 1.1 Controllo valori mancanti
# -----------------------------------------------------------------------------
cat("1.1 CONTROLLO VALORI MANCANTI\n")
cat(rep("-", 80), "\n", sep = "")

# Crea una copia per l'analisi
data_missing_check <- data

# Conta valori mancanti per colonna
missing_summary <- data.frame(
  Variable = character(),
  Missing_Count = integer(),
  Missing_Percent = numeric(),
  stringsAsFactors = FALSE
)

for (col_name in colnames(data_missing_check)) {
  col_data <- data_missing_check[[col_name]]
  n_missing <- sum(is.na(col_data))
  n_total <- length(col_data)
  pct_missing <- (n_missing / n_total) * 100
  
  missing_summary <- rbind(missing_summary, data.frame(
    Variable = col_name,
    Missing_Count = n_missing,
    Missing_Percent = round(pct_missing, 2),
    stringsAsFactors = FALSE
  ))
}

total_missing <- sum(missing_summary$Missing_Count)
total_cells <- nrow(data_missing_check) * ncol(data_missing_check)
pct_total_missing <- (total_missing / total_cells) * 100

cat(sprintf("Totale valori mancanti: %d su %d (%.2f%%)\n", 
            total_missing, total_cells, pct_total_missing))

if (total_missing > 0) {
  cat("\nDettaglio per variabile:\n")
  missing_vars <- missing_summary[missing_summary$Missing_Count > 0, ]
  if (nrow(missing_vars) > 0) {
    for (i in seq_len(nrow(missing_vars))) {
      cat(sprintf("  - %s: %d (%.2f%%)\n", 
                  missing_vars$Variable[i],
                  missing_vars$Missing_Count[i],
                  missing_vars$Missing_Percent[i]))
    }
  }
  
  # Rimuove righe con valori mancanti usando la funzione esistente
  cat("\nRimozione righe con valori mancanti...\n")
  data_missing_check <- remove_missing_values(data_missing_check)
} else {
  cat("✓ Nessun valore mancante trovato\n")
}

# Aggiorna il dataset principale dopo la rimozione valori mancanti
data <- data_missing_check

cat("\n")

# -----------------------------------------------------------------------------
# 1.2 Analisi sull'arrotondamento
# -----------------------------------------------------------------------------
cat("1.2 ANALISI SULL'ARROTONDAMENTO\n")
cat(rep("-", 80), "\n", sep = "")

# Crea una copia per l'analisi
data_rounding_check <- data

# Analizza l'impatto dell'arrotondamento
compare_rounding(data_rounding_check)

# -----------------------------------------------------------------------------
# 1.3 Arrotondamento valori numerici
# -----------------------------------------------------------------------------
cat("\n1.3 ARROTONDAMENTO VALORI NUMERICI\n")
cat(rep("-", 80), "\n", sep = "")

# Crea una copia per l'arrotondamento
data_rounded <- data

# Arrotonda i valori numerici secondo le specifiche
data_rounded <- round_numeric_variables(data_rounded)

# -----------------------------------------------------------------------------
# 1.4 Calcolo BMI (necessario per il controllo outlier)
# -----------------------------------------------------------------------------
cat("\n1.4 CALCOLO BMI\n")
cat(rep("-", 80), "\n", sep = "")

# Calcola BMI sul dataset arrotondato
data_rounded <- calculate_bmi(data_rounded)

# -----------------------------------------------------------------------------
# 1.5 Controllo outlier quantitativi
# -----------------------------------------------------------------------------
cat("\n1.5 CONTROLLO OUTLIER QUANTITATIVI\n")
cat(rep("-", 80), "\n", sep = "")

# Crea una copia per il controllo outlier (usa il dataset arrotondato con BMI)
data_outlier_check <- data_rounded

# Controlla outlier nelle variabili quantitative
# Nota: questa funzione analizza e segnala gli outlier ma NON rimuove le righe
anomalies_quantitative(
  data_outlier_check,
  variables = get_outlier_numeric_columns(),  # Variabili numeriche dal data dictionary
  output_dir = "output/anomalie"
)

# Il dataset rimane invariato dopo l'analisi degli outlier
data_cleaned <- data_outlier_check

# -----------------------------------------------------------------------------
# 1.6 Controllo anomalie qualitative
# -----------------------------------------------------------------------------
cat("\n1.6 CONTROLLO ANOMALIE QUALITATIVE\n")
cat(rep("-", 80), "\n", sep = "")

# Crea una copia per il controllo anomalie qualitative
data_qualitative_check <- data_cleaned

# Controlla anomalie nelle variabili qualitative
# Nota: questa funzione analizza e segnala le anomalie ma NON rimuove le righe
anomalies_qualitative(data_qualitative_check)

# Il dataset rimane invariato dopo l'analisi delle anomalie qualitative
data_cleaned <- data_qualitative_check

# -----------------------------------------------------------------------------
# 1.7 Rimozione duplicati
# -----------------------------------------------------------------------------
cat("\n1.7 RIMOZIONE DUPLICATI\n")
cat(rep("-", 80), "\n", sep = "")

# Crea una copia per la rimozione duplicati
data_duplicate_check <- data_cleaned

# Rimuove righe duplicate
data_cleaned <- find_duplicates(data_duplicate_check)

# Aggiorna il dataset principale con i dati puliti
data <- data_cleaned

cat("\n", rep("=", 80), "\n", sep = "")
cat("FASE 1 COMPLETATA\n")
cat(rep("=", 80), "\n", sep = "")
cat(sprintf("Dataset finale: %d righe, %d colonne\n", nrow(data), ncol(data)))
cat(sprintf("Righe rimosse: %d (da %d a %d)\n\n", 
            n_rows_initial - nrow(data), 
            n_rows_initial, 
            nrow(data)))

# =============================================================================
# FASE 2: ANALISI DISTRIBUZIONI
# =============================================================================
# - Generazione distribuzioni per tutte le variabili del dataset

cat("\n", rep("=", 80), "\n", sep = "")
cat("FASE 2: ANALISI DISTRIBUZIONI\n")
cat(rep("=", 80), "\n\n", sep = "")

# -----------------------------------------------------------------------------
# 2.1 Generazione distribuzioni per tutte le variabili
# -----------------------------------------------------------------------------
cat("2.1 GENERAZIONE DISTRIBUZIONI\n")
cat(rep("-", 80), "\n", sep = "")

# Crea una copia per l'analisi delle distribuzioni
data_distributions <- data

# Genera i grafici delle distribuzioni per tutte le variabili
# Per variabili quantitative: istogramma, densità, boxplot, statistiche
# Per variabili qualitative: barplot con frequenze
# Usa get_all_columns() per ottenere tutte le variabili tranne 'class'
all_vars <- get_all_columns()
available_vars <- all_vars[all_vars %in% colnames(data_distributions)]
plot_distributions(
  data_distributions,
  variables = available_vars,  # Tutte le variabili dal data dictionary (escluso 'class')
  output_dir = "output/distribuzioni"
)

cat("\n", rep("=", 80), "\n", sep = "")
cat("FASE 2 COMPLETATA\n")
cat(rep("=", 80), "\n\n", sep = "")

# =============================================================================
# FASE 3: ANALISI BIVARIATE
# =============================================================================
# - Analisi bivariata per variabili quantitative (correlazioni, scatter plots)
# - Analisi bivariata per variabili qualitative (tabelle di contingenza)

cat("\n", rep("=", 80), "\n", sep = "")
cat("FASE 3: ANALISI BIVARIATE\n")
cat(rep("=", 80), "\n\n", sep = "")

# -----------------------------------------------------------------------------
# 3.1 Analisi bivariata per variabili quantitative
# -----------------------------------------------------------------------------
cat("3.1 ANALISI BIVARIATA VARIABILI QUANTITATIVE\n")
cat(rep("-", 80), "\n", sep = "")

# Crea una copia per l'analisi bivariata quantitativa
data_bivariate_quant <- data

# Analizza le coppie di variabili quantitative
# Genera scatter plots, calcola correlazioni di Pearson, heatmap delle correlazioni
bivariate_analysis(
  data_bivariate_quant,
  variables = get_bivariate_numeric_columns(),  # Variabili quantitative dal data dictionary
  output_dir = "output/correlazioni"
)

# -----------------------------------------------------------------------------
# 3.2 Analisi bivariata per variabili qualitative
# -----------------------------------------------------------------------------
cat("\n3.2 ANALISI BIVARIATA VARIABILI QUALITATIVE\n")
cat(rep("-", 80), "\n", sep = "")

# Crea una copia per l'analisi bivariata qualitativa
data_bivariate_qual <- data

# Analizza le coppie di variabili qualitative
# Genera tabelle di contingenza, frequenze relative/condizionate, barplot
# Calcola Cramér's V per misurare l'associazione
contingency_bivariate_analysis(
  data_bivariate_qual,
  variables = NULL,  # NULL = tutte le variabili qualitative dal data dictionary
  output_dir = "output/contingency"
)

cat("\n", rep("=", 80), "\n", sep = "")
cat("FASE 3 COMPLETATA\n")
cat(rep("=", 80), "\n\n", sep = "")

# =============================================================================
# FASE 4: ANALISI IMPORTANZA FEATURE (PARETO)
# =============================================================================
# - Analisi delle feature qualitative tramite diagramma di Pareto
# - Ordina le feature per informatività (entropia di Shannon)

cat("\n", rep("=", 80), "\n", sep = "")
cat("FASE 4: ANALISI IMPORTANZA FEATURE (PARETO)\n")
cat(rep("=", 80), "\n\n", sep = "")

# -----------------------------------------------------------------------------
# 4.1 Diagramma di Pareto delle feature qualitative
# -----------------------------------------------------------------------------
cat("4.1 DIAGRAMMA DI PARETO DELLE FEATURE QUALITATIVE\n")
cat(rep("-", 80), "\n", sep = "")

# Crea una copia per l'analisi Pareto
data_pareto <- data

# Genera il diagramma di Pareto per le feature qualitative
# Calcola l'entropia di Shannon per ogni feature e ordina in modo decrescente
# Mostra barre con entropia e curva cumulativa percentuale
pareto_features(
  data_pareto,
  variables = NULL,  # NULL = tutte le variabili qualitative dal data dictionary
  output_dir = "output",
  output_file = "pareto_features.png"
)

# -----------------------------------------------------------------------------
# 4.2 Importanza feature tramite K-means
# -----------------------------------------------------------------------------
cat("\n4.2 IMPORTANZA FEATURE TRAMITE K-MEANS\n")
cat(rep("-", 80), "\n", sep = "")

# Crea una copia per l'analisi K-means
data_kmeans <- data

# Analizza l'importanza delle feature qualitative usando K-means
# Codifica le variabili categoriche con one-hot encoding
# Applica K-means e calcola l'importanza di ogni feature come rapporto
# tra varianza between-cluster e varianza within-cluster
# Se k non è specificato, viene scelto automaticamente con il metodo del gomito
kmeans_feature_importance(
  data_kmeans,
  variables = NULL,  # NULL = tutte le variabili qualitative dal data dictionary (escluso class)
  k = NULL,  # NULL = scelto automaticamente con metodo del gomito
  output_dir = "output/kmeans"
)

cat("\n", rep("=", 80), "\n", sep = "")
cat("FASE 4 COMPLETATA\n")
cat(rep("=", 80), "\n\n", sep = "")

# =============================================================================
# FASE 5: ANALISI MULTIVARIATA (MCA - Multiple Correspondence Analysis)
# =============================================================================
# - Analisi delle corrispondenze multiple per variabili qualitative
# - Riduzione della dimensionalità e visualizzazione delle associazioni

cat("\n", rep("=", 80), "\n", sep = "")
cat("FASE 5: ANALISI MULTIVARIATA (MCA)\n")
cat(rep("=", 80), "\n\n", sep = "")

# -----------------------------------------------------------------------------
# 5.1 Multiple Correspondence Analysis (MCA)
# -----------------------------------------------------------------------------
cat("5.1 MULTIPLE CORRESPONDENCE ANALYSIS (MCA)\n")
cat(rep("-", 80), "\n", sep = "")

# Crea una copia per l'analisi MCA
data_mca <- data

# Effettua MCA (Multiple Correspondence Analysis) per variabili qualitative
# Analizza la struttura delle associazioni tra categorie e riduce la dimensionalità
# Genera:
# - Scree plot con varianza spiegata per dimensione
# - Plot delle variabili sui primi 2 assi principali
# - Plot degli individui sui primi 2 assi principali
# - Barplot dei contributi delle variabili alla prima dimensione
pca_qualitative(
  data_mca,
  variables = NULL,  # NULL = tutte le variabili qualitative dal data dictionary (escluso class)
  ncp = 5,  # Numero di dimensioni/componenti da estrarre
  output_dir = "output/pca_qualitative"
)

cat("\n", rep("=", 80), "\n", sep = "")
cat("FASE 5 COMPLETATA\n")
cat(rep("=", 80), "\n\n", sep = "")

# =============================================================================
# FASE 6: GENERAZIONE FEATURE DERIVATE
# =============================================================================
# - Creazione Diet_Quality_Index (DQI) dalle variabili dietetiche
# - Creazione Activity_Index (AI) dalle variabili di attività fisica
# - Analisi delle nuove feature create

cat("\n", rep("=", 80), "\n", sep = "")
cat("FASE 6: GENERAZIONE FEATURE DERIVATE\n")
cat(rep("=", 80), "\n\n", sep = "")

# -----------------------------------------------------------------------------
# 6.1 Creazione Diet_Quality_Index (DQI)
# -----------------------------------------------------------------------------
cat("6.1 CREAZIONE DIET_QUALITY_INDEX (DQI)\n")
cat(rep("-", 80), "\n", sep = "")

# Crea una copia per la creazione delle feature
data_features <- data

# Crea Diet_Quality_Index dalle variabili dietetiche
# Formula: DQI = 0.4 * CH2O_n + 0.3 * (1 - FAVC_n) + 0.2 * FCVC_n + 0.1 * (1 - CAEC_n)
# Le variabili vengono normalizzate su [0,1] usando i range teorici
data_features <- create_dietary_summary_feature(
  data_features,
  feature_name = "Diet_Quality_Index",
  verbose = TRUE
)

# Analizza Diet_Quality_Index con grafici e statistiche
analyze_diet_quality_index(
  data_features,
  output_dir = "output/dqi_analysis",
  include_bmi = TRUE
)

# -----------------------------------------------------------------------------
# 6.2 Creazione Activity_Index (AI)
# -----------------------------------------------------------------------------
cat("\n6.2 CREAZIONE ACTIVITY_INDEX (AI)\n")
cat(rep("-", 80), "\n", sep = "")

# Crea Activity_Index dalle variabili di attività fisica
# Formula: AI = 0.5 * FAF_n + 0.3 * (1 - TUE_n) + 0.2 * MTRANS_score
# FAF_n e TUE_n normalizzati su [0,1]; MTRANS_score = MTRANS/3
data_features <- create_activity_index(
  data_features,
  feature_name = "Activity_Index",
  verbose = TRUE
)

# Analizza Activity_Index con grafici e statistiche
analyze_activity_index(
  data_features,
  output_dir = "output/activity_index_analysis",
  include_bmi = TRUE
)

# Aggiorna il dataset principale con le nuove feature
data <- data_features

cat("\n", rep("=", 80), "\n", sep = "")
cat("FASE 6 COMPLETATA\n")
cat(rep("=", 80), "\n\n", sep = "")
cat(sprintf("Nuove feature create: Diet_Quality_Index, Activity_Index\n"))
cat(sprintf("Dataset aggiornato: %d righe, %d colonne\n\n", nrow(data), ncol(data)))

# =============================================================================
# FASE 7: CONFRONTO DATASET REALE VS SINTETICO
# =============================================================================
# - Confronto variabili quantitative originali
# - Confronto variabili qualitative
# - Confronto feature derivate

cat("\n", rep("=", 80), "\n", sep = "")
cat("FASE 7: CONFRONTO DATASET REALE VS SINTETICO\n")
cat(rep("=", 80), "\n\n", sep = "")

# Assicurati che anche il dataset sintetico abbia le feature derivate
# (se non le ha già, devono essere create)
if (!"BMI" %in% colnames(syn_data)) {
  syn_data <- calculate_bmi(syn_data)
}
if (!"Diet_Quality_Index" %in% colnames(syn_data)) {
  syn_data <- create_dietary_summary_feature(syn_data, verbose = FALSE)
}
if (!"Activity_Index" %in% colnames(syn_data)) {
  syn_data <- create_activity_index(syn_data, verbose = FALSE)
}

# -----------------------------------------------------------------------------
# 7.1 Confronto variabili quantitative originali
# -----------------------------------------------------------------------------
cat("7.1 CONFRONTO VARIABILI QUANTITATIVE ORIGINALI\n")
cat(rep("-", 80), "\n", sep = "")

# Confronta le variabili quantitative originali (Age, Height, Weight)
compare_real_vs_synthetic(
  real_data = data,
  synthetic_data = syn_data,
  output_dir = "output/real_vs_synthetic/quantitative",
  numeric_vars = get_original_numeric_columns(),  # Solo variabili numeriche originali
  categorical_vars = NULL  # Solo quantitative in questa sezione
)

# -----------------------------------------------------------------------------
# 7.2 Confronto variabili qualitative
# -----------------------------------------------------------------------------
cat("\n7.2 CONFRONTO VARIABILI QUALITATIVE\n")
cat(rep("-", 80), "\n", sep = "")

# Confronta le variabili qualitative ottenute dal data dictionary
# La funzione get_categorical_columns() restituisce tutte le variabili categoriche
# escludendo automaticamente 'class' (variabile target)
categorical_vars_to_compare <- get_categorical_columns()
compare_real_vs_synthetic(
  real_data = data,
  synthetic_data = syn_data,
  output_dir = "output/real_vs_synthetic/categorical",
  numeric_vars = character(0),  # Nessuna variabile numerica
  categorical_vars = categorical_vars_to_compare  # Variabili categoriche dal data dictionary
)

# -----------------------------------------------------------------------------
# 7.3 Confronto feature derivate
# -----------------------------------------------------------------------------
cat("\n7.3 CONFRONTO FEATURE DERIVATE\n")
cat(rep("-", 80), "\n", sep = "")

# Confronta le feature derivate (BMI, Diet_Quality_Index, Activity_Index)
compare_real_vs_synthetic(
  real_data = data,
  synthetic_data = syn_data,
  output_dir = "output/real_vs_synthetic/derived_features",
  numeric_vars = get_derived_features(),  # Feature derivate dal data dictionary
  categorical_vars = NULL  # Solo feature derivate quantitative
)

cat("\n", rep("=", 80), "\n", sep = "")
cat("FASE 7 COMPLETATA\n")
cat(rep("=", 80), "\n\n", sep = "")

# =============================================================================
# FASE 8: VERIFICA DISTRIBUZIONI NOTE
# =============================================================================
# - Verifica se le variabili appartengono a distribuzioni statistiche note
# - Per variabili continue: test di normalità (Shapiro-Wilk, KS)
# - Per variabili binarie: analisi Bernoulli

cat("\n", rep("=", 80), "\n", sep = "")
cat("FASE 8: VERIFICA DISTRIBUZIONI NOTE\n")
cat(rep("=", 80), "\n\n", sep = "")

# -----------------------------------------------------------------------------
# 8.1 Verifica distribuzioni per variabili continue
# -----------------------------------------------------------------------------
cat("8.1 VERIFICA DISTRIBUZIONI VARIABILI CONTINUE\n")
cat(rep("-", 80), "\n", sep = "")

# Valuta se le variabili continue del dataset sintetico appartengono
# a distribuzioni normali usando test statistici (Shapiro-Wilk, Kolmogorov-Smirnov)
# e visualizzazioni (Q-Q plot, istogramma con densità teorica, ECDF vs CDF)
# Filtra solo le variabili presenti nel dataset sintetico
all_numeric_vars <- get_distribution_fit_numeric_columns()
available_numeric_vars <- all_numeric_vars[all_numeric_vars %in% colnames(syn_data)]
evaluate_synthetic_distribution_fit(
  syn_data,
  output_dir = "output/synthetic_distribution_fit",
  continuous_vars = available_numeric_vars,  # Tutte le variabili numeriche dal data dictionary
  binary_vars = NULL,  # NULL = auto-detect di colonne con esattamente 2 livelli
  alpha = 0.05
)

cat("\n", rep("=", 80), "\n", sep = "")
cat("FASE 8 COMPLETATA\n")
cat(rep("=", 80), "\n\n", sep = "")
