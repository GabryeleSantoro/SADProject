# SADProject - Statistica ed Analisi dei Dati

Progetto per l'esame di **Statistica ed Analisi dei Dati (SAD)** che esegue un'analisi completa e strutturata di un dataset relativo all'obesità, confrontando dati reali con dati sintetici generati.

## 📋 Descrizione del Progetto

Il progetto implementa un workflow completo di analisi statistica suddiviso in 8 fasi sequenziali:

1. **Controllo Qualità Dati**: Verifica valori mancanti, analisi dell'arrotondamento, identificazione outlier e rimozione duplicati
2. **Analisi Distribuzioni**: Generazione di grafici delle distribuzioni per tutte le variabili (quantitative e qualitative)
3. **Analisi Bivariate**: Correlazioni tra variabili quantitative e tabelle di contingenza per variabili qualitative
4. **Analisi Importanza Feature**: Diagramma di Pareto e analisi K-means per identificare le feature più informative
5. **Analisi Multivariata**: Multiple Correspondence Analysis (MCA) per variabili qualitative
6. **Generazione Feature Derivate**: Creazione di indici compositi (Diet_Quality_Index e Activity_Index)
7. **Confronto Dataset Reale vs Sintetico**: Valutazione della qualità dei dati sintetici rispetto ai dati reali
8. **Verifica Distribuzioni Note**: Test statistici per verificare se i dati sintetici seguono distribuzioni teoriche

## 🗂️ Struttura del Progetto

```
SADProject/
├── main.R                    # Script principale che esegue tutte le fasi
├── data/                     # Directory contenente i dataset
│   ├── Obesity_onevsrest_4_min_labeled.csv    # Dataset reale
│   └── syntetic_obesity_data.csv               # Dataset sintetico
├── R/                        # Directory con funzioni modulari
│   ├── data_dictionary.R     # Dizionario centralizzato delle variabili
│   ├── functions.R           # Funzioni utility e feature engineering
│   ├── packages.R            # Gestione pacchetti R necessari
│   ├── anomalies.R           # Rilevamento anomalie e duplicati
│   ├── analysis.R            # Analisi statistiche (bivariate, MCA, Pareto, K-means)
│   ├── distributions.R       # Visualizzazione distribuzioni
│   └── rounding.R           # Gestione arrotondamento valori
└── output/                   # Directory di output (creata automaticamente)
    ├── distribuzioni/        # Grafici delle distribuzioni
    ├── correlazioni/         # Scatter plots e heatmap correlazioni
    ├── contingency/          # Tabelle di contingenza
    ├── kmeans/               # Analisi importanza feature K-means
    ├── pca_qualitative/      # Risultati MCA
    ├── dqi_analysis/         # Analisi Diet_Quality_Index
    ├── activity_index_analysis/  # Analisi Activity_Index
    ├── real_vs_synthetic/    # Confronti dataset reale vs sintetico
    └── synthetic_distribution_fit/  # Test distribuzioni
```

## 🔧 Requisiti

- **R** (versione 4.0 o superiore)
- **Pacchetti R necessari** (installati automaticamente dallo script):
  - `FactoMineR`: Per l'analisi MCA (Multiple Correspondence Analysis)
  - `gridExtra`: Per la generazione di tabelle PNG

## 🚀 Procedura di Lancio

### 1. Preparazione

Assicurati di avere i file dei dataset nella directory `data/`:

- `Obesity_onevsrest_4_min_labeled.csv` (dataset reale)
- `syntetic_obesity_data.csv` (dataset sintetico)

### 2. Esecuzione

Apri R o RStudio nella directory del progetto e esegui:

```r
source("main.R")
```

Oppure dalla riga di comando:

```bash
Rscript main.R
```

### 3. Output

Lo script eseguirà automaticamente tutte le 8 fasi dell'analisi:

- **Stampa a console**: Progresso di ogni fase con statistiche e riepiloghi
- **File PNG**: Grafici e visualizzazioni salvati nella directory `output/`
- **Report**: Statistiche descrittive e risultati dei test stampati a console

### 4. Risultati

Al termine dell'esecuzione troverai:

- Grafici delle distribuzioni per tutte le variabili
- Analisi di correlazione tra variabili quantitative
- Tabelle di contingenza per variabili qualitative
- Diagramma di Pareto delle feature qualitative
- Analisi di importanza feature tramite K-means
- Risultati dell'analisi MCA
- Analisi delle feature derivate (DQI e Activity_Index)
- Confronti tra dataset reale e sintetico
- Test statistici per verificare distribuzioni note

## 📊 Dettagli delle Fasi

### Fase 1: Controllo Qualità Dati

- Conta e rimuove valori mancanti
- Analizza l'impatto dell'arrotondamento
- Arrotonda valori numerici secondo specifiche (AGE=0, Weight=1, Height=2, BMI=2)
- Identifica outlier usando metodo IQR e dominio specifico per BMI
- Rimuove righe duplicate

### Fase 2: Analisi Distribuzioni

- Genera istogrammi, densità, boxplot per variabili quantitative
- Genera barplot per variabili qualitative
- Include statistiche descrittive

### Fase 3: Analisi Bivariate

- **Quantitative**: Scatter plots, correlazioni di Pearson, heatmap correlazioni
- **Qualitative**: Tabelle di contingenza, frequenze condizionate, Cramér's V

### Fase 4: Importanza Feature

- **Pareto**: Ordina feature qualitative per entropia di Shannon
- **K-means**: Calcola importanza feature come rapporto varianza between/within cluster

### Fase 5: Analisi Multivariata

- MCA per ridurre dimensionalità variabili qualitative
- Scree plot, plot variabili/individui, contributi

### Fase 6: Feature Derivate

- **Diet_Quality_Index**: Indice composito da CH2O, FAVC, FCVC, CAEC
- **Activity_Index**: Indice composito da FAF, TUE, MTRANS
- Analisi grafiche complete per entrambi gli indici

### Fase 7: Confronto Reale vs Sintetico

- Confronta statistiche descrittive
- Grafici densità sovrapposti per variabili quantitative
- Barplot sovrapposti per variabili qualitative
- Analisi separata per variabili originali, qualitative e feature derivate

### Fase 8: Verifica Distribuzioni

- Test Shapiro-Wilk e Kolmogorov-Smirnov per normalità
- Q-Q plots, istogrammi con densità teorica, ECDF vs CDF
- Analisi Bernoulli per variabili binarie

## 📝 Note

- Il progetto utilizza un **data dictionary centralizzato** (`R/data_dictionary.R`) per gestire tipi di variabili, valori validi ed etichette
- Ogni fase crea una **copia del dataset** per preservare l'integrità dei dati originali
- I pacchetti R vengono **installati automaticamente** se non presenti
- Tutti i grafici vengono salvati automaticamente nella directory `output/`

## 🔍 Funzioni Principali

Le funzioni sono organizzate in moduli:

- **`data_dictionary.R`**: Funzioni per accedere a tipi, valori validi, etichette delle variabili
- **`functions.R`**: Feature engineering (BMI, DQI, Activity_Index), lettura CSV, pulizia dati
- **`anomalies.R`**: Rilevamento outlier quantitativi e qualitativi, rimozione duplicati
- **`analysis.R`**: Analisi statistiche avanzate (bivariate, Pareto, K-means, MCA, distribuzioni)
- **`distributions.R`**: Visualizzazione distribuzioni con palette coerente
- **`rounding.R`**: Gestione arrotondamento valori numerici
