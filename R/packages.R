# packages.R
# Gestione dei pacchetti R necessari per il progetto SADProject

# ============================================================================
# CONFIGURAZIONE
# ============================================================================

# Lista dei pacchetti necessari per il progetto
packages <- c(
  "dplyr",           # Manipolazione e trasformazione dati
  "tidyr",           # Pulizia e organizzazione dati
  "isotree"          # Alberi di regressione isotropi
)

# Verifica se i pacchetti sono installati
not_installed_packages <- c()
for (pkg in packages) {
  if (!pkg %in% rownames(installed.packages())) {
    not_installed_packages <- c(not_installed_packages, pkg)
  }
}

install.packages(not_installed_packages)

if (length(not_installed_packages) > 0) {
  stop(
    sprintf(
      "❌ I seguenti pacchetti NON sono stati installati correttamente: %s",
      paste(not_installed_packages, collapse = ", ")
    )
  )
}

message("✔ All packages installed")