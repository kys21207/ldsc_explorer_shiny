# Heritability & Genetic Correlation Dashboard

This Shiny application provides an interactive dashboard for exploring heritability (h2) and genetic correlation (rg) data. The dashboard allows users to select traits using MeSH terms or disease categories, visualize heritability and genetic correlation results, and access tutorial documentation.

## Features

- **Interactive Selection:** Choose classifications and categories to filter traits.
- **Data Visualization:** Visualize heritability and genetic correlation data using various plots.
- **Tutorials:** Access embedded tutorials for guidance.
- **User Guide:** Available for additional help and support.

## Prerequisites

Ensure you have the following R packages installed:

- `gtx`
- `implyr`
- `shinydashboardPlus`
- `shinydashboard`
- `shinyWidgets`
- `shinycssloaders`
- `shinyjs`
- `DT`
- `shiny`
- `bslib`
- `data.table`
- `glue`
- `tidyverse`
- `readxl`
- `ggplot2`
- `gghighlight`
- `ggtext`
- `cowplot`
- `reshape2`
- `RColorBrewer`
- `cluster`
- `ggdendro`

You can install these packages using the following command:

```r
install.packages(c("gtx", "implyr", "shinydashboardPlus", "shinydashboard", "shinyWidgets", "shinycssloaders", "shinyjs", "DT", "shiny", "bslib", "data.table", "glue", "tidyverse", "readxl", "ggplot2", "gghighlight", "ggtext", "cowplot", "reshape2", "RColorBrewer", "cluster", "ggdendro"))
```

## Getting Started

1. **Clone the Repository:**
   ```sh
   git clone https://github.com/your-repo/heritability-dashboard.git
   cd heritability-dashboard
   ```

2. **Source Functions:**
   ```r
   source("server_fxns.R")
   source("ui_fxns.R")
   source("general_fxns.R")
   ```

3. **Database Connection:**
   Ensure you have the correct database connection details:
   ```r
   dsn = "IPSCC"
   db_init = "gene_gwas_hg38_use"
   db = "gene_gwas_hg38_shared"
   conn <- get_connection(dsn = dsn, db = db_init, cache = FALSE)
   ```

4. **Run the Shiny App:**
   ```r
   shinyApp(ui, server)
   ```

## Usage

### Sidebar Menu
- **Tutorials:** Access embedded tutorials.
- **Heritability & Correlation:** Select traits and visualize results.
- **User Guide:** Access additional help.

### Heritability & Correlation Tab
- **Classification Selection:** Choose a classification (Disease, MeSH, MeSH label).
- **Category Selection:** Select a category based on the chosen classification.
- **Z h2 Threshold:** Set a Z h2 threshold for filtering traits.
- **Run Analysis:** Click the "RUN" button to execute the analysis.

### Visualization
- **Heritability Table:** View the heritability results in a table.
- **Genetic Correlation Table:** View the genetic correlation results in a table.
- **Plots:** Visualize the distributions and correlations using various plots.

