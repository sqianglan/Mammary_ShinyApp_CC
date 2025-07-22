# Embryonic Mammary RNA-seq Shiny Application

Interactive web application for analyzing RNA-seq and ATAC-seq data from embryonic mammary tissue development.

## Overview

This Shiny application provides comprehensive visualization and analysis tools for exploring gene expression patterns and chromatin accessibility in embryonic mammary tissue development. The application features four main analysis modules organized in separate tabs.

## Features

### 📊 Summary Tab
- Dataset overview and description
- Sample information and summary statistics
- General visualization of the dataset

### 🧬 Epithelium Tab
- Gene expression analysis in epithelial cells
- Interactive gene selection and sample filtering
- Expression plots and data tables
- Customizable visualization parameters

### 🔬 Mesenchyme Tab
- Mesenchymal cell gene expression analysis
- Comparison by developmental timepoint and treatment
- Multiple visualization types:
  - Expression plots
  - Heatmaps
  - Statistical summaries
- Threshold-based filtering

### 🧪 ATAC-seq (Epithelium) Tab
- Chromatin accessibility analysis for epithelial cells
- Peak browser with genomic region selection
- Peak calling parameter adjustment
- Multiple analysis views:
  - Peak browser
  - Accessibility heatmaps
  - Motif enrichment analysis
  - Peak-gene association analysis

## Application Structure

```
├── app.R                    # Main application file
├── test_app.R              # Testing script
└── Modules/                # Tab modules
    ├── tab_summary.R       # Summary tab UI and server
    ├── tab_epithelium.R    # Epithelium tab UI and server
    ├── tab_mesenchyme.R    # Mesenchyme tab UI and server
    └── tab_atacseq.R       # ATAC-seq tab UI and server
```

## Requirements

### R Packages
```r
install.packages(c("shiny", "DT"))
```

### System Requirements
- R (≥ 4.0.0)
- Web browser (Chrome, Firefox, Safari, or Edge)

## Installation and Usage

### 1. Clone the Repository
```bash
git clone https://github.com/YOUR_USERNAME/Embryonic_Mammary_RNAseq_ShinyApp.git
cd Embryonic_Mammary_RNAseq_ShinyApp
```

### 2. Install Dependencies
```r
# In R console
install.packages(c("shiny", "DT"))
```

### 3. Run the Application
```r
# Method 1: Using shiny
library(shiny)
runApp()

# Method 2: Direct execution
source("app.R")
```

The application will open in your default web browser at `http://127.0.0.1:PORT` where PORT is automatically assigned by Shiny.

## Data Input

Currently, the application uses placeholder data for demonstration purposes. To use with your own data:

1. Replace the data loading sections in each tab's server function
2. Update the gene and sample choice lists with your actual data
3. Modify plot parameters to match your dataset characteristics

## Customization

### Adding New Genes or Samples
- Edit the `choices` parameter in `selectInput()` and `checkboxGroupInput()` functions
- Update the data loading functions in each tab's server logic

### Modifying Visualizations
- Customize plot parameters in the `renderPlot()` functions
- Add new plot types by creating additional output elements

### Styling
- Modify the CSS in `app.R` under `tags$style(HTML(...))`
- Adjust colors, fonts, and layout parameters

## File Descriptions

| File | Description |
|------|-------------|
| `app.R` | Main application file containing UI layout and server logic coordination |
| `Modules/tab_summary.R` | Summary tab with dataset overview and general statistics |
| `Modules/tab_epithelium.R` | Epithelial gene expression analysis interface |
| `Modules/tab_mesenchyme.R` | Mesenchymal gene expression analysis with advanced filtering |
| `Modules/tab_atacseq.R` | ATAC-seq peak analysis and chromatin accessibility visualization |
| `test_app.R` | Testing script to verify all components load correctly |

## Development

### Testing
Run the test script to verify all components:
```r
source("test_app.R")
```

### Adding New Features
1. Create new UI elements in the appropriate tab's `*_tab_ui()` function
2. Add corresponding server logic in the `*_tab_server()` function
3. Test the changes using `test_app.R`

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/new-feature`)
3. Commit your changes (`git commit -am 'Add new feature'`)
4. Push to the branch (`git push origin feature/new-feature`)
5. Create a Pull Request

## License

This project is open source. Please add an appropriate license file if you plan to distribute this code.

## Citation

If you use this application in your research, please cite:

```
[Your Name]. (2025). Embryonic Mammary RNA-seq Shiny Application. 
GitHub repository: https://github.com/YOUR_USERNAME/Embryonic_Mammary_RNAseq_ShinyApp
```

## Contact

For questions, issues, or contributions, please contact [Your Email] or open an issue on GitHub.

---

**Note**: This application currently uses simulated data for demonstration. Replace with your actual RNA-seq and ATAC-seq datasets for production use.
