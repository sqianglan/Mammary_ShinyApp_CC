# Claude Configuration

This file contains configuration and context for Claude Code to better understand and work with this project.

## Project Overview
Embryonic Mammary RNA-seq Shiny App - An interactive web application for analyzing embryonic mammary gland RNA sequencing data.

## Key Technologies
- R
- Shiny
- RNA-seq data analysis

## Project Structure
- `app.R` - Main Shiny application file
- `Modules/` - Contains modular R components for different tabs
```
└── Modules/                # Tab modules
    ├── tab_summary.R       # Summary tab UI and server
    ├── tab_epithelium.R    # Epithelium tab UI and server
    ├── tab_mesenchyme.R    # Mesenchyme tab UI and server
    └── tab_atacseq.R       # ATAC-seq tab UI and server
```
- `rawData/` - Raw data files (not tracked in git)
- 'examples/' - Example code for plot to include in the 
## Common Commands
<!-- Add frequently used commands here -->

## Development Notes

### tab_epithelium Module Functionality
The epithelium tab includes the following key features:
1. **Gene Search** - Search for genes in the database
2. **Normalized Gene Count Plot** - Plot normalized gene counts similar to examples/Satta et al
3. **Statistical Analysis Options** - Two statistical methods available:
   - t-test
   - DESeq2
4. **DESeq2 Data Source** - DESeq2 statistics obtained from `rawData/Satta et al/`
5. **Additional Data Tables** - 3 additional tabs displaying tables from `rawData/Satta et al/`

<!-- Add any important development context or patterns -->