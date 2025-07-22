tab_epitheliumUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # Add custom CSS for reduced margins and equal heights
    tags$style(HTML("
      .content-wrapper, .right-side {
        padding-left: 7.5px !important;
        padding-right: 7.5px !important;
      }
      .box {
        margin-bottom: 10px !important;
      }
      .equal-height-row {
        display: flex;
        align-items: stretch;
      }
      .equal-height-row .col-sm-8,
      .equal-height-row .col-sm-4 {
        display: flex;
        flex-direction: column;
      }
      .equal-height-row .box {
        flex: 1;
        display: flex;
        flex-direction: column;
        min-height: 620px;
      }
      .equal-height-row .box .box-body {
        flex: 1;
      }
    ")),
    
    # Main content
    fluidRow(class = "equal-height-row",
        column(8, style = "padding-right: 7.5px;",
          # Combined Gene Expression Analysis and Plot box
          box(
            title = "Gene Expression in Mammary Epithelium", status = "primary", solidHeader = TRUE, width = 12, padding = 0,
            fluidRow(
              column(3,
                textInput(ns("epithelium_genes"), "Enter Gene Name:", 
                         value = "Myh9",
                         placeholder = "Type gene name (e.g., Myh9)...")
              ),
              column(3,
                selectInput(ns("selected_groups"), "Select Groups:",
                       choices = list("E13.5 vs E16.5 WT" = "wt_comparison",
                             "E13.5 WT vs Stab ß-catenin" = "e13_comparison",
                             "E16.5 WT vs Stab ß-catenin" = "e16_comparison",
                             "All Groups" = "all_groups"),
                       selected = "all_groups")
              ),
              column(3,
                radioButtons(ns("plot_style"), "Statistical Test:",
                            choices = list("t-test" = "ttest", "Wald test (DESeq2)" = "deseq2"),
                            selected = "ttest",
                            inline = TRUE)
              ),
              column(3,
                div(style = "margin-top: 25px;",
                    actionButton(ns("update_plot"), "Search & Update Plot", class = "btn-primary"))
              )
            ),
            plotOutput(ns("epithelium_main_plot"), height = "550px")
          )
        ),
        column(4, style = "padding-left: 7.5px;",
          box(
            title = "Study Information", status = "info", solidHeader = TRUE, width = 12,
              div(style = "text-align: left; display: flex; flex-direction: column; height: 100%;",
              div(style = "text-align: center; margin-bottom: 2px;",
                img(src = "JID_cover.png", width = "80%", style = "max-width: 100%; margin-bottom: 5px;"),
                p(strong("Cover Feature (Image courtesy of Dr. Qiang Lan)"), style = "margin-bottom: 2px; font-size: 12px;")
              ),
              div(style = "margin-top: auto; padding-top: 2px;",
                p("The study associated with this data has been published. Please refer to the following citation for more details:", 
                  style = "font-size: 11px; margin-bottom: 8px; text-align: justify;"),
                p(em("\"Stabilization of Epithelial β-Catenin Compromises Mammary Cell Fate Acquisition and Branching Morphogenesis\""), 
                  " Satta JP, Lan Q, Taketo MM, Mikkola ML. ",
                  a("J Invest Dermatol. 2024;144(6):1223-1237.e10", href = "https://doi.org/10.1016/j.jid.2023.11.018", target = "_blank"),
                  style = "font-size: 11px; margin-bottom: 8px; font-style: italic; text-align: left;"),
                p("The data is also available at GEO with accession number: ", a("GSE236630", href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE236630", target = "_blank"), style = "font-size: 11px; margin-bottom: 5px; text-align: left;")
              )
              )
          )
        )
      ),
      fluidRow(
        tabBox(
          title = "Data Tables", width = 12,
          tabPanel("E13.5 vs E16.5 WT", DT::dataTableOutput(ns("deseq_table1"))),
          tabPanel("E13.5 Stab ß-catenin vs WT", DT::dataTableOutput(ns("deseq_table2"))),
          tabPanel("E16.5 Stab ß-catenin vs WT", DT::dataTableOutput(ns("deseq_table3")))
        )
      )
  )
}

tab_epitheliumServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Create reactive value to track initial load
    initial_load <- reactiveVal(FALSE)
    
    # Store cached data
    cached_normalized_data <- reactiveVal(NULL)
    cached_deseq2_data <- reactiveVal(NULL)
    
    # Load data automatically when module initializes
    observe({
      # Only load once
      if (is.null(cached_normalized_data()) && is.null(cached_deseq2_data())) {
        tryCatch({
          dds <- readRDS("rawData/Satta et al/Deseq_dds_normalized_matrix.rds")
          gene_annotation <- readRDS("rawData/Satta et al/gene_annotation.rds")
          sample_table <- read.csv("rawData/Satta et al/sampleTable.csv", stringsAsFactors = TRUE)
          
          # Convert to long format
          normalized_count <- dds %>% 
            as.data.frame() %>% 
            .[unique(gene_annotation$ensembl_gene_id),] %>% 
            rownames_to_column("ensembl_gene_id") %>% 
            pivot_longer(., 2:ncol(.), names_to = "sample", values_to = "normalized_count") %>% 
            merge(., sample_table[, c("sampleName", "group")], by.x = "sample", by.y = "sampleName") %>% 
            mutate(group = factor(group, levels = c("E13.5_WT", "E13.5_Stab_bcat", "E16.5_WT", "E16.5_Stab_bcat"))) %>% 
            merge(gene_annotation[, 1:2], by = "ensembl_gene_id")
          
          # Load DESeq2 data
          e16_vs_e13 <- read.csv("rawData/Satta et al/24_group_E16.5_WT_vs_E13.5_WT_LFC_shrinkaged.csv")
          e13_stab_vs_wt <- read.csv("rawData/Satta et al/20_group_E13.5_Stab_bcat_vs_E13.5_WT_LFC_shrinkaged.csv")
          e16_stab_vs_wt <- read.csv("rawData/Satta et al/28_group_E16.5_Stab_bcat_vs_E16.5_WT_LFC_shrinkaged.csv")
          
          # Combine results
          e16_vs_e13$comparison <- "E16.5_WT_vs_E13.5_WT"
          e13_stab_vs_wt$comparison <- "E13.5_Stab_bcat_vs_E13.5_WT"
          e16_stab_vs_wt$comparison <- "E16.5_Stab_bcat_vs_E16.5_WT"
          combined_results <- rbind(e16_vs_e13, e13_stab_vs_wt, e16_stab_vs_wt)
          
          # Cache the data
          cached_normalized_data(normalized_count)
          cached_deseq2_data(combined_results)
          
        }, error = function(e) {
          showNotification(paste("Error loading data files:", e$message), type = "error")
        })
      }
    })
    
    # Button click just triggers plot update (data already loaded)
    observeEvent(input$update_plot, {
      # This just acts as a trigger for plot updates
      # Data is already loaded by the observe block above
    })
    
    # Function to get normalized data
    normalized_data <- reactive({
      return(cached_normalized_data())
    })
    
    # Function to get DESeq2 data  
    deseq2_data <- reactive({
      return(cached_deseq2_data())
    })
    
    # Auto-load Myh9 plot on module initialization
    observe({
      # Auto-trigger the plot for Myh9 when the module initializes
      if (!initial_load() && !is.null(input$epithelium_genes) && 
          input$epithelium_genes == "Myh9" && !is.null(input$selected_groups)) {
        initial_load(TRUE)
        # Wait a moment for reactive dependencies to be ready
        invalidateLater(100, session)
      }
    })
    
    # Filter data based on selected genes
    filtered_data <- reactive({
      data <- normalized_data()
      
      # Check if data is still loading
      if (is.null(data)) {
        return(NULL) # Return NULL instead of error when data is still loading
      }
      
      # Return NULL if no gene or groups selected
      if (is.null(input$epithelium_genes) || input$epithelium_genes == "" ||
          is.null(input$selected_groups)) {
        return(NULL)
      }
      
      # Clean and validate the gene name
      gene_name <- trimws(input$epithelium_genes)
      available_genes <- unique(data$external_gene_name)
      
      # Check if gene exists in the dataset
      if (!gene_name %in% available_genes) {
        return(data.frame(error = paste0("Gene '", gene_name, "' not found in dataset.")))
      }
      
      # Map the selected group key to actual group names
      selected_group_names <- switch(input$selected_groups,
        "wt_comparison" = c("E13.5_WT", "E16.5_WT"),
        "e13_comparison" = c("E13.5_WT", "E13.5_Stab_bcat"),
        "e16_comparison" = c("E16.5_WT", "E16.5_Stab_bcat"),
        "all_groups" = c("E13.5_WT", "E13.5_Stab_bcat", "E16.5_WT", "E16.5_Stab_bcat"),
        c("E13.5_WT", "E16.5_WT") # default fallback
      )
      
      # Filter for selected gene and groups
      selected_count <- data %>% 
        dplyr::filter(external_gene_name == gene_name) %>%
        dplyr::filter(group %in% selected_group_names) %>%
        mutate(
          external_gene_name = factor(external_gene_name, levels = gene_name),
          group = factor(group, levels = c('E13.5_WT', 'E13.5_Stab_bcat', 'E16.5_WT', 'E16.5_Stab_bcat'))
        )
      
      return(selected_count)
    })
    
    # Prepare plotting data with statistics
    plotting_data <- reactive({
      selected_count <- filtered_data()
      
      if (is.null(selected_count)) {
        return(NULL)
      }
      
      # Check for error in gene validation
      if ("error" %in% names(selected_count)) {
        return(selected_count)  # Return error data frame
      }
      
      if (nrow(selected_count) == 0) {
        return(data.frame(error = "No data found for the selected gene and groups."))
      }
      
      # Calculate means and standard deviations exactly like in the example
      df_mean <- selected_count %>% 
        group_by(group, external_gene_name) %>% 
        summarise(mean = mean(normalized_count), std = sd(normalized_count), .groups = 'drop')
      
      # Merge with original data
      df_plotting_with_std <- merge(selected_count, df_mean, by = c('external_gene_name', 'group')) %>% 
        mutate(
          external_gene_name = factor(external_gene_name, levels = trimws(input$epithelium_genes)),
          group = factor(group, levels = c('E13.5_WT', 'E13.5_Stab_bcat', 'E16.5_WT', 'E16.5_Stab_bcat'))
        )
      
      return(df_plotting_with_std)
    })
    
    # Create function to add DESeq2 annotations
    create_deseq2_annotations <- function(gene_name, present_groups) {
      deseq2_results <- deseq2_data()
      if (is.null(deseq2_results)) return(list())
      
      # Filter results for the selected gene
      gene_results <- deseq2_results[deseq2_results$external_gene_name == gene_name, ]
      
      annotations <- list()
      y_positions <- c(0.85, 0.9, 0.95)  # Relative positions for annotations
      
      # Create proper x-axis position mapping based on the factor levels of present groups
      all_groups <- c('E13.5_WT', 'E13.5_Stab_bcat', 'E16.5_WT', 'E16.5_Stab_bcat')
      present_groups_ordered <- all_groups[all_groups %in% present_groups]
      group_position_map <- setNames(seq_along(present_groups_ordered), present_groups_ordered)
      
      # Check which comparisons are possible based on present groups
      if (all(c("E13.5_WT", "E13.5_Stab_bcat") %in% present_groups)) {
        e13_result <- gene_results[gene_results$comparison == "E13.5_Stab_bcat_vs_E13.5_WT", ]
        if (nrow(e13_result) > 0) {
          pval <- e13_result$padj[1]
          lfc <- round(e13_result$log2FoldChange[1], 2)
          sig_symbol <- if (is.na(pval)) "ns" else if (pval < 0.001) "***" else if (pval < 0.01) "**" else if (pval < 0.05) "*" else "ns"
          
          # Calculate middle position between the two E13.5 groups
          x_pos <- (group_position_map["E13.5_WT"] + group_position_map["E13.5_Stab_bcat"]) / 2
          
          annotations <- append(annotations, list(list(
            x = x_pos, y = y_positions[length(annotations) + 1], 
            label = sig_symbol,
            comparison = c("E13.5_WT", "E13.5_Stab_bcat")
          )))
        }
      }
      
      if (all(c("E16.5_WT", "E16.5_Stab_bcat") %in% present_groups)) {
        e16_result <- gene_results[gene_results$comparison == "E16.5_Stab_bcat_vs_E16.5_WT", ]
        if (nrow(e16_result) > 0) {
          pval <- e16_result$padj[1]
          lfc <- round(e16_result$log2FoldChange[1], 2)
          sig_symbol <- if (is.na(pval)) "ns" else if (pval < 0.001) "***" else if (pval < 0.01) "**" else if (pval < 0.05) "*" else "ns"
          
          # Calculate middle position between the two E16.5 groups
          x_pos <- (group_position_map["E16.5_WT"] + group_position_map["E16.5_Stab_bcat"]) / 2
          
          annotations <- append(annotations, list(list(
            x = x_pos, y = y_positions[length(annotations) + 1], 
            label = sig_symbol,
            comparison = c("E16.5_WT", "E16.5_Stab_bcat")
          )))
        }
      }
      
      if (all(c("E13.5_WT", "E16.5_WT") %in% present_groups)) {
        dev_result <- gene_results[gene_results$comparison == "E16.5_WT_vs_E13.5_WT", ]
        if (nrow(dev_result) > 0) {
          pval <- dev_result$padj[1]
          lfc <- round(dev_result$log2FoldChange[1], 2)
          sig_symbol <- if (is.na(pval)) "ns" else if (pval < 0.001) "***" else if (pval < 0.01) "**" else if (pval < 0.05) "*" else "ns"
          
          # Calculate middle position between E13.5_WT and E16.5_WT based on their actual positions in the plot
          e13_pos <- group_position_map["E13.5_WT"]
          e16_pos <- group_position_map["E16.5_WT"]
          x_pos <- (e13_pos + e16_pos) / 2
          
          annotations <- append(annotations, list(list(
            x = x_pos, y = y_positions[length(annotations) + 1], 
            label = sig_symbol,
            comparison = c("E13.5_WT", "E16.5_WT")
          )))
        }
      }
      
      return(annotations)
    }
    
    # Create the base plot (optimized for single gene)
    create_base_plot <- function(data) {
      # Get the groups actually present in the data
      present_groups <- unique(data$group)
      
      # Create labels mapping for only the present groups
      group_labels <- c('E13.5_WT' = 'E13.5 WT', 
                       'E13.5_Stab_bcat' = 'E13.5 Stab ß-catenin', 
                       'E16.5_WT' = 'E16.5 WT', 
                       'E16.5_Stab_bcat' = 'E16.5 Stab ß-catenin')
      
      # Filter labels to only include present groups
      filtered_labels <- group_labels[names(group_labels) %in% present_groups]
      
      p <- ggplot(data, aes(x = group, y = normalized_count, color = group)) +
        theme_classic() +
        theme(
          strip.background = element_blank(),
          strip.text.x = element_text(face = "bold"),
          legend.position = "bottom",
          text = element_text(size = 12),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold")
        ) +
        geom_bar(stat = "summary", fun = "mean", position = position_dodge(width = 0.8), 
                 width = 0.7, aes(fill = group), linewidth = 0.4, alpha = 0.6,show.legend = FALSE) +
        geom_quasirandom(show.legend = FALSE, size = 3, alpha = 0.8) +
        scale_fill_manual(
          values = c('E13.5_WT' = '#91D1C2FF', 'E13.5_Stab_bcat' = '#4DBBD5FF', 
                     'E16.5_WT' = '#F39B7FFF', 'E16.5_Stab_bcat' = '#E64B35FF'), 
          labels = c("E13.5 WT", "E13.5 Stab ß-catenin", "E16.5 WT", "E16.5 Stab ß-catenin"), 
          name = "Group"
        ) +
        scale_color_manual(
          values = c('E13.5_WT' = '#008E51', 'E13.5_Stab_bcat' = '#3C5488FF',
                     'E16.5_WT' = '#E64B35FF', 'E16.5_Stab_bcat' = '#E64B35FF'),
          labels = c("E13.5 WT", "E13.5 Stab ß-catenin", "E16.5 WT", "E16.5 Stab ß-catenin"), 
          name = "Group"
        ) +
        xlab("") +
        scale_x_discrete(labels = filtered_labels) +
        ylab("Normalized Counts") +
        geom_errorbar(
          aes(ymin = mean - std, ymax = mean + std, color = group),
          width = 0.3, 
          linewidth = 0.3, 
          show.legend = FALSE
        )
      
      return(p)
    }
    
    # Main expression plot
    output$epithelium_main_plot <- renderPlot({
      data <- plotting_data()
      
      # Handle data loading state
      if (is.null(data)) {
        # Check if data is still loading
        if (is.null(cached_normalized_data())) {
          return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                         label = "Loading data, please wait...", 
                         size = 6, color = "#337ab7") +
                 theme_void() +
                 theme(plot.margin = margin(50, 50, 50, 50)))
        } else {
          return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                         label = "Please enter a gene name and click 'Search & Update Plot'", 
                         size = 6, color = "gray50") +
                 theme_void() +
                 theme(plot.margin = margin(50, 50, 50, 50)))
        }
      }
      
      # Check for error messages
      if ("error" %in% names(data)) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                       label = data$error[1], 
                       size = 5, color = "red") +
               theme_void() +
               theme(plot.margin = margin(50, 50, 50, 50)))
      }
      
      if (nrow(data) == 0) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                       label = "No data found for the selected gene and groups", 
                       size = 6, color = "gray50") +
               theme_void() +
               theme(plot.margin = margin(50, 50, 50, 50)))
      }
      
      p <- create_base_plot(data)
      
      # Get the gene name for the title
      gene_name <- as.character(unique(data$external_gene_name)[1])
      
      # Add different styling based on user selection
      if (input$plot_style == "ttest") {
        # Add t-test comparison only if we have the right groups
        present_group_levels <- unique(data$group)
        
        # Define potential comparisons
        comparisons <- list()
        if (all(c("E13.5_WT", "E13.5_Stab_bcat") %in% present_group_levels)) {
          comparisons <- append(comparisons, list(c("E13.5_WT", "E13.5_Stab_bcat")))
        }
        if (all(c("E16.5_WT", "E16.5_Stab_bcat") %in% present_group_levels)) {
          comparisons <- append(comparisons, list(c("E16.5_WT", "E16.5_Stab_bcat")))
        }
        if (all(c("E13.5_WT", "E16.5_WT") %in% present_group_levels)) {
          comparisons <- append(comparisons, list(c("E13.5_WT", "E16.5_WT")))
        }
        
        # Create descriptive title based on selected groups
        title_suffix <- switch(input$selected_groups,
          "wt_comparison" = "(E13.5 vs E16.5 WT)",
          "e13_comparison" = "(E13.5: WT vs Stab ß-catenin)",
          "e16_comparison" = "(E16.5: WT vs Stab ß-catenin)",
          "all_groups" = "(All Groups)",
          "(Multiple Groups)" # default fallback
        )
        
        # Only add statistical comparison if we have valid comparisons
        if (length(comparisons) > 0) {
          p <- p + 
            stat_compare_means(
              method = "t.test",
              aes(label = ..p.signif..), 
              show.legend = FALSE, 
              size = 6,
              comparisons = comparisons
            )
        }
        
        p <- p + ggtitle(paste0(gene_name, " Expression ", title_suffix, "\n(t-test)")) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
          
      } else if (input$plot_style == "deseq2") {
        # Add DESeq2 annotations
        present_group_levels <- unique(data$group)
        annotations <- create_deseq2_annotations(gene_name, present_group_levels)
        
        # Create descriptive title based on selected groups
        title_suffix <- switch(input$selected_groups,
          "wt_comparison" = "(E13.5 vs E16.5 WT)",
          "e13_comparison" = "(E13.5: WT vs Stab ß-catenin)",
          "e16_comparison" = "(E16.5: WT vs Stab ß-catenin)",
          "all_groups" = "(All Groups)",
          "(Multiple Groups)" # default fallback
        )
        
        # Add annotations to the plot
        if (length(annotations) > 0) {
          # Calculate y position based on data range
          y_range <- range(data$normalized_count)
          y_max <- y_range[2]
          y_spacing <- (y_max - y_range[1]) * 0.1
          
          for (i in seq_along(annotations)) {
            ann <- annotations[[i]]
            y_pos <- y_max + (i * y_spacing)
            
            p <- p + annotate("text", 
                             x = ann$x, 
                             y = y_pos,
                             label = ann$label, 
                             size = 6, 
                             hjust = 0.5,
                             color = "black")
            
            # Add bracket for comparison if both groups are present
            if (all(ann$comparison %in% present_group_levels)) {
              # Find the actual positions of the comparison groups in the current plot using the same logic as annotation positioning
              all_groups <- c('E13.5_WT', 'E13.5_Stab_bcat', 'E16.5_WT', 'E16.5_Stab_bcat')
              present_groups_ordered <- all_groups[all_groups %in% present_group_levels]
              group_position_map <- setNames(seq_along(present_groups_ordered), present_groups_ordered)
              
              if (all(ann$comparison %in% names(group_position_map))) {
                x1 <- group_position_map[ann$comparison[1]]
                x2 <- group_position_map[ann$comparison[2]]
                bracket_y <- y_pos - y_spacing * 0.3
                
                # Only draw brackets if the groups are not adjacent (to avoid overcrowding)
                if (abs(x2 - x1) > 0.5) {
                  p <- p + 
                    annotate("segment", x = x1, xend = x2, y = bracket_y, yend = bracket_y, size = 0.5) +
                    annotate("segment", x = x1, xend = x1, y = bracket_y - y_spacing * 0.1, yend = bracket_y, size = 0.5) +
                    annotate("segment", x = x2, xend = x2, y = bracket_y - y_spacing * 0.1, yend = bracket_y, size = 0.5)
                }
              }
            }
          }
        }
        
        p <- p + ggtitle(paste0(gene_name, " Expression ", title_suffix, "\n(DESeq2 Wald Test)")) +
         theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      }
      
      return(p)
    })
    
    # Load DESeq2 comparison tables for the data tables
    deseq_comparisons <- reactive({
      # Try to use the cached deseq2_data, but fall back to individual file loading if needed
      combined_data <- deseq2_data()
      
      if (!is.null(combined_data)) {
        # Split the combined data back into individual comparisons
        list(
          e16_vs_e13_wt = combined_data[combined_data$comparison == "E16.5_WT_vs_E13.5_WT", ],
          e13_stab_vs_wt = combined_data[combined_data$comparison == "E13.5_Stab_bcat_vs_E13.5_WT", ],
          e16_stab_vs_wt = combined_data[combined_data$comparison == "E16.5_Stab_bcat_vs_E16.5_WT", ]
        )
      } else {
        # Return empty data frames if files cannot be loaded
        list(
          e16_vs_e13_wt = data.frame(Error = "Unable to load comparison data"),
          e13_stab_vs_wt = data.frame(Error = "Unable to load comparison data"),
          e16_stab_vs_wt = data.frame(Error = "Unable to load comparison data")
        )
      }
    })
    
    # Render data tables
    output$deseq_table1 <- DT::renderDataTable({
      DT::datatable(
        deseq_comparisons()$e16_vs_e13_wt,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = "E16.5 WT vs E13.5 WT Comparison"
      )
    })
    
    output$deseq_table2 <- DT::renderDataTable({
      DT::datatable(
        deseq_comparisons()$e13_stab_vs_wt,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = "E13.5 Stab ß-catenin vs E13.5 WT Comparison"
      )
    })
    
    output$deseq_table3 <- DT::renderDataTable({
      DT::datatable(
        deseq_comparisons()$e16_stab_vs_wt,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = "E16.5 Stab ß-catenin vs E16.5 WT Comparison"
      )
    })
    
  })
}