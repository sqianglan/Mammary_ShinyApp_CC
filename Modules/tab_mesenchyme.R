tab_mesenchymeUI <- function(id) {
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
        min-height: 660px;
      }
      .equal-height-row .box .box-body {
        flex: 1;
      }
    ")),
    
    # Main content
    fluidRow(class = "equal-height-row",
        column(8, style = "padding-right: 3.5px;",
          # Combined Gene Expression Analysis and Plot box
          box(
            title = "Gene Expression in Mammary Mesenchyme", status = "primary", solidHeader = TRUE, width = 12, padding = 0,
            fluidRow(
              style = "align-items: center; margin-bottom: 8px; margin-left: -2px; margin-right: -2px;",
              column(2,
                style = "padding-left: 2px; padding-right: 2px;",
                selectizeInput(ns("mesenchyme_genes"), "Enter Gene Name:", 
                         choices = NULL,
                         selected = "Tbx18",
                         options = list(
                           placeholder = "Type gene name (e.g., Tbx18)...",
                           onInitialize = I('function() { this.setValue("Tbx18"); }'),
                           create = TRUE,
                           maxItems = 1
                         ))
              ),
              column(3, style = "padding-left: 2px; padding-right: 2px;",
                selectInput(ns("selected_groups"), "Select Groups:",
                       choices = list("E13.5 Mammary vs SMG Mes" = "e13_mammary_vs_smg",
                             "E16.5 Mammary vs E13.5 Mammary Mes" = "e16_vs_e13_mammary",
                             "E16.5 Mammary vs SMG Mes" = "e16_mammary_vs_smg",
                             "E16.5 Fatpad vs E13.5 Mammary Mes" = "e16_fatpad_vs_e13_mammary",
                             "E16.5 Fatpad vs SMG Mes" = "e16_fatpad_vs_smg",
                             "E13.5 Skin vs Mammary Mes" = "e13_skin_vs_mammary",
                             "All Groups" = "all_groups"),
                       selected = "all_groups")
              ),
              column(3, style = "padding-left: 20px; padding-right: 2px;",
                div(
                  tags$label("Statistical Test:", style = "margin-bottom: 10px; display: block; font-weight: bold;"),
                  radioButtons(ns("plot_style"), label = NULL,
                              choices = list("t-test" = "ttest", "Wald test (DESeq2)" = "deseq2"),
                              selected = "ttest",
                              inline = TRUE)
                )
              ),
                column(4, style = "padding-left: 2px; padding-right: 2px; ",
                div(style = "display: flex; justify-content: flex-end; align-items: center; gap: 35px; margin-top: 25px;",
                    actionButton(ns("update_plot"), "Search & Update Plot", class = "btn-primary"),
                    actionButton(ns("export_dialog"), "Export Plot", icon = icon("download"), 
                        class = "btn-success", style = "padding: 6px 12px; height: 34px;")
                )
                )
            ),
            plotOutput(ns("mesenchyme_main_plot"), height = "555px")
          )
        ),
        column(4, style = "padding-left: 3.5px;",
          box(
            title = "Study Information", status = "info", solidHeader = TRUE, width = 12, height = "697px",
              div(style = "text-align: left; display: flex; flex-direction: column; height: 100%;",
              div(style = "text-align: center; margin-bottom: 0px;  margin-top: 5px;",
                h4("Sample Preparation", style = "font-weight: bold; color: #337ab7; margin-bottom: 10px; text-align: center;"),
                img(src = "mesenchyme_sample_workflow.png", width = "100%", style = "max-width: 100%; margin-bottom: 4px;padding-top: 30px;")
                ),
                div(style = "margin-top: auto; padding-top: 2px;",
                p("The study associated with this data has been published. Please refer to the following citation for more details:", 
                  style = "font-size: 12px; margin-bottom: 0px; text-align: justify;"),
                br(),
                p(em("\"Mesenchyme instructs growth while epithelium directs branching in the mouse mammary gland\""), 
                  " Lan Q, Trela E, Lindström R, Satta JP, Kaczyńska B, Christensen MM, Holzenberger M, Jernvall J, Mikkola ML. ",
                  a("eLife. 2024;13:e93326", href = "https://doi.org/10.7554/eLife.93326", target = "_blank"),
                  style = "font-size: 12px; margin-bottom: 2px; font-style: italic; text-align: left;"),
                br(),
                p("The data is also available at GEO with accession number: ", a("GSE225821", href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE225821", target = "_blank"), style = "font-size: 12px; margin-bottom: 5px; margin-top: 0px; text-align: left;")
                )
              )
          )
        )
      ),
      fluidRow(
        tabBox(
          title = "Differential Expression Analysis by DESeq2", width = 12,
          tabPanel("E13.5 Mammary vs SMG Mes", DT::dataTableOutput(ns("deseq_table1"))),
          tabPanel("E16.5 Mammary Mes vs E13.5 Mammary Mes", DT::dataTableOutput(ns("deseq_table2"))),
          tabPanel("E16.5 Mammary Mes vs SMG Mes", DT::dataTableOutput(ns("deseq_table3"))),
          tabPanel("E16.5 Fatpad vs E13.5 Mammary Mes", DT::dataTableOutput(ns("deseq_table4"))),
          tabPanel("E16.5 Fatpad vs SMG Mes", DT::dataTableOutput(ns("deseq_table5"))),
          tabPanel("E13.5 Skin Mes vs Mammary Mes", DT::dataTableOutput(ns("deseq_table6")))
        )
      )
  )
}

tab_mesenchymeServer <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    
    # Create namespace function for IDs
    ns <- session$ns
    
    # Helper function for null coalescing
    `%||%` <- function(a, b) if (is.null(a)) b else a
    
    # Store cached data and track states
    cached_normalized_data <- reactiveVal(NULL)
    cached_deseq2_data <- reactiveVal(NULL)
    data_loaded <- reactiveVal(FALSE)
    initial_load <- reactiveVal(FALSE)
    show_loading <- reactiveVal(FALSE)
    
    # Store current plot for export and selected directory
    current_plot <- reactiveVal(NULL)
    selected_export_dir <- reactiveVal(path.expand("~/Desktop"))  # Default to Desktop
    
    # Export-related reactive values
    export_default_filename <- reactiveVal("")
    
    # Generate smart filename
    generate_filename <- reactive({
      # Safely get gene name
      gene_name <- tryCatch({
        if (!is.null(input$mesenchyme_genes) && input$mesenchyme_genes != "") {
          input$mesenchyme_genes
        } else {
          "plot"
        }
      }, error = function(e) "plot")
      
      # Generate filename with mammary_mesenchyme suffix and timestamp
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      
      return(paste(gene_name, "mammary_mesenchyme", timestamp, sep = "_"))
    })
    
    # Export functionality with simple modal dialog
    observeEvent(input$export_dialog, {
      # Check if we have a plot to export
      if (is.null(plotting_data()) || ("error" %in% names(plotting_data())) || nrow(plotting_data()) == 0) {
        showNotification("No plot available to export", type = "error", duration = 3)
        return()
      }
      
      # Get default filename
      default_filename <- generate_filename()
      
      # Show export options modal
      showModal(modalDialog(
        title = "Export Plot",
        size = "s",
        fluidRow(
          column(12,
            textInput(ns("export_filename"), "File Name:", value = default_filename),
            br(),
            actionButton(ns("choose_directory"), "Choose Directory", icon = icon("folder-open"), class = "btn-info"),
            br(),
            textOutput(ns("selected_directory")),
            br(),
            selectInput(ns("export_format"), "File Format:", 
                       choices = list("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff"),
                       selected = "pdf"),
            numericInput(ns("export_width"), "Width (inches):", value = 8, min = 1, max = 20, step = 0.5),
            numericInput(ns("export_height"), "Height (inches):", value = 6, min = 1, max = 20, step = 0.5),
            conditionalPanel(
              condition = paste0("input['", ns("export_format"), "'] != 'pdf'"),
              selectInput(ns("export_dpi"), "Resolution Quality:", 
                         choices = list(
                           "Web/Screen (150 DPI)" = 150,
                           "High Quality (300 DPI)" = 300,
                           "Publication (600 DPI)" = 600,
                           "Custom" = "custom"
                         ),
                         selected = 300),
              conditionalPanel(
                condition = paste0("input['", ns("export_dpi"), "'] == 'custom'"),
                numericInput(ns("export_dpi_custom"), "Custom DPI:", value = 300, min = 72, max = 1200, step = 50)
              )
            )
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("export_save"), "Save Plot", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })
    
    # Handle directory selection
    observeEvent(input$choose_directory, {
      tryCatch({
        if (Sys.info()["sysname"] == "Darwin") {  # macOS
          # Use AppleScript to choose folder on macOS
          script <- 'choose folder with prompt "Choose Export Directory"'
          
          result <- system(paste0('osascript -e \'', script, '\''), intern = TRUE)
          
          if (length(result) > 0 && result != "" && !grepl("User canceled", result)) {
            chosen_dir <- trimws(result)
            
            # Clean up the AppleScript result
            if (startsWith(chosen_dir, "alias ")) {
              chosen_dir <- substring(chosen_dir, 7)  # Remove "alias " prefix
            }
            
            # Convert from HFS path to POSIX path
            if (grepl(":", chosen_dir)) {
              # This is an HFS path, convert to POSIX
              # Remove "Macintosh HD:" prefix if present and replace colons with slashes
              if (startsWith(chosen_dir, "Macintosh HD:")) {
                chosen_dir <- substring(chosen_dir, 14)  # Remove "Macintosh HD:" prefix
              }
              
              # Replace all colons with forward slashes
              chosen_dir <- gsub(":", "/", chosen_dir)
              
              # Add leading slash to make it absolute
              if (!startsWith(chosen_dir, "/")) {
                chosen_dir <- paste0("/", chosen_dir)
              }
              
              # Remove trailing slash if present
              if (endsWith(chosen_dir, "/") && nchar(chosen_dir) > 1) {
                chosen_dir <- substring(chosen_dir, 1, nchar(chosen_dir) - 1)
              }
            }
            
            selected_export_dir(chosen_dir)
          }
        } else {
          # Use utils::choose.dir() for Windows/Linux
          chosen_dir <- utils::choose.dir(default = selected_export_dir(), caption = "Select Export Directory")
          
          if (!is.null(chosen_dir) && chosen_dir != "") {
            selected_export_dir(chosen_dir)
          }
        }
      }, error = function(e) {
        showNotification(
          paste("Directory selection failed:", e$message),
          type = "warning",
          duration = 3
        )
      })
    })
    
    # Display selected directory
    output$selected_directory <- renderText({
      paste("Save to:", selected_export_dir())
    })
    
    # Handle the actual save operation
    observeEvent(input$export_save, {
      tryCatch({
        # Get current plot from reactive value
        plot_obj <- current_plot()
        
        if (is.null(plot_obj)) {
          showNotification("No plot to export", type = "error", duration = 3)
          return()
        }
        
        # Get export settings from modal inputs
        filename <- input$export_filename
        format <- input$export_format
        width <- input$export_width
        height <- input$export_height
        
        # Handle DPI selection (only for non-PDF formats)
        dpi <- if (format != "pdf") {
          if (input$export_dpi == "custom") {
            input$export_dpi_custom
          } else {
            as.numeric(input$export_dpi)
          }
        } else {
          300  # Default for PDF (not used)
        }
        
        # Create full file path using selected directory
        full_path <- file.path(selected_export_dir(), paste0(filename, ".", format))
        
        # Save the currently displayed plot with format-specific parameters
        if (format == "pdf") {
          ggsave(
            filename = full_path,
            plot = plot_obj,
            device = format,
            width = width,
            height = height,
            units = "in"
          )
        } else {
          ggsave(
            filename = full_path,
            plot = plot_obj,
            device = format,
            width = width,
            height = height,
            units = "in",
            dpi = dpi
          )
        }
        
        # Close modal and show success message
        removeModal()
        showNotification(
          paste("Plot saved successfully to:", full_path),
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        showNotification(
          paste("Export failed:", e$message),
          type = "error",
          duration = 5
        )
      })
    })
    
    # Test basic button functionality  
    observeEvent(input$update_plot, {
      # This should work - basic button test
    })
    
    # Detect when mesenchyme tab is accessed and trigger loading
    observeEvent(parent_session$input$tabs, {
      if (parent_session$input$tabs == "mesenchyme" && !data_loaded()) {
        show_loading(TRUE)
        showModal(modalDialog(
          tags$div(
            style = "display: flex; align-items: center; gap: 18px; padding: 10px 0;",
            tags$div(
              class = "fa fa-spinner fa-spin",
              style = "font-size: 38px; color: #337ab7;"
            ),
            tags$div(
              tags$h4("Loading Data", style = "margin: 0 0 6px 0; font-weight: bold; color: #337ab7;"),
              tags$p(
                "Loading Mesenchyme RNAseq data... Please wait while we prepare the gene expression data.",
                style = "font-size: 16px; margin: 0; color: #333;"
              )
            )
          ),
          footer = NULL,
          easyClose = FALSE,
          size = "m"
        ))
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
    
    # Auto-load Tbx18 plot on module initialization
    observe({
      # Auto-trigger the plot for Tbx18 when the module initializes
      if (!initial_load() && !is.null(input$mesenchyme_genes) && 
          input$mesenchyme_genes == "Tbx18" && !is.null(input$selected_groups)) {
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
      if (is.null(input$mesenchyme_genes) || input$mesenchyme_genes == "" ||
          is.null(input$selected_groups)) {
        return(NULL)
      }
      
      # Clean and validate the gene name
      gene_name <- trimws(input$mesenchyme_genes)
      available_genes <- unique(data$external_gene_name)
      
      # Check if gene exists in the dataset
      if (!gene_name %in% available_genes) {
        return(data.frame(error = paste0("Gene '", gene_name, "' not found in dataset.")))
      }
      
      # Map the selected group key to actual group names
      selected_group_names <- switch(input$selected_groups,
        "e13_mammary_vs_smg" = c("E13.5_Mammary_Mes", "E13.5_SMG_Mes"),
        "e16_vs_e13_mammary" = c("E16.5_Mammary_Mes", "E13.5_Mammary_Mes"),
        "e16_mammary_vs_smg" = c("E16.5_Mammary_Mes", "E13.5_SMG_Mes"),
        "e16_fatpad_vs_e13_mammary" = c("E16.5_Fatpad", "E13.5_Mammary_Mes"),
        "e16_fatpad_vs_smg" = c("E16.5_Fatpad", "E13.5_SMG_Mes"),
        "e13_skin_vs_mammary" = c("E13.5_Skin_Mes_Ventral", "E13.5_Mammary_Mes"),
        "all_groups" = c("E13.5_Skin_Mes_Ventral", "E13.5_Mammary_Mes", 
                        "E16.5_Mammary_Mes", "E16.5_Fatpad", "E13.5_SMG_Mes"),
        c("E13.5_Mammary_Mes", "E13.5_SMG_Mes") # default fallback
      )
      
      # Filter for selected gene and groups
      selected_count <- data %>% 
        dplyr::filter(external_gene_name == gene_name) %>%
        dplyr::filter(groups %in% selected_group_names) %>%
        mutate(
          external_gene_name = factor(external_gene_name, levels = gene_name),
          groups = factor(groups, levels = c('E13.5_Skin_Mes_Ventral', 'E13.5_Mammary_Mes', 
                                            'E16.5_Mammary_Mes', 'E16.5_Fatpad', 'E13.5_SMG_Mes'))
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
        group_by(groups, external_gene_name) %>% 
        summarise(mean = mean(normalized_count), std = sd(normalized_count), .groups = 'drop')
      
      # Get gene name from the data
      current_gene_name <- unique(selected_count$external_gene_name)[1]
      
      # Merge with original data
      df_plotting_with_std <- merge(selected_count, df_mean, by = c('external_gene_name', 'groups')) %>% 
        mutate(
          external_gene_name = factor(external_gene_name, levels = trimws(input$mesenchyme_genes)),
          groups = factor(groups, levels = c('E13.5_Skin_Mes_Ventral', 'E13.5_Mammary_Mes', 
                                            'E16.5_Mammary_Mes', 'E16.5_Fatpad', 'E13.5_SMG_Mes'))
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
      all_groups <- c('E13.5_Skin_Mes_Ventral', 'E13.5_Mammary_Mes', 
                     'E16.5_Mammary_Mes', 'E16.5_Fatpad', 'E13.5_SMG_Mes')
      present_groups_ordered <- all_groups[all_groups %in% present_groups]
      group_position_map <- setNames(seq_along(present_groups_ordered), present_groups_ordered)
      
      # Check which comparisons are possible based on present groups and add annotations
      comparison_mappings <- list(
        list(groups = c("E13.5_Mammary_Mes", "E13.5_SMG_Mes"), comparison = "E13.5_Mammary_Mes_vs_E13.5_SMG_Mes"),
        list(groups = c("E16.5_Mammary_Mes", "E13.5_Mammary_Mes"), comparison = "E16.5_Mammary_Mes_vs_E13.5_Mammary_Mes"),
        list(groups = c("E16.5_Mammary_Mes", "E13.5_SMG_Mes"), comparison = "E16.5_Mammary_Mes_vs_E13.5_SMG_Mes"),
        list(groups = c("E16.5_Fatpad", "E13.5_Mammary_Mes"), comparison = "E16.5_Fatpad_vs_E13.5_Mammary_Mes"),
        list(groups = c("E16.5_Fatpad", "E13.5_SMG_Mes"), comparison = "E16.5_Fatpad_vs_E13.5_SMG_Mes"),
        list(groups = c("E13.5_Skin_Mes_Ventral", "E13.5_Mammary_Mes"), comparison = "E13.5_Skin_Mes_Ventral_vs_E13.5_Mammary_Mes")
      )
      
      for (mapping in comparison_mappings) {
        if (all(mapping$groups %in% present_groups)) {
          result <- gene_results[gene_results$comparison == mapping$comparison, ]
          if (nrow(result) > 0) {
            pval <- result$padj[1]
            lfc <- round(result$log2FoldChange[1], 2)
            sig_symbol <- if (is.na(pval)) "ns" else if (pval < 0.001) "***" else if (pval < 0.01) "**" else if (pval < 0.05) "*" else "ns"
            
            # Calculate middle position between the two groups
            x_pos <- (group_position_map[mapping$groups[1]] + group_position_map[mapping$groups[2]]) / 2
            
            annotations <- append(annotations, list(list(
              x = x_pos, y = y_positions[min(length(annotations) + 1, length(y_positions))], 
              label = sig_symbol,
              comparison = mapping$groups
            )))
          }
        }
      }
      
      return(annotations)
    }
    
    # Create the base plot (optimized for single gene)
    create_base_plot <- function(data) {
      # Get the groups actually present in the data
      present_groups <- unique(data$groups)
      
      # Create labels mapping for only the present groups
      group_labels <- c('E13.5_SMG_Mes' = 'E13.5 SMG Mes', 
                       'E13.5_Mammary_Mes' = 'E13.5 Mammary Mes',
                       'E13.5_Skin_Mes_Ventral' = 'E13.5 Skin Mes',
                       'E16.5_Mammary_Mes' = 'E16.5 Mammary Mes', 
                       'E16.5_Fatpad' = 'E16.5 Fatpad')
      
      # Filter labels to only include present groups
      filtered_labels <- group_labels[names(group_labels) %in% present_groups]
      
      p <- ggplot(data, aes(x = groups, y = normalized_count, color = groups)) +
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
                 width = 0.7, aes(fill = groups), linewidth = 0.4, alpha = 0.6, show.legend = FALSE) +
        geom_quasirandom(show.legend = FALSE, size = 3, alpha = 0.8) +
        scale_fill_manual(
          values = c('E13.5_SMG_Mes' = '#91D1C2FF', 'E13.5_Mammary_Mes' = '#4DBBD5FF', 
                     'E13.5_Skin_Mes_Ventral' = '#8491B4FF', 'E16.5_Mammary_Mes' = '#F39B7FFF', 
                     'E16.5_Fatpad' = '#E64B35FF'), 
          labels = c("E13.5 SMG Mes", "E13.5 Mammary Mes", "E13.5 Skin Mes", "E16.5 Mammary Mes", "E16.5 Fatpad"), 
          name = "Group"
        ) +
        scale_color_manual(
          values = c('E13.5_SMG_Mes' = '#008E51', 'E13.5_Mammary_Mes' = '#3C5488FF',
                     'E13.5_Skin_Mes_Ventral' = '#5C5C5CFF', 'E16.5_Mammary_Mes' = '#E64B35FF', 
                     'E16.5_Fatpad' = '#DC0000FF'),
          labels = c("E13.5 SMG Mes", "E13.5 Mammary Mes", "E13.5 Skin Mes", "E16.5 Mammary Mes", "E16.5 Fatpad"), 
          name = "Group"
        ) +
        xlab("") +
        scale_x_discrete(labels = filtered_labels) +
        ylab("Normalized Counts") +
        geom_errorbar(
          aes(ymin = mean - std, ymax = mean + std, color = groups),
          width = 0.3, 
          linewidth = 0.3, 
          show.legend = FALSE
        )
      
      return(p)
    }
    
    # Main expression plot
    output$mesenchyme_main_plot <- renderPlot({
      # Show loading message if explicitly set to show loading
      if (show_loading()) {
        # Start data loading in background only if not already started
        if (is.null(cached_normalized_data())) {
          # Use a delayed observer to load data
          observe({
            if (show_loading() && is.null(cached_normalized_data())) {
              
              tryCatch({
                # Load Lan et al data
                dds <- readRDS("rawData/Lan_et_al/All_data/Deseq_dds_normalized_matrix_Mes.rds")
                gene_annotation <- readRDS("rawData/Lan_et_al/All_data/gene_annotation.rds")
                sample_table <- read.csv("rawData/Lan_et_al/All_data/sampleTable.csv", stringsAsFactors = TRUE)
            
                # Convert to long format - note that Lan data uses 'groups' not 'group'
                normalized_count <- dds %>% 
                  as.data.frame() %>% 
                  .[unique(gene_annotation$ensembl_gene_id),] %>% 
                  rownames_to_column("ensembl_gene_id") %>% 
                  pivot_longer(., 2:ncol(.), names_to = "sample", values_to = "normalized_count") %>% 
                  merge(., sample_table[, c("sampleName", "groups")], by.x = "sample", by.y = "sampleName") %>% 
                  mutate(groups = factor(groups, levels = c("E13.5_Skin_Mes_Ventral", "E13.5_Mammary_Mes", 
                                                           "E16.5_Mammary_Mes", "E16.5_Fatpad", "E13.5_SMG_Mes"))) %>% 
                  merge(gene_annotation[, 1:2], by = "ensembl_gene_id")
                
                # Load DESeq2 comparison files
                deseq_files <- list.files("rawData/Lan_et_al/All_data/", pattern = "*_LFC_shrinkaged.csv", full.names = TRUE)
                deseq_list <- list()
                
                for (file in deseq_files) {
                  # Extract comparison name from filename
                  basename_file <- basename(file)
                  comparison_name <- gsub("^[0-9]+_groups_", "", basename_file)
                  comparison_name <- gsub("_LFC_shrinkaged.csv$", "", comparison_name)
                  
                  # Read the file, skip first column (row numbers), and rename Row.names to Ensembl_ID
                  data <- read.csv(file, row.names = NULL)
                  
                  # Remove the first column if it's just row numbers (X column)
                  if (names(data)[1] %in% c("X", "X.1")) {
                    data <- data[, -1]
                  }
                  
                  # Rename Row.names column to Ensembl_ID if it exists
                  if ("Row.names" %in% names(data)) {
                    names(data)[names(data) == "Row.names"] <- "Ensembl_ID"
                  }
                  
                  data$comparison <- comparison_name
                  deseq_list[[comparison_name]] <- data
                }
                
                # Combine all DESeq2 results
                combined_results <- do.call(rbind, deseq_list)
                
                # Cache the data
                cached_normalized_data(normalized_count)
                cached_deseq2_data(combined_results)
                
                # Update selectizeInput choices once data is loaded
                available_genes <- sort(unique(normalized_count$external_gene_name))
                updateSelectizeInput(session, "mesenchyme_genes", 
                                    choices = available_genes,
                                    selected = "Tbx18",
                                    server = TRUE)
                
                # Mark data as loaded and hide loading message
                data_loaded(TRUE)
                show_loading(FALSE)
                removeModal()  # Remove the loading modal
                
                
              }, error = function(e) {
               
                data_loaded(TRUE)
                show_loading(FALSE)
                removeModal()  # Remove the modal if error occurs
              })
            }
          })
        }
        
        # Return the loading plot
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                       label = "Loading Mesenchyme RNAseq data...\nPlease wait while we prepare the gene expression data.", 
                       size = 12, color = "#337ab7", hjust = 0.5, vjust = 0.5, fontface = "bold") +
               theme_void() +
               theme(plot.margin = margin(50, 50, 50, 50)) +
               xlim(0, 1) + ylim(0, 1))
      }
      
      # Show default loading message when data is not loaded (fallback)
      if (!data_loaded()) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                       label = "Loading data, please wait...", 
                       size = 8, color = "#337ab7") +
               theme_void() +
               theme(plot.margin = margin(50, 50, 50, 50)) +
               xlim(0, 1) + ylim(0, 1))
      }
      
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
      gene_name <- if (!is.null(input$mesenchyme_genes) && input$mesenchyme_genes != "") {
        as.character(input$mesenchyme_genes)
      } else {
        as.character(unique(data$external_gene_name)[1])
      }
      
      # Add different styling based on user selection
      if (input$plot_style == "ttest") {
        # Add t-test comparison only if we have the right groups
        present_group_levels <- unique(data$groups)
        
        # Define potential comparisons based on selected groups
        comparisons <- list()
        
        # Add all possible pairwise comparisons for the groups present
        if (all(c("E13.5_Mammary_Mes", "E13.5_SMG_Mes") %in% present_group_levels)) {
          comparisons <- append(comparisons, list(c("E13.5_Mammary_Mes", "E13.5_SMG_Mes")))
        }
        if (all(c("E16.5_Mammary_Mes", "E13.5_Mammary_Mes") %in% present_group_levels)) {
          comparisons <- append(comparisons, list(c("E16.5_Mammary_Mes", "E13.5_Mammary_Mes")))
        }
        if (all(c("E16.5_Mammary_Mes", "E13.5_SMG_Mes") %in% present_group_levels)) {
          comparisons <- append(comparisons, list(c("E16.5_Mammary_Mes", "E13.5_SMG_Mes")))
        }
        if (all(c("E16.5_Fatpad", "E13.5_Mammary_Mes") %in% present_group_levels)) {
          comparisons <- append(comparisons, list(c("E16.5_Fatpad", "E13.5_Mammary_Mes")))
        }
        if (all(c("E16.5_Fatpad", "E13.5_SMG_Mes") %in% present_group_levels)) {
          comparisons <- append(comparisons, list(c("E16.5_Fatpad", "E13.5_SMG_Mes")))
        }
        if (all(c("E13.5_Skin_Mes_Ventral", "E13.5_Mammary_Mes") %in% present_group_levels)) {
          comparisons <- append(comparisons, list(c("E13.5_Skin_Mes_Ventral", "E13.5_Mammary_Mes")))
        }
        
        # Create descriptive title based on selected groups
        title_suffix <- switch(input$selected_groups,
          "e13_mammary_vs_smg" = "(E13.5: Mammary vs SMG Mes)",
          "e16_vs_e13_mammary" = "(E16.5 vs E13.5 Mammary Mes)",
          "e16_mammary_vs_smg" = "(E16.5 Mammary vs SMG Mes)",
          "e16_fatpad_vs_e13_mammary" = "(E16.5 Fatpad vs E13.5 Mammary Mes)",
          "e16_fatpad_vs_smg" = "(E16.5 Fatpad vs SMG Mes)",
          "e13_skin_vs_mammary" = "(E13.5: Skin vs Mammary Mes)",
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
        present_group_levels <- unique(data$groups)
        annotations <- create_deseq2_annotations(gene_name, present_group_levels)
        
        # Create descriptive title based on selected groups
        title_suffix <- switch(input$selected_groups,
          "e13_mammary_vs_smg" = "(E13.5: Mammary vs SMG Mes)",
          "e16_vs_e13_mammary" = "(E16.5 vs E13.5 Mammary Mes)",
          "e16_mammary_vs_smg" = "(E16.5 Mammary vs SMG Mes)",
          "e16_fatpad_vs_e13_mammary" = "(E16.5 Fatpad vs E13.5 Mammary Mes)",
          "e16_fatpad_vs_smg" = "(E16.5 Fatpad vs SMG Mes)",
          "e13_skin_vs_mammary" = "(E13.5: Skin vs Mammary Mes)",
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
                             y = y_pos + y_spacing * 0.2,
                             label = ann$label, 
                             size = 6, 
                             hjust = 0.5,
                             color = "black")
            
            # Add bracket for comparison if both groups are present
            if (all(ann$comparison %in% present_group_levels)) {
              # Find the actual positions of the comparison groups in the current plot
              all_groups <- c('E13.5_Skin_Mes_Ventral', 'E13.5_Mammary_Mes', 
                             'E16.5_Mammary_Mes', 'E16.5_Fatpad', 'E13.5_SMG_Mes')
              present_groups_ordered <- all_groups[all_groups %in% present_group_levels]
              group_position_map <- setNames(seq_along(present_groups_ordered), present_groups_ordered)
              
              if (all(ann$comparison %in% names(group_position_map))) {
                x1 <- group_position_map[ann$comparison[1]]
                x2 <- group_position_map[ann$comparison[2]]
                bracket_y <- y_pos - y_spacing * 0.3
                
                # Only draw brackets if the groups are not adjacent (to avoid overcrowding)
                if (abs(x2 - x1) > 0.5) {
                  p <- p + 
                    annotate("segment", x = x1, xend = x2, y = bracket_y, yend = bracket_y, linewidth = 0.5) +
                    annotate("segment", x = x1, xend = x1, y = bracket_y - y_spacing * 0.1, yend = bracket_y, linewidth = 0.5) +
                    annotate("segment", x = x2, xend = x2, y = bracket_y - y_spacing * 0.1, yend = bracket_y, linewidth = 0.5)
                }
              }
            }
          }
        }
        
        p <- p + ggtitle(paste0(gene_name, " Expression ", title_suffix, "\n(DESeq2 Wald Test)")) +
         theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      }
      
      # Store the plot for export
      current_plot(p)
      
      return(p)
    })

    # Load DESeq2 comparison tables for the data tables
    deseq_comparisons <- reactive({
      # Try to use the cached deseq2_data, but fall back to individual file loading if needed
      combined_data <- deseq2_data()
      
      if (!is.null(combined_data)) {
        # Split the combined data back into individual comparisons
        list(
          e13_mammary_vs_smg = combined_data[combined_data$comparison == "E13.5_Mammary_Mes_vs_E13.5_SMG_Mes", ],
          e16_mammary_vs_e13_mammary = combined_data[combined_data$comparison == "E16.5_Mammary_Mes_vs_E13.5_Mammary_Mes", ],
          e16_mammary_vs_smg = combined_data[combined_data$comparison == "E16.5_Mammary_Mes_vs_E13.5_SMG_Mes", ],
          e16_fatpad_vs_e13_mammary = combined_data[combined_data$comparison == "E16.5_Fatpad_vs_E13.5_Mammary_Mes", ],
          e16_fatpad_vs_smg = combined_data[combined_data$comparison == "E16.5_Fatpad_vs_E13.5_SMG_Mes", ],
          e13_skin_vs_mammary = combined_data[combined_data$comparison == "E13.5_Skin_Mes_Ventral_vs_E13.5_Mammary_Mes", ]
        )
      } else {
        # Return empty data frames if files cannot be loaded
        list(
          e13_mammary_vs_smg = data.frame(Error = "Unable to load comparison data"),
          e16_mammary_vs_e13_mammary = data.frame(Error = "Unable to load comparison data"),
          e16_mammary_vs_smg = data.frame(Error = "Unable to load comparison data"),
          e16_fatpad_vs_e13_mammary = data.frame(Error = "Unable to load comparison data"),
          e16_fatpad_vs_smg = data.frame(Error = "Unable to load comparison data"),
          e13_skin_vs_mammary = data.frame(Error = "Unable to load comparison data")
        )
      }
    })
    
    # Render data tables
    output$deseq_table1 <- DT::renderDataTable({
      DT::datatable(
        deseq_comparisons()$e13_mammary_vs_smg,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = "E13.5 Mammary vs SMG Mes Comparison"
      )
    })
    
    output$deseq_table2 <- DT::renderDataTable({
      DT::datatable(
        deseq_comparisons()$e16_mammary_vs_e13_mammary,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = "E16.5 Mammary Mes vs E13.5 Mammary Mes Comparison"
      )
    })
    
    output$deseq_table3 <- DT::renderDataTable({
      DT::datatable(
        deseq_comparisons()$e16_mammary_vs_smg,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = "E16.5 Mammary Mes vs SMG Mes Comparison"
      )
    })
    
    output$deseq_table4 <- DT::renderDataTable({
      DT::datatable(
        deseq_comparisons()$e16_fatpad_vs_e13_mammary,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = "E16.5 Fatpad vs E13.5 Mammary Mes Comparison"
      )
    })
    
    output$deseq_table5 <- DT::renderDataTable({
      DT::datatable(
        deseq_comparisons()$e16_fatpad_vs_smg,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = "E16.5 Fatpad vs SMG Mes Comparison"
      )
    })
    
    output$deseq_table6 <- DT::renderDataTable({
      DT::datatable(
        deseq_comparisons()$e13_skin_vs_mammary,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = "E13.5 Skin Mes vs Mammary Mes Comparison"
      )
    })
    
  })
}