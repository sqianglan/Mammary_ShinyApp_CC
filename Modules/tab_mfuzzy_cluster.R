# mFuzzy Cluster Analysis Module

tab_mfuzzy_clusterUI <- function(id) {
  ns <- NS(id)
  
  tabBox(
    title = "mFuzzy Cluster Analysis", width = 12,
    tabPanel("Cluster Data", 
      fluidRow(
        column(3,
          div(
            DT::dataTableOutput(ns("mfuzzy_table"))
          )
        ),
        column(5,
          div(style = "text-align: center; padding: 5px;",
            h4("Gene Expression in Mammary Mesenchyme", style = "margin-bottom: 15px; color: #337ab7;"),
            p("Click a gene in the table to view expression", style = "font-size: 12px; color: #666; margin-bottom: 15px;"),
            shinycssloaders::withSpinner(
              plotOutput(ns("gene_expression_plot"), height = "450px"),
              type = 4,
              color = "#337ab7",
              size = 0.5
            )
          )
        ),
        column(4,
          div(style = "text-align: center; padding: 10px;",
            div(style = "margin-bottom: 15px;",
              uiOutput(ns("dynamic_buttons"))
            ),
            uiOutput(ns("dynamic_image_panel"))
          )
        )
      )
    )
  )
}

tab_mfuzzy_clusterServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Store the namespace function
    ns <- session$ns
    
    # Load mFuzzy cluster data
    mfuzzy_data <- reactive({
      tryCatch({
        data <- read.csv("rawData/Lan_et_al/mFuzzy_Cluster/66_normalized_count_MFuzz_k9_seed20_cluster_EL.csv", 
                        row.names = NULL)
        
        # Clean up the data - remove row numbers column and rename Row.names to Gene_ID
        if (names(data)[1] %in% c("X", "X.1")) {
          data <- data[, -1]
        }
        if ("Row.names" %in% names(data)) {
          names(data)[names(data) == "Row.names"] <- "Gene_ID"
        }
        
        return(data)
      }, error = function(e) {
        data.frame(Error = paste("Failed to load mFuzzy cluster data:", e$message))
      })
    })
    
    # Load Lan et al mesenchyme data for plotting
    lan_mesenchyme_data <- reactive({
      tryCatch({
        dds <- readRDS("rawData/Lan_et_al/All_data/Deseq_dds_normalized_matrix_Mes.rds")
        gene_annotation <- readRDS("rawData/Lan_et_al/All_data/gene_annotation.rds")
        sample_table <- read.csv("rawData/Lan_et_al/All_data/sampleTable.csv", stringsAsFactors = TRUE)
    
        # Convert to long format
        normalized_count <- dds %>% 
          as.data.frame() %>% 
          .[unique(gene_annotation$ensembl_gene_id),] %>% 
          rownames_to_column("ensembl_gene_id") %>% 
          pivot_longer(., 2:ncol(.), names_to = "sample", values_to = "normalized_count") %>% 
          merge(., sample_table[, c("sampleName", "groups")], by.x = "sample", by.y = "sampleName") %>% 
          mutate(groups = factor(groups, levels = c("E13.5_Skin_Mes_Ventral", "E13.5_Mammary_Mes", 
                                                   "E16.5_Mammary_Mes", "E16.5_Fatpad", "E13.5_SMG_Mes"))) %>% 
          merge(gene_annotation[, 1:2], by = "ensembl_gene_id")
        
        return(normalized_count)
      }, error = function(e) {
        data.frame(Error = paste("Failed to load mesenchyme data:", e$message))
      })
    })
    
    # Get selected gene from table row selection
    selected_gene <- reactive({
      req(input$mfuzzy_table_rows_selected)
      data <- mfuzzy_data()
      if (!is.null(data) && !"Error" %in% names(data)) {
        cluster_data <- data[, c("Gene_ID", "CLUSTER"), drop = FALSE]
        selected_row <- input$mfuzzy_table_rows_selected
        return(cluster_data$Gene_ID[selected_row])
      }
      return(NULL)
    })
    
    # Render mFuzzy cluster data table (only Gene_ID and CLUSTER columns)
    output$mfuzzy_table <- DT::renderDataTable({
      data <- mfuzzy_data()
      
      if ("Error" %in% names(data)) {
        DT::datatable(
          data,
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE,
          caption = "Error loading mFuzzy cluster data"
        )
      } else {
        # Select only Gene_ID and CLUSTER columns
        cluster_data <- data[, c("Gene_ID", "CLUSTER"), drop = FALSE]
        
        DT::datatable(
          cluster_data,
          selection = "single",  # Enable single row selection
          options = list(
            paging = FALSE,  # Remove pagination
            searching = TRUE,
            scrollY = "350px",
            scrollX = FALSE,
            columnDefs = list(
              list(width = "150px", targets = 0),  # Gene_ID column
              list(width = "80px", targets = 1),   # CLUSTER column
              list(className = "dt-center", targets = 1)
            )
          ),
          rownames = FALSE,
          caption = "mFuzzy Cluster Genes (k=9, seed=20)"
        )
      }
    })
    
    # Image state for three-way switching
    image_state <- reactiveVal("selected_data")  # Start with selected data view
    
    # Dynamic buttons with active styling
    output$dynamic_buttons <- renderUI({
      current_state <- image_state()
      
      div(style = "display: flex; justify-content: center; gap: 5px; flex-wrap: wrap;",
        actionButton(ns("btn_selected_data"), "Selected Data", 
                    style = paste0("border: none; padding: 6px 12px; border-radius: 4px; font-size: 11px; margin: 2px; ",
                                  if(current_state == "selected_data") "background-color: #337ab7; color: white;" else "background-color: #6c757d; color: white;")),
        actionButton(ns("btn_clustering"), "Clustering Results", 
                    style = paste0("border: none; padding: 6px 12px; border-radius: 4px; font-size: 11px; margin: 2px; ",
                                  if(current_state == "clustering") "background-color: #337ab7; color: white;" else "background-color: #6c757d; color: white;")),
        actionButton(ns("btn_heatmap"), "Heatmap", 
                    style = paste0("border: none; padding: 6px 12px; border-radius: 4px; font-size: 11px; margin: 2px; ",
                                  if(current_state == "heatmap") "background-color: #337ab7; color: white;" else "background-color: #6c757d; color: white;"))
      )
    })
    
    # Handle button clicks and update button styles
    observeEvent(input$btn_selected_data, {
      image_state("selected_data")
    })
    
    observeEvent(input$btn_clustering, {
      image_state("clustering")
    })
    
    observeEvent(input$btn_heatmap, {
      image_state("heatmap")
    })
    
    # Dynamic image panel output
    output$dynamic_image_panel <- renderUI({
      current_state <- image_state()
      
      if (current_state == "selected_data") {
        div(
          h4("Cluster Analysis Workflow", style = "margin-bottom: 10px; color: #337ab7;"),
          img(src = "mFuzzy_sample_selection.png", width = "100%", 
              style = "margin-bottom: 15px; border: 1px solid #ddd; border-radius: 5px;"),
          p("Sample selection for mFuzzy clustering analysis", 
            style = "font-size: 12px; color: #666; margin-bottom: 8px; font-weight: bold;"),
          p("Differential expressed (FC >1.5, padj <0.05, readcounts >200) extracellular matrix and ligand genes from multiple databases were extracted for Mfuzz analysis (644 total).", 
            style = "font-size: 10px; color: #555; margin-bottom: 10px;")
        )
      } else if (current_state == "clustering") {
        div(
          h4("Clustering Analysis (k=9, seed=20)", style = "margin-bottom: 10px; color: #337ab7;"),
          img(src = "51_clustering_with_Mfuzz_k9_seed20_core_plottinig.png", width = "100%", 
              style = "margin-bottom: 15px; border: 1px solid #ddd; border-radius: 5px;"),
          p("mFuzzy clustering results", 
            style = "font-size: 12px; color: #666; margin-bottom: 8px; font-weight: bold;"),
          p("Clustering analysis showing temporal expression patterns across developmental stages with k=9 clusters and seed=20.", 
            style = "font-size: 10px; color: #555; margin-bottom: 10px;")
        )
      } else {  # heatmap
        div(
          h4("Compiled Heatmap", style = "margin-bottom: 10px; color: #337ab7;"),
          img(src = "46_mFuzzy_complied_heatmap.png", width = "80%", 
              style = "margin-bottom: 15px; border: 1px solid #ddd; border-radius: 5px;"),
          p("Compiled heatmap visualization", 
            style = "font-size: 12px; color: #666; margin-bottom: 8px; font-weight: bold;"),
          p("Heatmap showing expression patterns of mFuzzy clustered genes across different developmental stages and tissue types.", 
            style = "font-size: 10px; color: #555; margin-bottom: 10px;")
        )
      }
    })
    
    # Filter data based on selected gene from table
    filtered_data <- reactive({
      gene_name <- selected_gene()
      if (is.null(gene_name)) {
        return(NULL)
      }
      
      data <- lan_mesenchyme_data()
      
      # Check if data is still loading
      if (is.null(data)) {
        return(NULL)
      }
      
      # Check if gene exists in the dataset
      available_genes <- unique(data$external_gene_name)
      if (!gene_name %in% available_genes) {
        return(data.frame(error = paste0("Gene '", gene_name, "' not found in dataset.")))
      }
      
      # Filter for selected gene (show all groups)
      selected_count <- data %>% 
        dplyr::filter(external_gene_name == gene_name) %>%
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
        return(selected_count)
      }
      
      if (nrow(selected_count) == 0) {
        return(data.frame(error = "No data found for the selected gene."))
      }
      
      # Calculate means and standard deviations
      df_mean <- selected_count %>% 
        group_by(groups, external_gene_name) %>% 
        summarise(mean = mean(normalized_count), std = sd(normalized_count), .groups = 'drop')
      
      # Merge with original data
      df_plotting_with_std <- merge(selected_count, df_mean, by = c('external_gene_name', 'groups')) %>% 
        mutate(
          external_gene_name = factor(external_gene_name, levels = selected_gene()),
          groups = factor(groups, levels = c('E13.5_Skin_Mes_Ventral', 'E13.5_Mammary_Mes', 
                                            'E16.5_Mammary_Mes', 'E16.5_Fatpad', 'E13.5_SMG_Mes'))
        )
      
      return(df_plotting_with_std)
    })
    
    # Create the base plot (same as mesenchyme module)
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
          values = c('E13.5_Skin_Mes_Ventral' = '#A6CEE3', 'E13.5_Mammary_Mes' = '#1F78B4', 
                     'E16.5_Mammary_Mes' = '#B2DF8A', 'E16.5_Fatpad' = '#33A02C', 
                     'E13.5_SMG_Mes' = '#FB9A99'), 
          labels = c("E13.5 Skin Mes", "E13.5 Mammary Mes", "E16.5 Mammary Mes", "E16.5 Fatpad", "E13.5 SMG Mes"), 
          name = "Group"
        ) +
        scale_color_manual(
          values = c('E13.5_Skin_Mes_Ventral' = '#4A90A4', 'E13.5_Mammary_Mes' = '#1F5582', 
                     'E16.5_Mammary_Mes' = '#5B8A3A', 'E16.5_Fatpad' = '#2E7D32', 
                     'E13.5_SMG_Mes' = '#C2717A'),
          labels = c("E13.5 Skin Mes", "E13.5 Mammary Mes", "E16.5 Mammary Mes", "E16.5 Fatpad", "E13.5 SMG Mes"), 
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
    output$gene_expression_plot <- renderPlot({
      data <- plotting_data()
      
      # Handle data loading state
      if (is.null(data)) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                       label = "Click a gene in the table to view expression", 
                       size = 6, color = "gray50") +
               theme_void() +
               theme(plot.margin = margin(50, 50, 50, 50)))
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
                       label = "No data found for the selected gene", 
                       size = 6, color = "gray50") +
               theme_void() +
               theme(plot.margin = margin(50, 50, 50, 50)))
      }
      
      p <- create_base_plot(data)
      
      # Get the gene name for the title
      gene_name <- as.character(unique(data$external_gene_name)[1])
      
      p <- p + ggtitle(paste0(gene_name, " Expression (All Groups)")) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
      return(p)
    })
    
  })
}