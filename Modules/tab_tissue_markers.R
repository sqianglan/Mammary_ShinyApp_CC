# Tissue Markers Analysis Module

tab_tissue_markersUI <- function(id) {
  ns <- NS(id)
  
  tabBox(
    title = "Tissue Markers Analysis", width = 12,
    tabPanel("Tissue Markers Data", 
      fluidRow(
        column(3,
          div(
            DT::dataTableOutput(ns("tissue_markers_table"))
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
            h4("Tissue Marker Expression", style = "margin-bottom: 10px; color: #337ab7;"),
            img(src = "23_complied_heatmap_pair_comapair_count_100_FC_2_markers_rlog_selected_genes.png", 
                width = "100%", style = "margin-bottom: 15px; margin-top: 20px; border: 1px solid #ddd; border-radius: 5px;"),
            p("Tissue-specific marker genes", 
              style = "font-size: 12px; color: #666; margin-bottom: 8px; font-weight: bold;"),
            p("Threshold: readcounts per group >100 and Fold change ≥2 and unique in one group", 
              style = "font-size: 10px; color: #555; margin-bottom: 10px;")
          )
        )
      )
    )
  )
}

tab_tissue_markersServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Define ColorBrewer Paired colors and tissue order (3,2,4,5,1)
    color_palette <- RColorBrewer::brewer.pal(5, "Paired")
    tissue_order <- c("E13.5_SMG_Mes", "E13.5_Skin_Mes_Ventral", "E16.5_Fatpad", 
                      "E16.5_Mammary_Mes", "E13.5_Mammary_Mes")
    
    # Load tissue markers data
    tissue_markers_data <- reactive({
      tryCatch({
        data <- read.csv("rawData/Lan_et_al/Tissue_markers/16_count_Matrix_top_all_count_100_FC_2_markers.csv", 
                        row.names = NULL)
        
        # Clean up the data - remove row numbers column if present
        if (names(data)[1] %in% c("X", "X.1")) {
          data <- data[, -1]
        }
        
        # Rename first column to Gene_ID for consistency
        if (names(data)[1] == "Genes") {
          names(data)[1] <- "Gene_ID"
        }
        
        return(data)
      }, error = function(e) {
        data.frame(Error = paste("Failed to load tissue markers data:", e$message))
      })
    })
    
    # Load gene expression data for plotting (same as mFuzzy cluster)
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
    
    # Create simplified table data (only Gene_ID and Tissue_Marker columns)
    simplified_table_data <- reactive({
      data <- tissue_markers_data()
      if ("Error" %in% names(data)) {
        return(data)
      }
      
      # Select only Gene_ID and Markers_of_tissue columns
      marker_col <- ncol(data)
      simplified <- data[, c(1, marker_col)]
      colnames(simplified) <- c("Gene_ID", "Tissue_Marker")
      return(simplified)
    })
    
    # Render the tissue markers table (simplified like mFuzzy)
    output$tissue_markers_table <- DT::renderDataTable({
      data <- simplified_table_data()
      
      if ("Error" %in% names(data)) {
        DT::datatable(
          data,
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE,
          caption = "Error loading tissue markers data"
        )
      } else {
        # Create color mapping for tissues using ColorBrewer Paired palette
        color_mapping <- setNames(color_palette, tissue_order)
        
        # Create row colors based on tissue type
        row_colors <- color_mapping[data$Tissue_Marker]
        
        DT::datatable(
          data,
          options = list(
            paging = FALSE,  # Remove pagination
            scrollY = "350px",  # Match plot height
            scrollX = FALSE,
            columnDefs = list(
              list(targets = "_all", className = "dt-center")
            ),
            dom = 'frtip'
          ),
          rownames = FALSE,
          selection = 'single',
          caption = "Click a gene to view expression profile"
        ) %>%
          DT::formatStyle(
            0,
            backgroundColor = DT::styleEqual(seq_len(nrow(data)), paste0(row_colors, "60"))
          )
      }
    })
    
    # Plot gene expression when a gene is selected (same approach as mFuzzy cluster)
    output$gene_expression_plot <- renderPlot({
      req(input$tissue_markers_table_rows_selected)
      
      selected_row <- input$tissue_markers_table_rows_selected
      data <- simplified_table_data()
      
      if ("Error" %in% names(data)) {
        return(NULL)
      }
      
      selected_gene_symbol <- data$Gene_ID[selected_row]
      
      # Get the processed mesenchyme data
      mesenchyme_data <- lan_mesenchyme_data()
      
      if (is.null(mesenchyme_data) || "Error" %in% names(mesenchyme_data)) {
        # If data not available, show message
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, 
                           label = "Gene expression data not available", 
                           size = 6) +
          ggplot2::theme_void()
      } else {
        # Check if gene exists in the dataset
        available_genes <- unique(mesenchyme_data$external_gene_name)
        if (!selected_gene_symbol %in% available_genes) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, 
                             label = paste("Gene", selected_gene_symbol, "not found in dataset"), 
                             size = 6) +
            ggplot2::theme_void()
        } else {
          # Filter for selected gene (show all groups)
          selected_count <- mesenchyme_data %>% 
            dplyr::filter(external_gene_name == selected_gene_symbol) %>%
            mutate(
              external_gene_name = factor(external_gene_name, levels = selected_gene_symbol),
              groups = factor(groups, levels = c('E13.5_Skin_Mes_Ventral', 'E13.5_Mammary_Mes', 
                                                'E16.5_Mammary_Mes', 'E16.5_Fatpad', 'E13.5_SMG_Mes'))
            )
          
          if (nrow(selected_count) == 0) {
            ggplot2::ggplot() +
              ggplot2::annotate("text", x = 0.5, y = 0.5, 
                               label = "No data found for the selected gene", 
                               size = 6) +
              ggplot2::theme_void()
          } else {
            # Calculate means and standard deviations
            df_mean <- selected_count %>% 
              group_by(groups, external_gene_name) %>% 
              summarise(mean = mean(normalized_count), std = sd(normalized_count), .groups = 'drop')
            
            # Merge with original data
            df_plotting_with_std <- merge(selected_count, df_mean, by = c('external_gene_name', 'groups')) %>% 
              mutate(
                external_gene_name = factor(external_gene_name, levels = selected_gene_symbol),
                groups = factor(groups, levels = c('E13.5_Skin_Mes_Ventral', 'E13.5_Mammary_Mes', 
                                                  'E16.5_Mammary_Mes', 'E16.5_Fatpad', 'E13.5_SMG_Mes'))
              )
            
            # Create the plot using the same style as mFuzzy cluster
            ggplot(df_plotting_with_std, aes(x = groups, y = normalized_count, color = groups)) +
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
              scale_x_discrete(labels = c("E13.5 Skin Mes", "E13.5 Mammary Mes", "E16.5 Mammary Mes", "E16.5 Fatpad", "E13.5 SMG Mes")) +
              ylab("Normalized Counts") +
              geom_errorbar(
                aes(ymin = mean - std, ymax = mean + std, color = groups),
                width = 0.3, 
                linewidth = 0.3, 
                show.legend = FALSE
              ) +
              ggtitle(paste0(selected_gene_symbol, " Expression (All Groups)"))
          }
        }
      }
    })
    
  })
}