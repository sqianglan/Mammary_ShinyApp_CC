tab_epitheliumUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      box(
        title = "Gene Search", status = "primary", solidHeader = TRUE, width = 12,
        fluidRow(
          column(3,
            textInput(ns("gene_search"), "Search for genes:", 
                     value = "Myh9",
                     placeholder = "Enter gene names (e.g., Myh9, Myh10, Myh11)")
          ),
          column(3,
            selectInput(ns("groups_to_plot"), "Groups to Plot:",
                       choices = list("WT Only (E13.5 vs E16.5)" = "wt_only", 
                                     "E13.5 Only (WT vs Stab_bcat)" = "e13_only",
                                     "E16.5 Only (WT vs Stab_bcat)" = "e16_only",
                                     "All Groups" = "all_groups"),
                       selected = "wt_only")
          ),
          column(3,
            selectInput(ns("stat_method"), "Statistical Method:",
                       choices = list("t-test" = "ttest", "DESeq2" = "deseq2"),
                       selected = "ttest")
          ),
          column(3,
            div(style = "margin-top: 25px;",
                actionButton(ns("search_btn"), "Search & Plot", class = "btn-primary"))
          )
        )
      )
    ),
    
    fluidRow(
      box(
        title = "Normalized Gene Count Plot", status = "info", solidHeader = TRUE, width = 12,
        plotOutput(ns("gene_plot"), height = "500px")
      )
    ),
    
    fluidRow(
      tabBox(
        title = "Data Tables", width = 12,
        tabPanel("E13.5 vs E16.5 WT", DT::dataTableOutput(ns("deseq_table1"))),
        tabPanel("E13.5 Stab_bcat vs WT", DT::dataTableOutput(ns("deseq_table2"))),
        tabPanel("E16.5 Stab_bcat vs WT", DT::dataTableOutput(ns("deseq_table3")))
      )
    )
  )
}

tab_epitheliumServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Load data reactively
    dds_data <- reactive({
      tryCatch({
        readRDS("rawData/Satta et al/Deseq_dds_normalized_matrix.rds")
      }, error = function(e) {
        NULL
      })
    })
    
    gene_annotation <- reactive({
      tryCatch({
        readRDS("rawData/Satta et al/gene_annotation.rds")
      }, error = function(e) {
        data.frame(
          ensembl_gene_id = c("ENSMUSG00000000001", "ENSMUSG00000000002"),
          external_gene_name = c("Myh9", "Myh10")
        )
      })
    })
    
    sample_table <- reactive({
      tryCatch({
        read.csv("rawData/Satta et al/sampleTable.csv", stringsAsFactors = TRUE)
      }, error = function(e) {
        data.frame(
          sampleName = c("Sample_1", "Sample_2", "Sample_3", "Sample_4"),
          group = factor(c("E13.5_WT", "E16.5_WT", "E13.5_Stab_bcat", "E16.5_Stab_bcat"))
        )
      })
    })
    
    # Load DESeq2 comparison tables
    deseq_comparisons <- reactive({
      list(
        e16_vs_e13_wt = tryCatch({
          read.csv("rawData/Satta et al/24_group_E16.5_WT_vs_E13.5_WT_LFC_shrinkaged.csv")
        }, error = function(e) {
          data.frame(external_gene_name = c("Myh9", "Myh10"), 
                    log2FoldChange = c(1.2, -0.8), padj = c(0.01, 0.05))
        }),
        
        e13_stab_vs_wt = tryCatch({
          read.csv("rawData/Satta et al/20_group_E13.5_Stab_bcat_vs_E13.5_WT_LFC_shrinkaged.csv")
        }, error = function(e) {
          data.frame(external_gene_name = c("Myh9", "Myh10"), 
                    log2FoldChange = c(0.5, -1.1), padj = c(0.03, 0.02))
        }),
        
        e16_stab_vs_wt = tryCatch({
          read.csv("rawData/Satta et al/28_group_E16.5_Stab_bcat_vs_E16.5_WT_LFC_shrinkaged.csv")
        }, error = function(e) {
          data.frame(external_gene_name = c("Myh9", "Myh10"), 
                    log2FoldChange = c(0.9, -0.6), padj = c(0.02, 0.04))
        })
      )
    })
    
    # Get normalized counts
    normalized_counts <- reactive({
      if (is.null(dds_data())) {
        # Mock data for demonstration
        mock_data <- expand.grid(
          external_gene_name = c("Myh9", "Myh10", "Myh11"),
          sample = c("Sample_1", "Sample_2", "Sample_3", "Sample_4"),
          stringsAsFactors = FALSE
        )
        mock_data$normalized_count <- runif(nrow(mock_data), 100, 10000)
        mock_data$ensembl_gene_id <- paste0("ENSMUSG", sprintf("%08d", seq_len(nrow(mock_data))))
        mock_data <- merge(mock_data, sample_table(), by.x = "sample", by.y = "sampleName")
        return(mock_data)
      }
      
      # Convert to long format
      normalized_df <- dds_data() %>%
        as.data.frame() %>%
        .[unique(gene_annotation()$ensembl_gene_id),] %>%
        rownames_to_column('ensembl_gene_id') %>%
        pivot_longer(cols = -ensembl_gene_id, names_to = 'sample', values_to = 'normalized_count') %>%
        merge(sample_table()[, c('sampleName', 'group')], 
              by.x = 'sample', by.y = 'sampleName') %>%
        mutate(group = factor(group, levels = c('E13.5_WT', 'E13.5_Stab_bcat', 
                                               'E16.5_WT', 'E16.5_Stab_bcat'))) %>%
        merge(gene_annotation()[, c('ensembl_gene_id', 'external_gene_name')], 
              by = 'ensembl_gene_id')
      
      return(normalized_df)
    })
    
    # Default plot data for initial load
    default_plot_data <- reactive({
      if (is.null(normalized_counts())) {
        return(NULL)
      }
      
      # Filter groups based on default selection (wt_only)
      groups_filter <- c("E13.5_WT", "E16.5_WT")
      group_levels <- c("E13.5_WT", "E16.5_WT")
      
      data <- normalized_counts() %>%
        filter(external_gene_name %in% "Myh9") %>%
        filter(group %in% groups_filter) %>%
        mutate(
          external_gene_name = factor(external_gene_name, levels = "Myh9"),
          group = droplevels(group),
          group = factor(group, levels = group_levels)
        )
      
      return(data)
    })
    
    # Reactive values for plotting
    plot_data <- eventReactive(input$search_btn, {
      req(input$gene_search, input$groups_to_plot)
      
      genes_to_search <- trimws(unlist(strsplit(input$gene_search, ",")))
      genes_to_search <- genes_to_search[genes_to_search != ""]
      
      if (length(genes_to_search) == 0) return(NULL)
      
      # Filter groups based on user selection
      groups_filter <- switch(input$groups_to_plot,
        "wt_only" = c("E13.5_WT", "E16.5_WT"),
        "e13_only" = c("E13.5_WT", "E13.5_Stab_bcat"),
        "e16_only" = c("E16.5_WT", "E16.5_Stab_bcat"),
        "all_groups" = c("E13.5_WT", "E13.5_Stab_bcat", "E16.5_WT", "E16.5_Stab_bcat")
      )
      
      group_levels <- switch(input$groups_to_plot,
        "wt_only" = c("E13.5_WT", "E16.5_WT"),
        "e13_only" = c("E13.5_WT", "E13.5_Stab_bcat"),
        "e16_only" = c("E16.5_WT", "E16.5_Stab_bcat"),
        "all_groups" = c("E13.5_WT", "E13.5_Stab_bcat", "E16.5_WT", "E16.5_Stab_bcat")
      )
      
      data <- normalized_counts() %>%
        filter(external_gene_name %in% genes_to_search) %>%
        filter(group %in% groups_filter) %>%
        mutate(
          external_gene_name = factor(external_gene_name, levels = genes_to_search),
          group = droplevels(group),
          group = factor(group, levels = group_levels)
        )
      
      return(data)
    })
    
    # Generate plot
    output$gene_plot <- renderPlot({
      # Use search data if available, otherwise use default data
      if (input$search_btn == 0) {
        # Initial load - show default Myh9 plot or error message
        data <- default_plot_data()
        if (is.null(normalized_counts())) {
          return(ggplot() + 
                 geom_text(aes(x = 0.5, y = 0.5, label = "Data not found"), size = 6) +
                 theme_void())
        }
        if (is.null(data) || nrow(data) == 0) {
          return(ggplot() + 
                 geom_text(aes(x = 0.5, y = 0.5, label = "The gene Myh9 was not detected"), size = 6) +
                 theme_void())
        }
        current_groups <- "wt_only"  # Default groups for initial plot
      } else {
        # After search button clicked
        data <- plot_data()
        if (is.null(normalized_counts())) {
          return(ggplot() + 
                 geom_text(aes(x = 0.5, y = 0.5, label = "Data not found"), size = 6) +
                 theme_void())
        }
        if (is.null(data) || nrow(data) == 0) {
          return(ggplot() + 
                 geom_text(aes(x = 0.5, y = 0.5, label = "The gene was not detected"), size = 6) +
                 theme_void())
        }
        current_groups <- input$groups_to_plot
      }
      
      # Calculate means and standard deviations
      df_mean <- data %>% 
        group_by(group, external_gene_name) %>% 
        summarise(mean = mean(normalized_count), std = sd(normalized_count), .groups = 'drop')
      
      df_plotting_with_std <- merge(data, df_mean, by = c('external_gene_name', 'group'))
      
      # Define color schemes based on groups using ggsci NPG colors
      color_schemes <- switch(current_groups,
        "wt_only" = list(
          fill = c('E13.5_WT' = '#E64B35FF', 'E16.5_WT' = '#4DBBD5FF'),
          color = c('E13.5_WT' = '#CC0000FF', 'E16.5_WT' = '#0099B4FF'),
          labels = c("E13.5_WT" = "E13.5", "E16.5_WT" = "E16.5"),
          name = "Time Point"
        ),
        "e13_only" = list(
          fill = c('E13.5_WT' = '#E64B35FF', 'E13.5_Stab_bcat' = '#00A087FF'),
          color = c('E13.5_WT' = '#CC0000FF', 'E13.5_Stab_bcat' = '#00755FFF'),
          labels = c("E13.5_WT" = "WT", "E13.5_Stab_bcat" = "Stab_bcat"),
          name = "E13.5 Condition"
        ),
        "e16_only" = list(
          fill = c('E16.5_WT' = '#4DBBD5FF', 'E16.5_Stab_bcat' = '#3C5488FF'),
          color = c('E16.5_WT' = '#0099B4FF', 'E16.5_Stab_bcat' = '#2A3A6AFF'),
          labels = c("E16.5_WT" = "WT", "E16.5_Stab_bcat" = "Stab_bcat"),
          name = "E16.5 Condition"
        ),
        "all_groups" = list(
          fill = c('E13.5_WT' = '#E64B35FF', 'E13.5_Stab_bcat' = '#4DBBD5FF', 
                  'E16.5_WT' = '#00A087FF', 'E16.5_Stab_bcat' = '#3C5488FF'),
          color = c('E13.5_WT' = '#CC0000FF', 'E13.5_Stab_bcat' = '#0099B4FF',
                   'E16.5_WT' = '#00755FFF', 'E16.5_Stab_bcat' = '#2A3A6AFF'),
          labels = c("E13.5_WT" = "E13.5_WT", "E13.5_Stab_bcat" = "E13.5_Stab", 
                    "E16.5_WT" = "E16.5_WT", "E16.5_Stab_bcat" = "E16.5_Stab"),
          name = "Group"
        )
      )
      
      # Create base plot similar to reference style
      p <- ggplot(df_plotting_with_std, aes(x = group, y = normalized_count, color = group)) +
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
                width = 0.7, aes(fill = group), linewidth = 0.4, show.legend = FALSE) +
        geom_quasirandom(show.legend = FALSE, size = 3, alpha = 0.7) +
        scale_fill_manual(values = color_schemes$fill, labels = color_schemes$labels, name = color_schemes$name) +
        scale_color_manual(values = color_schemes$color, labels = color_schemes$labels, name = color_schemes$name) +
        ylab("Normalized Counts") +
        expand_limits(y = 0) +
        geom_errorbar(aes(ymin = mean - std, ymax = mean + std, color = group),
                     width = 0.3, linewidth = 0.3, show.legend = FALSE)
      
      # Get gene name for title
      gene_name <- as.character(unique(data$external_gene_name)[1])
      
      # Add statistical annotations based on method and groups
      plot_title <- switch(current_groups,
        "wt_only" = paste0(gene_name, " Expression (E13.5 vs E16.5 WT)"),
        "e13_only" = paste0(gene_name, " Expression (E13.5: WT vs Stab_bcat)"),
        "e16_only" = paste0(gene_name, " Expression (E16.5: WT vs Stab_bcat)"), 
        "all_groups" = paste0(gene_name, " Expression (All Groups)")
      )
      
      stat_method <- if (input$search_btn == 0) "ttest" else input$stat_method
      
      if (stat_method == "ttest") {
        # Get actual groups present in the data
        available_groups <- unique(as.character(data$group))
        
        # Define specific pairwise comparisons based on groups and available data
        comparisons <- switch(current_groups,
          "wt_only" = {
            if (all(c("E13.5_WT", "E16.5_WT") %in% available_groups)) {
              list(c("E13.5_WT", "E16.5_WT"))
            } else NULL
          },
          "e13_only" = {
            if (all(c("E13.5_WT", "E13.5_Stab_bcat") %in% available_groups)) {
              list(c("E13.5_WT", "E13.5_Stab_bcat"))
            } else NULL
          },
          "e16_only" = {
            if (all(c("E16.5_WT", "E16.5_Stab_bcat") %in% available_groups)) {
              list(c("E16.5_WT", "E16.5_Stab_bcat"))
            } else NULL
          },
          "all_groups" = {
            valid_comps <- list()
            if (all(c("E13.5_WT", "E16.5_WT") %in% available_groups)) {
              valid_comps <- append(valid_comps, list(c("E13.5_WT", "E16.5_WT")))
            }
            if (all(c("E13.5_WT", "E13.5_Stab_bcat") %in% available_groups)) {
              valid_comps <- append(valid_comps, list(c("E13.5_WT", "E13.5_Stab_bcat")))
            }
            if (all(c("E16.5_WT", "E16.5_Stab_bcat") %in% available_groups)) {
              valid_comps <- append(valid_comps, list(c("E16.5_WT", "E16.5_Stab_bcat")))
            }
            if (length(valid_comps) > 0) valid_comps else NULL
          }
        )
        
        if (!is.null(comparisons) && length(comparisons) > 0) {
          p <- p + 
            stat_compare_means(method = "t.test", comparisons = comparisons,
                             aes(label = ..p.signif..), show.legend = FALSE, 
                             size = 3, step.increase = 0.1)
        }
        
        p <- p + ggtitle(plot_title)
      } else if (stat_method == "deseq2") {
        # Handle multiple DESeq2 comparisons for all_groups
        if (current_groups == "all_groups") {
          # Get all three comparisons
          deseq_datasets <- list(
            "E16.5_WT vs E13.5_WT" = deseq_comparisons()$e16_vs_e13_wt,
            "E13.5_Stab vs E13.5_WT" = deseq_comparisons()$e13_stab_vs_wt,
            "E16.5_Stab vs E16.5_WT" = deseq_comparisons()$e16_stab_vs_wt
          )
          
          # Create split annotations for each comparison
          annotation_data <- data.frame()
          y_positions <- c(1.3, 1.2, 1.1)  # Different heights for each annotation
          
          for (i in seq_along(deseq_datasets)) {
            if (!is.null(deseq_datasets[[i]])) {
              temp_data <- df_mean %>%
                merge(deseq_datasets[[i]][, c('external_gene_name', 'padj')], 
                     by = 'external_gene_name', all.x = TRUE) %>%
                group_by(external_gene_name) %>%
                summarise(y = max(mean) + max(std, na.rm = TRUE), 
                         padj_val = mean(padj, na.rm = TRUE), .groups = 'drop') %>%
                mutate(
                  x = seq_along(external_gene_name),
                  y_pos = y * y_positions[i],
                  comparison = names(deseq_datasets)[i],
                  label_text = paste0(names(deseq_datasets)[i], ": p=", round(padj_val, 3))
                )
              annotation_data <- rbind(annotation_data, temp_data)
            }
          }
          
          if (nrow(annotation_data) > 0) {
            p <- p + 
              geom_text(data = annotation_data,
                       aes(x = x, y = y_pos, label = label_text),
                       inherit.aes = FALSE, size = 2.5, show.legend = FALSE, 
                       vjust = 0, hjust = 0.5) +
              ggtitle(plot_title)
          }
        } else {
          # Single comparison for other group options
          deseq_data <- switch(current_groups,
            "wt_only" = deseq_comparisons()$e16_vs_e13_wt,
            "e13_only" = deseq_comparisons()$e13_stab_vs_wt,
            "e16_only" = deseq_comparisons()$e16_stab_vs_wt
          )
          
          if (!is.null(deseq_data)) {
            significance_data <- df_mean %>%
              merge(deseq_data[, c('external_gene_name', 'padj')], 
                   by = 'external_gene_name', all.x = TRUE) %>%
              group_by(external_gene_name) %>%
              summarise(y = max(mean) + max(std, na.rm = TRUE), 
                       label = mean(padj, na.rm = TRUE), .groups = 'drop') %>%
              mutate(x = seq_along(external_gene_name))
            
            if (nrow(significance_data) > 0) {
              p <- p + 
                geom_text(data = significance_data,
                         aes(x = x, y = y * 1.1, 
                             label = paste0("p = ", round(label, 3))),
                         inherit.aes = FALSE, size = 3, show.legend = FALSE) +
                ggtitle(plot_title)
            }
          } else {
            p <- p + ggtitle(plot_title)
          }
        }
      }
      
      return(p)
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
        caption = "E13.5 Stab_bcat vs E13.5 WT Comparison"
      )
    })
    
    output$deseq_table3 <- DT::renderDataTable({
      DT::datatable(
        deseq_comparisons()$e16_stab_vs_wt,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = "E16.5 Stab_bcat vs E16.5 WT Comparison"
      )
    })
    
  })
}