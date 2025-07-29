library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(DT)
library(googlesheets4)

# Function to get client IP address
get_client_ip <- function(session) {
  # Try multiple headers to get the real client IP
  headers <- list(
    session$request$HTTP_X_FORWARDED_FOR,
    session$request$HTTP_X_REAL_IP,
    session$request$HTTP_CF_CONNECTING_IP,
    session$request$REMOTE_ADDR
  )
  
  # Return the first non-null IP
  for (ip in headers) {
    if (!is.null(ip) && ip != "") {
      # If multiple IPs (comma-separated), take the first one
      return(strsplit(ip, ",")[[1]][1])
    }
  }
  
  return("Unknown")
}

# Function to get geolocation from IP
get_geolocation <- function(ip) {
  if (ip == "Unknown" || ip == "127.0.0.1" || grepl("^192\\.168\\.", ip)) {
    return(list(
      country = "Local/Unknown",
      region = "Local/Unknown", 
      city = "Local/Unknown",
      timezone = "Unknown"
    ))
  }
  
  tryCatch({
    # Using ip-api.com (free service, no API key required)
    url <- paste0("http://ip-api.com/json/", ip)
    response <- GET(url)
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"))
      return(list(
        country = ifelse(is.null(data$country), "Unknown", data$country),
        region = ifelse(is.null(data$regionName), "Unknown", data$regionName),
        city = ifelse(is.null(data$city), "Unknown", data$city),
        timezone = ifelse(is.null(data$timezone), "Unknown", data$timezone)
      ))
    }
  }, error = function(e) {
    message("Geolocation API error: ", e$message)
  })
  
  # Fallback if API fails
  return(list(
    country = "Unknown",
    region = "Unknown",
    city = "Unknown", 
    timezone = "Unknown"
  ))
}

# Google Sheets Configuration
SHEETS_ID <- Sys.getenv("GOOGLE_SHEETS_ID", "")  # Set via environment variable
SHEET_NAME <- "visitor_logs"

# Initialize Google Sheets authentication
init_sheets_auth <- function() {
  tryCatch({
    # Check if running in non-interactive mode (server)
    if (!interactive()) {
      # Use service account authentication for servers
      service_key <- Sys.getenv("GOOGLE_SERVICE_KEY")
      if (service_key != "") {
        # If service key is provided as environment variable (JSON string)
        temp_key_file <- tempfile(fileext = ".json")
        writeLines(service_key, temp_key_file)
        gs4_auth(path = temp_key_file)
        unlink(temp_key_file)
        message("Sheets: Using service account authentication")
      } else {
        # Try service account file path
        key_file <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")
        if (key_file != "" && file.exists(key_file)) {
          gs4_auth(path = key_file)
          message("Sheets: Using service account key file")
        } else {
          message("Sheets: No authentication found, falling back to local storage")
          return(FALSE)
        }
      }
    } else {
      # Interactive mode - use browser authentication
      gs4_auth()
      message("Sheets: Using interactive authentication")
    }
    return(TRUE)
  }, error = function(e) {
    message("Sheets authentication failed: ", e$message)
    return(FALSE)
  })
}

# Read visitor data from Google Sheets
read_visitor_data_sheets <- function() {
  tryCatch({
    if (SHEETS_ID == "") {
      message("Google Sheets ID not configured")
      return(data.frame())
    }
    
    # Check if sheet exists
    sheet_info <- gs4_get(SHEETS_ID)
    if (!SHEET_NAME %in% sheet_info$sheets$name) {
      message("Sheets: Creating new sheet tab")
      # Create the sheet with headers
      headers <- data.frame(
        timestamp = character(0),
        ip_address = character(0),
        country = character(0),
        region = character(0),
        city = character(0),
        timezone = character(0),
        user_agent = character(0),
        session_id = character(0),
        stringsAsFactors = FALSE
      )
      sheet_write(headers, ss = SHEETS_ID, sheet = SHEET_NAME)
      return(data.frame())
    }
    
    # Read existing data
    data <- read_sheet(SHEETS_ID, sheet = SHEET_NAME)
    if (nrow(data) > 0) {
      # Convert to data.frame and ensure correct column types
      data <- as.data.frame(data)
      message("Sheets: Successfully loaded ", nrow(data), " visitor records")
    }
    return(data)
  }, error = function(e) {
    message("Failed to read from Google Sheets: ", e$message)
    return(data.frame())
  })
}

# Write visitor data to Google Sheets
write_visitor_data_sheets <- function(data) {
  tryCatch({
    if (SHEETS_ID == "") {
      message("Google Sheets ID not configured")
      return(FALSE)
    }
    
    # Clear existing data and write new data
    range_clear(SHEETS_ID, sheet = SHEET_NAME)
    sheet_write(data, ss = SHEETS_ID, sheet = SHEET_NAME)
    
    message("Sheets: Successfully uploaded ", nrow(data), " visitor records")
    return(TRUE)
  }, error = function(e) {
    message("Failed to write to Google Sheets: ", e$message)
    return(FALSE)
  })
}

# Function to log visitor data (updated for Google Sheets)
log_visitor <- function(session, use_sheets = TRUE) {
  ip <- get_client_ip(session)
  geo <- get_geolocation(ip)
  
  visitor_data <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    ip_address = ip,
    country = geo$country,
    region = geo$region,
    city = geo$city,
    timezone = geo$timezone,
    user_agent = ifelse(is.null(session$request$HTTP_USER_AGENT), 
                       "Unknown", session$request$HTTP_USER_AGENT),
    session_id = session$token,
    stringsAsFactors = FALSE
  )
  
  if (use_sheets && SHEETS_ID != "" && init_sheets_auth()) {
    # Use Google Sheets storage
    existing_data <- read_visitor_data_sheets()
    if (nrow(existing_data) > 0) {
      combined_data <- rbind(existing_data, visitor_data)
    } else {
      combined_data <- visitor_data
    }
    
    if (write_visitor_data_sheets(combined_data)) {
      message("Visitor logged to Google Sheets: ", ip, " from ", geo$city, ", ", geo$country)
    } else {
      message("Failed to log to Google Sheets, falling back to local storage")
      use_sheets <- FALSE
    }
  }
  
  if (!use_sheets) {
    # Fallback to local storage
    log_file <- file.path(getwd(), "visitor_logs.csv")
    
    if (file.exists(log_file)) {
      existing_data <- read.csv(log_file, stringsAsFactors = FALSE)
      combined_data <- rbind(existing_data, visitor_data)
    } else {
      combined_data <- visitor_data
    }
    
    tryCatch({
      write.csv(combined_data, log_file, row.names = FALSE)
      message("Visitor logged locally: ", ip, " from ", geo$city, ", ", geo$country)
    }, error = function(e) {
      message("Failed to write local visitor log: ", e$message)
    })
  }
  
  return(visitor_data)
}

# Analytics UI module
analytics_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # Analytics content (no authentication required here)
    fluidRow(
      column(12,
        h3("Website Analytics Dashboard", style = "text-align: center; margin-bottom: 20px;")
      )
    ),
      fluidRow(
        box(
          title = "Website Analytics", status = "info", solidHeader = TRUE, width = 12,
        tabsetPanel(
          tabPanel("Recent Visitors",
            br(),
            fluidRow(
              column(3,
                selectInput(ns("time_range"), "Time Range:",
                  choices = list(
                    "Last 1 Month" = "1month",
                    "Last 3 Months" = "3months", 
                    "Last 6 Months" = "6months",
                    "Last 1 Year" = "1year",
                    "Last 3 Years" = "3years",
                    "All Time" = "all"
                  ),
                  selected = "3months"
                )
              ),
              column(3,
                div(style = "margin-top: 25px;",
                  downloadButton(ns("download_logs"), "Download Logs", 
                                class = "btn-success btn-sm",
                                icon = icon("download"))
                )
              ),
              column(6,
                div(style = "text-align: right; margin-top: 25px;",
                  textOutput(ns("data_range_info"))
                )
              )
            ),
            br(),
            DT::dataTableOutput(ns("visitor_table"))
          ),
          tabPanel("Location Summary",
            br(),
            fluidRow(
              column(6, plotOutput(ns("country_plot"))),
              column(6, plotOutput(ns("city_plot")))
            )
          ),
          tabPanel("Usage Statistics",
            br(),
            fluidRow(
              column(4,
                selectInput(ns("timeline_range"), "Timeline Range:",
                  choices = list(
                    "Last 1 Month" = "1month",
                    "Last 3 Months" = "3months", 
                    "Last 6 Months" = "6months",
                    "Last 1 Year" = "1year",
                    "Last 3 Years" = "3years",
                    "All Time" = "all"
                  ),
                  selected = "6months"
                )
              ),
              column(8,
                div(style = "text-align: right; margin-top: 25px;",
                  textOutput(ns("timeline_range_info"))
                )
              )
            ),
            br(),
            fluidRow(
              column(4, 
                valueBoxOutput(ns("total_visits"), width = NULL)
              ),
              column(4,
                valueBoxOutput(ns("unique_ips"), width = NULL) 
              ),
              column(4,
                valueBoxOutput(ns("countries"), width = NULL)
              )
            ),
            br(),
            plotOutput(ns("timeline_plot"))
          )
        )
      )
    )
  )
}

# Analytics Server module
analytics_server <- function(id, use_sheets = TRUE) {
  moduleServer(id, function(input, output, session) {
    
    # Helper function to filter data by time range
    filter_by_time_range <- function(data, range) {
      if (nrow(data) == 0 || range == "all") return(data)
      
      cutoff_date <- switch(range,
        "1month" = Sys.Date() - 30,
        "3months" = Sys.Date() - 90,
        "6months" = Sys.Date() - 180,  
        "1year" = Sys.Date() - 365,
        "3years" = Sys.Date() - (365 * 3),
        Sys.Date() - 365  # default to 1 year
      )
      
      data[as.Date(data$timestamp) >= cutoff_date, ]
    }
    
    # Reactive data loading
    visitor_data <- reactive({
      invalidateLater(30000)  # Refresh every 30 seconds
      
      if (use_sheets && SHEETS_ID != "" && init_sheets_auth()) {
        # Load from Google Sheets
        data <- read_visitor_data_sheets()
      } else {
        # Fallback to local file
        local_file <- file.path(getwd(), "visitor_logs.csv")
        if (file.exists(local_file)) {
          data <- read.csv(local_file, stringsAsFactors = FALSE)
        } else {
          data <- data.frame()
        }
      }
      
      if (nrow(data) > 0) {
        # Handle timestamp parsing with error handling
        data$timestamp <- sapply(data$timestamp, function(ts) {
          # First, try to detect if it's a Unix timestamp (numeric)
          if (is.numeric(ts) || grepl("^[0-9]+\\.?[0-9]*$", ts)) {
            # Convert Unix timestamp to POSIXct
            tryCatch({
              as.POSIXct(as.numeric(ts), origin = "1970-01-01")
            }, error = function(e) {
              Sys.time()
            })
          } else {
            # Try standard datetime formats
            tryCatch({
              as.POSIXct(ts, format = "%Y-%m-%d %H:%M:%S")
            }, error = function(e) {
              tryCatch({
                as.POSIXct(ts)
              }, error = function(e2) {
                Sys.time()
              })
            })
          }
        })
        # Convert back to POSIXct class
        data$timestamp <- as.POSIXct(data$timestamp, origin = "1970-01-01")
      }
      
      return(data)
    })
    
    # Filtered data for visitor table
    filtered_visitor_data <- reactive({
      data <- visitor_data()
      range <- input$time_range
      if (is.null(range)) range <- "3months"
      filter_by_time_range(data, range)
    })
    
    # Filtered data for timeline
    filtered_timeline_data <- reactive({
      data <- visitor_data()
      range <- input$timeline_range  
      if (is.null(range)) range <- "6months"
      filter_by_time_range(data, range)
    })
    
    # Data range info for visitor table
    output$data_range_info <- renderText({
      data <- filtered_visitor_data()
      if (nrow(data) > 0) {
        min_date <- format(min(as.Date(data$timestamp)), "%Y-%m-%d")
        max_date <- format(max(as.Date(data$timestamp)), "%Y-%m-%d")
        paste("Showing", nrow(data), "visits from", min_date, "to", max_date)
      } else {
        "No data available for selected range"
      }
    })
    
    # Visitor table
    output$visitor_table <- DT::renderDataTable({
      data <- filtered_visitor_data()
      if (nrow(data) > 0) {
        # Show most recent 100 visits
        recent_data <- tail(data, 100)
        
        # Format timestamp safely
        tryCatch({
          recent_data$timestamp <- format(recent_data$timestamp, "%Y-%m-%d %H:%M:%S")
        }, error = function(e) {
          recent_data$timestamp <- as.character(recent_data$timestamp)
        })
        
        # Select and rename columns for display
        display_data <- recent_data[, c("timestamp", "ip_address", "country", 
                                       "region", "city", "timezone")]
        colnames(display_data) <- c("Visit Time", "IP Address", "Country", 
                                   "Region", "City", "Timezone")
        
        DT::datatable(display_data, 
                     options = list(pageLength = 25, order = list(list(0, 'desc'))))
      } else {
        # Show empty table with proper columns
        empty_data <- data.frame(
          "Visit Time" = character(0),
          "IP Address" = character(0),
          "Country" = character(0),
          "Region" = character(0),
          "City" = character(0),
          "Timezone" = character(0),
          stringsAsFactors = FALSE
        )
        DT::datatable(empty_data, 
                     options = list(pageLength = 25))
      }
    })
    
    # Country plot
    output$country_plot <- renderPlot({
      data <- visitor_data()
      if (nrow(data) > 0) {
        country_counts <- table(data$country)
        barplot(sort(country_counts, decreasing = TRUE)[1:10], 
                main = "Top 10 Countries", 
                las = 2, cex.names = 0.8)
      }
    })
    
    # City plot
    output$city_plot <- renderPlot({
      data <- visitor_data()
      if (nrow(data) > 0) {
        city_counts <- table(paste(data$city, data$country, sep = ", "))
        barplot(sort(city_counts, decreasing = TRUE)[1:10],
                main = "Top 10 Cities",
                las = 2, cex.names = 0.8)
      }
    })
    
    # Value boxes (using timeline filtered data)
    output$total_visits <- renderValueBox({
      data <- filtered_timeline_data()
      valueBox(
        value = if(nrow(data) > 0) nrow(data) else 0,
        subtitle = "Total Visits",
        icon = icon("eye"),
        color = "blue",
        width = NULL
      )
    })
    
    output$unique_ips <- renderValueBox({
      data <- filtered_timeline_data()
      valueBox(
        value = if(nrow(data) > 0) length(unique(data$ip_address)) else 0,
        subtitle = "Unique IPs", 
        icon = icon("users"),
        color = "green",
        width = NULL
      )
    })
    
    output$countries <- renderValueBox({
      data <- filtered_timeline_data()
      valueBox(
        value = if(nrow(data) > 0) length(unique(data$country)) else 0,
        subtitle = "Countries",
        icon = icon("globe"),
        color = "orange",
        width = NULL
      )
    })
    
    # Timeline range info
    output$timeline_range_info <- renderText({
      data <- filtered_timeline_data()
      if (nrow(data) > 0) {
        min_date <- format(min(as.Date(data$timestamp)), "%Y-%m-%d")
        max_date <- format(max(as.Date(data$timestamp)), "%Y-%m-%d")
        paste("Timeline:", min_date, "to", max_date, "(", nrow(data), "visits)")
      } else {
        "No data available for selected range"
      }
    })
    
    # Download handler for visitor logs
    output$download_logs <- downloadHandler(
      filename = function() {
        paste("visitor_logs_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        # Get filtered data based on current time range selection
        data <- filtered_visitor_data()
        if (nrow(data) > 0) {
          # Format timestamps for better readability
          data$timestamp <- format(data$timestamp, "%Y-%m-%d %H:%M:%S")
          write.csv(data, file, row.names = FALSE)
        } else {
          # Create empty CSV with headers if no data
          empty_data <- data.frame(
            timestamp = character(0),
            ip_address = character(0),
            country = character(0),
            region = character(0),
            city = character(0),
            timezone = character(0),
            user_agent = character(0),
            session_id = character(0),
            stringsAsFactors = FALSE
          )
          write.csv(empty_data, file, row.names = FALSE)
        }
      }
    )
    
    # Timeline plot
    output$timeline_plot <- renderPlot({
      data <- filtered_timeline_data()
      if (nrow(data) > 0) {
        tryCatch({
          # Convert timestamps to dates safely
          data$date <- as.Date(data$timestamp)
          daily_counts <- table(data$date)
          
          if (length(daily_counts) > 0) {
            dates <- as.Date(names(daily_counts))
            counts <- as.numeric(daily_counts)
            
            # Set x-axis limits based on selected time range
            range_sel <- input$timeline_range
            if (is.null(range_sel)) range_sel <- "6months"
            
            if (range_sel != "all") {
              cutoff_date <- switch(range_sel,
                "1month" = Sys.Date() - 30,
                "3months" = Sys.Date() - 90,
                "6months" = Sys.Date() - 180,
                "1year" = Sys.Date() - 365,
                "3years" = Sys.Date() - (365 * 3),
                Sys.Date() - 365
              )
              xlim_range <- c(cutoff_date, Sys.Date())
            } else {
              xlim_range <- range(dates)
            }
            
            plot(dates, counts,
                 type = "b", main = "Daily Visits Over Time",
                 xlab = "Date", ylab = "Number of Visits",
                 pch = 16, col = "blue", lwd = 2,
                 xlim = xlim_range)
            
            # Add grid for better readability
            grid(col = "lightgray", lty = "dotted")
          } else {
            # Show empty plot with message
            plot(1, 1, type = "n", xlab = "Date", ylab = "Number of Visits",
                 main = "Daily Visits Over Time")
            text(1, 1, "No visit data available", cex = 1.2, col = "gray")
          }
        }, error = function(e) {
          # Error handling - show message plot
          plot(1, 1, type = "n", xlab = "Date", ylab = "Number of Visits",
               main = "Daily Visits Over Time")
          text(1, 1, paste("Error loading data:", e$message), cex = 1, col = "red")
        })
      } else {
        # No data - show empty plot
        plot(1, 1, type = "n", xlab = "Date", ylab = "Number of Visits",
             main = "Daily Visits Over Time")
        text(1, 1, "No visit data available", cex = 1.2, col = "gray")
      }
    })
  })
}