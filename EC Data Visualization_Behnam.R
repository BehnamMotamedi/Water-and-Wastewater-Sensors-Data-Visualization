library(shiny)
library(readxl)
library(tidyverse)
library(plotly)

# Read the data from the Excel file
excel_file_path <- 'C:/Users/bmotamed22/Desktop/DAWR_2022/Assignment_1/EC sensors.xlsx'
data <- readxl::read_excel(excel_file_path)

# Reshape the data for plotly
data_long <- tidyr::gather(data, key = "Parameter", value = "Value", 
                           JVP111, JVP113, JVP116, JVP153, GW_Level, 
                           `Precipitation (mm)`, `Snow Depth (cm)`, `Air Temperature (°C)`)

# Convert the Time column to POSIXct class with the correct format
data_long$Time <- as.POSIXct(data_long$Time, format = "%m/%d/%Y %H:%M:%S")

# Handle non-numeric values
data_long$Value <- as.numeric(data_long$Value)
data_long$Value[!is.na(data_long$Value) & !is.finite(data_long$Value)] <- NA

# Remove rows with NA values in the Value column
data_long <- na.omit(data_long)

# Define UI
ui <- fluidPage(
  titlePanel("EC Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Select Date Range:",
                     start = min(data_long$Time),
                     end = max(data_long$Time),
                     min = min(data_long$Time),
                     max = max(data_long$Time)),
      numericInput("yAxisMin", "Y-Axis Min (Left):", value = 0),
      numericInput("yAxisMax", "Y-Axis Max (Left):", value = 1500),
      numericInput("yAxisMinRight", "Y-Axis Min (Right):", value = 1),
      numericInput("yAxisMaxRight", "Y-Axis Max (Right):", value = 2),
      checkboxGroupInput("selectedParameters1", "Select Parameters for Graph 1:",
                         choices = setdiff(unique(data_long$Parameter), c("Precipitation (mm)", "Snow Depth (cm)", "Air Temperature (°C)")),
                         selected = setdiff(unique(data_long$Parameter), c("Precipitation (mm)", "Snow Depth (cm)", "Air Temperature (°C)"))),
      checkboxGroupInput("selectedParameters2", "Select Parameters for Graph 2:",
                         choices = setdiff(unique(data_long$Parameter), c("JVP111", "JVP113", "JVP116", "JVP153", "GW_Level")),
                         selected = setdiff(unique(data_long$Parameter), c("JVP111", "JVP113", "JVP116", "JVP153", "GW_Level")))
    ),
    mainPanel(
      plotlyOutput("graph1"),
      plotlyOutput("graph2"),
      tableOutput("summaryTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  conversion_factor <- 1  # Define the conversion factor
  
  filtered_data1 <- reactive({
    data_long %>%
      filter(between(Time, input$dateRange[1], input$dateRange[2]),
             Parameter %in% input$selectedParameters1)
  })
  
  filtered_data2 <- reactive({
    data_long %>%
      filter(between(Time, input$dateRange[1], input$dateRange[2]),
             Parameter %in% input$selectedParameters2)
  })
  
  output$graph1 <- renderPlotly({
    gw_level_range <- range(filtered_data1()$Value[filtered_data1()$Parameter == "GW_Level"]) * conversion_factor
    
    # Define color mapping for each Parameter in the first graph
    color_mapping <- c("JVP111" = "red", "JVP113" = "purple", "JVP116" = "gold", "JVP153" = "silver")
    
    p <- plot_ly() %>%
      add_trace(data = filtered_data1() %>% filter(Parameter %in% c("JVP111", "JVP113", "JVP116", "JVP153")),
                x = ~Time, y = ~Value, color = ~Parameter, type = "scatter", mode = "markers",
                marker = list(size = 3, color = ~factor(Parameter, levels = names(color_mapping), labels = color_mapping))) %>%
      add_trace(data = filtered_data1() %>% filter(Parameter == "GW_Level"),
                x = ~Time, y = ~Value * conversion_factor, color = ~Parameter,
                type = "scatter", mode = "lines", line = list(width = 2, color = "blue"), yaxis = "y2") %>%
      layout(title = "Graph 1: EC & GW_Level Over Time",
             xaxis = list(title = "Time"),
             yaxis = list(title = "Electric Conductivity (μS/cm)", side = "left", range = c(input$yAxisMin, input$yAxisMax)),
             yaxis2 = list(title = "GW_Level (meters)", side = "right", overlaying = "y", range = gw_level_range),
             showlegend = TRUE)
    
    p
  })
  
  output$graph2 <- renderPlotly({
    # Define fixed colors for each Parameter in the second graph
    fixed_colors_graph2 <- c("Precipitation (mm)" = "blue", "Snow Depth (cm)" = "black", "Air Temperature (°C)" = "red")
    
    # Filter data for selected parameters in Graph 2
    filtered_data2 <- data_long %>%
      filter(between(Time, input$dateRange[1], input$dateRange[2]),
             Parameter %in% input$selectedParameters2)
    
    p <- plot_ly() %>%
      add_trace(
        data = filtered_data2 %>% filter(Parameter %in% c("Precipitation (mm)", "Snow Depth (cm)")),
        x = ~Time, y = ~Value, color = ~Parameter, type = "scatter", mode = "lines",
        line = list(width = 1, color = ~factor(Parameter, levels = names(fixed_colors_graph2), labels = fixed_colors_graph2))
      ) %>%
      add_trace(
        data = filtered_data2 %>% filter(Parameter == "Air Temperature (°C)"),
        x = ~Time, y = ~Value, color = ~Parameter, type = "scatter", mode = "lines",
        line = list(width = 1, color = ~factor(Parameter, levels = names(fixed_colors_graph2), labels = fixed_colors_graph2)),
        yaxis = "y2"
      ) %>%
      layout(
        title = "Graph 2: Precipitation & Air Temperature Over Time",
        xaxis = list(title = "Time"),
        yaxis = list(
          title = "Precipitation",
          side = "left",
          range = c(min(filtered_data2$Value), max(filtered_data2$Value)),
          showgrid = FALSE  # Remove grid for the left Y axis
        ),
        yaxis2 = list(
          title = "Air Temperature (°C)",
          side = "right",
          overlaying = "y",
          range = c(min(filtered_data2$Value[filtered_data2$Parameter == "Air Temperature (°C)"]),
                    max(filtered_data2$Value[filtered_data2$Parameter == "Air Temperature (°C)"])),
          showgrid = FALSE  # Remove grid for the right Y axis
        ),
        showlegend = TRUE
      )
    
    p
  })
  
  output$summaryTable <- renderTable({
    # Combine selected parameters from both graphs
    selected_params <- c(input$selectedParameters1, input$selectedParameters2)
    
    # Generate summary statistics for the selected parameters
    summary_stats <- data_long %>%
      filter(Parameter %in% selected_params) %>%
      group_by(Parameter) %>%
      summarise(Mean = mean(Value, na.rm = TRUE),
                Min = min(Value, na.rm = TRUE),
                Max = max(Value, na.rm = TRUE),
                SD = sd(Value, na.rm = TRUE),
                Median = median(Value, na.rm = TRUE),
                Q1 = quantile(Value, 0.25, na.rm = TRUE),
                Q3 = quantile(Value, 0.75, na.rm = TRUE),
                IQR = IQR(Value, na.rm = TRUE),
                MAD = mad(Value, na.rm = TRUE),
                CV = sd(Value, na.rm = TRUE) / mean(Value, na.rm = TRUE))
    
    summary_stats
  })
}

# Run the Shiny app
shinyApp(ui, server)


# Designer: Behnam Motamedi
#Doctoral Researcher
#Water, Energy, and Environmental Engineering
#Faculty of Technology
#University of Oulu, Finland 
#Room no: SÄ333
#Phone: 358 40 859 8551
#E-mail: behnam.motamedi@oulu.fi

