library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

# Read your data
new_data <- read.csv("data/raw/sfpd_incidents_120223.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Incident Reports Line Plot"),
  sidebarLayout(
    sidebarPanel(
      # Initialize slider with a range of available years
      sliderInput("year_range", "Select Year Range",
                  min = min(new_data$year), max = max(new_data$year),
                  value = c(min(new_data$year), max(new_data$year)),
                  step = 1),
      width = 3
    ),
    mainPanel(
      plotOutput("line_plot")
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Reactive function to filter data based on year range
  filtered_data <- reactive({
    filter(new_data, year %in% seq(input$year_range[1], input$year_range[2]))
  })
  
  # Update slider when dataset changes
  observe({
    updateSliderInput(session, "year_range",
                      min = min(new_data$year), max = max(new_data$year),
                      value = c(min(new_data$year), max(new_data$year)))
  })
  
  # Create line plot
  output$line_plot <- renderPlot({
    lineplot_data <- filtered_data() %>%
      mutate(month = month(date, label = TRUE),
             month = factor(month.abb[month], levels = month.abb)) %>%
      group_by(year, month) %>%
      summarize(count = n())
    
    ggplot(lineplot_data, aes(x = as.numeric(month), y = count, group = year, color = as.factor(year))) +
      geom_line() +
      geom_point() +
      labs(
        x = "Month",
        y = "Count Per Month",
        title = "Incident Reports Line Plot"
      )
  })
}

# Run the app
shinyApp(ui, server)
