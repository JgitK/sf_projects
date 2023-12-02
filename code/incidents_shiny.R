library(tidyverse)
library(shiny)

incidents <- read_csv("data/raw/incident_reports_new.csv") |>
  select(id = `Incident ID`, date = `Incident Date`, year = `Incident Year`, neighborhood = `Analysis Neighborhood`, 
         p_district = `Police District`, category = `Incident Category`, lat = `Latitude`, long = `Longitude`)

ui <- fluidPage(
  titlePanel("Crime Counts by Year and Type"),
  
  sidebarLayout(
    sidebarPanel(
      # Select neighborhood dropdown
      selectInput("neighborhood", "Select Neighborhood",
                  choices = unique(incidents$neighborhood),
                  selected = "Your_Default_Neighborhood"),
      
      # Add other inputs if needed
    ),
    
    mainPanel(
      # Display the plot
      plotOutput("crimePlot")
    )
  )
)

server <- function(input, output) {
  output$crimePlot <- renderPlot({
    # Filter data based on selected neighborhood
    filtered_incidents <- incidents %>%
      filter(neighborhood == input$neighborhood) %>%
      group_by(year) %>%
      summarize(total_count = n(),
                violent_count = sum(`Incident ID` %in% violent$`Incident ID`))
    
    # Plot the data
    ggplot(filtered_incidents, aes(x = year)) +
      geom_bar(aes(y = total_count, fill = "Total Crime"), position = "dodge", stat = "identity") +
      geom_bar(aes(y = violent_count, fill = "Violent Crime"), position = "dodge", stat = "identity") +
      labs(title = paste("Crime Counts by Year in", input$neighborhood),
           x = "Incident Year",
           y = "Incident Count",
           fill = "Crime Type") +
      scale_fill_manual(values = c("Total Crime" = "blue", "Violent Crime" = "red")) +
      theme_minimal() +
      ylim(0, 15000)
  })
}

shinyApp(ui, server)