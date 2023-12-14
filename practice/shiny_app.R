library(tidyverse)
library(plotly)
library(lubridate)
library(htmlwidgets)
library(scales)
library(patchwork)
library(tools)
library(shiny)

file_path <- file.path("/Users/jackson/Documents/sf_projects", "data/raw/sfpd_incidents_120223.csv")
data <- read_csv("data/raw/sfpd_incidents_120223.csv")
map_data <- read_csv(file_path) |>
  select(id = `Incident ID`, date = `Incident Date`, year = `Incident Year`, neighborhood = `Analysis Neighborhood`, 
         p_district = `Police District`, category = `Incident Category`, lat = `Latitude`, long = `Longitude`) |>
  mutate(category = case_when(
    category == "Human Trafficking (A), Commercial Sex Acts" ~ "Human Trafficking (A)",
    category == "Human Trafficking (B), Involuntary Servitude" ~ "Human Trafficking (B)",
    TRUE ~ category),
    violent = ifelse(category %in% c("Homicide", "Rape", "Robbery", "Assault",
                                     "Human Trafficking (B)", 
                                     "Human Trafficking (A)"), 1, 0),
    property = ifelse(category %in% c("Burglary", "Larceny Theft", "Motor Vehicle Theft", "Arson"), 1, 0),
    color = ifelse(violent > 0, "red", "#365188")
  )

max(map_data$date)

# UI
ui <- fluidPage(
  titlePanel("SF Crime Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("neighborhood", "Select Neighborhood", choices = unique(map_data$neighborhood)),
      ?dateRangeInput("dateRange", "Select Date Range", 
                     start = "2018-01-01", end = Sys.Date(),
                     min = "2018-01-01", max = max(map_data$date))
    ),
    mainPanel(
      plotlyOutput("propertyCrimePlot"),
      plotlyOutput("lineplot")
    )
  )
)

# Server
server <- function(input, output) {
  
  #addClass(selector = "body", class = "sidebar-collapse")
  
  filtered_data <- reactive({
    new_data %>%
      filter(neighborhood == input$neighborhood,
             date >= input$dateRange[1], date <= input$dateRange[2])
  })
  
  output$propertyCrimePlot <- renderPlot({
    # Your property crime plot code using filtered_data() instead of new_data
    per_year <- filtered_data() |>
      filter(year %in% c(this_year, last_year, two_years, three_years), category %in% property_crimes$category) %>%
      group_by(category, year) |> 
      summarize(count = n())
    
    yr_span_avg <- filtered_data() |> 
      filter(year %in% c(last_year, two_years, three_years), category %in% property_crimes$category) %>%
      group_by(category, year) |> 
      summarize(count = n()) |> 
      ungroup() |>
      group_by(category) |> 
      summarize(avg = mean(count))
    
    per_year |>
      inner_join(yr_span_avg, by = "category") |>
      filter(year == this_year | year == last_year) |>
      mutate(this_year = year == this_year) |>
      ggplot(aes(x = category, y = ifelse(this_year, count, avg), fill = as.factor(year))) +
      geom_col(position = "dodge", color = "black") +
      labs(title = "Decrease in Property Crime This Year \n Compared to 3-Year Average",
           x = "Category",
           y = "Incident Count",
           fill = "Year") +
      scale_fill_manual(
        values = c("2023" = "#AD7D2E", 
                   "2022" = "#365188")
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "black", color = "grey"),
        panel.background = element_rect(fill = "black", color = "grey"),
        panel.grid = element_blank(), 
        axis.text = element_text(color = "grey", size = 13),
        axis.ticks = element_line(color = "grey"),
        axis.ticks.length = unit(-5, "pt"),
        axis.title = element_text(color = "grey", size = 13),
        plot.title = element_text(color = "grey", hjust = 0.5, size = 15),
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        legend.text = element_text(color = "grey"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x = element_blank()
      )
  })
  
  output$lineplot <- renderPlotly({
    # Your lineplot code using filtered_data() instead of new_data
    lineplot_data <- filtered_data() |>
      mutate(month = month(date, label = T),
             month = factor(month.abb[month], levels = month.abb)) |>
      filter(date < floor_date(today(), "month")) |>
      group_by(year, month) |>
      summarize(count = n(),
                avg = mean(count),
                category = category)
    
    last_dec <- lineplot_data |>
      filter(month == "Dec") |>
      mutate(year = year - 1,
             month = "last_Dec") 
    
    ### Month of Jan next yr
    next_jan <- lineplot_data |>
      filter(month == "Jan") |>
      mutate(year = year + 1,
             month = "next_Jan") |>
      filter(year != this_year)
    
    ### Concatenate
    bind_rows(last_dec, lineplot_data, next_jan) |> 
      mutate(month = factor(month, levels =c("last_Dec", month.abb, "next_Jan")),
             month_number = as.numeric(month) - 1) |>
      filter(year != 2018) |>
      ggplot(aes(month_number, count, group = year, color = year)) +
      geom_hline(yintercept = mean(lineplot_data$avg), color = "#CCCCCC") +
      geom_line() +
      geom_point(shape = 21, size = 0.5) +
      scale_x_continuous(breaks = 1:12,
                         labels = month.abb
      ) +
      coord_cartesian(xlim = c(1,12)) +
      scale_y_continuous(breaks = seq(5000, 20000, 2000), sec.axis = dup_axis(name = NULL, label = NULL)) +
      scale_size_manual(breaks = c(FALSE, TRUE),
                        values = c(0.25, 1), guide = "none") +
      scale_color_gradient(breaks = seq(2018, 2023, 2), low = "#365188", high = "#AD7D2E") +
      labs(
        x = NULL,
        y = "Monthly Count",
        title = "Total Incident Reports from 2018 to Present By Month and Year"
      ) +
      theme(
        plot.background = element_rect(fill = "black", color = "grey"),
        panel.background = element_rect(fill = "black", color = "grey"),
        panel.grid = element_blank(),
        axis.text = element_text(color = "grey", size = 13),
        axis.ticks = element_line(color = "grey"),
        axis.ticks.length = unit(-5, "pt"),
        axis.title = element_text(color = "grey", size = 13),
        plot.title = element_text(color = "grey", hjust = 0.5, size = 15),
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        legend.text = element_text(color = "grey"),
        legend.key.height = unit(50, "pt")
      )
  })
}

shinyApp(ui, server)
