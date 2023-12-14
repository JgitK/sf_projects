library(shiny)
library(mapboxer)
library(ggplot2)

shinyServer(function(input, output, session) {

  circle_color <- '#AD7D2E'
    
  #   list('step',
  #                      list('get', 'point_count'),
  #                      '#AD7D2E', 300, '#AD7D2E', 1000, '#AD7D2E'
  # )

  circle_radius <- 18
  #   list(
  #   'step',
  #   list('get', 'point_count'), 9, 400, 13, 3000, 20
  # )

  cluster_style <- list(
    "id" = "cluster-count",
    "type" = "symbol",
    "filter" = c("has", "point_count"),
    "layout" = list(
      "text-field" = "{point_count_abbreviated}",
      "text-size" = 12
    )
  ) 

  filtered_data <- reactive({
    filter_data <- map_data %>%
      filter((input$neighborhoods == "All" | map_data$neighborhood %in% input$neighborhoods),
             date >= input$date_range[1] & date <= input$date_range[2],
             (
               input$crime_type == "All" |
                 (input$crime_type == "Part 1 Violent Crime" & violent > 0) |
                 (input$crime_type == "Part 1 Property Crime" & property > 0)
             )
      )
    return(filter_data)
  })

  output$map <- renderMapboxer({

    filtered_data() %>%
      as_mapbox_source(lng = "long", lat = "lat",
                       cluster = TRUE,
                       clusterMaxZoom = 12,
                       clusterRadius = 50
      ) %>%
      mapboxer(
        style = basemaps$Mapbox$dark_v10,
        token = MAPBOX_API_TOKEN,
        center = c(-122.4254, 37.7743),
        zoom = 11
      ) %>%
      add_navigation_control(pos = "top-left") %>%
      add_circle_layer(
        id = "circle-layer",
        popup = "Incident Category: {{category}}</br>Date: {{date}}",
        circle_color = c("get", "color"),
        circle_radius = 3
      ) %>%
      add_circle_layer(
        id = "cluster_layer",
        filter = c("has", "point_count"),
        circle_blur = 0.3,
        circle_stroke_color = "white",
        circle_stroke_opacity = 1,
        circle_stroke_width = 1,
        circle_color = circle_color,
        circle_radius = circle_radius
      ) %>%
      add_layer(cluster_style) %>%
      add_scale_control(
        pos = "bottom-left",
        unit = "miles"
      )
  })
  
  output$barchart <- renderPlot({
    filtered_data() %>%
      mutate(month = month(date)) %>%
      group_by(month) %>%
      summarize(`Monthly Count` = n()) %>%
      ggplot(aes(x = month, y = `Monthly Count`)) +
      geom_col(fill = "#AD7D2E") +
      scale_x_continuous(breaks = seq(1, 12, 1), labels = month.abb) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#7A7A7A", color = "#7A7A7A"
          ),
        panel.background = element_rect(fill = "#7A7A7A", color = "#7A7A7A"
          ),
        panel.grid = element_blank(), 
        axis.text = element_text(color = "white", size = 12),
        axis.ticks = element_line(color = "white"),
        axis.ticks.length = unit(-5, "pt"),
        axis.title = element_blank(),
        plot.title = element_text(color = "black", hjust = 0.5, size = 15),
        legend.title = element_text(),
        legend.background = element_rect(fill = NA),
        legend.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 40, hjust = 1),
        axis.title.x = element_blank()
      )
  })
  
  # showPopup <- function(id, lat, lng) {
  #   selectedData <- map_data[map_data$id == id, ]
  #   content <- as.character(tagList(
  #     tags$h4("Additional Information"),
  #     # Customize the content based on your data frame columns
  #     tags$strong("ID: ", id), tags$br(),
  #     tags$strong("Incident Category: ", selectedData$category), tags$br(),
  #     tags$span("Date: ", selectedData$date)
  #     # Add more tags as needed
  #   ))
  #   
  #   mapboxer_proxy("map") %>%
  #     addPopups(lng, lat, content, layerId = id)
  #     
  # }
  # 
  # # When map is clicked, show a popup with additional information
  # observeEvent({
  #   mapbox_proxy("map") %>% clearPopups()
  #   event <- input$map_shape_click
  #   if (is.null(event))
  #     return()
  #   
  #   isolate({
  #     showPopup(event$id, event$lat, event$lng)
  #   })
  #   update_mapboxer()
  # })
  
  # last_dec <- lineplot_data |>
  #   filter(month == "Dec") |>
  #   mutate(year = year - 1,
  #          month = "last_Dec") 
  # 
  # next_jan <- lineplot_data |>
  #   filter(month == "Jan") |>
  #   mutate(year = year + 1,
  #          month = "next_Jan") |>
  #   filter(year != this_year)
  
  # filtered_data_line <- reactive({
  #   filter_data <- lineplot_data %>%
  #     filter((input$neighborhoods == "All" | lineplot_data$neighborhood %in% input$neighborhoods),
  #            date >= input$date_range[1] & date <= input$date_range[2],
  #            if (input$crime_type == "All") TRUE else
  #              (input$crime_type == "Violent Crime" & violent > 0) |
  #              (input$crime_type == "Property Crime" & property > 0))
  #   return(filter_data)
  
  output$legend <- renderUI({
    legend_content <- tags$div(
      tags$div(
        tags$span(style = "color:red; font-size: 20px; margin-right: 10px;", HTML("&#8226;")),
        "Part 1 Violent Crime", style = "color:white; font-size: 16px;"
      ),
      tags$div(
        tags$span(style = "color:yellow; font-size: 20px; margin-right: 10px;", HTML("&#8226;")),
        "Part 1 Property Crime", style = "color:white; font-size: 16px;"
      ),
      tags$div(
        tags$span(style = "color:#365188; font-size: 20px; margin-right: 10px;", HTML("&#8226;")),
        "Other", style = "color:white; font-size: 16px;"
      )
      
    )
    return(legend_content)
  })
  
  # filtered_data_bar <- reactive({
  #   filter_data <- data %>%
  #     filter((input$neighborhoods == "All" | data$neighborhood %in% input$neighborhoods),
  #            date >= input$date_range[1] & date <= input$date_range[2],
  #            if (input$crime_type == "All") TRUE else
  #              (input$crime_type == "Violent Crime" & violent > 0) |
  #              (input$crime_type == "Property Crime" & property > 0))
  #   return(filter_data)
  # })
  
  # output$barchart <- renderPlot({
  #   filtered_data_bar() |>
  #     group_by(year) |>
  #     summarize(`Incident Count` = n()) |>
  #     ggplot(aes(x = year, y = `Incident Count`)) +
  #     geom_col(fill = "#AD7D2E") +
  #     scale_x_continuous(breaks = seq(2018, 2023, 1)) +
  #     theme_minimal() +
  #     theme(
  #       plot.background = element_rect(fill = "grey", color = "grey"),
  #       panel.background = element_rect(fill = "grey", color = "grey"),
  #       panel.grid = element_blank(), 
  #       axis.text = element_text(color = "grey", size = 13),
  #       axis.ticks = element_line(color = "grey"),
  #       axis.ticks.length = unit(-5, "pt"),
  #       axis.title = element_text(color = "grey", size = 13),
  #       plot.title = element_text(color = "grey", hjust = 0.5, size = 15),
  #       legend.title = element_blank(),
  #       legend.background = element_rect(fill = NA),
  #       legend.text = element_text(color = "grey"),
  #       axis.text.x = element_text(angle = 30, hjust = 1),
  #       axis.title.x = element_blank()
  #     )
  # })

  # output$lineplot <- renderPlot({
  #   bind_rows(last_dec, filtered_data_line(), next_jan) |>
  #     mutate(month = factor(month, levels =c("last_Dec", month.abb, "next_Jan")),
  #            month_number = as.numeric(month) - 1) |>
  #     filter(year != 2018) |>
  #     ggplot(aes(month_number, count, group = year, color = year)) +
  #     geom_hline(yintercept = mean(lineplot_data$avg), color = "#CCCCCC") +
  #     geom_line() +
  #     geom_point(shape = 21, size = 0.5) +
  #     scale_x_continuous(breaks = 1:12,
  #                        labels = month.abb
  #     ) +
  #     coord_cartesian(xlim = c(1,12)) +
  #     scale_y_continuous(breaks = seq(5000, 20000, 2000), sec.axis = dup_axis(name = NULL, label = NULL)) +
  #     scale_size_manual(breaks = c(FALSE, TRUE),
  #                       values = c(0.25, 1), guide = "none") +
  #     scale_color_gradient(breaks = seq(2003, 2023, 10), low = "#365188", high = "#AD7D2E") +
  #     labs(
  #       x = NULL,
  #       y = "Count Per Month"
  #       #, title = "Total Incident Reports from 2003 to Present By Year"
  #     ) +
  #     theme(
  #       plot.background = element_rect(fill = "black", color = "grey"),
  #       panel.background = element_rect(fill = "black", color = "grey"),
  #       panel.grid = element_blank(),
  #       axis.text = element_text(color = "grey", size = 13
  #                                , family = "hn"
  #       ),
  #       axis.ticks = element_line(color = "grey"),
  #       axis.ticks.length = unit(-5, "pt"),
  #       axis.title = element_text(color = "grey", size = 13
  #                                 , family = "hn"
  #       ),
  #       plot.title = element_text(color = "grey", hjust = 0.5, size = 15
  #                                 , family = "hn"
  #       ),
  #       legend.title = element_blank(),
  #       legend.background = element_rect(fill = NA),
  #       legend.text = element_text(color = "grey"
  #                                  , family = "hn"
  #       ),
  #       legend.key.height = unit(50, "pt")
  #     )
  # })
}
)

?add_popups
