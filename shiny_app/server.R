library(shiny)
library(mapboxer)

function(input, output, session) {

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

  text_style <- list(
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
      filter((input$neighborhoods == "All" | neighborhood %in% input$neighborhoods),
             date >= input$date_range[1] & date <= input$date_range[2],
             if (input$crime_type == "All") TRUE else
               (input$crime_type == "Violent Crime" & violent > 0) |
               (input$crime_type == "Property Crime" & property > 0))
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
        center = c(-122.4254, 37.7743),
        zoom = 11
      ) %>%
      add_navigation_control(pos = "top-left") %>%
      add_circle_layer(
        circle_color = c("get", "color"),
        circle_radius = 3,
        popup = "Incident Category: {{category}}</br>Date: {{date}}"
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
      add_layer(text_style) %>%
      add_scale_control(
        pos = "bottom-left",
        unit = "miles"
      )
  })
}


