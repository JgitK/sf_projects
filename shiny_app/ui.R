library(shiny)
library(mapboxer)

shinyUI(fillPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  tabPanel(
    "Interactive map",
           div(class="outer",
               tags$head(
                 tags$style(HTML("
      div.outer {
  position: fixed;
  top: 0
  //41px;
  left: 0;
  right: 0;
  bottom: 0;
  overflow: hidden;
  padding: 0;
      }

  body, label, input, button, select {
  font-family: 'Helvetica Neue', Helvetica;
  font-weight: 200;
}
h1, h2, h3, h4 { font-weight: 400; }

#controls {
  background-color: grey;
  padding: 0 20px 20px 20px;
  cursor: move;
  opacity: 0.5;
  zoom: 0.8;
  transition: opacity 500ms 1s;
}

#controls:hover {
  opacity: 0.95;
  transition-delay: 0;
}

#cite {
  position: absolute;
  bottom: 10px;
  left: 100px;
  font-size: 12px;
}

.mapbox-container {
  background-color: black !important;
}

    "))
               ),

    mapboxerOutput("map", width = 1550, height = 1000),

    # titlePanel("SF Crime Map"),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
                  width = 330, height = "auto",
                  h3("Incident Reports SF"),
                  # sidebarLayout(
                  #   sidebarPanel(
                  #     id = "map-sidebar",

                  selectInput("neighborhoods", "Select Neighborhoods:", choices = c("All", unique(map_data$neighborhood))),
                  dateRangeInput("date_range", "Select Date Range:", start = "2022-01-01", end = "2022-12-31"),
                  selectInput("crime_type", "Select Crime Type:", c("All", "Part 1 Violent Crime", "Part 1 Property Crime"))
    ),

    tags$div(id="cite",
             'Data sourced from data.sfgov.org')
    # mainPanel(
    #   mapboxerOutput("map", width = 700, height = 800)
    # )
           )
  )
)
)
