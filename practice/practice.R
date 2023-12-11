library(tidyverse)
library(flexdashboard)
library(ggridges)
library(shiny)
library(visdat)
library(plotly)
library(lubridate)
library(tidycensus)
library(leaflet)

incidents <- read_csv("data/raw/incident_reports_new.csv") |>
  select(id = `Incident ID`, date = `Incident Date`, year = `Incident Year`, neighborhood = `Analysis Neighborhood`, 
         p_district = `Police District`, category = `Incident Category`, lat = `Latitude`, long = `Longitude`)

violent_crimes <- incidents |> 
  filter(category %in% c("Homicide", "Rape", "Robbery", "Assault",
                         "Human Trafficking (B), Involuntary Servitude", 
                         "Human Trafficking (A), Commercial Sex Acts"))

this_year = year(today()) - 1
last_year = this_year - 1

property_crimes <- incidents |> 
  filter(category %in% c("Burglary", "Larceny Theft", "Motor Vehicle Theft", "Arson"))

part_1 = incidents |>
  filter(category %in% violent_crimes | property_crimes)

filtered_data <- incidents %>%
  filter(year %in% c(this_year, last_year), category %in% violent_crimes$category)

# Group by category, year, and calculate the count
grouped_data <- filtered_data %>%
  group_by(category, year) %>%
  summarize(count = n())

grouped_data <- grouped_data |>
  mutate(category = case_when(
    category == "Human Trafficking (A), Commercial Sex Acts" ~ "Human Trafficking (A)",
    category == "Human Trafficking (B), Involuntary Servitude" ~ "Human Trafficking (B)",
    TRUE ~ category
  ))

grouped_data |>
  ggplot(aes(x = category, y = count, color = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

# Create a bar plot
p <- ggplot(grouped_data, aes(x = category, y = count, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Comparison of Total Counts for Each Category",
       x = "Category",
       y = "Incident Count",
       fill = "Year") +
  scale_fill_brewer(palette = "Accent") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplotly(p)

?vis_dat

incidents |>
  sample_n(5000) |>
  vis_dat(facet = p_district)

### High density of NA values "outside of sf"; will drop.

incidents <- incidents |>
  drop_na()

incidents |>
  distinct(category) |>
  print(n = 50)

violent_crimes |> 
  group_by(year) |>
  summarize(count = n()) |>
  ggplot(aes(x = year, y = count)) +
  geom_col()

p1 <- ggplotly(p)
p1

violent_crimes <- incidents |> 
  filter(category %in% c("Homicide", "Rape", "Robbery", "Assault",
                         "Human Trafficking (B), Involuntary Servitude", 
                         "Human Trafficking (A), Commercial Sex Acts"))

incidents |>
  mutate(violent = ifelse(category %in% violent_crimes$category, category, NA)) |>
  group_by(category, year) |>
  summarize(count = n(),
            violent = count(violent)) |>
  ggplot(aes(x = year, y = count, fill = violent)) +
  geom_col(position = "dodge") 


q1 <- ggplotly(q)

q1




violent |>
  filter(year == 2023) |>
  group_by(category) |>
  summarize(n = n())


incidents |> 
  group_by(neighborhood, year) |>
  summarize(incident_count = n(),
            violent_count = n(violent)) |>
  #filter(neighborhood == "North Beach") |>
  ggplot(aes(x = year, y = incident_count, fill = violent_count)) +
  geom_bar(stat = "identity") +
  theme_light()

incidents |>
  ggplot(aes(x = year, y = ))

violent |>
  group_by(neighborhood, year) |>
  summarize(incident_count = n()) |>
  ggplot(aes(x = year, y = incident_count))  +
  geom_bar(stat = "identity")


p <- incidents %>%
  group_by(neighborhood, year) %>%
  summarize(total_count = n(),
            violent_count = sum(id %in% violent$id)) %>%
  ggplot(aes(x = year)) +
  geom_bar(aes(y = total_count, fill = "Total Crime"), position = "dodge", stat = "identity") +
  geom_bar(aes(y = violent_count, fill = "Violent Crime"), position = "dodge", stat = "identity") +
  labs(title = "Total vs Violent Crime Counts by Year",
       x = "Incident Year",
       y = "Incident Count",
       fill = "Crime Type") +
  scale_fill_manual(values = c("Total Crime" = "darkblue", "Violent Crime" = "aquamarine")) +
  scale_x_continuous(breaks = seq(2018, 2023, 1)) +
  theme_minimal() 

ggplotly(p)

violent |>
  filter(!is.na(neighborhood)) |>
  group_by(neighborhood, year) |>
  summarize(total_count = n()) |>
  ggplot(aes(x = year, y = total_count)) +
  geom_bar(stat = "identity")

ggplot(df, aes(x = year, y = incident_count, fill = drv, color = drv)) +
  geom_density_ridges(alpha = 0.5, show.legend = FALSE)
#> Picking joint bandwidth of 1.28


incidents |> 
  filter(p_district == "Richmond", category == "Motor Vehicle Theft") |> 
  group_by(year) |> 
  summarize(count = n()) |> 
  ggplot(aes(year, count)) +
  geom_bar(stat="identity") 

incidents |> 
  group_by(p_district) |> 
  summarize(count = n()) |> 
  ggplot(aes(p_district, count, fill=p_disctrict)) +
  geom_bar(stat="identity") +
  facet_wrap(~year)

ggplot(incidents, aes(x=fct_infreq(p_district))) + ## fct_infrew reorders by count value
  geom_bar(aes(fill = p_district)) +
  facet_wrap(~year) +
  theme(axis.text.x = element_blank()) +
  coord_flip()

incidents |>
  mutate(p_district |> fct_infreq()) |>
  ggplot(aes(x = p_district)) +
  geom_bar()

ggplot(df, aes(x=year)) +
  geom_bar() +
  facet_wrap(~`Police District`)

ggplot(df, aes(year, y=after_stat(prop), group=1)) +
  geom_bar()


ggplot(df, aes(`Longitude`, `Latitude`, group = group)) +
  geom_polygon(fill = "white", color = "black")

ca_counties <- map_data("county", "california") |> 
  select(lon = long, lat, group, id = "subregion")

ca <- map_data("county", "california") |> 
  select(long, lat, subregion)

ca


ggplot(ca_counties, aes(lon, lat, group = group)) +
  geom_polygon(aes(fill = "white", color = "black")) +
  coord_quickmap()

ggplot(ca_counties, aes(lon, lat)) +
  geom_point(size = 0.25, show.legend=F) +
  coord_quickmap()

sf <- ca_counties |> 
  filter(id == "san francisco")

sf

ggplot(sf, aes(lon, lat)) +
  geom_point(size = .25, show.legend = F) +
  coord_quickmap()

sf_map <- ggplot(sf, aes(lon, lat)) +
  geom_polygon(fill = NA, color = "red") +
  coord_quickmap()
sf_map

ggplot(df, aes(`Longitude`, `Latitude`)) +
  geom_point(size = 0.25, alpha = 0.05) +
  coord_quickmap()

sf_map_points <- sf |> 
  select(lon, lat)
sf_map_points


df |> 
  filter(year == 2020) |> 
  ggplot(aes(`Longitude`, `Latitude`)) +
  geom_point(size=0.25) +
  coord_quickmap()

ggplot(sf, aes(lon, lat, id=id)) +
  geom_map(map = sf, data = sf,
           fill = NA, color = "red")

?geom_map

sf <- sf |> 
  mutate(
    x = lon,
    y = lat
  )

ggplot(df, aes(`Longitude`, `Latitude`)) +
  geom_map(data = sf, aes(map_id = id),
           map = sf, fill = NA, color = "red") +
  expand_limits(x=sf$lon, y=sf$lat) +
  geom_point(alpha = 0.2)

ggplot(sf, aes(lon, lat)) +
  geom_map(data = sf, aes(map_id = id), map = sf, fill = NA, color = "red") +
  expand_limits(x = sf$lon, y = sf$lat) +
  geom_point(data = df, aes(Longitude, Latitude), size = 0.1, alpha = 0.1) +
  coord_fixed()

head(sf)

head(df)

### Distinct categories of crime old/new
distinct_crimes_new <- incidents |>
  distinct(category)

unique_to_new <- anti_join(distinct_crimes_new, distinct_crimes_old) |> print(n = 31)
unique_to_old <- anti_join(distinct_crimes_old, distinct_crimes_new)



library(tidyverse)
library(plotly)
library(lubridate)
library(htmlwidgets)
library(scales)
library(patchwork)
library(tools)
library(shiny)
#library(showtext)
### Dates
this_year = year(today())
this_month = month(today())
last_year = this_year - 1
two_years = last_year - 1
three_years = two_years - 1


### New Data
new_data <- read_csv("data/raw/sfpd_incidents_120223.csv") |>
  select(id = `Incident ID`, date = `Incident Date`, year = `Incident Year`, neighborhood = `Analysis Neighborhood`, 
         p_district = `Police District`, category = `Incident Category`, lat = `Latitude`, long = `Longitude`) |>
  mutate(category = case_when(
    category == "Human Trafficking (A), Commercial Sex Acts" ~ "Human Trafficking (A)",
    category == "Human Trafficking (B), Involuntary Servitude" ~ "Human Trafficking (B)",
    TRUE ~ category
  )) |>
  select(category, date, year, neighborhood)

new_data |>
  distinct(category) |> print(n=50)



violent_crimes <- new_data |> 
  filter(category %in% c("Homicide", "Rape", "Robbery", "Assault",
                         "Human Trafficking (B)", 
                         "Human Trafficking (A)"))

property_crimes <- new_data |> 
  filter(category %in% c("Burglary", "Larceny Theft", "Motor Vehicle Theft", "Arson"))

yr_span_avg <- new_data |> 
  filter(year %in% c(last_year, two_years, three_years), category %in% property_crimes$category) %>%
  group_by(category, year) |> 
  summarize(count = n()) |> 
  ungroup() |>
  group_by(category) |> 
  summarize(avg = mean(count))

per_year <- new_data |>
  filter(year %in% c(this_year, last_year, two_years, three_years), category %in% property_crimes$category) %>%
  group_by(category, year) |> 
  summarize(count = n())

p1 <- per_year |>
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

lineplot_data <- new_data |>
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
p2 <- bind_rows(last_dec, lineplot_data, next_jan) |> 
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
  scale_color_gradient(breaks = seq(2003, 2023, 10), low = "#365188", high = "#AD7D2E") +
  # scale_fill_stepsn(colors = c("#365188", "#CCCCCC", "#AD7D2E"),
  #                   values = rescale(c(min(combined_data$year), c(mean(combined_data$year)), max(combined_data$year))),
  #                   limits = c(min(combined_data$year), max(combined_data$year))) +
  # scale_color_viridis_c(breaks = seq(2003, 2023, 4), 
  #                       guide = guide_colorbar(frame.colour = "white", frame.linewidth = 0.5)) +
  labs(
    x = NULL,
    y = "Monthly Count",
    title = "Total Incident Reports from 2018 to Present By Month and Year"
  ) +
  theme(
    plot.background = element_rect(fill = "black", color = "grey"),
    panel.background = element_rect(fill = "black", color = "grey"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "grey", size = 13
                             #, family = "sourceserif4", cex = 4
    ),
    axis.ticks = element_line(color = "grey"),
    axis.ticks.length = unit(-5, "pt"),
    axis.title = element_text(color = "grey", size = 13
                              #, family = "sourceserif4", cex = 4
    ),
    plot.title = element_text(color = "grey", hjust = 0.5, size = 15
                              #, family = "sourceserif4", cex = 4
    ),
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA),
    legend.text = element_text(color = "grey"
                               #, family = "sourceserif4"
    ),
    legend.key.height = unit(50, "pt")
  ) 

p1 / p2
Sys.setenv(MAPBOX_API_TOKEN = "pk.eyJ1Ijoia2VudGV1cjM3MiIsImEiOiJjbG9hYTNhMG4wYnBpMmtyMTcxN3c2eHc0In0.VK1I6nS5DVbKt7sh83LGvw")
library(tidyverse)
library(shiny)
library(shinydashboard)
library(mapboxer)
library(dplyr)
library(shinyjs)

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

install.packages("RSocrata")
library(RSocrata)

df <- read.socrata(
  "https://data.sfgov.org/resource/wg3w-h783.csv"
  # ,
  # app_token = "cDDMnHCMX0FrwTVTc9tkuXmHT"
)
head(df)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .floating-panel {
        position: fixed;
        top: 10px;
        left: 10px;
        z-index: 1000;
        background-color: white;
        padding: 10px;
        border: 1px solid #ddd;
        border-radius: 5px;
        opacity: 0.8;
        transition: opacity 0.3s;
      }
      .floating-panel:hover {
        opacity: 1;
      }
      .map-container {
        margin-left: 250px; /* Adjust margin to accommodate the sidebar width */
      }
    "))
  ),
  titlePanel("SF Crime Map"),
  shinyjs::useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      selectInput("neighborhoods", "Select Neighborhoods:", choices = c("All", unique(map_data$neighborhood))),
      dateRangeInput("date_range", "Select Date Range:", start = "2022-01-01", end = "2022-12-31"),
      selectInput("crime_type", "Select Crime Type:", c("All", "Violent Crime", "Property Crime"))
    ),
    mainPanel(
      mapboxerOutput("map"),
      shinyjs::enable("map_controls")
    ),
    absolutePanel(
      id = "map_controls",
      class = "floating-panel",
      draggable = TRUE
    )
  )
)

ui <- fillPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  tabPanel("Interactive map",
           div(class="outer",
  tags$head(
    tags$style(HTML("
      .map-sidebar {
        position: fixed;
        top: 10px;
        left: 10px;
        z-index: 1000;
        background-color: white;
        padding: 10px;
        border: 1px solid #ddd;
        border-radius: 5px;
        opacity: 0.8;
        transition: opacity 0.3s;
      }
      .map-sidebar:hover {
        opacity: 1;
      }
    "))
  ),
  
  mapboxerOutput("map", width = 900, height = 800),
  
  # titlePanel("SF Crime Map"),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto", 
                h3("Incident Reports SF"),
  # sidebarLayout(
  #   sidebarPanel(
  #     id = "map-sidebar",  
      
      selectInput("neighborhoods", "Select Neighborhoods:", choices = c("All", unique(map_data$neighborhood))),
      dateRangeInput("date_range", "Select Date Range:", start = "2022-01-01", end = "2022-12-31"),
      selectInput("crime_type", "Select Crime Type:", c("All", "Violent Crime", "Property Crime"))
    ),
  
  tags$div(id="cite",
           'Data sourced from data.sfgov.org')
    # mainPanel(
    #   mapboxerOutput("map", width = 700, height = 800)
    # )
  )
)
)

# Define server
server <- function(input, output, session) {
  
  circle_color <- list('step',
                       list('get', 'point_count'),
                       '#AD7D2E', 300, '#AD7D2E', 1000, '#AD7D2E'
  )
  
  circle_radius <- list(
    'step',
    list('get', 'point_count'), 9, 400, 13, 3000, 20
  )
  
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

# Run the app
shinyApp(ui, server)
