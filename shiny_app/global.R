library(readr)
library(dplyr)
library(ggplot2)
library(shiny)
library(RSocrata)
library(lubridate)
library(mapboxer)

#MAPBOX_API_TOKEN = "pk.eyJ1Ijoia2VudGV1cjM3MiIsImEiOiJjbHExcWFjeTYwOG5jMmpqd2c0OHB3cWdyIn0.EKTr6nh50rib3jY6tVN-Iw"

# library(showtext)
#
# source("keys.R")
# font_add_google(name = "Open Sans",
#                 family = "hn")
# showtext_auto()

# Define crime categories as constants for better performance
VIOLENT_CRIMES <- c("Homicide", "Rape", "Robbery", "Assault",
                    "Human Trafficking (B)", "Human Trafficking (A)")
PROPERTY_CRIMES <- c("Burglary", "Larceny Theft", "Motor Vehicle Theft", "Arson")

this_year <- year(today())

# Optimized data pipeline: combined loading and transformation in single pipeline
# Precompute derived fields (month, violent, property, color) to avoid repeated calculations
map_data <- read_csv(
  "sf_incidents_new.csv",
  col_types = cols(
    `Incident Date` = col_date(format = ""),
    `Incident Year` = col_integer(),
    `Analysis Neighborhood` = col_character(),
    `Incident Category` = col_character(),
    `Latitude` = col_double(),
    `Longitude` = col_double(),
    `Incident ID` = col_character()
  )
) |>
  filter(`Incident Year` == 2023) |>
  #read.socrata("https://data.sfgov.org/resource/wg3w-h783.csv") |>
  select(date = `Incident Date`, year = `Incident Year`,
         neighborhood = `Analysis Neighborhood`,
         category = `Incident Category`, lat = `Latitude`,
         long = `Longitude`, id = `Incident ID`) |>
  # select(incident_id, date = incident_date, year = incident_year, neighborhood = analysis_neighborhood,
  #        p_district = police_district, category = incident_category, lat = latitude, long = longitude) |>
  mutate(
    category = case_when(
      category == "Human Trafficking (A), Commercial Sex Acts" ~ "Human Trafficking (A)",
      category == "Human Trafficking (B), Involuntary Servitude" ~ "Human Trafficking (B)",
      TRUE ~ category
    ),
    # Use as.integer() instead of ifelse() for better performance
    violent = as.integer(category %in% VIOLENT_CRIMES),
    property = as.integer(category %in% PROPERTY_CRIMES),
    # Use case_when() instead of nested ifelse() for clarity and performance
    color = case_when(
      violent == 1 ~ "red",
      property == 1 ~ "yellow",
      TRUE ~ "#365188"
    ),
    # Precompute month to avoid recalculating in renderPlot
    month = month(date)
  )

# lineplot_data <- data |>
#   select(category, date, year, neighborhood, violent, property) |>
#   mutate(month = month(date, label = T),
#          month = factor(month.abb[month], levels = month.abb)) |>
#   filter(date < floor_date(today(), "month")) |>
#   group_by(year, month) |>
#   mutate(count = n(),
#             avg = mean(count),
#          violent = violent,
#          property = property) |>
#   ungroup()