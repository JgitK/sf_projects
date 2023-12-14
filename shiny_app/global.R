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

this_year = year(today())

data <- 
   read_csv("sf_incidents_new.csv") |>
   filter(`Incident Year` == 2023) |>
  #read.socrata("https://data.sfgov.org/resource/wg3w-h783.csv") |>
   select(date = `Incident Date`, year = `Incident Year`, neighborhood = `Analysis Neighborhood`,
          category = `Incident Category`, lat = `Latitude`, long = `Longitude`, id = `Incident ID`) |>
    # select(incident_id, date = incident_date, year = incident_year, neighborhood = analysis_neighborhood, 
    #        p_district = police_district, category = incident_category, lat = latitude, long = longitude) |>
  mutate(category = case_when(
    category == "Human Trafficking (A), Commercial Sex Acts" ~ "Human Trafficking (A)",
    category == "Human Trafficking (B), Involuntary Servitude" ~ "Human Trafficking (B)",
    TRUE ~ category),
    violent = ifelse(category %in% c("Homicide", "Rape", "Robbery", "Assault",
                                     "Human Trafficking (B)", 
                                     "Human Trafficking (A)"), 1, 0),
    property = ifelse(category %in% c("Burglary", "Larceny Theft", "Motor Vehicle Theft", "Arson"), 1, 0),
    )

map_data <- data |>
  mutate(color = ifelse(violent > 0, "red", ifelse(property > 0, "yellow", "#365188")))

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