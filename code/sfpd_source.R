library(tidyverse)
library(plotly)
library(lubridate)
library(htmlwidgets)
library(scales)
library(patchwork)
library(tools)
library(shiny)
#library(showtext)

# Define crime categories as constants for better performance
VIOLENT_CRIMES <- c("Homicide", "Rape", "Robbery", "Assault",
                    "Human Trafficking (B)", "Human Trafficking (A)")
PROPERTY_CRIMES <- c("Burglary", "Larceny Theft", "Motor Vehicle Theft", "Arson")

### Dates
this_year <- year(today())
this_month <- month(today())
last_year <- this_year - 1
two_years <- last_year - 1
three_years <- two_years - 1


### Old Data
# old_data <- read_csv("data/raw/incident_reports_old.csv") |>
#   select(category = "Category", date = "Date") |>
#   mutate(date = mdy(date),
#          year = year(date),
#          category = str_replace(toTitleCase(tolower(category)), "/", " ")) |>
#   filter(year != 2018)


### New Data - Optimized pipeline
new_data <- read_csv("data/raw/sfpd_incidents_120223.csv") |>
  select(id = `Incident ID`, date = `Incident Date`, year = `Incident Year`,
         neighborhood = `Analysis Neighborhood`,
         p_district = `Police District`, category = `Incident Category`,
         lat = `Latitude`, long = `Longitude`) |>
  mutate(
    category = case_when(
      category == "Human Trafficking (A), Commercial Sex Acts" ~ "Human Trafficking (A)",
      category == "Human Trafficking (B), Involuntary Servitude" ~ "Human Trafficking (B)",
      TRUE ~ category
    )
  ) |>
  select(category, date, year, neighborhood)

new_data |>
  distinct(category) |> print(n=50)


### Merge old and new data

###Right now only getting the count for the month as of whatever day it is in that month two months ago from today...

# Optimized: fix summarize issues - removed problematic category and avg calculations
combined_data <- bind_rows(old_data, new_data) |>
  filter(date < floor_date(today(), "month")) |>
  mutate(month = month(date, label = TRUE)) |>
  count(year, month, name = "count")



# Homogenize category names
# bind_rows(old_data, new_data) |>
#   group_by(category) |>
#   summarize(n = n()) |>
#   print(n = 70)

# combined_data <- combined_data |>
#   mutate(category = case_when(
#     category == "Human Trafficking, Commercial Sex Acts" ~ "Human Trafficking (A)",
#     category == "Motor Vehicle Theft" | category == "Motor Vehicle Theft?" ~ "Vehicle Theft",
#     category == "Sex Offenses, Forcible" ~ "Rape",
#     TRUE ~ category
#
#   ))

### Define Violent/Property crimes

# violent_crimes <- combined_data |>
#   filter(category %in% VIOLENT_CRIMES)
#
# property_crimes <- combined_data |>
#   filter(category %in% PROPERTY_CRIMES)

# Use constants for better performance
violent_crimes <- new_data |>
  filter(category %in% VIOLENT_CRIMES)

property_crimes <- new_data |>
  filter(category %in% PROPERTY_CRIMES)

