library(tidyverse)
library(plotly)
library(lubridate)
library(htmlwidgets)
library(scales)
library(patchwork)
library(showtext)
### Dates
this_year = year(today())
this_month = month(today())
last_year = this_year - 1
two_years = last_year - 1
three_years = two_years - 1


### New Data
new_data <- read_csv("data/raw/incident_reports_new.csv") |>
  select(id = `Incident ID`, date = `Incident Date`, year = `Incident Year`, neighborhood = `Analysis Neighborhood`, 
         p_district = `Police District`, category = `Incident Category`, lat = `Latitude`, long = `Longitude`) |>
  mutate(category = case_when(
           category == "Human Trafficking (A), Commercial Sex Acts" ~ "Human Trafficking (A)",
           category == "Human Trafficking (B), Involuntary Servitude" ~ "Human Trafficking (B)",
           TRUE ~ category
         )) |>
  select(category, date, year)
  


### Old Data
old_data <- read_csv("data/raw/incident_reports_old.csv") |>
  select(category = "Category", date = "Date") |>
  mutate(date = mdy(date),
         year = year(date),
         category = str_replace(toTitleCase(tolower(category)), "/", " ")) |>
  filter(year != 2018)
  

### Merge old and new data


combined_data <- bind_rows(old_data, new_data) |>
  mutate(month = month(date, label = T),
         month = factor(month.abb[month], levels = month.abb)) |>
  filter(as.numeric(date) <= as.numeric(today()) - 60) |>
  group_by(year, month) |>
  summarize(count = n(),
            avg = mean(count), 
            category = category)

today() - days(60)


# Homogenize category names
# bind_rows(old_data, new_data) |>
#   group_by(category) |>
#   summarize(n = n()) |>
#   print(n = 70)

combined_data |>
  mutate(category = case_when(
    category == "Human Trafficking, Commercial Sex Acts" ~ "Human Trafficking (A)",
    category == "Motor Vehicle Theft" | category == "Motor Vehicle Theft?" ~ "Vehicle Theft",
    category == "Sex Offenses, Forcible" ~ "Rape",
    TRUE ~ category
    
  ))

### Define Violent/Property crimes

violent_crimes <- combined_data |> 
  filter(category %in% c("Homicide", "Rape", "Robbery", "Assault",
                         "Human Trafficking (B)", 
                         "Human Trafficking (A)"))

property_crimes <- incidents |> 
  filter(category %in% c("Burglary", "Larceny Theft", "Motor Vehicle Theft", "Arson"))

