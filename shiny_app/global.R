library(tidyverse)
library(RSocrata)

#data <- read.socrata("https://data.sfgov.org/resource/wg3w-h783.csv")
map_data <- read.socrata("https://data.sfgov.org/resource/wg3w-h783.csv") |>
  select(incident_id, date = incident_date, year = incident_year, neighborhood = analysis_neighborhood, 
         p_district = police_district, category = incident_category, lat = latitude, long = longitude) |>
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
