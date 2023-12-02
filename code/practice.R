library(tidyverse)
library(flexdashboard)
library(ggridges)
library(shiny)
library(visdat)
library(plotly)
library(lubridate)

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




