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

new_data <- read_csv("data/raw/sfpd_incidents_120223.csv") |>
  select(id = `Incident ID`, date = `Incident Date`, year = `Incident Year`, neighborhood = `Analysis Neighborhood`, 
         p_district = `Police District`, category = `Incident Category`, lat = `Latitude`, long = `Longitude`) |>
  mutate(category = case_when(
    category == "Human Trafficking (A), Commercial Sex Acts" ~ "Human Trafficking (A)",
    category == "Human Trafficking (B), Involuntary Servitude" ~ "Human Trafficking (B)",
    TRUE ~ category
  )) |>
  select(category, date, year, neighborhood, lat, long)

violent_crimes <- new_data |> 
  filter(category %in% c("Homicide", "Rape", "Robbery", "Assault",
                         "Human Trafficking (B)", 
                         "Human Trafficking (A)"))

property_crimes <- new_data |> 
  filter(category %in% c("Burglary", "Larceny Theft", "Motor Vehicle Theft", "Arson"))

map_data <- new_data |>
  mutate(violent = ifelse(category %in% c("Homicide", "Rape", "Robbery", "Assault",
                                          "Human Trafficking (B)", 
                                          "Human Trafficking (A)"), 
                          1, 0),
         property = ifelse(category %in% c("Burglary", "Larceny Theft", "Motor Vehicle Theft", "Arson"),
                           1, 0)
        )


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

ggsave("visuals/incidents_barchart.png")








avg_date_range <- c(last_year, two_years, three_years)

# Calculate the average incidents over the specified date range
yr_span_avg <- new_data %>%
  filter(year(date) %in% avg_date_range, category %in% property_crimes$category) %>%
  group_by(category) %>%
  summarize(avg = mean(n()))

# Filter data for both average and this year
per_year <- new_data %>%
  filter((year(date) == this_year | year(date) %in% avg_date_range) & category %in% property_crimes$category) %>%
  group_by(category, year) %>%
  summarize(count = n())

# Join the datasets for plotting
plot_data <- per_year %>%
  inner_join(yr_span_avg, by = "category") %>%
  filter(year == this_year | year %in% avg_date_range) %>%
  mutate(this_year = year == this_year)

# Plotting with ggplot2
p1 <- ggplot(plot_data, aes(x = category, y = ifelse(this_year, count, avg), fill = as.factor(year))) +
  geom_col(position = "dodge", color = "black") +
  labs(title = "Comparison of Property Crime This Year\nwith 3-Year Average",
       x = "Category",
       y = "Incident Count",
       fill = "Year") +
  scale_fill_manual(values = c("2023" = "#AD7D2E", "2022" = "#365188")) +
  theme_minimal()

# Display the plot
print(p1)

