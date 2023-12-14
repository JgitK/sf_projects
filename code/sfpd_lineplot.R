library(tidyverse)
library(plotly)
library(lubridate)
library(htmlwidgets)
library(scales)
library(patchwork)
library(tools)
library(shiny)
library(showtext)

#ls()

font_add_google(name = "Helvetica Nue",
                family = "hn")
font_add_google(name = "Source Serif 4",
                family = "sourceserif4")
showtext_auto()
# 
# ?font_add_google

### Month of Dec last yr
# last_dec <- combined_data |>
#   filter(month == "Dec") |>
#   mutate(year = year - 1,
#          month = "last_Dec") 
# 
# ### Month of Jan next yr
# next_jan <- combined_data |>
#   filter(month == "Jan") |>
#   mutate(year = year + 1,
#          month = "next_Jan") |>
#   filter(year != this_year)

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

 lineplot_data <- new_data |>
   mutate(month = month(date, label = T),
                   month = factor(month.abb[month], levels = month.abb)) |>
   filter(date < floor_date(today(), "month")) |>
   group_by(year, month) |>
   summarize(count = n(),
             avg = mean(count),
             category = category)
 
 this_year = year(today())
 this_month = month(today())
 last_year = this_year - 1
 two_years = last_year - 1
 three_years = two_years - 1

last_dec <- lineplot_data |>
  filter(month == "Dec") |>
  mutate(year = year - 1,
         month = "last_Dec") 

next_jan <- lineplot_data |>
  filter(month == "Jan") |>
  mutate(year = year + 1,
         month = "next_Jan") |>
  filter(year != this_year)

monthly_barchart <- data %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarize(`Incident Count` = n()) %>%
  ggplot(aes(x = month, y = `Incident Count`)) +
  geom_col(fill = "#AD7D2E") +
  scale_x_continuous(breaks = seq(1, 12, 1), labels = month.abb) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "grey", color = "grey"),
    panel.background = element_rect(fill = "grey", color = "grey"),
    panel.grid = element_blank(), 
    axis.text = element_text(color = "black", size = 13),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(-5, "pt"),
    axis.title = element_text(color = "black", size = 13),
    plot.title = element_text(color = "black", hjust = 0.5, size = 15),
    legend.title = element_text(),
    legend.background = element_rect(fill = NA),
    legend.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 40, hjust = 1),
    axis.title.x = element_blank()
  )

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
  labs(
    x = NULL,
    y = "Count Per Month",
    title = "Total Incident Reports from 2003 to Present By Year"
  ) +
  theme(
    plot.background = element_rect(fill = "black", color = "grey"),
    panel.background = element_rect(fill = "black", color = "grey"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "grey", size = 13
                             , family = "hn"
                             ),
    axis.ticks = element_line(color = "grey"),
    axis.ticks.length = unit(-5, "pt"),
    axis.title = element_text(color = "grey", size = 13
                              , family = "hn"
                              ),
    plot.title = element_text(color = "grey", hjust = 0.5, size = 15
                              , family = "hn"
                              ),
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA),
    legend.text = element_text(color = "grey"
                               , family = "hn"
                               ),
    legend.key.height = unit(50, "pt")
  ) 
  

ggplotly(p2)

both <- p1 / p2

ggsave("visuals/sfpd_lineplot_new.png", width = 8, height = 4)


htmlwidgets::saveWidget(p2, "visuals/sfpd_lineplot_new.html", selfcontained = FALSE, libdir = "lib/")

bind_rows(last_dec, combined_data, next_jan) |> 
  mutate(month = factor(month, levels =c("last_Dec", month.abb, "next_Jan")),
         month_number = as.numeric(month) - 1) |>
 group_by(year, month) |>
  summarize(count) |>
  arrange(desc(year)) |> print(n = 15)


incidents_line <- read_csv("data/raw/incident_reports_new.csv") |>
  select(year = `Incident Year`, date = `Incident Date`, category = `Incident Category`) |>
  mutate(month = month(date, label = T),
         month = factor(month.abb[month], levels = month.abb)) |>
  group_by(year, month) |>
  summarize(count = n()) |>
  ggplot(aes(month, count, group = year, color = year)) +
  geom_line()

