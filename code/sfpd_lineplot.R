source("code/sfpd_source.R")

ls()

# font_add_google(name = "Bree Serif",
#                 family = "breeserif")
# font_add_google(name = "Source Serif 4",
#                 family = "sourceserif4")
# showtext_auto()
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
    y = "Count Per Month",
    title = "Total Incident Reports from 2003 to Present By Year"
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
