
new_data <- read_csv("data/raw/sfpd_incidents_120223.csv") |>
  select(date = `Incident Date`, category = `Incident Category`,
          lat = `Latitude`, long = `Longitude`) |>
  filter(year(date) == 2022 & category == "Assault") |>
  drop_na(lat, long)
write_csv(new_data, "new_data_output.csv")
