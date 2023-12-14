library(tidyverse)
library(lubridate)
library(tools)



df |>
  select(category = "Category", date = "Date") |>
  mutate(date = mdy(date),
         year = year(date)) |>
  group_by(year) |>
  summarize(n = n())
  

df |>
  distinct(location)

sample <- df[sample(nrow(df), 10000, replace = F),]

head(sample$"Neighborhoods 2")

View(df)

sample |>
  select(category = "Category", date = "Date") |>
  mutate(date = mdy(date),
         year = year(date),
         category = str_replace(toTitleCase(tolower(category)), "/", " ")) |>
  group_by(year) |>
  summarize(n = n())



sample |>
  group_by(year) |>
  summarize(count = n())

### Need date, year, and category for line plot




