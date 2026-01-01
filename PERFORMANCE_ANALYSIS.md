# Performance Analysis Report

## Executive Summary

This report identifies performance anti-patterns, inefficient algorithms, and optimization opportunities in the SF Projects Shiny application codebase.

---

## Critical Performance Issues

### 1. **Inefficient Reactive Filtering (server.R:31-41)**
**Severity: HIGH**
**Location:** `shiny_app/server.R:31-41`

**Issue:**
```r
filtered_data <- reactive({
  filter_data <- map_data %>%
    filter((input$neighborhoods == "All" | map_data$neighborhood %in% input$neighborhoods),
           date >= input$date_range[1] & date <= input$date_range[2],
           (
             input$crime_type == "All" |
               (input$crime_type == "Part 1 Violent Crime" & violent > 0) |
               (input$crime_type == "Part 1 Property Crime" & property > 0)
           )
    )
  return(filter_data)
})
```

**Problems:**
- References `map_data$neighborhood` instead of using piped data
- Filter conditions are evaluated on every reactive trigger even when inputs haven't changed
- Multiple OR conditions evaluated sequentially instead of using optimized vector operations

**Impact:** Unnecessary data frame lookups on every filter operation

**Recommendation:**
```r
filtered_data <- reactive({
  data_to_filter <- map_data

  # Apply neighborhood filter
  if (input$neighborhoods != "All") {
    data_to_filter <- data_to_filter %>%
      filter(neighborhood %in% input$neighborhoods)
  }

  # Apply date filter
  data_to_filter <- data_to_filter %>%
    filter(date >= input$date_range[1], date <= input$date_range[2])

  # Apply crime type filter
  if (input$crime_type != "All") {
    if (input$crime_type == "Part 1 Violent Crime") {
      data_to_filter <- data_to_filter %>% filter(violent > 0)
    } else if (input$crime_type == "Part 1 Property Crime") {
      data_to_filter <- data_to_filter %>% filter(property > 0)
    }
  }

  data_to_filter
})
```

---

### 2. **Redundant Date Processing (server.R:82-84)**
**Severity: MEDIUM**
**Location:** `shiny_app/server.R:82-84`

**Issue:**
```r
output$barchart <- renderPlot({
  filtered_data() %>%
    mutate(month = month(date)) %>%
    group_by(month) %>%
    summarize(`Monthly Count` = n()) %>%
    ...
})
```

**Problem:** Month extraction from date is performed on every reactive update, even though the source data doesn't change

**Impact:** Unnecessary computation on every render (could be thousands of date conversions per render)

**Recommendation:** Precompute month in `global.R`:
```r
map_data <- data |>
  mutate(color = ifelse(violent > 0, "red", ifelse(property > 0, "yellow", "#365188")),
         month = month(date))
```

Then in `server.R`:
```r
output$barchart <- renderPlot({
  filtered_data() %>%
    group_by(month) %>%
    summarize(`Monthly Count` = n()) %>%
    ...
})
```

---

### 3. **Duplicate Data Pipelines (global.R:20-39)**
**Severity: MEDIUM**
**Location:** `shiny_app/global.R:20-39`

**Issue:**
```r
data <-
   read_csv("sf_incidents_new.csv") |>
   filter(`Incident Year` == 2023) |>
   select(...) |>
   mutate(...)

map_data <- data |>
  mutate(color = ifelse(violent > 0, "red", ifelse(property > 0, "yellow", "#365188")))
```

**Problem:**
- Creates intermediate `data` object that's only used to create `map_data`
- Two separate mutation steps when one would suffice

**Impact:** Unnecessary memory allocation and processing overhead

**Recommendation:**
```r
map_data <- read_csv("sf_incidents_new.csv") |>
  filter(`Incident Year` == 2023) |>
  select(date = `Incident Date`, year = `Incident Year`,
         neighborhood = `Analysis Neighborhood`,
         category = `Incident Category`, lat = `Latitude`,
         long = `Longitude`, id = `Incident ID`) |>
  mutate(
    category = case_when(
      category == "Human Trafficking (A), Commercial Sex Acts" ~ "Human Trafficking (A)",
      category == "Human Trafficking (B), Involuntary Servitude" ~ "Human Trafficking (B)",
      TRUE ~ category
    ),
    violent = as.integer(category %in% c("Homicide", "Rape", "Robbery", "Assault",
                                         "Human Trafficking (B)", "Human Trafficking (A)")),
    property = as.integer(category %in% c("Burglary", "Larceny Theft",
                                          "Motor Vehicle Theft", "Arson")),
    color = case_when(
      violent == 1 ~ "red",
      property == 1 ~ "yellow",
      TRUE ~ "#365188"
    )
  )
```

---

### 4. **Inefficient Conditional Logic (global.R:32-36)**
**Severity: LOW-MEDIUM**
**Location:** `shiny_app/global.R:32-36`

**Issue:**
```r
violent = ifelse(category %in% c("Homicide", "Rape", "Robbery", "Assault",
                                 "Human Trafficking (B)",
                                 "Human Trafficking (A)"), 1, 0),
property = ifelse(category %in% c("Burglary", "Larceny Theft", "Motor Vehicle Theft", "Arson"), 1, 0),
```

**Problem:**
- Using `ifelse()` instead of `as.integer()` for boolean to numeric conversion
- Multiple `%in%` lookups when categories could be stored in vectors

**Impact:** Minor performance overhead, but accumulates with large datasets

**Recommendation:**
```r
# Define at top of global.R
VIOLENT_CRIMES <- c("Homicide", "Rape", "Robbery", "Assault",
                    "Human Trafficking (B)", "Human Trafficking (A)")
PROPERTY_CRIMES <- c("Burglary", "Larceny Theft", "Motor Vehicle Theft", "Arson")

# Then in data pipeline:
violent = as.integer(category %in% VIOLENT_CRIMES),
property = as.integer(category %in% PROPERTY_CRIMES),
```

---

### 5. **Nested ifelse Anti-Pattern (global.R:39)**
**Severity: LOW**
**Location:** `shiny_app/global.R:39`

**Issue:**
```r
color = ifelse(violent > 0, "red", ifelse(property > 0, "yellow", "#365188"))
```

**Problem:** Nested `ifelse()` is harder to read and slightly less efficient than `case_when()`

**Recommendation:**
```r
color = case_when(
  violent == 1 ~ "red",
  property == 1 ~ "yellow",
  TRUE ~ "#365188"
)
```

---

## Reactive Rendering Issues

### 6. **Missing Reactive Isolation (server.R:43-79)**
**Severity: MEDIUM**
**Location:** `shiny_app/server.R:43-79`

**Issue:** Both map and barchart depend on `filtered_data()` and will re-render whenever ANY input changes (neighborhood, date range, or crime type), even if the visual output wouldn't actually change.

**Impact:**
- Map re-renders unnecessarily (expensive operation with mapboxer)
- Barchart recalculates even when month aggregation would be identical

**Recommendation:** Consider using `reactive()` with `req()` to prevent unnecessary updates, or use `isolate()` for specific inputs:

```r
# Only update map when filters actually change the data
observe({
  invalidateLater(100)  # Debounce rapid filter changes
})

# Or use eventReactive for more control
filtered_data <- eventReactive(
  c(input$neighborhoods, input$date_range, input$crime_type),
  {
    # filtering logic
  },
  ignoreNULL = FALSE
)
```

---

## Issues in Practice Files

### 7. **Multiple Grouping Operations (practice/shiny_app.R:58-112)**
**Severity: MEDIUM**
**Location:** `practice/shiny_app.R:58-112`

**Issue:**
```r
per_year <- filtered_data() |>
  filter(year %in% c(this_year, last_year, two_years, three_years),
         category %in% property_crimes$category) %>%
  group_by(category, year) |>
  summarize(count = n())

yr_span_avg <- filtered_data() |>
  filter(year %in% c(last_year, two_years, three_years),
         category %in% property_crimes$category) %>%
  group_by(category, year) |>
  summarize(count = n()) |>
  ungroup() |>
  group_by(category) |>
  summarize(avg = mean(count))

per_year |>
  inner_join(yr_span_avg, by = "category") |>
  ...
```

**Problems:**
- Filters `filtered_data()` twice with similar conditions
- Performs multiple group/ungroup operations
- Could be done in a single pass with window functions

**Impact:** Duplicate data scanning and processing

**Recommendation:**
```r
plot_data <- filtered_data() |>
  filter(category %in% property_crimes$category,
         year %in% c(last_year, two_years, three_years, this_year)) %>%
  group_by(category, year) |>
  summarize(count = n(), .groups = 'drop') |>
  group_by(category) |>
  mutate(
    avg = mean(count[year != this_year]),
    is_current = year == this_year
  ) |>
  filter(year == this_year | year == last_year)

# Then plot using plot_data directly
```

---

### 8. **Redundant Data Calculations (practice/shiny_app.R:105-112)**
**Severity: MEDIUM**
**Location:** `practice/shiny_app.R:105-112`

**Issue:**
```r
lineplot_data <- filtered_data() |>
  mutate(month = month(date, label = T),
         month = factor(month.abb[month], levels = month.abb)) |>
  filter(date < floor_date(today(), "month")) |>
  group_by(year, month) |>
  summarize(count = n(),
            avg = mean(count),
            category = category)
```

**Problems:**
- `avg = mean(count)` calculates mean of a single value (grouped count)
- `category = category` in summarize will cause issues (multiple values reduced to one)
- Month extraction and factor conversion happens on every render

**Impact:** Computation errors and inefficiency

**Recommendation:**
Precompute month aggregation or cache results:
```r
lineplot_data <- reactive({
  filtered_data() |>
    filter(date < floor_date(today(), "month")) |>
    mutate(month = month(date, label = TRUE)) |>
    count(year, month, name = "count")
})
```

---

## Data Loading Optimization

### 9. **CSV Reading Without Type Specification**
**Severity: LOW**
**Location:** `shiny_app/global.R:21`, `practice/shiny_app.R:12`, `code/sfpd_source.R:28`

**Issue:**
```r
data <- read_csv("sf_incidents_new.csv") |>
```

**Problem:** `read_csv()` infers column types, which adds overhead on every app startup

**Recommendation:**
```r
# Specify column types explicitly
data <- read_csv(
  "sf_incidents_new.csv",
  col_types = cols(
    `Incident Date` = col_date(format = "%Y-%m-%d"),
    `Incident Year` = col_integer(),
    `Analysis Neighborhood` = col_character(),
    `Incident Category` = col_character(),
    `Latitude` = col_double(),
    `Longitude` = col_double(),
    `Incident ID` = col_character()
  )
)

# Or even better: use .rds format for faster loading
# saveRDS(data, "sf_incidents_new.rds")
# data <- readRDS("sf_incidents_new.rds")
```

---

## Memory Optimization

### 10. **Unused Data Objects**
**Severity: LOW**
**Location:** `shiny_app/global.R:20`, `code/sfpd_source.R`

**Issue:** The `data` object is created but only used to create `map_data`, then remains in memory

**Recommendation:**
```r
# Remove intermediate object after use
map_data <- data |>
  mutate(color = ifelse(violent > 0, "red", ifelse(property > 0, "yellow", "#365188")))
rm(data)  # Free memory
gc()      # Force garbage collection if needed
```

Or better yet, combine into one pipeline (see Issue #3)

---

## Potential N+1 Query Patterns

### 11. **No Database N+1 Issues (CSV-based)**
**Severity: N/A**

**Analysis:** The application uses CSV files, not a database, so traditional N+1 query anti-patterns don't apply. However, the filtering patterns could be optimized as noted above.

**Note:** If this were connected to a database via `RSocrata` (commented code in global.R:23), ensure proper filtering at the API level:
```r
# Good - filter at source
read.socrata("https://data.sfgov.org/resource/wg3w-h783.csv?incident_year=2023")

# Bad - download everything then filter
read.socrata("https://data.sfgov.org/resource/wg3w-h783.csv") |> filter(year == 2023)
```

---

## UI Performance

### 12. **Large Dataset Rendering**
**Severity: MEDIUM**
**Location:** `shiny_app/server.R:43-79`

**Issue:** Rendering entire filtered dataset to map without pagination or limiting

**Impact:** With potentially thousands of points, map rendering can be slow

**Recommendation:**
- Already using clustering (good!), but consider:
  - Limiting points rendered at low zoom levels
  - Implementing server-side aggregation
  - Using `clusterProperties` for better performance

```r
# Add max points limit if needed
filtered_data_limited <- reactive({
  data <- filtered_data()
  if (nrow(data) > 10000) {
    # Sample or aggregate for performance
    data <- data %>% slice_sample(n = 10000)
  }
  data
})
```

---

## Summary of Recommendations

### High Priority
1. Fix inefficient reactive filtering (Issue #1)
2. Precompute month values (Issue #2)
3. Combine data pipelines (Issue #3)

### Medium Priority
4. Optimize ifelse to as.integer conversion (Issue #4)
5. Fix multiple grouping operations (Issue #7)
6. Fix redundant calculations in lineplot (Issue #8)
7. Implement reactive isolation/debouncing (Issue #6)

### Low Priority
8. Replace nested ifelse with case_when (Issue #5)
9. Specify CSV column types (Issue #9)
10. Remove unused data objects (Issue #10)
11. Consider point limiting for large datasets (Issue #12)

---

## Estimated Performance Gains

- **Startup time:** 10-20% improvement (from optimized data loading and pipeline consolidation)
- **Filter response:** 30-40% improvement (from optimized reactive filtering)
- **Render time:** 20-30% improvement (from precomputed values and reactive isolation)
- **Memory usage:** 15-25% reduction (from removing duplicate data objects)

---

## Testing Recommendations

1. Profile with `profvis` package to measure actual impact:
   ```r
   library(profvis)
   profvis({
     # Run your shiny app operations
   })
   ```

2. Use `shiny::reactlog` to visualize reactive dependencies:
   ```r
   options(shiny.reactlog = TRUE)
   # Then run app and press Ctrl+F3
   ```

3. Benchmark data operations with `microbenchmark`:
   ```r
   library(microbenchmark)
   microbenchmark(
     current = { # current implementation },
     optimized = { # optimized implementation },
     times = 100
   )
   ```

---

*Report Generated: 2026-01-01*
