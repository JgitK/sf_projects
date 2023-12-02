library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  output$crime_plot <- renderPlot({
    selected_crimes <- if (input$crime_type == "Property Crime") property_crimes$category
    else violent_crimes$category
    
    filtered_data <- incidents %>%
      filter(neighborhood == input$neighborhood, category %in% selected_crimes)
    
    three_yr_avg <- filtered_data %>%
      filter(year %in% c(last_year, two_years, three_years)) %>%
      group_by(category, year) %>%
      summarize(count = n()) %>%
      ungroup() %>%
      group_by(category) %>%
      summarize(avg = mean(count))
    
    per_year <- filtered_data %>%
      filter(year %in% c(this_year, last_year, two_years, three_years)) %>%
      group_by(category, year) %>%
      summarize(count = n())
    
    p <- per_year %>%
      inner_join(three_yr_avg, by = "category") %>%
      filter(year == this_year | year == last_year) %>%
      mutate(this_year = year == this_year) %>%
      ggplot(aes(x = category, y = ifelse(this_year, count, avg), fill = as.factor(year))) +
      geom_col(position = "dodge", color = "black") +
      labs(title = paste("Comparison of", input$crime_type, "This Year vs 3-Year Average"),
           x = "Category",
           y = "Incident Count",
           fill = "Year") +
      scale_fill_manual(values = c("2023" = "#AD7D2E", "2022" = "#365188")) +
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
    
    print(p)
  })
})