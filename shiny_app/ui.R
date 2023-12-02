library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  titlePanel("Property and Violent Crimes"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("neighborhood", "Select Neighborhood", choices = unique(incidents$neighborhood)),
      radioButtons("crime_type", "Select Crime Type", choices = c("Property Crime", "Violent Crime"))
    ),
    
    mainPanel(
      plotOutput("crime_plot")
    )
  )
))