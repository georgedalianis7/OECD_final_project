#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)
library(readr)
library(plotly)

primary_data <- read_rds("primary_data.rds")
inequality <- read_rds("inequality_data.rds")
education <- read_rds("education_data.rds")

ui <- fluidPage(theme = shinytheme("cosmo"),
  titlePanel("Social Spending Programs in the OECD"),
  tabsetPanel(
    tabPanel("About",
             h2("What is the OECD?"),
             p("Founded in 1961, the Organization for Economic Co-operation and
                Development (OECD) is an intergovernmental economic organization
                consisting of 36 member countries. Its primary goal is to 
                stimulate international economic progress, facilitate world trade, and collect
                data regarding economic development."),
               mainPanel(
                 plotlyOutput("map"),
                 h2("About the Data"))),
    tabPanel("Overview", 
           mainPanel(
             h3("Social Spending Intro"), 
             h5("testing to see if this works because I need"),
             plotOutput("image"),
             h3("This graph is an demonstration of social spending in the OECD Countries"),
             h5("testing to see if this works"),
             plotOutput("animation"))),
    tabPanel("Economic Factors",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "gdp_per_capita",
              label = "Select Input:",
              choices = c("Economic Inequality", "GDP Per Capita (PPP-adjusted)", "Life Expectancy"), selected = "Economic Inequality")),
        mainPanel(
          plotOutput("gdp_per_cap_graph"))),
        mainPanel(
          p("Cleary, talk about correlation blah blah blaheeing if this works"),
          h2("Labor Market Freedom"),
          plotOutput("labor_market_graph"),
          p("Analysis of Coefficient and stuff"))),
    tabPanel("About",
             h2("About the Data"),
             p("All data was sourced from the",
               a(href = "https://data.worldbank.org/", "World Bank"),
               "and the",
               a(href = "https://data.oecd.org/", "OECD."),
               "Specifically, I used data regarding",
               a(href = "https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?locations=OE", "GDP"),
               "and",
               a(href = "https://data.worldbank.org/indicator/SP.POP.TOTL?locations=OE", "population"),
               "from the World Bank. I utilized OECD data for",
               a(href = "https://data.oecd.org/socialexp/social-spending.htm", "social spending,"),
               a(href = "https://data.oecd.org/healthstat/life-expectancy-at-birth.htm", "life expectancy,"),
               a(href = "https://data.oecd.org/healthstat/life-expectancy-at-birth.htm", "economic inequality,"),
               "and",
               a(href = "https://data.oecd.org/eduatt/adult-education-level.htm", "education"),
               "statistics."),
             h2("Project Motivations"),
             p("After taking coursework in comparative politics and economics, I became very interested in international economic growth."),
             h2("About Me"),
             p("My name is George Dalianis, and I am a freshman at Harvard College studying government and economics. The source code for this project on my", 
               a(href = "https://github.com/georgedalianis7", "Github"), "account. Contact me at gdalianis@college.harvard.edu.")
    )
  ))
           

server <- function(input, output) {
  output$map <- renderPlotly({
    primary_data %>%
      mutate(x = rep(1, 778)) %>%
      filter(year == 2016) %>%
      plot_geo(locationmode = "country names") %>%
      add_trace(z = ~x, locations = ~country_name, showscale = FALSE) %>%
      layout(title = "Current OECD Member Nations",
             annotations = list(x = 0.9, y = -0.05, text = "Source: OECD", showarrow = FALSE))
  })
  
  output$image <- renderPlot({
      readRDS(file = "soc_spending_plot.rds")
  })
  
  output$animation <- renderImage({
    list(src = "life_exp_animation",
         contentType = "image/gif",
         width = 600)}, deleteFile = FALSE)
  
  
  output$gdp_per_cap_graph <- renderPlot({
    if(input$gdp_per_capita == "Economic Inequality") {
      readRDS(file = "ineq_regression.rds")
    }
    else if(input$gdp_per_capita == "GDP Per Capita (PPP-adjusted)"){
      readRDS(file = "soc1_regression.rds")
    } 
    else{
      readRDS(file = "life_exp_regression.rds")
    }
    
  })
  output$labor_market_graph <- renderPlot({
    readRDS(file = "mark_regression.rds")
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
